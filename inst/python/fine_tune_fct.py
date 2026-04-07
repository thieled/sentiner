"""
Fine-tune an NLI model for targeted classification.

Designed to be called from R via reticulate or used as a standalone Python
module.  Implements correct evaluation-metric chunking: the test set carries
*n_hypotheses* rows per observation (one per possible label), while the
(down-sampled) training set typically has 2 rows per observation.

Functions
---------
detect_nli_indices   Auto-detect entailment / contradiction label indices.
train_nli_model      Full training pipeline: dataset prep → tokenise → train →
                     evaluate → save model + JSON summary.
"""

from __future__ import annotations

import json
import os
import random
from pathlib import Path

os.environ["TRANSFORMERS_NO_TF"] = "1"

import datasets
import numpy as np
import pandas as pd
import torch
import torch._dynamo
from sklearn.metrics import (
    accuracy_score,
    balanced_accuracy_score,
    precision_recall_fscore_support,
)
from transformers import (
    AutoConfig,
    AutoModelForSequenceClassification,
    AutoTokenizer,
    Trainer,
    TrainerCallback,
    TrainingArguments,
)

import sys

torch._dynamo.config.suppress_errors = True  # fall back to eager mode

__all__ = ["detect_nli_indices", "train_nli_model"]


class _ProgressCallback(TrainerCallback):
    """Print training progress to stdout (visible in R console via reticulate)."""

    def on_epoch_begin(self, args, state, control, **kwargs):
        epoch = int(state.epoch or 0) + 1
        print(f"\n--- Epoch {epoch}/{int(args.num_train_epochs)} ---",
              flush=True)

    def on_evaluate(self, args, state, control, metrics=None, **kwargs):
        if metrics:
            f1 = metrics.get("eval_f1_macro", None)
            loss = metrics.get("eval_loss", None)
            parts = [f"  Epoch {state.epoch:.0f}"]
            if loss is not None:
                parts.append(f"eval_loss={loss:.4f}")
            if f1 is not None:
                parts.append(f"eval_f1_macro={f1:.4f}")
            print(" | ".join(parts), flush=True)

    def on_log(self, args, state, control, logs=None, **kwargs):
        if logs and "loss" in logs:
            step = state.global_step
            loss = logs["loss"]
            lr = logs.get("learning_rate", None)
            msg = f"  step {step}: loss={loss:.4f}"
            if lr is not None:
                msg += f", lr={lr:.2e}"
            print(msg, flush=True)
            sys.stdout.flush()


# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

def detect_nli_indices(model_name_or_path: str) -> tuple[int, int]:
    """
    Auto-detect the entailment and contradiction label indices from a model's
    config.json.

    Looks for keys named 'entailment' and 'contradiction' /
    'not_entailment' in label2id (case-insensitive).

    Parameters
    ----------
    model_name_or_path : str
        HuggingFace model identifier or local path.

    Returns
    -------
    tuple of (entailment_index, contradiction_index)
    """
    config = AutoConfig.from_pretrained(model_name_or_path)
    label2id = {k.lower().strip(): int(v) for k, v in config.label2id.items()}

    ent_idx = label2id.get("entailment")
    con_idx = label2id.get("contradiction", label2id.get("not_entailment"))

    if ent_idx is None:
        raise ValueError(
            f"Cannot auto-detect entailment index. label2id: {config.label2id}. "
            "Pass `entailment_index` explicitly."
        )
    if con_idx is None:
        raise ValueError(
            f"Cannot auto-detect contradiction index. label2id: {config.label2id}. "
            "Pass `contradiction_index` explicitly."
        )
    return ent_idx, con_idx


def _to_hf_dataset(
    df, *, shuffle: bool = False, seed: int = 42
) -> datasets.Dataset:
    """Convert a pandas / R data frame to a HuggingFace Dataset."""
    df = pd.DataFrame(df)
    keep = ["idx", "text", "hypothesis", "label", "label_nli_explicit"]
    df = df[[c for c in keep if c in df.columns]].copy()

    df["idx"]   = pd.to_numeric(df["idx"], errors="coerce").fillna(0).astype(int)
    df["label"] = df["label"].astype(int)
    df["text"]  = df["text"].astype(str)
    df["hypothesis"] = df["hypothesis"].astype(str)
    if "label_nli_explicit" in df.columns:
        df["label_nli_explicit"] = df["label_nli_explicit"].astype(str)

    if shuffle:
        df = df.sample(frac=1, random_state=seed).reset_index(drop=True)

    return datasets.Dataset.from_pandas(df)


def _make_compute_metrics(n_hypotheses: int, entailment_index: int):
    """
    Return a compute_metrics callable for the HuggingFace Trainer.

    The test set has n_hypotheses contiguous rows per observation (one per
    possible label).  For each observation the function selects the hypothesis
    with the highest entailment logit and compares to the gold label.

    This is the corrected version of the metric that was previously hard-coded
    to chunk by 2 (the NLI output dimension) instead of the actual number of
    hypotheses per observation.

    Parameters
    ----------
    n_hypotheses : int
        Number of hypotheses (= unique labels) per observation in the test set.
    entailment_index : int
        Column index for the entailment logit in the model's output.
    """

    def compute_metrics(eval_pred):
        predictions, labels = eval_pred
        total = len(predictions)

        if total % n_hypotheses != 0:
            raise ValueError(
                f"Total predictions ({total}) is not divisible by "
                f"n_hypotheses ({n_hypotheses}).  Check that the test set has "
                f"a consistent number of rows per observation and is not "
                f"shuffled."
            )

        # Chunk into groups of n_hypotheses
        pred_chunks  = [predictions[i : i + n_hypotheses]
                        for i in range(0, total, n_hypotheses)]
        label_chunks = [labels[i : i + n_hypotheses]
                        for i in range(0, total, n_hypotheses)]

        # Predicted class: hypothesis with highest entailment logit
        y_pred = [
            int(np.argmax(np.array(chunk)[:, entailment_index]))
            for chunk in pred_chunks
        ]
        # Gold class: position of label == 0 (entailment / true)
        y_true = [int(np.argmin(chunk)) for chunk in label_chunks]

        prec_ma, rec_ma, f1_ma, _ = precision_recall_fscore_support(
            y_true, y_pred, average="macro", zero_division=0
        )
        prec_mi, rec_mi, f1_mi, _ = precision_recall_fscore_support(
            y_true, y_pred, average="micro", zero_division=0
        )

        return {
            "accuracy":          accuracy_score(y_true, y_pred),
            "accuracy_balanced": balanced_accuracy_score(y_true, y_pred),
            "f1_macro":          f1_ma,
            "f1_micro":          f1_mi,
            "precision_macro":   prec_ma,
            "recall_macro":      rec_ma,
            "precision_micro":   prec_mi,
            "recall_micro":      rec_mi,
        }

    return compute_metrics


# ---------------------------------------------------------------------------
# Main training function
# ---------------------------------------------------------------------------

def train_nli_model(
    train_data,
    test_data,
    n_hypotheses: int,
    model_name: str = "MoritzLaurer/deberta-v3-base-zeroshot-v2.0",
    run_name: str | None = None,
    output_dir: str = "./nli_model",
    learning_rate: float  = 2e-5,
    train_batch_size: int = 16,
    eval_batch_size: int  = 80,
    num_epochs: int       = 3,
    warmup_ratio: float   = 0.25,
    weight_decay: float   = 0.1,
    seed: int             = 42,
    use_fp16: bool        = False,
    entailment_index: int | None    = None,
    contradiction_index: int | None = None,
) -> str:
    """
    Fine-tune an NLI model for targeted classification.

    Parameters
    ----------
    train_data : DataFrame
        Training data with columns: idx, text, hypothesis, label,
        label_nli_explicit.  Typically 2 rows per observation (1 entailment +
        1 not-entailment) when down-sampled.
    test_data : DataFrame
        Test data -- must have exactly n_hypotheses contiguous rows per
        observation in a consistent order (sorted by idx, then
        label_hypothesis).
    n_hypotheses : int
        Number of hypotheses (= unique labels) per observation in the test set.
    model_name : str
        HuggingFace model identifier or local path.
    run_name : str or None
        Human-readable name for this training run (used in logs and the
        training summary).  Defaults to the output_dir basename.
    output_dir : str
        Directory for the saved model, checkpoints, and logs/ sub-folder.
    learning_rate, train_batch_size, eval_batch_size, num_epochs,
    warmup_ratio, weight_decay, seed : numeric
        Training hyper-parameters.
    use_fp16 : bool
        Mixed-precision training (requires CUDA).
    entailment_index : int or None
        Index for the entailment logit in the model output.
        None = auto-detected from the model config.
    contradiction_index : int or None
        Index for the contradiction / not-entailment logit.
        None = auto-detected from the model config.

    Returns
    -------
    str
        Path to the saved model directory.
    """

    # --- Type coercion (safety for reticulate) -----------------------------
    n_hypotheses     = int(n_hypotheses)
    seed             = int(seed)
    train_batch_size = int(train_batch_size)
    eval_batch_size  = int(eval_batch_size)
    num_epochs       = int(num_epochs)

    # --- Reproducibility ---------------------------------------------------
    torch.manual_seed(seed)
    random.seed(seed)
    np.random.seed(seed)

    device = "cuda" if torch.cuda.is_available() else "cpu"
    print(f"Device: {device}")

    # --- Auto-detect NLI label indices -------------------------------------
    if entailment_index is None or contradiction_index is None:
        det_ent, det_con = detect_nli_indices(model_name)
        entailment_index    = int(entailment_index    if entailment_index    is not None else det_ent)
        contradiction_index = int(contradiction_index if contradiction_index is not None else det_con)
    else:
        entailment_index    = int(entailment_index)
        contradiction_index = int(contradiction_index)

    print(f"NLI indices — entailment: {entailment_index}, "
          f"contradiction: {contradiction_index}")
    print(f"Hypotheses per test observation: {n_hypotheses}")

    # --- Load model & tokenizer --------------------------------------------
    tokenizer = AutoTokenizer.from_pretrained(
        model_name, use_fast=True, model_max_length=512
    )
    model = AutoModelForSequenceClassification.from_pretrained(model_name)
    model.to(device)

    # --- Convert to HF datasets --------------------------------------------
    dataset = datasets.DatasetDict({
        "train": _to_hf_dataset(train_data, shuffle=True,  seed=seed),
        "test":  _to_hf_dataset(test_data,  shuffle=False, seed=seed),
    })

    n_train = len(dataset["train"])
    n_test  = len(dataset["test"])
    n_train_obs = len(set(dataset["train"]["idx"]))
    n_test_obs  = len(set(dataset["test"]["idx"]))
    print(f"Train rows: {n_train} ({n_train_obs} obs × "
          f"{n_train / max(n_train_obs, 1):.0f} rows/obs)")
    print(f"Test  rows: {n_test} ({n_test_obs} obs × {n_hypotheses} hyp/obs)")

    if n_test % n_hypotheses != 0:
        raise ValueError(
            f"Test set size ({n_test}) is not divisible by n_hypotheses "
            f"({n_hypotheses}).  Ensure every observation has exactly "
            f"{n_hypotheses} rows."
        )

    # --- Tokenize ----------------------------------------------------------
    def tokenize_fn(examples):
        return tokenizer(
            examples["text"],
            examples["hypothesis"],
            truncation=True,
            max_length=512,
        )

    dataset = dataset.map(tokenize_fn, batched=True)

    cols_to_remove = [
        c for c in ["label_nli_explicit", "idx", "text", "hypothesis"]
        if c in dataset["train"].column_names
    ]
    dataset = dataset.remove_columns(cols_to_remove)

    # --- Mixed precision ---------------------------------------------------
    do_fp16 = use_fp16 and device == "cuda"
    do_bf16 = False

    # If the model weights are already in FP16 (torch_dtype: float16),
    # PyTorch's GradScaler will fail with "Attempting to unscale FP16
    # gradients".  Upcast to FP32 so AMP can manage the casting itself.
    if do_fp16 and next(model.parameters()).dtype == torch.float16:
        print("Model weights are FP16 — upcasting to FP32 for AMP training.")
        model = model.float()
        model.to(device)

    # --- Directories -------------------------------------------------------
    log_dir = os.path.join(output_dir, "logs")
    os.makedirs(log_dir, exist_ok=True)

    # --- Training arguments ------------------------------------------------
    training_args = TrainingArguments(
        output_dir            = output_dir,
        run_name              = run_name or os.path.basename(output_dir),
        logging_dir           = log_dir,
        logging_strategy      = "steps",
        logging_steps         = 50,
        save_strategy         = "epoch",
        eval_strategy         = "epoch",
        disable_tqdm          = False,
        log_level             = "info",
        learning_rate         = learning_rate,
        per_device_train_batch_size = train_batch_size,
        per_device_eval_batch_size  = eval_batch_size,
        num_train_epochs      = num_epochs,
        warmup_ratio          = warmup_ratio,
        weight_decay          = weight_decay,
        seed                  = seed,
        load_best_model_at_end = True,
        metric_for_best_model  = "f1_macro",
        greater_is_better      = True,
        report_to             = "none",
        fp16                  = do_fp16,
        bf16                  = do_bf16,
    )

    # --- Compute metrics (with correct chunk size) -------------------------
    compute_metrics = _make_compute_metrics(n_hypotheses, entailment_index)

    # --- Trainer -----------------------------------------------------------
    trainer = Trainer(
        model          = model,
        args           = training_args,
        train_dataset  = dataset["train"],
        eval_dataset   = dataset["test"],
        compute_metrics = compute_metrics,
        processing_class = tokenizer,
        callbacks      = [_ProgressCallback()],
    )

    # --- Train -------------------------------------------------------------
    train_result = trainer.train()

    # --- Save best model to output_dir root --------------------------------
    model.save_pretrained(output_dir)
    tokenizer.save_pretrained(output_dir)

    # --- Final evaluation --------------------------------------------------
    eval_results = trainer.evaluate()

    # --- Write training summary JSON ---------------------------------------
    summary = {
        "model_name":          model_name,
        "run_name":            run_name or os.path.basename(output_dir),
        "output_dir":          str(Path(output_dir).resolve()),
        "n_hypotheses":        n_hypotheses,
        "entailment_index":    entailment_index,
        "contradiction_index": contradiction_index,
        "data": {
            "train_rows":         n_train,
            "train_observations": n_train_obs,
            "test_rows":          n_test,
            "test_observations":  n_test_obs,
        },
        "hyperparameters": {
            "learning_rate":    learning_rate,
            "train_batch_size": train_batch_size,
            "eval_batch_size":  eval_batch_size,
            "num_epochs":       num_epochs,
            "warmup_ratio":     warmup_ratio,
            "weight_decay":     weight_decay,
            "fp16":             bool(training_args.fp16),
            "seed":             seed,
        },
        "train_metrics":        {k: v for k, v in train_result.metrics.items()},
        "eval_metrics":         {k: v for k, v in eval_results.items()},
        "best_model_checkpoint": trainer.state.best_model_checkpoint,
        "best_metric_value":     trainer.state.best_metric,
    }

    summary_path = os.path.join(log_dir, "training_summary.json")
    with open(summary_path, "w") as f:
        json.dump(summary, f, indent=2, default=str)

    print(f"\nTraining complete.  Model saved to: {output_dir}")
    print(f"Training summary:  {summary_path}")
    print(f"Best checkpoint:   {trainer.state.best_model_checkpoint}")
    print(f"Best f1_macro:     {trainer.state.best_metric:.4f}")

    return str(output_dir)
