import os
import json
import sys
import math
from datetime import datetime, timezone
import torch
import pandas as pd
from tqdm import tqdm
from transformers import AutoTokenizer, AutoModelForSequenceClassification


def get_best_checkpoint(model_path, metric="eval_f1"):
    """
    Find the checkpoint with the best evaluation metric.

    Args:
        model_path (str): Path to the directory containing checkpoints.
        metric (str): The metric to use for checkpoint selection (default: "eval_f1").

    Returns:
        str: Path to the best checkpoint folder.
    """
    checkpoint_dirs = [d for d in os.listdir(model_path) if d.startswith("checkpoint-")]
    if not checkpoint_dirs:
        print("No checkpoints found. Using the final model.")
        return model_path

    best_checkpoint = None
    best_metric_value = float("-inf")

    for checkpoint in checkpoint_dirs:
        trainer_state_path = os.path.join(model_path, checkpoint, "trainer_state.json")
        if not os.path.exists(trainer_state_path):
            continue

        with open(trainer_state_path, "r") as f:
            trainer_state = json.load(f)

        # Extract metric
        if isinstance(trainer_state.get("best_metric"), dict):
            metric_value = trainer_state["best_metric"].get(metric, None)
        else:
            metric_value = trainer_state.get("best_metric", None)

        if metric_value is not None and metric_value > best_metric_value:
            best_metric_value = metric_value
            best_checkpoint = checkpoint

    if best_checkpoint:
        best_checkpoint_path = os.path.join(model_path, best_checkpoint)
        print(f"Using best checkpoint: {best_checkpoint_path} (Best {metric}: {best_metric_value})")
        return best_checkpoint_path

    latest_checkpoint = sorted(checkpoint_dirs, key=lambda x: int(x.split("-")[-1]), reverse=True)[0]
    latest_checkpoint_path = os.path.join(model_path, latest_checkpoint)
    print(f"No metric found, using latest checkpoint: {latest_checkpoint_path}")
    return latest_checkpoint_path


def load_model(model_path, fp16=False):
    """
    Load a fine-tuned NLI model from a local directory or Hugging Face Hub.

    Args:
        model_path (str): Local directory or Hugging Face model name.
        fp16 (bool): Whether to use FP16 inference.

    Returns:
        tokenizer, model, device: Loaded tokenizer and model objects plus device string.
    """
    if os.path.isdir(model_path):
        best_checkpoint_path = get_best_checkpoint(model_path, metric="eval_f1")
        tokenizer = AutoTokenizer.from_pretrained(best_checkpoint_path)
        model = AutoModelForSequenceClassification.from_pretrained(best_checkpoint_path)
    else:
        tokenizer = AutoTokenizer.from_pretrained(model_path)
        model = AutoModelForSequenceClassification.from_pretrained(model_path)

    device = "cuda" if torch.cuda.is_available() else "cpu"
    model.to(device)

    if fp16 and device == "cuda":
        model.half()

    model.eval()
    return tokenizer, model, device


def classify_sentiments_nli(
    model_path,
    df,
    id_var="id",
    batch_size=16,
    fp16=False,
    entailment_index=0,
    contradiction_index=2,
    show_progress=True
):
    """
    Perform targeted sentiment analysis on multiple text-target pairs with batch processing.

    Args:
        model_path (str): Local directory or Hugging Face model name.
        df (pd.DataFrame): DataFrame with id_var, 'text', and 'target' columns.
        id_var (str): Column name for the unique identifier.
        batch_size (int): Batch size for inference.
        fp16 (bool): Whether to use FP16 inference.
        entailment_index (int): Index for 'entailment' in model logits.
        contradiction_index (int): Index for 'contradiction' in model logits.
        show_progress (bool): Whether to show a tqdm progress bar (works well via reticulate).

    Returns:
        pd.DataFrame: DataFrame with id, text, target, sentiment, sentiment_confidence,
                      sentiment_model, sentiment_datetime.
    """

    # Ensure necessary columns exist
    required_cols = {id_var, "text", "target"}
    missing_cols = required_cols - set(df.columns)
    if missing_cols:
        raise ValueError(f"Missing required columns: {missing_cols}")

    # Load model and tokenizer
    tokenizer, model, device = load_model(model_path, fp16=fp16)

    # Metadata columns
    sentiment_model = str(model_path)
    sentiment_datetime = datetime.now(timezone.utc).isoformat(timespec="seconds")

    # Define hypothesis templates (ASCII only)
    hypothesis_templates = {
        "positive": "The sentiment towards {} is positive.",
        "negative": "The sentiment towards {} is negative.",
        "neutral":  "The sentiment towards {} is neutral."
    }
    label_order = ["positive", "negative", "neutral"]

    texts = df["text"].tolist()
    targets = df["target"].tolist()
    ids = df[id_var].tolist()

    results = []

    n = len(texts)
    n_batches = int(math.ceil(n / float(batch_size)))

    # tqdm -> prints to stdout (reticulate forwards nicely to R console)
    pbar = tqdm(
        total=n_batches,
        desc="Sentiment batches",
        unit="batch",
        disable=(not show_progress),
        file=sys.stdout,
        dynamic_ncols=True,
        ascii=True
    )

    try:
        for start in range(0, n, batch_size):
            end = min(start + batch_size, n)

            batch_texts = texts[start:end]
            batch_targets = targets[start:end]
            batch_ids = ids[start:end]

            # Build (premise, hypothesis) pairs in a stable order:
            # for each row -> 3 hypotheses in label_order
            premises = []
            hypotheses = []
            for text, target in zip(batch_texts, batch_targets):
                for lab in label_order:
                    premises.append(text)
                    hypotheses.append(hypothesis_templates[lab].format(target))

            batch_inputs = tokenizer(
                premises,
                hypotheses,
                truncation=True,
                padding=True,
                return_tensors="pt"
            )

            batch_inputs = {k: v.to(device) for k, v in batch_inputs.items()}

            with torch.no_grad():
                logits = model(**batch_inputs).logits

            # Shape: (batch_size * 3, num_labels) -> (batch_size, 3, num_labels)
            probs = torch.softmax(logits, dim=-1).view(len(batch_texts), 3, -1)

            # Entailment score for each hypothesis (batch_size, 3)
            entailment_probs = probs[:, :, entailment_index].detach().cpu().numpy()

            for j, target in enumerate(batch_targets):
                sentiment_idx = int(entailment_probs[j].argmax())
                sentiment_label = label_order[sentiment_idx]
                confidence = float(round(entailment_probs[j][sentiment_idx], 4))

                results.append({
                    id_var: batch_ids[j],
                    "text": batch_texts[j],
                    "target": target,
                    "sentiment": sentiment_label,
                    "sentiment_confidence": confidence,
                    "sentiment_model": sentiment_model,
                    "sentiment_datetime": sentiment_datetime
                })

            pbar.update(1)
    finally:
        pbar.close()

    return pd.DataFrame(results)