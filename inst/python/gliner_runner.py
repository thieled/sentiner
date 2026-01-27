import sys
import torch
import warnings
import simdjson
import json
import random
import numpy as np
import os
from gliner import GLiNER
from transformers import logging as hf_logging

warnings.filterwarnings("ignore", category=UserWarning, module="transformers")

# --------------------------------------------------
# Suppress noisy output
# --------------------------------------------------
os.environ["HF_HUB_DISABLE_PROGRESS_BARS"] = "1"
os.environ["TOKENIZERS_PARALLELISM"] = "false"

hf_logging.set_verbosity_error()

# ----------------------------
# Device setup
# ----------------------------
device = "cuda" if torch.cuda.is_available() else "cpu"
if device == "cuda":
    print(f"[gliner_runner] Using GPU: {torch.cuda.get_device_name(0)}",
          flush=True)
else:
    print("[gliner_runner] Using CPU", flush=True)

# ----------------------------
# Reproducibility setup
# ----------------------------
SEED = int(os.environ.get("GLINER_SEED", "42"))

random.seed(SEED)
np.random.seed(SEED)
torch.manual_seed(SEED)
if torch.cuda.is_available():
    torch.cuda.manual_seed(SEED)
    torch.cuda.manual_seed_all(SEED)

torch.backends.cudnn.deterministic = True
torch.backends.cudnn.benchmark = False

print(f"[gliner_runner] Random seed set to {SEED}", flush=True)

# ----------------------------
# CLI entrypoint
# ----------------------------
if len(sys.argv) < 3:
    print("Usage: python gliner_runner.py <input_json> <output_json> [model_name]",
          flush=True)
    sys.exit(1)

input_file, output_file = sys.argv[1], sys.argv[2]
model_name = sys.argv[3] if len(sys.argv) > 3 else "urchade/gliner_multi-v2.1"

print(f"[gliner_runner] Loading model: {model_name} on {device}...", flush=True)
model = GLiNER.from_pretrained(model_name)
model = model.to(device)
model.eval()

print(f"[gliner_runner] Model loaded. Device: {next(model.parameters()).device}", flush=True)

# ----------------------------
# Entity extraction
# ----------------------------
def extract_entities(doc_ids, texts, labels, batch_size=8, threshold=0.1):
    """
    Extract entities using GLiNER's inference method.
    The model.inference() handles batching internally and uses the device
    the model was loaded to (cuda if available, cpu otherwise).
    
    Default threshold is 0.1 to capture more entities (lower = more sensitive).
    """
    # inference() processes all texts with internal batching
    batch_ents = model.inference(
        texts,
        labels,
        flat_ner=True,
        threshold=threshold,
        multi_label=False,
        batch_size=batch_size
    )
    
    results = []
    for doc_id, ents in zip(doc_ids, batch_ents):
        for idx, ent in enumerate(ents, start=1):
            results.append({
                "doc_id": doc_id,
                "entity_count": idx,
                "entity_name": ent["text"],
                "label": ent.get("label"),
                "score": ent.get("score"),
                "start": ent["start"] + 1,  # R-style indexing
                "end": ent["end"] + 1       # inclusive
            })
    
    print(f"[gliner_runner] Processed {len(texts)} docs, extracted {len(results)} entities",
          flush=True)
    return results

# ----------------------------
# Main
# ----------------------------
parser = simdjson.Parser()
doc = parser.load(input_file, recursive=True)

doc_ids = list(doc["doc_ids"])
texts = list(doc["texts"])
labels = list(doc["labels"])

raw_bs = doc.get("batch_size", 8)
batch_size = int(raw_bs[0] if isinstance(raw_bs, list) else raw_bs)

raw_th = doc.get("threshold", 0.1)
threshold = float(raw_th[0] if isinstance(raw_th, list) else raw_th)

entities = extract_entities(doc_ids, texts, labels, batch_size=batch_size, threshold=threshold)

with open(output_file, "w", encoding="utf-8") as f:
    json.dump(entities, f, ensure_ascii=False, indent=2)

print(f"[gliner_runner] Done. Extracted {len(entities)} entities.",
      flush=True)
