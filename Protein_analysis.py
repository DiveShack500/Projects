import sys
import pandas as pd
from Bio import SeqIO
from transformers import AutoTokenizer, AutoModel
import torch
import os

# Load ProtBert model and tokenizer
tokenizer = AutoTokenizer.from_pretrained("Rostlab/prot_bert", do_lower_case=False)
model = AutoModel.from_pretrained("Rostlab/prot_bert")

def embed_sequence(sequence):
    sequence = ' '.join(sequence.upper())  # space-separated AAs
    tokens = tokenizer(sequence, return_tensors="pt", truncation=True)
    with torch.no_grad():
        output = model(**tokens)
    # Mean pooling over sequence length
    return output.last_hidden_state.mean(dim=1).squeeze().numpy()

def main(fasta_path):
    if not os.path.exists(fasta_path):
        print(f"File not found: {fasta_path}")
        return

    embeddings = []
    for record in SeqIO.parse(fasta_path, "fasta"):
        try:
            emb = embed_sequence(str(record.seq))
            embeddings.append({
                "id": record.id,
                **{f"emb_{i}": val for i, val in enumerate(emb)}
            })
        except Exception as e:
            print(f"Failed to embed {record.id}: {e}")

    df = pd.DataFrame(embeddings)
    df.to_csv("protein_embeddings.csv", index=False)
    print("Saved protein embeddings to protein_embeddings.csv")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python3 file.py <fasta_file>")
    else:
        main(sys.argv[1])
