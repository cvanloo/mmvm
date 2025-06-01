#!/usr/bin/env python3
from os import listdir
from pathlib import Path
import itertools
from difflib import SequenceMatcher
import sys
if len(sys.argv) < 2:
    print("please pass a directory")
    sys.exit(1)
dir = Path(sys.argv[1])
if not dir.is_dir():
    print(f"{dir} is not a directory")
    sys.exit(1)
files = [str(f) for f in dir.iterdir() if f.is_file()]
similarities = {}
for (a, b) in itertools.combinations(files, 2):
	try:
		with open(a, 'r') as left, open(b, 'r') as right:
			left = left.read()
			right = right.read()
	except UnicodeDecodeError as e:
		print(f"skipping pair ({a}, {b}) due to decode error {e}")
		continue
	ratio = SequenceMatcher(None, left, right).ratio()
	similarities[(a, b)] = ratio
	similarities[(b, a)] = ratio
most_similar = {}
for file in files:
	related = [(right, ratio) for (left, right), ratio in similarities.items() if left == file and right != file]
	most_similar[file] = max(related, key=lambda x: x[1])
for file, (match, ratio) in most_similar.items():
	print(f"{file} is most similar to {match} (similarity: {ratio:.2%})")
