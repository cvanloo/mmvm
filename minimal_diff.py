#!/usr/bin/env python3
from os import listdir
from pathlib import Path
import itertools
from difflib import SequenceMatcher
import sys
from collections import defaultdict

SIMILARITY_TRESHOLD = 0.9

if len(sys.argv) < 2:
    print("please pass a directory")
    sys.exit(1)
dir = Path(sys.argv[1])
if not dir.is_dir():
    print(f"{dir} is not a directory")
    sys.exit(1)
files = [str(f) for f in dir.iterdir() if f.is_file()]
similarities = defaultdict(set)
for (a, b) in itertools.combinations(files, 2):
	try:
		with open(a, 'r') as left, open(b, 'r') as right:
			left = left.read()
			right = right.read()
	except UnicodeDecodeError as e:
		print(f"skipping pair ({a}, {b}) due to decode error {e}")
		continue
	ratio = SequenceMatcher(None, left, right).ratio()
	if ratio >= SIMILARITY_TRESHOLD:
		similarities[a].add(b)
		similarities[b].add(a)
def dfs(node, visited, group):
	visited.add(node)
	group.append(node)
	for neighbor in similarities[node]:
		if neighbor not in visited:
			dfs(neighbor, visited, group)
visited = set()
groups = []
for file in files:
	if file not in visited:
		group = []
		dfs(file, visited, group)
		if len(group) > 1:
			groups.append(group)
for i, group in enumerate(groups, 1):
	print(f"\nGroup {i}:")
	for f in sorted(group):
		print(f" {f}")
