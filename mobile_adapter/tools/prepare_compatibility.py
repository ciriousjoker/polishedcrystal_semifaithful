#!/usr/bin/env python3
"""Embed the exact clean counterpart's SHA-256 without changing that ROM."""
import argparse
import hashlib
from pathlib import Path


def generate(baseline, output):
    image = baseline.read_bytes()
    if len(image) != 2 * 1024 * 1024 or image[0x147:0x14a] != bytes((0x10, 6, 3)):
        raise ValueError("Compatibility identity requires the normal 2 MiB RTC counterpart")
    digest = hashlib.sha256(image).digest()
    output.parent.mkdir(parents=True, exist_ok=True)
    if not output.exists() or output.read_bytes() != digest:
        output.write_bytes(digest)
    return digest.hex()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--baseline', type=Path, required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    print('Clean ROM compatibility identity:', generate(args.baseline, args.output))
