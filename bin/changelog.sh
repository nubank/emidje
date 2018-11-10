#!/usr/bin/env bash
set -euo pipefail

version=$1

changelog=CHANGELOG.md

today=$(date --utc +'%Y-%m-%d')

sed -ie "s/\(##\s*\[Unreleased\]\)/\1\n\n## [$version] - $today/g" $changelog
