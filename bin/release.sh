#!/usr/bin/env bash
set -euo pipefail

version=$1

update_changelog() {
    changelog=CHANGELOG.md
    today=$(date --utc +'%Y-%m-%d')
    sed -ie "s/\(##\s*\[Unreleased\]\)/\1\n\n## [$version] - $today/g" $changelog
    git add $changelog
}

commit_and_push() {
    git commit -m "Release version $version"
    git push origin
}

create_tag() {
    git tag $version
    git push origin $version
}

dirty=$(git status --porcelain)

if [ ! -z ${dirty+x} ]; then
    echo "Error: your working tree is dirty. Aborting release."
    exit 1
fi

update_changelog
commit_and_push
create_tag
