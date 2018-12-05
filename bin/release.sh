#!/usr/bin/env bash
set -euo pipefail

version=$1

changelog=CHANGELOG.md

update_changelog() {
    update_unreleased_section
    update_reference_links
    rm -f ${changelog}e
}

update_unreleased_section() {
    today=$(date --utc +'%Y-%m-%d')
    sed -ie "s/\(##\s*\[Unreleased\]\)/\1\n\n## [$version] - $today/g" $changelog
}

update_reference_links() {
    local unreleased_ref="[Unreleased]: https:\/\/github.com\/nubank\/emidje\/compare\/$version...HEAD"
    sed -ie "s/\[Unreleased\]\(:.*\)HEAD$/$unreleased_ref\n[$version]\1$version/g" $changelog
}

commit_and_push() {
    local current_branch=$(git symbolic-ref HEAD | sed 's!refs\/heads\/!!')
    git add $changelog
    git commit -m "Release version $version"
    git push origin $current_branch
}

create_tag() {
    git tag $version
    git push origin $version
}

dirty=$(git status --porcelain)

if [ ! -z "$dirty" ]; then
    echo "Error: your working tree is dirty. Aborting release."
    exit 1
fi

update_changelog
commit_and_push
create_tag
