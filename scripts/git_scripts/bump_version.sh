#!/usr/bin/env bash

VERSION_FILE="VERSION.md"
PR_TITLE="$1"

RAW_VERSION=$(sed 's/^v//' "$VERSION_FILE")

BASE_VERSION="${RAW_VERSION%%-beta.*}"
BETA="${RAW_VERSION##*-beta.}"

IFS='.' read -r MAJOR MINOR PATCH <<< "$BASE_VERSION"

PR_TYPE=$(echo "$PR_TITLE" | cut -d':' -f1 | tr '[:upper:]' '[:lower:]')
declare -A TYPE_MAP=(
    ["fix"]="PATCH"
    ["perf"]="PATCH"
    ["refactor"]="PATCH"
    ["feat"]="MINOR"
    ["breaking"]="MAJOR"
    ["docs"]="NONE"
    ["style"]="NONE"
    ["chore"]="NONE"
    ["test"]="NONE"
    ["build"]="NONE"
    ["ci"]="NONE"
)

BUMP_TYPE="${TYPE_MAP[$PR_TYPE]:-PATCH}"

if [[ "$BUMP_TYPE" == "NONE" ]]; then
    echo ""
    exit 0
fi

if [[ "$BUMP_TYPE" == "MAJOR" ]]; then
    ((MAJOR++))
    MINOR=0
    PATCH=0
    BETA=1
elif [[ "$BUMP_TYPE" == "MINOR" ]]; then
    ((MINOR++))
    PATCH=0
    BETA=1
else
    ((BETA++))
fi

NEW_VERSION="$MAJOR.$MINOR.$PATCH-beta.$BETA"
echo "$NEW_VERSION"
