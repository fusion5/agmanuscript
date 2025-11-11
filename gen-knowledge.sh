[ $# -eq 0 ] && { echo "Error: No directory specified." >&2; exit 1; }
[ ! -d "$1" ] && { echo "Error: '$1' is not a valid directory." >&2; exit 1; }

DIR=$1

rm --verbose "${DIR:?}"/*

find . -type f \( -name "*.md" -o -name "*.cabal" -o -name "*.hs" -o -name "*yaml" -o -name ".nix" \) -not -path "./dist-newstyle*" \
  | while read -r file; do
    OUTFILENAME=${file#./}
    OUTFILENAME=${OUTFILENAME//\//-}
    OUTFILEPATH="$DIR"/"$OUTFILENAME"
    echo "--- SNIPPET $file ---" > "$OUTFILEPATH"
    cat "$file" >> "$OUTFILEPATH"
done
