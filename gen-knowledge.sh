find . -type f \( -name "*.cabal" -o -name "*.hs" -o -name "*yaml" -o -name ".nix" \) -not -path "./dist-newstyle*" \
  | while read -r file; do
    echo "--- SNIPPET $file ---"
    cat "$file"
done
