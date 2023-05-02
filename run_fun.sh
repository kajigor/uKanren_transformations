#!/bin/bash

while IFS=";" read -r path ground relation
do
  filename=$(basename "$path")
  directory=$(dirname "$path")
  suffix="_${relation}_${ground}"
  outputDir="${directory}/out/"
  output="${outputDir}${filename%.*}${suffix}.hs"
  echo "$output"
  echo "stack run -- --translate -i $path --ground $ground --rel $relation"
  mkdir -p "$outputDir"
  cp "$path" "$outputDir"
  echo "import Stream" > "$output"
  echo "import Control.Monad" >> "$output"
  stack run -- --translate -i "$path" --ground "$ground" --rel "$relation" >> "$output" 2>&1
  echo ""
done < <(tail -n +2 inputs.csv)
