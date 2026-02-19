#!/bin/bash

cd Raw_data/respiration_data_UTF16LE

for file in *.csv; do iconv -f UTF-16LE -t UTF-8 <"$file" >"$file".tmp && mv "$file".tmp ../respiration_data_UTF8/"$file"; done