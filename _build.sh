#!/bin/sh

set -eu

Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"

# Copy standalone interactive labs into the published site
mkdir -p docs/labs
cp labs/*.html docs/labs/
