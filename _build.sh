#!/bin/sh

set -eu

Rscript -e "bookdown::render_book('index.Rmd', 'bookdown::gitbook')"
