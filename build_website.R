# Complete local build:
# 1. Generate the township source pages with their parameter values.
# 2. Render the full static website to docs/ for GitHub Pages.

source("generate_township_pages.R")
quarto::quarto_render()
