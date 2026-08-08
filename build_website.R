# Complete local build:
# 1. Generate one source page per township.
# 2. Generate one source page per municipality.
# 3. Render the full static website to docs/ for GitHub Pages.

source("generate_township_pages.R")
source("generate_municipality_pages.R")
quarto::quarto_render()
