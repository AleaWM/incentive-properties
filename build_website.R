# Complete local build:
# 1. Generate one source page per township.
# 2. Generate one source page per municipality.
# 3. Render the full static website to docs/ for GitHub Pages.

source("generate_township_pages.R")
source("generate_municipality_pages.R")
quarto::quarto_render()


# note to self:
# prevents Quarto from clearing docs first
# build_website.R currently regenerates both sets of source files and
# renders the entire site. For township-only work, skip that script.

#  If you changed the township template, and only want to rerender the townshps, first run:
# source("generate_township_pages.R")   # to refresh the qmd files in the townships folder with the updated code
# have _quarto.hml file include:

# project:
#     render:
#       - "townships/*.qmd"

# then run this in the terminal:
# quarto render townships --no-clean
