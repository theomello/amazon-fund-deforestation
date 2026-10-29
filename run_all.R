# Rebuilds everything from public sources: open amazon-fund-deforestation.Rproj
# in RStudio and source this file, or run `Rscript run_all.R` from the repo root.
# Downloads are cached in data/raw/; set REFRESH <- TRUE to fetch them again.
if (!dir.exists("data")) stop("Working directory must be the repository root (open the .Rproj file)")
suppressPackageStartupMessages(library(dplyr))
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
REFRESH <- FALSE
