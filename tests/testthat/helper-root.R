# testthat runs in tests/testthat; pipeline functions expect the repository root.
repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
in_repo <- function(expr) withr::with_dir(repo_root, expr)
# A file offloaded by iCloud (".<name>.icloud") counts: the pipeline restores it.
has_raw <- function(path) {
  full <- file.path(repo_root, path)
  file.exists(full) || file.exists(file.path(dirname(full), paste0(".", basename(full), ".icloud")))
}
