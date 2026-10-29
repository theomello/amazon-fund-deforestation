# testthat runs in tests/testthat; pipeline functions expect the repository root.
repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."))
in_repo <- function(expr) withr::with_dir(repo_root, expr)
has_raw <- function(path) file.exists(file.path(repo_root, path))
