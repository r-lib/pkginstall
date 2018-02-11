
#' Intall Packages from Local Files
#'
#' Provides a replacement for `utils::install.packages(repo = NULL)`.
#' I.e. it builds binary packages from source packages, and extracts the
#' compressed archives into the package library.
#'
#' @section Features:
#'
#' Compared to `utils::install.packages()` it
#'
#' - Has robust support for installing packages in parallel.
#' - Fails immediately when the first package fails when installing
#'   multiple packages, rather than returning a warning.
#' - Uses the same code paths on all platforms, rather than similar (but
#'   not identical) code paths.
#' - Succeeds or fails atomically. Either the complete package is installed
#'   or it fails with an informative error message.
#' - Has additional tests for package validity before installing
#' - Always uses per-package lock files, to protect against simultaneous
#'   installation.
#' - Has a robust set of tests, to ensure correctness and ease debugging
#'   installation issues.
#'
#' @section Locking:
#'
#' pkginstall uses the `install.lock` option. If this is set to `FALSE`,
#' then no locking is performed. For all other values (including if the
#' option is unset or is `NULL`), per-package lock files are used, via the
#' filelock package.
#'
"_PACKAGE"
