# pkginstall

Provides a replacement for `utils::install.packages(repo = NULL)` for binary
packages. i.e. it extracts the compressed archives into the package library.

Compared to `utils::install.packages()` it

- Uses the same code paths on all platforms, rather than close (but not identical) code paths.
- Provides more specific error messages
- Has additional tests for package validity before installing
- Always uses per-package lock files, to protect against simultaneous installation
- Has a robust set of tests, to ensure correctness and ease debugging installation issues.

## Installation

```r
devtools::install_github("pkginstall")
```

## Example

``` r
files <- download.packages("remotes", type = "binary", ".")
pkginstall::install_binary(files[[2]])
```
