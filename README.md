# pkginstall
[![Travis build status](https://travis-ci.org/r-lib/pkginstall.svg?branch=master)](https://travis-ci.org/r-lib/pkginstall)
[![Coverage status](https://codecov.io/gh/r-lib/pkginstall/branch/master/graph/badge.svg)](https://codecov.io/github/r-lib/pkginstall?branch=master)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/r-lib/pkginstall?branch=master&svg=true)](https://ci.appveyor.com/project/r-lib/pkginstall)

Provides a replacement for `utils::install.packages(repo = NULL)` for binary
packages. i.e. it extracts the compressed archives into the package library.

Compared to `utils::install.packages()` it

- Uses the same code paths on all platforms, rather than close (but not identical) code paths.
- Succeeds or fails atomically. Either the complete package is installed or it fails with an informative error message.
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
