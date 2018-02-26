
make_install_process <- function(filename, lib = .libPaths()[[1L]],
                                 metadata = NULL) {
  filename; lib; metadata

  now <- Sys.time()

  type <- detect_package_archive_type(filename)
  if (type == "unknown") {
    abort(type = "invalid_input",
          "Cannot extract {filename}, unknown type")
  }

  lib_cache <- library_cache(lib)
  mkdirp(pkg_cache <- tempfile(tmpdir = lib_cache))

  ppfun <- function() {
    install_extracted_binary(filename, lib_cache, pkg_cache, lib,
                             metadata, now)
  }

  p <- if (type == "zip") {
    make_unzip_process(filename, exdir = pkg_cache, post_process = ppfun)
  } else {
    ## TODO: we already know the package type, no need to detect again
    make_untar_process(filename, exdir = pkg_cache, post_process = ppfun)
  }

  reg.finalizer(p, function(...) unlink(pkg_cache, recursive = TRUE), TRUE)

  p
}
