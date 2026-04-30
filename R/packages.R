# Runtime and dependency helpers for the SDM project.

sdm_required_packages <- c("terra")
sdm_app_packages <- c("shiny", "bslib", "terra")
sdm_setup_packages <- c("shiny", "bslib", "terra", "geodata")

detect_available_cores <- function(logical = TRUE) {
  cores <- tryCatch(parallel::detectCores(logical = logical), error = function(e) NA_integer_)
  if (is.na(cores) || cores < 1) 1L else as.integer(cores)
}

normalize_core_count <- function(n_cores = NULL, reserve_one = FALSE) {
  available <- detect_available_cores(TRUE)
  if (is.null(n_cores) || length(n_cores) == 0 || is.na(suppressWarnings(as.integer(n_cores[1])))) {
    n <- available - as.integer(reserve_one)
  } else {
    n <- suppressWarnings(as.integer(n_cores[1]))
  }
  max(1L, min(available, n))
}

set_compile_threads <- function(n_cores) {
  n_cores <- normalize_core_count(n_cores)
  Sys.setenv(
    MAKEFLAGS = paste0("-j", n_cores),
    OMP_NUM_THREADS = as.character(n_cores),
    OPENBLAS_NUM_THREADS = as.character(n_cores),
    MKL_NUM_THREADS = as.character(n_cores),
    VECLIB_MAXIMUM_THREADS = as.character(n_cores),
    NUMEXPR_NUM_THREADS = as.character(n_cores)
  )
  options(Ncpus = n_cores, mc.cores = n_cores)
  invisible(n_cores)
}

configure_user_library <- function() {
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(user_lib)) {
    user_lib <- file.path(Sys.getenv("LOCALAPPDATA", unset = path.expand("~")), "R", "win-library", paste(R.version$major, R.version$minor, sep = "."))
    Sys.setenv(R_LIBS_USER = user_lib)
  }
  user_lib <- path.expand(user_lib)
  if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  normalized_libs <- normalizePath(.libPaths(), winslash = "/", mustWork = FALSE)
  normalized_user_lib <- normalizePath(user_lib, winslash = "/", mustWork = FALSE)
  if (dir.exists(user_lib) && !(normalized_user_lib %in% normalized_libs)) {
    .libPaths(c(user_lib, .libPaths()))
  }
  invisible(user_lib)
}

ensure_sdm_packages <- function(packages = sdm_required_packages, install = TRUE, n_cores = NULL) {
  configure_user_library()
  n_cores <- set_compile_threads(normalize_core_count(n_cores, reserve_one = FALSE))
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    if (!install) stop("Missing required R packages: ", paste(missing, collapse = ", "), call. = FALSE)
    message("Installing missing packages with ", n_cores, " compile worker(s): ", paste(missing, collapse = ", "))
    message("Package library: ", .libPaths()[1])
    install.packages(missing, repos = "https://cloud.r-project.org", Ncpus = n_cores, lib = .libPaths()[1])
  }
  invisible(TRUE)
}

configure_parallel <- function(n_cores = NULL, log_fun = NULL) {
  n_cores <- normalize_core_count(n_cores, reserve_one = is.null(n_cores))
  set_compile_threads(n_cores)
  if (requireNamespace("terra", quietly = TRUE)) {
    try(terra::terraOptions(memfrac = 0.75, progress = 1), silent = TRUE)
  }
  log_message(log_fun, "Using ", n_cores, " CPU core(s) for package compilation, cross-validation, and raster prediction")
  n_cores
}
