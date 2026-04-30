# Compatibility loader for the refactored SDM engine.

.__sdm_ofiles <- vapply(sys.frames(), function(frame) {
  if (!is.null(frame$ofile)) frame$ofile else NA_character_
}, character(1))
.__sdm_ofile_dirs <- dirname(normalizePath(.__sdm_ofiles[!is.na(.__sdm_ofiles)], winslash = "/", mustWork = FALSE))
.__sdm_module_candidates <- unique(c(
  .__sdm_ofile_dirs,
  file.path(getwd(), "R"),
  dirname(normalizePath(file.path("R", "optimized_sdm.R"), winslash = "/", mustWork = FALSE)),
  dirname(normalizePath("optimized_sdm.R", winslash = "/", mustWork = FALSE))
))
.__sdm_module_dir <- .__sdm_module_candidates[file.exists(file.path(.__sdm_module_candidates, "load.R"))][1]
if (is.na(.__sdm_module_dir)) {
  stop("Could not locate R/load.R. Re-extract the full SDM project folder.", call. = FALSE)
}
source(file.path(.__sdm_module_dir, "load.R"), local = FALSE)
sdm_set_project_root(dirname(.__sdm_module_dir))
rm(.__sdm_module_dir, .__sdm_module_candidates, .__sdm_ofile_dirs, .__sdm_ofiles)
