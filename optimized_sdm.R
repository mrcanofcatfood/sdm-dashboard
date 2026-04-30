# Root-level compatibility loader.
# Prefer R/optimized_sdm.R; this file exists so older launch paths still work.

.__sdm_root_loader_ofiles <- vapply(sys.frames(), function(frame) {
  if (!is.null(frame$ofile)) frame$ofile else NA_character_
}, character(1))
.__sdm_root_loader_path <- .__sdm_root_loader_ofiles[!is.na(.__sdm_root_loader_ofiles) & basename(.__sdm_root_loader_ofiles) == "optimized_sdm.R"]
.__sdm_root_loader_dir <- if (length(.__sdm_root_loader_path) > 0) dirname(normalizePath(.__sdm_root_loader_path[length(.__sdm_root_loader_path)], winslash = "/", mustWork = FALSE)) else getwd()

source(file.path(.__sdm_root_loader_dir, "R", "bootstrap.R"))
sdm_set_project_root(.__sdm_root_loader_dir)

engine_file <- sdm_project_path("R", "optimized_sdm.R")
if (!file.exists(engine_file)) {
  stop("R/optimized_sdm.R was not found. Re-extract the full SDM project folder.", call. = FALSE)
}
source(engine_file)
rm(.__sdm_root_loader_dir, .__sdm_root_loader_path, .__sdm_root_loader_ofiles)
