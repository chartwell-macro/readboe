# Local maintainer rebuild. Publication/redistribution is a separate rights decision.
# Run from the repository after a successful explicit boe_mpr_update(rebuild=TRUE).
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop('Usage: Rscript data-raw/build-mpr-snapshot.R DATA_DIR OUTPUT_DIRECTORY')
pkgload::load_all('.', quiet = TRUE)
loaded <- readboe:::boe_mpr_load_saved(args[[1L]])
if (loaded$origin != 'refreshed' || !is.null(loaded$fallback)) stop('A validated refreshed generation is required')
readboe:::boe_mpr_write_bundle(loaded$snapshot, args[[2L]])
