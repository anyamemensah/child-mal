### Source main process and render scripts
# Author: Ama Nyame-Mensah, Ph.D.
# Last Modified: 2026-09-01

source_scripts <- function(scripts_dir = "./_scripts") {
  r_files <- c("data_process.R","render_reports.R", "render_index.R")
  invisible(lapply(r_files, \(x) source(file.path(scripts_dir, x))))
}
