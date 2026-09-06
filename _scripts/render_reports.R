### Main script to render summary report pages
# Author: Ama Nyame-Mensah, Ph.D.
# Last updated: 2026-09-01

render_reports <- function(report_year,
                           scripts_dir = "./_scripts",
                           data_dir = "./_data",
                           input_qmd = "fact_sheets.qmd") {
  
  curr_frame  <- environment()
  
  r_files <- list.files(
    path = scripts_dir, 
    pattern = "data_load\\.R$|render_yml\\.R$", 
    full.names = TRUE
  )
  
  invisible(lapply(r_files, source, echo = FALSE, local = curr_frame))
  
  src_data <- fetch_report_data(report_year = report_year, scripts_dir = scripts_dir, data_dir = data_dir)
  
  common_params <- 
    list(
      report_year = report_year,
      scripts_dir = "./_scripts",
      data_dir = "./_data"
    )

  ## tibble with information to pass to report
  report_inputs <-
    src_data$hex_map_data |>
    dplyr::mutate(
      State = stringr::str_squish(State),
      input = input_qmd,
      output_file = paste0(tolower(abbrev), "_factsheet.html"),
      params = purrr::map2(State, abbrev, \(state, abbrev) c(
        list(state = state, abbrev = abbrev), common_params))
    ) |>
    dplyr::select(-c(abbrev:State))
  
  ## render report pages
  if (!dir.exists("./docs")) {
    dir.create("./docs")
  }
  
  render_quarto_yml(output_dir = "docs/", proj_dir = ".")
  
  report_inputs |>
    purrr::pwalk(\(input, output_file, params) {
      quarto::quarto_render(
        input = input,
        output_file = output_file,
        execute_params = params,
        quiet = TRUE
      )
    })
  
  delete_quarto_yml(proj_dir = ".")
}
