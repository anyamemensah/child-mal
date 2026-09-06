### Process data for child maltreatment report pages
# Author: Ama Nyame-Mensah, Ph.D.
# Last Modified: 2026-09-06

process_data <- function(report_year, scripts_dir = "./_scripts", data_dir = "./_data") {
  
  # Stop if report_year is not between 2015 and 2023
  if (!report_year %in% 2015:2023) {
    stop(paste(
      "ERROR: The value for report_year must be between 2015 and 2023. You entered:", 
      report_year)
    )
  }
  
  # Key paths
  clean_data_dir <- file.path(data_dir, "clean")
  file_to_check <- file.path(clean_data_dir, paste0("child_mal_", report_year, ".parquet"))
  
  # Check for existing data
  if (file.exists(file_to_check)) {
    message(
      paste0(
        "Processed data for report year ", report_year,
        " already exists at ", clean_data_dir, ". Skipping processing."
      )
    )
    return(invisible(TRUE))
  }
  
  # Initialize output directory
  if (!dir.exists(clean_data_dir)) {
    message(paste("Creating clean data directory:", clean_data_dir))
    dir.create(clean_data_dir)
  }
  
  # Load utility functions
  message("Loading utility functions...")
  source(file.path(scripts_dir, "utils.R"), local = TRUE, echo = FALSE)
  
  # Load codebook & create renaming mapping
  message("Loading codebook and preparing column mappings...")
  src_cb <- readxl::read_excel(path = file.path(data_dir, "codebook.xlsx"), sheet = "cleaning")
  
  renaming_map <- src_cb |>
    dplyr::select(old_name, new_name) |>
    dplyr::distinct() |>
    tibble::deframe()
  
  # Standardize hex map data (state info)
  clean_hex_path <- file.path(data_dir, "src_hex_data.csv")
  raw_hex_path <- file.path(data_dir, "raw", "hex_data.csv")
  
  if (!file.exists(clean_hex_path)) {
    message("Standardizing hex map data...")
    src_hex <- readr::read_csv(raw_hex_path, show_col_types = FALSE) |>
      dplyr::mutate(State = stringr::str_squish(State)) |>
      dplyr::rename(
        abbrev = "Abbreviation",
        row = "Row",
        column = "Column"
      )
    # export standardized version
    readr::write_csv(x = src_hex, file = clean_hex_path)
    message(paste("Standardized hex map exported to:", clean_hex_path))
  } else {
    message("Using existing hex map data.")
    src_hex <- readr::read_csv(clean_hex_path, show_col_types = FALSE)
  }
  
  # Load source table info
  src_tbl <- 
    readr::read_csv(file.path(data_dir, "src_table.csv"), show_col_types = FALSE) |>
    dplyr::mutate(df_name = paste(var_group, var_type, sep = "_")) |>
    dplyr::select(raw_filepath, df_name)
  
  # Source data & do some light cleaning
  message("Loading and cleaning raw source data...")
  src_dfs <- src_tbl |>
    dplyr::select(raw_filepath) |>
    purrr::pmap(function(raw_filepath) {
      data_chunk <- readxl::read_excel(raw_filepath) |>
        dplyr::rename_with(
          .fn = ~ unname(renaming_map[.x]),
          .cols = dplyr::any_of(names(renaming_map))
        ) |>
        dplyr::mutate(
          time_frame = as.integer(time_frame),
          value2 = dplyr::if_else(value %in% c("<.5%", "N.R."), "0", value)
        ) |>
        dplyr::select(-c(value)) |>
        dplyr::rename(value = value2) |>
        dplyr::filter(time_frame == report_year)
      return(data_chunk)
    }) |>
    stats::setNames(src_tbl$df_name)

  message("Starting data processing and aggregation...")

  # Process data
  processed_metrics_list <- purrr::map2(
    .x = rep(names(src_dfs), each = 3),
    .y = rep(c("number", "percent", "rate per 1,000"), times = length(names(src_dfs))),
    .f = ~ {
      process_data(
        df = src_dfs[[.x]],
        df_name = .x,
        cb = src_cb,
        data_fmt = .y
      )
    }
  )
  
  processed_df_list <- Filter(Negate(is.null), processed_metrics_list)
  processed_df_joined <- purrr::reduce(processed_df_list, dplyr::inner_join, by = "location")
  
  # Merge processed_df_join with state hex map data
  processed_df <- 
    processed_df_joined |>
    dplyr::left_join(src_hex, by = c("location" = "State")) |>
    dplyr::relocate(dplyr::any_of(c("abbrev", "row", "column")), .after = location) |>
    dplyr::mutate(
      abbrev = dplyr::case_when(location == "United States" ~ "USA", TRUE ~ abbrev),
      row = dplyr::case_when(location == "United States" ~ -100, TRUE ~ row),
      column = dplyr::case_when(location == "United States" ~ -100, TRUE ~ column)
    )
  
  # Export and clean up
  processed_df_filepath <- file.path(clean_data_dir, paste0("child_mal_", report_year, ".parquet"))
  arrow::write_parquet(x = processed_df, sink = processed_df_filepath)
  
  # Final msg
  message(paste(
    "Successfully processed and exported dataset for report year", 
    report_year, 
    "to:", 
    processed_df_filepath)
  )
}
