### Generate tables used in child maltreatment report pages
# Author: Ama Nyame-Mensah, Ph.D.
# Last Modified: 2026-09-06

# Fetch report data
fetch_report_data <- function(report_year, scripts_dir, data_dir) {
  
  # Source some scripts
  source(file.path(scripts_dir, "utils.R"), local = TRUE, echo = FALSE)
  
  # Load valid columns
  valid_cols <- find_valid_cols(report_year = report_year)
  
  # Load map dataset
  hex_map_data <-  
    readr::read_csv(file.path(data_dir, "src_hex_data.csv"), show_col_types = FALSE)
  
  # Load full dataset
  all_data <- find_data(dir = file.path(data_dir, "clean"), report_year = report_year)
  
  # Subset full dataset by state/territory-only data
  child_data <- 
    all_data |>
    dplyr::filter(!is.na(abbrev))
  
  # Nation-level record
  us_data <- 
    all_data |> 
    dplyr::filter(location == "United States")
  
  ## Load value recoding character vector
  recoding_chr <- 
    readxl::read_excel(path = file.path(data_dir, "codebook.xlsx"), sheet = "analysis") |>
    tibble::deframe()
  
  return(
    list(
      valid_cols = valid_cols,
      recoding_chr = recoding_chr,
      hex_map_data = hex_map_data,
      child_data = child_data,
      us_data = us_data
    )
  )
}

# Fetch report data for a given state/territory
fetch_state_data <- function(params) {
  
  # source utils functions
  source(file.path(params$scripts_dir, "utils.R"), local = TRUE, echo = FALSE)
  
  # Load current state's data
  state_data <- 
    params$child_data |>
    dplyr::filter(abbrev == params$abbrev) 

  # Generate tables with embedded bar graphs
  # Gender data
  gender_mal <-
    pull_data(
      df = state_data,
      pop = "mal",
      grp = "gender",
      dtype = "perc",
      valid_cols = params$valid_cols,
      recoding_chr = params$recoding_chr,
      remove_name = FALSE
    )
  
  gender_inPop <-
    pull_data(
      df = state_data,
      pop = "pop",
      grp = "gender",
      dtype = "perc",
      valid_cols = params$valid_cols,
      recoding_chr = params$recoding_chr,
      remove_name = FALSE
    )
  
  all_gender <- 
    gender_mal |>
    dplyr::left_join(gender_inPop, by = "name") |>
    dplyr::mutate(
      none = "",
      blank_space = ""
    ) |>
    dplyr::relocate(none, .after = percent_mal)
  
  # Race/ethnicity data
  race_eth_mal <- 
    pull_data(
      df = state_data,
      pop = "mal",
      grp = "race",
      dtype = "perc",
      valid_cols = params$valid_cols,
      recoding_chr = params$recoding_chr,
      remove_name = FALSE
    )
  
  race_eth_inPop <-
    pull_data(
      df = state_data,
      pop = "pop",
      grp = "race",
      dtype = "perc",
      valid_cols = params$valid_cols,
      recoding_chr = params$recoding_chr,
      remove_name = FALSE
    )
  
  all_race_eth <- merge_tbl_data(race_eth_mal, race_eth_inPop)
  
  # Age data
  age_mal <-
    pull_data(
      df = state_data,
      pop = "mal",
      grp = "age",
      dtype = "perc",
      valid_cols = params$valid_cols,
      recoding_chr = params$recoding_chr,
      remove_name = FALSE
    )
  
  age_inPop <-
    pull_data(
      df = state_data,
      pop = "pop",
      grp = "age",
      dtype = "perc",
      valid_cols = params$valid_cols,
      recoding_chr = params$recoding_chr,
      remove_name = FALSE
    )
  
  all_age <- merge_tbl_data(age_mal, age_inPop)
  
  # Maltreatment type data
  type_mal <- 
    pull_data(
      df = state_data,
      pop = "mal",
      grp = "mal_type",
      dtype = "perc",
      valid_cols = params$valid_cols,
      recoding_chr = params$recoding_chr,
      remove_name = FALSE
    )
  
  type_inPop <-
    pull_data(
      df =  params$us_data,
      pop = "pop",
      grp = "mal_type",
      dtype = "perc",
      valid_cols = params$valid_cols,
      recoding_chr = params$recoding_chr,
      remove_name = FALSE
    )
  
  all_type <- merge_tbl_data(type_mal, type_inPop)
  
  return(
    list(
      state_data = state_data,
      all_gender = all_gender,
      all_race_eth = all_race_eth,
      all_age = all_age,
      all_type = all_type
    )
  )
}