### A series of functions used to generate report page tables
# Author: Ama Nyame-Mensah, Ph.D.
# Last Modified: 2026-09-06

# Find select valid columns
find_valid_cols <- function(dir = "./_data/clean", report_year) {
  pq <- list.files(dir, full.names = TRUE)
  pq <- pq[grepl(report_year, pq)]
  
  if (length(pq) != 1) {
    stop(
      paste(
        "Check the clean data directory. A single parquet file for report year",
        report_year,
        "was not found."
      )
    )
  }
  
  df <- arrow::read_parquet(pq[1])
  
  valid_cols <- 
    gsub("^perc_|^num_|^rate_", "", names(df)) |>
    unique() |>
    (\(x) x[!grepl("under_19$|_18$|under18$|race_total$", x)])()
  
  return(valid_cols)
}


# Find processed dataset
find_data <- function(dir = "./_data/clean", report_year) {
  pq <- list.files(dir, full.names = TRUE)
  pq <- pq[grepl(report_year, pq)]
  
  if (length(pq) != 1) {
    stop(
      paste(
        "Check the clean data directory. A single parquet file for report year",
        report_year,
        "was not found."
      )
    )
  }
  
  return(arrow::read_parquet(pq[1]))
}


# Process single age group data (special)
process_s_age_group <- function(df, grp_col, rename_grp_chr, data_fmt, data_prefix) {
  
  if (data_fmt == "rate per 1,000") {
    NULL
  }
  
  df2 <- 
    df |>
    dplyr::select(location, dplyr::all_of(grp_col), data_format, value) |>
    dplyr::filter(tolower(data_format) == "number") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(grp_col),
        ~ dplyr::replace_values(
          .x,
          from = names(rename_grp_chr),
          to = unname(rename_grp_chr)
        )
      ),
      value = as.numeric(value),
      !! rlang::sym(grp_col) := factor(!!rlang::sym(grp_col), levels = unique(unname(rename_grp_chr)))
    ) |>
    dplyr::filter(!.data[[grp_col]] %in% c("18", "under_19")) |>
    dplyr::group_by(location, .data[[grp_col]]) |>
    dplyr::summarise(value = sum(value), .groups = "drop")
  
  if (data_fmt == "percent") {
    df2 <-
      df2 |>
      dplyr::group_by(location) |>
      dplyr::mutate(value = value / sum(value)) |>
      dplyr::ungroup()
  } 
  
  df2 |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(grp_col),
      values_from = value,
      names_prefix = data_prefix
    ) |>
    dplyr::rename_with(tolower)
}


# Process data without groups
process_no_grp <- function(df, df_name, data_fmt, data_prefix) {
  df |>
    dplyr::select(c(location, data_format, value)) |>
    dplyr::filter(tolower(data_format) == data_fmt) |>
    dplyr::select(-c(data_format)) |>
    dplyr::rename(!!paste(data_prefix, df_name, sep = "") := value) |>
    dplyr::mutate(dplyr::across(.cols = -location, as.numeric)) |>
    dplyr::rename_with(.fn = ~ {tolower(.x)}, .cols = dplyr::everything())
}


# Process grouped data (non single age group only)
process_grp <- function(df, vars_to_select, grp_col, rename_grp_chr, data_fmt, data_prefix) {
  df |>
    dplyr::select(dplyr::all_of(vars_to_select)) |>
    dplyr::filter(tolower(data_format) == data_fmt) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::all_of(grp_col), 
      .fns = ~ dplyr::replace_values(
        .x, from = names(rename_grp_chr), 
        to = unname(rename_grp_chr))
    )
    ) |>
    dplyr::mutate(
      value = as.numeric(value),
      !! rlang::sym(grp_col) := factor(!!rlang::sym(grp_col), levels = unique(unname(rename_grp_chr)))
    ) |>
    dplyr::group_by(.data[["location"]], .data[[grp_col]]) |>
    dplyr::summarise(value = sum(value), .groups = "drop_last") |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(
      names_from = dplyr::all_of(grp_col),
      values_from = value,
      names_prefix = data_prefix
    ) |>
    dplyr::rename_with(.fn = ~ tolower(.x), .cols = dplyr::everything())
}


# General dispatch for processing data
process_data <- function(df, df_name, cb, data_fmt) {
  cb_grp_cols <- unique(cb$new_name[cb$group_col])
  
  grp_col <- intersect(names(df), cb_grp_cols)
  grp_col <- if (length(grp_col)) grp_col else NULL
  
  rename_grp_chr <- 
    if (!is.null(grp_col)) {
      cb |>
        dplyr::filter(new_name %in% grp_col) |>
        dplyr::select(values, pivot_name) |>
        dplyr::filter(!is.na(pivot_name)) |>
        tibble::deframe()
    } else {
      NULL
    }
  
  data_fmt <- tolower(data_fmt)
  
  data_prefix <- dplyr::case_when(
    data_fmt == "percent" ~ "perc_",
    data_fmt == "number" ~ "num_",
    grepl("^rate", data_fmt) ~ "rate_"
  )
  
  if (!is.null(rename_grp_chr)) {
    data_prefix <- paste0(data_prefix, df_name, "_")
    
    vars_to_select <- c("location", grp_col, "data_format", "value")
  }
  
  has_format <- data_fmt %in% tolower(unique(df$data_format))
  
  if (!has_format && is.null(grp_col) | !has_format && !is.null(grp_col) &&  grp_col != "s_age_grp") {
    return(NULL)
  } else if (!is.null(grp_col) && grp_col == "s_age_grp") {
    return(process_s_age_group(df, grp_col, rename_grp_chr, data_fmt, data_prefix))
  } else if (!is.null(grp_col) && grp_col != "s_age_grp") {
    return(process_grp(df, vars_to_select, grp_col, rename_grp_chr, data_fmt, data_prefix))
  } else {
    return(process_no_grp(df, df_name, data_fmt, data_prefix))
  } 
}


# Pull state data
pull_loc_data <- function(df,
                          cols_to_extract,
                          dtype,
                          col_pop_grp,
                          recoding_chr,
                          rename_chr,
                          remove_name) {
  df |>
    dplyr::select(dplyr::all_of(c(cols_to_extract))) |>
    dplyr::mutate(dplyr::across(.cols = dplyr::everything(), as.character)) |>
    tibble::rownames_to_column() |>
    tidyr::pivot_longer(-rowname) |>
    dplyr::select(-c(rowname)) |>
    dplyr::mutate(
      percent = dplyr::if_else(value %in% c(NA, 0), NA, as.numeric(value)),
      value = dplyr::case_when(
        value %in% c(0, NA) ~ "N.R.", 
        TRUE ~ sprintf("%.1f%%", as.numeric(value) * 100)
      )
    ) |>
    dplyr::mutate(
      name = gsub(paste0(paste(dtype, col_pop_grp, sep = "_"), "_"), "", name),
      name = dplyr::replace_values(name, from = names(recoding_chr), to = unname(recoding_chr)),
      name = as.character(name),
    ) |>
    dplyr::rename_with(~ rename_chr, c("name", "value", "percent")) |>
    dplyr::select(-if (remove_name == TRUE) any_of("name") else NULL)
}


# Pull data for report
pull_data <- 
  function(df,
           pop,
           grp,
           dtype,
           valid_cols,
           recoding_chr,
           remove_name = FALSE) {
    # normalize data type
    dtype <- dplyr::case_when(
      tolower(dtype) == "percent" ~ "perc",
      tolower(dtype) == "number" ~ "num",
      TRUE ~ dtype
    )
    
    # find columns to extract based on inputs
    col_pop_grp <- if (grepl("^mal_", grp,)) paste(grp,sep = "_") else paste(pop, grp, sep = "_")
    cols_to_extract <- paste(dtype, grep(col_pop_grp, valid_cols, value = TRUE), sep = "_")
    
    # set renaming character vector
    rename_chr <-
      if (pop %in% c("mal", "maltreated")) {
        c("name", "value_mal", "percent_mal")
      } else {
        c("name", "value_pop", "percent_pop")
      }
    
    # return table data
    return(
      pull_loc_data(
        df,
        cols_to_extract,
        dtype,
        col_pop_grp,
        recoding_chr,
        rename_chr,
        remove_name
      )
    )
  }


# Merge maltreatment and population table data
merge_tbl_data <- function(df_mal, df_pop, merge_col = "name") {
  df_mal |>
    dplyr::left_join(df_pop, by = merge_col) |>
    dplyr::mutate(none = "", blank_space = "") |>
    dplyr::relocate(none, .after = percent_mal) |>
    dplyr::mutate(dplyr::across(
      .cols = dplyr::starts_with("value_"),
      ~ dplyr::if_else(is.na(.x), "N.R.", .x)
    ))
}
