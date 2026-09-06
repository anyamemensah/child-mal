### Script to render index.qmd + index.html for main report page
# Author: Ama Nyame-Mensah, Ph.D.
# Last updated: 2026-09-06

render_index <- function(report_year, scripts_dir = "./_scripts", data_dir = "./_data") {
  
  # Source render_yml.R
  source(file.path(scripts_dir, "render_yml.R"), local = TRUE, echo = FALSE)
  
  # Check if index.qmd already exists
  if (file.exists("index.qmd")) {
    warning("index.qmd is about to overwritten...", immediate. = TRUE)
  }
  
  all_inputs <-
    readr::read_csv(file.path(data_dir, "src_hex_data.csv"), show_col_types = FALSE) |>
    dplyr::mutate(output_file = paste0(tolower(abbrev), "_factsheet.html"))
  
  # inputs for rendering individual reports
  report_inputs <-
    all_inputs |>
    dplyr::select(-c(abbrev:State))
  
  # resources section
  resources <-
    all_inputs |>
    dplyr::pull(output_file) |> (\(x)
    paste0("  - './docs/", x, "'")) () |>
    paste(collapse = "\n")
  
  # dropdown options
  dropdown_options <-
    all_inputs |>
    dplyr::transmute(option = paste0(
      "  <option value='./docs/", 
      output_file, "'>", 
      State, "</option>")
    ) |>
    dplyr::pull(option) |>
    paste(collapse = "\n")
  
  # create index.qmd
  index_template <- "---
title: ''
resources:
__RESOURCES__
format:
  html:
    embed-resources: true
    css: resources/styles.css
    theme: cosmo
    toc: true
    page-layout: full
---

<br>
<div class='index-title-wrapper'>
<span class='state-h1'>__YEAR__ State-Level Profiles</span>
<h1 class='cmv-h1'>Child Maltreatment Victims</h1>
</div>

<p>As state officials work to improve the health and well-being of children, data on child maltreatment victims can play a key role in supporting their efforts. This resource provides a comparison of state-level demographic data from __YEAR__ on child maltreatment victims across various dimensions, including gender, race/ethnicity, age, and type of maltreatment. For more information on child maltreatment, visit the [Children's Bureau's Child Maltreatment page](https://www.acf.hhs.gov/cb/data-research/child-maltreatment) from the Office of the Administration for Children and Families. The data for this report was sourced from the [Kids Count Data Center](https://datacenter.kidscount.org/data).

Use the dropdown menu below to select and view a state report:</p>

<select id='htmlDropdown' name='htmlDropdown' onchange='navigateToPage()'>
  <option value=''>--Select a State--</option>
__DROPDOWN_OPTIONS__
</select>

<script>
function navigateToPage() {
  var dropdown = document.getElementById('htmlDropdown');
  var page = dropdown.value;

  if (page) {
    window.location.href = page;
  }
}
</script>
"
  
  # replace placeholders with dynamic values
  index_qmd <-
    index_template |>
    stringr::str_replace("__RESOURCES__", resources) |>
    stringr::str_replace("__DROPDOWN_OPTIONS__", dropdown_options) |>
    stringr::str_replace_all("__YEAR__", as.character(report_year)) # converting report_year for injection
  
  # write index.qmd to directory
  writeLines(index_qmd, "index.qmd")
  
  # wait a few seconds and then render qmd to html
  Sys.sleep(3)
  
  render_quarto_yml(output_dir = ".", proj_dir = ".")
  
  quarto::quarto_render(
    input = "index.qmd",
    output_file = "index.html",
    quiet = TRUE
  )
  
  delete_quarto_yml(proj_dir = ".")
  
  message("index.qmd successfully exported and index.html successfully rendered.")
}
