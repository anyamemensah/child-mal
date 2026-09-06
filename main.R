### Generating the main landing page and report pages
# Process data for a given report year
process_data(
  report_year = 2023,
  scripts_dir = "./_scripts",
  data_dir = "./_data"
)

### Render index.qmd and index.html
render_index(
  report_year = 2023, 
  scripts_dir = "./_scripts",
  data_dir = "./_data"
)

# Render the report pages
render_reports(
  report_year = 2023,
  scripts_dir = "./_scripts",
  data_dir = "./_data",
  input_qmd = "fact_sheets.qmd"
)
