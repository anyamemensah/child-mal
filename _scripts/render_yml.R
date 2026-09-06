# Render yml
render_quarto_yml <- function(output_dir = ".", proj_dir = ".") {
  
  if (file.exists(file.path(proj_dir, "_quarto.yml"))) {
    delete_quarto_yml(proj_dir)
  }

  config <- list(
    project = list(
      type = "default",
      "output-dir" = output_dir
    )
  )
  
  yaml::write_yaml(config, file = file.path(proj_dir, "_quarto.yml"))
}

# Delete yml
delete_quarto_yml <- function(proj_dir = ".") {
  invisible(file.remove(file.path(proj_dir, "_quarto.yml")))
}