### Functions to generate map + plots
# Author: Ama Nyame-Mensah, Ph.D.
# Last updated: 2026-09-01

# render hex map
render_hex_map <- function(params) {
  params$hex_map_data |>
    ggplot2::ggplot(ggplot2::aes(
      x = column,
      y = row,
      fill = State == params$state)
    ) +
    ggstar::geom_star(starshape = "hexagon", size = 15) +
    ggplot2::geom_text(
      ggplot2::aes(label = abbrev),
      color = "#FFFFFF",
      vjust = 0.5,
      hjust = 0.5,
      fontface = "bold",
      size = 5,
      family = "Helvetica Neue"
    ) +
    ggplot2::scale_fill_manual(values = c(`TRUE` = "#7030A0", `FALSE` = "#A6A6A6"),guide = "none") +
    ggplot2::coord_cartesian(xlim = c(0, 12), ylim = c(-0.5, 8.5)) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"))
  
}


# render table embedded with bar charts
render_viz_table <- function(df, child_pop) {
  df |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::starts_with("percent"), 
        ~ dplyr::coalesce(.x, 0) * 100
      )
    ) |>
    gt::gt() |>
    gt::cols_width(1 ~ gt::pct(20),
                   4 ~ gt::pct(10),
                   c(2, 5) ~ gt::pct(5),
                   c(3, 6) ~ gt::pct(45),
                   c(7) ~ gt::pct(20)) |>
    gt::tab_spanner(
      label = ifelse(child_pop == "state", "State Child population", "USA Child Population"),
      columns = dplyr::ends_with("pop")) |>
    gt::tab_spanner(
      label = "Maltreatment Victims",
      columns = dplyr::ends_with("mal")) |>
    gt::tab_options(heading.border.bottom.style = "hidden",
                    table.border.top.style = "hidden", 
                    table.border.bottom.style = "hidden",
                    table.border.bottom.color = "#FFFFFF",
                    table.font.names = "Helvetica Neue",
                    table.font.color = "#262626",
                    table.font.size = 18,
                    column_labels.background.color = "#FFFFFF",
                    column_labels.border.top.style = "hidden",
                    column_labels.border.bottom.style = "hidden") |>
    gt::tab_style(style = list(gt::cell_borders(sides = "all", color = "#FFFFFF",
                                                weight = "0.5px")),
                  locations = list(gt::cells_body(), gt::cells_column_labels())) |>
    gt::tab_style(style = gt::cell_text(weight = "bold", size = gt::px(18), align = "left",
                                        color = "#262626"),
                  locations = list(gt::cells_column_spanners(gt::matches("Population")))) |>
    gt::tab_style(style = gt::cell_text(weight = "bold", size = gt::px(18), align = "left",
                                        color = "#7030A0"),
                  locations = list(gt::cells_column_spanners(gt::matches("Maltreatment")))) |>
    gt::tab_style(style = list(gt::cell_text(align = "left", weight = "normal")),
                  locations = gt::cells_body(columns = 1, rows = gt::everything())) |>
    gt::tab_style(style = list(gt::cell_text(align = "left", weight = "bold", 
                                             color = "#7030A0")),
                  locations = gt::cells_body(columns = 2, rows = gt::everything())) |>
    gt::tab_style(style = list(gt::cell_text(align = "left", weight = "bold", 
                                             color = "#616161")),
                  locations = gt::cells_body(columns = 5, rows = gt::everything())) |>
    gt::cols_label(name = " ",
                   value_mal = "  ",
                   percent_mal = "   ",
                   none = "    ",
                   value_pop = "     ",
                   percent_pop = "      ",
                   blank_space = "       ") |>
    gtExtras::gt_plt_bar_pct(column = 3,height = 25, scaled = TRUE,
                             fill = "#7030A0", background = "#F2F2F2") |>
    gtExtras::gt_plt_bar_pct(column = 6,height = 25, scaled = TRUE,
                             fill = "#808080", background = "#F2F2F2") |>
    gt::tab_options(quarto.disable_processing = TRUE) |>
    gt::fmt_markdown(columns = 1)
}
