theme_nkjp <- function(base_size = 14, base_family = "ubuntu", xgrid = FALSE, ygrid = TRUE, fallback_google = TRUE) {
  showtext::showtext_auto(enable = TRUE)
  loaded_fonts <- sysfonts::font_families()
  out <- ggplot2::theme_light(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(vjust = 1, size = ggplot2::rel(0.7), color = "gray30", margin = ggplot2::margin(12, 0, 0, 0)),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "gray85"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(face = "bold", colour = "gray20", size = ggplot2::rel(0.8)),
      axis.text = ggplot2::element_text(color = "gray30", size = ggplot2::rel(0.8)),
      plot.title = ggplot2::element_text(face = "bold", colour = "gray10"),
      plot.subtitle = ggplot2::element_text(color = "gray20"),
      panel.background = ggplot2::element_rect(fill = "gray100"),
      panel.border = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "gray95"),
      strip.text = ggplot2::element_text(color = "gray20"))
  return(out)
}
