
#' theme_mwk
#'
#' My ggplot2 theme
#'
#' @param base_size Base font size
#' @param base_family Font family.
#' @param dark Primary forefront color.
#' @param light Primary background color.
#' @param gray Gray color.
#' @return My ggplot2 theme (similar to theme_minimal/theme_bw)
#' @export
theme_mwk <- function(base_size = 12,
                      base_family = "Avenir Next LT Pro",
                      dark = "#24292e",
                      light = "#ffffff",
                      gray = "#ededed",
                      caption = TRUE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("must have ggplot2 installed to use theme_mwk")
  }
  if (caption) {
    caption <- ggplot2::element_text(
      hjust = -ggplot2::rel(0.01), vjust = 0,margin = ggplot2::margin(0, 0, 0.5, 0.25, "lines"),
      size = ggplot2::rel(.6), color = "#808c9fee", family = "Font Awesome 5 Brands")
  } else {
    caption <- ggplot2::element_text(
      face = "italic", size = ggplot2::rel(.75))
  }
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(1.05)),
      legend.key = ggplot2::element_rect(fill = light),
      legend.background = ggplot2::element_rect(
        fill = light, colour = light),
      legend.text = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(1.0)),
      plot.background = ggplot2::element_rect(
        fill = light, colour = light),
      plot.caption = caption,
      panel.background = ggplot2::element_rect(
        fill = light, colour = light),
      panel.border = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        face = "bold", colour = dark, size = ggplot2::rel(1.6)),
      plot.subtitle = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(1.05)),
      text = ggplot2::element_text(colour = dark),
      strip.background = ggplot2::element_rect(
        fill = gray, colour = dark),
      axis.text = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(.80)),
      axis.title = ggplot2::element_text(
        colour = dark, size = ggplot2::rel(.95), hjust = .95, face = "italic"),
      panel.grid = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(
        color = "#111111aa", size = ggplot2::rel(.060)),
      panel.grid.minor.x = ggplot2::element_line(
        color = "#111111aa", size = ggplot2::rel(.040)),
      panel.grid.major.y = ggplot2::element_line(
        color = "#111111aa", size = ggplot2::rel(.060)),
      panel.grid.minor.y = ggplot2::element_line(
        color = "#111111aa", size = ggplot2::rel(.040)),
      axis.ticks = ggplot2::element_blank()
    )
}

#' @export
#' @param caption Text to use as source caption.
#' @rdname theme_mwk
#' @export
theme_mwk_caption_text <- function() {
  #paste0(captext$rstats_dataviz, substr(Sys.Date(), 1, 4),
  #  captext$mkearney_kearneymw)
  caption_text <- paste0(    "github𝗆𝗄𝖾𝖺𝗋𝗇𝖾𝗒 twitter𝗄𝖾𝖺𝗋𝗇𝖾𝗒𝗆𝗐")
  caption_text
}
