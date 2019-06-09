#' A ggplot theme for the icaeDesign package
#'
#' A theme summarizing the typical adjustments made to ggplots.
#'
#' This theme is not specifically tuned to the ICAE color scheme, but
#'  summarizes typical modifications that improve readability of a
#'  ggplot. It is in principle suitable for all kinds of plots and has
#'  a decent touch of blue/grey colors for lines and fonts.
#'
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'  geom_point(size = 4) +
#'  scale_color_icae("hot") +
#'  theme_icae()
#' @family ggplot themes
theme_icae <- function(base_size = 11,
                       base_family = "",
                       base_line_size = base_size / 170,
                       base_rect_size = base_size / 170){
  theme_minimal(base_size = base_size,
                base_family = base_family,
                base_line_size = base_line_size) %+replace%
    theme(
      axis.line = element_line(
        color = rgb(188, 197, 207, maxColorValue = 255), #188, 197, 207
        linetype = "solid", size = 0.5
        ),
      legend.position = "bottom",
      legend.spacing.x = unit(0.2, "cm"),
      legend.title = element_blank(),
      plot.title = element_text(
        color = rgb(43, 49, 62, maxColorValue = 255),
        # face = "bold",
        hjust = 0.5
        ),
      axis.title = element_text(
        color = rgb(23, 56, 92, maxColorValue = 255),
        size = rel(0.75)
        ),
      axis.text = element_text(
        color = rgb(110, 113, 123, maxColorValue = 255),
        size = rel(0.5)
        ),
      panel.grid.major = element_line(
        rgb(188, 197, 207, maxColorValue = 255),#139, 155, 173
        linetype = "solid"),
      panel.grid.minor = element_line(
        rgb(233, 234, 233, maxColorValue = 255), #188, 197, 207
        linetype = "dotted",
        size = rel(4)
        ),
      complete = TRUE
    )
}
