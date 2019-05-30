# This file creates two functions to be used within ggplot
# They allow access a number of color schemes
# currently, the following color schemes are there:
#
  # `main`  = icae_public_cols("sand", "purple", "dark red"),
  # `cool`  = icae_public_cols("purple", "dark green", "dark blue"),
  # `hot`   = icae_public_cols("sand", "dark red"),
  # `mixed` = icae_public_cols("orange", "dark blue", "purple", "sand", "dark red"),
  # `grey`  = icae_public_cols("light grey", "dark grey")
#
# For line plots, the mixed palette is best
#
# The functions defined for use in ggplot so far are:
  # scale_color_icae_public(palette = "main", discrete = TRUE, reverse = FALSE, ...)
  #  scale_fill_icae_public(palette = "main", discrete = TRUE, reverse = FALSE, ...)
#
# Dependencies: ggplot2
#
# Based on:
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2

icae_public_colors <- c(
  `orange` = "#ff9900",
  `purple` = "#8600b3",
  `dark green` = "#006600",
  `sand` = "#d8c469",
  `dark blue` = "#002b80",
  `dark red` = "#800000")

icae_public_cols <- function(...) {  # Extracts hex codes
  cols <- c(...)

  if (is.null(cols))
    return (icae_public_colors)

  icae_public_colors[cols]
}

icae_public_cols()

icae_public_palettes <- list(# Links the colors above to palettes
  `main`  = icae_public_cols("dark green", "sand", "purple"),

  `cool`  = icae_public_cols("purple", "dark green", "dark blue"),

  `hot`   = icae_public_cols("sand", "dark red"),

  `mixed` = icae_public_cols("dark green", "orange", "dark blue", "purple", "sand", "dark red"),

  `grey`  = icae_public_cols("light grey", "dark grey")
)

icae_public_pal <- function(palette = "main", reverse = FALSE, ...) {
  # extrapolates the palletes
  pal <- icae_public_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale in the ICAE color scheme.
#'
#' Creates a color scale in accordance with the official ICAE color scheme.
#'
#' These functions can be used to transform the color scheme of a \code{ggplot}
#'  so that it is in accordance with the official ICAE color scheme.
#'  The function provides functionality for continuous and discrete color
#'  schemes but has still problems in handling plots with too many different
#'  colors. For the \code{colour} aesthetic use \code{scale_color_icae}, for
#'  the \code{fill} aesthtetic use \code{scale_fill_icae}.
#'
#' @param palette The type of palette to be returned. Currently, the follwoing
#'  palettes are supported: \code{main}, \code{cool}, \code{hot}, \code{mixed},
#'  and  \code{grey}.
#' @param discrete If TRUE returnes a discrete scheme.
#' @param reverse If TRUE reverses the resulting color scheme.
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'  geom_point(size = 4) +
#'  scale_color_icae("hot")
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'  geom_point(size = 4, alpha = .6) +
#'  scale_color_icae(discrete = FALSE, palette = "cool")
#' @family color scheme functions
#' @name coloring
NULL

#' @rdname coloring
scale_color_icae <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- icae_public_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("icae_public_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @rdname coloring
#' @rdname coloring
#' @rdname coloring
scale_fill_icae <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- icae_public_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("icae_public_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}
