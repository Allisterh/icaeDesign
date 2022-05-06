#' Get hex codes for colors
#'
#' Contains the hex codes for all the colors that are part of the color scheme.
#'
#' @seealso \code{\link{euf_public_pal}} for the function that assembles the
#'  colors into consistent palettes and \code{\link{scale_color_euf}} for the
#'  application to \code{ggplot2} objects. See \code{\link{icae_public_pal}}
#'  and \code{\link{scale_color_icae}} for the corresponding functions for the
#'  ICAE case.
euf_public_cols <- function(...) {
  cols <- c(...)

  euf_public_colors <- c(
    `blue` = "#00395B",
    `grey` = "#6F6F6F",
    `red` = "#e65032",
    `green` = "#5fb46e",
    `lightblue` = "#69aacd")

  if (is.null(cols))
    return (euf_public_colors)

  return(euf_public_colors[cols])
}

#' Set of EUF-style palettes
#'
#' Assembles colors defined in \code{\link{euf_public_colors}} into palettes.
#'
#' This function takes all the colors of which the hex codes are collected in
#'  \code{\link{euf_public_colors}} and assembles them into consistent palettes
#'  that are all consistent with the corporate design of the EUF
#' @param palette The name of the desired palette.
#' @param reverse If TRUE the colors of the palette are reversed.
#' @return colorRampPalette A function used to create palettes.
#'  See \code{\link[grDevices]{colorRampPalette}}.
#'
#' @seealso \code{\link{euf_public_cols}} for the collection of hex codes for
#'  the ICAE colors that are available. \code{\link{get_euf_colors}} to return
#'  a vector of hex codes for a given ICAE palette.
euf_public_pal <- function(palette = "main", reverse = FALSE, ...) {

  euf_public_palettes <- list(
    `main`  = euf_public_cols("grey", "blue"),

    `cool`  = euf_public_cols("green", "lightblue", "blue"),

    `hot`   = euf_public_cols("grey", "red"),

    `mixed` = euf_public_cols("red", "green", "lightblue", "blue")
  )

  pal <- euf_public_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' Color scale in the EUF color scheme.
#'
#' Creates a color scale in accordance with the official EUF color scheme.
#'
#' These functions can be used to transform the color scheme of a \code{ggplot}
#'  so that it is in accordance with the official EUF color scheme.
#'  The function provides functionality for continuous and discrete color
#'  schemes but has still problems in handling plots with too many different
#'  colors. For the \code{colour} aesthetic use \code{scale_color_euf}, for
#'  the \code{fill} aesthetic use \code{scale_fill_euf}.
#'
#'  The palettes currently available are the following:
#'
#'  \code{main}: "grey", "blue"
#'
#'  \code{cool}: "green", "lightblue", "blue"
#'
#'  \code{hot}: "grey", "red"
#'
#'  \code{mixed}: "blue", "red", "green", "lightblue", "grey"
#'
#'  \code{grey}: "light grey", "dark grey"
#'
#' @param palette The type of palette to be returned. Currently, the follwoing
#'  palettes are supported: \code{main}, \code{cool}, \code{hot}, and
#'  \code{mixed}.
#' @param discrete If TRUE returnes a discrete scheme.
#' @param reverse If TRUE reverses the resulting color scheme.
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'  geom_point(size = 4) +
#'  scale_color_euf("hot")
#'
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#'  geom_point(size = 4, alpha = .6) +
#'  scale_color_euf(discrete = FALSE, palette = "cool")
#'
#' @family color scheme functions
#' @seealso The package was loosely built upon the explanations in
#' \url{https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2}.
#' @name coloring
NULL

#' @rdname coloring
#' @export
scale_color_euf <- function(palette = "main",
                            discrete = TRUE, reverse = FALSE, ...) {
  pal <- euf_public_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour",
                            paste0("euf_public_", palette),
                            palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @rdname coloring
#' @export
scale_fill_euf <- function(palette = "main",
                           discrete = TRUE, reverse = FALSE, ...) {
  pal <- euf_public_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill",
                            paste0("euf_public_", palette),
                            palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' Get EUF colors
#'
#' Returns a vector of colors of a chose EUF-color palette.
#'
#' This function takes one of the palettes defined in
#'  \code{\link{euf_public_pal}} and returns a vector with the requested
#'  number of colors. To return all base colors of the EUF color scheme
#'  call the function with \code{col_name="all"}.
#'
#' @param n The number of different colors requested.
#' @param col_name The name of a color as defined in
#'  \code{\link{euf_public_cols}}. If 'all' returns all EUF colors.
#' @param palette_used The type of palette to be returned. Currently, the
#'  following palettes are supported: \code{main}, \code{cool}, \code{hot},
#'  and  \code{mixed}.
#' @param reverse_pal If TRUE reverses the resulting color scheme.
#' @return A vector of hex codes with colors from \code{palette}
#'  (if \code{col_name} is \code{FALSE}), a named vector of hex codes from
#'  the colors specified in \code{col_name} otherwise.
#'
#' @examples
#' get_euf_colors(3)
#'
#' get_euf_colors(2, palette_used = "mixed", reverse_pal = TRUE)
#'
#' get_euf_colors("lightblue")
#'
#' get_euf_colors(c("blue", "red"))
#'
#' get_euf_colors(col_name="all")
#'
#' @family color scheme functions
#' @seealso \code{\link{euf_public_pal}} for the function that assembles the
#'  colors into consistent palettes and \code{\link{scale_color_euf}} for the
#'  application to \code{ggplot2} objects. See \code{\link{icae_public_pal}}
#'  and \code{\link{scale_color_euf}} for the ICAE pendants.
#' @export
get_euf_colors <- function(n=1, col_name=NULL,
                           palette_used="main", reverse_pal=FALSE, ...){
  if (is.character(n)){
    col_name <- n
    n <- 1
    if (col_name[1]=="all"){
      return(euf_public_cols())
    }
  }
  if (!is.null(col_name)){
    if (n!=1){
      warning("If col_name is given, values for n are ignored. For several
              colors from an EUF palette set col_name to FALSE")
    }
    col_vector <- euf_public_cols(col_name)
  } else{
    col_vector <- euf_public_pal(palette = palette_used,
                                 reverse = reverse_pal)(n)
  }
  return(col_vector)
}
