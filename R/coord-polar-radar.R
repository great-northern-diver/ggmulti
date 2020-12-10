############## Inspired by Daniel Strengejacke
#' @inherit ggplot2::coord_polar
#' @param is_linear Returns \code{TRUE} if the coordinate system is linear; \code{FALSE} otherwise.
#' Mainly used for radial axes
#' @export
coord_polar <- function (theta = "x", start = 0, direction = 1, clip = "on", is_linear = FALSE) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto(NULL,
          ggplot2::CoordPolar,
          theta = theta,
          r = r,
          start = start,
          direction = sign(direction),
          clip = clip,
          is_linear = function(coord) is_linear)
}

#' @title Radar axes
#' @description A radar (spider) coordinate.
#' @inheritParams ggplot2::coord_polar
#' @export
#' @examples
#' ggplot(iris, mapping = aes(colour = Species)) +
#'   geom_serialaxes(axes.sequence = c(colnames(iris), colnames(iris)[1])) +
#'   coord_radar()
coord_radar <- function (theta = "x", start = 0, direction = 1, clip = "on") {
  coord_polar(theta = theta, start = start, direction = direction,
              clip = clip, is_linear = TRUE)
}
