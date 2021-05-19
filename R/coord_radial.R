#' @title Radial axes
#' @description A radar (spider) coordinate. A wrapper of the function \code{coord_polar()} by
#' forcing it linear.
#' @inheritParams ggplot2::coord_polar
#' @export
#' @examples
#' ggplot(iris, mapping = aes(colour = Species)) +
#'   geom_serialaxes(axes.sequence = c(colnames(iris), colnames(iris)[1])) +
#'   coord_radar()
#'
coord_radial <- function (theta = "x", start = 0, direction = 1, clip = "on") {

  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"

  ggproto(NULL,
          ggplot2::CoordPolar,
          theta = theta,
          r = r,
          start = start,
          direction = sign(direction),
          clip = clip,
          is_linear = function(coord) TRUE)
}
