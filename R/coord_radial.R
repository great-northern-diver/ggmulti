#' @title Radial axes
#' @description A radial (spider) coordinate. A wrapper of the function \code{coord_polar()} by
#' forcing it linear.
#'
#' @details The serial histogram and serial density cannot be applied on
#' a radial coordinate yet.
#' @inheritParams ggplot2::coord_polar
#' @export
#' @examples
#' if(require("dplyr")) {
#' ggplot(NBAstats2021, mapping = aes(colour = Playoff)) +
#'   geom_serialaxes(
#'     axes.sequence = c("PTS", "OPTS", "3PM", "O3PM", "PTS"),
#'       scaling = "variable"
#'     ) +
#'   coord_radial() +
#'   scale_x_continuous(
#'     breaks = 1:5,
#'     labels = c("Points",
#'                "Oppo Points",
#'                "3P Made",
#'                "Oppo 3P Made",
#'                "Points Per Game")) +
#'   scale_y_continuous(labels = NULL) +
#'   facet_wrap(~CONF)
#'   }
coord_radial <- function (theta = "x", start = 0, direction = 1, clip = "on") {

  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"

  ggplot2::ggproto(NULL,
                   ggplot2::CoordPolar,
                   theta = theta,
                   r = r,
                   start = start,
                   direction = sign(direction),
                   clip = clip,
                   is_linear = function(coord) TRUE)
}
