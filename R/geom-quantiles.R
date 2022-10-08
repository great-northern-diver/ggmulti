#' @title Add quantile layers on serial axes coordinate
#' @description In \code{ggplot2}, \code{geom_quantile()} is used to fit a quantile regression to the data and draws
#' the fitted quantiles with lines. However, \code{geom_quantiles()} is mainly used to draw quantile lines
#' on serial axes. See examples
#' @inheritParams ggplot2::geom_quantile
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @export
#' @seealso \code{\link{geom_serialaxes_quantile}}
#' @examples
#' p <- ggplot(iris,
#'             mapping = aes(
#'               Sepal.Length = Sepal.Length,
#'               Sepal.Width = Sepal.Width,
#'               Petal.Length = Petal.Length,
#'               Petal.Width = Petal.Width
#'             )
#'   ) +
#'   geom_path(alpha = 0.2)  +
#'   coord_serialaxes(scaling = "variable")
#' p + geom_quantiles(colour = c("red", "green", "blue"),
#'                    quantiles = c(0.25, 0.5, 0.75),
#'                    size = 2)
#'
geom_quantiles <- function (mapping = NULL, data = NULL, stat = "quantile",
                            position = "identity", ..., lineend = "butt",
                            linejoin = "round", linemitre = 10, na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat,
                 geom = GeomQuantiles,
                 position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(lineend = lineend, linejoin = linejoin,
                               linemitre = linemitre, na.rm = na.rm, ...)
  )
}

#' @rdname Geom-ggproto
#' @export
GeomQuantiles <- ggplot2::ggproto(
  "GeomQuantiles",
  ggplot2::GeomQuantile
)
