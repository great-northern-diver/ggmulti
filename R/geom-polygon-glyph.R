#' @title Add polygon glyphs on scatter plot
#' @description Each point glyph can be a polygon object.
#' Available polygons coords can be achieved in \code{\link{polygon_glyph}}
#' @inheritParams geom_serialaxes_glyph
#' @param polygon_x nested list of x-coordinates of polygons, one list element for each scatterplot point.
#' If not provided, \code{geom_point()} will be executed.
#' @param polygon_y nested list of y-coordinates of polygons, one list element for each scatterplot point.
#' If not provided, \code{geom_point()} will be executed.
#' @export
#'
#' @section Aesthetics:
#' geom_..._glyph() understands the following aesthetics (required aesthetics are in bold):
#' \itemize{
#' \item{\strong{x}}
#' \item{\strong{y}}
#' \item{alpha}
#' \item{colour}
#' \item{fill}
#' \item{group}
#' \item{shape}
#' \item{size}
#' \item{stroke}
#' \item{linetype}
#' }
#'
#' @return a \code{geom} layer
#' @seealso \code{\link{geom_serialaxes_glyph}}, \code{\link{geom_image_glyph}}
#'
#' @examples
#' # polygon glyph
#' p <- ggplot(data = data.frame(x = 1:4, y = 1:4),
#'             mapping = aes(x = x, y = y)) +
#'   geom_polygon_glyph(polygon_x = list(x_star, x_cross, x_hexagon, x_airplane),
#'                      polygon_y = list(y_star, y_cross, y_hexagon, y_airplane),
#'                      colour = 'black', fill = 'red')
#' p
#'
#' # the coords of each polygons can be achieved by calling function `ggplot_build`
#' build <- ggplot2::ggplot_build(p)
#' polygon_x <- build$data[[1]]$polygon_x
#' polygon_y <- build$data[[1]]$polygon_y
#'
geom_polygon_glyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                               position = 'identity', ...,
                               polygon_x, polygon_y, linewidth = 1,
                               na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE) {
  if(missing(polygon_x) || missing(polygon_y) || is.null(polygon_x) || is.null(polygon_y))
    return(
      ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = ggplot2::GeomPoint,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
          na.rm = na.rm,
          ...
        )
      )
    )

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPolygonGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      polygon_x = polygon_x,
      polygon_y = polygon_y,
      linewidth = linewidth,
      na.rm = na.rm,
      ...
    )
  )
}

GeomPolygonGlyph <- ggplot2::ggproto('GeomPolygonGlyph',
                                     ggplot2::Geom,
                                     required_aes = c('x', 'y'),
                                     default_aes = ggplot2::aes(colour = 'black',
                                                                fill = 'black', size = 1,
                                                                linetype = 1, alpha = 1,
                                                                shape = 19, stroke = 0.5),
                                     draw_key = ggplot2::draw_key_polygon,
                                     setup_params = function(data, params) {

                                       n <- dim(data)[1]
                                       polygon_x <- params$polygon_x
                                       polygon_y <- params$polygon_y

                                       params$polygon_x <- glyph_input_setup(polygon_x, n = n)
                                       params$polygon_y <- glyph_input_setup(polygon_y, n = n)

                                       params
                                     },
                                     setup_data = function(data, params) {
                                       # store list inside of data
                                       # mainly used for extraction
                                       data$polygon_x <- params$polygon_x
                                       data$polygon_y <- params$polygon_y
                                       data$linewidth <- rep_len(params$linewidth, dim(data)[1])
                                       data
                                     },
                                     draw_panel = function(data, panel_params, coord,
                                                           polygon_x, polygon_y, linewidth = 1, na.rm = FALSE) {

                                       data <- coord$transform(data, panel_params)
                                       n <- dim(data)[1]
                                       fill <- data$fill

                                       show.area <- !any(is.na(fill))
                                       poly_x <- poly_coord(polygon_x, data,
                                                            orientation = "x",
                                                            show.area = show.area)
                                       poly_y <- poly_coord(polygon_y, data,
                                                            orientation = "y",
                                                            show.area = show.area)

                                       fun <- if(show.area) {
                                         grid::polygonGrob
                                       } else {
                                         grid::polylineGrob
                                       }

                                       ggname("geom_polygon_glyph",
                                              fun(
                                                x = do.call(grid::unit.c, poly_x),
                                                y = do.call(grid::unit.c, poly_y),
                                                id = rep(seq(length(poly_x)), lengths(poly_x)),
                                                gp = grid::gpar(
                                                  fill = fill,
                                                  col =  data$colour,
                                                  lwd = data$linewidth,
                                                  alpha = data$alpha
                                                )
                                              )
                                       )
                                     }
)

poly_coord <- function(poly, data, orientation = "x", show.area = FALSE) {

  n <- dim(data)[1]

  lapply(seq_len(n),
         function(i) {
           if(show.area) {
             grid::unit(data[[orientation]][i], 'native') +
               grid::unit(poly[[i]] * as_r_polygonGlyph_size(data$size[i]), "mm")
           } else {
             grid::unit(data[[orientation]][i], 'native') +
               grid::unit(c(poly[[i]], poly[[i]][1]) * as_r_polygonGlyph_size(data$size[i]), "mm")
           }
         })
}

glyph_input_setup <- function(x, n = integer(1L)) {

  if(is.atomic(x)) {
    x <- lapply(1:n, function(i) x)
  } else {
    if(length(x) == 1) {
      x <- rep(x, n)
    } else if(length(x) == n) {
      NULL
    } else {
      rlang::abort(
        paste("The length of", deparse(substitute(x)),
              "must be either length 1 or the same as the data", "n")
      )
    }
  }

  return(x)
}
