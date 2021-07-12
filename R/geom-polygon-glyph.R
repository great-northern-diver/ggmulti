#' @title Add polygon glyphs on scatter plot
#' @description Each point glyph can be a polygon object.
#' We provide some common polygon coords in \code{\link{polygon_glyph}}. Also, users can
#' customize their own polygons.
#' @inheritParams geom_serialaxes_glyph
#' @param polygon_x nested list of x-coordinates of polygons, one list element for each scatterplot point.
#' If not provided, a point visual (\code{geom_point()}) will be displayed.
#' @param polygon_y nested list of y-coordinates of polygons, one list element for each scatterplot point.
#' If not provided, a point visual (\code{geom_point()}) will be displayed.
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
#' \item{size}
#' \item{linetype}
#' \item{shape}
#' \item{stroke}
#' }
#'
#' The size unit is \code{cm}
#'
#' Note that the shape and stroke do not have real meanings unless the essential
#' argument  \code{polygon_x} or \code{polygon_y} is missing.
#' If so, a point visual will be displayed with corresponding shape and stroke.
#'
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

#' @rdname Geom-ggproto
#' @export
GeomPolygonGlyph <- ggplot2::ggproto('GeomPolygonGlyph',
                                     ggplot2::Geom,
                                     required_aes = c('x', 'y'),
                                     default_aes = ggplot2::aes(colour = 'black',
                                                                fill = 'black', size = 0.5,
                                                                linetype = 1, alpha = 1,
                                                                shape = 21, stroke = 0.5),
                                     draw_key = function (data, params, size) {
                                       data$size <- ggplot2::GeomPoint$default_aes$size/0.5 * data$size
                                       ggplot2::draw_key_point(data, params, size)
                                     },
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

                                       # fill is NA --> polyline
                                       # fill is not NA --> polygon
                                       polyline <- is.na(fill)
                                       polygon <- !is.na(fill)

                                       if(sum(polyline) > 0) {

                                         poly_x <- poly_coord(polygon_x[polyline], data[polyline, ],
                                                              orientation = "x",
                                                              show.area = FALSE)
                                         poly_y <- poly_coord(polygon_y[polyline], data[polyline, ],
                                                              orientation = "y",
                                                              show.area = FALSE)

                                         linegrob <- grid::polylineGrob(
                                           x = do.call(grid::unit.c, poly_x),
                                           y = do.call(grid::unit.c, poly_y),
                                           id = rep(seq(length(poly_x)), lengths(poly_x)),
                                           gp = grid::gpar(
                                             col =  data$colour[polyline],
                                             lwd = data$linewidth[polyline],
                                             alpha = data$alpha[polyline]
                                           )
                                         )
                                       } else {linegrob <- grid::grob()}


                                       if(sum(polygon) > 0) {

                                         poly_x <- poly_coord(polygon_x[polygon], data[polygon, ],
                                                              orientation = "x",
                                                              show.area = TRUE)
                                         poly_y <- poly_coord(polygon_y[polygon], data[polygon, ],
                                                              orientation = "y",
                                                              show.area = TRUE)

                                         gongrob <- grid::polygonGrob(
                                           x = do.call(grid::unit.c, poly_x),
                                           y = do.call(grid::unit.c, poly_y),
                                           id = rep(seq(length(poly_x)), lengths(poly_x)),
                                           gp = grid::gpar(
                                             fill = fill[polygon],
                                             col =  data$colour[polygon],
                                             lwd = data$linewidth[polygon],
                                             alpha = data$alpha[polygon]
                                           )
                                         )
                                       } else {gongrob <- grid::grob()}


                                       ggname("geom_polygon_glyph",
                                              grid::gTree(
                                                children = grid::gList(
                                                  linegrob,
                                                  gongrob
                                                )
                                              )
                                       )
                                     }
)

poly_coord <- function(poly, data, orientation = "x", show.area = FALSE, unit = "cm") {

  n <- dim(data)[1]

  lapply(seq_len(n),
         function(i) {
           if(show.area) {
             grid::unit(data[[orientation]][i], 'native') +
               grid::unit(poly[[i]] * data$size[i], unit)
           } else {
             grid::unit(data[[orientation]][i], 'native') +
               grid::unit(c(poly[[i]], poly[[i]][1]) * data$size[i], unit)
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
      stop("The length of ", deparse(substitute(x)),
           " must be either length 1 or the same as the data ", n)
    }
  }

  return(x)
}
