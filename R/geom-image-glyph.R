#' @title Add image glyphs on scatter plot
#' @description Each point glyph can be an image (png, jpeg, etc) object.
#' @inheritParams geom_serialaxes_glyph
#' @param images a list of images (a raster object, bitmap image). If not provided, \code{geom_point()} will be executed.
#' @param imagewidth Numerical; width of image
#' @param imageheight Numerical; height of image
#' @param units A character vector specifying the units for the image height and width, see \code{\link{unit}};
#' default is "cm" (force the width and height).
#' @import grid
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
#' @seealso \code{\link{geom_serialaxes_glyph}}, \code{\link{geom_polygon_glyph}}
#'
#' @examples
#' \donttest{
#' # image glyph
#' if(requireNamespace("png")) {
#'
#' img_path <- list.files(file.path(find.package(package = 'ggmulti'),
#'                                  "images"),
#'                        full.names = TRUE)
#' Raptors <- png::readPNG(img_path[1L])
#'
#' p <- ggplot(data = data.frame(x = 0, y = 0),
#'             mapping = aes(x = x, y = y)) +
#'        geom_image_glyph(images = Raptors,
#'                         units = "native",
#'                         imagewidth = 1,
#'                         imageheight = 1)
#' p
#'
#' }
#' }

geom_image_glyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                             position = 'identity', ...,
                             images, imagewidth = 1.2, imageheight = 0.9, units = "cm",
                             na.rm = FALSE, show.legend = NA,
                             inherit.aes = TRUE) {

  if(missing(images) || is.null(images))
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
    geom = GeomImageGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      images = images,
      imagewidth = imagewidth,
      imageheight = imageheight,
      units = units,
      na.rm = na.rm,
      ...
    )
  )
}

GeomImageGlyph <- ggplot2::ggproto('GeomImageGlyph', Geom,
                                   required_aes = c('x', 'y'),
                                   default_aes = aes(colour = NA,
                                                     fill = NA, size = 1,
                                                     linetype = 1, alpha = 1,
                                                     shape = 19, stroke = 0.5),
                                   draw_key = ggplot2::draw_key_rect,
                                   setup_params = function(data, params) {

                                     n <- dim(data)[1]
                                     images <- params$images

                                     params$images <- glyph_input_setup(images, n = n)

                                     params
                                   },
                                   setup_data = function(data, params) {
                                     # store list inside of data
                                     # mainly used for extraction
                                     data$images <- params$images
                                     n <- nrow(data)
                                     data$imagewidth <- rep_len(params$imagewidth, n)
                                     data$imageheight <- rep_len(params$imageheight, n)
                                     data$units <- rep_len(params$units, n)
                                     data
                                   },
                                   draw_panel = function(data, panel_params, coord, images,
                                                         imagewidth = 1.6, imageheight = 1.2, units = "cm",
                                                         na.rm = FALSE) {

                                     data <- coord$transform(data, panel_params)
                                     width_p <- grid::unit(imagewidth * data$size, units)
                                     height_p <- grid::unit(imageheight * data$size, units)

                                     ggname("geom_image_glyph",
                                            grid::gTree(
                                              children = do.call(grid::gList,
                                                                 lapply(1:length(images),
                                                                        function(i) {
                                                                          grid::gList(
                                                                            grid::rectGrob(x = grid::unit(data$x[i], "native"),
                                                                                           y = grid::unit(data$y[i], "native"),
                                                                                           just = "centre",
                                                                                           width = width_p[i] + unit(2, "mm"),
                                                                                           height = height_p[i] + unit(2, "mm"),
                                                                                           gp = grid::gpar(
                                                                                             fill = data$fill[i],
                                                                                             col = data$colour[i],
                                                                                             alpha = data$alpha[i]
                                                                                           )
                                                                            ),
                                                                            grid::rasterGrob(image = if(is.list(images)) images[[i]] else images,
                                                                                             x = grid::unit(data$x[i], "native"),
                                                                                             y = grid::unit(data$y[i], "native"),
                                                                                             just = "centre",
                                                                                             width = width_p[i],
                                                                                             height = height_p[i],
                                                                                             gp = grid::gpar(
                                                                                               alpha = data$alpha[i]
                                                                                             )
                                                                            )
                                                                          )
                                                                        })
                                              )
                                            )
                                     )
                                   }
)
