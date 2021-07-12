#' @title Add image glyphs on scatter plot
#' @description Each point glyph can be an image (png, jpeg, etc) object.
#' @inheritParams geom_serialaxes_glyph
#' @param images a list of images (a raster object, bitmap image). If not provided, a point visual (\code{geom_point()}) will be displayed.
#' @param imagewidth Numerical; width of image
#' @param imageheight Numerical; height of image
#' @param interpolate A logical value indicating whether to linearly interpolate the image (the alternative is to use nearest-neighbour interpolation,
#' which gives a more blocky result). See \code{\link{rasterGrob}}.
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
#' \item{size}
#' \item{linetype}
#' \item{shape}
#' \item{stroke}
#' }
#'
#' The size unit is \code{cm}
#'
#' Note that the shape and stroke do not have real meanings unless the essential
#' argument \code{images} is missing. If so, a point visual will be displayed with
#' corresponding shape and stroke.
#'
#' @return a \code{geom} layer
#' @seealso \code{\link{geom_serialaxes_glyph}}, \code{\link{geom_polygon_glyph}}
#'
#' @examples
#' \donttest{
#' # image glyph
#' if(require("png")) {
#' img_path <- list.files(file.path(find.package(package = 'ggmulti'),
#'                                  "images"),
#'                        full.names = TRUE)
#' Raptors <- png::readPNG(img_path[2L])
#' Warriors <- png::readPNG(img_path[3L])
#'
#' pg <- ggplot(data = data.frame(x = 1:2, y = rep(1, 2)),
#'        mapping = aes(x = x, y = y)) +
#'   geom_image_glyph(images = list(Raptors,
#'                                  Warriors),
#'                    imagewidth = rep(1.2, 2),
#'                    imageheight = c(0.9, 1.2)) +
#'   coord_cartesian(xlim = extendrange(c(1,2)))
#' pg
#' # query the images (a numerical array)
#' build <- ggplot2::ggplot_build(pg)
#' # `imageRaptors` and `imageWarriors` are three dimensional
#' # arrays (third dimension specifying the plane)
#' imageRaptors <- build$data[[1]]$images[[1]]
#' imageWarriors <- build$data[[1]]$images[[2]]
#'
#' if(require("grid")) {
#' grid.newpage()
#' grid.raster(imageRaptors)
#' grid.newpage()
#' grid.raster(imageWarriors)
#' }
#'
#' # THIS IS SLOW
#' mercLogo <- png::readPNG(img_path[1L])
#'
#' p <- ggplot(mapping = aes(x = hp, y = mpg)) +
#'        geom_point(
#'          data = mtcars[!grepl("Merc", rownames(mtcars)), ],
#'          color = "skyblue") +
#'        geom_image_glyph(
#'          data = mtcars[grepl("Merc", rownames(mtcars)), ],
#'          images = mercLogo,
#'          imagewidth = 1.5
#'        )
#' p
#'
#' }
#' }

geom_image_glyph <- function(mapping = NULL, data = NULL, stat = 'identity',
                             position = 'identity', ...,
                             images, imagewidth = 1.2, imageheight = 0.9,
                             interpolate = TRUE,
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
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname Geom-ggproto
#' @export
GeomImageGlyph <- ggplot2::ggproto(
  'GeomImageGlyph', Geom,
  required_aes = c('x', 'y'),
  default_aes = aes(colour = NA,
                    fill = NA, size = 1, alpha = 1,
                    shape = 21, stroke = 0.5),
  draw_key = function(data, params, size) {
    data$size <- ggplot2::GeomPoint$default_aes$size/1 *
      data$size
    ggplot2::draw_key_point(data, params, size)
  },
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
    data
  },
  draw_panel = function(data, panel_params, coord, images,
                        imagewidth = 1.2, imageheight = 0.9,
                        interpolate = TRUE, na.rm = FALSE) {

    data <- coord$transform(data, panel_params)
    width_p <- grid::unit(imagewidth * data$size, "cm")
    height_p <- grid::unit(imageheight * data$size, "cm")
    n <- nrow(data)

    ggname(
      "geom_image_glyph",
      grid::gTree(
        children = do.call(grid::gList,
                           lapply(seq(n),
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
                                                       interpolate = interpolate,
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
