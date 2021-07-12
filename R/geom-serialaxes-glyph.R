#' @title Add serial axes glyphs on scatter plot
#' @description To visualize high dimensional data on scatterplot.
#' Each point glyph is surrounded by a serial axes (parallel axes or radial axes) object.
#' @inheritParams geom_serialaxes
#' @param serialaxes.data a serial axes numerical data set.
#' If not provided, a point visual (\code{geom_point()}) will be displayed.
#' @param axes.layout either "radial" or "parallel"
#' @param andrews Logical; Andrew's plot (a Fourier transformation)
#' @param show.axes boolean to indicate whether axes should be shown or not
#' @param show.enclosing boolean to indicate whether enclosing should be shown or not
#' @param linewidth line width of the "glyph" object
#' @param axescolour axes color
#' @param bboxcolour bounding box color
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
#' argument \code{serialaxes.data} is missing. If so, a point visual will be displayed with
#' corresponding shape and stroke.
#'
#' @return a \code{geom} layer
#' @seealso \code{\link{geom_polygon_glyph}}, \code{\link{geom_image_glyph}}
#'
#' @examples
#' # serial axes glyph
#' p <- ggplot(data = iris,
#'             mapping = aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
#'   geom_serialaxes_glyph(serialaxes.data = iris[, -5],
#'                         axes.layout = "radial")
#' p

geom_serialaxes_glyph <- function(mapping = NULL, data = NULL, stat = "identity",
                                  position = "identity", ..., serialaxes.data,
                                  axes.sequence = character(0L),
                                  scaling = c("data", "variable", "observation", "none"),
                                  axes.layout = c("parallel", "radial"),
                                  andrews = FALSE, show.axes = FALSE, show.enclosing = FALSE,
                                  linewidth = 1, axescolour = "black", bboxcolour = "black",
                                  na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {

  if(missing(serialaxes.data) || is.null(serialaxes.data))
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

  scaling <- match.arg(scaling)
  axes.layout <- match.arg(axes.layout)

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSerialAxesGlyph,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      serialaxes.data = serialaxes.data,
      axes.sequence = axes.sequence,
      scaling = scaling,
      axes.layout = axes.layout,
      andrews = andrews,
      show.axes = show.axes,
      show.enclosing = show.enclosing,
      axescolour = axescolour,
      bboxcolour = bboxcolour,
      linewidth = linewidth,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname Geom-ggproto
#' @export
GeomSerialAxesGlyph <- ggplot2::ggproto("GeomSerialAxesGlyph", Geom,
                                        required_aes = c("x", "y"),
                                        default_aes = aes(colour = "black",
                                                          size = 1.5, fill = NA,
                                                          linetype = 1, alpha = 1,
                                                          shape = 21, stroke = 0.5),
                                        draw_key = function (data, params, size) {
                                          data$size <- ggplot2::GeomPoint$default_aes$size/1.5
                                          ggplot2::draw_key_point(data, params, size)
                                        },
                                        setup_params = function(data, params) {

                                          col.names <- colnames(params$serialaxes.data)
                                          axes.sequence <- char2null(params$axes.sequence) %||% col.names
                                          if(!is.atomic(axes.sequence)) {
                                            stop("`axes.sequence` should be atomic, instead of ",
                                                 class(axes.sequence),
                                                 call. = FALSE)
                                          }

                                          if(!all(axes.sequence %in% col.names)) {
                                            matched_names <- axes.sequence %in% col.names

                                            warning("Cannot find ",
                                                    paste(axes.sequence[!matched_names]),
                                                    " in `serialaxes.data`",
                                                    call. = FALSE)


                                            axes.sequence <- char2null(axes.sequence[matched_names], warn = TRUE,
                                                                       message = "The `axes.sequence` is illegal;
                                                                       `serialaxes.data` will be set autometically") %||% col.names
                                          }

                                          stopifnot(
                                            exprs = {
                                              is.logical(params$show.axes)
                                              is.logical(params$show.enclosing)
                                            }
                                          )

                                          params$axes.sequence <- not_in_column_names(colnames(data), axes.sequence, ".1")
                                          params
                                        },
                                        setup_data = function(data, params) {

                                          n <- dim(data)[1]
                                          serialaxes.data <- params$serialaxes.data

                                          if(n != dim(serialaxes.data)[1]) {
                                            stop("`serialaxes.data` has ", dim(serialaxes.data)[1],
                                                 " observations that is not equal to ", n,
                                                 call. = FALSE)
                                          }
                                          # avoid duplicated names in merged dataset
                                          newnames <- not_in_column_names(colnames(data), colnames(serialaxes.data), ".1")
                                          colnames(serialaxes.data) <- newnames
                                          serialaxes.data <- params$serialaxes.data[, params$axes.sequence]
                                          colnames(serialaxes.data) <- paste0("serialaxes.data.", colnames(serialaxes.data))
                                          d <- cbind(data, serialaxes.data)
                                          d$linewidth <- params$linewidth
                                          d$scaling <- params$scaling
                                          d$axes.layout <- params$axes.layout
                                          d$andrews <- params$andrews
                                          d$show.axes <- params$show.axes
                                          d$show.enclosing <- params$show.enclosing
                                          d$axescolour <- params$axescolour
                                          d$bboxcolour <- params$bboxcolour
                                          d
                                        },
                                        draw_panel = function(data, panel_params, coord,
                                                              serialaxes.data, axes.sequence, scaling = "data",
                                                              axes.layout = "radial", andrews = FALSE, show.axes = FALSE,
                                                              show.enclosing = FALSE,
                                                              axescolour, bboxcolour, linewidth, na.rm) {

                                          data <- coord$transform(data, panel_params)

                                          # each element color
                                          color <- data$colour
                                          fill <- data$fill
                                          # size
                                          size <- data$size
                                          # parallel or radial
                                          scaledData <- get_scaledData(data = data[, grepl("serialaxes.data", colnames(data))],
                                                                       sequence = NULL,
                                                                       scaling = scaling)

                                          p <- ncol(scaledData)

                                          if(andrews) {
                                            fourierTrans <- andrews(p = p, k = 200)
                                            scaledData <- as.matrix(scaledData) %*% fourierTrans$matrix

                                            dataRange <- range(scaledData)
                                            d <- if(diff(dataRange) == 0) 1 else diff(dataRange)

                                            scaledData <- (scaledData - min(scaledData))/d
                                          }

                                          dimension <- dim(scaledData)[2]

                                          # position
                                          xpos <- data$x
                                          ypos <- data$y

                                          show.area <- !any(is.na(fill))

                                          switch(
                                            axes.layout,
                                            "parallel" = {
                                              # height:width = 1:2
                                              scale.x <- data$size
                                              scale.y <- data$size/2

                                              xaxis <- t(sapply(scale.x, function(x) seq(-0.5 * x, 0.5 * x, length.out = dimension)))
                                              yaxis <- (scaledData - 0.5) * scale.y
                                            },
                                            "radial" = {
                                              # size is diameter
                                              scale.x <- data$size/2
                                              scale.y <- data$size/2

                                              angle <- seq(0, 2*base::pi, length.out = dimension + 1)[1:dimension]

                                              xaxis <- t(sapply(1:length(scale.x), function(i) scale.x[i] * scaledData[i, ] * cos(angle)))
                                              yaxis <- t(sapply(1:length(scale.y), function(i) scale.y[i] * scaledData[i, ] * sin(angle)))
                                            }
                                          )

                                          gridAesthetic <- get_gridAesthetic(axes.layout = axes.layout,
                                                                             andrews = andrews,
                                                                             xpos = xpos, ypos = ypos,
                                                                             scale.x = scale.x, scale.y = scale.y,
                                                                             xaxis = xaxis, yaxis = yaxis,
                                                                             dimension = dimension,
                                                                             p = p, show.area = show.area,
                                                                             show.enclosing = show.enclosing)

                                          fun <- if(show.area) {
                                            grid::polygonGrob
                                          } else {
                                            grid::polylineGrob
                                          }

                                          ggname("geom_serialaxes_glyph",
                                                 grid::gTree(
                                                   children = grid::gList(
                                                     if(show.enclosing) {

                                                       grid::polylineGrob(
                                                         x = gridAesthetic$enclosingX,
                                                         y = gridAesthetic$enclosingY,
                                                         id = gridAesthetic$enclosingId,
                                                         gp = grid::gpar(col = bboxcolour)
                                                       )
                                                     },
                                                     if(show.axes) {
                                                       grid::polylineGrob(
                                                         x = gridAesthetic$axesX,
                                                         y = gridAesthetic$axesY,
                                                         id = gridAesthetic$axesId,
                                                         gp = grid::gpar(col = axescolour)
                                                       )
                                                     },
                                                     fun(
                                                       x = gridAesthetic$serialCoordX,
                                                       y = gridAesthetic$serialCoordY,
                                                       id = gridAesthetic$serialCoordId,
                                                       gp = grid::gpar(col = data$colour,
                                                                       fill = data$fill,
                                                                       lwd = linewidth,
                                                                       alpha = data$alpha)
                                                     )
                                                   )
                                                 )
                                          )
                                        }
)


get_gridAesthetic <- function(axes.layout, andrews, xpos, ypos,
                              scale.x, scale.y, xaxis, yaxis,
                              dimension, p, show.area, show.enclosing,
                              unit = "cm") {

  enclosingX <- enclosingY <- enclosingId <- list()
  axesX <- axesY <- axesId <- list()
  serialCoordX <- serialCoordY <- list()

  N <- length(xpos)

  # side effect
  if(axes.layout == "parallel") {
    lapply(1:N,
           function(i){

             # `<<-` is used inside the function of `lapply`
             # such operation only changes vars of my own namespace
             # (i.e. `loon_get_scaledData`, `get_gridAesthetic`, etc)
             # and global environment will not be affected.
             # The main reason is to avoid the heavy `for` loop

             # enclosing
             enclosingX[[i]] <<- grid::unit(xpos[i], 'native') +
               grid::unit((c(0, 0, 1, 0, 0, 1, 1, 1) - 0.5) * scale.x[i], unit)
             enclosingY[[i]] <<- grid::unit(ypos[i], 'native') +
               grid::unit((c(0, 0, 0, 1, 1, 0, 1, 1) - 0.5) * scale.y[i], unit)
             enclosingId[[i]] <<- rep(((i - 1)*4 + 1):(4 * i), 2)


             # axes
             axesX[[i]] <<- grid::unit(xpos[i], 'native') +
               rep(grid::unit(pth(xaxis[i, ], p), unit), each = 2)
             axesY[[i]] <<- grid::unit(ypos[i], 'native') +
               rep(grid::unit(c(-0.5 * scale.y[i],
                                0.5 * scale.y[i]), unit),
                   p)
             axesId[[i]] <<- rep(((i - 1)*p + 1):(p * i),
                                 each = 2)

             # serialCoord
             if(show.area) {
               serialCoordX[[i]] <<- grid::unit(xpos[i], 'native') +
                 grid::unit(c(xaxis[i, ], rev(xaxis[i, ])), unit)
               serialCoordY[[i]] <<- grid::unit(ypos[i], 'native') +
                 grid::unit(c(yaxis[i, ], rep(-0.5 * scale.y[i], dimension)), unit)
             } else {
               serialCoordX[[i]] <<- grid::unit(xpos[i], 'native') +
                 grid::unit(xaxis[i, ], unit)
               serialCoordY[[i]] <<- grid::unit(ypos[i], 'native') +
                 grid::unit(yaxis[i, ], unit)
             }
           })

    serialCoordId <- if(show.area) rep(1:N, each = 2*dimension) else rep(1:N, each = dimension)

  } else if (axes.layout == "radial") {

    len_radial <- 101
    angle <- seq(0, 2*base::pi, length.out = dimension + 1)[1:dimension]

    lapply(1:N,
           function(i){

             # `<<-` is used inside the function of `lapply`
             # such operation only changes vars of my own namespace
             # (i.e. `loon_get_scaledData`, `get_gridAesthetic`, etc)
             # and global environment will not be affected.
             # The main reason is to avoid the heavy `for` loop

             # enclosing
             enclosingX[[i]] <<- grid::unit(xpos[i], 'native') +
               grid::unit(scale.x[i] * cos(seq(0, 2*base::pi, length=len_radial)), unit)
             enclosingY[[i]] <<- grid::unit(ypos[i], 'native') +
               grid::unit(scale.y[i] * sin(seq(0, 2*base::pi, length=len_radial)), unit)


             if (show.enclosing) {

               # axes
               axesX[[i]] <<- grid::unit(xpos[i], 'native') +
                 grid::unit(c(rep(0, p), pth(scale.x[i] * cos(angle), p, TRUE)), unit)
               axesY[[i]] <<- grid::unit(ypos[i], 'native') +
                 grid::unit(c(rep(0, p), pth(scale.y[i] * sin(angle), p, TRUE)), unit)
               axesId[[i]] <<- rep(((i - 1)*p + 1):(p * i), 2)

             } else {
               # axes
               axesX[[i]] <<- grid::unit(xpos[i], 'native') +
                 grid::unit(c(rep(0, p), pth(xaxis[i, ], p, TRUE)), unit)
               axesY[[i]] <<- grid::unit(ypos[i], 'native') +
                 grid::unit(c(rep(0, p), pth(yaxis[i, ], p, TRUE)), unit)
               axesId[[i]] <<- rep(((i - 1)*p + 1):(p * i), 2)
             }

             # serialCoord
             serialCoordX[[i]] <<- grid::unit(xpos[i], 'native') +
               grid::unit(c(xaxis[i, ], rev(xaxis[i, 1])), unit)
             serialCoordY[[i]] <<- grid::unit(ypos[i], 'native') +
               grid::unit(c(yaxis[i, ], rev(yaxis[i, 1])), unit)
           })

    enclosingId <- rep(1:N, each = len_radial)
    serialCoordId <- rep(1:N, each = (dimension + 1))

  } else stop('unknown axes layout',
              call. = FALSE)

  list(
    enclosingX = do.call(grid::unit.c, enclosingX),
    enclosingY = do.call(grid::unit.c, enclosingY),
    enclosingId = unlist(enclosingId),
    axesX = do.call(grid::unit.c, axesX),
    axesY = do.call(grid::unit.c, axesY),
    axesId = unlist(axesId),
    serialCoordX = do.call(grid::unit.c, serialCoordX),
    serialCoordY = do.call(grid::unit.c, serialCoordY),
    serialCoordId = serialCoordId
  )
}

pth <- function(x, p, circle = FALSE) {
  len <- length(x)
  if(len == p) return(x)
  # In a circle, the first one and the last one are identical
  if(circle) {
    x[round(seq(1, len, length.out = p + 1))][- (p + 1)]
  } else {
    x[round(seq(1, len, length.out = p))]
  }
}

