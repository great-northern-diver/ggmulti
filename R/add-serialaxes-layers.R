#' @title Layers for serial axes coordinate
#' @description Project the regular \code{geom} layers onto the serial axes coordinate.
#' @param layer a layer object
#' @param plot a \code{ggplot} object
#' @param object some parameters used to modify this serial axes \code{ggplot} object (i.e. \code{axes.sequence}, ...)
#' @param axes canvas sequence axes
#' @details The class is determined by layers you add. For example, you want to add a boxplot layer
#' on serial axes coordinate.
#' By the ggplot syntax, it should be \code{ggplot(data, mapping) + geom_boxplot() + coord_serialaxes()}
#' To make it work, object \code{add_serialaxes_layers.GeomBoxplot} must be created. In this function,
#' some computations will be applied.
#'
#' @export
#' @import ggplot2 methods
#' @importFrom stats setNames quantile
add_serialaxes_layers <- function(layer, plot, object, axes) {

  data <- {if(is.waive(layer$data)) NULL else layer$data} %||% plot$data

  if(is.null(data)) {
    warning("Data is not found, neither in the function `ggplot()` nor in the layer.",
            call. = FALSE)
    return(plot)
  }

  UseMethod("add_serialaxes_layers", layer$geom)
}

#' @export
add_serialaxes_layers.default <- function(layer, plot, object, axes) {
  warning("The layer ", class(layer$geom)[1], " is not implemented in serialaxes coordinate yet. ",
          "It will be omitted.",
          call. = FALSE)
  plot + ggplot2::geom_blank()
}

#' @export
add_serialaxes_layers.GeomPath <- function(layer, plot, object, axes) {

  fun <- geom_serialaxes
  args <- setup_args(layer, plot, object, axes, fun,
                     transform = layer$stat_params$transform,
                     aesthetics = names(ggplot2::GeomPath$default_aes))

  plot + do.call(fun, args)
}

#' @export
add_serialaxes_layers.GeomRibbon <- function(layer, plot, object, axes) {
  warning("Not implenmented yet",
          call. = FALSE)
  plot + ggplot2::geom_blank()
}

#' @export
add_serialaxes_layers.GeomDensity <- function(layer, plot, object, axes) {

  if(object$axes.layout == "radial") {
    message("The density cannot be embedded on the radial axes yet")
    # maintain layer position
    return(plot + ggplot2::geom_blank())
  }

  fun <- geom_serialaxes_density

  args <- setup_args(layer, plot, object, axes, fun,
                     position = reset_position(layer$position),
                     positive = layer$stat_params$positive %||% object$positive %||% TRUE,
                     scale.y = layer$stat_params$scale.y %||% "data",
                     as.mix =  layer$stat_params$as.mix %||% TRUE,
                     prop = layer$stat_params$prop %||% 0.9,
                     aesthetics = names(ggplot2::GeomDensity$default_aes))

  plot + do.call(fun, args)
}

#' @export
add_serialaxes_layers.GeomFreqpoly <- function(layer, plot, object, axes) {
  warning("Not implenmented yet",
          call. = FALSE)
  plot
}

#' @export
add_serialaxes_layers.GeomBar <- function(layer, plot, object, axes) {

  if(object$axes.layout == "radial") {
    message("The histogram cannot be embedded on the radial axes yet")
    # maintain layer position
    return(plot + ggplot2::geom_blank())
  }

  fun <- geom_serialaxes_hist

  args <- setup_args(layer, plot, object, axes, fun,
                     position = reset_position(layer$position),
                     positive = layer$stat_params$positive %||% object$positive %||% TRUE,
                     scale.y = layer$stat_params$scale.y %||% "data",
                     as.mix =  layer$stat_params$as.mix %||% TRUE,
                     prop = layer$stat_params$prop %||% 0.9,
                     aesthetics = names(ggplot2::GeomBar$default_aes))

  plot + do.call(fun, args)
}

#' @export
add_serialaxes_layers.GeomQuantiles <- function(layer, plot, object, axes) {

  fun <- geom_serialaxes_quantile

  args <- setup_args(layer, plot, object, axes, fun,
                     quantiles = layer$stat_params$quantiles %||% seq(0, 1, length.out = 5),
                     aesthetics = names(ggplot2::GeomPath$default_aes))

  plot + do.call(fun, args)
}

#' @export
add_serialaxes_layers.GeomQuantile <- function(layer, plot, object, axes) {
  add_serialaxes_layers.GeomQuantiles(layer, plot, object, axes)
}

setup_args <- function(layer, plot, object, axes,
                       fun, aesthetics = c("colour", "size", "alpha", "linetype"),
                       position = layer$position,
                       ...) {

  axes.sequence <- set_default_axes_sequence(plot, object, layer = layer)
  layer_data <- if(is.waive(layer$data)) NULL else layer$data
  data <- plot$data %||% layer_data

  stat <- formals(fun)$stat

  should_merge <- function(plot, layer, axes.sequence) {
    mapping <- c(names(plot$mapping), names(layer$mapping))
    if(all(axes.sequence %in% mapping)) {
      FALSE
    } else TRUE
  }

  geomArgs <- remove_null(
    mapping = layer$mapping,
    data = layer_data,
    stat = if(valid_stat(stat, layer)) layer$stat else stat,
    position = position,
    axes.sequence = axes.sequence,
    merge = should_merge(plot, layer, axes.sequence),
    scaling = layer$stat_params$scaling %||% object$scaling,
    axes.position = set_axes_position(layer, object, axes,
                                      axes.sequence),
    orientation = set_orientation(layer, object),
    inherit.aes = TRUE
  )

  aesArgs <- remove_null(
    stats::setNames(
      lapply(aesthetics,
             function(aes) {
               layer$aes_params[[aes]]
               # valid_aes(layer$aes_params[[aes]], data,
               #           axes.sequence_length(axes.sequence, plot$mapping, layer))
             }),
      aesthetics
    ),
    as_list = FALSE)

  c(geomArgs,
    aesArgs,
    remove_null(list(...),
                as_list = FALSE)
  )
}
