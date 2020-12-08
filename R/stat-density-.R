#' @rdname geom_density_
#' @export
stat_density_ <- function(mapping = NULL, data = NULL,
                          geom = "density_", position = "stack_",
                          ...,
                          bw = "nrd0",
                          adjust = 1,
                          kernel = "gaussian",
                          n = 512,
                          trim = FALSE,
                          na.rm = FALSE,
                          orientation = NA,
                          show.legend = NA,
                          inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatDensity_,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      bw = bw,
      adjust = adjust,
      kernel = kernel,
      n = n,
      trim = trim,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @title Base Stat ggproto classes for ggplot2
#' @name Stat-ggproto
#' @description All \code{stat_} functions (like \code{stat_bin()})
#' return a layer that contains a \code{Stat} object (like \code{StatBin}).
#' The  \code{Stat} object is responsible for rendering the data in the plot.
#' Each of the \code{Stat} objects is a \code{ggproto} object,
#' descended from the top-level \code{Stat}, and each implements various methods and fields.
#' @export
StatDensity_ <- ggplot2::ggproto("StatDensity_",
                                 ggplot2::StatDensity,

                                 setup_params = function(self, data, params) {

                                   params$flipped_aes <- ggplot2::has_flipped_aes(data, params, main_is_continuous = TRUE)

                                   has_x <- !(is.null(data$x) && is.null(params$x))
                                   has_y <- !(is.null(data$y) && is.null(params$y))
                                   # provide both x and y
                                   if (!has_x && !has_y) {
                                     abort("stat_density() requires an x or y aesthetic.")
                                   }

                                   if((has_x && !has_y) || (has_y && !has_x)) {
                                     return(
                                       ggplot2::ggproto_parent(ggplot2::StatDensity, self)$setup_params(data, params)
                                     )
                                   }

                                   params
                                 },

                                 setup_data = function(self, data, params) {

                                   has_x <- !(is.null(data$x) && is.null(params$x))
                                   has_y <- !(is.null(data$y) && is.null(params$y))

                                   # accept Both "x" and "y"?
                                   # if not, call `StatDensity`
                                   data$acceptBoth <- TRUE
                                   if((has_x && !has_y) || (has_y && !has_x)) {
                                     data$acceptBoth <- FALSE
                                     return(
                                       ggplot2::ggproto_parent(ggplot2::StatDensity, self)$setup_data(data, params)
                                     )
                                   }
                                   setup_group(data, params)
                                 },

                                 compute_group = function(self, data, scales,
                                                          # Hack to recognize `geom_density_` parameters
                                                          scale.x = NULL, scale.y = "data", as.mix = FALSE,
                                                          bw = "nrd0", adjust = 0.9, kernel = "gaussian",
                                                          n = 512, trim = FALSE, na.rm = FALSE, orientation = NA,
                                                          flipped_aes = TRUE) {
                                   # the same group --> the same location
                                   location <- data[[ggplot2::flipped_names(flipped_aes)$y]][1]

                                   data <- ggplot2::ggproto_parent(ggplot2::StatDensity, self)$compute_group(
                                     data = data,
                                     scales = scales,
                                     bw = bw, adjust = adjust, kernel = kernel,
                                     n = n, trim = trim, na.rm = na.rm, flipped_aes = flipped_aes
                                   )

                                   data[["location"]] <- location %||% 0

                                   data
                                 }

)
