#' @rdname geom_hist_
#' @inheritParams ggplot2::stat_bin
#' @importFrom glue glue
#' @export
stat_hist_ <- function(mapping = NULL, data = NULL, geom = "bar_",
                       position = "stack_",
                       ...,
                       binwidth = NULL, bins = NULL,
                       center = NULL, boundary = NULL, breaks = NULL,
                       closed = c("right", "left"), pad = FALSE, width = NULL,
                       na.rm = FALSE, orientation = NA, show.legend = NA,
                       inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatHist_,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = TRUE,
    params = list(
      binwidth = binwidth,
      bins = bins,
      center = center,
      boundary = boundary,
      breaks = breaks,
      closed = closed,
      pad = pad,
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname geom_hist_
#' @inheritParams ggplot2::stat_bin
#' @export
stat_bin_ <- function(mapping = NULL, data = NULL, geom = "bar_",
                      position = "stack_",
                      ...,
                      binwidth = NULL, bins = NULL,
                      center = NULL, boundary = NULL, breaks = NULL,
                      closed = c("right", "left"), pad = FALSE,
                      na.rm = FALSE, orientation = NA, show.legend = NA,
                      inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBin_,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = TRUE,
    params = list(
      binwidth = binwidth,
      bins = bins,
      center = center,
      boundary = boundary,
      breaks = breaks,
      closed = closed,
      pad = pad,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname geom_hist_
#' @inheritParams ggplot2::stat_bin
#' @export
stat_count_ <- function(mapping = NULL, data = NULL, geom = "bar_",
                        position = "stack_",
                        ...,
                        width = NULL, na.rm = FALSE,
                        orientation = NA, show.legend = NA, inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatCount_,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = TRUE,
    params = list(
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @inherit ggplot2::StatBin
#' @export
StatHist_ <- ggplot2::ggproto("StatHist_",
                              ggplot2::StatBin,

                              setup_params = function(self, data, params) {

                                has_x <- !(is.null(data$x) && is.null(params$x))
                                has_y <- !(is.null(data$y) && is.null(params$y))

                                if (!has_x && !has_y) {
                                  rlang::abort("stat_bin() requires an x or y aesthetic.")
                                }

                                params$flipped_aes <- ggplot2::has_flipped_aes(data, params, main_is_continuous = TRUE)

                                if((has_x && !has_y) || (has_y && !has_x)) {

                                  params$flipped_aes <- ggplot2::has_flipped_aes(data,
                                                                                 params,
                                                                                 main_is_orthogonal = FALSE)

                                  x <- ggplot2::flipped_names(params$flipped_aes)$x
                                  if (is.integer(data[[x]])) {

                                    params$binwidth <- NULL
                                    params$bins <- NULL
                                    params$center <- NULL
                                    params$boundary <- NULL
                                    params$breaks <- NULL
                                    params$closed <- NULL
                                    params$pad <- NULL

                                    return(ggplot2::ggproto_parent(ggplot2::StatCount, self)$setup_params(data, params))

                                  } else {

                                    params$width <- NULL
                                    return(ggplot2::ggproto_parent(ggplot2::StatBin, self)$setup_params(data, params))

                                  }
                                }

                                x <- ggplot2::flipped_names(params$flipped_aes)$x
                                # the count
                                if(is.integer(data[[x]])) return(params)

                                if (!is.null(params$drop)) {
                                  rlang::warn("`drop` is deprecated. Please use `pad` instead.")
                                  params$drop <- NULL
                                }
                                if (!is.null(params$origin)) {
                                  rlang::warn("`origin` is deprecated. Please use `boundary` instead.")
                                  params$boundary <- params$origin
                                  params$origin <- NULL
                                }
                                if (!is.null(params$boundary) && !is.null(params$center)) {
                                  rlang::abort("Only one of `boundary` and `center` may be specified.")
                                }

                                if (is.null(params$breaks) && is.null(params$binwidth) && is.null(params$bins)) {
                                  message_wrap("`stat_bin()` using `bins = 30`. Pick better value with `binwidth`.")
                                  params$bins <- 30
                                }

                                params
                              },
                              setup_data = function(data, params) {

                                has_x <- !(is.null(data$x) && is.null(params$x))
                                has_y <- !(is.null(data$y) && is.null(params$y))

                                # accept Both "x" and "y"?
                                # if not, call `StatDensity`
                                data$acceptBoth <- TRUE
                                if((has_x && !has_y) || (has_y && !has_x)) {
                                  data$acceptBoth <- FALSE
                                  return(data)
                                }

                                setup_group(data, params)
                              },
                              compute_group = function(self, data, scales,
                                                       # Hack to recognize `geom_hist_` parameters
                                                       scale.x = NULL, scale.y = c("data", "variable"), as.mix = FALSE,
                                                       binwidth = NULL, orientation = NA,
                                                       na.rm = FALSE, bins = NULL, center = NULL, width = NULL,
                                                       boundary = NULL, breaks = NULL, flipped_aes = TRUE,
                                                       closed = c("right", "left"), pad = FALSE) {

                                x <- ggplot2::flipped_names(flipped_aes)$x
                                y <- ggplot2::flipped_names(flipped_aes)$y

                                location <- na.omit(data[[y]])[1L] %||% 0

                                acceptBoth <- na.omit(data$acceptBoth[1L])

                                isBin <- is.null(attributes(data[[x]]))

                                data <- if(isBin) {

                                  ggplot2::ggproto_parent(
                                    ggplot2::StatBin,
                                    self)$compute_group(data = data,
                                                        scales = scales,
                                                        binwidth = binwidth, bins = bins,
                                                        center = center, boundary = boundary,
                                                        closed = closed, pad = pad,
                                                        breaks = breaks, flipped_aes = flipped_aes)
                                } else {

                                  ggplot2::ggproto_parent(
                                    ggplot2::StatCount,
                                    self)$compute_group(data = data, scales = scales,
                                                        width = width, flipped_aes = flipped_aes)
                                }

                                data$acceptBoth <- acceptBoth
                                data[["location"]] <- location

                                data
                              }
)

#' @inherit ggplot2::StatBin
#' @export
StatBin_ <- ggplot2::ggproto("StatBin_",
                             StatHist_,
                             setup_params = function(self, data, params) {

                               has_x <- !(is.null(data$x) && is.null(params$x))
                               has_y <- !(is.null(data$y) && is.null(params$y))

                               if((has_x && !has_y) || (has_y && !has_x)) {
                                 return(ggplot2::ggproto_parent(ggplot2::StatBin, self)$setup_params(data, params))
                               }

                               flipped_aes <- ggplot2::has_flipped_aes(data, params, main_is_continuous = TRUE)
                               x <- ggplot2::flipped_names(flipped_aes)$x
                               if (is.integer(data[[x]])) {
                                 rlang::abort(glue::glue("StatBin_ requires a continuous {x} variable: the {x} variable is discrete.",
                                                         "Perhaps you want stat=\"count_\"?"))
                               }
                               ggplot2::ggproto_parent(StatHist_, self)$setup_params(data, params)
                             }
)

#' @inherit ggplot2::StatBin
#' @export
StatCount_ <- ggplot2::ggproto("StatCount_",
                               StatHist_,
                               setup_params = function(self, data, params) {

                                 has_x <- !(is.null(data$x) && is.null(params$x))
                                 has_y <- !(is.null(data$y) && is.null(params$y))

                                 if((has_x && !has_y) || (has_y && !has_x)) {
                                   return(ggplot2::ggproto_parent(ggplot2::StatCount, self)$setup_params(data, params))
                                 }

                                 ggplot2::ggproto_parent(StatHist_, self)$setup_params(data, params)
                               }
)
