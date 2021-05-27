#' @rdname geom_serialaxes_hist
#' @inheritParams ggplot2::stat_bin
#' @export
stat_serialaxes_hist <- function(mapping = NULL, data = NULL, geom = "serialaxes_hist",
                                position = "stack_",
                                ...,
                                axes.sequence = character(0L),
                                scaling = c("data", "variable", "observation", "none"),
                                axes.position = NULL,
                                binwidth = NULL, bins = NULL,
                                center = NULL, boundary = NULL, breaks = NULL,
                                closed = c("right", "left"), pad = FALSE, width = NULL,
                                na.rm = FALSE, orientation = NA, show.legend = NA,
                                inherit.aes = TRUE) {

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSerialaxesHist,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      axes.sequence = axes.sequence,
      scaling = match.arg(scaling),
      axes.position = axes.position,
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

#' @rdname Stat-ggproto
#' @export
StatSerialaxesHist <- ggplot2::ggproto(
  "StatSerialaxesHist",
  ggplot2::StatBin,
  setup_params = function(self, data, params) {

    if(is.null(params$x))
      params$x <- NA
    if(is.null(params$y))
      params$y <- NA

    params <- StatHist_$setup_params(data, params)
    serialaxes_setup_params(data, params)

  },
  setup_data = function(data, params) {

    n <- nrow(data)
    newData <- na.omit(data)
    nNew <- nrow(newData)

    if(nNew != n) {
      warning("Removed ", n - nNew,
              " rows containing missing values (stat_serialaxes_hist).",
              call. = FALSE)
    }

    newData %>%
      serialaxes_setup_data(params) %>%
      dplyr::mutate(
        acceptBoth = TRUE
      )
  },
  compute_group = function(self, data, scales,
                           axes.sequence = character(0L), orientation = NA,
                           axes.position = NULL, scaling = "data",
                           scale.y = c("data", "group"), as.mix = TRUE,
                           positive = TRUE, prop = 0.9,
                           binwidth = NULL, bins = NULL, center = NULL,
                           width = NULL, boundary = NULL, breaks = NULL,
                           na.rm = FALSE, flipped_aes = TRUE, closed = c("right", "left"), pad = FALSE) {

    scales[[flipped_names(flipped_aes)$x]] <- ggplot2::ggproto(NULL,
                                                               ggplot2::ScaleContinuousPosition,
                                                               name = self$name,
                                                               breaks = self$breaks,
                                                               labels = self$labels,
                                                               limits = c(0, 1)
    )

    serilaxes_compute_group(self, data, scales, parent = StatHist_,
                            binwidth = binwidth, orientation = orientation,
                            na.rm = na.rm, bins = bins, center = center, width = width,
                            boundary = boundary, breaks = breaks, flipped_aes = flipped_aes,
                            closed = closed, pad = pad)
  }
)
