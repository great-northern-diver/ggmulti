#' @rdname geom_serialaxes_density
#' @export
stat_serialaxes_density <- function(mapping = NULL, data = NULL,
                                    geom = "serialaxes_density",
                                    position = "stack_",
                                    ...,
                                    axes.sequence = character(0L),
                                    merge = TRUE, axes.position = NULL,
                                    scaling = c("variable", "observation", "data", "none"),
                                    bw = "nrd0",
                                    adjust = 1,
                                    kernel = "gaussian",
                                    n = 512,
                                    trim = FALSE,
                                    na.rm = FALSE,
                                    orientation = NA,
                                    show.legend = NA,
                                    inherit.aes = TRUE) {


  if (merge) {
    axes.sequence_aes <- suppressWarnings(
      ggplot2::aes_all(axes.sequence)
    )
    axes.sequence_names <- names(axes.sequence)

    if(!is.null(axes.sequence_names)) {
      names(axes.sequence_aes) <- axes.sequence_names
    }

    mapping <- suppressWarnings(
      mbind(
        axes.sequence_aes,
        mapping
      )
    )
  }


  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSerialaxesDensity,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      axes.sequence = axes.sequence,
      scaling = match.arg(scaling),
      axes.position = axes.position,
      bw = bw,
      trim = trim,
      adjust = adjust,
      kernel = kernel,
      n = n,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @inherit StatDensity_
#' @export
StatSerialaxesDensity <- ggplot2::ggproto(
  "StatSerialaxesDensity",
  ggplot2::StatDensity,
  setup_params = function(data, params) {

    serialaxes_setup_params(data, params)
  },
  setup_data = function(data, params) {

    data %>%
      serialaxes_setup_data(params) %>%
      dplyr::mutate(
        acceptBoth = TRUE
      )
  },

  compute_group = function(self, data, scales, axes.sequence = character(0L), orientation = NA,
                           scaling = "variable", scale.y = c("data", "variable"), axes.position = NULL,
                           as.mix = TRUE, positive = TRUE,
                           bw = "nrd0", trim = FALSE, adjust = 0.9, kernel = "gaussian", n = 512,
                           na.rm = FALSE, flipped_aes = TRUE) {

    scales[[flipped_names(flipped_aes)$x]] <- ggplot2::ggproto(NULL,
                                                               ggplot2::ScaleContinuousPosition,
                                                               name = self$name,
                                                               breaks = self$breaks,
                                                               labels = self$labels,
                                                               limits = c(0, 1)
    )

    serilaxes_compute_group(self, data, scales, parent = StatDensity_,
                            bw = bw, adjust = adjust, kernel = kernel, n = n,
                            trim = trim, na.rm = na.rm, orientation = orientation,
                            flipped_aes = flipped_aes)
  }
)
