#' @rdname geom_serialaxes_quantile
#' @export
stat_serialaxes_quantile <- function(mapping = NULL, data = NULL,
                                     geom = "serialaxes_quantile",
                                     position = "identity", ...,
                                     axes.sequence = character(0L), merge = TRUE,
                                     quantiles = seq(0, 1, 0.25),
                                     scaling = c("data", "variable", "observation", "none"),
                                     axes.position = NULL,
                                     na.rm = FALSE, orientation = NA,
                                     show.legend = NA, inherit.aes = TRUE) {

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
    stat = StatSerialaxes,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      axes.sequence = axes.sequence,
      scaling = match.arg(scaling),
      axes.position = axes.position,
      quantiles = quantiles,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}
