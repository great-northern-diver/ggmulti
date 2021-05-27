#' @title Smoothed density estimates for "widens" data under serial axes coordinate
#' @name geom_serialaxes_density
#' @description Computes and draws kernel density estimates on serial axes coordinate
#' for each non-aesthetics component defined in the mapping \code{aes()}.
#' @inheritParams geom_serialaxes
#' @inheritParams geom_density_
#' @export
#' @seealso \code{\link{geom_density_}}, \code{\link{geom_serialaxes}},
#' \code{\link{geom_serialaxes_quantile}}, \code{\link{geom_serialaxes_hist}}
#' @examples
#' p <- ggplot(iris, mapping = aes(Sepal.Length = Sepal.Length,
#'                                 Sepal.Width = Sepal.Width,
#'                                 Petal.Length = Petal.Length,
#'                                 Petal.Width = Petal.Width,
#'                                 colour = Species,
#'                                 fill = Species)) +
#'        geom_serialaxes(alpha = 0.2) +
#'        geom_serialaxes_density(alpha = 0.5) +
#'        scale_x_continuous(breaks = 1:4,
#'                           labels = colnames(iris)[-5]) +
#'        scale_y_continuous(labels = NULL) +
#'        xlab("variable") +
#'        ylab("") +
#'        theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
#' p
geom_serialaxes_density <- function(mapping = NULL, data = NULL, stat = "serialaxes_density",
                                    position = "identity_", ...,
                                    axes.sequence = character(0L), merge = TRUE,
                                    scale.y = c("data", "group"), as.mix = TRUE,
                                    positive = TRUE,
                                    prop = 0.9, na.rm = FALSE, orientation = NA,
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
    stat = stat,
    geom = GeomSerialaxesDensity,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      axes.sequence = axes.sequence,
      positive = positive,
      as.mix = as.mix,
      prop = prop,
      scale.y = match.arg(scale.y),
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname Geom-ggproto
#' @export
GeomSerialaxesDensity <- ggplot2::ggproto(
  "GeomSerialaxesDensity",
  GeomDensity_,
  setup_data = function(self, data, params) {

    # fix the hack
    data <- data %>%
      dplyr::mutate(x = .x,
                    y = .y) %>%
      dplyr::select(-c(.x, .y))

    ggplot2::ggproto_parent(GeomDensity_, self)$setup_data(data, params)
  }
)
