#' @title Quantile layer for serial axes coordinate
#' @name geom_serialaxes_quantile
#' @description Draw a quantile layer for serial axes coordinate.
#' Don't be confused with \code{geom_quantile()} which is a quantile regression. See examples.
#' @inheritParams geom_serialaxes
#' @param quantiles numeric vector of probabilities with values in [0,1]. (Values up to 2e-14 outside that
#' range are accepted and moved to the nearby endpoint.)
#' @seealso \code{\link{geom_density_}}, \code{\link{geom_serialaxes}},
#' \code{\link{geom_serialaxes_density}}, \code{\link{geom_serialaxes_hist}}
#' @export
#' @examples
#' # lower quantile, median and upper quantile
#' p <- ggplot(iris, mapping = aes(Sepal.Length = Sepal.Length,
#'                                 Sepal.Width = Sepal.Width,
#'                                 Petal.Length = Petal.Length,
#'                                 Petal.Width = Petal.Width)) +
#'        geom_serialaxes(stat = "dotProduct") +
#'        geom_serialaxes_quantile(stat = "dotProduct",
#'                                 quantiles = c(0.25, 0.5, 0.75),
#'                                 colour = c("red", "blue", "green"), size = 2)
#' p
#'
geom_serialaxes_quantile <- function(mapping = NULL, data = NULL, stat = "serialaxes",
                                     position = "identity", ...,
                                     axes.sequence = character(0L), merge = TRUE,
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
    stat = stat,
    geom = GeomSerialaxesQuantile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      axes.sequence = axes.sequence,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname Geom-ggproto
#' @export
GeomSerialaxesQuantile <- ggplot2::ggproto(
  "GeomSerialaxesQuantile",
  ggplot2::GeomPath,
  setup_data = function(self, data, params) {

    accepted_aes <- names(self$default_aes)
    flip <- data$flipped_aes[1]
    data %>%
      ggplot2::flip_data(flip = flip) %>%
      dplyr::group_by_at(dplyr::vars(dplyr::any_of(c("PANEL", "y", accepted_aes)))) %>%
      dplyr::summarise(x = stats::quantile(x, probs = params$quantiles)) %>%
      dplyr::ungroup() %>%
      pivot_group(vars = c("PANEL", accepted_aes),
                  flipped_aes = flip,
                  quantiles = params$quantiles) %>%
      as.data.frame() %>%
      ggplot2::flip_data(flip = flip)
  },
  use_defaults = function(self, data, params = list(), modifiers = aes()) {

    n <- dim(data)[1]
    uniGroup <- unique(data$group)
    uniPANEL <- unique(data$PANEL)

    groupNum <- length(uniGroup)
    m <- n/groupNum

    if(is.wholenumber(m) && groupNum > 0) {

      PANELNum <- length(uniPANEL)

      params <- lapply(params,
                       function(param) {
                         if(length(param) == 1 || length(param) == n) return(param)
                         if(length(param) != groupNum/PANELNum) return(param) # A error may occur, but leave it to default `use_defaults`
                         rep(rep(param, PANELNum), m)
                       }
      )
    }

    ggplot2::ggproto_parent(ggplot2::GeomPath, self)$use_defaults(data, params, modifiers)
  },
  draw_panel = function(self, data, panel_params, coord, arrow = NULL,
                        quantiles = seq(0, 1, 0.25), lineend = "butt", linejoin = "round",
                        linemitre = 10, na.rm = FALSE) {
    ggplot2::ggproto_parent(ggplot2::GeomPath, self)$draw_panel(data, panel_params, coord, arrow,
                                                                lineend, linejoin, linemitre, na.rm)
  }
)

options(dplyr.summarise.inform = TRUE)


pivot_group <- function(data, vars, flipped_aes, ...) {

  args <- list(...)
  n <- sum(lengths(args), na.rm = TRUE)
  if(n== 0) n <- 1

  for(var in vars) {
    v <- data[[var]]
    if(!is.null(v))
      n <- n * length(unique(v))
  }
  data %>%
    dplyr::mutate(group = rep(seq(n),
                              length(unique(y))),
                  flipped_aes = flipped_aes)
}
