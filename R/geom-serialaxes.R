#' @title Serial axes layer
#' @name geom_serialaxes
#' @description Draw a serial axes layer, parallel axes under Cartesian system and radial axes under Polar system.
#' It only takes the "widens" data. Each non-aesthetics component defined in the mapping \code{aes()} will
#' be treated as an axis.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_path
#' @param axes.sequence A vector to define the axes sequence. In serial axes coordinate, the sequence can be either
#' determined in \code{mapping} (function \code{aes()}) or by \code{axes.sequence}. The only difference is that
#' the \code{mapping} aesthetics will omit the duplicated axes (check examples in \code{\link{geom_serialaxes}}).
#' @param merge Should \code{axes.sequence} be merged with mapping aesthetics
#' as a single mapping \code{uneval} object?
#' @importFrom tidyr pivot_longer
#' @seealso \code{\link{coord_serialaxes}}, \code{\link{geom_serialaxes_density}},
#' \code{\link{geom_serialaxes_quantile}}, \code{\link{geom_serialaxes_hist}}
#' @details The difference between the "lengthens" data and "widens" data can be found in
#' \href{http://vita.had.co.nz/papers/tidy-data.pdf}{Tidy Data}.
#' How to transform one to the other is explained in \href{https://tidyr.tidyverse.org/articles/pivot.html}{tidyr}
#' @export
#' @examples
#' # parallel coordinate
#' p <- ggplot(NBAstats2021,
#'             mapping = aes(FGA = FGA,
#'                           `3PA` = `3PA`,
#'                           FTA = FTA,
#'                           OFGA = OFGA,
#'                           O3PA = O3PA,
#'                           OFTA = OFTA,
#'                           colour = CONF))
#'
#' # Teams in West are more likely to make 3-point field goals.
#' # Besides, they have a better performance in restricting opponents
#' # to make 3-point field goals.
#' p +
#'   geom_serialaxes(scaling = "variable",
#'                   alpha = 0.4,
#'                   linewidth = 3) +
#'   scale_x_continuous(breaks = 1:6,
#'                      labels = c("FGA", "3PA", "FTA",
#'                                 "OFGA", "O3PA", "OFTA")) +
#'   scale_y_continuous(labels = NULL)
#'
#' # andrews plot
#' p + geom_serialaxes(stat = "dotProduct",
#'                     scaling = "variable",
#'                     transform = andrews) # default
#'
#' # Legendre polynomials
#' p + geom_serialaxes(stat = "dotProduct",
#'                     scaling = "variable",
#'                     transform = legendre)
#'
#' \donttest{
#' ############# Determine axes sequence
#' # 1. set the duplicated axes by mapping aesthetics
#' ggplot(iris, mapping = aes(Sepal.Length = Sepal.Length,
#'                            Sepal.Width = Sepal.Width,
#'                            Sepal.Length = Sepal.Length,
#'                            Sepal.Width = Sepal.Width,
#'                            colour = Species)) +
#'   # only two axes, duplicated axes are removed
#'   geom_serialaxes()
#'
#' # 2. set the duplicated axes by axes.sequence
#' ggplot(iris, mapping = aes(colour = Species)) +
#'   geom_serialaxes(
#'     axes.sequence = c("Sepal.Length", "Sepal.Width",
#'                       "Sepal.Length", "Sepal.Width"))
#' }
geom_serialaxes <- function(mapping = NULL, data = NULL, stat = "serialaxes",
                            position = "identity", ..., axes.sequence = character(0L),
                            merge = TRUE, na.rm = FALSE, orientation = NA,
                            show.legend = NA, inherit.aes = TRUE) {

  # merge axes.sequence as mapping
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
    geom = GeomSerialaxes,
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
GeomSerialaxes <- ggplot2::ggproto(
  "GeomSerialaxes",
  ggplot2::GeomPath,
  required_aes = NULL,
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
                         rep(rep(param, PANELNum), each = m)
                       }
      )
    }

    ggplot2::ggproto_parent(ggplot2::GeomPath, self)$use_defaults(data, params, modifiers)
  }
)

