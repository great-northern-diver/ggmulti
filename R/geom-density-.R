#' @title More general smoothed density estimates
#' @name geom_density_
#' @description Computes and draws kernel density estimate.
#' Compared with \code{geom_density()}, it provides more general cases that
#' accepting \code{x} and \code{y}. See details
#' @inheritParams ggplot2::geom_density
#' @param scale.x A sorted length 2 numerical vector representing
#' the range of the whole data will be scaled to. The default value is (0, 1).
#' @param scale.y one of \code{data} and \code{group} to specify.
#'   \tabular{ll}{ \strong{Type} \tab \strong{Description}
#'   \cr data (default) \tab The density estimates are scaled by the whole data set
#'   \cr group \tab The density estimates are scaled by each group
#'   }
#'   If the \code{scale.y} is \code{data}, it is meaningful to compare the density (shape and area) across all groups; else
#'   it is only meaningful to compare the density within each group. See details.
#' @param as.mix Logical. Within each group, if \code{TRUE}, the sum of the density estimate area is mixed and
#' scaled to maximum 1. The area of each subgroup (in general, within each group one color represents one subgroup)
#' is proportional to the count; if \code{FALSE} the area of each subgroup is the same, with maximum 1. See details.
#' @param positive If \code{y} is set as the density estimate, where the smoothed curved is faced to,
#' right (`positive`) or left (`negative`) as vertical layout; up (`positive`) or down (`negative`) as horizontal layout?
#' @param prop adjust the proportional maximum height of the estimate (density, histogram, ...).
#' @details
#' The \code{x} (or \code{y}) is a group variable (categorical) and \code{y} (or \code{x}) is the target variable (numerical) to be plotted.
#' If only one of \code{x} or \code{y} is provided, it will treated as a target variable and
#' \code{ggplot2::geom_density} will be executed.
#'
#' There are four combinations of \code{scale.y} and \code{as.mix}.
#' \describe{
#'   \item{\code{scale.y} = "group" and \code{as.mix} = FALSE}{The density estimate area of each subgroup (represented by each color)
#'   within the same group is the same.}
#'   \item{\code{scale.y} = "group" and \code{as.mix} = TRUE}{The density estimate area of each subgroup (represented by each color)
#'   within the same group is proportional to its own counts.}
#'   \item{\code{scale.y} = "data" and \code{as.mix} = FALSE}{The sum of density estimate area of all groups is scaled to maximum of 1.
#'   and the density area for each group is proportional to the its count. Within each group, the area of each subgroup is the same.}
#'   \item{\code{scale.y} = "data" and \code{as.mix} = TRUE}{The sum of density estimate area of all groups is scaled to maximum of 1
#'   and the area of each subgroup (represented by each color) is proportional to its own count.}
#' }
#' See vignettes[https://great-northern-diver.github.io/ggmulti/articles/histogram-density-.html] for more intuitive explanation.
#' @export
#' @eval rd_orientation()
#' @seealso \code{\link{geom_density}}, \code{\link{geom_hist_}}
#' @examples
#' if(require(dplyr)) {
#'   mpg %>%
#'     dplyr::filter(drv != "f") %>%
#'     ggplot(mapping = aes(x = drv, y = cty, fill = factor(cyl))) +
#'     geom_density_(alpha = 0.1)
#'
#'   # only `x` or `y` is provided
#'   # that would be equivalent to call function `geom_density()`
#'   diamonds %>%
#'     dplyr::sample_n(500) %>%
#'     ggplot(mapping = aes(x = price)) +
#'     geom_density_()
#'
#'   # density and boxplot
#'   # set the density estimate on the left
#'   mpg %>%
#'     dplyr::filter(drv != "f") %>%
#'     ggplot(mapping = aes(x = drv, y = cty,
#'                          fill = factor(cyl))) +
#'     geom_density_(alpha = 0.1,
#'                   scale.y = "group",
#'                   as.mix = FALSE,
#'                   positive = FALSE) +
#'     geom_boxplot()
#'
#'   # x as density
#'   set.seed(12345)
#'   suppressWarnings(
#'     diamonds %>%
#'       dplyr::sample_n(500) %>%
#'       ggplot(mapping = aes(x = price, y = cut, fill = color)) +
#'       geom_density_(orientation = "x", prop = 0.25,
#'                     position = "stack_",
#'                     scale.y = "group")
#'   )
#' }
#' # settings of `scale.y` and `as.mix`
#' \donttest{
#' ggplots <- lapply(list(
#'                       list(scale.y = "data", as.mix = TRUE),
#'                       list(scale.y = "data", as.mix = FALSE),
#'                       list(scale.y = "group", as.mix = TRUE),
#'                       list(scale.y = "group", as.mix = FALSE)
#'                     ),
#'                    function(vars) {
#'                      scale.y <- vars[["scale.y"]]
#'                      as.mix <- vars[["as.mix"]]
#'                      ggplot(mpg,
#'                             mapping = aes(x = drv, y = cty, fill = factor(cyl))) +
#'                        geom_density_(alpha = 0.1, scale.y = scale.y, as.mix = as.mix) +
#'                        labs(title = paste("scale.y =", scale.y),
#'                             subtitle = paste("as.mix =", as.mix))
#'                    })
#' suppressWarnings(
#'   gridExtra::grid.arrange(grobs = ggplots)
#' )}
#'
geom_density_ <- function(mapping = NULL, data = NULL, stat = "density_",
                          position = "identity_", ...,
                          scale.x = NULL, scale.y = c("data", "group", "variable"),
                          as.mix = FALSE, positive = TRUE, prop = 0.9, na.rm = FALSE,
                          orientation = NA, show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomDensity_,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      as.mix = as.mix,
      positive = positive,
      scale.x = scale.x,
      scale.y = match.arg(scale.y),
      prop = prop,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @title Base Geom ggproto classes for ggplot2
#' @name Geom-ggproto
#' @description All \code{geom_} functions (like \code{geom_point}) return a layer that contains a \code{Geom}
#' object (like \code{GeomPoint}). The \code{Geom} object is responsible for rendering the data in the plot.
#' Each of the \code{Geom} objects is a \code{ggproto} object,
#' descended from the top-level Geom, and each implements various methods and fields.
#' Compared to \code{Stat} and \code{Position}, \code{Geom} is a little different
#' because the execution of the setup and compute functions is split up.
#' setup_data runs before position adjustments, and \code{draw_layer}
#' is not run until render time, much later. This means there is no \code{setup_params}
#' because it's hard to communicate the changes.
#' @export
GeomDensity_ <- ggplot2::ggproto(
  "GeomDensity_",
  ggplot2::GeomRibbon,
  setup_params = function(data, params) {

    params$flipped_aes <- has_flipped_aes(data, params, range_is_orthogonal = TRUE)
    params$as.mix <- params$as.mix %||% FALSE
    params$positive <- params$positive %||% TRUE
    params$scale.y <- params$scale.y %||% "data"
    params$prop <- params$prop %||% 0.9

    if(!is.null(params$scale.x)) {
      if(!all(is.numeric(params$scale.x)))
        stop("`scale.x` must be numerical, but it is ", class(params$scale.x),
             call. = FALSE)
      if(length(params$scale.x) != 2)
        stop("`scale.x` should be a length 2 vector, but it has length ", length(params$scale.x),
             call. = FALSE)
    }

    params
  },
  setup_data = function(data, params) {

    data <- ggplot2::flip_data(data, params$flipped_aes)
    acceptBoth <- na.omit(data$acceptBoth)[1L]
    if(!acceptBoth) {
      # Question: should we allow `as.mix` to participate the settings of `geom_density`?
      if(params$as.mix) {
        data <- data %>%
          dplyr::group_by(group, PANEL) %>%
          summarise(sum.n = sum(n, na.rm = TRUE))%>%
          dplyr::ungroup() %>%
          dplyr::transmute(group = group, PANEL = PANEL, proportion = sum.n/ sum(sum.n, na.rm = TRUE)) %>%
          dplyr::right_join(data, by = c("group", "PANEL")) %>%
          dplyr::mutate(density = density * proportion,
                        y = y * proportion,
                        count = count * proportion,
                        scaled = scaled * proportion,
                        ndensity = ndensity * proportion)
      }
      data <- ggplot2::flip_data(data, params$flipped_aes)
      return(ggplot2::GeomArea$setup_data(data, params))
    }

    if(!is.null(params$scale.x)) {

      data <- data %>%
        dplyr::group_by(location) %>%
        dplyr::mutate(x = rescale(x, params$scale.x)) %>%
        dplyr::ungroup()
    }

    # at each group, the sum of the area is **one** instead of **one** for each group
    if(params$as.mix) {

      sum.l <- data %>%
        dplyr::group_by(location, PANEL) %>%
        summarise(sum.l = sum(n, na.rm = TRUE))

      sum.g <- data %>%
        dplyr::group_by(group, location, PANEL) %>%
        summarise(sum.g = sum(n, na.rm = TRUE))

      data <- sum.g %>%
        dplyr::left_join(sum.l,
                         by = c("location", "PANEL")) %>%
        dplyr::ungroup() %>%
        dplyr::transmute(location = location, group = group,
                         PANEL = PANEL,
                         proportion = sum.g/sum.l) %>%
        dplyr::right_join(data, by = c("location", "group", "PANEL")) %>%
        dplyr::mutate(density = density * proportion,
                      count = count * proportion,
                      scaled = scaled * proportion,
                      ndensity = ndensity * proportion)
    }

    data$positive <- params$positive

    # swap x, y and such groups to make sure that
    # **x** is always our interest.
    data %>%
      compute_scales("density", params) %>%
      dplyr::mutate(
        ymin = 0,
        ymax = y
      ) %>%
      ggplot2::flip_data(params$flipped_aes)
  },

  draw_group = function(self, data, panel_scales, coord, positive = TRUE,
                        scale.x = NULL, scale.y = "data", as.mix = FALSE,
                        prop = 0.9, na.rm = FALSE) {

    flipped_aes <- ggplot2::has_flipped_aes(data)

    return(ggplot2::ggproto_parent(ggplot2::GeomRibbon, self)$draw_group(data,
                                                                         panel_scales,
                                                                         coord, na.rm = na.rm,
                                                                         flipped_aes = flipped_aes,
                                                                         outline.type = "both"))
  },
  default_aes = ggplot2::aes(fill = NA, weight = 1, colour = "black", linewidth = 0.5,
                             alpha = NA, size = 0.5, linetype = 1),
  required_aes = c("x", "y")
)
