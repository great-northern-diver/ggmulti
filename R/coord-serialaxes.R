#' @title Serial axes coordinates
#' @description
#' It is mainly used to visualize the high dimensional data set
#' either on the parallel coordinate or the radial coordinate.
#'
#' @param axes.layout Serial axes layout, either "parallel" or "radial".
#' @param scaling One of \code{data}, \code{variable}, \code{observation} or
#' \code{none} (not suggested the layout is the same with \code{data})
#' to specify how the data is scaled.
#' @param axes.sequence A vector with variable names that defines the axes sequence.
#' @param positive If \code{y} is set as the density estimate, where the smoothed curved is faced to,
#' right (\code{positive}) or left (\code{negative}) as vertical layout;
#' up (\code{positive}) or down (\code{negative}) as horizontal layout?
#' @param ... other arguments used to modify layers
#' @details Serial axes coordinate system (parallel or radial) is different from the
#' Cartesian coordinate system or its transformed system (say \code{polar} in \code{ggplot2})
#' since it does not have a formal transformation
#' (i.e. in polar coordinate system, "x = rcos(theta)", "y = rsin(theta)").
#' In serial axes coordinate system, mapping aesthetics does not really require "x" or "y". Any "non-aesthetics"
#' components passed in the \code{mapping} system will be treated as an individual axis.
#'
#' To project a common \code{geom} layer on such serialaxes,
#' users can customize function \code{\link{add_serialaxes_layers}}.
#'
#' @section Potential Risk:
#' In package \code{ggmulti}, the function \code{ggplot_build.gg} is provided.
#' At the \code{ggplot} construction time, the system will call \code{ggplot_build.gg}
#' first. If the plot input is not a \code{CoordSerialaxes} coordinate system, the next method
#' \code{ggplot_build.ggplot} will be called to build a "gg" plot; else
#' some geometric transformations will be applied first, then the next method
#' \code{ggplot_build.ggplot} will be executed. So, the potential risk is, if some other packages
#' e.g. \code{foo}, also provide a function \code{ggplot_build.gg} that is used for their
#' specifications but the namespace is beyond the \code{ggmulti} (\code{ggmulti:::ggplot_build.gg} is
#' covered), error may occur. If so, please consider using the
#' \code{\link{geom_serialaxes}}.
#'
#' @importFrom utils getFromNamespace globalVariables
#'
#' @return a \code{ggproto} object
#'
#' @examples
#' if(require("dplyr")) {
#' # Data
#' nba <- NBAstats2021 %>%
#'   mutate(
#'     dPTS = PTS - OPTS,
#'     dREB = REB - OREB,
#'     dAST = AST - OAST,
#'     dTO = TO - OTO
#'   )
#' # set sequence by `axes.sequence`
#' p <- ggplot(nba,
#'             mapping = aes(
#'               dPTS = dPTS,
#'               dREB = dREB,
#'               dAST = dAST,
#'               dTO = dTO,
#'               colour = Win
#'             )) +
#'        geom_path(alpha = 0.2) +
#'        coord_serialaxes(axes.layout = "radial") +
#'        scale_color_gradient(low="blue", high="red")
#' p
#' # quantile layer
#' p + geom_quantiles(quantiles = c(0.5),
#'                    colour = "green", size = 1.2)
#'
#' # facet
#' p +
#'   facet_grid(Playoff ~ CONF)
#' }
#' @export
coord_serialaxes <- function(axes.layout = c("parallel", "radial"),
                             scaling = c("data", "variable", "observation", "none"),
                             axes.sequence = character(0L),
                             positive = TRUE, ...) {

  ggplot2::ggproto("CoordSerialaxes",
                   ggplot2::Coord,
                   axes.layout = match.arg(axes.layout),
                   scaling = match.arg(scaling),
                   axes.sequence = axes.sequence,
                   positive = positive,
                   ...
  )
}

#' @export
ggplot_build.gg <- function(plot) {

  object <- plot$coordinates
  # regular call
  if(!inherits(object, "CoordSerialaxes"))
    return(NextMethod()) # call next method `ggplot_build.ggplot`

  plot$coordinates <- switch(
    object$axes.layout,
    "parallel" = {
      plot$coordinates <- ggplot2::coord_cartesian(xlim = object$xlim %||% NULL,
                                                   ylim = object$ylim %||% NULL,
                                                   expand = object$expand %||% TRUE,
                                                   clip = object$clip %||% "on")
    },
    "radial" = {
      plot$coordinates <- coord_radial(theta = object$theta %||% "x",
                                       start = object$start %||% 0,
                                       direction = object$direction %||% 1,
                                       clip = object$clip %||% "on")
    }, NULL
  )
  plot <- update_CoordSerialaxes(plot, object)
  NextMethod() # call next method `ggplot_build.ggplot`
}

update_CoordSerialaxes <- function(p, object) {

  axes <- set_default_axes_sequence(p, object)

  if(length(p$layers) > 0) {
    p <- serialaxes_layers(p, object, axes)
  } else {
    warning("No layers are detected. Did you forget to add the `geom_path()` object?",
            call. = FALSE)
  }

  flipped_aes <- has_flipped_aes_(object$orientation)

  # do not set any themes for p
  if(is_dotProduct(p)) return(p)

  # set labels and themes
  p <- p +
    ggplot2::labs(
      x = if(flipped_aes) "axes sequence" else NULL,
      y = if(flipped_aes) NULL else "axes sequence"
    ) +
    serialaxes_themes(flipped_aes)

  # it has chance to fail
  # as the x or y is discrete
  # (users set `mapping = aes(x = discrete var, y = XXX, ...)`,
  #  note that a quick way to fix this is to avoid x and y,
  #  e.g. set `mapping = aes(xx = ..., yy = ...)`)
  # rather than fixing it, we throw a warning to users
  # try avoid using names `x` or `y`, give more meaningful names

  if(aes_xy(p)) {
    p
  } else {
    # `scale_x_continous_` and `scale_y_continous_`
    # are customized function. Don't confuse with the
    # `scale_x_continous` and `scale_y_continous`
    p +
      scale_x_continous_(
        flipped_aes = flipped_aes,
        labels = axes,
        breaks = object$axes.position %||% seq(axes),
        expand = c(0,0)
      ) +
      scale_y_continous_(
        flipped_aes = flipped_aes,
        labels = axes,
        breaks = object$axes.position %||% seq(axes),
        expand = c(0,0)
      )
  }
}

aes_xy <- function(p) {

  data <- p$data
  mapping <- p$mapping
  # is x or y in mapping aesthetics?
  if(any(c("x", "y") %in% names(mapping))) return(TRUE)
  for(layer in p$layers) {
    if(any(c("x", "y") %in% names(layer$mapping))) return(TRUE)
  }

  return(FALSE)
}

scale_x_continous_ <- function(flipped_aes = FALSE,
                               breaks = ggplot2::waiver(),
                               labels = ggplot2::waiver(),
                               expand = c(0, 0),
                               ...) {
  if(flipped_aes) {
    ggplot2::scale_x_continuous(
      labels = labels,
      breaks = breaks,
      expand = expand,
      ...
    )
  } else {
    ggplot2::scale_x_continuous(
      expand = expand,
      labels = NULL,
      ...
    )
  }
}

scale_y_continous_ <- function(flipped_aes = FALSE,
                               breaks = ggplot2::waiver(),
                               labels = ggplot2::waiver(),
                               expand = c(0, 0),
                               ...) {
  if(flipped_aes) {
    ggplot2::scale_y_continuous(
      expand = expand,
      labels = NULL,
      ...
    )
  } else {
    ggplot2::scale_y_continuous(
      labels = labels,
      breaks = breaks,
      expand = expand,
      ...
    )
  }
}

serialaxes_layers <- function(p, object, axes) {

  layers <- p$layers
  # remove all layers
  p$layers <- list()

  if(length(layers) == 0) return(p)

  for(layer in layers) {
    p <- add_serialaxes_layers(layer, p, object, axes)
  }

  return(p)
}

# themes provides some default settings to make the serialaxes look better
serialaxes_themes <- function(flipped_aes = TRUE) {

  if(flipped_aes) {
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.margin = grid::unit(c(5,12,5,12), "mm")
    )
  } else {
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.margin = grid::unit(c(5,12,5,12), "mm")
    )
  }
}
