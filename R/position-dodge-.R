#' @name position_dodge_
#' @description Dodging preserves the vertical position of an geom while adjusting the horizontal position.
#' \code{position_dodge_()} dodges bars side by side but conditional on locations.
#' @inherit ggplot2::position_dodge
#' @details It is built based on \code{\link{position_dodge}}, but used for multiple locations, such as
#' \code{geom_hist_()} or \code{geom_density_()}. Check examples to see the difference.
#' @seealso
#' See \code{\link{geom_hist_}} and \code{\link{geom_serialaxes_hist}} for more examples.
#'
#' Other position adjustments for multiple locations:
#' \code{\link{position_identity_}},
#' \code{\link{position_stack_}}, \code{\link{position_fill_}}
#'
#' Parent: \code{\link{position_dodge}}
#'
#' @export
#' @examples
#' if(require(dplyr)) {
#' p <- iris %>%
#'   tidyr::pivot_longer(cols = -Species,
#'                       names_to = "Outer sterile whorls",
#'                       values_to = "values") %>%
#'   ggplot(data,
#'          mapping = aes(x = `Outer sterile whorls`,
#'                        y = values,
#'                        fill = Species))
#'
#' p + geom_hist_(position = position_dodge_())
#' }
#'
#' \donttest{
#' # all bins are shifted on the left
#' p +
#'   geom_hist_(position = position_dodge())
#' }
#'
#'
position_dodge_ <- function(width = NULL, preserve = c("total", "single")) {
  ggplot2::ggproto(NULL, PositionDodge_,
                   width = width,
                   preserve = match.arg(preserve)
  )
}

#' @title Base Position ggproto classes for ggplot2
#' @name Position-ggproto
#' @description All \code{position_} functions (like \code{position_dodge})
#' return a \code{Position} object (like \code{PositionDodge}).
#' The \code{Position} object is responsible for adjusting the position of overlapping geoms.
#' The way that the \code{position_} functions work is slightly different from the
#' \code{geom_} and \code{stat_} functions, because a \code{position_} function
#' actually "instantiates" the \code{Position} object by creating a descendant,
#' and returns that.
#' Each of the \code{Position} objects is a \code{ggproto} object,
#' descended from the top-level \code{Position}.
#' @export
PositionDodge_ <- ggplot2::ggproto("PositionDodge_", ggplot2::PositionDodge,

                                   compute_panel = function(data, params, scales) {

                                     data <- ggplot2::flip_data(data, params$flipped_aes)

                                     collided <- collide_(
                                       data,
                                       params$width,
                                       name = "position_dodge",
                                       strategy = pos_dodge,
                                       collide.fun = collide,
                                       n = params$n,
                                       check.width = FALSE
                                     )
                                     ggplot2::flip_data(collided, params$flipped_aes)
                                   }
)

#' @rdname position_dodge_
#' @inherit ggplot2::position_dodge2
#' @export
position_dodge2_ <- function(width = NULL, preserve = c("total", "single"),
                             padding = 0.1, reverse = FALSE) {
  ggplot2::ggproto(NULL, PositionDodge2_,
                   width = width,
                   preserve = match.arg(preserve),
                   padding = padding,
                   reverse = reverse
  )
}

#' @rdname Position-ggproto
#' @export
PositionDodge2_ <- ggproto("PositionDodge2_", ggplot2::PositionDodge2,

                           compute_panel = function(data, params, scales) {

                             data <- ggplot2::flip_data(data, params$flipped_aes)

                             collided <- collide_(
                               data,
                               params$width,
                               name = "position_dodge2",
                               strategy = pos_dodge2,
                               collide.fun = collide2,
                               n = params$n,
                               padding = params$padding,
                               check.width = FALSE,
                               reverse = params$reverse
                             )
                             ggplot2::flip_data(collided, params$flipped_aes)
                           }
)
