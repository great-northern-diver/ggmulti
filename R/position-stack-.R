#' @title Stack overlapping objects on top of each another
#' @description \code{position_stack_} stacks bars on top of each other, conditional on locations.
#' @name position_stack_
#' @inherit ggplot2::position_stack
#' @details It is built based on \code{\link{position_stack}}, but used for multiple locations, such as
#' \code{\link{geom_hist_}} or \code{\link{geom_density_}}. Rather than stack everything on top of each other,
#' \code{position_stack_} stacks bars based on locations.
#' Check examples to see the difference.
#' @seealso
#' See \code{\link{geom_hist_}}, \code{\link{geom_density_}},
#' \code{\link{geom_serialaxes_density}} and \code{\link{geom_serialaxes_hist}} for more examples.
#'
#' Other position adjustments for multiple locations:
#' \code{\link{position_identity_}},
#' \code{\link{position_dodge_}}, \code{\link{position_dodge2_}}
#'
#' Parent: \code{\link{position_stack}}
#'
#' @export
#' @examples
#' p <- ggplot(iris,
#'        mapping = aes(Sepal.Length = Sepal.Length,
#'                      Sepal.Width = Sepal.Width,
#'                      Petal.Length = Petal.Length,
#'                      Petal.Width = Petal.Width,
#'                      colour = Species))
#' p +
#'  geom_serialaxes_density(position = position_stack_())
#'
#' \donttest{
#' p +
#'   geom_serialaxes_density(position = position_stack())
#' }
#'
position_stack_ <- function(vjust = 1, reverse = FALSE) {
  ggplot2::ggproto(NULL, PositionStack_, vjust = vjust, reverse = reverse)
}

#' @rdname position_stack_
#' @inherit ggplot2::position_fill
#' @export
position_fill_ <- function(vjust = 1, reverse = FALSE) {
  ggplot2::ggproto(NULL, PositionFill_, vjust = vjust, reverse = reverse)
}

#' @export
#' @rdname Position-ggproto
PositionStack_ <- ggplot2::ggproto("PositionStack_",
                                   ggplot2::PositionStack,
                                   setup_params = function(self, data) {

                                     params <- ggplot2::ggproto_parent(ggplot2::PositionStack, self)$setup_params(data)
                                     c(
                                       params,
                                       list(positive = has_positive(data))
                                     )
                                   },
                                   compute_panel = function(data, params, scales) {

                                     data <- flip_data(data, params$flipped_aes)
                                     if (is.null(params$var)) {
                                       return(data)
                                     }

                                     negative <- data$ymax < 0
                                     negative[is.na(negative)] <- FALSE

                                     neg <- data[negative, , drop = FALSE]
                                     pos <- data[!negative, , drop = FALSE]

                                     if (any(negative)) {
                                       neg <- collide_(neg, NULL, "position_stack",
                                                       strategy = pos_stack,
                                                       collide.fun = collide,
                                                       vjust = params$vjust,
                                                       fill = params$fill,
                                                       reverse = params$reverse,
                                                       positive = params$positive)
                                     }
                                     if (any(!negative)) {
                                       pos <- collide_(pos, NULL, "position_stack",
                                                       strategy = pos_stack,
                                                       collide.fun = collide,
                                                       vjust = params$vjust,
                                                       fill = params$fill,
                                                       reverse = params$reverse,
                                                       positive = params$positive)
                                     }

                                     data <- rbind(neg, pos)[match(seq_len(nrow(data)),
                                                                   c(which(negative), which(!negative))),]
                                     flip_data(data, params$flipped_aes)
                                   }

)

#' @export
#' @rdname Position-ggproto
PositionFill_ <- ggproto("PositionFill_", PositionStack_, fill = TRUE)
