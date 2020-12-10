#' @title Don't adjust position
#' @seealso Other position adjustments for multiple locations:
#' \code{\link{position_stack_}}, \code{\link{position_fill_}},
#' \code{\link{position_dodge_}}, \code{\link{position_dodge2_}}
#' @export
position_identity_ <- function() {
  PositionIdentity_
}

#' @export
#' @rdname Position-ggproto
PositionIdentity_ <- ggproto("PositionIdentity_", ggplot2::PositionIdentity,
                             setup_params = function(self, data) {

                               list(positive = has_positive(data),
                                    flipped_aes = ggplot2::has_flipped_aes(data))

                             },
                             compute_layer = function(data, params, scales) {

                               data <- ggplot2::flip_data(data, flip = params$flipped_aes)

                               # if `positive` is null
                               positive <- data$positive %||% params$positive

                               data %>%
                                 dplyr::mutate(
                                   positive = positive,
                                   y = ifelse(positive, y + location, -y + location),
                                   ymin = ifelse(positive, ymin + location, -ymin + location),
                                   ymax = ifelse(positive, ymax + location, -ymax + location)
                                 ) %>%
                                 ggplot2::flip_data(flip = params$flipped_aes)
                             }
)
