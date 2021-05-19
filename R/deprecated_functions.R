#' coord_radar (deprecated)
#' @description use \code{\link{coord_radial}}
#' @export
#' @inheritParams coord_radial
#' @keywords internal
#' @name coord_radar-deprecated
#'
coord_radar <- function(theta = "x", start = 0, direction = 1, clip = "on") {

  .Deprecated("coord_radial", package= "ggmulti")
  coord_radial(theta = theta, start = start,
               direction = direction, clip = clip)
}
