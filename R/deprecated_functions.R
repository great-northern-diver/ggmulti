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

as_r_serialaxesGlyph_size <- function(size, coord, axesLayout) {

  # loon default `as_r_serialaxesGlyph_size`
  fun <- function(size, coord, axesLayout){
    if (is.numeric(size)) {
      # trial and error to choose scale for size
      if (axesLayout == "radial") {
        size <- sqrt(size) * 5
      } else if (axesLayout == "parallel"){
        if (coord == "x") {
          size <- sqrt(size) * 6.4
        } else if (coord == "y"){
          size <- sqrt(size) * 3.2
        } else size <- NA
      } else size <- NA
      size[size == 0] <- 0.01
    }
    size
  }
  2 * fun(size, coord, axesLayout)
}

as_r_polygonGlyph_size <- function(size) {

  # loon default `as_r_polygonGlyph_size`
  fun <- function(size){
    if (is.numeric(size)) {
      # trial and error to choose scale for size
      size <- size/1.25
      size[size < 0.01] <- 0.01
      size
    }
    size
  }
  4 * fun(size)
}
