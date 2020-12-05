#' @title Andrews plot
#' @name dot_product
#' @description An Andrews plot or Andrews curve is a way to visualize structure in high-dimensional data
#' by defining a finite Fourier series.
#' @param k The number of dimensions
#' @param length.out The series length
#' @param ... Other arguments passed on to methods. Mainly used for customized transformation function
#' @details
#' In Andrews transformation, we project the data point onto the following k dimensional Fourier series,
#' (\bold{1/sqrt(2)}^T, sin(\bold{t}^T), cos(\bold{t}^T), sin(\bold{2t}^T), cos(\bold{2t}^T), ...)^T
#' @return
#' A list contains two contents
#' \enumerate{
#'   \item A series
#'   \item Transformed matrix
#' }
#' @references
#' Andrews, David F. "Plots of high-dimensional data." \emph{Biometrics} (1972): 125-136.
#' @export

andrews <- function(k = 4,
                    length.out = 50 * (k - 1),
                    ...) {

  stopifnot(
    {
      is.numeric(length.out)
      is.numeric(k)
    }
  )

  k <- as.integer(k)
  length.out <- as.integer(length.out)

  t <- seq(-base::pi, base::pi, length.out = length.out)

  values <- sapply(seq(k),
                   function(i) {
                     if(i == 1) return(rep(1/sqrt(2), length.out))
                     fun <- if((i %% 2) == 0) {
                       # even
                       base::sin
                     } else {
                       # odd
                       base::cos
                     }

                     fun(2^(floor(i/2) - 1) * t)
                   })
  # return a list
  # with defined period and matrix
  list(
    series = t,
    matrix = matrix(values, nrow = k, byrow = TRUE)
  )
}


#' @title Legendre polynomials
#' @rdname dot_product
#' @description Legendre polynomials are a system of complete and orthogonal polynomials.
#' @details
#' P_n(\bold{x}) is polynomial of degreen n. Legendre polynomials are a list of polynomials match that
#'
#' integral_{-1}^1 P_n(\bold{x})P_m(\bold{x}) dx = 0
#'
#' where n is not equal to m
#' @references
#' Abramowitz, Milton, and Irene A. Stegun, eds. "Chapter 8"
#' \emph{Handbook of mathematical functions with formulas, graphs, and mathematical tables}.
#' Vol. 55. US Government printing office, 1948.
#' @export

legendre <- function(k = 4,
                     length.out = 50 * (k - 1),
                     ...) {

  stopifnot(
    {
      is.numeric(length.out)
      is.numeric(k)
    }
  )

  k <- as.integer(k)
  length.out <- as.integer(length.out)

  t <- seq(-1, 1, length.out = length.out)
  if(k > 10) {
    rlang::warn("So far, `legendre` can only accommodate maximum 10 dimensions.")
    k <- 10
  }

  values <- sapply(seq(k),
                   function(i) {
                     switch(as.character(i),
                            "1" = t,
                            "2" = 1/2 * (3 * t^2 - 1),
                            "3" = 1/2 * (5 * t^3 - 3 * t),
                            "4" = 1/8 * (35 * t^4 - 30 * t^2 + 3),
                            "5" = 1/8 * (63 * t^5 - 70 * t^3 + 15 * t),
                            "6" = 1/16 * (231 * t^6 - 315 * t^4 + 105 * t^2 - 5),
                            "7" = 1/16 * (429 * t^7 - 693 * t^5 + 315 * t^3 - 35 * t),
                            "8" = 1/128 * (6435 * t^8 - 12012 * t^6 + 6930 * t^4 - 1260 * t^2 + 35),
                            "9" = 1/128 * (12155 * t^9 - 25740 * t^7 + 18018 * t^5 - 4620 * t^3 + 315 * t),
                            "10" = 1/256 * (46189 * t^10 - 109395 * t^8 + 90090 * t^6 - 30030 * t^4 + 3465 * t^2 - 63))
                   })
  # return a list
  # with defined series and matrix
  list(
    series = t,
    matrix = matrix(values, nrow = k, byrow = TRUE)
  )
}
