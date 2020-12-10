#' @title Transformation Coefficients
#' @name dot_product
#' @description The dimension of the original data set is \code{n\*p}. It can be projected
#' onto a \code{n\*k} space. The functions below are to provide such transformations, e.g.
#' the \code{Andrews coefficient} (a Fourier transformation) and the \code{Legendre} polynomials.
#' @param p The number of dimensions
#' @param k The sequence length
#' @param ... Other arguments passed on to methods. Mainly used for customized transformation function
#' @return
#' A list contains two named components
#' \enumerate{
#'   \item vector: A length \code{k} vector (define the domain)
#'   \item matrix: A \code{p\*k} transformed coefficient matrix
#' }
#' @references
#' Andrews, David F. "Plots of high-dimensional data." \emph{Biometrics} (1972): 125-136.
#'
#' @export
#' @examples
#' x <- andrews(p = 4)
#' dat <- iris[, -5]
#' proj <- t(as.matrix(dat) %*% x$matrix)
#' matplot(x$vector, proj,
#'         type = "l", lty = 1,
#'         col = "black",
#'         xlab = "x",
#'         ylab = "Andrews coefficients",
#'         main = "Iris")

andrews <- function(p = 4,
                    k = 50 * (p - 1),
                    ...) {

  stopifnot(
    {
      is.numeric(k)
      is.numeric(p)
    }
  )

  p <- as.integer(p)
  k <- as.integer(k)

  t <- seq(-base::pi, base::pi, length.out = k)

  values <- sapply(seq(p),
                   function(i) {
                     if(i == 1) return(rep(1/sqrt(2), k))
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
    vector = t,
    matrix = matrix(values, nrow = p, byrow = TRUE)
  )
}



#' @rdname dot_product
#' @references
#' Abramowitz, Milton, and Irene A. Stegun, eds. "Chapter 8"
#' \emph{Handbook of mathematical functions with formulas, graphs, and mathematical tables}.
#' Vol. 55. US Government printing office, 1948.
#' @export
legendre <- function(p = 4,
                     k = 50 * (p - 1),
                     ...) {

  stopifnot(
    {
      is.numeric(k)
      is.numeric(p)
    }
  )

  p <- as.integer(p)
  k <- as.integer(k)

  t <- seq(-1, 1, length.out = k)
  if(p > 10) {
    warning("So far, `legendre` can only accommodate maximum 10 dimensions.",
            call. = FALSE)
    p <- 10
  }

  values <- sapply(seq(p),
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
    vector = t,
    matrix = matrix(values, nrow = p, byrow = TRUE)
  )
}
