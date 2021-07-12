#' @title Polygon glyph coordinates
#' @description polygon coordinates scaled to (0, 1)
#' @name polygon_glyph
#' @seealso \link{geom_polygon_glyph}
#' @export
#' @examples
#' if(require("grid")) {
#'   library(grid)
#'   grid.newpage()
#'   grid.polygon(x=(x_star + 1)/2,
#'                y=(y_star + 1)/2)
#'   grid.newpage()
#'   grid.polygon(x=(x_cross + 1)/2,
#'                y=(y_cross + 1)/2)
#'   grid.newpage()
#'   grid.polygon(x=(x_hexagon + 1)/2,
#'                y=(y_hexagon + 1)/2)
#'   grid.newpage()
#'   grid.polygon(x=(x_airplane + 1)/2,
#'                y=(y_airplane + 1)/2)
#'   grid.newpage()
#'   grid.polygon(x=(x_maple + 1)/2,
#'                y=(y_maple + 1)/2)
#' }
x_star <-
  c(-0.000864304235090734, 0.292999135695765, 0.949870354364736,
    0.474503025064823, 0.586862575626621, -0.000864304235090734,
    -0.586430423509075, -0.474070872947277, -0.949438202247191, -0.29256698357822)

#' @rdname polygon_glyph
#' @export
y_star <-
  -c(-1, -0.403630077787381, -0.308556611927398, 0.153846153846154,
     0.808556611927398, 0.499567847882455, 0.808556611927398,
     0.153846153846154, -0.308556611927398, -0.403630077787381)

#' @rdname polygon_glyph
#' @export
x_cross <-
  c(-0.258931143762604, -0.258931143762604, -0.950374531835206,
    -0.950374531835206, -0.258931143762604, -0.258931143762604,
    0.259651397291847, 0.259651397291847, 0.948934024776722,
    0.948934024776722, 0.259651397291847, 0.259651397291847)

#' @rdname polygon_glyph
#' @export
y_cross <-
  c(-0.950374531835206, -0.258931143762604, -0.258931143762604,
    0.259651397291847, 0.259651397291847, 0.948934024776722,
    0.948934024776722, 0.259651397291847, 0.259651397291847,
    -0.258931143762604, -0.258931143762604, -0.950374531835206)

#' @rdname polygon_glyph
#' @export
x_hexagon <-
  c(0.773552290406223, 0, -0.773552290406223, -0.773552290406223,
    0, 0.773552290406223)

#' @rdname polygon_glyph
#' @export
y_hexagon <-
  c(0.446917314894843, 0.894194756554307, 0.446917314894843,
    -0.447637568424085, -0.892754249495822, -0.447637568424085)

airplane <- function(boxxrange = c(-1, 1), boxyrange = c(-1, 1)) {

  airplane_coords <- c(30.8,0.5,57.4,27.1,85.6,16.5,89.9,17,78.7,30.9,183.5,27.7,
                       223.5,6.4,234.6,7.4,222.9,22.3,240,21.8,253.8,26.1,264.5,
                       33.5,276.2,39.4,283.1,42,286.5,50.6,282,57.5,273.5,63.9,
                       260.2,69.7,246.9,72.4,217.1,76.1,176.6,78.8,151.6,78.8,
                       88.8,105.9,62.7,95.8,117,70.8,87.7,70.8,73.9,68.1,56.3,
                       63.3,44.6,53.2,20.7,61.2,11.6,57.5,34,44.2)
  x_ap <- airplane_coords[seq(1, length(airplane_coords), by=2)]
  y_ap <- -airplane_coords[seq(2, length(airplane_coords), by=2)]

  new_coords <- scale_in_box(x_ap, y_ap,
                             boxxrange,
                             boxyrange)

  list(
    x_airplane = new_coords$x,
    y_airplane = new_coords$y
  )
}

scale_in_box <- function(x, y,
                         boxxrange = c(-1, 1),
                         boxyrange = c(-1 ,1)) {

  xrange <- range(x)
  yrange <- range(y)

  xyprop <- (diff(xrange) * diff(boxxrange))/(diff(yrange) * diff(boxyrange))

  if(xyprop > 1)
    boxyrange <- boxyrange/xyprop
  else
    boxxrange <- boxxrange * xyprop

  rescale <- function(x, range) {

    minx <- min(x, na.rm = TRUE)
    maxx <- max(x, na.rm = TRUE)
    range <- sort(range)

    (x - minx)/(maxx - minx) * diff(range) + range[1L]
  }

  list(
    x = rescale(x, boxxrange),
    y = rescale(y, boxyrange)
  )
}


#' @rdname polygon_glyph
#' @export
x_airplane <- airplane()$x_airplane

#' @rdname polygon_glyph
#' @export
y_airplane <- airplane()$y_airplane

maple <- function(boxxrange = c(-1, 1), boxyrange = c(-1, 1)) {

  maple_positive_x <- c(0, 78.043, 150, 121.862, 216, 248.934,
                        360, 335.172, 372, 191.921, 203, 27.929, 18)
  x_maple <- c(maple_positive_x, -rev(maple_positive_x))

  maple_positive_y <- c(400, 275.467, 302, 89.111, 171, 126.683,
                        137, 18.517, -13, -175.553, -244, -232.423, -406)
  y_maple <- c(maple_positive_y, rev(maple_positive_y))

  new_coords <- scale_in_box(x_maple, y_maple,
                             boxxrange,
                             boxyrange)

  list(
    x_maple = new_coords$x,
    y_maple = new_coords$y
  )
}

#' @rdname polygon_glyph
#' @export
x_maple <- maple()$x_maple

#' @rdname polygon_glyph
#' @export
y_maple <- maple()$y_maple
