`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

char2null <- function(x, warn = FALSE, message = "") {
  if(length(x) == 0) {
    if(warn) {
      warning(message,
              call. = FALSE)
    }
    return(NULL)
  }
  x
}

remove_null <- function(..., as_list = TRUE) {
  if(as_list)
    Filter(Negate(is.null),
           list(...)
    )
  else
    Filter(Negate(is.null), ...)
}

plot_range <- function(x = "x.range", panelParams, flip = FALSE) {
  if(flip) {
    x <- if(grepl("x", x)) {
      gsub("x", "y", x)
    } else {
      gsub("y", "x", x)
    }
  }

  panelParams[[x]] %||% c(0, 1)
}

utils::globalVariables(c("PANEL", "axes.sequence", "density", "group",
                         "height", "positive", "setup_mapping", "x", "y",
                         "ymax", "ymin", "max_density", "prop_density", "prop_n", "scalingYprop",
                         "sum_n", "max_y", "prop_y"))

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


not_in_column_names <- function(colnames, name = "", pattern = "") {

  vapply(name,
         function(x) {
           while(x %in% colnames) {
             x <- paste0(x, pattern)
           }
           x
         }, character(1L))
}

rescale <- function(x, range) {

  stopifnot(
    exprs = {
      is.numeric(x)
      is.numeric(range)
    }
  )

  len <- length(x)
  minx <- min(x, na.rm = TRUE)
  maxx <- max(x, na.rm = TRUE)
  range <- sort(range)
  if(minx == maxx)
    return(rep(range[1L], len))

  (x - minx)/(maxx - minx) * diff(range) + range[1L]
}
