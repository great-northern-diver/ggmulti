########### copy the following code from loon ################
# to remove the dependency
#' @title scale data
#' @description It is mainly used in serial axes
#' @param data A data frame
#' @param sequence vector with variable names that defines the axes sequence.
#' If \code{NULL}, it will be set as the column names automatically.
#' @param scaling one of \code{data}, \code{variable}, \code{observation} or
#' \code{none} (not suggested the layout is the same with \code{data}) to specify how the data is scaled.
#' @param displayOrder the order of the display
#' @param reserve If \code{TRUE}, return the variables not shown in \code{sequence} as well;
#' else only return the variables defined in \code{sequence}.
#' @param as.data.frame Return a matrix or a data.frame
# loon::l_getScaledData
get_scaledData <- function(data,
                           sequence = NULL,
                           scaling = c("data", "variable", "observation", "none"),
                           displayOrder = NULL,
                           reserve = FALSE,
                           as.data.frame = FALSE) {

  data <- as.data.frame(data)

  if(missing(data)) return(NULL)

  scaling <- match.arg(scaling)
  displayOrder <- displayOrder %||% seq(nrow(data))

  if(reserve && !is.null(sequence)) {

    colNames <- colnames(data)
    leftNames <- setdiff(colNames, sequence)

    leftData <- data[, leftNames]
    scaledData <- data[, sequence]

    d <- suppressWarnings(loon_get_scaledData(data = scaledData,
                                              sequence = sequence,
                                              scaling = scaling,
                                              displayOrder = displayOrder))
    rightNames <- colnames(d)

    # f return a matrix
    d <- cbind(leftData, d)
    colnames(d) <- c(leftNames, rightNames)
  } else {
    d <- suppressWarnings(loon_get_scaledData(data = data,
                                              sequence = sequence,
                                              scaling = scaling,
                                              displayOrder = displayOrder))
  }

  if(as.data.frame)
    as.data.frame(d, stringsAsFactors = FALSE)
  else
    as.matrix(d)
}

# loon default `get_scaledData`
# no dependency to loon any more
loon_get_scaledData <-  function(data,
                                 sequence = NULL,
                                 scaling = c("data", "variable", "observation", "none"),
                                 displayOrder = NULL) {

  # data is the original data set
  if(missing(data) || is.null(data)) return(NULL)

  if(is.null(displayOrder)) displayOrder <- seq(nrow(data))

  if(!is.null(sequence)) {

    if(!all(sequence %in% colnames(data))) {

      colNames <- colnames(data)

      if(!all(sequence %in% colNames)) {
        warning("The sequence names, ",
                setdiff(sequence, colNames),
                ", are not found in the data.",
                call. = FALSE)
        sequence <- intersect(sequence, colNames)
      }
    }

    data <-  data[, sequence]
  }

  scaling <- match.arg(scaling)

  is_char <- FALSE
  is_factor <- FALSE
  is_logical <- FALSE

  dat <- sapply(data,
                function(x) {
                  # `<<-` is used inside the function of `sapply`
                  # such operation only changes vars of my own namespace (i.e. `loon_get_scaledData`, etc)
                  # and global environment will not be affected.
                  # The main reason is to avoid the heavy `for` loop
                  if(is.numeric(x)) x
                  else if(is.character(x)) {
                    is_char <<- TRUE
                    as.numeric(as.factor(x))
                  } else if (is.factor(x)) {
                    is_factor <<- TRUE
                    as.numeric(x)
                  } else if(is.logical(x)) {
                    is_logical <<- TRUE
                    as.numeric(x)
                  } else stop("unknown data structure")
                })
  # give warning once
  if(is_char || is_factor || is_logical)
    warning("No numerical columns exist",
            call. = FALSE)

  if(length(displayOrder) == 1) {
    dat <- setNames(as.data.frame(matrix(dat, nrow = 1)), names(dat))
    if(scaling == "variable") {
      warning("Only one observation in serialAxesData, 'scaling' will be set as 'data' by default",
              call. = FALSE)
      scaling <- 'data'
    }
  }

  switch(scaling,
         "variable" = {
           minV <- apply(dat, 2, "min")
           maxV <- apply(dat, 2, "max")
           dat <- dat[displayOrder, ]
           t(
             (t(dat) - minV) / (maxV  - minV)
           )
         },
         "observation" = {
           minO <- apply(dat, 1, "min")
           maxO <- apply(dat, 1, "max")
           dat <- (dat - minO) / (maxO - minO)
           dat[displayOrder, ]
         },
         "data" = {
           minD <- min(dat)
           maxD <- max(dat)
           dat <- dat[displayOrder, ]
           (dat - minD)/ (maxD - minD)
         },
         "none" = {
           dat[displayOrder, ]
         })
}

#' # `geom_serialaxes` can be considered as a wrap of `geom_path`
#' # Following example illustrates how to convert a "widens" data to a "lengthens" data
#' # and use `geom_path` to construct the parallel axes
#' if(require("tidyr") && require("dplyr")) {
#'   # pivot iris from wide to long
#'   long_data <- iris %>%
#'     # set the scale of data
#'     get_scaledData(scaling = "variable",
#'                    as.data.frame = TRUE) %>%
#'     # add new variables
#'     dplyr::mutate(group = seq(dplyr::n()),
#'                   colour = Species) %>%
#'     # pivot data from wide to long
#'     tidyr::pivot_longer(cols = Sepal.Length:Species,
#'                         names_to = "x",
#'                         values_to = "y") %>%
#'     # change the variable type
#'     dplyr::mutate(
#'       x = unclass(factor(x, levels = colnames(iris))),
#'       colour = factor(colour)
#'     )
#'   # a glance
#'   long_data
#'   p <- ggplot(long_data,
#'               mapping = aes(x = x, y = y, colour = colour)) +
#'          geom_path(mapping = aes(group = group), alpha = 0.5)
#'   p
#'   # add density
#'   p + geom_density_(mapping = aes(fill = colour), alpha = 0.5)
#'   }
