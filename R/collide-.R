# a wrapper of collide
# to accommodate `geom_hist_` and `geom_density_`
collide_ <- function(data, width = NULL, name, strategy, collide.fun,
                     ..., check.width = TRUE, reverse = FALSE, positive = TRUE) {

  data$positive <- data$positive %||% positive

  dapply(data, "location", function(data) {
    data %>%
      as.data.frame() %>%
      # this is necessary, the reason is because after filtering
      # the data.frame object will be converted to a tibble object
      # the difference between tibble and data.frame is the returned object
      # for example:
      # dd <- data.frame(x = 1:4)
      # dd[, "x"] returns a vector
      # dt <- tibble(x = 1:4)
      # dt[, "x"] returns a tibble object
      # an issue may occur here.
      collide.fun(width = width, name = name, strategy = strategy,
                  ..., check.width = check.width, reverse = reverse) %>%
      dplyr::mutate(maxheight = max(ymax),
                    y = ifelse(positive, y + location, -y + location),
                    ymin = ifelse(positive, ymin + location, -ymin + location),
                    ymax = ifelse(positive, ymax + location, -ymax + location))
  })

  # d <- lapply(sort(unique(data$location)),
  #             function(i) {
  #
  #               data %>%
  #                 dplyr::filter(location == i) %>%
  #                 as.data.frame() %>%
  #                 # this is necessary, the reason is because after filtering
  #                 # the data.frame object will be converted to a tibble object
  #                 # the difference between tibble and data.frame is the returned object
  #                 # for example:
  #                 # dd <- data.frame(x = 1:4)
  #                 # dd[, "x"] returns a vector
  #                 # dt <- tibble(x = 1:4)
  #                 # dt[, "x"] returns a tibble object
  #                 # an issue may occur here.
  #                 collide.fun(width = width, name = name, strategy = strategy,
  #                             ..., check.width = check.width, reverse = reverse) %>%
  #                 dplyr::mutate(y = ifelse(positive, y + location, -y + location),
  #                               ymin = ifelse(positive, ymin + location, -ymin + location),
  #                               ymax = ifelse(positive, ymax + location, -ymax + location))
  #             })
  #
  # do.call(rbind, unify_columns(d))
}

unify_columns <- function(x) {

  if(is.data.frame(x)) return(x)
  if(!is.list(x)) stop(paste(deparse(substitute(x)), "is not a list"),
                       call. = FALSE)

  cols <- lapply(seq(length(x)),
                 function(i) {

                   data <- x[[i]]

                   if(!is.data.frame(data))
                     stop("The ", i, "th object in ", deparse(substitute(x)),
                          " is not a data frame",
                          call. = FALSE)
                   colnames(data)
                 })
  # To each dataset,
  # try to find the same columns
  newcols <- Reduce(intersect, cols)
  # get ready for `rbind`
  lapply(x,
         function(data) {
           data[, newcols]
         })
}
