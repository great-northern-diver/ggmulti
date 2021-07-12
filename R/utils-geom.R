has_flipped_aes_ <- function(orientation) {
  if(any(is.na(orientation)) || is.null(orientation)) {
    TRUE
  } else {
    switch(orientation,
           "x" = FALSE,
           "y" = TRUE)
  }
}

setup_group <- function(data, params) {

  data <- ggplot2::flip_data(data, params$flipped_aes)
  y <- data$y
  data$flipped_aes <- params$flipped_aes

  # input y is categorical
  if(!is.null(attributes(y)))
    return(ggplot2::flip_data(data, params$flipped_aes))

  # input y is numeric
  group <- data$group

  uni_y <- unique(y)
  uni_group <- unique(group)

  len_y <- length(y)

  potential_tot_group <- length(uni_y) * length(uni_group)

  if(potential_tot_group == 1)
    return(ggplot2::flip_data(data, params$flipped_aes))

  group_id <- 0
  newgroup <- rep(-1, len_y)

  for(i in seq(length(uni_y))) {
    match <- y == uni_y[i]
    # new group under certain y
    group_y <- group[match]
    g <- unclass(factor(group_y)) + group_id
    group_id <- group_id + length(unique(g))
    newgroup[match] <- g
  }

  data$group <- newgroup
  ggplot2::flip_data(data, params$flipped_aes)
}

valid_aes <- function(aes, data, k) {

  if(is.null(aes)) return(NULL)
  n <- nrow(data)
  lengh.out <- n * k

  if(length(aes) == 1 || length(aes) == lengh.out) {
    aes
  } else if(length(aes) == n) {
    rep(aes, each = k)
  } else {
    stop(deparse(substitute(aes)),
         " must be either length 1 or the same as the data ",
         lengh.out,
         call. = FALSE)
  }
}

valid_stat <- function(stat, layer) {
  any(grepl(stat, class(layer$stat), ignore.case = TRUE))
}

default_aes <- function(...) {

  # TODO
  # how to access these from a Geom object?
  defaults <- c("PANEL", "group", "label", "color", "col")

  args <- list(...)
  if(length(args) == 0) {
    # default settings
    args <- list(
      ggplot2::GeomPath,
      ggplot2::GeomBar,
      ggplot2::GeomText
    )
  }

  defaultAes <- sapply(args,
                       function(x) {
                         if(!inherits(x, "Geom"))
                           stop("It is not a Geom object",
                                call. = FALSE)

                         names(x$default_aes)
                       })
  unique(c(defaults, unlist(defaultAes)))
}

mbind <- function(new_mapping = aes(), mapping) {

  if (!missing(mapping) && !inherits(mapping, "uneval") &&
      !missing(new_mapping) && !inherits(new_mapping, "uneval")) {
    stop("Mapping should be created with `aes()`.",
         call. = FALSE)
  }

  new_aes(new_mapping %<-% mapping)
}

`%<-%` <- function(x, y) {
  if(is.null(names(x)) || is.null(names(y)))
    return(c(x,y))
  else {

    if(!is.list(x)) x <- as.list(x)
    if(!is.list(y)) y <- as.list(y)

    merged_list <- c(x, y)
    list_names <- names(merged_list)
    merged_list[duplicated(list_names, fromLast = TRUE)] <- NULL

    return(merged_list[unique(list_names)])
  }
}

is.waive <- function (x) inherits(x, "waiver")

has_positive <- function(data) {

  # Is orientation already encoded in data?
  if (!is.null(data$positive)) {
    not_na <- which(!is.na(data$positive))
    if (length(not_na) != 0) {
      return(data$positive[[not_na[1L]]])
    }
  }

  # default setting
  return(TRUE)
}

layered_real.axes.sequence <- function(axes.sequence, mapping = ggplot2::aes()) {

  # example: mapping = aes(x = x, y = y, z = z, colour = f), axes.sequence = c(x, r, k)
  # the real axes.sequence is x, y, z, x, r, k
  real.axes.sequence <- c(names(mapping), axes.sequence)
  # do not use `setdiff`
  # it will remove all the duplicated terms
  real.axes.sequence[!(real.axes.sequence %in% default_aes())]
}

canvas_real.axes.sequence <- function(old, new) {

  if(length(old) == 0) return(new)
  if(length(new) == 0) return(old)

  l <- lapply(unique(c(old, new)),
              function(x) {
                rep(x, max(c(sum(old %in% x), sum(new %in% x))))
              })
  unlist(l)
}

set_default_axes_sequence <- function(plot, object, layer = NULL) {

  if(!is.null(char2null(object$axes.sequence))) return(object$axes.sequence)

  axes.sequence <- layered_real.axes.sequence(NULL, plot$mapping)

  if(is.null(layer)) {

    for(l in plot$layers) {
      # get the real axes.sequence for this layer
      real.axes.sequence <- layered_real.axes.sequence(l$stat_params$axes.sequence, l$mapping)
      # merge with the previous axes.sequence
      ## rules:
      ## real axes.sequence in layer1: a b c d
      ## real axes.sequence in layer1: a b c c c
      ## real axes.sequence in layer3: a b e f
      ## the overall real axes.sequence is
      ## a b c c c d e f
      axes.sequence <- canvas_real.axes.sequence(axes.sequence,
                                                 real.axes.sequence)
    }

  } else {
    real.axes.sequence <- layered_real.axes.sequence(layer$stat_params$axes.sequence, layer$mapping)
    axes.sequence <- canvas_real.axes.sequence(axes.sequence,
                                               real.axes.sequence)
  }

  if(object$axes.layout == "radial") {
    # complete the closure
    # separate the first axes.sequence and the last one
    axes.sequence <- c(axes.sequence, axes.sequence[1])
  }

  axes.sequence
}

set_orientation <- function(layer, object) {
  orientation <- if(is.null(layer$stat_params$orientation) || any(is.na(layer$stat_params$orientation))) {
    object$orientation
  } else {
    layer$stat_params$orientation
  }
  orientation %||% NA
}

set_axes_position <- function(layer, object, axes, axes.sequence) {
  # axes are the canvas axes labels
  # axes.sequence is the axes labels for this layer
  layered_positions <- layer$stat_params$axes.position
  if(!is.null(layered_positions)) return(layered_positions)
  if(is.DotProduct(layer$stat)) return(NULL)

  all_positions <- object$axes.position %||% seq(axes)
  position <- all_positions[axes %in% axes.sequence]
  if(length(position) < length(axes.sequence)) {
    stop("Improper `axes.sequence` setting in `coord_serialaxes`.",
         call. = FALSE)
  }
  position
}

reset_position <- function(position) {

  pos_class <- class(position)
  if(any(grepl("_", pos_class))) return(position)

  tolower(gsub("Position", "", paste0(pos_class[1], "_")))
}


# axes_sequence_mapping <- function(layer, mapping) {
#
#   if(is.null(layer)) return(ggplot2::aes())
#
#   mappingNames <- names(mapping)
#   defaultAes <- unique(c(default_aes(), names(layer$geom$default_aes)))
#
#
#   layerMapping <- layer$mapping
#   layerMappingNames <- setdiff(names(layerMapping), defaultAes)
#   mappingNames <- setdiff(mappingNames, defaultAes)
#
#   mbind(mapping[mappingNames],
#         layerMapping[layerMappingNames])
# }
