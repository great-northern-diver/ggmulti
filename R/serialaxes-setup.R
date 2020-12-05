serialaxes_setup_params <- function(data, params) {

  flipped_aes <- has_flipped_aes_(params$orientation)
  axes.sequence <- char2null(params$axes.sequence) %||% setdiff(names(data), default_aes())

  axes.position <- char2null(params$axes.position) %||% seq(axes.sequence)

  if(length(axes.position) != length(axes.sequence)) {
    rlang::warn(
      glue::glue(
        "The length of `axes.position` is {length(axes.position)}, which does not match the length of `axes.sequence` {length(axes.sequence)}"
      )
    )

    axes.position <- seq(axes.sequence)
  }

  params$axes.position <- axes.position
  params$axes.sequence <- axes.sequence
  params$flipped_aes <- flipped_aes
  params
}

serialaxes_setup_data <- function(data, params, setGroup = TRUE, as.data.frame = TRUE) {

  # syntactically valid names out of character vectors
  # colnames(data) <- make.names(colnames(data), unique = TRUE)

  n <- nrow(data)
  sequence <- names(params$axes.sequence) %||% params$axes.sequence

  if(any(sequence %in% c('x', 'y'))) {
    rlang::warn(
      glue::glue("The names for aesthetics 'x' and 'y' are meaningless in the serialaxes coordinate. Please consider to use more meaningful names?")
    )
  }

  d <- data %>%
    get_scaledData(sequence = sequence,
                   scaling = params$scaling,
                   reserve = TRUE,
                   as.data.frame = TRUE) %>%
    # Make syntactically valid names out of character vectors.
    tidyr::pivot_longer(cols = dplyr::all_of(make.names(sequence, unique = TRUE)),
                        names_to = "names",
                        values_to = ggplot2::flipped_names(params$flipped_aes)$x) %>%
    dplyr::mutate(!!ggplot2::flipped_names(params$flipped_aes)$y := rep(params$axes.position, n),
                  flipped_aes = params$flipped_aes) %>%
    dplyr::select(-names)

  if(setGroup) {
    d <- d %>%
      setup_group(params)
  }

  if(as.data.frame) {
    as.data.frame(d)
  } else
    d
}

serilaxes_compute_group <- function(self, data, scales, parent, flipped_aes, ...) {

  flip <- flipped_aes
  # FIX ME!
  # a hack
  # for some reason, 'x' and 'y' will be converted to 'density' in
  # `GeomSerialaxesDensity` and `GeomSerialaxesHist`
  position <- data[[ggplot2::flipped_names(flip = flip)$y]][1]

  ggplot2::ggproto_parent(parent, self)$compute_group(data, scales, flipped_aes = flip,
                                                      ...) %>%
    dplyr::mutate(.x = if(flip) position else x,
                  .y = if(flip) y else position)
}
