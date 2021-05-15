#' @rdname geom_serialaxes
#' @param scaling one of \code{data}, \code{variable}, \code{observation} or
#' \code{none} (not suggested the layout is the same with \code{data})
#' to specify how the data is scaled.
#' @param axes.position A numerical vector to determine the axes sequence position;
#' the length should be the same with the length of \code{axes.sequence} (or mapping \code{aesthetics}, see examples).
#' @export
stat_serialaxes <- function(mapping = NULL, data = NULL,
                            geom = "serialaxes", position = "identity",
                            ...,
                            axes.sequence = character(0L), merge = TRUE,
                            axes.position = NULL,
                            scaling = c("data", "variable", "observation", "none"),
                            na.rm = FALSE,
                            orientation = NA,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  if (merge) {
    axes.sequence_aes <- suppressWarnings(
      ggplot2::aes_all(axes.sequence)
    )
    axes.sequence_names <- names(axes.sequence)

    if(!is.null(axes.sequence_names)) {
      names(axes.sequence_aes) <- axes.sequence_names
    }

    mapping <- suppressWarnings(
      mbind(
        axes.sequence_aes,
        mapping
      )
    )
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSerialaxes,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      axes.sequence = axes.sequence,
      axes.position = axes.position,
      scaling = match.arg(scaling),
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname geom_serialaxes
#' @param scaling one of \code{data}, \code{variable}, \code{observation} or
#' \code{none} (not suggested the layout is the same with \code{data})
#' to specify how the data is scaled.
#' @param transform A transformation function, can be either \code{andrews}, \code{legendre} or
#' some other customized transformation functions.
#' @seealso Andrews plot \code{\link{andrews}}, Legendre polynomials \code{\link{legendre}}
#' @export
stat_dotProduct <- function(mapping = NULL, data = NULL,
                            geom = "path", position = "identity",
                            ...,
                            axes.sequence = character(0L), merge = TRUE,
                            scaling = c("data", "variable", "observation", "none"),
                            transform = andrews,
                            na.rm = FALSE,
                            orientation = NA,
                            show.legend = NA,
                            inherit.aes = TRUE) {

  if (merge) {
    axes.sequence_aes <- suppressWarnings(
      ggplot2::aes_all(axes.sequence)
    )
    axes.sequence_names <- names(axes.sequence)

    if(!is.null(axes.sequence_names)) {
      names(axes.sequence_aes) <- axes.sequence_names
    }

    mapping <- suppressWarnings(
      mbind(
        axes.sequence_aes,
        mapping
      )
    )
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatDotProduct,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(
      axes.sequence = axes.sequence,
      scaling = match.arg(scaling),
      na.rm = na.rm,
      transform = transform,
      orientation = orientation,
      ...
    )
  )
}

#' @rdname Stat-ggproto
#' @export
StatSerialaxes <- ggplot2::ggproto(
  "StatSerialaxes",
  ggplot2::Stat,
  default_aes = ggplot2::aes(colour = "grey20",
                             weight = NULL),
  extra_params = c("na.rm", "orientation"),

  setup_params = function(data, params) {
    params <- serialaxes_setup_params(data, params)
    params$quantiles <- params$quantiles %||% seq(0, 1, 0.25)
    params
  },
  setup_data = function(data, params) {

    n <- nrow(data)
    newData <- na.omit(data)
    nNew <- nrow(newData)

    if(nNew != n) {
      warning("Removed ", n - nNew,
      " rows containing missing values (stat_serialaxes).",
      call. = FALSE)
    }

    len <- length(params$axes.position)

    newData %>%
      serialaxes_setup_data(params, setGroup = FALSE) %>%
      dplyr::mutate(group = rep(seq(nNew), each = len)) %>%
      as.data.frame()
  },
  compute_group = function(data, scales, axes.sequence, scaling = "data", axes.position = NULL,
                           quantiles = seq(0, 1, 0.25),
                           na.rm = FALSE, flipped_aes = TRUE) {
    # Hack to ensure that axes.sequence, scaling and axes.position are detected as parameter
    data
  }
)

#' @rdname Stat-ggproto
#' @export
StatDotProduct <- ggplot2::ggproto(
  "StatDotProduct",
  StatSerialaxes,
  setup_params = function(self, data, params) {
    if(!is.null(params$axes.position)) {
      warning("`axes.position` will be omitted automatically in dot product transformation.",
              call. = FALSE)
    }
    ggplot2::ggproto_parent(StatSerialaxes, self)$setup_params(data, params)
  },
  setup_data = function(data, params) {

    n <- nrow(data)
    newData <- na.omit(data)
    nNew <- nrow(newData)

    if(nNew != n) {
      warning("Removed ", n - nNew,
              " rows containing missing values (stat_serialaxes).",
              call. = FALSE)
    }

    axes.sequence <- params$axes.sequence
    transform <- params$transform %||% andrews
    len_s <- length(axes.sequence)
    Trans <- do.call(transform,
                     c(params,
                       p = len_s))
    t <- Trans$vector
    m <- Trans$matrix

    len_t <- length(t)

    scaledData <- newData %>%
      get_scaledData(sequence = axes.sequence,
                     scaling = params$scaling,
                     reserve = TRUE,
                     as.data.frame = TRUE)

    newSeqName <- paste0(not_in_column_names(colnames = colnames(data),
                                             name = "V"),
                         seq(len_t))

    computeTrans <- (as.matrix(scaledData[, axes.sequence]) %*% m) %>%
      as.data.frame() %>%
      stats::setNames(nm = newSeqName)

    cbind(newData, computeTrans) %>%
      dplyr::select(-axes.sequence) %>%
      tidyr::pivot_longer(cols = dplyr::all_of(newSeqName),
                          names_to = "names",
                          values_to = ggplot2::flipped_names(params$flipped_aes)$x) %>%
      ggplot2::flip_data(params$flipped_aes) %>%
      dplyr::mutate(x = x,
                    y = rep(t, nNew),
                    acceptBoth = TRUE,
                    group = rep(seq(nNew), each = len_t),
                    flipped_aes = params$flipped_aes) %>%
      dplyr::select(-names) %>%
      ggplot2::flip_data(params$flipped_aes) %>%
      as.data.frame()
  },
  compute_group = function(data, scales, axes.sequence, scaling = "data",
                           transform = andrews, axes.position = NULL, quantiles = seq(0, 1, 0.25),
                           na.rm = FALSE, flipped_aes = TRUE) {
    # Hack to ensure that transform is detected as parameter
    data
  }
)
