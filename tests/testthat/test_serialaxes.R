context("test serialaxes")
library(tidyverse)
library(ggmulti)
pdf(NULL)

test_that("test serialaxes", {

  mapping <- aes(
    Sepal.Length = Sepal.Length,
    Sepal.Width = Sepal.Width,
    Petal.Length = Petal.Length,
    Petal.Width = Petal.Width,
    Species = Species
  )
  ######## parallel

  # test athetics colour, size, ...
  p <- ggplot(iris,
              mapping = mapping) +
    geom_serialaxes() +
    geom_serialaxes_hist() +
    geom_serialaxes_density(positive = FALSE, color = "red") +
    geom_serialaxes_quantile(color = "blue")
  b <- ggplot_build(p)
  expect_equal(b$plot$layers[[3]]$aes_params$colour, "red")

  # check mapping aesthetics
  p <- ggplot(iris,
              mapping = mapping) +
    geom_serialaxes(mapping = aes(colour = Species)) +
    geom_serialaxes_hist(positive = FALSE, mapping = aes(fill = Species)) +
    geom_serialaxes_density(color = "red") +
    geom_serialaxes_quantile(color = "blue")
  b <- ggplot_build(p)
  expect_equal(length(unique(b$data[[1]]$colour)), 3)
  expect_equal(length(unique(b$data[[2]]$fill)), 3)
  expect_false(b$plot$layers[[2]]$geom_params$positive)
})


test_that("test coord_serialaxes", {

  mapping <- aes(
    Sepal.Length = Sepal.Length,
    Sepal.Width = Sepal.Width,
    Petal.Length = Petal.Length,
    Petal.Width = Petal.Width,
    Species = Species
  )
  ######## parallel
  # coord_serialaxes
  p <- ggplot(iris, mapping = mapping) +
    geom_path(mapping = aes(colour = Species)) +
    coord_serialaxes(axes.position = c(1,5,7,9, 10))
  b <- ggplot_build(p)
  expect_equal(unique(b$data[[1]]$x), c(1,5,7,9, 10))

  q <- p +
    geom_density() +
    geom_histogram(fill = "blue") +
    geom_quantiles(colour = "red")
  b <- ggplot_build(q)
  expect_equal(b$plot$layers[[3]]$aes_params$fill, "blue")
  expect_equal(b$plot$layers[[4]]$aes_params$colour, "red")

  ## radial axes
  q <- p + coord_serialaxes(axes.layout = "radial")
  b <- ggplot_build(q)
  expect_true("CoordPolar" %in% class(b$plot$coordinates))

  # flip axes
  p <- ggplot(iris, mapping = aes(colour = Species)) +
    geom_path(alpha = 0.2) +
    geom_density(alpha = 0.5) +
    geom_histogram(fill = "blue", alpha = 0.3, position = "dodge2_") +
    geom_quantiles(colour = "red") +
    coord_serialaxes(axes.sequence = colnames(iris),
                     axes.position = c(1,5,7,9, 10), orientation = "x")
  b <- ggplot_build(p)
  expect_true("PositionIdentity_" %in% class(b$plot$layers[[2]]$position))
  expect_true("PositionDodge2_" %in% class(b$plot$layers[[3]]$position))

  # Legendre transformation
  p <- ggplot(iris, mapping = aes(colour = Species)) +
    geom_path(stat = "dotProduct", axes.sequence = colnames(iris),
              transform = legendre, scaling = "data") +
    coord_serialaxes(orientation = "x")
  b <- ggplot_build(p)
  expect_true(all(c("StatDotProduct", "StatSerialaxes") %in% class(b$plot$layers[[1]]$stat)))

  # axes.sequence
  ggplot(iris,
         mapping = aes(colour = factor(Species))) +
    geom_path(alpha = 0.2,
              axes.sequence = colnames(iris)[-5],
              stat = "serialaxes")  +
    geom_density(alpha = 0.8,
                 axes.sequence = colnames(iris)[c(1,3)],
                 stat = "serialaxesDensity") +
    coord_serialaxes() -> p
  b <- ggplot_build(p)
  expect_equal(unique(floor(b$data[[2]]$x)), c(1, 3))

  # histogram
  ### parallel
  ggplot(iris) +
    geom_path(alpha = 0.2) +
    geom_histogram(alpha = 0.8,
                   mapping = aes(fill = factor(Species))) +
    coord_serialaxes(axes.sequence = colnames(iris)) -> p
  x <- ggplot_build(p)
  expect_equal(length(x$plot$layers), 2)
  expect_true("CoordSerialaxes" %in% class(p$coordinates))

  ### radial
  p$coordinates$axes.layout <- "radial"
  x <- ggplot_build(p)
  expect_true("CoordPolar" %in% class(x$plot$coordinates))
})

test_that("fourier transformation dot product", {
  coords <- andrews(p = 4)
  expect_equal(round(range(coords$vector), 2), c(-3.14, 3.14))
  expect_equal(dim(coords$matrix), c(4, 150))
})


test_that("test duplicated axes", {

  ########## set mapping
  expect_warning(
    mapping <- aes(
      Sepal.Length = Sepal.Length,
      Sepal.Width = Sepal.Width,
      Sepal.Length = Sepal.Length,
      Sepal.Width = Sepal.Width,
      Species = Species
    )
  )

  p <- ggplot(iris, mapping = mapping) +
    geom_path() +
    coord_serialaxes()

  b <- ggplot_build(p)
  expect_equal(unique(b$data[[1]]$x), 1:5)

  p <- ggplot(iris, mapping = mapping) +
    geom_serialaxes()
  b <- ggplot_build(p)
  expect_equal(unique(b$data[[1]]$x), 1:3)

  #### mapping in layer
  mapping <- aes(
    Sepal.Length = Sepal.Length,
    Sepal.Width = Sepal.Width
  )
  expect_warning(
    p <- ggplot(iris, mapping = mapping) +
      geom_path(mapping = aes(
        Sepal.Length = Sepal.Length,
        Sepal.Width = Sepal.Width,
        Species = Species
      )) +
      coord_serialaxes()
  )
  b <- ggplot_build(p)
  expect_equal(unique(b$data[[1]]$x), 1:3)

  p <- ggplot(iris, mapping = mapping) +
    geom_serialaxes(mapping = aes(
      Sepal.Length = Sepal.Length,
      Sepal.Width = Sepal.Width,
      Species = Species
    ))
  b <- ggplot_build(p)
  expect_equal(unique(b$data[[1]]$x), 1:3)

  #### check axes.sequence
  p <- ggplot(iris, mapping = mapping) +
    geom_serialaxes(axes.sequence = colnames(iris))
  b <- ggplot_build(p)
  expect_equal(unique(b$data[[1]]$x), 1:5)

  p <- ggplot(iris, mapping = mapping) +
    geom_path() +
    coord_serialaxes(axes.sequence = colnames(iris))
  b <- ggplot_build(p)
  expect_equal(unique(b$data[[1]]$x), 1:5)

  expect_warning(
    p <- ggplot(iris) +
      geom_path(mapping = mapping) +
      coord_serialaxes(axes.sequence = colnames(iris)[-1])
  )

  b <- ggplot_build(p)
  expect_equal(unique(b$data[[1]]$x), 1:4)

  # test NAs
  withNA <- ggplot(airquality,
                   mapping = aes(Ozone = Ozone,
                                 Solar.R = Solar.R,
                                 Wind = Wind,
                                 Temp = Temp)) +
    geom_path() +
    geom_histogram() +
    geom_density(color = "red") +
    geom_quantiles(color = "blue") +
    coord_serialaxes()
  expect_warning(plot(withNA))
})
