context("test stat")
library(tidyverse)
library(ggmulti)
library(ggplot2movies)

pdf(NULL)
test_that("test stat", {
  ### histogram
  data <- iris %>%
    tidyr::pivot_longer(cols = -Species,
                 names_to = "Outer sterile whorls",
                 values_to = "x")
  p <- ggplot(data,
              mapping = aes(x = `Outer sterile whorls`,
                            y = x,
                            fill = Species))
  q <- p + stat_hist_(prop = 0.5)
  b <- ggplot_build(q)
  expect_equal(b$plot$layers[[1]]$geom_params$prop, 0.5)

  q <- p + stat_bin_(scale.x = c(0, 1))
  b <- ggplot_build(q)
  expect_true(max(b$data[[1]]$y) < 1 + 1e-6)

  q <- ggplot(data,
              mapping = aes(x = `Outer sterile whorls`,
                            y = Species)) +
    stat_count_()
  b <- ggplot_build(q)
  expect_true("ggplot_built" %in% class(b))

  ### density
  p <- ggplot(data,
              mapping = aes(x = `Outer sterile whorls`,
                            y = x,
                            fill = Species))
  q <- p + stat_density_(colour = NA, scale.y = "group")
  b <- ggplot_build(q)
  expect_equal(b$plot$layers[[1]]$geom_params$scale.y, "group")

  ### test error
  q <- p + stat_density_(colour = NA, scale.x = c('foo'))
  expect_error(ggplot_build(q))

  ### serialaxes
  p <- ggplot(iris,
              mapping = aes(Sepal.Length = Sepal.Length,
                            Sepal.Width = Sepal.Width,
                            Petal.Length = Petal.Length,
                            Petal.Width = Petal.Width,
                            colour = Species))

  q <- p + stat_serialaxes()
  b <- ggplot_build(q)
  expect_equal(unique(b$data[[1]]$x), c(1,2,3,4))

  ### fourier
  q <- p + stat_dotProduct(transform = legendre)
  b <- ggplot_build(q)
  expect_equal(max(b$data[[1]]$x), 1)
  expect_equal(min(b$data[[1]]$x), -1)

  ### serialaxes_density
  q <- p + stat_serialaxes_density()
  b <- ggplot_build(q)
  expect_true("PositionStack_" %in% class(b$plot$layers[[1]]$position))


  ### serialaxes_hist
  q <- p + stat_serialaxes_hist(position = "dodge_")
  b <- ggplot_build(q)
  expect_equal(max(b$data[[1]]$y), 1)
  expect_equal(min(b$data[[1]]$y), 0)

  ### serialaxes_quantile
  q <- p + stat_serialaxes_quantile(quantiles = c(0.25, 0.5, 0.75),
                                    colour = c("red", "green", "blue"))
  b <- ggplot_build(q)
  expect_equal(length(unique(b$data[[1]]$group)), 3)
})
