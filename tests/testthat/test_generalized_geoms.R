context("test geoms")
library(ggmulti)
library(tidyverse)
library(ggplot2movies)
pdf(NULL)

############## geom_hist_ and geom_density_ ##############
test_that("test built geoms in ggplot", {

  data <- iris %>%
    pivot_longer(cols = -Species,
                 names_to = "Outer sterile whorls",
                 values_to = "x")
  p <- ggplot(data,
              mapping = aes(x = `Outer sterile whorls`,
                            y = x,
                            fill = Species)) +
    geom_hist_(scale.y = "group",
               as.mix = FALSE,
               prop = 0.6,
               alpha = 0.5) +
    geom_density_(scale.y = "group",
                  as.mix = FALSE,
                  prop = 0.6,
                  alpha = 0.5,
                  positive = FALSE)
  b <- ggplot_build(p)
  expect_equal(b$plot$layers[[1]]$geom_params$scale.y, "group")
  expect_equal(b$plot$layers[[2]]$geom_params$scale.y, "group")

  # flip aes; position dodge2
  p <- ggplot(data,
              mapping = aes(y = `Outer sterile whorls`,
                            x = x,
                            fill = Species)) +
    geom_hist_(scale.y = "group",
               position = "dodge2_",
               as.mix = FALSE,
               prop = 0.6,
               alpha = 0.5) +
    geom_density_(scale.y = "group",
                  as.mix = FALSE,
                  prop = 0.6,
                  alpha = 0.5,
                  positive = FALSE)
  b <- ggplot_build(p)
  expect_equal(class(p), c("gg", "ggplot"))
  # flip aes; position dodge;
  p <- ggplot(data,
              mapping = aes(y = `Outer sterile whorls`,
                            x = x,
                            fill = Species)) +
    geom_hist_(scale.y = "data",
               position = "dodge_",
               as.mix = TRUE,
               positive = FALSE)+
    geom_density_(scale.y = "group",
                  position = "stack_",
                  as.mix = TRUE)
  b <- ggplot_build(p)
  expect_true("PositionDodge_" %in% class(b$plot$layers[[1]]$position))
  expect_true("PositionStack_" %in% class(b$plot$layers[[2]]$position))

  # with NAs
  p <- airquality %>%
    ggplot(mapping = aes(x = factor(Month), y = Solar.R)) +
    geom_hist_() +
    geom_density_()
  expect_warning(plot <- ggplot_build(p))
})


test_that("test only x or y is provided", {

  p <- ggplot(diamonds, aes(depth, colour = cut)) +
    geom_density_() +
    xlim(55, 70)
  expect_warning(b <- ggplot_build(p))
  expect_equal(b$layout$panel_scales_x[[1]]$range$range, c(55, 70))

  # Stacked density plots: if you want to create a stacked density plot, you
  # probably want to 'count' (density * n) variable instead of the default
  # density

  # Loses marginal densities
  p <- ggplot(diamonds, aes(carat, fill = cut)) +
    geom_density_(position = "stack")
  b <- ggplot_build(p)
  expect_true("PositionStack" %in% class(b$plot$layers[[1]]$position))

  m <- ggplot(movies, aes(x = rating))
  m0 <- m +
    geom_histogram_(binwidth = 0.5) +
    scale_y_sqrt()
  b <- ggplot_build(m0)
  expect_true("ggplot_built" %in% class(b))
})

# test_that("reexport", {
#   p <- ggplot(sample_n(diamonds, 500), aes(price, colour = cut)) +
#     geom_freqpoly(binwidth = 50)
#   expect_true("GeomFreqpoly" %in% class(p$layers[[1]]$geom))
# })
