context("test position")
library(tidyverse)
library(ggmulti)

pdf(NULL)
test_that("test position", {
  ### dodge
  data <- iris %>%
    pivot_longer(cols = -Species,
                 names_to = "Outer sterile whorls",
                 values_to = "x")
  p <- ggplot(data,
              mapping = aes(x = `Outer sterile whorls`,
                            y = x,
                            fill = Species))
  q <- p +
    stat_hist_(prop = 0.5,
               position = position_dodge2_(preserve = "single"))
  b <- ggplot_build(q)
  expect_true("PositionDodge2_" %in% class(b$plot$layers[[1]]$position))

  q <- p +
    stat_hist_(prop = 0.5,
               position = position_dodge_(preserve = "single"))
  b <- ggplot_build(q)
  expect_true("PositionDodge_" %in% class(b$plot$layers[[1]]$position))

  ### stack_
  q <- p +
    stat_density_(prop = 0.5,
                  position = position_stack_(reverse = TRUE))
  b <- ggplot_build(q)
  expect_true("PositionStack_" %in% class(b$plot$layers[[1]]$position))

  ### identity_
  q <- p +
    stat_density_(prop = 0.5,
                  position = position_identity_())
  b <- ggplot_build(q)
  expect_true("PositionIdentity_" %in% class(b$plot$layers[[1]]$position))
})
