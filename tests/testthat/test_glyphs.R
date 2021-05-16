context("test glyphs")
library(tidyverse)
library(ggmulti)
pdf(NULL)
test_that("test geom glyphs in ggplot", {

  # polygon glyph
  p <- ggplot(data = data.frame(x = 1:4, y = 1:4),
              mapping = aes(x = x, y = y)) +
    geom_polygon_glyph(polygon_x = list(x_star, x_cross, x_hexagon, x_airplane),
                       polygon_y = list(y_star, y_cross, y_hexagon, y_airplane),
                       colour = 'black', fill = 'red')
  ############ the coords of each polygons can be achieved by calling function `ggplot_build`
  build <- ggplot2::ggplot_build(p)
  polygon_x <- build$data[[1]]$polygon_x
  polygon_y <- build$data[[1]]$polygon_y
  expect_equal(length(polygon_x), length(polygon_y))

  ########### test polygon chars
  expect_equal(length(airplane()$x_airplane), length(airplane()$y_airplane))
  expect_equal(length(maple()$x_maple), length(maple()$y_maple))


  # serialaxes glyph
  p <- ggplot(data = iris,
              mapping = aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
    geom_serialaxes_glyph(serialaxes.data = iris[, -5],
                          axes.layout = "radial")
  build <- ggplot2::ggplot_build(p)
  expect_equal(length(build$data[[1]]$serialaxes.data.Sepal.Length), 150)

  ############# serialaxes glyphs
  p <- ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_serialaxes_glyph(serialaxes.data = iris[, -5],
                          axes.sequence = colnames(iris)[c(1,3,4)],
                          scaling = "data",
                          show.axes = TRUE,
                          bboxcolour = alpha("black", 0.2),
                          show.enclosing = TRUE)
  p
  expect_equal(class(p), c("gg", "ggplot"))

  p <- ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
    geom_serialaxes_glyph(serialaxes.data = iris[, -5],
                          axes.layout = "radial",
                          axes.sequence = colnames(iris)[c(1,3,4)],
                          scaling = "observation",
                          show.axes = TRUE,
                          bboxcolour = alpha("black", 0.2),
                          show.enclosing = TRUE)
  p
  expect_equal(class(p), c("gg", "ggplot"))

  ### test warning
  p <- ggplot(data = iris,
              mapping = aes(x = Sepal.Length, y = Sepal.Width)) +
    geom_serialaxes_glyph(serialaxes.data = iris[, -5],
                          axes.sequence = c(colnames(iris), "a", "b"))
  expect_warning(print(p))


  p <- ggplot(data = iris,
              mapping = aes(x = Sepal.Length, y = Sepal.Width)) +
    geom_image_glyph()
  b <- ggplot_build(p)
  expect_true("GeomPoint" %in% class(p$layers[[1]]$geom))
})
