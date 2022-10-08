## ----setup, include=FALSE, warning=FALSE, message=FALSE-----------------------
knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE,
                      message = FALSE,
                      fig.align = "center", 
                      fig.width = 7, 
                      fig.height = 6,
                      out.width = "60%", 
                      collapse = TRUE,
                      comment = "#>",
                      tidy.opts = list(width.cutoff = 65),
                      tidy = FALSE)
library(knitr)
set.seed(12314159)
imageDirectory <- "./images/glyph"
dataDirectory <- "./data/glyph"
path_concat <- function(path1, ..., sep="/") {
  # The "/" is standard unix directory separator and so will
  # work on Macs and Linux.
  # In windows the separator might have to be sep = "\" or 
  # even sep = "\\" or possibly something else. 
  paste(path1, ..., sep = sep)
}
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)

## ----glyph_ggplot, fig.width=10-----------------------------------------------
library(ggmulti)
library(nycflights13)
library(maps)

# Flight destinations
destinations <- nycflights13::airports %>% 
  dplyr::rename(dest = faa) %>% 
  dplyr::semi_join(nycflights13::flights, by = "dest") %>% 
  dplyr::mutate(tzone = gsub("America/", "", tzone)) %>% 
  dplyr::filter(lon > -151, 
                lat < 55)

# New York City coordinates
NY <- data.frame(
  lon = -73.935242,
  lat = 40.730610
)
US <- map_data("state")  %>% 
  ggplot(aes(long, lat)) +
  geom_polygon(mapping = aes(group = group), 
               color="black", fill="cornsilk") 
NYflightDestinationMap <- US + 
  geom_polygon_glyph(data = destinations,
                     mapping = aes(x = lon, y = lat),
                     fill = "pink",
                     # negate x to have each plane face west
                     polygon_x = -x_airplane, 
                     polygon_y = y_airplane,
                     alpha = 0.75) + 
  geom_polygon_glyph(data = NY,
                     mapping = aes(x = lon, y = lat),
                     polygon_x = x_star,
                     polygon_y = y_star, 
                     alpha = 0.75, 
                     fill = "blue")
NYflightDestinationMap

