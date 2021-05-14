# ggmulti 1.0.2

  1. Fix a bug: in this version, all functions can handle data with `NA`s
  
  2. For functions `geom_hist_` and `geom_density2d`, if the input `x` and `y` are both numerical (neighter could be considered as the group variable), a warning will be given. 

# ggmulti 1.0.1

For polygon glyph, if the fill is NA (the fill color is none), it is a poly-line glyph; if the fill is not NA (some real color), it is a polygon glyph. In the old version `0.1.0`, the output can only be one type. For example, if `fill = c(NA, "red")`, then, NA dominates that both glyphs are polylines. In this version, it is more flexible that both polygon and polyline can appear on the same plot with one `geom_polygon_glyph` call.

# ggmulti 1.0.0

Date of Birth: 2021-01-05
