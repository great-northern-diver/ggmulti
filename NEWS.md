# ggmulti 1.0.3

  1. Fix a bug: for `geom_histogram_`, the scaling strategy (`scale.y` and `as.mix`) is not correct.
  
  2. The argument name `adjust` (adjust the proportional maximum height of the estimate, i.e. density, histogram, ...) is changed to `prop` since `adjust` has already been used in setting the `density`.

# ggmulti 1.0.2

  1. Fix a bug: in this version, all functions can handle data with `NA`s
  
  2. For functions `geom_hist_` and `geom_density2d`, if the input `x` and `y` are both numerical, neither will be considered as the group variable and a warning will be given. 
  
  3. `coord_radar` (deprecated) --> `coord_radial`; also un-export the function `coord_polar` (reduce the confusion)
  
  4. Update documentation (fix typos; make it more readable; point out potential risks of `coord_serialaxes`)

# ggmulti 1.0.1

For polygon glyph, if the fill is NA (the fill color is none), it is a poly-line glyph; if the fill is not NA (some real color), it is a polygon glyph. In the old version `0.1.0`, the output can only be one type. For example, if `fill = c(NA, "red")`, then, NA dominates that both glyphs are polylines. In this version, it is more flexible that both polygon and polyline can appear on the same plot with one `geom_polygon_glyph` call.

# ggmulti 1.0.0

Date of Birth: 2021-01-05
