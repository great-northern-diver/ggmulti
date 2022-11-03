# ggmulti 1.0.6

Bug fix:

- Bug 1

  * Error: `geom_hist_` fails and provides unexpected warnings

  * Reason: the class name of the discrete variable in `ggplot2` is changed from `mapped_discrete` to `ggplot2_mapped_discrete`. Therefore, many binary checks by `is_mapped_discrete()` does not pass.
  
- Bug 2

  * Error: `stat_count_` fails to draw bins
  
  * Reason: a default setting of the bin width of the new version `ggplot2::stat_count` is recently set in `set_params`; when we replicate `set_params` of the old version `ggplot2`, the bin width is not set.

# ggmulti 1.0.5

Minor changes on documentations

# ggmulti 1.0.4

  1. For the radial coordinate, the serial histogram and density are displayed incorrectly (I suppose the engine, `ggplot2::coord_polar` makes some changes). So, in this version, these features are unavailable for now. Hopefully, we can bring them back in the next release. 
  
  2. Give meaningful content to the size of each non-primitive glyph. 
  
      + polygon glyph: the default size is 0.5 with unit "cm". The size of the polygon glyph is the "size" multiplies the polygon coordinates.
      
      + serial axes glyph: the default size is 1.5 with unit "cm". 
      
          - If it is the "radial" coordinate, the size is the diameter of the enclosing;
          
          - If it is the "parallel" coordinate, the size is the width of the enclosing and the aspect ratio of height and width is 1:2.
          
      + image glyph: the default size is 1 with unit "cm". The height of the image is determined by `imageheight * size` and the width of the image is `imagewidth * size`.
      
  3. Export a data fame `NBAstats2021` which contains 30 observations and 42 statistics summaries, e.g. team names, points per game, etc. 

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
