#' @import dplyr scales
compute_scales <- function(data, obj, params, ...) {
  class(obj) <- obj
  UseMethod("compute_scales", obj)
}

compute_scales.density <- function(data, obj, params) {

  scale.y <- params$scale.y
  adjust <- params$adjust

  switch(scale.y,
         "data" = {

           scalingProp <- data %>%
             dplyr::group_by(PANEL, y) %>%
             dplyr::summarise(sum_n = sum(n, na.rm = TRUE),
                              max_density = max(density, na.rm = TRUE)) %>%
             dplyr::mutate(prop_n = sum_n/max(sum_n, na.rm = TRUE),
                           prop_density = 1/max(max_density)) %>%
             dplyr::transmute(PANEL = PANEL,
                              y = y,
                              scalingYprop = prop_n * prop_density)

           data %>%
             dplyr::left_join(scalingProp, by = c("PANEL", "y")) %>%
             dplyr::mutate(
               y = density * scalingYprop * adjust
             ) %>%
             dplyr::select(-scalingYprop)
         },
         "variable" = {

           scalingProp <- data %>%
             dplyr::group_by(PANEL, y) %>%
             dplyr::summarise(scalingYprop = 1/max(density, na.rm = TRUE))

           data %>%
             dplyr::left_join(scalingProp, by = c("PANEL", "y")) %>%
             dplyr::mutate(
               y = density * scalingYprop * adjust
             ) %>%
             dplyr::select(-scalingYprop)

         },
         "group" = { # deprecated

           data %>%
             dplyr::group_by(PANEL, group) %>%
             dplyr::mutate(
               y = scales::rescale(density, c(0, 1))
             )
         },
         "none" = { # deprecated
           data %>%
             dplyr::mutate(
               y = density,
             )
         }
  )
}

compute_scales.histogram <- function(data, obj, params) {

  scale.y <- params$scale.y
  adjust <- params$adjust

  switch(scale.y,
         "data" = {

           maxHeights <- data %>%
             dplyr::group_by(PANEL, location, x) %>%
             dplyr::summarise(height = sum(y, na.rm = TRUE)) %>%
             dplyr::ungroup() %>%
             dplyr::group_by(PANEL) %>%
             dplyr::summarise(max_height = max(height, na.rm = TRUE))

           data %>%
             dplyr::left_join(maxHeights, by = "PANEL") %>%
             dplyr::mutate(
               y = y/max_height * adjust
             ) %>%
             dplyr::select(-max_height)
         },
         "variable" = {

           maxHeights <- data %>%
             dplyr::group_by(PANEL, location, x) %>%
             dplyr::summarise(height = sum(y, na.rm = TRUE)) %>%
             dplyr::ungroup() %>%
             dplyr::group_by(location, PANEL) %>%
             dplyr::summarise(max_height = max(height, na.rm = TRUE))

           data %>%
             dplyr::left_join(y = maxHeights, by = c("location", "PANEL")) %>%
             dplyr::group_by(location) %>%
             dplyr::mutate(
               y = y/max_height * adjust
             ) %>%
             dplyr::select(-max_height)

         },
         "group" = { # deprecated

           rlang::abort("`group` is aborted in `geom_hist_`")
         },
         "none" = { # deprecated
           data
         }
  )
}
