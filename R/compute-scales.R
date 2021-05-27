#' @import dplyr
compute_scales <- function(data, obj, params, ...) {
  class(obj) <- obj
  UseMethod("compute_scales", obj)
}

compute_scales.density <- function(data, obj, params) {

  scale.y <- params$scale.y
  prop <- params$prop

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
               y = density * scalingYprop * prop
             ) %>%
             dplyr::select(-scalingYprop)
         },
         "group" = {

           scalingProp <- data %>%
             dplyr::group_by(PANEL, y) %>%
             dplyr::summarise(scalingYprop = 1/max(density, na.rm = TRUE))

           data %>%
             dplyr::left_join(scalingProp, by = c("PANEL", "y")) %>%
             dplyr::mutate(
               y = density * scalingYprop * prop
             ) %>%
             dplyr::select(-scalingYprop)

           # data %>%
           #   dplyr::mutate(
           #     y = density * prop
           #   )
         },
         "variable" = {

           warning("`scale.y = variable` is deprecated now. Use `group` instead.",
                   call. = FALSE)

           scalingProp <- data %>%
             dplyr::group_by(PANEL, y) %>%
             dplyr::summarise(scalingYprop = 1/max(density, na.rm = TRUE))

           data %>%
             dplyr::left_join(scalingProp, by = c("PANEL", "y")) %>%
             dplyr::mutate(
               y = density * scalingYprop * prop
             ) %>%
             dplyr::select(-scalingYprop)

           # data %>%
           #   dplyr::mutate(
           #     y = density * prop
           #   )
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
  prop <- params$prop

  switch(scale.y,
         "data" = {

           scalingProp <- data %>%
             dplyr::group_by(PANEL, location) %>%
             dplyr::summarise(sum_n = sum(count, na.rm = TRUE),
                              max_y = max(y, na.rm = TRUE)) %>%
             dplyr::mutate(prop_n = sum_n/max(sum_n, na.rm = TRUE),
                           prop_y = 1/max(max_y)) %>%
             dplyr::transmute(PANEL = PANEL,
                              location = location,
                              scalingYprop = prop_n * prop_y)

           data %>%
             dplyr::left_join(scalingProp, by = c("PANEL", "location")) %>%
             dplyr::mutate(
               y = y * scalingYprop * prop
             ) %>%
             dplyr::select(-scalingYprop)

         },
         "group" = {

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
               y = y/max_height * prop
             ) %>%
             dplyr::select(-max_height)

         },
         "variable" = {

           warning("`scale.y = variable` is deprecated now. Use `group` instead.",
                   call. = FALSE)

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
               y = y/max_height * prop
             ) %>%
             dplyr::select(-max_height)

         },
         "none" = { # deprecated
           data
         }
  )
}
