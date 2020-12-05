is.DotProduct <- function(stat) {
  "StatDotProduct" %in% class(stat)
}

is_dotProduct <- function(plot, any = TRUE) {
  layers <- plot$layers
  if(length(layers) == 0) return(FALSE)
  dotProduct <- vapply(layers, function(layer) is.DotProduct(layer$stat), logical(1L))
  if(any) any(dotProduct) else all(dotProduct)
}
