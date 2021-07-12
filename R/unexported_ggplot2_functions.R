################################ Un-exported functions in `ggplot2` ################################
# ggname <- utils::getFromNamespace("ggname", "ggplot2")
ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}

# message_wrap <- utils::getFromNamespace("message_wrap", "ggplot2")
message_wrap <- function (...) {
  msg <- paste(..., collapse = "", sep = "")
  wrapped <- strwrap(msg, width = getOption("width") -
                       2)
  message(paste0(wrapped, collapse = "\n"))
}

new_aes <- utils::getFromNamespace("new_aes", "ggplot2")
compute_density <- utils::getFromNamespace("compute_density", "ggplot2")
stack_var <- utils::getFromNamespace("stack_var", "ggplot2")
collide <- utils::getFromNamespace("collide", "ggplot2")
collide2 <- utils::getFromNamespace("collide2", "ggplot2")
pos_stack <- utils::getFromNamespace("pos_stack", "ggplot2")
rd_orientation <- utils::getFromNamespace("rd_orientation", "ggplot2")
pos_dodge <- utils::getFromNamespace("pos_dodge", "ggplot2")
pos_dodge2 <- utils::getFromNamespace("pos_dodge2", "ggplot2")
# is_mapped_discrete <- utils::getFromNamespace("is_mapped_discrete", "ggplot2")
is_mapped_discrete <- function (x) inherits(x, "mapped_discrete")
dapply <- utils::getFromNamespace("dapply", "ggplot2")
