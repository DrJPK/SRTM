#sample code for resolving names

resolve_name <- function(x){
  if (is.character(x) && length(x) == 1L) {
    x_name <- x
  } else {
    x_name <- rlang::as_string(rlang::ensym(x))
  }
  x_name
}
