# fun_max <- function(x, ...) {
#   x[1] == max(x, ...)
# }

# function for checking if x is the maximum in a rolling window
fun_max <- function(x, win_size, ...) {
  x[win_size] == max(x, ..., na.rm = TRUE)
}
# function for checking if x is the maximum in a rolling window
fun_min <- function(x, win_size, ...) {
  x[win_size] == min(x, ..., na.rm = TRUE)
}

