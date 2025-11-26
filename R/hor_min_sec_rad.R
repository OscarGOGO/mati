#' Convert time (HH:MM:SS) to radians
#'
#' @param x character. vector with HH:MM:SS
#' @export
hor_min_sec_rad <- function(x) {
  parts <- strsplit(x, ":", fixed = TRUE)
  h <- as.numeric(sapply(parts, `[`, 1))
  m <- as.numeric(sapply(parts, `[`, 2))
  s <- as.numeric(sapply(parts, `[`, 3))
  x <- (h + m/60 + s/3600) * (pi/12)
  return(x)
}


