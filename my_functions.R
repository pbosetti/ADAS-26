# Script esempio di funzioni in R riutilizzabili
# Lezione ADAS del 5/10/2026

my_sum <- function(v, na.rm = FALSE) {
  s <- 0
  for (e in v) {
    # if (!is.na(e)) s <- s + e
    # oppure
    s <- s + ifelse(is.na(e), 0, e)
  }
  return(s)
}

my_f <- function(a, b = 2) {
  return(a * b)
}