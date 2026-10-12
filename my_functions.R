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


t_test <- function(c1, c2, var.equal = TRUE, alpha = 0.05) {
  m1 <- mean(c1); s1 <- sd(c1)
  n1 <- length(c1)
  m2 <- mean(c2); s2 <- sd(c2)
  n2 <- length(c2)
  result = list() # Mi preparo la lista di risultato (vuota)

  # Primo caso: test di Student
  if (var.equal) {
    cat("Student's T-test\n")
    Sp <- sqrt( ((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n2-2))
    result$nu <- n1 + n2 -2
    denom <- (Sp*sqrt(1/n1+1/n2))
    result$t0 <- (m1-m2)/denom
    ci_width <- qt(alpha/2, result$nu, lower.tail=F) * denom

  # secondo caso: Test di Welch
  } else {
    cat("Welch's Test\n")
    result$nu <- ((s1^2/n1 + s2^2/n2)^2) / ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
    denom <- sqrt(s1^2/n1 + s2^2/n2)
    result$t0 <- (m1-m2) / denom
    ci_width <- qt(alpha/2, result$nu, lower.tail=F) * denom
  }

  # completo il risultato
  result$p.value <- 2 * pt(abs(result$t0), result$nu, lower.tail=F)
  result$m <- c(m1, m2)
  result$ci <- c(-1, 1)*ci_width + (m1-m2)

  # Stampo le informazioni
  cat(paste("nu:", result$nu, "\n"))
  cat(paste("t0:", result$t0, "\n"))
  cat(paste("p.value:", result$p.value, "\n"))
  cat(paste("Means:", result$m[1], result$m[2], "\n"))

  # ritorno la lista in modo invisibile
  invisible(result)
}
