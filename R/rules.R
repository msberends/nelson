# ==================================================================== #
# TITLE                                                                #
# Nelson Rules For Quality Control                                     #
#                                                                      #
# AUTHORS                                                              #
# Berends MS (m.berends@certe.nl)                                      #
# Meijer BC (b.meijer@certe.nl)                                        #
#                                                                      #
# LICENCE                                                              #
# This R package is free software; you can redistribute it and/or      #
# modify it under the terms of the GNU General Public License          #
# version 2.0, as published by the Free Software Foundation.           #
# This R package is distributed in the hope that it will be useful,    #
# but WITHOUT ANY WARRANTY; without even the implied warranty of       #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the         #
# GNU General Public License for more details.                         #
# ==================================================================== #

#' Nelson's Quality Control Rules
#'
#' These rules are used for quality control (QC). Default values are set for Nelson's criteria, but they also support Westgard, AIAG, Montgomery and Healthcare QC rules.
#' @param x vector with values
#' @param m mean
#' @param s standard deviation
#' @param min.run minimal amount of sequential values before rule is triggered (defaults to Nelson's)
#' @param direction.mean a logical to indicate whether \emph{n} observations in a row must be tested for alternating in direction of the mean
#' @section Rules list:
#'   \tabular{lccccc}{
#'     \emph{Nelson (N), Westgard (W), AIAG (A), Montgomery (M), Healthcare (H):}             \tab  N \tab  W \tab  A \tab  M \tab  H\cr
#'     ---------------------------------------------------------------------------------------\tab ----- \tab ----- \tab ----- \tab  ----- \tab -----\cr
#'     \strong{#1} One point is more than 3 standard deviations from the mean.                \tab  1 \tab  1 \tab  1 \tab  1 \tab  1\cr
#'     \strong{#2} \emph{n} (or more) points in a row are on the same side of the mean        \tab  9 \tab  9 \tab  7 \tab  8 \tab  8\cr
#'     \strong{#3} \emph{n} (or more) points in a row are continually incr. or decr.          \tab  6 \tab  - \tab  6 \tab  6 \tab  6\cr
#'     \strong{#4} \emph{n} (or more) points in a row alternate in direction, incr. then decr.\tab 14 \tab -  \tab 14 \tab 14 \tab - \cr
#'     \strong{#5} \emph{n} - 1 out of \emph{n} points in a row are >2 sd from the mean       \tab  3 \tab  3 \tab  3 \tab  3 \tab  3\cr
#'     \strong{#6} \emph{n} - 1 out of \emph{n} points in a row are >1 sd from the mean       \tab  5 \tab  5 \tab  5 \tab  5 \tab - \cr
#'     \strong{#7} >= \emph{n} points in a row are within 1 sd of the mean                    \tab 15 \tab  - \tab 15 \tab 15 \tab 15\cr
#'     \strong{#8} >= \emph{n} points in a row outside 1 sd of the mean, in both directions   \tab  8 \tab  - \tab  8 \tab  8 \tab -
#'   }
#' @source Nelson LS. \strong{The Shewhart Control Chart—Tests for Special Causes}. Journal of Quality Technology [Internet]. Informa UK Limited; 1984 Oct;16(4):237–9. Available from: \url{http://dx.doi.org/10.1080/00224065.1984.11978921}
#' @keywords nelson rule qc qcc
#' @name rules
#' @aliases rule nelson
#' @rdname rules
#' @export
rule1 <- function(x, m = mean(x), s = sd(x)) {
  which(abs((x - m) / s) >= 3)
}

#' @rdname rules
#' @export
rule2 <- function(x, m = mean(x), min.run = 9) {
  if (length(x) < min.run) {
    return(integer(0))
  }
  n <- length(x)
  counts <- sign(x - m)
  result <- counts
  for (runlength in 2:min.run)
    result <- result + c(counts[runlength:n], rep(0, runlength - 1))
  which(abs(result) >= min.run)
}

#' @rdname rules
#' @export
rule3 <- function(x, min.run = 6) {
  if (length(x) < min.run) {
    return(integer(0))
  }
  n <- length(x)
  signs <- sign(c(x[-1], x[n]) - x)
  counts <- signs
  for (rl in 2:(min.run - 1)) {
    counts <- counts + c(signs[rl:n], rep(0, rl - 1))
  }
  # Between 6 observations you have 5 instances of increasing or decreasing. Therefore min.run - 1.
  which(abs(counts) >= min.run - 1)
}

#' @rdname rules
#' @export
rule4 <- function(x, m = mean(x), min.run = 14, direction.mean = FALSE) {
  if (length(x) < min.run) {
    return(integer(0))
  }
  n <- length(x)
  if (direction.mean == TRUE) {
    signs <- sign(x - m)
  } else {
    signs <- sign(c(x[-1],x[n]) - x)
  }
  counts <- signs
  fac <- -1
  for (rl in 2:min.run) {
    counts <- counts + fac * c(signs[rl:n], rep(0, rl - 1))
    fac <- -fac
  }
  counts <- abs(counts)
  which(counts >= min.run)
}

#' @rdname rules
#' @export
rule5 <- function(x, m = mean(x), s = sd(x), min.run = 3) {
  if (length(x) < min.run) {
    return(integer(0))
  }
  n <- length(x)
  pos <- 1 * ((x - m) / s > 2)
  neg <- 1 * ((x - m) / s < -2)
  poscounts <- pos
  negcounts <- neg
  for (rl in 2:min.run) {
    poscounts <- poscounts + c(pos[rl:n], rep(0, rl - 1))
    negcounts <- negcounts + c(neg[rl:n], rep(0, rl - 1))
  }
  counts <- apply(cbind(poscounts, negcounts), 1, max)
  which(counts >= min.run - 1)
}

#' @rdname rules
#' @export
rule6 <- function(x, m = mean(x), s = sd(x), min.run = 5) {
  if (length(x) < min.run) {
    return(integer(0))
  }
  n <- length(x)
  pos <- 1 * ((x - m) / s > 1)
  neg <- 1 * ((x - m) / s < -1)
  poscounts <- pos
  negcounts <- neg
  for (rl in 2:min.run) {
    poscounts <- poscounts + c(pos[rl:n], rep(0, rl - 1))
    negcounts <- negcounts + c(neg[rl:n], rep(0, rl - 1))
  }
  counts <- apply(cbind(poscounts, negcounts), 1, max)
  which(counts >= min.run - 1)
}

#' @rdname rules
#' @export
rule7 <- function(x, m = mean(x), s = sd(x), min.run = 15) {
  if (length(x) < min.run) {
    return(integer(0))
  }
  n <- length(x)
  within <- 1 * (abs((x - m) / s) < 1)
  counts <- within
  for (rl in 2:min.run)
    counts <- counts + c(within[rl:n], rep(0, rl - 1))
  which(counts >= min.run)
}

#' @rdname rules
#' @export
rule8 <- function(x, m = mean(x), s = sd(x), min.run = 8) {
  if (length(x) < min.run) {
    return(integer(0))
  }
  n <- length(x)
  outofrange <- 1 * (abs((x - m) / s) > 1)
  counts <- outofrange
  for (rl in 2:min.run)
    counts <- counts + c(outofrange[rl:n], rep(0, rl - 1))
  which(counts >= min.run)
}

nelson.text <- function(rule, min.run) {
  if (rule == 1) {
    tekst <- 'Detect more than 3 standard deviations from the mean'
  }
  if (rule == 2) {
    tekst <- 'Detect {n} observations on same side of the mean'
  }
  if (rule == 3) {
    tekst <- 'Detecteer strikte toename of afname bij >= {n} waarnemingen op een rij'
  }
  if (rule == 4) {
    tekst <- 'Detecteer {n} waarnemingen op een rij alternerend in richting van het gemiddelde, of alternerend in toename en afname'
  }
  if (rule == 5) {
    tekst <- 'Detecteer {n-1} van de {n} >2 sd van gemiddelde in dezelfde richting'
  }
  if (rule == 6) {
    tekst <- 'Detecteer {n-1} van de {n} >1 sd van gemiddelde in dezelfde richting'
  }
  if (rule == 7) {
    tekst <- 'Detecteer >= {n} waarnemingen op een rij binnen 1 sd van het gemiddelde'
  }
  if (rule == 8) {
    tekst <- 'Detecteer >= {n} waarnemingen op een rij allen buiten 1sd'
  }
  tekst <- gsub('{n-1}', min.run - 1, tekst, fixed = TRUE)
  tekst <- gsub('{n}', min.run, tekst, fixed = TRUE)
  tekst
}
