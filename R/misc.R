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

# No export, no Rd
percent <- function(x, round = 1, force_zero = FALSE, ...) {
  val <- base::round(x * 100, digits = round)
  if (force_zero == TRUE & any(val == as.integer(val) & !is.na(val))) {
    val[val == as.integer(val)] <- paste0(val[val == as.integer(val)], ".", strrep(0, round))
  }
  pct <- base::paste0(val, "%")
  pct[pct == "NA%"] <- NA_character_
  pct
}

# Coefficient of variation (CV)
cv <- function(x, na.rm = TRUE) {
  stats::sd(x, na.rm = na.rm) / base::abs(base::mean(x, na.rm = na.rm))
}

# Coefficient of dispersion, or coefficient of quartile variation (CQV).
# (Bonett et al., 2006: Confidence interval for a coefficient of quartile variation).
cqv <- function(x, na.rm = TRUE) {
  fives <- stats::fivenum(x, na.rm = na.rm)
  (fives[4] - fives[2]) / (fives[4] + fives[2])
}

# Exponentially Weighted Moving Average (EWMA)
ewma <- function(x, lambda, m = mean(x)) {
  res <- m
  res.s <- vapply(x, function(xs) res <<- res * lambda + xs * (1 - lambda), 0)
  res.s
}
