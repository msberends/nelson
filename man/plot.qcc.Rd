% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot.qcc}
\alias{plot.qcc}
\title{Quality Control Chart (QCC)}
\usage{
\method{plot}{qcc}(x, dates = rep(NA, length(x)), m = mean(x),
  s = sd(x), round = 2, type = "Nelson", colours = rainbow(8),
  title = paste("Quality Control according to", type), subtitle = "",
  subtitle.colour = colours[1], plot.withoutrule1 = FALSE,
  plot.print = TRUE, x.lbl = "Observation", y.lbl = "Value",
  markdown = FALSE, force = FALSE)
}
\description{
Quality Control Chart (QCC)
}
