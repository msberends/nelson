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

#' Kwaliteitsanalyse o.b.v. Nelson, Westgard of anders
#'
#' Met deze functie kan een gegevensreeks getoetst worden op kwaliteitsregels. Daarnaast wordt getoetst of er een significante lineaire trend bestaat.
#' @rdname qc
#' @param x Gegevensreeks.
#' @param dates Standaard is \code{rep(NA, length(x))}. Datums van gegevenswaarden.
#' @param m Standaard is \code{mean(x)}. Gemiddelde van \code{x} dat gebruikt wordt in de kwaliteitsregels.
#' @param s Standaard is \code{sd(x)}. Standaardafwijking van \code{x} dat gebruikt wordt in de kwaliteitsregels.
#' @param round Standaard is \code{2}. Aantal decimalen waarop alle getallen afgerond worden.
#' @param type Standaard is \code{"Nelson"}. Geldige opties zijn \code{"Nelson"}, \code{"Westgard"}, \code{"AIAG"}, \code{"Montgomery"} of \code{"Healthcare"}. Zie \strong{Rules list} (onderaan) voor meer informatie.
#' @param text.show Standaard is \code{TRUE}. Tekstanalyse weergeven.
#' @param plot.show Standaard is \code{TRUE}. Grafiek weergeven met gemiddelde (zie \code{\link[certedata]{mean}}), EWMA (zie \code{\link{ewma}}), 1-3x de standaardafwijking (zie \code{\link[certedata]{sd}}) en de waarnemingen met eventueel overtreden kwaliteitsregels.
#' @param title Standaard is \code{paste("Kwaliteitscontrole volgens", type)}. Titel van de toets en de grafiek. Tekst tussen sterretjes wordt cursief gemaakt.
#' @param subtitle Standaard is \code{""}. Ondertitel van de grafiek. Tekst tussen sterretjes wordt cursief gemaakt.
#' @param subtitle.colour Standaard is \code{colourpicker("certeblauw")}. Zie ook \code{\link{colourpicker}}. Kleur van de ondertitel.
#' @param plot.withoutrule1 Standaard is \code{FALSE}. Als regel 1 overtreden wordt (zie \code{\link{rule1}}), de plot opnieuw weergeven met data zonder deze waarnemingen.
#' @param plot.print Standaard is \code{TRUE}. De grafiek direct printen met de functie \code{print}. Met \code{FALSE} wordt de grafiek als ggplot-model geretourneerd.
#' @param x.lbl Standaard is \code{"Waarneming"}. De tekst op de x-as.
#' @param y.lbl Standaard is \code{"Waarde"}. De tekst op de y-as.
#' @param markdown Standaard is \code{FALSE}. Tekstanalyse afdrukken in markdown-formaat met \code{\link{tbl_markdown}}.
#' @param force Standaard is \code{FALSE}. Bij grote afwijkingen wordt de analyse gestaakt. Gebruik deze optie om de analyse te forceren.
#' @keywords nelson westgard qcc qc
#' @export
qc.test <- function(x,
                    dates = rep(NA, length(x)),
                    m = mean(x),
                    s = sd(x),
                    round = 2,
                    type = 'Nelson',
                    text.show = TRUE,
                    plot.show = TRUE,
                    colours = rainbow(8),
                    include_EWMA = TRUE,
                    title = paste('Quality Control according to', type),
                    subtitle = '',
                    subtitle.colour = colours[1],
                    plot.withoutrule1 = FALSE,
                    plot.print = TRUE,
                    x.lbl = 'Observation',
                    y.lbl = 'Value',
                    markdown = FALSE,
                    force = FALSE) {

  if (text.show == FALSE & plot.show == FALSE) {
    stop('`text.show` or `plot.show` must be TRUE.')
  }
  if (!tolower(type) %in% tolower(c('Nelson', 'Westgard', 'AIAG', 'Montgomery', 'Healthcare'))) {
    stop('Invalid type: ', type, '.')
  }
  if (length(boxplot.stats(x)$out) > 30 & force == FALSE) {
    stop('Data contains ', length(boxplot.stats(x)$out), ' outliers. Use `force = TRUE` to force analysis.')
  }
  if (length(boxplot.stats(x)$out) / length(x) > 0.1 & force == FALSE) {
    stop('Data consists of ', percent(length(boxplot.stats(x)$out) / length(x)), ' outliers. Use `force = TRUE` to force analysis.')
  }
  if (length(x) > 250 & force == FALSE) {
    stop('Data consists of ', length(x), ' observations. Use `force = TRUE` to force analysis.')
  }
  if (length(colours) != 8) {
    stop('`colours` must be a vector of length 8.')
  }

  require(ggplot2)

  hasdates <- !identical(dates, rep(NA, length(x)))
  data <- x
  markdown.bold <- ''
  markdown.italic <- ''
  markdown.header4 <- ''
  if (markdown == TRUE) {
    markdown.bold <- '**'
    markdown.italic <- '*'
    markdown.header4 <- '#### '
  }
  n.nelson1 <- NA
  n.total <- 0

  symbol.closed <- 16
  symbol.open <- 1

  print_exec <- function(rule_nr, afw, min_count) {
    if (length(afw) > 0) {
      n.total <<- n.total + length(afw)
      if (rule_nr == 1) {
        n.nelson1 <<- afw
        result <- data.frame(Observation = afw,
                             Value = round(x[afw], round),
                             stringsAsFactors = FALSE)
        if (hasdates == TRUE) {
          result <- cbind(result, date = format(dates[afw], '%Y-%m-%d'))
        }
        if (plot.show == TRUE) {
          p <<- p + geom_point(data = data.frame(x = afw,
                                                 y = data[afw]),
                               colour = colours[rule_nr],
                               shape = symbol.open,
                               size = 3,
                               stroke = 1.5)
        }
      } else {
        result <- data.frame(Observations = paste(afw, 'to', afw + min_count - 1),
                             FirstValue = round(x[afw], round),
                             stringsAsFactors = FALSE)
        if (hasdates == TRUE) {
          result <- cbind(result, FirstDate = format(dates[afw], '%Y-%m-%d'))
        }
        if (plot.show == TRUE) {
          for (i in 1:length(afw)) {
            # plot a line for every rule violation
            mx <- min(afw[i] + min_count - 1, length(x))
            p <<- p + geom_line(data = data.frame(x = afw[i]:mx,
                                                  y = data[afw[i]:mx]),
                                size = 1,
                                colour = colours[rule_nr])
            for (j in afw[i]:mx) {
              # plot a point for every line
              y.value <- data[j]
              symbol <- symbol.open
              if (j == afw[i]) {
                # first point must be closed symbol
                symbol <- symbol.closed
                # plot number (#) of rule
                y.distance = max(m + 3 * s, max(data)) - min(m - 3 * s, min(data))
                p <<- p +
                  geom_point(data = data.frame(x = j,
                                               y = y.value + (0.03 * y.distance)),
                             colour = 'gray25',
                             #color = colours[rule_nr],
                             shape = symbol.open,
                             size = 4.5) +
                  geom_text(data = data.frame(x = j,
                                              y = y.value + (0.03 * y.distance)),
                            label = rule_nr,
                            hjust = 0.5,
                            vjust = 0.25,
                            colour = 'gray25',
                            #colour = colours[rule_nr],
                            size = 3)
              }
              p <<- p + geom_point(data = data.frame(x = j,
                                                     y = y.value),
                                   colour = colours[rule_nr],
                                   shape = symbol,
                                   size = 2.5)
            }
          }
        }
      }

      if (text.show == TRUE) {
        cat(paste0(markdown.header4,
                   '#',
                   rule_nr,
                   ': ',
                   nelson.text(rule_nr, min_count)))
        if (markdown == FALSE) {
          cat('\n')
          print.data.frame(result, row.names = FALSE, right = FALSE)
        } else {
          tbl_markdown(result, align = 'c', format.dates = 'd mmm yyyy')
        }
        cat('\n')
      }
    }
  }

  fit <- lm(x ~ c(1:length(x)))
  var.p <- summary(fit)$coefficients[2, 4]
  if (var.p < 0.05) {
    significant.text <- 'Yes'
  } else {
    significant.text <- 'No'
  }
  var.F <- summary(fit)$fstatistic['value']
  var.R2 <- summary(fit)$r.squared

  header <- paste(paste0('\n', markdown.bold, title, markdown.bold),
                  '\n\nNo. of observations:  ', length(x),
                  '\nMean:                 ', round(mean(x), round),
                  '\nStandard deviation:   ', round(sd(x), round),
                  '\nCoeff. of variation:  ', round(cv(x), round),
                  '\nCoeff. of dispersion: ', round(cqv(x), round),
                  '\nSignificant trend:    ', paste0(markdown.bold, significant.text, markdown.bold,
                                                     ' (p = ',
                                                     round(var.p, round),
                                                     '; F = ',
                                                     round(var.F, round),
                                                     '; R^2 = ',
                                                     round(var.R2, round),
                                                     ')'),
                  '\n')
  if (markdown == TRUE) {
    header <- gsub('R^2', 'R^2^', header, fixed = TRUE)
    header <- gsub('\n', '\n\n',  header, fixed = TRUE)
  }
  if (text.show == TRUE) {
    cat(header)
  }

  # create plot evn before testing the observations
  if (plot.show == TRUE) {
    p <- ggplot(data.frame(x = c(1:length(data)), y = data), aes(x = x, y = y)) +
      theme_minimal() +
      theme(panel.grid.minor.y = element_blank()) +
      labs(title = title,
           subtitle = subtitle,
           x = x.lbl,
           y = y.lbl) +
      scale_y_continuous(
        limits = c(
          min(m - 3 * s, min(data)),
          max(m + 3 * s, max(data)))) +

      # observations:
      geom_point(colour = 'darkgray', shape = 4)

    if (include_EWMA == TRUE) {
      p <- p + # EWMA:
        geom_ribbon(aes(ymin = m, ymax = ewma(y, 0.9)), fill = rgb(0, 1, 0, 0.2)) +
        geom_line(aes(x = x, y = ewma(y, 0.9)), size = 0.25, colour = rgb(0, 0.5, 0, 0.5))
    }

    p <- p +
      # mean, plot on top of EWMA
      geom_hline(yintercept = m, size = 0.75, colour = rgb(0, 0.5, 0)) +
      # 1sd:
      geom_hline(yintercept = m + s, size = 0.75, colour = 'orange', linetype = 3) +
      geom_hline(yintercept = m - s, size = 0.75, colour = 'orange', linetype = 3) +
      # 2sd:
      geom_hline(yintercept = m + 2 * s, size = 0.75, colour = 'orange', linetype = 2) +
      geom_hline(yintercept = m - 2 * s, size = 0.75, colour = 'orange', linetype = 2) +
      # 3sd:
      geom_hline(yintercept = m + 3 * s, size = 0.75, colour = 'red', linetype = 2) +
      geom_hline(yintercept = m - 3 * s, size = 0.75, colour = 'red', linetype = 2)

    if (var.p < 0.05) {
      # with 95% conf interval (level = 0.95) and standard error (se = TRUE)
      p <- p +
        geom_smooth(colour = rgb(0, 170, 240, maxColorValue = 255),
                    size = 0.75,
                    level = 0.95,
                    method = "lm",
                    se = TRUE,
                    alpha = 0.15,
                    fill = rgb(0, 170, 240, maxColorValue = 255))
    }
  }

  if (text.show == TRUE) {
    cat(paste0('\n', markdown.italic, 'Rule violations:', markdown.italic, '\n'))
    if (markdown == TRUE) {
      cat('\n')
    }
  }

  if (tolower(type) == tolower('Nelson')) {
    print_exec(1, rule1(x, m, s), 1)
    print_exec(2, rule2(x, m, 9), 9)
    print_exec(3, rule3(x, 6), 6)
    print_exec(4, rule4(x, m, 14, FALSE), 14)
    print_exec(5, rule5(x, m, s, 3), 3)
    print_exec(6, rule6(x, m, s, 5), 5)
    print_exec(7, rule7(x, m, s, 15), 15)
    print_exec(8, rule8(x, m, s, 8), 8)
  } else if (tolower(type) == tolower('Westgard')) {
    print_exec(1, rule1(x, m, s), 1)
    print_exec(2, rule2(x, m, 9), 9)
    print_exec(5, rule5(x, m, s, 3), 3)
    print_exec(6, rule6(x, m, s, 5), 5)
  } else if (tolower(type) == tolower('AIAG')) {
    print_exec(1, rule1(x, m, s), 1)
    print_exec(2, rule2(x, m, 7), 7)
    print_exec(3, rule3(x, 6), 6)
    print_exec(4, rule4(x, m, 14, FALSE), 14)
    print_exec(5, rule5(x, m, s, 3), 3)
    print_exec(6, rule6(x, m, s, 5), 5)
    print_exec(7, rule7(x, m, s, 15), 15)
    print_exec(8, rule8(x, m, s, 8), 8)
  } else if (tolower(type) == tolower('Montgomery')) {
    print_exec(1, rule1(x, m, s), 1)
    print_exec(2, rule2(x, m, 8), 8)
    print_exec(3, rule3(x, 6), 6)
    print_exec(4, rule4(x, m, 14, FALSE), 14)
    print_exec(5, rule5(x, m, s, 3), 3)
    print_exec(6, rule6(x, m, s, 5), 5)
    print_exec(7, rule7(x, m, s, 15), 15)
    print_exec(8, rule8(x, m, s, 8), 8)
  } else if (tolower(type) == tolower('Healthcare')) {
    print_exec(1, rule1(x, m, s), 1)
    print_exec(2, rule2(x, m, 8), 8)
    print_exec(3, rule3(x, 6), 6)
    print_exec(5, rule5(x, m, s, 3), 3)
    print_exec(7, rule7(x, m, s, 15), 15)
  }

  if (text.show == TRUE & n.total == 0) {
    cat('No rule violations found.\n')
  }

  if (plot.show == TRUE) {
    suppressWarnings(
      if (plot.print == TRUE) {
        print(p)
      } else {
        return(p)
      }
    )
    if (plot.withoutrule1 == TRUE & !identical(NA, n.nelson1)) {
      x <- x[-n.nelson1]
      dates <- dates[-n.nelson1]
      plot.qcc(x = x,
               dates = dates,
               m = m,
               s = s,
               round = round,
               type = type,
               title = title,
               subtitle = paste('NOTE: tested without', length(n.nelson1), 'observation(s) that were >3 sd.'),
               x.lbl = x.lbl,
               y.lbl = y.lbl,
               colours = colours,
               force = TRUE
      )
    }
  }

}

#' Quality Control Chart (QCC)
#'
#' @export plot.qcc
plot.qcc <- function(x,
                     dates = rep(NA, length(x)),
                     m = mean(x),
                     s = sd(x),
                     round = 2,
                     type = 'Nelson',
                     colours = rainbow(8),
                     title = paste('Quality Control according to', type),
                     subtitle = '',
                     subtitle.colour = colours[1],
                     plot.withoutrule1 = FALSE,
                     plot.print = TRUE,
                     x.lbl = 'Observation',
                     y.lbl = 'Value',
                     markdown = FALSE,
                     force = FALSE) {
  qc.test(x = x,
          dates = dates,
          m = m,
          s = s,
          round = round,
          type = type,
          colours = colours,
          title = title,
          subtitle = subtitle,
          subtitle.colour = subtitle.colour,
          plot.withoutrule1 = plot.withoutrule1,
          plot.print = plot.print,
          x.lbl = x.lbl,
          y.lbl = y.lbl,
          markdown = markdown,
          force = force,
          text.show = FALSE,
          plot.show = TRUE)
}
