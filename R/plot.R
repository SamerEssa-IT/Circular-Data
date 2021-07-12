library(tibble)
library(ggplot2)
library(ggforce)

dir <- c(43, 45, 52, 61, 75, 88, 88, 279, 357, 88, 88, 74, 45, 76, 76, 345, 2, 872, 872, 872, 872, 872, 872, 872, 872)

circularPlot <- function(directions, diagramRadius = 1, datapointRadius = 0.02,
                         xlimits = c(-2, 2), ylimits = c(-2, 2), ticks = 4,
                         diagramcolor = "black", datapointcolor = "black") {
  count <- table(directions)
  tickangles <- seq(0, 2 * pi, length.out = ticks + 1)[-(ticks + 1)]
  x <- 0
  y <- 0
  for (i in seq_along(count)) {
    angle <- (as.integer(names(count[i])) * pi / 180) %% (2 * pi)
    for (j in seq_len(count[i])) {
      range <- (diagramRadius + (j - 1) * (3 * datapointRadius))
      x <- c(x, cos(angle) * range)
      y <- c(y, sin(angle) * range)
    }
  }

  points <- tibble(x = x,
                   y = y,
                   r = c(diagramRadius, rep(datapointRadius, length(x) - 1)),
                   a = c(0, rep(1, length(x) - 1)),
                   c = c(diagramcolor, rep(datapointcolor, length(x) - 1)))

  lines <- tibble(x = rep(cos(tickangles), each = 2) * c(diagramRadius, diagramRadius * 0.92),
                  y = rep(sin(tickangles), each = 2) * c(diagramRadius, diagramRadius * 0.92),
                  group = rep(tickangles, each = 2))

  labels <- tibble(x = cos(tickangles) * (diagramRadius * 0.84),
                   y = sin(tickangles) * (diagramRadius * 0.84),
                   label = round(seq(0, 360, length.out = ticks + 1)[-(ticks + 1)], 1))

  ggplot() +
    geom_circle(aes(x0 = x, y0 = y, r = r, fill = c, alpha = a), data = points, show.legend = F) +
    geom_point(aes(x = 0, y = 0, shape = 3)) +
    xlim(xlimits) +
    ylim(ylimits) +
    scale_shape_identity() +
    scale_colour_identity() +
    scale_fill_identity() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    coord_fixed() +
    geom_line(data = lines, aes(x = x, y = y, group = group)) +
    geom_text(data = labels, aes(x = x, y = y, label = label))
}

circularPlot(dir, ticks = 20, datapointRadius = 0.02)