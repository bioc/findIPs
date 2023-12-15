
## function used in plotIPs

lollipop <- function(x, y, topn = 5, ylim = NULL) {

  plot.new()
  if (is.null(ylim))
    ylim <- c(0, max(y) * 1.1)
  plot.window(xlim = c(min(x), max(x)), ylim = ylim, yaxs = "i")
  axis(1)
  axis(2)
  box()

  points(x, y, pch = 19, cex = 0.8)

  for (i in seq_along(x)) {
    lines(c(x[i], x[i]), c(0, y[i]), lwd = 0.7)
  }

  topn <- order(y, decreasing = TRUE)[seq_len(topn)]
  text(x[topn], y[topn], pos = 3, cex = 0.8, labels = topn)
  mtext("Observations", side = 1, line = 2.2)
  mtext("Scores", side = 2, line = 2.2)
}
