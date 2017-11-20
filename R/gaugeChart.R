#' Gauge Chart
#'
#' Plot gauge charts using \link[grid:grid-package]{grid} graphics package.
#'
#' @param value numeric, value to display
#' @param min,max numeric, minimum and maximum value for gauge chart axis
#' @param sectors an object of \link{gaugeChartSectors-class} specifying the
#'  the sectors for gaug chart axis
#' @param header string, header of the gauge chart, see header section below
#' @param label string, label for the gauge chart axis
#' @param gp an object of class \link[grid]{gpar}
#' @param vp an \link[grid]{viewport} object defining the plot region for the
#'  gauge chart, see viewports section below
#' @param fmt opional format string, if supplied, the \code{value} and axis
#'  ranges \code{min}/\code{max} will be formatted using
#'  \code{\link[base]{sprintf}}
#'
#' @section Viewports:
#' The different parts of the gauge chart will be shown in separate
#' \link[grid]{viewport}s. All \link[grid]{viewport}s inherit from the parent
#' \link[grid]{viewport} \code{vp}. If \code{vp} is not given, the current
#' plot region will be taken as paren \link[grid]{viewport}.
#'
#' @section Header:
#' If \code{header} is given, the gaug chart's \link[grid]{viewport} will be
#' split in two \link[grid]{viewport}s. The upper one has a height of
#' \code{\link[grid:unit]{unit(4, "lines")}} and contains the header.
#'
#' @examples
#' \dontrun{
#' library('grid')
#' sectors <- gaugeChartSectors(200, 100, 0, rev = TRUE)
#'
#' pushViewport(gp = gpar(fontsize = 9, col = '#d7d7d7', fill = 'white'))
#'
#' gaugeChart(125, 0, 300, header = 'Tagesanmeldungen',
#'     sectors = sectors, label = 'on risk')
#'
#' popViewport()
#' }
#'
#' @export
#' @keywords hplot
gaugeChart <- function(value, min, max, sectors = gaugeChartSectors(), header,
                       label, gp = gpar(), vp, fmt = NULL) {
  if (!missing(vp)) {
    pushViewport(vp)
    on.exit(popViewport())
  }

  grid.rect()

  if (!missing(header)) {
    vpLayout <- grid.layout(2, 1,
                            heights = unit(c(4, 1), c('lines', 'null')))
    pushViewport(viewport(layout = vpLayout))
    on.exit(popViewport(1), add = TRUE)

    .gaugeHeader(header, vp = viewport(layout.pos.row = 1, name = 'header'))

    pushViewport(viewport(layout.pos.row = 2, name = 'chart'))
    on.exit(popViewport(1), add = TRUE)
  }

  col <- .gaugeBeamColor(sectors, value)
  .gaugeDisplay(value, min = min, max = max, label = label, fmt = fmt,
                gp.beam = gpar(col = col, fill = col))

}





#' @describeIn gaugeChart Plot the gauge chart's header.
#' @keywords internal
.gaugeHeader <- function(header, vp) {

  if (!missing(vp)) {
    pushViewport(vp)
    on.exit(popViewport())
  }
  grid.abline(0, 0)

  pushViewport(viewport(width = 0.90))
  on.exit(popViewport(), add = TRUE)

  grid.text(header, x = 0, just = 'left',
            gp = gpar(fontsize = 18, col = '#808080'))

}



#' @describeIn gaugeChart the main auxilliary function for plotting gauge
#'  charts.
#'
#' @param gp.beam an object of class \link[grid]{gpar} specifying the gauge
#'  chart's beam's color
#'
#' @section Viewports:
#' The gauge chart will be displayed in a separate viewport with aspect ratio
#' (i.e. width to height) of 2. The viewport will be fitted in 95% of the
#' parent viewport's width and height.
#'
#' The axis labels will be plotted below the viewport with an offset of
#' \code{\link[grid:unit]{unit(0.5, "lines")}}.
#'
#' @keywords internal
.gaugeDisplay <- function(x, min = 0, max = 1, label = NULL, fmt = NULL,
                          gp.beam = gpar(), vp) {

  if (!missing(vp)) {
    pushViewport(vp)
    on.exit(popViewport())
  }

  pushViewport(viewport(height = 0.90, width = 0.90, clip = 'off'))
  pushViewport(viewport(height = unit(1, 'npc') - unit(1.5, 'lines'),
                        y = unit(1.5, 'lines'), just = 'bottom'))
  on.exit(popViewport(2), add = TRUE)

  ht <- min(convertHeight(unit(1, 'npc'), 'inch'),
            convertWidth(unit(0.5, 'npc'), 'inch'))

  pushViewport(viewport(height = ht, width = 2 * ht, xscale = c(-1, 1),
                        clip = 'off'))
  on.exit(popViewport(), add = TRUE)

  .gaugeAxis(min, max, label = label, fmt = fmt)
  .gaugeBeam(stats::approx(c(min, max), c(0, 1), xout = x, rule = 2)$y,
             gp = gp.beam)

  if (is.null(fmt)) label <- as.character(x) else label <- sprintf(fmt, x)
  grid.text(label, y = 0, just = 'bottom',
            gp = gpar(fontsize = 24, fontface = 'bold', col = '#808080'))

}



#' @describeIn gaugeChart Plot the gauge chart's axis.
#'
#' @param r numeric, inner and outer radius for gauge chart axis
#' @param n number of steps for the shading of the axis background
#'
#' @keywords internal
.gaugeAxis <- function(min = 0, max = 1, label = NULL, fmt = NULL,
                       r = c(0.6, 1.0), n = 5) {
  .phi <- seq(0, pi, length.out = 50)

  .dr <- abs(diff(r)) / 4

  .r <- .dr + .dr * cos(seq(0, pi / 2, length.out = n + 1))
  .r <- mean(r) + c(-.r, rev(.r))

  pushViewport(viewport(xscale = c(-1, 1), yscale = c(0, 1),
                        gp = gpar(fontsize = 9)))
  on.exit(popViewport(1))

  col <- grDevices::colorRampPalette(c('#e1dfdf', '#edebeb', '#e1dfdf'))(2 * n + 1)

  for (i in seq_along(col)) {
    grid.polygon(c(.r[i] * cos(.phi), .r[i + 1] * cos(rev(.phi))),
                 c(.r[i] * sin(.phi), .r[i + 1] * sin(rev(.phi))),
                 default.units = 'native',
                 gp = gpar(col = NA, fill = col[i]))
  }

  grid.polygon(c(.r[1] * cos(.phi), .r[length(.r)] * cos(rev(.phi))),
               c(.r[1] * sin(.phi), .r[length(.r)] * sin(rev(.phi))),
               default.units = 'native',
               gp = gpar(col = col[1], fill = 'transparent'))

  if (!is.null(label)) {
    grid.text(label, unit(0, 'native'), unit(-1, 'lines'))
  }

  label <- c(min, max)
  if (is.null(fmt)) {
    label <- as.character(label)
  } else {
    label <- sprintf(fmt, label)
  }

  grid.text(label, unit(mean(r) * c(-1, 1), 'native'), unit(-1, 'lines'))

}



#' @describeIn gaugeChart Plot gauge chart's beam.
#'
#' @keywords internal
.gaugeBeam <- function(x, r = c(0.6, 1.0), gp = gpar()) {
  phi <- stats::approx(c(0, 1), c(pi, 0), xout = x)$y

  .phi <- seq(pi, 0, length.out = 50)
  .phi <- c(.phi[.phi > phi], phi)

  pushViewport(viewport(xscale = c(-1, 1), yscale = c(0, 1)))
  on.exit(popViewport(1))

  grid.polygon(c(r[1] * cos(.phi), r[2] * cos(rev(.phi))),
               c(r[1] * sin(.phi), r[2] * sin(rev(.phi))),
               default.units = 'native', gp = gp)
}
