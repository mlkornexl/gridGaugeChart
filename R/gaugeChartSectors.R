#' Gauge Chart Sectors
#'
#' Gaug chart sectors define colored backgrounds for gauge chart axis based
#' on KPI ranges \emph{success}, \emph{warning}, and \emph{danger}.
#'
#' @param success,warning,danger numeric, lower and upper limits for KPI ranges
#'  \emph{success}, \emph{warning}, and \emph{danger}, see range definition
#'  section for details
#' @param colors color definition for background of KPI ranges; see colors
#'  section for details@details
#' @param  rev logical, if TRUE, the order of KPI ranges is reverted; see
#'  range definition section for the effects
#'
#' @section Range Definition:
#' The sectors will be ordered according to \emph{success}, \emph{warning},
#' and \emph{danger}. If \code{rev = TRUE} the order will be reverted.
#'
#' Range definition \code{success}, \code{warning}, or \code{danger} can be
#' given either as numeric dupel with lower and upper limit. If they are given
#' by a single number, this will be assumed to be the lower limit.
#'
#' @section Colors:
#' Colors may be specified by a vector of any of the three kinds of \code{R}
#' color specifications, i.e. either a color name (see
#' \code{\link[grDevices]{colors}()} for a list of colors), a hexidecimal
#' string of the form \code{"#rrggbb"} or \code{"#rrggbbaa"} (see
#' \code{\link[grDevices]{rgb}()}), or a positive integer \code{i} meaning
#' \code{\link[grDevices]{palette}()[i]}. The colors can either be named
#' according to the KPI ranges or given using the same sequence as the input
#' of the KPI ranges.
#'
#' @aliases gaugeChartSectors-class
#'
#' @keywords classes
#'
#' @export
gaugeChartSectors <- function(success, warning, danger, colors = c(),
                              rev = FALSE) {
  sectors <- list()
  if (!missing(success)) sectors$success <- success
  if (!missing(warning)) sectors$warning <- warning
  if (!missing(danger)) sectors$danger <- danger
  if (rev) sectors <- rev(sectors)

  .colors <- c(success = '#70c153', warning = '#f59553', danger = '#ef4064')
  if (length(colors) == 0) {
    colors <- .colors[names(sectors)]
  } else if (is.null(names(colors))) {
    colors <- rep(colors, length.out = length(sectors))
    if (rev) colors <- rev(colors)
    names(colors) <- names(sectors)
  } else {
    colors <- c(colors, .colors[setdiff(names(.colors), names(colors))])
    colors <- colors[names(sectors)]
  }

  attr(sectors, 'colors') <- colors
  class(sectors) <- 'gaugeChartSectors'

  return(sectors)

}


#' @describeIn gaugeChartSectors With \code{.gaugeBeamColor} the background
#'  color of the KPI range (\emph{success}, \emph{warning}, or \emph{danger})
#'  given by \code{x} will be returned.
#'
#' @param sectors a \code{gaugeSectors}-class object
#' @param x numeric, value of the KPI
#' @param default color specification if \code{x} is out side of defined
#'  KPI ranges
#'
#' @section Range Definition:
#' The respective KPI range for a given value \code{x} is calculated according
#' to:
#' \enumerate{
#'  \item Select the KPI range with lower bound less or equal to value \code{x}
#'    and - if specified - the upper bound greater or equal to value \code{x}.
#'  \item If more than one range is returned with step 1., use the last of the
#'    KPI ranges (c.f. ordering of KPI ranges above).
#' }
#'
#' @keywords internal
.gaugeBeamColor <- function(sectors, x, default = '#808080') {
  if (length(sectors) == 0) return(default)

  ii <- vapply(sectors, function(range, x) {
    range <- c(range, rep(NA_real_, 2))
    c(x >= range[1], x <= range[2])
  }, x = x, FUN.VALUE = logical(2))

  col <- attr(sectors, 'colors')[apply(ii, 2, all, na.rm = TRUE)]
  if (length(col) >= 1) {
    col <- col[length(col)]
  } else {
    col <- default
  }

  return(col)

}
