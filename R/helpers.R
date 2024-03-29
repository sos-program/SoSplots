# helper functions for generating SoS plot panels for markdown reports
#  Author: Brigitte Dorner
#  March 2023

#' An extended version of do.call
#'
#' This version allows the user to specify a second set of args. If provided, the second set of args is added to the first set
#' and will override elements in the first set of args with the same name. Useful for letting the user pass on additional arguments to library
#' functions with lots of optional arguments.
#'
#' @param fun the function to call
#' @param args a named list that specifies the arguments for the function by name
#' @param override a named list with additional arguments; if arguments from the args list are included here,
#'                 the version in override will be used instead
#'
#' @return the response of the call to fun with the arguments specified
#' @examples
#' custom_plot <- function(x = 1:10, y = 1:10, additionalArgs = NULL) {
#'   do.call2(plot, args=list(x=x, y=y, xlab='x label', ylab='y label'), override=additionalArgs)
#' }
#' # show the default version of the plot
#' custom_plot()
#' # show a customized version
#' plotArgs <- list(main = 'A Custom Plot', xlab = 'new x label', ylab = 'new y label')
#' custom_plot(additionalArgs = plotArgs)
#'
#' @seealso \code{\link[base]{do.call}}
#'
#' @export
#'
do.call2 <- function(fun, args, override) {
  if (is.null(override)) do.call(fun, args) else do.call(fun, utils::modifyList(args, override))
}

# calc running mean of a time series
running_mean <- function(ts, period) {
  stats::filter(ts, filter = rep(1/period, times = period), sides = 1)
}

# generate a scaling factor and y-label for prettier y axes in plots of abundance
y_axis_specs <- function(ymax, tsName, useLog) {
  if (useLog) return(list(scale = 1, label = paste0('log(', tsName, ')')))
  magnitude <- floor(log10(ymax))
  if (magnitude < 3) return(list(scale = 1, label = tsName))
  if (magnitude < 4) return(list(scale = 100, label = paste0(tsName, " (100s)")))
  if (magnitude < 6) return(list(scale = 1000, label = paste0(tsName, " (1000s)")))
  return(list(scale = 10^6, label = paste0(tsName, " (Mill)")))
}

# if CU is cyclic, return dominant cycle year
get_dom_cycle_yr <- function(attr) {
  if(('Cyclic' %in% names(attr)) && ('Cyc_Dom_Year' %in% names(attr)) &&
      !is.na(attr$Cyclic) && !is.na(attr$Cyc_Dom_Year) && (attr$Cyclic == 'TRUE')) {
    as.numeric(attr$Cyc_Dom_Year)
  }
  else
    NULL
}

get_av_gen <- function(attr) {
  if(('AvGen' %in% names(attr)) && !is.na(attr$AvGen))
    attr$AvGen
  else
    NULL
}

# get a style from the list of style parameters; pull a list of generic style parameters
# (e.g., gpar$hline), or pull the specific style for a named element (e.g., gpar$hline$ABS_LBM)
get_style <- function(gpar, main, sub=NULL) {
  style <- NULL
  if (main %in% names(gpar)) {
    style <- gpar[[main]]
    if (sub %in% names(style)) style <- style[[sub]]
  }
  style
}

# create series of y-axis tickmarks for use in a log plot, based on largest value to be plotted
log_ticks <- function(ymax) {
  ticks <- log(10^c(0:8))
  names(ticks) <- c("1", "10", "100", "1k", "10k", "100k", "1M", "10M", "100M")
  ticks[ticks <= ymax]
}

# get min and max for an axis range, chosen so values fit nicely within the plot area
pretty_range <- function(presetRange, dataRange, nearest = 1) {
  # if presets are specified, let them override the range obtained from the data, otherwise use range obtained from the data
  lower <- ifelse('min' %in% names(presetRange), presetRange$min, min(dataRange))
  upper <- ifelse('max' %in% names(presetRange), presetRange$max, max(dataRange))
  # adjust range to contain nearest pretty tick mark
  lower <- nearest * floor(lower / nearest)
  upper <- nearest * ceiling(upper / nearest)
  c(lower, upper)
}

# create y values, log-transformed if requested
as_y <- function(vals, asLog) {
  vals <- as.numeric(vals)
  valid.vals <- !is.na(vals)
  if (asLog) {
    stopifnot(all(vals[valid.vals] >= 0))
    vals[vals == 0] <- 0.9
    vals[valid.vals] <- log(vals[valid.vals]) }
  vals
}

# draw a single status square at (x, y) in a timeline plot
draw_square <- function(x, y, status, gpar) {
  if (!is.na(status) && (status != 'NA')) {
    # note: Gottfried draws the rectangles as points, but this causes issues with scaling across devices
    # drawing actual rectangles gives more control to ensure that the cells always fill the available space and don't overlap,
    # irrespective of the length and number of metric series
    #do.call2(fun=graphics::points, args=list(x=x, y=y, pch=22, col=outline_color(status), bg=fill_color(status)), override=gpar$metric.square)
    d <- 0.49
    do.call2(fun=graphics::rect,
            args=list(xleft=x-d, ybottom=y-d, xright=x+d, ytop=y+d, col=fill_color(status), border=outline_color(status)),
            override=NULL)
    do.call2(fun=graphics::text, args=list(x, y, status_letter(status)), override=gpar$metric.text)
  }
}

# draw a row of status squares in a timeline plot
draw_metric_row <- function(y, m, startYr, endYr, gpar) {
  do.call2(fun=graphics::lines, args=list(x=c(startYr-0.5, endYr+0.5), y=c(y, y)), gpar$metric.line)
  #do.call2(fun=graphics::abline, args=list(h = y), gpar$metric.line) # note: this sometimes results in the line overlapping with the label
  for (yr in startYr:endYr) {
    myr <- as.character(yr)
    status <- ifelse(myr %in% names(m), as.character(m[myr]), NA)
    draw_square(yr, y, status, gpar) }
}

# get the font to use with graphics::par('font')
# allow this to be specified either as the font index or in plain text
get_font <- function(f) {
  if (is.null(f)) return(1)
  if (is.numeric(f)) return(f)
  switch(f, plain = 1, bold = 2, italic = 3, `bold italic` = 4, 1) }

# get a label from a string that might or might not contain a valid expression
# if the string contains a valid expression, return the parsed expression, otherwise
# just pass through the string.
# l: the string to use as a label
parse_label <- function(l) {
  label <- try(parse(text=l), silent=T)
  # if parse fails, just pass through l, otherwise return parsed expression
  if (inherits(label, "try-error")) l else label
}

# save extraction of a metric time series from a data frame
get_ts <- function(m, ds) {
  stopifnot(m %in% colnames(ds))
  stopifnot('Year' %in% colnames(ds))
  v <- ds[[m]]
  names(v) <- as.character(ds$Year)
  return(v)
}

# get proportion of values in v that match one of the elements in match
get_proportion <- function(v, match) { sum(v %in% match)/length(v) }

# add custom plot specs
add_specs <- function(default, additional) {
  if (is.null(additional)) return(default)
  specs <- default
  for (s in names(additional)) {
    if (is.null(specs[[s]]))
      specs[[s]] <- additional[[s]]
    else
      specs[[s]] <- utils::modifyList(specs[[s]], additional[[s]])
  }
  specs
}


