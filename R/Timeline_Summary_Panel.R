#  Code for generating plots used in the Timeline Summary panel
#  Adapted from Gottfried Pestal's 5_Summary_Timeline_Plots.R code
#  Author: Brigitte Dorner
#  Jan 2024

require(graphics)

#-----------  Timeline Summary Plot -------------------

#' Plot a summary of status time series for a set of CUs.
#' Status series are represented by series of squares, with letters and colouring indicating status
#' Each row in the block corresponds to a timeline for an individual CU.
#' CUs are grouped into blocks, with horizontal spaces between blocks and group labels shown above each block.
#' @param data a data frame containing the status time series, years in rows, CU IDs in columns
#' @param groups a list of groups, each of which should consist of the CU IDs of all the CUs belonging to the group.
#'               The name of the group will be used to label the group in the plot.
#'               list(group1 = c('CU_ID1', 'CU_ID2', ...), group2 = c('CU_ID5', 'CU_ID6', ...))
#' @param labels a named list with labels for the CU IDs
#' @param title the plot title (optional)
#' @param startYr optional: if provided, the first year to show in the timeline
#' @param endYr optional: if provided the final year to show in the timeline
#' @param gpar plot styling - see timelineSummaryPlotSpecs.default() for details
#' @return generates a plot on the current graphics device
#' @examples
#' data('RapidStatus', package='SoSplots')
#' data('Fraser_sockeye_groups', package='SoSplots')
#' data('sockeye_labels', package='SoSplots')
#' timeline_summary_plot(data=RapidStatus, group=Fraser_sockeye_groups, labels=sockeye_labels, title='Fraser Sockeye' )
#' @export
#'
timeline_summary_plot <- function(data, groups, labels, title = "Status Summary", startYr = NULL, endYr = NULL, gpar = NULL) {

  gpar <- add_specs(timelineSummaryPlotSpecs.default(), gpar)

  # plot setup
  do.call2(fun = graphics::par, args = list(mar = c(2, 7, 1, 2)), override = gpar$par)
  if (is.null(startYr))
    startYr <- min(as.numeric(row.names(data)))
  if (is.null(endYr))
    endYr <- max(as.numeric(pretty(row.names(data))))
  # get the plot area set up - no plotting yet
  xlim <- c(startYr, endYr)

  # calc ylim: we need one line for each CU, plus two lines above each group to fit the label:
  nrows <- length(unlist(groups)) + 2*length(groups)
  ylim <- c(- nrows - 0.3, 0)
  do.call2(fun = graphics::plot,
           args = list(x = 1:5, y = 1:5, type = "n", xlim = xlim, ylim = ylim, xlab = "", ylab = "", axes = FALSE),
           override = gpar$main)

  # add x axis
  do.call2(fun = graphics::axis,
           args = list(side = 3, at = pretty(data$Year), pos=0),
           override = gpar$x.axis)

  # add plot title
  do.call2(fun = graphics::mtext,
           args = list(text = title, side = 3, line = 1, xpd = NA),
           override = gpar$title)

  # add the status time series
  ypos <- 1
  for(grp in names(groups)){
    # leave one line space at the top of each group
    ypos <- ypos + 1
    # plot the group label
    do.call2(fun = graphics::text,
             args = list(x=graphics::par("usr")[1], y=-ypos, labels=grp, adj=c(0.5), xpd=NA, font=2),
             override = gpar$group_label)
    ypos <- ypos + 1

    for(CU_ID in groups[[grp]]) {
      if (CU_ID %in% names(data)) { # allow for CUs missing from dataset
        # plot the time series label
        if (CU_ID %in% names(labels)) label<-labels[CU_ID] else label <- CU_ID
        do.call2(fun = graphics::text,
               args = list(x=graphics::par("usr")[1], y=-ypos, labels=label, adj=c(1), xpd=NA, font=1),
               override = gpar$label)
        # draw the timeline
        ts <- data[, CU_ID]
        names(ts) <- row.names(data)
        draw_metric_row(y=-ypos, m=ts,  startYr=xlim[1], endYr=xlim[2], gpar=gpar)
        ypos <- ypos + 1
      }
    }
  }
}

#' Default plot styling for use with timeline_summary_plot
#'
#' This data structure provides the default gpar settings for the \link{timeline_summary_plot} function.
#' An equivalent data structure with additional values can be provided to timeline_summary_plot
#' via the gpar parameter to further customize the plot. Values provided via gpar will
#' override the defaults set here.
#' \describe{
#'  \item{par}{Additional arguments to be passed to \link[graphics]{par}}
#'  \item{main}{Additional arguments to be passed to \link[base]{plot}}
#'  \item{x.axis}{Additional styling for the x-axis (passed \link[graphics]{axis})}
#'  \item{label}{Additional styling for the CU time series labels (passed to \link[graphics]{text})}
#'  \item{status.text}{Additional text styling for the letters representing status values (passed to \link[graphics]{text})}
#'  \item{status.line}{Additional styling for the horizontal lines shown as the backdrop for each status series (passed to \link[graphics]{abline})}
#' }
#' @examples
#' timelineSummaryPlotSpecs.default()
#' # show timeline summary plot with new title styled in red
#' data('RapidStatus', package='SoSplots')
#' data('Fraser_sockeye_groups', package='SoSplots')
#' data('sockeye_labels', package='SoSplots')
#' timeline_summary_plot(data=RapidStatus, group=Fraser_sockeye_groups, labels=sockeye_labels, gpar=list(title=list(text='Fraser Sockeye', col='red', cex=1.2)))
#' @seealso
#' \code{\link{timeline_summary_plot}}
#' @export
timelineSummaryPlotSpecs.default <- function(){
  list(par = list(mar=c(2, 7, 5, 2)),                 # margins etc
       main = list(asp=1),                            # passed to plot
       x.axis = list(cex.axis=1.4),                   # x axis styling
       title = list(font=2, col='darkblue', cex=1.5), # plot title styling
       label = list(font=1, cex=1, col='black'),    # styling for CU time series labels
       group_label = list(font=2, cex=1.1, col='darkblue'), # styling for group labels
       metric.text = list(font=2, col='darkblue', cex=1),   # styling for status letter
       metric.line = list(col='darkgrey'))            # styling for status square
}

# -------------- define some default colors -------------------------

# status colors for drawing outline of cells in the timeline
# color scheme borrowed from Gottfried's
outline_color <- function(status, alpha = 1) {
  switch(status,
         Red = 'firebrick1',
         RedAmber = 'firebrick1',
         Amber = 'orange',
         AmberGreen = 'green',
         Green = 'green',
         High = "darkblue",
         Moderate = "darkblue",
         Low = "darkblue",
         'darkgrey')
}

# status colors for drawing fill of cells in the timeline
fill_color <- function(status) { status_color(status, withAlpha = F) }

