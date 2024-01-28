#  Code for generating plots used in the Rapid Status panel
#  Adapted from Gottfried Pestal's SOS_RapidStatus_CaseStudies2022 code
#  https://github.com/SOLV-Code/SOS_RapidStatus_CaseStudies2022
#  Author: Brigitte Dorner
#  March 2023

require(graphics)

#----------- defaults ------------------


#' Default plot specs for use with status_overview_plot
#'
#' This data structure provides the default gpar settings for the \link{status_overview_plot} function.
#' An equivalent data structure with additional values can be provided to status_overview_plot
#' via the gpar parameter to further customize the plot. Values provided via gpar will
#' override the defaults set here.
#' \describe{
#'  \item{par}{Additional arguments to be passed to \link[graphics]{par}}
#'  \item{main}{Additional arguments to be passed to \link[base]{plot}}
#'  \item{x.axis}{Additional styling for the x-axis (passed to \link[graphics]{axis})}
#'  \item{y.axis}{Additional styling for the y-axis (passed \link[graphics]{axis})}
#'  \item{legend}{Additional styling for the legend (passed to \link[graphics]{barplot} via the *args.legend* parameter)}
#'  \item{grid}{Additional styling for the grid lines (passed to \link[graphics]{abline})}
#' }
#' @examples
#' statusOverviewSpecs.default()
#' @seealso
#' \code{\link{status_overview_plot}}
#' @export
statusOverviewSpecs.default <- function(){
  list(par = list(mar = c(4,6,5,6)),
       x.axis = list(cex.axis=1),
       y.axis = list(cex.axis=1),
       legend = list(cex=1.3),
       main = list(cex.main=1),
       grid = list(col='white', lwd=2))
}

#-----------  Stacked Bar Plot -------------------
#' Status Overview Pane
#'
#' Generate a stacked bar plot that shows the proportion of CUs with red vs amber vs green status over time.
#' @param CUstatus a matrix or data frame with CUs in columns and years in rows
#'                 row names of CUstatus will be used to indicate years and be plotted along the x axis
#' @param plotTitle the plot title
#' @param gpar plot styling - see \link{statusOverviewSpecs.default} for details
#' @return generates a plot on the current graphics device
#'
#' @examples
#' # show status overview for Fraser CUs
#' data('RapidStatus', package='SoSplots')
#' status_overview_plot(RapidStatus)
#' # again, with modified title
#' gpar <- list(main=list(main='Rapid Status for Fraser Sockeye CUs', cex.main=0.8))
#' status_overview_plot(RapidStatus, gpar=gpar)
#' @export
#'
status_overview_plot <- function(CUstatus, plotTitle = 'All CUs in Rapid Status Scan', gpar = NULL) {

  # allow for additional styling
  gpar <- add_specs(statusOverviewSpecs.default(), gpar)

  # make sure CUstatus matrix has cuntinuous data years
  yrs <- as.numeric(rownames(CUstatus))
  rowNamesExpected <- as.character(seq(min(yrs), max(yrs)))
  stopifnot(all(rownames(CUstatus) == rowNamesExpected))

  statusProps <- as.matrix(data.frame(Red = apply(CUstatus, 1, get_proportion, match=c('Red', 'RedAmber')),
                                      Amber = apply(CUstatus, 1, get_proportion, match='Amber'),
                                      Green = apply(CUstatus, 1, get_proportion, match=c('Green', 'AmberGreen')),
                                      `NA` = apply(CUstatus, 1, get_proportion, match=c('DD', 'NA', NA))))
  rownames(statusProps) <- rownames(CUstatus)
  colnames(statusProps) <- c('Red', 'Amber', 'Green', '?')

  # plot specs
  do.call2(fun = graphics::par, args = list(mar = c(4,6,5,6)), override = gpar$par)
  startYr <- min(yrs)
  xTicks <- pretty(c(0, length(yrs) - 1))
  barColors <- c(status_color('Red', withAlpha = F),
                 status_color('Amber', withAlpha = F),
                 status_color('Green', withAlpha = F),
                 status_color('NA', withAlpha = F))

  legendSpecs <- list(bty="n", horiz=FALSE, x=max(xTicks)+1, xjust=0, xpd=NA, inset=0)
  if (!is.null(gpar$legend))
    legendSpecs <- utils::modifyList(legendSpecs, gpar$legend)

  # create the barplot
  do.call2(fun = graphics::barplot,
          args = list(t(statusProps), border=NA, space=0, axes=F, xaxt="n",xpd=F, legend=T, args.legend=legendSpecs,
                      xlab="Year", ylab="% CUs", main=plotTitle, col=barColors),
          override =  gpar$main)
  # add y axis
  do.call2(fun = graphics::axis,
          args = list(side=2, at=seq(0, 1, by=0.2), labels=paste(seq(0, 100, by=20), "%", sep=""), xpd=T),
          override = gpar$y.axis)
  # add x axis - add an offset here so the tickmarks appear in the centre of the bar
  do.call2(fun = graphics::axis,
          args = list(side=1, at=xTicks+0.5, labels=xTicks + startYr, xpd=T),
          override = gpar$x.axis)
  # add grid lines
  do.call2(fun = graphics::abline,  args = list(h=seq(0.2, 0.8, by=0.2)), override = gpar$grid)
}



