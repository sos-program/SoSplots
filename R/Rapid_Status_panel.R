#  Code for generating plots used in the Rapid Status panel
#  Adapted from Gottfried Pestal's SOS_RapidStatus_CaseStudies2022 code
#  https://github.com/SOLV-Code/SOS_RapidStatus_CaseStudies2022
#  Author: Brigitte Dorner
#  March 2023

require(graphics)

#----------- defaults ------------------

statusOverviewSpecs.default <- list(x.axis = list(cex.axis=1),
                                    y.axis = list(cex.axis=1),
                                    legend = list(cex=1.3),
                                    main = list(cex.main=1),
                                    grid = list(col='white', lwd=2))

#-----------  Stacked Bar Plot -------------------
#' Generate a stacked bar plot that shows the proportion of CUs with red vs amber vs green status over time
#' @param CUstatus a matrix or data frame with CUs in columns and years in rows
#'                 row names of CUstatus will be used to indicate years and be plotted along the x axis
#' @param plotTitle the plot title
#' @param gpar plot styling
#' @return generates a plot on the current graphics device
#' @export
#'
status_overview_plot <- function(CUstatus, plotTitle = 'All CUs in Rapid Status Scan', gpar = statusOverviewSpecs.default) {

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
  graphics::par(mar = c(4,6,5,6))
 # graphics::par(mar = c(5,10,3,10))
  startYr <- min(yrs)
  xTicks <- pretty(c(0, length(yrs) - 1))
  barColors <- c(status_color('Red', withAlpha = F),
                 status_color('Amber', withAlpha = F),
                 status_color('Green', withAlpha = F),
                 status_color('NA', withAlpha = F))

  legendSpecs <- utils::modifyList(list(bty="n", horiz=FALSE, x=max(xTicks)+1, xjust=0, xpd=NA, inset=0), gpar$legend)
  # create the barplot
  do.plot(fun = graphics::barplot,
          args = list(t(statusProps), border=NA, space=0, axes=F, xaxt="n",xpd=F, legend=T, args.legend=legendSpecs,
                      xlab="Year", ylab="% CUs", main=plotTitle, col=barColors),
          override =  gpar$main)
  # add y axis
  do.plot(fun = graphics::axis,
          args = list(side=2, at=seq(0, 1, by=0.2), labels=paste(seq(0, 100, by=20), "%", sep=""), xpd=T),
          override = gpar$y.axis)
  # add x axis - add an offset here so the tickmarks appear in the centre of the bar
  do.plot(fun = graphics::axis,
          args = list(side=1, at=xTicks+0.5, labels=xTicks + startYr, xpd=T),
          override = gpar$x.axis)
  # add grid lines

  do.plot(fun = graphics::abline,  args = list(h=seq(0.2, 0.8, by=0.2)), override = gpar$grid)
}



