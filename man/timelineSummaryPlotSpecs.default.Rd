% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Timeline_Summary_Panel.R
\name{timelineSummaryPlotSpecs.default}
\alias{timelineSummaryPlotSpecs.default}
\title{Default plot styling for use with timeline_summary_plot}
\usage{
timelineSummaryPlotSpecs.default()
}
\description{
This data structure provides the default gpar settings for the \link{timeline_summary_plot} function.
An equivalent data structure can be provided to timeline_summary_plot via the gpar parameter to further customize the plot.
Values provided via the gpar parameter will override the defaults set here.
\describe{
 \item{par}{Arguments to be passed to \link[graphics]{par}}
 \item{main}{Arguments to be passed to \link[base]{plot}}
 \item{x.axis}{Styling for the x-axis (passed \link[graphics]{axis})}
 \item{group_label}{Styling for the group labels (passed to \link[graphics]{text})}
 \item{label}{Styling for the CU time series labels (passed to \link[graphics]{text})}
 \item{status.text}{Styling for the letters representing status values (passed to \link[graphics]{text})}
 \item{status.line}{Styling for the horizontal lines shown as the backdrop for each status series (passed to \link[graphics]{abline})}
}
}
\examples{
timelineSummaryPlotSpecs.default()
# show timeline summary plot with new title styled in red
data('RapidStatus', package='SoSplots')
data('Fraser_sockeye_groups', package='SoSplots')
data('sockeye_labels', package='SoSplots')
timeline_summary_plot(data=RapidStatus,
                      group=Fraser_sockeye_groups,
                      labels=sockeye_labels,
                      gpar=list(title=list(text='Fraser Sockeye', col='red', cex=1.2)))
}
\seealso{
\code{\link{timeline_summary_plot}}
}
