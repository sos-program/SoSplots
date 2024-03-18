# This file contains documentation for the sample datasets included with the package

#' Attributes for the Bowron CU
#' @format
#' A named vector with elements:
#' \describe{
#'   \item{CU_ID}{ID}
#'   \item{CU_Name}{Name}
#'   \item{AvGen}{Average Generation Length}
#'   \item{DataQualkIdx}{Describes the type of abundance data}
#'   \item{Cyclic}{Cyclic dominance}
#'   \item{Cyc_Dom_Year}{If the CU has cyclic dominance, first dominant cycle year in the abundance data}
#'   \item{RelAbd_LBM}{Lower benchmark for the RelAbd metric}
#'   \item{RelAbd_UBM}{Upper benchmark for the RelAbd metric}
#'   \item{AbsAbd_LBM}{Lower benchmark for the AbsAbd metric}
#'   \item{AbsAbd_UBM}{Upper benchmark for the AbsAbd metric}
#'   \item{LongTrend_LBM}{Lower benchmark for the LongTrend metric}
#'   \item{LongTrend_UBM}{Upper benchmark for the LongTrend metric}
#'   \item{PercChange_LBM}{Lower benchmark for the PercChange metric}
#'   \item{PercChange_UBM}{Upper benchmark for the PercChange metric}
#'   \item{AbdMetric}{Boolean, specifies whether the CU has data of high-enough quality to inform status determination}
#' }
"Bowron_attribs"

#' Metrics for the Bowron CU
#' @format
#' A data frame with columns:
#' \describe{
#'   \item{CU_ID}{ID}
#'   \item{Year}{Data year}
#'   \item{RelUBM}{RelUBM metric}
#'   \item{RelUBM.Status}{CU status as measured by the RelUBM metric}
#'   \item{RelUBM.Confidence}{Confidence in the RelUBM status}
#'   \item{AbsUBM}{AbsUBM metric}
#'   \item{AbsUBM.Status}{CU status as measured by the AbsUBM metric}
#'   \item{AbsUBM.Confidence}{Confidence in the AbsUBM status}
#'   \item{RelLBM.Status}{CU status as measured by the RelLBM metric}
#'   \item{RelLBM.Confidence}{Confidence in the RelLBM status}
#'   \item{AbsLBM}{AbsLBM metric}
#'   \item{AbsLBM.Status}{CU status as measured by the AbsLBM metric}
#'   \item{AbsLBM.Confidence}{Confidence in the AbsLBM status}
#'   \item{LongTrend}{LongTrend metric}
#'   \item{LongTrend.Status}{CU status as measured by the LongTrend metric}
#'   \item{LongTrend.Confidence}{Confidence in the LongTrend status}
#'   \item{PercChange}{PercChange metric}
#'   \item{PercChange.Status}{CU status as measured by the PercChange metric}
#'   \item{PercChange.Confidence}{Confidence in the PercChange status}
#'   \item{IntStatus.Status}{Status determined by Integrated Status Assessment}
#'   \item{IntStatus.Confidence}{Confidence in the Integrated Status}
#'   \item{RapidStatus.Status}{Status determined by Rapid Status Assessment}
#'   \item{RapidStatus.Confidence}{Confidence in the Rapid Status}
#'   \item{IntStatus.Year}{Year the Integrated Status was last determined}
#'   \item{IntDtatusRaw}{Integrated Status, only for assessment years}
#' }
"Bowron_metrics"

#' Escapement data for the Bowron CU
#' @format
#' A data frame with columns:
#' \describe{
#'   \item{CU_ID}{ID}
#'   \item{Year}{Data year}
#'   \item{Escapement_Wild}{Wild spawning escapment}
#'   }
"Bowron_escapement"

#' Rapid Status Summary for Fraser Sockeye
#' @format
#'A data frame with CUs (identified by their CU_ID) in columns and assessment years in rows
"RapidStatus"

#' Groups for Fraser sockeye CUs
#' @format
#'A list of lists that specifies group membership for Fraser sockeye CUs
"Fraser_sockeye_groups"

#' Labels for sockeye CUs
#' @format
#'A list that specifies labels for sockeye CU IDs
"sockeye_labels"

