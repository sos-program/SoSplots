# setup for generating SoS plot panels for markdown reports
# color definitions and other bits and pieces of specs
# Note: alpha channel doesn't seem to work for rendering to some devices :(
#       left here for documentation, but currently not used
#  Author: Brigitte Dorner
#  March 2023

#-------------  color defs  ---------------

# status colors borrowed from Michael Arbeider's color scheme for IF Coho RPA
# via Gottfried Pestal
#' Generate a color value from a status or status confidence value
#' @param status  a character string that gives the status or status confidence;
#'                valid status options are 'Green', 'AmberGreen', 'Amber', 'AmberRed', 'Red', 'DD'
#'                valid status confidence options are 'High', 'Moderate', 'Low'
#'                a default color is returned for unrecognized status values
#' @param alpha  optional, and integer between 0 and 1, specifies the alpha channel (i.e., transparency) value
#' @param withAlpha boolean; if TRUE, color strings will include an alpha channel - note that this may not be supported on all devices
#' @returns a string representing the status color in hex format
#' @export
status_color <- function(status, alpha = 1, withAlpha = T) {
  if (!withAlpha) alpha <- NULL
  switch(status,
         Green = grDevices::rgb(184, 225, 134, alpha=alpha, maxColorValue = 255),
         AmberGreen = grDevices::rgb(184, 225, 134, alpha=alpha, maxColorValue = 255),
         Amber = grDevices::rgb(255, 255, 191, alpha=alpha, maxColorValue = 255),
         RedAmber = grDevices::rgb(241, 182, 218, alpha=alpha, maxColorValue = 255),
         Red = grDevices::rgb(241, 182, 218, alpha=alpha, maxColorValue = 255),
         DD = grDevices::rgb(176, 196, 222, alpha=alpha, maxColorValue = 255),
         High = 'lightblue',
         Moderate = 'lightgrey',
         Low = 'white',
         grDevices::rgb(140, 140, 140, alpha=alpha, maxColorValue = 255))
}

#' Generate a color value from a status color for shading plot background
#' @param status  a character string that gives the status or status confidence;
#'                valid status options are 'Green', 'AmberGreen', 'Amber', 'AmberRed', 'Red', 'DD'
#'                valid status confidence options are 'High', 'Moderate', 'Low'
#'                a default color is returned for unrecognized status values
#' @returns a string representing the status color in hex format
#' @export
bg_status_color <- function(status) {
# (note - using no-alpha approximation here, since alpha channel doesn't render on some devices)
# bg_status_color <- function(status) { status_color(status, alpha = 0.5, withAlpha = F) }
  switch(status,
         Green = '#D1E1C6',
         AmberGreen = '#D1E1C6',
         Amber = '#FEFFD4',
         RedAmber = '#F1DBE7',
         Red = '#F1DBE7',
         DD = '#CED4DE',
         High = 'lightblue',
         Moderate = 'lightgrey',
         Low = 'white',
         '#CED4DE')
}


#----------------- letters to use as abbreviation for status and status confidence ------------

#' Generate a letter from a status color for use as an abbreviated description of the status
#' @param status  a character string that gives the status or status confidence;
#'                valid status options are 'Green', 'AmberGreen', 'Amber', 'AmberRed', 'Red', 'DD'
#'                valid status confidence options are 'High', 'Moderate', 'Low'
#' @returns a string containing one or more letters representing the status or status confidence
#' @export
status_letter <- function(status) {
  switch(status,
         Red = 'R', RedAmber = 'RA', Amber = 'A', AmberGreen = 'AG', Green = 'G', DD = '',
         Low = 'L', Medium = 'M', High = 'H', '')
}


