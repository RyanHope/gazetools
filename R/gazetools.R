#' gazetools
#' 
#' A collection of tools for processing and classifying eye gaze data.
#' 
#' @name gazetools
#' @docType package
#' @author Ryan M. Hope \email{rmh3093@@gmail.com}
#' @importFrom methods setClass setMethod as
#' @useDynLib gazetools
NULL

#' Gaze data from a SMI RED 500 eyetracker at 500 Hz.
#' 
#' Gaze data of a participant performing a visual search task.
#' 
#' \itemize{
#'  \item Samplerate was 500hz.
#'  \item Screen resolution was 1680 x 1050 (px).
#'  \item Screen size was 473.76 x 296.1 (mm).
#' }
#'
#' @docType data
#' @keywords datasets
#' @name smi
#' @usage data(smi)
#' 
#' @format
#' \describe{
#'   \item{smi_time}{sample time}
#'   \item{smi_type}{sample type}
#'   \item{smi_sxl}{horizontal gaze location of left eye}
#'   \item{smi_sxr}{horizontal gaze location of right eye}
#'   \item{smi_syl}{vertical gaze location of left eye}
#'   \item{smi_syr}{vertical gaze location of right eye}
#'   \item{smi_dxl}{width of left pupil}
#'   \item{smi_dxr}{width of right pupil}
#'   \item{smi_dyl}{height of left pupil}
#'   \item{smi_dyr}{height of right pupil}
#'   \item{smi_exl}{horizontal offset of the left eye relative to the center of the screen}
#'   \item{smi_exr}{horizontal offset of the right eye relative to the center of the screen}
#'   \item{smi_eyl}{vertical offset of the left eye relative to the center of the screen}
#'   \item{smi_eyr}{vertical offset of the right eye relative to the center of the screen}
#'   \item{smi_ezl}{distance of the left eye to the center of the screen}
#'   \item{smi_ezr}{distance of the right eye to the center of the screen}
#' }
#' 
NULL

#' Gaze data from a SMI iView X Hi-Speed 1250 eyetracker at 1250 Hz.
#' 
#' Gaze data of a participant performing a reading task.
#' 
#' \itemize{
#'  \item Samplerate was 1250hz.
#'  \item Screen resolution was 1024 x 768 (px).
#'  \item Screen size was 0.38 x 0.30 (m).
#'  \item Viewing distance was 0.67 (m).
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name highspeed
#' @usage data(highspeed)
#' 
#' @format
#' \describe{
#'   \item{x}{horizontal gaze location}
#'   \item{y}{vertical gaze location}
#' }
#' 
NULL

#' Gaze data from a LC Technologies binocular eyetracker at 120 Hz.
#' 
#' Gaze data of a participant performing a visual search task.
#' 
#' \itemize{
#'  \item Samplerate was 120hz (alternating 60hz per eye).
#'  \item Screen resolution was 1280 x 1024 (px).
#'  \item Screen size was 33.97 x 27.31 (cm).
#'  \item Viewing distance was 58.74 (cm).
#'  \item Vertical viewing offset was 4.55 (cm).
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name lc120
#' @usage data(lc120)
#' 
#' @format
#' \describe{
#'   \item{status}{signal status}
#'   \item{pupil}{pupil size}
#'   \item{x}{horizontal gaze location}
#'   \item{y}{vertical gaze location}
#'   \item{field}{field/camera number}
#'   \item{timestamp}{time stamp}
#' }
#' 
NULL

#' Gazetools logo
#' 
#' The gazetools logo
#' 
#' @docType data
#' @keywords datasets
#' @name logo
#' @usage data(logo)
#' 
NULL