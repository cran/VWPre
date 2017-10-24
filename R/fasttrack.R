#' Fast-track basic preprocessing
#' 
#' \code{fasttrack} is a meta-function for advanced users who are 
#' already familiar with the package functions and do not need to take remedial 
#' actions such as recoding interest areas, remapping gaze data, or performing
#' message alignment.It takes all necessary arguments for the component functions
#' to produce proportion looks and can output either empirical logits or 
#' binomial data. The function returns a dataframe containing the result of 
#' the series of subroutines.
#' 
#' @export
#' @import dplyr
#' 
#' @param data A data frame object created from an Eyelink Sample Report.
#' @param Subject An obligatory string containing the column name corresponding 
#' to the subject identifier.
#' @param Item An optional string containing the column name corresponding to 
#' the item identifier; by default, NA.
#' @param EventColumns A vector specifying the columns which will be used for creating 
#' the Event variable; by default, Subject and TRIAL_INDEX.
#' @param NoIA A positive integer indicating the number of interest areas defined 
#' when creating the study. 
#' @param Adjust An integer indicating amount of time in milliseconds by which to 
#' offset the time series.
#' @param Recording A string indicating which eyes were used for recording gaze data.
#' @param WhenLandR A string indicating which eye ("Right" or "Left) to use 
#' if gaze data is available for both eyes (i.e., Recording = "LandR"). 
#' @param BinSize A positive integer indicating the size of the binning window 
#' (in milliseconds).
#' @param SamplingRate A positive integer indicating the sampling rate (in Hertz) 
#' used to record the gaze data.
#' @param ObsPerBin A positive integer indicating the desired number of 
#' observations to be used in the calculations.
#' @param ObsOverride A logical value controlling restrictions on the value
#' provided to ObsPerBin. Default value is FALSE.
#' @param Constant A positive number used for the empirical logit and weights
#' calculation; by default, 0.5 as in Barr (2008).
#' @param CustomBinom An optional parameter specifying a vector containing two 
#' integers corresponding to the interest area IDs to be combined.
#' @param Output An obligatory string containing either "ELogit" or "Binomial".
#' @return A data table containing formatting and calculations.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Perform meta-function on data
#' df <- fasttrack(data = dat, Subject = "RECORDING_SESSION_LABEL", Item = "itemid", 
#'        EventColumns = c("Subject", "TRIAL_INDEX"), NoIA = 4, Adjust = 100, 
#'				Recording = "LandR", WhenLandR = "Right", BinSize = 20, 
#'				SamplingRate = 1000, ObsPerBin = 20, Constant = 0.5, 
#'				Output = "ELogit")
#' }
fasttrack = function(data = data, Subject = NULL, Item = NA, 
                     EventColumns = c("Subject", "TRIAL_INDEX"), NoIA = NoIA,
                     Adjust = 0, Recording = NULL, 
                     WhenLandR = NA, BinSize = NULL, SamplingRate = NULL,
                     ObsPerBin = NULL, ObsOverride = FALSE, 
                     Constant = 0.5, CustomBinom = NULL, Output = NULL) {
  
  dat <- data
  Subject <- Subject
  Item <- Item
  EventColumns <- EventColumns
  NoIA <- NoIA
  Adjust <- Adjust
  Recording <- Recording
  WhenLandR <- WhenLandR
  BinSize <- BinSize
  SamplingRate <- SamplingRate
  ObsPerBin <- ObsPerBin
  ObsOverride <- ObsOverride
  Constant <- Constant
  CustomBinom <- CustomBinom
  Output <- Output
  
  message("Preparing data...")
  dat0 <- prep_data(data = dat, Subject = Subject, Item = Item, EventColumns = EventColumns)
  
  message(paste("Relabelling outside of", NoIA, "interest areas...", sep = " "))
  dat1 <- relabel_na(data = dat0, NoIA = NoIA)
  rm(dat0)
  
  check_ia(data = dat1)
  
  message(paste("Creating time series with", Adjust, "ms adjustment...", sep = " "))
  dat2 <- create_time_series(data = dat1, Adjust = Adjust)
  rm(dat1)
  
  check_time_series(data = dat2)
  check_eye_recording(data = dat2)
  
  message(paste("Selecting recorded eye..."))
  dat3 <- select_recorded_eye(data = dat2, Recording = Recording, WhenLandR = WhenLandR)
  rm(dat2)
  
  check_samplingrate(dat3)
  
  message(paste("Binning", SamplingRate, "Hz data into", BinSize, "ms bins..."))
  message("Calculating proportions...")
  dat4 <- bin_prop(dat3, NoIA = NoIA, BinSize = BinSize, SamplingRate = SamplingRate)
  rm(dat3)
  
  check_samplingrate(dat4)
  check_samples_per_bin(dat4)
  
  message(paste("Preparing", Output, "output...", sep = " "))
  if(is.null(Output)) {
    stop("Please specify the desired output type!")
  } else if (Output == "ELogit") {
    dat5 <- transform_to_elogit(dat4, NoIA = NoIA, ObsPerBin = ObsPerBin, 
                                Constant = Constant, ObsOverride = ObsOverride)
  } else if (Output == "Binomial") {
    dat5 <- create_binomial(data = dat4, NoIA = NoIA, ObsPerBin = ObsPerBin,
                            ObsOverride = ObsOverride, CustomBinom = CustomBinom)
  }
  
  rm(dat4)
  
  return(ungroup(dat5))
  
}