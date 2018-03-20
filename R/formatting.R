#' Check the classes of specific columns and re-assigns as necessary.
#' 
#' \code{prep_data} converts the data frame to a data table and examines the 
#' required columns (RECORDING_SESSION_LABEL, LEFT_INTEREST_AREA_ID, 
#' RIGHT_INTEREST_AREA_ID, LEFT_INTEREST_AREA_LABEL, RIGHT_INTEREST_AREA_LABEL, 
#' TIMESTAMP, and TRIAL_INDEX) and optional columns (SAMPLE_MESSAGE, LEFT_GAZE_X,
#' LEFT_GAZE_Y, RIGHT_GAZE_X, and RIGHT_GAZE_Y). It renames the subject and item 
#' columns, ensures required/optional columns are of the appropriate data class, 
#' and creates a new column called Event which indexes each unique 
#' series of samples corresponding to the combination of Subject and 
#' TRIAL_INDEX (can be changed), necessary for performing subsequent operations.
#' 
#' @export
#' @import dplyr
#' @import rlang
#' 
#' @param data A data frame object created from an Eyelink Sample Report.
#' @param Subject An obligatory string containing the column name corresponding to the subject identifier.
#' @param Item An optional string containing the column name corresponding to the item identifier; by default, NA.
#' @param EventColumns A vector specifying the columns which will be used for creating 
#' the Event variable; by default, Subject and TRIAL_INDEX. 
#' @return An object of type data table as described in \link[dplyr]{tbl_df}.
#' @examples
#' \dontrun{
#' # Typical DataViewer output contains a column called "RECORDING_SESSION_LABEL"
#' # corresponding to the subject.
#' # To prepare the data...
#' library(VWPre)
#' df <- prep_data(data = dat, Subject = "RECORDING_SESSION_LABEL", Item = "ItemCol")
#' }
prep_data <- function(data, Subject = NULL, Item = NA,
                      EventColumns=c("Subject","TRIAL_INDEX")){
  
  reqcols <- data.frame(Column=c("RECORDING_SESSION_LABEL", 
                                 "LEFT_INTEREST_AREA_ID","LEFT_INTEREST_AREA_LABEL", 
                                 "RIGHT_INTEREST_AREA_ID","RIGHT_INTEREST_AREA_LABEL", 
                                 "TIMESTAMP","TRIAL_INDEX"), Present=NA)
  optcols <- data.frame(Column=c("SAMPLE_MESSAGE", "LEFT_GAZE_X", "LEFT_GAZE_Y", 
                                 "RIGHT_GAZE_X", "RIGHT_GAZE_Y", "EYE_TRACKED"), Present=NA)
  
  data <- tbl_df(data)
  
  message("Checking required columns...")
  
  for (x in 1:nrow(reqcols)) {
    if (!(reqcols[x,1] %in% names(data))) {
      reqcols[x,2] <- 0
    }
    else {
      reqcols[x,2] <- 1
    }
  }
  
  missingcols <- filter(reqcols, Present==0)
  
  if (nrow(missingcols) > 0) {
    stop(paste("\n The following column is required to process the data: ", unique(as.factor(missingcols$Column))))
  } else {
    message("    All required columns are present in the data.")
  }
  
  
  message("Checking optional columns...")
  
  for (x in 1:nrow(optcols)) {
    if (!(optcols[x,1] %in% names(data))) {
      optcols[x,2] <- 0
    }
    else {
      optcols[x,2] <- 1
    }
  }
  
  missingoptcols <- filter(optcols, Present==0)
  
  if (nrow(missingoptcols) > 0) {
    message(paste("    The following optional is not present in the data: ", unique(as.factor(missingoptcols$Column)), "\n"))
  } else {
    message("    All optional columns are present in the data.")
  }
  
  
  message("Working on required columns...")
  
  if(is.null(Subject)){
    stop("Please supply the name of the subject column!")
  } else {
    subject <- Subject
    subject <- enquo(subject)
  }
  
  data <- rename(data, Subject = !!subject)
  message(paste("   ", quo_name(subject), "renamed to Subject. "))
  
  if (is.factor(data$Subject) == F){
    data$Subject <- as.factor(as.character(data$Subject))
    message("    Subject converted to factor.")
  } else {
    message("    Subject already factor.")
  }
  
  item <- Item
  
  if (!is.na(item)) {
    
    if (!(item %in% names(data))) {
      stop(paste(item, "is not a column name in the data."))
    }
    
    item <- enquo(item)
    data <- rename(data, Item = !!item)
    message(paste("   ", quo_name(item), "renamed to Item."))
    if (is.factor(data$Item) == F){
      data$Item <- as.factor(as.character(data$Item))
      message("    Item converted to factor.")
    } else {
      message("    Item already factor.")
    } 
  } else {
    message("    No Item column specified.")
  }
  
  if (is.numeric(data$LEFT_INTEREST_AREA_ID) == F){
    data$LEFT_INTEREST_AREA_ID <- as.numeric(as.character(data$LEFT_INTEREST_AREA_ID))
    message("    LEFT_INTEREST_AREA_ID converted to numeric.")
  } else {
    message("    LEFT_INTEREST_AREA_ID already numeric.")
  }
  
  if (is.numeric(data$RIGHT_INTEREST_AREA_ID) == F){
    data$RIGHT_INTEREST_AREA_ID <- as.numeric(as.character(data$RIGHT_INTEREST_AREA_ID))
    message("    RIGHT_INTEREST_AREA_ID converted to numeric.")
  } else {
    message("    RIGHT_INTEREST_AREA_ID already numeric.")
  }
  
  if (is.factor(data$LEFT_INTEREST_AREA_LABEL) == F){
    data$LEFT_INTEREST_AREA_LABEL <- as.factor(as.character(data$LEFT_INTEREST_AREA_LABEL))
    message("    LEFT_INTEREST_AREA_LABEL converted to factor.")
  } else {
    message("    LEFT_INTEREST_AREA_LABEL already factor.")
  }
  
  if (is.factor(data$RIGHT_INTEREST_AREA_LABEL) == F){
    data$RIGHT_INTEREST_AREA_LABEL <- as.factor(as.character(data$RIGHT_INTEREST_AREA_LABEL))
    message("    RIGHT_INTEREST_AREA_LABEL converted to factor.")
  } else {
    message("    RIGHT_INTEREST_AREA_LABEL already factor.")
  }
  
  if (is.numeric(data$TIMESTAMP) == F){
    data$TIMESTAMP <- as.numeric(as.character(data$TIMESTAMP))
    message("    TIMESTAMP converted to numeric.")
  } else {
    message("    TIMESTAMP already numeric.")
  }
  
  if (is.numeric(data$TRIAL_INDEX) == F){
    data$TRIAL_INDEX <- as.numeric(as.character(data$TRIAL_INDEX))
    message("    TRIAL_INDEX converted to numeric.")
  } else {
    message("    TRIAL_INDEX already numeric.")
  }
  
  if (!(EventColumns[1] %in% names(data))) {
    stop(paste(EventColumns[1], "is not a column name in the data."))
  }
  if (!(EventColumns[2] %in% names(data))) {
    stop(paste(EventColumns[2], "is not a column name in the data."))
  }
  
  data$Event <- interaction(data[,EventColumns], drop=TRUE)
  message(paste("    Event variable created from", EventColumns[1], "and", EventColumns[2]), "")
  
  
  message("Working on optional columns...")
  
  if (nrow(missingoptcols) == nrow(optcols)) {
    message("    No optional columns present in the data.")
  } 
  
  if ("SAMPLE_MESSAGE" %in% names(data)) {
    if (is.factor(data$SAMPLE_MESSAGE) == F){
      data$SAMPLE_MESSAGE <- as.factor(as.character(data$SAMPLE_MESSAGE))
      message("    Optional column SAMPLE_MESSAGE converted to factor.")
    } else {
      message("    Optional column SAMPLE_MESSAGE already factor.")
    }
  }
  
  if ("LEFT_GAZE_X" %in% names(data)) {
    if (is.numeric(data$LEFT_GAZE_X) == F){
      data$LEFT_GAZE_X <- as.numeric(as.character(data$LEFT_GAZE_X))
      message("    Optional column LEFT_GAZE_X converted to numeric.")
    } else {
      message("    Optional column LEFT_GAZE_X already numeric.")
    }
  }
  
  if ("LEFT_GAZE_Y" %in% names(data)) {
    if (is.numeric(data$LEFT_GAZE_Y) == F){
      data$LEFT_GAZE_Y <- as.numeric(as.character(data$LEFT_GAZE_Y))
      message("    Optional column LEFT_GAZE_Y converted to numeric.")
    } else {
      message("    Optional column LEFT_GAZE_Y already numeric.")
    }
  }
  
  if ("RIGHT_GAZE_X" %in% names(data)) {
    if (is.numeric(data$RIGHT_GAZE_X) == F){
      data$RIGHT_GAZE_X <- as.numeric(as.character(data$RIGHT_GAZE_X))
      message("    Optional column RIGHT_GAZE_X converted to numeric.")
    } else {
      message("    Optional column RIGHT_GAZE_X already numeric.")
    }
  }
  
  if ("RIGHT_GAZE_Y" %in% names(data)) {
    if (is.numeric(data$RIGHT_GAZE_Y) == F){
      data$RIGHT_GAZE_Y <- as.numeric(as.character(data$RIGHT_GAZE_Y))
      message("    Optional column RIGHT_GAZE_Y converted to numeric.")
    } else {
      message("    Optional column RIGHT_GAZE_Y already numeric.")
    }
  }
  
  if ("EYE_TRACKED" %in% names(data)) {
    if (is.factor(data$EYE_TRACKED) == F){
      data$EYE_TRACKED <- as.factor(as.character(data$EYE_TRACKED))
      message("    Optional column EYE_TRACKED converted to factor.")
    } else {
      message("    Optional column EYE_TRACKED already factor.")
    }
  }
  
  return(ungroup(data))
}




#' Aligns samples to a specific message.
#' 
#' \code{align_msg} examines the data from each recording event and locates the
#' first instance of the specified message in the column SAMPLE_MESSAGE.
#' The function creates a new column containing the aligned series which can be 
#' utilized by subsequent functions for checking and creating the time series
#' column.
#' 
#' @export
#' @import dplyr
#' @import rlang
#' 
#' @param data A data table object output from \code{prep_data}.
#' @param Msg An obligatory string containing the message to be found in 
#' the column SAMPLE_MESSAGE or a regular expression for locating the 
#' appropriate message.
#' @return A data table object.
#' @examples
#' \dontrun{
#' # To align the samples to a specific message...
#' library(VWPre)
#' df <- align_msg(data = dat, Msg = "ExperimentDisplay")
#'  
#' # For a more complete tutorial on VWPre plotting functions:
#' vignette("SR_Message_Alignment", package="VWPre")
#' }
#' 
align_msg <- function(data, Msg = NULL) {
  
  if(is.null(Msg)){
    stop("Please supply the message text or regular expression!")
  } else {
    msg <- Msg
    msg <- enquo(msg)
  }
  
  tmp1 <- data %>% group_by(Event) %>% 
    mutate(Align = ifelse(grepl(!!msg, SAMPLE_MESSAGE), TIMESTAMP, NA)) %>%
    filter(!is.na(Align)) %>% select(Event, Align) %>% filter(Align==min(Align))
  tmp2 <- inner_join(data, tmp1, by="Event") %>% mutate(Align = TIMESTAMP - Align)
  
  return(ungroup(tmp2))
}


#' Checks for and removes unnecessary DV output columns.
#' 
#' \code{rm_extra_DVcols} checks for unnecessary DataViewer output columns and 
#' removes them, unless specified.
#' 
#' @export
#' @import dplyr
#' @import rlang
#' 
#' @param data A data frame object created from an Eyelink Sample Report.
#' @param Keep An optional string or character vector containing the column names 
#' of SR sample report columns the user would like to keep in the data set. 
#' @return An object of type data table as described in \link[dplyr]{tbl_df}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' df <- rm_extra_DVcols(data = dat, Keep = NULL)
#' }
rm_extra_DVcols <- function(data, Keep=NULL){

  SR_not_needed <- c(
    "AVERAGE_ACCELERATION_X", 
    "AVERAGE_ACCELLERATION_X", # THIS IS A DV MISSPELLING
    "AVERAGE_ACCELERATION_Y", 
    "AVERAGE_ACCELLERATION_Y", # THIS IS A DV MISSPELLING
    "AVERAGE_GAZE_X", 
    "AVERAGE_GAZE_Y", 
    "AVERAGE_IN_BLINK",
    "AVERAGE_IN_SACCADE", 
    "AVERAGE_INTEREST_AREAS",
    "AVERAGE_INTEREST_AREA_DATA", 
    "AVERAGE_INTEREST_AREA_DISTANCE", 
    "AVERAGE_INTEREST_AREA_ID", 
    "AVERAGE_INTEREST_AREA_LABEL", 
    "AVERAGE_INTEREST_AREA_PIXEL_AREA", 
    "AVERAGE_PUPIL_SIZE", 
    "AVERAGE_VELOCITY_X", 
    "AVERAGE_VELOCITY_Y", 
    "HTARGET_DISTANCE", 
    "HTARGET_FLAGS", 
    "HTARGET_X", 
    "HTARGET_Y", 
    "IP_DURATION", 
    "IP_END_EVENT_MATCHED", 
    "IP_END_TIME", 
    "IP_INDEX", 
    "IP_LABEL", 
    "IP_START_EVENT_MATCHED", 
    "IP_START_TIME", 
    "LEFT_ACCELERATION_X", 
    "LEFT_ACCELLERATION_X", # THIS IS A DV MISSPELLING
    "LEFT_ACCELERATION_Y", 
    "LEFT_ACCELLERATION_Y", # THIS IS A DV MISSPELLING
    "LEFT_FIX_INDEX", 
    "LEFT_INTEREST_AREAS", 
    "LEFT_INTEREST_AREA_DATA", 
    "LEFT_INTEREST_AREA_DISTANCE",
    "LEFT_INTEREST_AREA_PIXEL_AREA", 
    "LEFT_PUPIL_SIZE", 
    "LEFT_SACCADE_INDEX", 
    "LEFT_VELOCITY_X", 
    "LEFT_VELOCITY_Y", 
    "RESOLUTION_X", 
    "RESOLUTION_Y", 
    "RIGHT_ACCELERATION_X", 
    "RIGHT_ACCELLERATION_X", # THIS IS A DV MISSPELLING
    "RIGHT_ACCELERATION_Y", 
    "RIGHT_ACCELLERATION_Y", # THIS IS A DV MISSPELLING
    "RIGHT_FIX_INDEX", 
    "RIGHT_INTEREST_AREAS", 
    "RIGHT_INTEREST_AREA_DATA", 
    "RIGHT_INTEREST_AREA_DISTANCE", 
    "RIGHT_INTEREST_AREA_PIXEL_AREA", 
    "RIGHT_PUPIL_SIZE", 
    "RIGHT_SACCADE_INDEX", 
    "RIGHT_VELOCITY_X", 
    "RIGHT_VELOCITY_Y", 
    "SAMPLE_BUTTON", 
    "SAMPLE_INPUT", 
    "TARGET_ACCELERATION_X", 
    "TARGET_ACCELLERATION_X", # THIS IS A DV MISSPELLING
    "TARGET_ACCELERATION_Y", 
    "TARGET_ACCELLERATION_Y", # THIS IS A DV MISSPELLING
    "TARGET_VELOCITY_X", 
    "TARGET_VELOCITY_Y", 
    "TARGET_VISIBLE",
    "TARGET_X", 
    "TARGET_Y",
    "TRIAL_LABEL",
    "TRIAL_START_TIME",
    "VIDEO_FRAME_INDEX",
    "VIDEO_NAME"
  )
  
  data <- ungroup(data)
  
  delcols <- data.frame(Column=SR_not_needed, Present=NA, Keep=NA)

  message("Checking for extra (deletable) DV columns...")
  
  for (x in 1:nrow(delcols)) {
    if (!(delcols[x,1] %in% names(data))) {
      delcols[x,2] <- 0
      delcols[x,3] <- 0
    }
    else {
      delcols[x,2] <- 1
      if (delcols[x,1] %in% Keep) {
        delcols[x,3] <- 1
      } else {
        delcols[x,3] <- 0
      }
    }
  }
  
  deletecols <- filter(delcols, Present==1) %>% droplevels()
  
  if (nrow(deletecols) == 0) {
    stop("No deletable columns present in the data.")
  } 
  
  if (!(is.null(Keep))) {
  message("Checking for columns to keep...")
  }
  message("     Columns kept: ", paste(levels(droplevels(deletecols[deletecols$Keep==1,]$Column)), collapse = ", "))
  deletecols <- filter(deletecols, Keep==0) %>% droplevels()
  
  del <- unique(levels(deletecols$Column))

  message("Removing deletable columns...")
  message("     Columns removed: ", paste(levels(deletecols$Column), collapse = ", "))
  data <- select(data, -one_of(del))

  return(ungroup(data))
}




#' Select the eye used during recording
#' 
#' \code{select_recorded_eye} examines each event and determines which eye contains 
#' interest area information, based on the \code{Recording} parameter (which 
#' can be determined using \code{\link{check_eye_recording}}). 
#' This function then selects the data from the recorded eye and copies it to
#' new columns (IA_ID, IA_LABEL, IA_Data). The function prints a summary of the output.  
#' 
#' @export
#' @import dplyr
#' @import rlang
#' 
#' @param data A data table object output by \code{\link{create_time_series}}.
#' @param Recording A string indicating which eyes were used for recording gaze data.
#' @param WhenLandR A string indicating which eye ("Right" or "Left) to use 
#' if gaze data is available for both eyes (i.e., Recording = "LandR"). 
#' @return A data table with four additional columns ('EyeRecorded', 'EyeSelected', 
#' 'IA_ID', 'IA_LABEL', 'IA_Data') added to \code{data}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Create a unified columns for the gaze data...
#' df <- select_recorded_eye(data = dat, Recording = "LandR", WhenLandR = "Right")
#' }
select_recorded_eye <- function(data, Recording = NULL, WhenLandR = NA) {
  
  if("EYE_TRACKED" %in% names(data)) {
    message("Selecting gaze data using Data Viewer column EYE_TRACKED")
  } else {
    if(is.null(Recording)){
      stop("Please supply the recording eye(s)!")
    }
    message(paste("Selecting gaze data using Data Viewer columns LEFT_INTEREST_AREA_ID and RIGHT_INTEREST_AREA_ID and the Recording argument:", Recording))
  }
  
  if("EYE_TRACKED" %in% names(data)) {	
    
    if(("Both" %in% unique(data$EYE_TRACKED)) & is.na(WhenLandR)){
      stop("Please specify which eye to use when Recording is set to 'LandR'!")
    }
    tmp <- data %>%
      group_by(Event) %>%
      mutate(., EyeRecorded = as.character(EYE_TRACKED)) %>% 
      do(
        mutate(., EyeSelected = ifelse(EyeRecorded == "Both" & WhenLandR == "Right", "Right", 
                                       ifelse(EyeRecorded == "Both" & WhenLandR == "Left", "Left",
                                              ifelse(EyeRecorded == "Right", EyeRecorded, 
                                                     ifelse(EyeRecorded == "Left", EyeRecorded, NA)))))
      )
    
  } else {
    
    if (Recording == "LandR") {
      
      if(is.na(WhenLandR)){
        stop("Please specify which eye to use when Recording is set to 'LandR'!")
      }
      
      tmp <- data %>%
        group_by(Event) %>%
        mutate(., EyeRecorded = ifelse(sum(LEFT_INTEREST_AREA_ID) > 0 & 
                                         sum(RIGHT_INTEREST_AREA_ID) > 0, "Both", 
                                       ifelse(sum(LEFT_INTEREST_AREA_ID) > 0 & 
                                                sum(RIGHT_INTEREST_AREA_ID) == 0, "Left", 
                                              ifelse(sum(LEFT_INTEREST_AREA_ID) == 0 & 
                                                       sum(RIGHT_INTEREST_AREA_ID) > 0, 
                                                     "Right", "NoIAData")))) %>%
        do(
          mutate(., EyeSelected = ifelse(EyeRecorded == "Both" & WhenLandR == "Right", "Right", 
                                         ifelse(EyeRecorded == "Both" & WhenLandR == "Left", "Left",
                                                ifelse(EyeRecorded == "Right", EyeRecorded, 
                                                       ifelse(EyeRecorded == "Left", EyeRecorded, 
                                                              ifelse(EyeRecorded == "NoIAData", "Neither"))))))
        ) 
        
      
    }
    
    if (Recording == "LorR") {
      
      tmp <- data %>%
        group_by(Event) %>%
        mutate(., EyeRecorded = ifelse(sum(LEFT_INTEREST_AREA_ID) > 0 & 
                                         sum(RIGHT_INTEREST_AREA_ID) == 0, "Left", 
                                       ifelse(sum(LEFT_INTEREST_AREA_ID) == 0 & 
                                                sum(RIGHT_INTEREST_AREA_ID) > 0, 
                                              "Right", "NoIAData"))) %>%
        do(
          mutate(., EyeSelected = ifelse(EyeRecorded == "Right", EyeRecorded, 
                                         ifelse(EyeRecorded == "Left", EyeRecorded, 
                                                ifelse(EyeRecorded == "NoIAData", "Neither"))))
        ) 
      
    }
    
    if (Recording == "R" | Recording == "L") {
    if (Recording == "R") {
        col <- "RIGHT_INTEREST_AREA_ID"
        col <- enquo(col)
        val <- "Right"
        val <- enquo(val)
        } else {
        col <- "LEFT_INTEREST_AREA_ID"
        col <- enquo(col)
        val <- "Left"
        val <- enquo(val)
        }

      tmp <- data %>%
        group_by(Event) %>%
        mutate(., EyeRecorded = ifelse(sum(UQ(sym(eval_tidy(col)))) > 0, quo_name(val), "NoIAData")) %>%
         do(
          mutate(., EyeSelected = ifelse(EyeRecorded == quo_name(val), EyeRecorded, 
                                          ifelse(EyeRecorded == "NoIAData", "Neither")))
         )
    } 
    
#        if (Recording == "R") {
#
#      tmp <- data %>%
#        group_by(Event) %>%
#        mutate(., EyeRecorded = ifelse(sum(RIGHT_INTEREST_AREA_ID) > 0, "Right", "NoIAData")) %>%
#         do(
#          mutate(., EyeSelected = ifelse(EyeRecorded == "Right", EyeRecorded, 
#                                          ifelse(EyeRecorded == "NoIAData", "Neither")))
#         )
#    } 
#   
#   if (Recording == "L") {
#
#      tmp <- data %>%
#        group_by(Event) %>%
#        mutate(., EyeRecorded = ifelse(sum(LEFT_INTEREST_AREA_ID) > 0, "Left", "NoIAData")) %>%
#         do(
#          mutate(., EyeSelected = ifelse(EyeRecorded == "Left", EyeRecorded, 
#                                          ifelse(EyeRecorded == "NoIAData", "Neither")))
#         )
#    } 
    
  } 
  
  tmp <- tmp %>% 
    do(
      mutate(., IA_ID = ifelse(EyeSelected == "Right", RIGHT_INTEREST_AREA_ID, 
                               ifelse(EyeSelected == "Left", LEFT_INTEREST_AREA_ID,
                                      ifelse(EyeSelected == "Neither", 0, NA))),
             IA_LABEL = ifelse(EyeSelected == "Right", as.character(RIGHT_INTEREST_AREA_LABEL), 
                               ifelse(EyeSelected == "Left", as.character(LEFT_INTEREST_AREA_LABEL),
                                      ifelse(EyeSelected == "Neither", "Outside", NA))))
    )
  
  tmp$EyeRecorded <- as.factor(as.character(tmp$EyeRecorded))
  tmp$EyeSelected <- as.factor(as.character(tmp$EyeSelected))
  tmp$IA_ID <- as.numeric(as.character(tmp$IA_ID))
  tmp$IA_LABEL <- as.factor(as.character(tmp$IA_LABEL))
  
  tmp <- tmp %>%
    mutate(IA_Data = ifelse(sum(IA_ID) > 0, "Contains_IA_Looks", "No_IA_Looks"))
  tmp$IA_Data <- as.factor(as.character(tmp$IA_Data))
  
  message(paste("Gaze data summary for", length(unique(levels(tmp$Event))), "events:"))
  
  if (!(is.na(WhenLandR))) {
    message(paste(nrow(filter(tmp, Time==first(Time) & EyeRecorded=="Both")), "event(s) contained gaze data for both eyes, for which the", WhenLandR, "eye has been selected." ))
  }
  
  if (("EYE_TRACKED" %in% names(tmp)) | Recording == "LandR" | Recording == "LorR" | Recording == "R" ) {
    message(paste("The final data frame contains", nrow(filter(tmp, Time==first(Time) & EyeSelected=="Right")), "event(s) using gaze data from the right eye."))
  }
  
  if (("EYE_TRACKED" %in% names(tmp)) | Recording == "LandR" | Recording == "LorR" | Recording == "L" ) {
    message(paste("The final data frame contains", nrow(filter(tmp, Time==first(Time) & EyeSelected=="Left")), "event(s) using gaze data from the left eye."))
  }
  
  message(paste("The final data frame contains", nrow(filter(tmp, Time==first(Time) & IA_Data=="No_IA_Looks")), "event(s) with no samples falling within any interest area during the given time series."))
  
  return(ungroup(tmp))
}





#' Create a time series column
#' 
#' \code{create_time_series} standardizes the starting point for each 
#' event, creates a time series for each event including the offset for the 
#' amount of time prior to (or after) the zero point. The time series
#' is indicated in a new column called Time.
#' 
#' @export
#' @import dplyr
#' @import rlang
#' 
#' @param data A data table object output by \code{\link{relabel_na}} or 
#' \code{\link{align_msg}}.
#' @param Adjust Optionally an integer value or a text string. If an integer 
#' (positive or negative), this will indicate an amount of time in 
#' milliseconds. The value is subtracted from the time points: positive values
#' shift the zero forward; negative values shift the zero backward.
#' If a text string, this will be the name of a column in 
#' the data set which contains values indicating when the critical stimulus
#' was presented relative to the zero point. 
#' @return A data table object.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # To create the Time column...
#' df <- create_time_series(data = dat, Adjust = "SoundOnsetColumn")
#' # or  
#' df <- create_time_series(data = dat, Adjust = -100)
#' # or
#' df <- create_time_series(data = dat, Adjust = 100)
#' }
create_time_series <- function (data, Adjust = 0) 
{
  
  #    if ("Adj" %in% names(match.call())) {
  #    stop("'Adj' is deprecated; please use 'Adjust' instead. Please refer to the vignettes for explanation of usage.")
  #    } else if ("Offset" %in% names(match.call())) {
  #      stop("'Offset' is deprecated; please use 'Adjust' instead. Please refer to the vignettes for explanation of usage.")
  #    }
  
  adjust <- enquo(Adjust)
  # print(quo_name(adjust)) convert input expression to a string
  # print(eval_tidy(adjust)) evaluate expression in special environment
  if (is.numeric(eval_tidy(adjust))==T && !("Align" %in% colnames(data))) {
    if (eval_tidy(adjust)==0) {
      message("No adjustment applied.")
    } else {
      message(paste(quo_name(adjust), "ms adjustment applied."))
    }
    data %>% arrange(., Event, TIMESTAMP) %>% group_by(Event) %>% 
      mutate(Time = TIMESTAMP - first(TIMESTAMP) - !! adjust) %>% ungroup()
  } 
  else if (is.numeric(eval_tidy(adjust))==T && "Align" %in% colnames(data)) {
    if (eval_tidy(adjust)==0) {
      message("No adjustment applied.")
    } else {
      message(paste(quo_name(adjust), "ms adjustment applied."))
    }
    data %>% arrange(., Event, Align) %>% group_by(Event) %>% 
      mutate(Time = Align - !! adjust) %>% ungroup()
  } 
  else if (is.character(eval_tidy(adjust))==T) {
    message(paste("Adjustment applied using values contained in", quo_name(adjust)))
    data %>% arrange(., Event, Align) %>% group_by(Event) %>% 
      mutate(Time = Align - UQ(sym(eval_tidy(adjust)))) %>% ungroup()
  }
}

