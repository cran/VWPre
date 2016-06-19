#' Check the classes of specific columns and re-assigns as necessary.
#' 
#' \code{prep_data} converts the data frame to a data table and examines the 
#' class of the following columns: Subject, Item, LEFT_INTEREST_AREA_ID, 
#' RIGHT_INTEREST_AREA_ID, LEFT_INTEREST_AREA_LABEL, RIGHT_INTEREST_AREA_LABEL, 
#' TIMESTAMP, and TRIAL_INDEX. If they were not encoded with the correct 
#' class upon importing the data, the function will reassign the class and print 
#' a summary of the reassignments. Additionally, the data table output by 
#' the function contains a new column called Event which indexes each unique 
#' series of samples corresponding to the combination of Subject and 
#' TRIRAL_Index, necessary for performing subsequent operations.
#' 
#' @export
#' @import dplyr
#' @import tidyr
#' @import lazyeval
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
#' df <- prep_data(data = dat, Subject = "RECORDING_SESSION_LABEL", Item = "itemID")
#' }
prep_data <- function(data = data, Subject = Subject, Item = NA,
                      EventColumns=c("Subject","TRIAL_INDEX")){
  subject <- Subject
  item <- Item
  
  data <- tbl_df(data)
  
  message("Step 1 of 9...")
  data <- rename_(data, Subject = interp(~subject, subject = as.name(subject)))
  message(paste(subject, "renamed to Subject."))
  
  if (is.factor(data$Subject) == F){
    data$Subject <- as.factor(as.character(data$Subject))
    message("Subject converted to factor.")
  } else {
    message("Subject already factor.")
  }
  
  message("Step 2 of 9...")
  if (!is.na(item)) {
    data <- rename_(data, Item = interp(~item, item = as.name(item)))
    message(paste(item, "renamed to Item."))
    if (is.factor(data$Item) == F){
      data$Item <- as.factor(as.character(data$Item))
      message("Item converted to factor")
    } else {
      message("Item already factor")
    } 
  } else {
    message("No Item column specified.")
  }
  
  message("Step 3 of 9...")
  if (is.numeric(data$LEFT_INTEREST_AREA_ID) == F){
    data$LEFT_INTEREST_AREA_ID <- as.numeric(as.character(data$LEFT_INTEREST_AREA_ID))
    message("LEFT_INTEREST_AREA_ID converted to numeric.")
  } else {
    message("LEFT_INTEREST_AREA_ID already numeric.")
  }
  
  message("Step 4 of 9...")
  if (is.numeric(data$RIGHT_INTEREST_AREA_ID) == F){
    data$RIGHT_INTEREST_AREA_ID <- as.numeric(as.character(data$RIGHT_INTEREST_AREA_ID))
    message("RIGHT_INTEREST_AREA_ID converted to numeric.")
  } else {
    message("RIGHT_INTEREST_AREA_ID already numeric.")
  }
  
  message("Step 5 of 9...")
  if (is.factor(data$LEFT_INTEREST_AREA_LABEL) == F){
    data$LEFT_INTEREST_AREA_LABEL <- as.factor(as.character(data$LEFT_INTEREST_AREA_LABEL))
    message("LEFT_INTEREST_AREA_LABEL converted to factor.")
  } else {
    message("LEFT_INTEREST_AREA_LABEL already factor.")
  }
  
  message("Step 6 of 9...")
  if (is.factor(data$RIGHT_INTEREST_AREA_LABEL) == F){
    data$RIGHT_INTEREST_AREA_LABEL <- as.factor(as.character(data$RIGHT_INTEREST_AREA_LABEL))
    message("RIGHT_INTEREST_AREA_LABEL converted to factor.")
  } else {
    message("RIGHT_INTEREST_AREA_LABEL already factor.")
  }
  
  message("Step 7 of 9...")
  if (is.numeric(data$TIMESTAMP) == F){
    data$TIMESTAMP <- as.numeric(as.character(data$TIMESTAMP))
    message("TIMESTAMP converted to numeric.")
  } else {
    message("TIMESTAMP already numeric.")
  }
  
  message("Step 8 of 9...")
  if (is.numeric(data$TRIAL_INDEX) == F){
    data$TRIAL_INDEX <- as.numeric(as.character(data$TRIAL_INDEX))
    message("TRIAL_INDEX converted to numeric.")
  } else {
    message("TRIAL_INDEX already numeric.")
  }
  
  message("Step 9 of 9...")
  data$Event <- interaction(data[,EventColumns], drop=TRUE)
  message(paste("Event variable created from", EventColumns[1], "and", EventColumns[2]))
  
  return(ungroup(data))
}





#' Relabel samples containing 'NA' as outside any interest area
#' 
#' \code{relabel_na} examines interest area columns (LEFT_INTEREST_AREA_ID, 
#' RIGHT_INTEREST_AREA_ID, LEFT_INTEREST_AREA_LABEL, and RIGHT_INTEREST_AREA_LABEL)
#' for cells containing NAs. If NA, the missing values in the ID columns are 
#' relabeled as 0 and missing values in the LABEL columns are relabeled as 'Outside'.
#' 
#' @export
#' @import dplyr
#' @import tidyr
#' @import lazyeval
#' 
#' @param data A data table object output by \code{\link{prep_data}}.
#' @param NoIA A positive integer indicating the number of interest areas defined 
#' when creating the study.
#' @return A data table with the same dimensions as \code{data}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # To relabel the NAs...
#' df <- relabel_na(data = dat, NoIA = 4)
#' }
relabel_na <- function(data = data, NoIA = NoIA){
  NoIA <- NoIA
  
  if (length(levels(data$LEFT_INTEREST_AREA_LABEL)) == NoIA) {
  message("LEFT_INTEREST_AREA_LABEL: Number of levels match NoIA.")
  } else {
  message("LEFT_INTEREST_AREA_LABEL: Number of levels DO NOT match NoIA.")
  }
  
  if (length(levels(data$RIGHT_INTEREST_AREA_LABEL)) == NoIA) {
  message("RIGHT_INTEREST_AREA_LABEL: Number of levels match NoIA.")
  } else {
  message("RIGHT_INTEREST_AREA_LABEL: Number of levels DO NOT match NoIA.")
  } 
  
  data$RIGHT_INTEREST_AREA_ID[is.na(data$RIGHT_INTEREST_AREA_ID)] = 0
  levels(data$RIGHT_INTEREST_AREA_LABEL)[NoIA+1] <- "Outside"
  data$RIGHT_INTEREST_AREA_LABEL[is.na(data$RIGHT_INTEREST_AREA_LABEL)] = "Outside"
  
  data$LEFT_INTEREST_AREA_ID[is.na(data$LEFT_INTEREST_AREA_ID)] = 0
  levels(data$LEFT_INTEREST_AREA_LABEL)[NoIA+1] <- "Outside"
  data$LEFT_INTEREST_AREA_LABEL[is.na(data$LEFT_INTEREST_AREA_LABEL)] = "Outside"
  
  return(ungroup(data))
  
}


#' Recode interest area IDs and/or interest area labels
#' 
#' \code{recode_ia} replaces existing interest area IDs and/or labels for both 
#' eyes. For subsequent data processing, it is important that the ID values range 
#' between 0 and 8 (with 0 representing Outside all predefined interest areas).
#' LEFT_INTEREST_AREA_ID, 
#' RIGHT_INTEREST_AREA_ID, LEFT_INTEREST_AREA_LABEL, and RIGHT_INTEREST_AREA_LABEL)
#' for cells containing NAs. If NA, the missing values in the ID columns are 
#' relabeled as 0 and missing values in the LABEL columns are relabeled as 'Outside'.
#' 
#' @export
#' @import dplyr
#' @import tidyr
#' @import lazyeval
#' 
#' @param data A data table object output by \code{\link{relabel_na}}.
#' @param IDs A named character vector specifying the desired interest area 
#' IDs and the corresponding existing IDs where the first element is the old
#' value and the second element is the new value.
#' @param Labels A named character vector specifying the desired interest area 
#' labels and the corresponding existing labels where the first element is the 
#' old value and the second element is the new value.
#' @return A data table with the same dimensions as \code{data}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # To recode both IDs and Labels...
#' df <- recode_ia(data=dat, IDs=c("234"="2", "0"="0", "35"="3", "11"="1", "
#' 4"="6666"), Labels=c(Outside="Dumb", Target="Targ", Dist2="Stupid", 
#' Comp="Comp", Dist1="Distractor1"))
#' }
recode_ia <- function(data = data, IDs = NULL, Labels = NULL) {
  
  data <- data
  IDs <- IDs
  x <- setNames(names(IDs), IDs)
  IDs <- as.list(x)
  Labels <- Labels
  y <- setNames(names(Labels), Labels)
  Labels <- as.list(y)
  
  if(!(is.null(IDs))) {
    
    data$RIGHT_INTEREST_AREA_ID <- as.factor(as.character(data$RIGHT_INTEREST_AREA_ID))
    levels(data$RIGHT_INTEREST_AREA_ID) <- IDs
    data$RIGHT_INTEREST_AREA_ID <- as.integer(as.character(data$RIGHT_INTEREST_AREA_ID))
    message("Right interest area IDs recoded.")
    
    data$LEFT_INTEREST_AREA_ID <- as.factor(as.character(data$LEFT_INTEREST_AREA_ID))
    levels(data$LEFT_INTEREST_AREA_ID) <- IDs
    data$LEFT_INTEREST_AREA_ID <- as.integer(as.character(data$LEFT_INTEREST_AREA_ID))
    message("Left interest area IDs recoded.")
    
  }
  
  if(!(is.null(Labels))) {
    
    data$RIGHT_INTEREST_AREA_LABEL <- as.factor(as.character(data$RIGHT_INTEREST_AREA_LABEL))
    levels(data$RIGHT_INTEREST_AREA_LABEL) <- Labels
    message("Right interest area labels recoded.")
    
    data$LEFT_INTEREST_AREA_LABEL <- as.factor(as.character(data$LEFT_INTEREST_AREA_LABEL))
    levels(data$LEFT_INTEREST_AREA_LABEL) <- Labels
    message("Left interest area labels recoded.")
    
  }
  
  data <- droplevels(data)
  return(ungroup(data))
  
}

#' Select the eye used during recording
#' 
#' \code{select_recorded_eye} examines each event and determines which eye contains 
#' interest area information, based on the \code{Recording} parameter (which 
#' can be determined using \code{\link{check_eye_recording}}). 
#' This function then selects the data from the recorded eye and copies it
#' new columns (IA_ID and IA_LABEL). The function prints a summary of output.  
#' 
#' @export
#' @import dplyr
#' @import lazyeval
#' 
#' @param data A data table object output by \code{\link{create_time_series}}.
#' @param Recording A string indicating which eyes were used for recording gaze data.
#' @param WhenLandR A string indicating which eye ("Right" or "Left) to use 
#' if gaze data is available for both eyes (i.e., Recording = "LandR"). 
#' @return A data table with four additional columns ('EyeRecorded', 'EyeSelected',
#' 'IA_ID','IA_LABEL') added to \code{data}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Create a unified columns for the gaze data...
#' df <- select_recorded_eye(data = dat, Recording = "LandR", WhenLandR = "Right")
#' }
select_recorded_eye <- function(data = data, Recording = Recording, WhenLandR = NA) {
  Recording = Recording
  WhenLandR = WhenLandR
  
  if (Recording == "LandR") {
    
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
      ) %>%
      do(
        mutate(., IA_ID = ifelse(EyeSelected == "Right", RIGHT_INTEREST_AREA_ID, 
                                 ifelse(EyeSelected == "Left", LEFT_INTEREST_AREA_ID,
                                        ifelse(EyeSelected == "Neither", 0, NA))),
               IA_LABEL = ifelse(EyeSelected == "Right", as.character(RIGHT_INTEREST_AREA_LABEL), 
                                 ifelse(EyeSelected == "Left", as.character(LEFT_INTEREST_AREA_LABEL),
                                        ifelse(EyeSelected == "Neither", "Outside", NA))))
      )
    
  } else if (Recording == "LorR") {
    
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
      ) %>%
      do(
        mutate(., IA_ID = ifelse(EyeSelected == "Right", RIGHT_INTEREST_AREA_ID, 
                                 ifelse(EyeSelected == "Left", LEFT_INTEREST_AREA_ID,
                                        ifelse(EyeSelected == "Neither", 0, NA))),
               IA_LABEL = ifelse(EyeSelected == "Right", as.character(RIGHT_INTEREST_AREA_LABEL), 
                                 ifelse(EyeSelected == "Left", as.character(LEFT_INTEREST_AREA_LABEL),
                                        ifelse(EyeSelected == "Neither", "Outside", NA))))
      )
    
  } else if (Recording == "L") {
    
    tmp <- data %>%
      group_by(Event) %>%
      mutate(., EyeRecorded = ifelse(sum(LEFT_INTEREST_AREA_ID) > 0, "Left", "NoIAData")) %>%
      do(
        mutate(., EyeSelected = ifelse(EyeRecorded == "Left", EyeRecorded, 
                                       ifelse(EyeRecorded == "NoIAData", "Neither")))
      ) %>%
      do(
        mutate(., IA_ID = ifelse(EyeSelected == "Left", LEFT_INTEREST_AREA_ID,
                                 ifelse(EyeSelected == "Neither", 0, NA)),
               IA_LABEL = ifelse(EyeSelected == "Left", as.character(LEFT_INTEREST_AREA_LABEL),
                                 ifelse(EyeSelected == "Neither", "Outside", NA)))
      )
    
  } else if (Recording == "R") {
    
    tmp <- data %>%
      group_by(Event) %>%
      mutate(., EyeRecorded = ifelse(sum(RIGHT_INTEREST_AREA_ID) > 0, "Right", "NoIAData")) %>%
      do(
        mutate(., EyeSelected = ifelse(EyeRecorded == "Right", EyeRecorded, 
                                       ifelse(EyeRecorded == "NoIAData", "Neither")))
      ) %>%
      do(
        mutate(., IA_ID = ifelse(EyeSelected == "Right", RIGHT_INTEREST_AREA_ID,
                                 ifelse(EyeSelected == "Neither", 0, NA)),
               IA_LABEL = ifelse(EyeSelected == "Right", as.character(RIGHT_INTEREST_AREA_LABEL),
                                 ifelse(EyeSelected == "Neither", "Outside", NA)))
      )
    
  } 
  
  tmp$EyeRecorded <- as.factor(as.character(tmp$EyeRecorded))
  tmp$EyeSelected <- as.factor(as.character(tmp$EyeSelected))
  tmp$IA_ID <- as.numeric(as.character(tmp$IA_ID))
  tmp$IA_LABEL <- as.factor(as.character(tmp$IA_LABEL))
  
  message(paste("Gaze data summary for", length(unique(levels(tmp$Event))), "events:"))
  
  if (Recording == "LandR") {
    message(paste(nrow(filter(tmp, Time==first(Time) & EyeRecorded=="Both")), "event(s) contained gaze data for both eyes, for which the", WhenLandR, "eye has been selected." ))
  }
  
  if (Recording == "LandR" | Recording == "LorR" | Recording == "R" ) {
    message(paste("The final data frame contains", nrow(filter(tmp, Time==first(Time) & EyeSelected=="Right")), "event(s) using gaze data from the right eye."))
  }
  
  if (Recording == "LandR" | Recording == "LorR" | Recording == "L" ) {
    message(paste("The final data frame contains", nrow(filter(tmp, Time==first(Time) & EyeSelected=="Left")), "event(s) using gaze data from the left eye."))
  }
  
  message(paste("The final data frame contains", nrow(filter(tmp, Time==first(Time) & EyeSelected=="Neither")), "event(s) with no samples falling within any interest area during the given time series."))
  
  return(ungroup(tmp))
}




#' Create a meaningful time series column
#' 
#' \code{create_time_series} aligns the starting point for each 
#' event, creates a time series for each event including the offset for the 
#' amount of time prior to the onset auditory stimulus. The time series
#' is indicated in a new column called Time.
#' 
#' @export
#' @import dplyr
#' @import lazyeval
#' 
#' @param data A data table object output by \code{\link{relabel_na}}.
#' @param Offset A positive integer indicating amount of time in milliseconds.
#' @return A data table object.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # To create the Time column...
#' df <- create_time_series(data = dat, Offset = 100)
#' }
create_time_series = function(data = data, Offset = Offset){
  offset <- Offset
  data %>%
    arrange(., Event, TIMESTAMP) %>%
    group_by(Event) %>%
    # apply the offset to the data to create a proper time series
    mutate_(Time = interp(~TIMESTAMP - first(TIMESTAMP) - offset)) %>%
    ungroup()
}


