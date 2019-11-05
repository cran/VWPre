#' Check the new time series
#' 
#' \code{check_time_series} examines the first value in the Time column
#' for each event. If they are equal, it will return a single value.  The returned
#' value(s) will vary depending on the interest period (if defined), message 
#' alignment (if completed), and the Adjustment parameter (`Adj`) supplied to 
#' \code{\link{create_time_series}}. Optionally, the result can be output to a 
#' dataframe containing all event-level information.
#' 
#' @export
#' @import dplyr
#' 
#' @param data A data table object output by \code{\link{create_time_series}}.
#' @param ReturnData A logical indicating whether to return a data table 
#' containing Start Time information for each event.
#' @return The value(s) of Time (in milliseconds) at which events begin relative 
#' to the onset of the auditory stimulus.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Check the starting Time column...
#' check_time_series(data = dat)
#' }
check_time_series <- function(data, ReturnData = FALSE) {
  event_start_table = data %>% group_by(Event) %>% summarise(Start_Time = min(Time))
  # message(paste(utils::capture.output(unique(event_start_table$Start_Time)), collapse = "\n"))
  message(paste(utils::capture.output(distinct(event_start_table[, "Start_Time"], Start_Time)), collapse = "\n"))
  if(ReturnData==TRUE) {
    return(droplevels(ungroup(event_start_table)))
  }else {
    message("Set ReturnData to TRUE to output full, event-specific information.")
  }
}


#' Check the time value(s) at a specific message
#' 
#' \code{check_msg_time} examines the time point of a specific Sample Message
#' for each event. Depending on the format of the data, it will use one of three
#' columns: TIMESTAMP, Align, or Time.  If times at which the message occurs 
#' are equal, it will return a single value.  Optionally, the result can be 
#' output to a dataframe containing all event-level information.
#' 
#' @export
#' @import dplyr
#' @import rlang
#' 
#' @param data A data table object output by \code{\link{relabel_na}},
#' \code{\link{align_msg}}, or \code{\link{create_time_series}}.
#' @param Msg A character string containing the exact message to be found in 
#' the column SAMPLE_MESSAGE or a regular expression for locating the 
#' appropriate message.
#' @param ReturnData A logical indicating whether to return a data table 
#' containing Message Time information for each event.
#' @return The value(s) of Time (in milliseconds) at which the Sample Message
#' is found.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Check the Sample Message time...
#' check_msg_time(data = dat)
#' }
check_msg_time <- function(data, Msg = NULL, ReturnData = FALSE) {
  
  if(is.null(Msg)){
    stop("Please supply the message text or regular expression!")
  } else {
    msg <- Msg
    msg <- enquo(msg)
  }
  
  if(!("Time" %in% colnames(data)) && !("Align" %in% colnames(data))) {
    tmp <- data %>% filter(grepl(!!msg, SAMPLE_MESSAGE)) %>% select(Event, SAMPLE_MESSAGE, TIMESTAMP)
    message(paste(utils::capture.output(print(distinct(tmp[, c("SAMPLE_MESSAGE", "TIMESTAMP")], TIMESTAMP, .keep_all = TRUE))), collapse = "\n"))
  } else if (!("Time" %in% colnames(data)) && ("Align" %in% colnames(data))) {
    tmp <- data %>% filter(grepl(!!msg, SAMPLE_MESSAGE)) %>% select(Event, SAMPLE_MESSAGE, Align)
    message(paste(utils::capture.output(print(distinct(tmp[, c("SAMPLE_MESSAGE", "Align")], Align, .keep_all = TRUE))), collapse = "\n"))
  } else if ("Time" %in% colnames(data)) {
    tmp <- data %>% filter(grepl(!!msg, SAMPLE_MESSAGE)) %>% select(Event, SAMPLE_MESSAGE, Time)
    message(paste(utils::capture.output(print(distinct(tmp[, c("SAMPLE_MESSAGE", "Time")], Time, .keep_all = TRUE))), collapse = "\n"))
  }
  
  if(ReturnData==TRUE) {
    return(droplevels(tmp))
  } else {
    message("Set ReturnData to TRUE to output full, event-specific information.")
  }
}


#' Check the number of samples in each bin
#' 
#' \code{check_samples_per_bin} determines the number of samples in each
#' bin produced by \code{\link{bin_prop}}.
#' This function may be helpful for determining the obligatory parameter 
#' `ObsPerBin` which is input to \code{\link{transform_to_elogit}}.
#' 
#' @export
#' 
#' @param data A data table object output by \code{\link{bin_prop}}.
#' @return A printed summary of the number of samples in each bin.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Determine the number of samples per bin...
#' check_samples_per_bin(dat)
#' }
check_samples_per_bin <- function (data) {
  samples <- max(data$NSamples)
  rate <- abs(data$Time[2] - data$Time[1])
  message(paste("There are", samples, "samples per bin."))
  message(paste("One data point every", rate, "millisecond(s)"))
  
  if(length(unique(data$NSamples)) > 1){
    message(paste("\nThere are data points with less than", samples, "samples per bin."))
    message("Subsequent Empirical Logit calculations may be influenced by the number of samples (depending on the number of observations requested).")

    tmp <- data %>% group_by(Event) %>% 
        mutate(IsMaxTime = ifelse(Time == max(Time), TRUE, FALSE)) %>% 
        filter(NSamples != samples) 
    
    
    if(all(tmp$IsMaxTime==TRUE)) {
    message(paste("These all occur in the last bin of the time series (typical of Data Viewer output)."))
      
    } else {
      message("")
      warning(paste("Differing number of samples occur throughout the time series, at time bin(s):", utils::capture.output(cat(unique(tmp$Time))),
                    "\n  This may indicate sampling issues, perhaps due to the removal of data prior to binning.\n  Because of this, tranformation to Empirical Logits should be done with caution.", collapse = "\n"))
    }
    
  }
  
}




#' Determine the sampling rate present in the data 
#' 
#' \code{check_samplingrate} determines the sampling rate in the data.
#' This function is helpful for determining the obligatory parameter input to 
#' \code{\link{bin_prop}}. If different sampling rates were used, the 
#' function adds a sampling rate column, which can be used to subset the 
#' data for further processing.
#' 
#' @export
#' @import dplyr
#' 
#' @param data A data table object output by \code{\link{select_recorded_eye}}.
#' @param ReturnData A logical indicating whether to return a data table containing 
#' a new column called SamplingRate
#' @return A printed summary and/or a data table object 
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Determine the sampling rate...
#' check_samplingrate(dat)
#' }
check_samplingrate <- function(data, ReturnData = FALSE) {
  ReturnData <- ReturnData
  
  tmp <- data %>%
    group_by(Event) %>%
    mutate(., SamplingRate = 1000 / (Time[2] - Time[1]))
  message(paste("Sampling rate(s) present in the data are:", utils::capture.output(cat(unique(tmp$SamplingRate))), "Hz."), collapse = "\n")

  if (length(unique(tmp$SamplingRate)) > 1) {
    warning("There are multiple sampling rates present in the data. \n Please use the ReturnData parameter to include a sampling rate column in the dataset. \n This can be used to subset by sampling rate before proceeding with the remaining preprocessing operations.")
  } 
  
  if (ReturnData == TRUE) {
  return(droplevels(ungroup(tmp)))
  } else {
    message("Set ReturnData to TRUE to output full, event-specific information.")
  }
  
}



#' Determine downsampling options based on current sampling rate
#' 
#' \code{ds_options} determines the possible rates to which 
#' the current sampling rate can downsampled. It then prints the 
#' options in both bin size (milliseconds) and corresponding 
#' sampling rate (Hertz).
#' 
#' @export
#' 
#' @param SamplingRate A positive integer indicating the sampling rate (in Hertz) 
#' used to record the gaze data, which can be determined with the function 
#' \code{\link{check_samplingrate}}.
#' @param OutputRates A string ("Suggested" or "All") controlling if all rates 
#' are output, or if only whole rates (default) are output.
#' @return A printed summary of options (bin size and rate) for downsampling.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Determine downsampling options...
#' ds_options(SamplingRate = 1000)
#' }
ds_options <- function(SamplingRate, OutputRates = "Suggested") {
  samp <- 1000/SamplingRate
  if (OutputRates == "Suggested") {
    message("Suggested binning/downsampling options:")
  }
  else if (OutputRates == "All") {
    message("All binning/downsampling options:")
  }
  for (x in 1:100) {
    if (x %% (1000/SamplingRate) == 0) {
      if (OutputRates == "Suggested") {
        if((1000/x) %% 1 == 0) {
          message(paste("Bin size:", x, "ms; Samples per bin:", x/samp, "samples;", "Downsampled rate:", round(1000/x, 4), "Hz"))
        }
      }
      else if (OutputRates == "All") {
        message(paste("Bin size:", x, "ms; Samples per bin:", x/samp, "samples;", "Downsampled rate:", round(1000/x, 4), "Hz"))
      }
    }
  }
}


#' Rename default column names for interest areas.
#' 
#' \code{rename_columns} will replace the default numerical coding of the 
#' interest area columns with more meaningful user-specified names. For example,
#' IA_1_C and IA_1_P could be converted to IA_Target_C and IA_Target_P. Again, 
#' this will work for upto 8 interest areas.
#' 
#' @export
#' 
#' @param data A data table object output by either \code{\link{bin_prop}}. 
#' \code{\link{transform_to_elogit}}, or \code{\link{create_binomial}}.
#' @param Labels A named character vector specifying the interest areas and the
#' desired names to be inserted in place of the numerical labelling.
#' @return A data table object with renamed columns.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # For renaming default interest area columns
#' dat2 <- rename_columns(dat, Labels = c(IA1="Target", IA2="Rhyme", 
#'                            IA3="OnsetComp", IA4="Distractor")) 
#' }
rename_columns <- function(data, Labels = NULL) {
  
  # Check if PupilPre is installed
  .check_for_PupilPre(type="NotAvailable")
  
  tmp <- data
  
  if(is.null(Labels)){
    stop("Please supply the interest area names!")
  }
  
  if (length(names(Labels))>8) {
    stop("You have more than 8 interest areas.")
  } else {
    message(paste("Renaming", length(names(Labels)), "interest areas.", sep = " "))
  }
  
  Labels <- c("0" = "outside", Labels)
  
  NoIA <- length(names(Labels))
  
  for (x in 1:NoIA) {
    Labels[[x]] <- paste("_",Labels[[x]],"_", sep = "")
    names(Labels)[x] <- paste("_",x-1,"_", sep = "")
    tmp<-stats::setNames(tmp, gsub(names(Labels)[x],Labels[[x]],names(tmp)))
  }
  
  custcol <- data.frame(C1 = grep("[_0-9]+[_V_]+[0-9_]", names(tmp), value = TRUE),
                        C2 = rep(NA, length(grep("[_0-9]+[_V_]+[0-9_]", names(tmp), value = TRUE))))
  
  if (nrow(custcol) > 0) {
    for (y in 1:nrow(custcol)) {
      for (z in 1:length(Labels)) {
        custcol[y,2]<-gsub(paste(z-1), Labels[[z]], custcol[y,1])
        tmp<-stats::setNames(tmp, gsub(custcol[y,1], custcol[y,2], names(tmp)))
      }
    }
  }
  
  return(droplevels(ungroup(tmp)))
  
}


#' Output all messages with timestamps
#' 
#' \code{check_all_msgs} outputs all sample messages present in the data. 
#' Optionally, the result can be output to a dataframe containing all event-level
#' information.
#' 
#' @export
#' @import dplyr
#' @import rlang
#' 
#' @param data A data table object output by \code{\link{prep_data}},
#' \code{\link{align_msg}}, or \code{\link{create_time_series}}.
#' @param ReturnData A logical indicating whether to return a data table 
#' containing Message Time information for each event.
#' @return All Sample Messages found in the data.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Check all messages in the data...
#' check_all_msgs(data = dat)
#' }
check_all_msgs <- function(data, ReturnData = FALSE) {
  
  # Find time columns in data
  cols <- c("Event", "Subject", "TRIAL_INDEX", "SAMPLE_MESSAGE", "TIMESTAMP")
  if("Time" %in% colnames(data)) {
    cols <- c(cols, "Time")
  } 
  if("Align" %in% colnames(data)) {
    cols <- c(cols, "Align")
  } 
  
  selcols <- cols
  selcols <- quos(UQ(selcols))
  msgs <- data %>% filter(!is.na(data$SAMPLE_MESSAGE)) %>% 
    select(!!!selcols)
    
  if(ReturnData==FALSE) {
    message(paste(utils::capture.output(print(distinct(msgs[, "SAMPLE_MESSAGE"], SAMPLE_MESSAGE))), collapse = "\n"))
    message("Set ReturnData to TRUE to output full, event-specific information.")
  } else if(ReturnData==TRUE) {
    return(droplevels(msgs))
  }
}

#' Check which eyes were recorded during the experiment
#' 
#' \code{check_eye_recording} quickly checks which eyes contain gaze data
#' either using the EYE_TRACKED column (if available) or the Right and 
#' Left interest area columns. It prints a summary and 
#' suggests which setting to use for the \code{Recording} parameter in the 
#' function \code{\link{select_recorded_eye}}.
#' 
#' @export
#' @import dplyr
#' @import rlang
#' 
#' @param data A data table object output by \code{\link{create_time_series}}.
#' @return Text feedback and instruction.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Create a unified columns for the gaze data...
#' check_eye_recording(dat)
#' }
check_eye_recording <- function(data) {

  # Check if PupilPre is installed
  .check_for_PupilPre(type = "UseOther", suggest = "ppl_check_eye_recording")

  # Establish which columns needed, if EYE_TRACKED not present
  lcol <- "LEFT_INTEREST_AREA_ID"
  rcol <- "RIGHT_INTEREST_AREA_ID"
  
  {## SHARED CODE BEGINS HERE ## 

  if("EYE_TRACKED" %in% names(data)) {
    
    message("Checking data using Data Viewer column EYE_TRACKED")
    tmp <- as.data.frame(table(data$EYE_TRACKED))
    
  } else if(!("EYE_TRACKED" %in% names(data))) {	
    
    # Prep columns for dplyr
    lcol <- enquo(lcol)
    rcol <- enquo(rcol)
    
    message(paste0("Checking gaze data using Data Viewer columns ", eval_tidy(lcol), " and ", eval_tidy(rcol), "."))
    tmp <- data %>% group_by(Event) %>% 
      summarize(left = sum(UQ(sym(eval_tidy(lcol))), na.rm = TRUE), right = sum(UQ(sym(eval_tidy(rcol))), na.rm = TRUE)) %>%
      mutate(Var1 = ifelse(left > 0 & right > 0, "Both", 
                           ifelse(left > 0 & right == 0, "Left", 
                                  ifelse(left == 0 & right > 0, "Right", NA))))
  }
  
  if ("Both" %in% unique(tmp$Var1)) {
    b <- 1
  } else {
    b <- 0
  }
  if ("Left" %in% unique(tmp$Var1)) {
    l <- 1
  } else {
    l <- 0
  }
  if ("Right" %in% unique(tmp$Var1)) {
    r <- 1
  } else {
    r <- 0
  }
  
  if (b == 0 && l == 0 && r == 0) {
    message("No gaze data detected.")
  }
  if (b > 0) {
    if (l == 0 & r == 0) {
      message("The dataset contains recordings for both eyes (ALL participants had both eyes tracked). \n Set the Recording parameter in select_recorded_eye() to 'LandR' and the WhenLandR parameter to either 'Left' or 'Right'.")
    } else {
      message("The dataset contains recordings for both eyes (SOME participants had both eyes tracked). \n Set the Recording parameter in select_recorded_eye() to 'LorR'.")
    }
  } else {
    if (l > 0 && r == 0) {
      message("The dataset contains recordings for ONLY the left eye. \n Set the Recording parameter in select_recorded_eye() to 'L'.")
    } 
    if (l == 0 && r > 0) {
      message("The dataset contains recordings for ONLY the right eye. \n Set the Recording parameter in select_recorded_eye() to 'R'.")
    }
    if (l > 0 && r > 0) {
      message("The dataset contains recordings for both eyes (Participants had either the left eye OR the right eye tracked). \n Set the Recording parameter in select_recorded_eye() to 'LorR'.")
    }
  } 
    
  } ## SHARED CODE ENDS HERE ##
  
}

#' Check the interest area IDs and labels
#' 
#' \code{check_ia} examines both the interest area IDs and interest area labels
#' (and their mapping) for both eyes. It returns a summary of the information
#' and will stop if the data are not consistent with the requirements for
#' further processing.
#' 
#' @export
#' 
#' @param data A data table object output by \code{\link{relabel_na}}.
#' @return The value(s) and label(s) of interest areas and how they map for
#' each eye.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Check the interest area information...
#' check_ia(dat)
#' }
check_ia <- function(data) {
  
  # Check if PupilPre is installed
  .check_for_PupilPre(type="NotAvailable")
  
  # Check right eye
  Rias <- data %>% 
        rename(RIGHT_IA_ID = RIGHT_INTEREST_AREA_ID, 
           RIGHT_IA_LABEL = RIGHT_INTEREST_AREA_LABEL) %>% 
    group_by(RIGHT_IA_LABEL, RIGHT_IA_ID) %>% 
    summarise() %>% arrange(RIGHT_IA_ID) %>% 
    mutate(N = n())

  R_bad_id <- 0
  for (x in 1:nrow(Rias)) {
    if (Rias[x, "RIGHT_IA_ID"] >= 0 & Rias[x, "RIGHT_IA_ID"] <= 8) {
    } else {
      R_bad_id <- R_bad_id + 1
    }
  }
  
  R_bad_labels <- filter(Rias, N > 1)
  
  # Check left eye
  Lias <- data %>% 
    rename(LEFT_IA_ID = LEFT_INTEREST_AREA_ID, 
           LEFT_IA_LABEL = LEFT_INTEREST_AREA_LABEL) %>% 
    group_by(LEFT_IA_LABEL, LEFT_IA_ID) %>% 
    summarise() %>% arrange(LEFT_IA_ID) %>% 
    mutate(N = n())
  
  L_bad_id <- 0
  for (x in 1:nrow(Lias)) {
    if (Lias[x, "LEFT_IA_ID"] >= 0 & Lias[x, "LEFT_IA_ID"] <= 8) {
    } else {
      L_bad_id <- L_bad_id + 1
    }
  }
  
  L_bad_labels <- filter(Lias, N > 1)

  # Print mappings
  message(paste(utils::capture.output(print(as.data.frame(Rias[,2:1]), row.names=FALSE)), collapse = "\n"))
  message(paste(utils::capture.output(print(as.data.frame(Lias[,2:1]), row.names=FALSE)), collapse = "\n"))
  
  # Determine messages
  if (R_bad_id > 0) {
    Rstop <- "Interest Area IDs for the right eye are not between 0 and 8. Please recode before proceeding with data processing."
    Rmsg <- NULL
  } else {
    Rmsg <- "Interest Area IDs for the right eye are coded appropriately between 0 and 8."
    Rstop <- NULL
  }
  
  if (L_bad_id > 0) {
    Lstop <- "Interest Area IDs for the left eye are not between 0 and 8. Please recode before proceeding with data processing."
    Lmsg <- NULL
  } else {
    Lmsg <- "Interest Area IDs for the left eye are coded appropriately between 0 and 8."
    Lstop <- NULL
  }
  
  if(nrow(R_bad_labels) > 0) {
    Rstop2 <- "Interest Area ID and label combinations for the right eye are not consistent. Please correct before proceeding with data processing."
    Rmsg2 <- NULL
  } else {
    Rmsg2 <- "Interest Area ID and label mapping combinations for the right eye are consistent."
    Rstop2 <- NULL
  }
  
  if(nrow(L_bad_labels) > 0) {
    Lstop2 <- "Interest Area ID and label combinations for the left eye are not consistent. Please correct before proceeding with data processing."
    Lmsg2 <- NULL
  } else {
    Lmsg2 <- "Interest Area ID and label mapping combinations for the left eye are consistent."
    Lstop2 <- NULL
  }
  
  # Print messages
  if(!is.null(Rmsg)){
    msg <- Rmsg
  } else{
    msg <- character()
  }
  if(!is.null(Lmsg)){
    if(length(msg)>0){
      msg <- paste(msg, Lmsg, sep = "\n")
    } else{
      msg <- Lmsg
    }
  }
  if(!is.null(Rmsg2)){
    if(length(msg)>0){
    msg <- paste(msg, Rmsg2, sep = "\n")
    } else{
      msg <- Rmsg2
    }
  }
  if(!is.null(Lmsg2)){
    if(length(msg)>0){
    msg <- paste(msg, Lmsg2, sep = "\n")
    } else{
      msg <- Lmsg2
    }
  }
  if(length(msg)==0){
    msg <- NULL
  } else {message(msg)
    }

  # Print errors
  if(!is.null(Rstop)){
    stp <- Rstop
  } else{
    stp <- character()
  }
  if(!is.null(Lstop)){
    if(length(stp)>0){
      stp <- paste(stp, Lstop, sep = "\n")
    } else{
      stp <- Lstop
    }
  }
  if(!is.null(Rstop2)){
    if(length(stp)>0){
      stp <- paste(stp, Rstop2, sep = "\n")
    } else{
      stp <- Rstop2
    }
  }
  if(!is.null(Lstop2)){
    if(length(stp)>0){
      stp <- paste(stp, Lstop2, sep = "\n")
    } else{
      stp <- Lstop2
    }
  }
  if(length(stp)==0){
    stp <- NULL
  } else {stop(stp)
  }
  
# Possible stringr implementation  
#   message(str_glue('RIGHT_IA_ID \tRIGHT_IA_LABEL'))
#   message(
#   head(Rias,8) %>% str_glue_data("
# * {RIGHT_IA_ID} \t\t{RIGHT_IA_LABEL}
# 
# "))
#   
#   message(str_glue('LEFT_IA_ID \tLEFT_IA_LABEL'))
#   message(
#     head(Lias,8) %>% str_glue_data("
# * {LEFT_IA_ID} \t\t{LEFT_IA_LABEL}
# 
# "))
#   
#   message(str_glue('
# Messages: {ifelse(is.null(Rmsg), "", paste0("\n* ", Rmsg))} \\
# {ifelse(is.null(Rmsg2), "", paste0("\n* ", Rmsg2))} \\
# {ifelse(is.null(Lmsg), "", paste0("\n* ", Lmsg))} \\
# {ifelse(is.null(Lmsg2), "", paste0("\n* ", Lmsg2))} \n
# '))
#   
#   stop(str_glue('
# {ifelse(is.null(Rstop), "", paste0("\n! ", Rstop))} \\
# {ifelse(is.null(Rstop2), "", paste0("\n! ", Rstop2))} \\
# {ifelse(is.null(Lstop), "", paste0("\n! ", Lstop))} \\
# {ifelse(is.null(Lstop2), "", paste0("\n! ", Lstop2))} 
# '))
}




#' Internal helper function, not intended to be called externally. 
#'
#' @param type A string, "NotAvailable" of "UseOther".
#' @param suggest A string, PupilPre function name.
#' @return Text feedback and instruction. 
#' 
#'  
.check_for_PupilPre <- function(type, suggest) {

	if("PupilPre" %in% (.packages())){
		if(type == "NotAvailable") {
			stop("This function is not available for processing pupil size data using PupilPre. \n
			If you are processing Visual World Paradigm data using VWPre, please detach PupilPre using: detach(\"package:PupilPre\", unload=TRUE).")
		}
		if(type == "UseOther") {
			stop(paste0("This function is only available for Visual World Paradigm data. \n
			If you are processing pupil size data using PupilPre, please use ", suggest, ".\n
			If you are processing Visual World Paradigm data using VWPre, please detach PupilPre using: detach(\"package:PupilPre\", unload=TRUE)."))
		}
	}

}
