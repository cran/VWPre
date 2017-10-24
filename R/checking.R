#' Check the interest area IDs and labels
#' 
#' \code{check_ia} examines both the interest area IDs and interest area labels
#' (and their mapping) for both eyes. It returns a summary of the information.
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
  
  Rias <-
    data.frame(
      ID = unique(data$RIGHT_INTEREST_AREA_ID),
      Label = unique(data$RIGHT_INTEREST_AREA_LABEL)
    )
  
  Rcnt <- 0
  for (x in 1:nrow(Rias)) {
    if (Rias[x, "ID"] >= 0 & Rias[x, "ID"] <= 8) {
    } else {
      Rcnt <- Rcnt + 1
    }
  }
  Rias <- Rias[order(Rias$ID),]
  
  if (Rcnt > 0) {
    message("Interest Area IDs for the right eye are not between 0 and 8. Please recode.")
    message(paste(capture.output(print(Rias, row.names=F)), collapse = "\n"))
  } else {
    message("Interest Area IDs for the right eye are coded appropriately.")
    message(paste(capture.output(print(Rias, row.names=F)), collapse = "\n"))
  }
  
  
  Lias <-
    data.frame(
      ID = unique(data$LEFT_INTEREST_AREA_ID),
      Label = unique(data$LEFT_INTEREST_AREA_LABEL)
    )
  
  Lcnt <- 0
  for (x in 1:nrow(Lias)) {
    if (Lias[x, "ID"] >= 0 & Lias[x, "ID"] <= 8) {
    } else {
      Lcnt <- Lcnt + 1
    }
  }
  Lias <- Lias[order(Lias$ID),]
  
  if (Lcnt > 0) {
    message("Interest Area IDs for the left eye are not between 0 and 8. Please recode.")
    message(paste(capture.output(print(Lias, row.names=F)), collapse = "\n"))
  } else {
    message("Interest Area IDs for the left eye are coded appropriately.")
    message(paste(capture.output(print(Lias, row.names=F)), collapse = "\n"))
  }
  
}


#' Check the new time series
#' 
#' \code{check_time_series} examines the first value in the Time column
#' for each event. If they are equal, it will return a single value.  The returned
#' value(s) will vary depending on the interest period (if defined), message 
#' alignment (if completed), and the Adjustment parameter (`Adj`) supplied to 
#' \code{\link{create_time_series}}.
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
  message(paste(capture.output(unique(event_start_table$Start_Time)), collapse = "\n"))
  if(ReturnData==T) {
    return(ungroup(event_start_table))
  }
}


#' Check the time value(s) at a specific message
#' 
#' \code{check_msg_time} examines the time point of a specific Sample Message
#' for each event. Depending on the format of the data, it will use one of three
#' columns: TIMESTAMP, Align, or Time. 
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
    message(paste(capture.output(print(tmp)), collapse = "\n"))
  } else if (!("Time" %in% colnames(data)) && ("Align" %in% colnames(data))) {
    tmp <- data %>% filter(grepl(!!msg, SAMPLE_MESSAGE)) %>% select(Event, SAMPLE_MESSAGE, Align)
    message(paste(capture.output(print(tmp)), collapse = "\n"))
  } else if ("Time" %in% colnames(data)) {
    tmp <- data %>% filter(grepl(!!msg, SAMPLE_MESSAGE)) %>% select(Event, SAMPLE_MESSAGE, Time)
    message(paste(capture.output(print(tmp)), collapse = "\n"))
  }
  
  if(ReturnData==T) {
    return(tmp)
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
  message(paste("Sampling rate(s) present in the data are:", capture.output(cat(unique(tmp$SamplingRate))), "Hz."), collapse = "\n")

  if (length(unique(tmp$SamplingRate)) > 1) {
    warning("There are multiple sampling rates present in the data. \n Please use the ReturnData parameter to include a sampling rate column in the dataset. \n This can be used to subset by sampling rate before proceeding with the remaining preprocessing operations.")
  } 
  
  if (ReturnData == TRUE) {
  return(ungroup(tmp))
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




#' Check which eyes were recorded during the experiment
#' 
#' \code{check_eye_recording} quickly checks if the dataset contains gaze data
#' in both the Right and Left interest area columns. It prints a summary and 
#' suggests which setting to use for the \code{Recording} parameter in the 
#' function \code{\link{select_recorded_eye}}.
#' 
#' @export
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
  
  if (sum(data$LEFT_INTEREST_AREA_ID) > 0 & sum(data$RIGHT_INTEREST_AREA_ID) > 0) {
    message("The dataset contains recordings for both eyes. \n If any participants had both eyes tracked, set the Recording parameter in select_recorded_eye() to 'LandR'. \n If participants had either the left OR the right eye tracked, set the Recording parameter in select_recorded_eye() to 'LorR'.")
  } else if (sum(data$LEFT_INTEREST_AREA_ID) > 0 & sum(data$RIGHT_INTEREST_AREA_ID) == 0) {
    message("The dataset contains recordings for ONLY the left eye. \n Set the Recording parameter in select_recorded_eye() to 'L'.")
  } else if (sum(data$LEFT_INTEREST_AREA_ID) == 0 & sum(data$RIGHT_INTEREST_AREA_ID) > 0) {
    message("The dataset contains recordings for ONLY the right eye. \n Set the Recording parameter in select_recorded_eye() to 'R'.")
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
  
  custcol <- data.frame(C1 = grep("[_0-9]+[_V_]+[0-9_]", names(tmp), value = T),
                        C2 = rep(NA, length(grep("[_0-9]+[_V_]+[0-9_]", names(tmp), value = T))))
  
  if (nrow(custcol) > 0) {
    for (y in 1:nrow(custcol)) {
      for (z in 1:length(Labels)) {
        custcol[y,2]<-gsub(paste(z-1), Labels[[z]], custcol[y,1])
        tmp<-stats::setNames(tmp, gsub(custcol[y,1], custcol[y,2], names(tmp)))
      }
    }
  }
  
  return(ungroup(tmp))
  
}
