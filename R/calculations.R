#' Bins the sample data and calculates proportion looks by interest area
#' 
#' \code{bin_prop} calculates the proportion of looks (samples) to each 
#' interest area in a particular window of time (bin size). This function first
#' checks to see if the procedure is possible given the sampling rate and 
#' desired bin size. It then performs the calculation and downsampling, returning
#' new columns corresponding to each interest area ID (e.g., 'IA_1_C', 'IA_1_P').
#' The extention '_C' indicates the count of samples in the bin and the 
#' extension '_P' indicates the proportion. N.B.: This function will work for 
#' data with a maximum of 8 interest areas.
#' 
#' @export
#' @import dplyr
#' 
#' @param data A data table object output by \code{\link{select_recorded_eye}}
#' or \code{\link{check_samplingrate}}.
#' @param NoIA A positive integer indicating the number of interest areas defined 
#' when creating the study. 
#' @param BinSize A positive integer indicating the size of the binning window 
#' (in milliseconds).
#' @param SamplingRate A positive integer indicating the sampling rate (in Hertz) 
#' used to record the gaze data, which can be determined with the function 
#' \code{\link{check_samplingrate}}.
#' @return A data table with additional columns (the number of which depends on 
#' the number of interest areas specified) added to \code{data}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Bin samples and calculation proportions...
#' df <- bin_prop(dat, NoIA = 4, BinSize = 20, SamplingRate = 1000)
#' }
bin_prop <- function(data, NoIA = NULL, BinSize = NULL, SamplingRate = NULL) {
  
  # Check if PupilPre is installed
  .check_for_PupilPre(type="NotAvailable")
  
  if(is.null(NoIA)){
    stop("Please supply the number of interest areas!")
  } else if (NoIA == 0) {
    stop("You must have at least one interest area!")
  } else if (NoIA > 8) {
    stop("You have more than 8 interest areas; you must modify this function.")
  } 
  
  if(is.null(BinSize)){
    stop("Please supply the bin size!")
  } 
  if(is.null(SamplingRate)){
    stop("Please supply the sampling rate!")
  } 
  
  SamplesPerBin <- (SamplingRate / 1000) * BinSize
  samplerate <-  data$Time[2] - data$Time[1]
  
  if (BinSize %% samplerate != 0) {
    stop("Sample frequency of data is not a multiple of the target frequency. Please use the function ds_options to determine an appropriate bin size.")
  } else {
    message(paste("Binning information: \n", "Original rate of", SamplingRate, 
                  "Hz with one sample every", samplerate, "ms. \n", "Downsampled rate of", 
                  round(1000/BinSize, 2), "Hz using", BinSize, "ms bins. \n New bins contain", BinSize/samplerate, "samples."))
  }
  
  data$DS <- data$Time %/% BinSize
  data$DS <- data$DS * BinSize 
  
  message("Binning...")
  # Calculate counts
  if (NoIA >= 1) {
    data <- data %>%
      group_by(Event, DS) %>%
      mutate(., NSamples = n(), 
             IA_0_C = length(IA_ID[which(IA_ID == 0)]),
             IA_1_C = length(IA_ID[which(IA_ID == 1)])) 
  } 
  if (NoIA >= 2) {
    data <- data %>%
      #group_by(Event, DS) %>%
      mutate(., IA_2_C = length(IA_ID[which(IA_ID == 2)])) 
  } 
  if (NoIA >= 3) {
    data <- data %>%
      #group_by(Event, DS) %>%
      mutate(., IA_3_C = length(IA_ID[which(IA_ID == 3)])) 
  } 
  if (NoIA >= 4) {
    data <- data %>%
      #group_by(Event, DS) %>%
      mutate(., IA_4_C = length(IA_ID[which(IA_ID == 4)])) 
  } 
  if (NoIA >= 5) {
    data <- data %>%
      #group_by(Event, DS) %>%
      mutate(., IA_5_C = length(IA_ID[which(IA_ID == 5)])) 
  } 
  if (NoIA >= 6) {
    data <- data %>%
      #group_by(Event, DS) %>%
      mutate(., IA_6_C = length(IA_ID[which(IA_ID == 6)])) 
  } 
  if (NoIA >= 7) {
    data <- data %>%
      #group_by(Event, DS) %>%
      mutate(., IA_7_C = length(IA_ID[which(IA_ID == 7)]))
  } 
  if (NoIA == 8) {
    data <- data %>%
      #group_by(Event, DS) %>%
      mutate(., IA_8_C = length(IA_ID[which(IA_ID == 8)]))
  }
  
  # Downsample
  data <- data %>%
    group_by(Event, DS) %>%
    filter(., Time %in% DS)
  
  message("Calculating proportions...")
  # Calculate proportions
  if (NoIA >= 1) {
    data <- data %>%
      mutate(., IA_0_P = IA_0_C / NSamples,
             IA_1_P = IA_1_C / NSamples)
  } 
  if (NoIA >= 2) {
    data <- data %>%
      mutate(., IA_2_P = IA_2_C / NSamples)
  } 
  if (NoIA >= 3) {
    data <- data %>%
      mutate(., IA_3_P = IA_3_C / NSamples)
  } 
  if (NoIA >= 4) {
    data <- data %>%
      mutate(., IA_4_P = IA_4_C / NSamples)
  } 
  if (NoIA >= 5) {
    data <- data %>%
      mutate(., IA_5_P = IA_5_C / NSamples)
  } 
  if (NoIA >= 6) {
    data <- data %>%
      mutate(., IA_6_P = IA_6_C / NSamples)
  } 
  if (NoIA >= 7) {
    data <- data %>%
      mutate(., IA_7_P = IA_7_C / NSamples)
  } 
  if (NoIA == 8) {
    data <- data %>%
      mutate(., IA_8_P = IA_8_C / NSamples)
  }
  
  # tmp <- data %>% filter(NSamples != SamplesPerBin) %>% group_by(Event) 
  # message(paste("There are", nrow(tmp), "data points with less than", SamplesPerBin, "samples per bin."))
  # message("These can be examined and/or removed using the column 'NSamples'.")
  # message(paste("These occur at time bin(s):", utils::capture.output(cat(unique(tmp$Time))), collapse = "\n"))
  # message("Subsequent Empirical Logit calculations may be influenced by the number of samples (depending on the number of observations requested).")
  
  if(length(unique(data$NSamples)) > 1){

    tmp <- data %>%  group_by(Event) %>% 
        mutate(IsMaxTime = ifelse(Time == max(Time), TRUE, FALSE)) %>%
        filter(NSamples != SamplesPerBin) 
 
    message(paste("There are", nrow(tmp), "data points with less than", SamplesPerBin, "samples per bin."))
    message("These can be examined and/or removed using the column 'NSamples'.")
    message("Subsequent Empirical Logit calculations may be influenced by the number of samples (depending on the number of observations requested).")

  if(all(tmp$IsMaxTime==TRUE)) {
    message(paste("These all occur in the last bin of the time series (typical of Data Viewer output)."))
      
    } else {
      message("")
      warning(paste("Differing number of samples occur throughout the time series, at time bin(s):", utils::capture.output(cat(unique(tmp$Time))),
                    "\n  This may indicate sampling issues, perhaps due to the removal of data prior to binning.\n  Because of this, tranformation to Empirical Logits should be done with caution.", collapse = "\n"))
    }
  
  }
  
  return(droplevels(ungroup(data)))
}





#' Transforms proportion looks to empirical logits.
#' 
#' \code{transform_to_elogit} transforms the proportion of looks for 
#' each interest area to empirical logits. Proportions are inherently bound 
#' between 0 and 1 and are therefore not suitable for some types of analysis. 
#' Logits provide an unbounded measure, though range from negative infinity to 
#' infinity, so it is important to know that this logit function adds a constant 
#' (hence, empirical logit). Additionally this calculates weights which estimate 
#' the variance in each bin (because the variance of the logit depends on the 
#' mean). This is important for regression analyses. N.B.: This function will 
#' work for data with a maximum of 8 interest areas.
#' 
#' These calculations were adapted from:
#' Barr, D. J., (2008) Analyzing 'visual world' eyetracking data using 
#' multilevel logistic regression, \emph{Journal of Memory and Language}, 
#' \emph{59}(4), 457--474.
#' 
#' @export
#' @import dplyr
#' @import rlang
#' 
#' @param data A data table object output by \code{\link{bin_prop}}.
#' @param NoIA A positive integer indicating the number of interest areas defined 
#' when creating the study. 
#' @param ObsPerBin A positive integer indicating the number of observations to
#' use in the calculation. Typically, this will be the number of samples per 
#' bin, which can be determined with \code{\link{check_samples_per_bin}}.
#' @param ObsOverride A logical value controlling restrictions on the value
#' provided to ObsPerBin. Default value is FALSE.
#' @param Constant A positive number used for the empirical logit and weights
#' calculation; by default, 0.5 as in Barr (2008).
#' @return A data table with additional columns (the number of which depends on 
#' the number of interest areas specified) added to \code{data}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Convert proportions to empirical logits and calculate weights...
#' df <- transform_to_elogit(dat, NoIA = 4, ObsPerBin = 20, Constant = 0.5)
#' }
transform_to_elogit <- function(data, NoIA = NULL, ObsPerBin = NULL,
                                Constant = 0.5, ObsOverride = FALSE) {
  
  # Check if PupilPre is installed
  .check_for_PupilPre(type="NotAvailable")
  
  if(is.null(NoIA)){
    stop("Please supply the number of interest areas!")
  } else if (NoIA == 0) {
    stop("You must have at least one interest area!")
  } else if (NoIA > 8) {
    stop("You have more than 8 interest areas; you must modify this function.")
  }
  
  if(is.null(ObsPerBin)){
    stop("Please supply the number of observations per bin!")
  } 
  
  if (ObsPerBin == max(data$NSamples)) {
    message("Number of Observations equal to Number of Samples. \n Calculation will be based on Number of Samples.")
    data$Obs <- data$NSamples
  } else if (ObsPerBin == (data$Time[2] - data$Time[1])) {
    if (ObsPerBin > max(data$NSamples) & ObsOverride == FALSE) {
      stop("It is not advisable have a Number of Observations greater than the Number of Samples present in the data.  \n This error can be overridden with the parameter ObsOverride, though, do so with caution.")
    } else {
      message("Number of Observations has been user-defined and equals the Bin Size in milliseconds. \n Calculation will be based on time in milliseconds.")
      data$Obs <- ObsPerBin
    }
  } else {
    if (ObsPerBin > max(data$NSamples) & ObsOverride == FALSE) {
      stop("It is not advisable have a Number of Observations greater than the Number of Samples present in the data.  \n This error can be overridden with the parameter ObsOverride, though, do so with caution.")
    } else {
      message("Number of Observations has been user-defined. \n Calculation will be based on user-defined value.")
      data$Obs <- ObsPerBin
    }
  }  
  
  elogit = function(proportion=proportion, observations=observations, constant=constant) {
    return(log((proportion * observations + constant)/((1 - proportion) * 
                                                         observations + constant)))
  }
  
  weight = function(proportion=proportion, observations=observations, constant=constant) {
    return((1/(proportion * observations + constant)) + (1/((1 - proportion) * observations + constant)))
  }
  
  
  if (NoIA >= 1) {
    data <- data %>% ungroup() %>% 
      mutate(IA_0_ELogit = elogit(proportion=IA_0_P, observations=Obs, constant=Constant),
             IA_0_wts = weight(proportion=IA_0_P, observations=Obs, constant=Constant),
             IA_1_ELogit = elogit(proportion=IA_1_P, observations=Obs, constant=Constant),
             IA_1_wts = weight(proportion=IA_1_P, observations=Obs, constant=Constant)
      )
  }
  if (NoIA >= 2) {
    data <- data %>%
      mutate(IA_2_ELogit = elogit(proportion=IA_2_P, observations=Obs, constant=Constant),
             IA_2_wts = weight(proportion=IA_2_P, observations=Obs, constant=Constant)
      )
  }
  if (NoIA >= 3) {
    data <- data %>%
      mutate(IA_3_ELogit = elogit(proportion=IA_3_P, observations=Obs, constant=Constant),
             IA_3_wts = weight(proportion=IA_3_P, observations=Obs, constant=Constant)
      )
  }
  if (NoIA >= 4) {
    data <- data %>%
      mutate(IA_4_ELogit = elogit(proportion=IA_4_P, observations=Obs, constant=Constant),
             IA_4_wts = weight(proportion=IA_4_P, observations=Obs, constant=Constant)
      )
  }
  if (NoIA >= 5) {
    data <- data %>%
      mutate(IA_5_ELogit = elogit(proportion=IA_5_P, observations=Obs, constant=Constant),
             IA_5_wts = weight(proportion=IA_5_P, observations=Obs, constant=Constant)
      )
  }
  if (NoIA >= 6) {
    data <- data %>%
      mutate(IA_6_ELogit = elogit(proportion=IA_6_P, observations=Obs, constant=Constant),
             IA_6_wts = weight(proportion=IA_6_P, observations=Obs, constant=Constant)
      )
  }
  if (NoIA >= 7) {
    data <- data %>%
      mutate(IA_7_ELogit = elogit(proportion=IA_7_P, observations=Obs, constant=Constant),
             IA_7_wts = weight(proportion=IA_7_P, observations=Obs, constant=Constant)
      )
  }
  if (NoIA == 8) {
    data <- data %>%
      mutate(IA_8_ELogit = elogit(proportion=IA_8_P, observations=Obs, constant=Constant),
             IA_8_wts = weight(proportion=IA_8_P, observations=Obs, constant=Constant)
      )
  }
  return(droplevels(ungroup(data)))
}





#' Creates a success/failure column for each IA based on counts.
#' 
#' \code{create_binomial} uses interest area count columns to create 
#' a success/failure column for each IA which is suitable for logistic regression. 
#' N.B.: This function will work for data with a maximum of 8 interest areas.
#' 
#' @export
#' @import dplyr
#' @import rlang
#' 
#' @param data A data table object output by either \code{\link{bin_prop}} or
#' \code{\link{transform_to_elogit}}.
#' @param NoIA A positive integer indicating the number of interest areas defined 
#' when creating the study. 
#' @param ObsPerBin A positive integer indicating the number of observations to
#' use in the calculation. Typically, this will be the number of samples per 
#' bin, which can be determined with \code{\link{check_samples_per_bin}}.
#' @param ObsOverride A logical value controlling restrictions on the value
#' provided to ObsPerBin. Default value is FALSE.
#' @param CustomBinom An optional parameter specifying a vector containing two 
#' integers corresponding to the interest area IDs to be combined.
#' @return A data table with additional columns (the number of which depends on 
#' the number of interest areas specified) added to \code{data}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Create binomial (success/failure) column...
#' df <- create_binomial(data = dat, NoIA = 4, ObsPerBin = 20)
#' }
create_binomial <- function(data, NoIA = NULL, ObsPerBin = NULL,
                            ObsOverride = FALSE, CustomBinom = NULL) {
  
  # Check if PupilPre is installed
  .check_for_PupilPre(type="NotAvailable")
  
  data <- data %>% ungroup()
  
  if(is.null(NoIA)){
    stop("Please supply the number of interest areas!")
  } else if (NoIA == 0) {
    stop("You must have at least one interest area!")
  } else if (NoIA > 8) {
    stop("You have more than 8 interest areas; you must modify this function.")
  }
  
  if(is.null(ObsPerBin)){
    stop("Please supply the number of observations per bin!")
  } 
  
  if (ObsPerBin == max(data$NSamples)) {
    message(
      "Number of Observations equal to Number of Samples. \n Counts will remain as present in the data."
    )
    data$Obs <- data$NSamples
  } else if (ObsPerBin == (data$Time[2] - data$Time[1])) {
    if (ObsPerBin %% 2 == 1 | ObsPerBin > max(data$NSamples)) {
      if (ObsOverride == FALSE) {
        stop("It is not advisable have an odd Number of Observations (induces artifacts in the scaling); \n nor is it advisable to have a Number of Observations greater than the Number of Samples present in the data. \n This error can be overridden with the parameter ObsOverride, though, do so with caution.")
      } else {
        message(
          "Number of Observations has been user-defined. \n Counts will be scaled to time in milliseconds using the proportion columns."
        )
        data$Obs <- ObsPerBin
      }
    } else {
      message(
        "Number of Observations has been user-defined. \n Counts will be scaled to time in milliseconds using the proportion columns."
      )
      data$Obs <- ObsPerBin
    }
  } else {
    if (ObsPerBin %% 2 == 1 | ObsPerBin > max(data$NSamples)) {
      if (ObsOverride == FALSE) {
        stop("It is not advisable have an odd Number of Observations (induces artifacts in the scaling); \n nor is it advisable to have a Number of Observations greater than the Number of Samples present in the data. \n This error can be overridden with the parameter ObsOverride, though, do so with caution.")
      } else {
        message(
          "Number of Observations has been user-defined. \n Counts will be scaled to the user-defined value using the proportion columns."
        )
        data$Obs <- ObsPerBin
      }
    } else {
      message(
        "Number of Observations has been user-defined. \n Counts will be scaled to the user-defined value using the proportion columns."
      )
      data$Obs <- ObsPerBin
    }
  }
  
  
  if (NoIA >= 1) {
    tmp <- data  
    tmp$IA_0_Looks = cbind(as.integer(round(tmp$IA_0_P*tmp$Obs)), as.integer(round((1-tmp$IA_0_P)*tmp$Obs)))
    tmp$IA_1_Looks = cbind(as.integer(round(tmp$IA_1_P*tmp$Obs)), as.integer(round((1-tmp$IA_1_P)*tmp$Obs)))
  }
  
  if (NoIA >= 2) {
    tmp$IA_2_Looks = cbind(as.integer(round(tmp$IA_2_P*tmp$Obs)), as.integer(round((1-tmp$IA_2_P)*tmp$Obs)))
  }
  
  if (NoIA >= 3) {
    tmp$IA_3_Looks = cbind(as.integer(round(tmp$IA_3_P*tmp$Obs)), as.integer(round((1-tmp$IA_3_P)*tmp$Obs)))
  }
  
  if (NoIA >= 4) {
    tmp$IA_4_Looks = cbind(as.integer(round(tmp$IA_4_P*tmp$Obs)), as.integer(round((1-tmp$IA_4_P)*tmp$Obs)))
  }
  
  if (NoIA >= 5) {
    tmp$IA_5_Looks = cbind(as.integer(round(tmp$IA_5_P*tmp$Obs)), as.integer(round((1-tmp$IA_5_P)*tmp$Obs)))
  }
  
  if (NoIA >= 6) {
    tmp$IA_6_Looks = cbind(as.integer(round(tmp$IA_6_P*tmp$Obs)), as.integer(round((1-tmp$IA_6_P)*tmp$Obs)))
  }
  
  if (NoIA >= 7) {
    tmp$IA_7_Looks = cbind(as.integer(round(tmp$IA_7_P*tmp$Obs)), as.integer(round((1-tmp$IA_7_P)*tmp$Obs)))
  }
  
  if (NoIA == 8) {
    tmp$IA_8_Looks = cbind(as.integer(round(tmp$IA_8_P*tmp$Obs)), as.integer(round((1-tmp$IA_8_P)*tmp$Obs)))
  }
  
  
  if (!(is.null(CustomBinom))) {
    newcol <- enquo(newcol)
    bindcol1 <- paste("IA_", CustomBinom[1], "_Looks", sep="")
    bindcol2 <- paste("IA_", CustomBinom[2], "_Looks", sep="")
    newcol <- paste("IA_", CustomBinom[1], "_V_", CustomBinom[2], "_Looks", sep="")
    tmp$Custom <- cbind(tmp[[bindcol1]][,1], tmp[[bindcol2]][,1])
    tmp <- rename(tmp, !!newcol := Custom)
  }
  
  
  return(droplevels(tmp))
}
