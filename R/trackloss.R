#' Mark trackloss by blink and/or screen size
#'
#' \code{mark_trackloss} marks data points related to trackloss for those in 
#' blink, off-screen, or both.
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data table object output by \code{\link{select_recorded_eye}}.
#' @param Type A string indicating "Blink", "OffScreen", or "Both".
#' @param ScreenSize A numeric vector specifying (in pixels) the dimensions
#' of the x and y of the screen used during the experiment.
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Mark trackloss...
#' df <- mark_trackloss(data = dat, Type = "Both", ScreenSize = c(1920, 1080))
#' }
mark_trackloss <- function(data,
                           Type = NULL,
                           ScreenSize = NULL) {
  
  # Check if PupilPre is installed
  .check_for_PupilPre(type="NotAvailable")
  
  if (is.null(Type)) {
    stop("Please choose type of trackloss method.")
  }
  
  if (!("Gaze_X" %in% names(data)) | !("Gaze_Y" %in% names(data))) {
    stop(
      "Gaze data columns Gaze_X and Gaze_Y not present for marking off-screen data.\nPlease set Type = 'Blink'."
    )
  }
  
  if ("NSamples" %in% names(data)) {
    stop(
      "This step should be performed prior to binning the data (i.e., the function 'bin_prop')."
    )
  }
  
  if (is.null(ScreenSize) & Type != "Blink") {
    stop("Please input screen size in pixels in the format: c(x, y).")
  }
  
  if (Type != "Blink") {
    # Mark data points
    message(paste0(
      "Marking data points outside of ",
      ScreenSize[1],
      "x",
      ScreenSize[2],
      "."
    ))
    data <- data %>% group_by(Event) %>%
      do(mutate(
        .,
        Screen = case_when(
          is.na(Gaze_X) | is.na(Gaze_Y) ~ "Unknown",
          (Gaze_X < 0) |
            (Gaze_X > ScreenSize[1]) ~ "OffScreen",
          (Gaze_Y < 0) |
            (Gaze_Y > ScreenSize[2]) ~ "OffScreen",
          TRUE ~ "OnScreen"
        )
      ))
    message(paste0("\n", round((
      nrow(data[data$Screen == "OffScreen",]) / nrow(data)
    ) * 100, 2), "% of data marked as off-screen"))
  }
  
  if (Type == "Blink") {
    data <- data %>% group_by(Event) %>%
      mutate(Trackloss = ifelse(In_Blink == 1, T, F))
  } else if (Type == "OffScreen") {
    data <- data %>% group_by(Event) %>%
      mutate(Trackloss = ifelse(Screen != "OnScreen", T, F))
  } else {
    data <- data %>% group_by(Event) %>%
      mutate(Trackloss = ifelse((Screen != "OnScreen") |
                                  In_Blink == 1, T, F))
  }
  
  message(paste0("\n", round((
    nrow(data[data$Trackloss == T,]) / nrow(data)
  ) * 100, 2), "% of data marked as trackloss"))
  
  return(droplevels(ungroup(data)))
}


#' Removes events with excessive trackloss
#'
#' \code{rm_trackloss_events} removes events with less data than the specified
#' amount.
#'
#' @export
#' @import dplyr
#' @import rlang
#'
#' @param data A data table object output by \code{\link{mark_trackloss}}.
#' @param RequiredData A number indicating the percentage of data required to 
#' be included (i.e., removes events with less than this amount of data).
#' @return An object of type data table as described in \link[tibble]{tibble}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Remove events...
#' df <- rm_trackloss_events(data = dat, RequiredData = 50)
#' }
rm_trackloss_events <- function(data = data, RequiredData = NULL) {
  
  # Check if PupilPre is installed
  .check_for_PupilPre(type="NotAvailable")
  
  # Check for Trackloss column
  if (!("Trackloss" %in% names(data))) {
    stop("Please determine trackloss using `mark_trackloss` before proceeding.")
  }
  
  # Calculate trackloss by event
  pre <- length(unique(data$Event))
  
  tmp <- data %>%
    group_by(Event) %>%
    summarise(Present = 100 - (sum(Trackloss) / n() * 100)) %>%
    filter(Present <= RequiredData) %>% droplevels()
  
  post <- length(unique(tmp$Event))
  
  if (pre - post == 0) {
    message("No events are below the required data threshold.")
  } else if(pre - post == pre) {
    message("All events are below the required data threshold.")
  } else {
    message(paste0(
      "Removing ",
      pre - post,
      " events with less than ",
      RequiredData,
      "% data present."
    ))
    
    # Drop events
    data <- data %>% filter(Event %in% unique(levels(tmp$Event)))
  }
  
  # return
  return(droplevels(ungroup(data)))
  
}

