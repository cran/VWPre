#' Relabel samples containing 'NA' as outside any interest area
#' 
#' \code{relabel_na} examines interest area columns (LEFT_INTEREST_AREA_ID, 
#' RIGHT_INTEREST_AREA_ID, LEFT_INTEREST_AREA_LABEL, and RIGHT_INTEREST_AREA_LABEL)
#' for cells containing NAs. If NA, the missing values in the ID columns are 
#' relabeled as 0 and missing values in the LABEL columns are relabeled as 'Outside'.
#' 
#' @export
#' @import dplyr
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
relabel_na <- function(data, NoIA = NULL){
  
  # Check if PupilPre is installed
  .check_for_PupilPre(type="NotAvailable")
  
  if(is.null(NoIA)){
    stop("Please supply the number of interest areas!")
  }
  
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
  
  return(droplevels(ungroup(data)))
  
}


#' Recode interest area IDs and/or interest area labels
#' 
#' \code{recode_ia} replaces existing interest area IDs and/or labels for both 
#' eyes. For subsequent data processing, it is important that the ID values range 
#' between 0 and 8 (with 0 representing Outside all predefined interest areas).
#' 
#' @export
#' @import dplyr
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
#' df <- recode_ia(data=dat, IDs=c("234"="2", "0"="0", "35"="3", "11"="1", 
#' "4"="6666"), Labels=c(Outside="Outside", Target="NewTargName", 
#' Dist2="NewDist2Name", Comp="NewCompName", Dist1="NewDist1Name"))
#'  
#' # For a more complete tutorial on VWPre plotting functions:
#' vignette("SR_Interest_Areas", package="VWPre")
#' }
#' 
recode_ia <- function(data, IDs = NULL, Labels = NULL) {
  
  # Check if PupilPre is installed
  .check_for_PupilPre(type="NotAvailable")
  
  data <- data
  IDs <- IDs
  x <- stats::setNames(names(IDs), IDs)
  IDs <- as.list(x)
  Labels <- Labels
  y <- stats::setNames(names(Labels), Labels)
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
  
  return(droplevels(ungroup(data)))
  
}

#' Map gaze data to newly defined interest areas
#' 
#' \code{custom_ia} uses a lookup data frame to map Left and Right gaze data
#' to newly defined/supplied interest areas for each recording event. The lookup data
#' should contain columns Event, IA_LABEL, IA_ID, Top, Bottom, Left, Right, which 
#' specify the Interest area label, its corresponding ID, and the boundaries (in
#' pixel values) for each recording event. The function will overwrite existing
#' columns RIGHT_INTEREST_AREA_LABEL, RIGHT_INTEREST_AREA_ID, 
#' LEFT_INTEREST_AREA_LABEL, and LEFT_INTEREST_AREA_ID.
#' 
#' @export
#' @import dplyr
#' 
#' @param data A data table object output by \code{\link{prep_data}}.
#' @param iaLookup A data frame object containing by-event mapping information. 
#' @return A data table object.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # Map gaze data to newly defined interest areas...
#' df <- custom_ia(data = dat, iaLookup = LookUpDF)
#'  
#' # For a more complete tutorial on VWPre plotting functions:
#' vignette("SR_Interest_Areas", package="VWPre")
#' }
#' 
custom_ia <- function(data, iaLookup=NULL) {
  
  # Check if PupilPre is installed
  .check_for_PupilPre(type="NotAvailable")
  
  boxhit <- function(Xpos = Xpos, Ypos = Ypos, lookup = lookup, event = event) {
    lookuptmp <- filter(lookup, Event==event)
    lev<-as.vector(unique(levels(lookuptmp$IA_LABEL)))
    Hit <- "Outside"
    if (!(is.na(Xpos)) && !(is.na(Ypos))) {
      for (n in lev) {
        if (Xpos >= lookuptmp[lookuptmp$IA_LABEL==n,]$Left && Xpos <= lookuptmp[lookuptmp$IA_LABEL==n,]$Right && Ypos >= lookuptmp[lookuptmp$IA_LABEL==n,]$Top && Ypos <= lookuptmp[lookuptmp$IA_LABEL==n,]$Bottom) {
          Hit <- n
        }
      }
    }
    return(as.character(Hit))
  }
  
  labelid <- function(Hit = Hit, IAinfo = IAinfo) {
    lev<-as.factor(levels(IAinfo$IA_LABEL))
    Code <- 0
    for (n in 1:length(lev)) {
      if (as.character(Hit) == as.character(IAinfo[n,]$IA_LABEL)) {
        Code <- IAinfo[n,]$IA_ID
      }
    }
    return(as.integer(Code))
  }
  
  if(is.null(iaLookup)){
  stop("Please supply the interest area lookup dataframe!")  
  } else {
  iaLookup <- iaLookup
  }
  
  IAinfo <- data.frame(unique(iaLookup[, c("IA_LABEL", "IA_ID")]))
  
  hitdat <- data %>% group_by(Event) %>% rowwise() %>%
    mutate(HitLabelR = boxhit(
      Xpos = RIGHT_GAZE_X,
      Ypos = RIGHT_GAZE_Y,
      event = Event[1],
      lookup = iaLookup
    )) %>% 
    mutate(HitIDR = labelid(
      Hit = HitLabelR[1],
      IAinfo = IAinfo
    )) %>%
    mutate(HitLabelL = boxhit(
      Xpos = LEFT_GAZE_X,
      Ypos = LEFT_GAZE_Y,
      event = Event[1],
      lookup = iaLookup
    )) %>% 
    mutate(HitIDL = labelid(
      Hit = HitLabelL[1],
      IAinfo = IAinfo
    )) %>% ungroup()
  
  hitdat$HitLabelR <- as.factor(as.character(hitdat$HitLabelR))
  hitdat[hitdat$HitLabelR=="Outside",]$HitLabelR <- NA
  hitdat[hitdat$HitIDR==0,]$HitIDR <- NA
  hitdat$HitLabelL <- as.factor(as.character(hitdat$HitLabelL))
  hitdat[hitdat$HitLabelL=="Outside",]$HitLabelL <- NA
  hitdat[hitdat$HitIDL==0,]$HitIDL <- NA
  hitdat <- droplevels(hitdat)
  
  hitdat <- hitdat %>% 
    select(., -RIGHT_INTEREST_AREA_LABEL, -RIGHT_INTEREST_AREA_ID,
           -LEFT_INTEREST_AREA_LABEL, -LEFT_INTEREST_AREA_ID) %>% 
    rename(., RIGHT_INTEREST_AREA_LABEL = HitLabelR, 
           RIGHT_INTEREST_AREA_ID = HitIDR,
           LEFT_INTEREST_AREA_LABEL = HitLabelL, 
           LEFT_INTEREST_AREA_ID = HitIDL)
  
  return(droplevels(ungroup(hitdat)))
}

