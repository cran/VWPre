VWPreEnv <- new.env()

.onLoad <- function(...) {
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c(".", "%>%", "Time", "Event", 
  "IA_0_C", "IA_1_C", "IA_2_C", "IA_3_C", "IA_4_C", 
  "IA_5_C", "IA_6_C", "IA_7_C", "IA_8_C", "Prop", "wt",
  "DS", "NSamples", "starts_with", "se", "Cond", "Cond1", "Cond2", 
  "Diff", "IA_ID", "TIMESTAMP", "VALUE", "IA", "Item", "Subject", "Avg", 
  "Zscore", "StDev", "EyeSelected", "EyeRecorded", "LEFT_INTEREST_AREA_LABEL", 
  "LEFT_INTEREST_AREA_ID", "RIGHT_INTEREST_AREA_LABEL", 
  "RIGHT_INTEREST_AREA_ID", "Variable", "binomial", "coef", "plogis",
  "predict", "sd", "setNames", "capture.output",
  "Align", "HitIDL", "HitIDR", "HitLabelL", "HitLabelR", "LEFT_GAZE_X",
  "LEFT_GAZE_Y", "Present", "RIGHT_GAZE_X", "RIGHT_GAZE_Y", 
  "SAMPLE_MESSAGE"), add = FALSE)
}

.onAttach <- function(...) { 
    name <- utils::packageDescription("VWPre", fields = c("Package", "Version"))[[1]]
	version <- utils::packageDescription("VWPre", fields = c("Package", "Version"))[[2]]
    hello <- paste("This is ", name, " version ",version,". \nFor an overview of the package, type 'help(package=\"VWPre\")'.",sep="")
    packageStartupMessage(hello)
}

