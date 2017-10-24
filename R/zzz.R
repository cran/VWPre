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
  "SAMPLE_MESSAGE", "CalcCol", "Comp", "Condition", "CustCond1", "CustCond2",
  "Custom", "DC1", "DC1m", "DC1sd", "DC2", "DC2m", "DC2sd", "IA_0_P", "IA_1_P", 
  "IA_2_P", "IA_3_P", "IA_4_P", "IA_5_P", "IA_6_P", "IA_7_P", "IA_8_P", 
  "IAmean", "Obs", "degfree", "error_lower", "error_upper", "group", "meanDiff", 
  "n1", "n2", "Avg2", "StDev2"), add = FALSE)
}

.onAttach <- function(...) { 
    name <- utils::packageDescription("VWPre", fields = c("Package", "Version"))[[1]]
	version <- utils::packageDescription("VWPre", fields = c("Package", "Version"))[[2]]
    hello <- paste("This is ", name, " version ",version,". \nFor an overview of the package, type 'help(package=\"VWPre\")'.", "\nIMPORTANT CHANGES - See NEWS.",sep="")
    packageStartupMessage(hello)
}

