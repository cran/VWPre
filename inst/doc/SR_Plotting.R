## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=4, warning=FALSE)

## ---- eval = TRUE, echo=FALSE, results='hide', message=FALSE-------------
library(VWPre)
data(VWdat)
dat <- fasttrack(data = VWdat, Subject = "RECORDING_SESSION_LABEL", Item = "itemid", 
	EventColumns = c("Subject", "TRIAL_INDEX"), NoIA = 4, Adj = -100, Recording = "LandR", 
  WhenLandR = "Right", BinSize = 20, SamplingRate = 1000,
  ObsPerBin = 20, Constant = 0.5, Output = "ELogit")

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
plot_avg(data = dat, type = "proportion", xlim = c(0, 1000), 
    IAColumns = c(IA_1_P = "Target", IA_2_P = "Rhyme", IA_3_P = "OnsetComp", 
                  IA_4_P = "Distractor"),
    Condition1 = NA, Condition2 = NA, Cond1Labels = NA, Cond2Labels = NA,
    ErrorBar = TRUE, VWPreTheme = TRUE) 

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
plot_avg(data = dat, type = "proportion", xlim = c(0, 1000), 
    IAColumns = c(IA_1_P = "Target", IA_2_P = "Rhyme", IA_3_P = "OnsetComp", 
                  IA_4_P = "Distractor"),
    Condition1 = NA, Condition2 = NA, Cond1Labels = NA, Cond2Labels = NA,
    ErrorBar = TRUE, VWPreTheme = TRUE) + ggtitle("Grand Average Plot")

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
plot_avg(data = dat, type = "proportion", xlim = c(0, 1000), 
    IAColumns = c(IA_1_P = "Target", IA_2_P = "Rhyme", IA_3_P = "OnsetComp", 
                  IA_4_P = "Distractor"),
    Condition1 = NA, Condition2 = NA, Cond1Labels = NA, Cond2Labels = NA,
    ErrorBar = TRUE, VWPreTheme = FALSE) + theme(axis.text = element_text(size = 15))

## ---- eval= TRUE, fig.show='hold', fig.height=5, results='asis', message=FALSE----
plot_avg(data = dat, type = "proportion", xlim = c(0, 1000), 
    IAColumns = c(IA_1_P = "Target", IA_2_P = "Rhyme", IA_3_P = "OnsetComp", 
                  IA_4_P = "Distractor"), Condition1 = "talker", 
    Condition2 = NA, Cond1Labels = c(CH1 = "Chinese 1", CH10 = "Chinese 3", 
                                     CH9 = "Chinese 2", EN3 = "English 1"),
    Cond2Labels = NA, ErrorBar = TRUE, VWPreTheme = TRUE)

## ---- eval= TRUE, fig.show='hold', fig.width=7, fig.height=3.5, results='asis', message=FALSE----
plot_avg(data = dat, type = "proportion", xlim = c(0, 1000), 
    IAColumns = c(IA_1_P = "Target", IA_2_P = "Rhyme", IA_3_P = "OnsetComp", 
                  IA_4_P = "Distractor"), Condition1 = NA, 
    Condition2 = "talker", Cond1Labels = NA, Cond2Labels = c(CH1 = "Chinese 1", 
                                                             CH10 = "Chinese 3", 
                                                             CH9 = "Chinese 2", 
                                                             EN3 = "English 1"), 
    ErrorBar = TRUE, VWPreTheme = TRUE)

## ---- eval= TRUE, fig.show='hold', fig.width=7, fig.height=5, results='asis', message=FALSE----
plot_avg(data = dat, type = "proportion", xlim = c(0, 1000), 
    IAColumns = c(IA_1_P = "Target", IA_2_P = "Rhyme", IA_3_P = "OnsetComp", 
                  IA_4_P = "Distractor"), Condition1 = "talker", 
    Condition2 = "Exp", Cond1Labels = c(CH1 = "Chinese 1", CH10 = "Chinese 3", 
                                     CH9 = "Chinese 2", EN3 = "English 1"),
    Cond2Labels = c(High = "High Exp", Low = "Low Exp"), ErrorBar = TRUE, 
    VWPreTheme = TRUE)

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
plot_avg_diff(data = dat, xlim = c(0, 1000), DiffCols = c(IA_1_P = "Target", IA_2_P = "Rhyme"), 
            Condition1 = NA, Condition2 = NA, Cond1Labels = NA,
            Cond2Labels = NA, ErrorBar = TRUE, VWPreTheme = TRUE)

## ---- eval= TRUE, fig.show='hold', fig.height=5, results='asis', message=FALSE----
plot_avg_diff(data = dat, xlim = c(0, 1000), DiffCols = c(IA_1_P = "Target", IA_2_P = "Rhyme"), 
            Condition1 = "talker", Condition2 = NA, Cond1Labels = c(CH1 = "Chinese 1", 
            CH10 = "Chinese 3", CH9 = "Chinese 2", EN3 = "English 1"),
            Cond2Labels = NA, ErrorBar = TRUE, VWPreTheme = TRUE)

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
plot_avg_contour(data = dat, IA = "IA_1_P", type = "proportion", Var = "Rating", 
VarLabel = "Accent Rating", xlim = c(0,1000), Theme = FALSE, 
Color = c("gray20", "gray90"))

## ---- eval= TRUE, fig.show='hold', results='asis', message=FALSE---------
plot_avg_contour(data = dat, IA = "IA_1_P", type = "proportion", Var = "Rating", 
VarLabel = "Accent Rating", xlim = c(0,1000), Theme = FALSE, 
Color = c("red", "green")) + ggtitle("Looks to target")

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  plot_transformation_app()

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  plot_var_app(dat)

## ---- eval=FALSE, echo=TRUE, results='asis'------------------------------
#  plot_indiv_app(dat)

