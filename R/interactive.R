#' Plots diagnostic plots of the empirical logit transformation.
#' 
#' \code{plot_transformation_app} plots the empirical logit values for a 
#' given number of observations and constant against proportions, in order
#' to examine the effect of these variables on the resulting transformation.
#' 
#' @export
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' 
#' @examples
#' \dontrun{
#' library(VWPre)
#' # For plotting the empirical logit transformation
#' plot_transformation_app() 
#' }
plot_transformation_app <- function ()
{
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Empirical Logit Transformation"),
      shiny::splitLayout(cellWidths = c("50%", "50%"), cellArgs = list(style = "padding: 6px"), shiny::plotOutput("PlotElog"), shiny::plotOutput("PlotWts")),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::numericInput("obs", "Observations:", 0),
          shiny::numericInput("cons", "Constant", 0.5)
        ),
        shiny::column(
          8, shiny::htmlOutput("TextVals")
        )
      ),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(1,
                      shiny::actionButton("quit", "Quit")
        )
      )
    ),
    server = function(input, output) {
      
      dat <- data.frame(Prop=seq(0, 1, .01))
      elogit = function (proportion, observations, constant) 
      {
        return(log((proportion * observations + constant)/((1 - proportion) * observations + constant)))
      }
      weight = function (proportion, observations, constant) 
      {
        return((1/(proportion * observations + constant)) + (1/((1 - proportion) * observations + constant)))
      }
      
      shiny::observe({
        if (input$quit > 0) {
          shiny::stopApp(NULL)
        }
      })
      
      plotdat <- shiny::reactive({
        obs <- input$obs
        cons <- input$cons
        if(input$obs <= 0 | input$cons <= 0) {
        } else {
          dat <- dat %>% mutate(elogit = elogit(Prop, obs, cons),
                                wt = weight(Prop, obs, cons))
          return(dat)
        }
      })
      
      
      output$PlotElog <- shiny::renderPlot({
        plotdat <- plotdat()
        if(input$obs <= 0 | input$cons <= 0) {
        } else {
          ggplot2::ggplot(plotdat, aes(x = Prop, y = elogit)) +
            geom_line() +
            xlab("Proportion") +
            ylab("Empirical Logit") +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              plot.title = element_text(hjust = 0.5, vjust = 1)
            ) +
            ggtitle("Transformation")
        }
      })
      
      output$PlotWts <- shiny::renderPlot({
        plotdat <- plotdat()
        if(input$obs <= 0 | input$cons <= 0) {
        } else {
          ggplot2::ggplot(plotdat, aes(x = Prop, y = wt)) +
            geom_line() +
            xlab("Proportion") +
            ylab("Weight") +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              plot.title = element_text(hjust = 0.5, vjust = 1)
            ) + 
            ggtitle("Variance Estimation")
        }
      })
      
      output$TextVals <- shiny::renderUI({ 
        plotdat <- plotdat()
        if(input$obs <= 0 | input$cons <= 0) {
          shiny::HTML("Both Number of Observations and Constant should be greater than 0")
        } else {
          str1 <- paste("Empirical logit range: ", round(min(plotdat$elogit),4), "to", round(max(plotdat$elogit),4))
          str2 <- paste("Weight range: ", round(min(plotdat$wt),4), "to", round(max(plotdat$wt),4))
          shiny::HTML(paste(str1, str2, sep = "<br/><br/>"))
        }
      })
      
    }
  )
}


#' Plots diagnostic plots of subject/item variance.
#' 
#' \code{plot_var_app} calculates and plots within-subject/item standard deviation,
#' along with standardized by-subject/item means for a given interest area, within
#' a given time window.
#' 
#' @export
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' 
#' @param data A data table object output by either \code{\link{bin_prop}}. 
#' \code{\link{transform_to_elogit}}, or \code{\link{create_binomial}}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # For plotting the grand average with the included theme
#' plot_var_app(data = dat) 
#' }
plot_var_app <- function (data)
{
  dat <- data
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Variability"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("type",
                             "Group:", choices = c("Subjects", "Items")),
          shiny::selectInput(
            "scale",
            "Input:",
            choices = c("Proportions", "Empirical Logits")
          ),
          shiny::conditionalPanel(
            condition = "input.scale == 'Proportions'",
            shiny::selectInput("PCol", "Interest Areas",
                               c("Choose", intersect(
                                 grep("_P", names(data), value = TRUE),
                                 grep("IA_", names(data), value = TRUE)
                               )))
          ),
          shiny::conditionalPanel(
            condition = "input.scale == 'Empirical Logits'",
            shiny::selectInput("ECol", "Interest Areas",
                               c("Choose", intersect(
                                 grep("_E", names(data), value = TRUE),
                                 grep("IA_", names(data), value = TRUE)
                               )))
          ),
          shiny::sliderInput(
            "rng",
            "Time range:",
            value = c(min(data$Time),max(data$Time)),
            min = min(data$Time),
            max = max(data$Time),
            step = data$Time[2]-data$Time[1]
          )
        ),
        shiny::mainPanel(shiny::plotOutput("Plot"))
      ),
      shiny::hr(),
      shiny::actionButton("quit", "Quit")
    ),
    server = function(input, output) {
      
      shiny::observe({
        if (input$quit > 0) {
          shiny::stopApp(NULL)
        }
      })
      
      dat <- shiny::reactive({
        data <- data
        return(data)
      })
      SCALE <-
        shiny::reactive({
          SCALE <- input$scale
          return(SCALE)
        })
      TYPE <-
        shiny::reactive({
          if (input$type=="Subjects") {
            TYPE <- "Subject"
          } else if (input$type=="Items") {
            TYPE <- "Item"
          } 
          return(TYPE)
        })
      Col <- shiny::reactive({
        if (input$scale == "Proportions") {
          col <- input$PCol
        }
        else if (input$scale == "Empirical Logits") {
          col <- input$ECol
        }
        return(col)
      })
      output$Plot <- shiny::renderPlot({
        
        Col <- Col()
        if (Col=="Choose") {
          message("Please select an interest area.")
        } else {
          Col <- enquo(Col)
          scale <- SCALE()
          type <- TYPE()
          type <- enquo(type)
          
          dat1 <- dat() %>% filter(Time >= input$rng[1] & Time <= input$rng[2]) %>%
            rename(Comp = !!type, CalcCol = !!Col) %>% group_by(Comp) 
          
          if (scale == "Empirical Logits") {
            dat1 <- dat1 %>%
              summarise(Avg = mean(CalcCol), StDev = sd(CalcCol)) %>%
              ungroup() %>% mutate(., Zscore = (Avg - mean(Avg)) / sd(Avg))
          } else
            if (scale == "Proportions") {
              dat1 <- dat1 %>%
                summarise(Avg = mean(CalcCol), StDev = sqrt((mean(CalcCol)*(1-mean(CalcCol)))/n())) %>%
                ungroup() %>% mutate(Avg2 = mean(Avg), StDev2 = sqrt((mean(Avg)*(1-mean(Avg)))/nrow(.))) %>%
				mutate(., Zscore = (Avg - Avg2) / StDev2)
            }
          
          ggplot(dat1, aes(Comp, Zscore)) +
            geom_segment(aes(x = Comp, y = 0, xend = Comp, yend = Zscore)) +
            geom_point(aes(size = StDev), shape = 21, fill = "gray", alpha = 0.75) +
            geom_hline(yintercept = 0) +
            geom_hline(yintercept = 2.5, color = "gray",  linetype = 2) +
            geom_hline(yintercept = -2.5, color = "gray",  linetype = 2) +
            labs(y = "Z-score of looks", x = paste(quo_expr(type))) +
            scale_size(name = paste0("Within\n", quo_expr(type), " SD")) +
            theme_bw() + theme(
              panel.grid.major.x = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)
            )
        }
      })
    }
  )
}



#' Plots diagnostic average plots of subjects/items.
#' 
#' \code{plot_indiv_app} calculates and plots interest area averages for a 
#' given subject/item.
#' 
#' @export
#' @import ggplot2
#' @import dplyr
#' @import rlang
#' 
#' @param data A data table object output by either \code{\link{bin_prop}}. 
#' \code{\link{transform_to_elogit}}, or \code{\link{create_binomial}}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # For plotting the grand average with the included theme
#' plot_indiv_app(data = dat)
#' } 
plot_indiv_app <- function (data)
{
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Individual Averages"),
      shiny::plotOutput("Indiv"),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(
          4,
          offset = 0,
          shiny::selectInput(
            "scale",
            "Scale:",
            choices = c("Proportions",
                        "Empirical Logits")
          ),
          shiny::conditionalPanel(
            condition = "input.scale == 'Proportions'",
            shiny::selectizeInput(
              "PCols",
              "Interest Areas",
              intersect(
                grep("_P", names(data), value = TRUE),
                grep("IA_", names(data), value = TRUE)
              ),
              selected = NULL,
              multiple = TRUE,
              options = list(placeholder = "select interest areas")
            )
          ),
          shiny::conditionalPanel(
            condition = "input.scale == 'Empirical Logits'",
            shiny::selectizeInput(
              "ECols",
              "Interest Areas",
              intersect(
                grep("_E", names(data), value = TRUE),
                grep("IA_", names(data), value = TRUE)
              ),
              selected = NULL,
              multiple = TRUE,
              options = list(placeholder = "select interest areas")
            )
          )
        ),
        shiny::column(
          4,
          offset = 0,
          shiny::selectInput("type",
                             "Group:", choices = c("Subjects", "Items")),
          shiny::conditionalPanel(
            condition = "input.type == 'Items'",
            shiny::selectInput("item", "Plot:", choices = unique(levels(data$Item)))
          ),
          shiny::conditionalPanel(
            condition = "input.type == 'Subjects'",
            shiny::selectInput("subj", "Individual:", choices = unique(levels(data$Subject)))
          )
        ),
        shiny::column(
          4,
          offset = 0,
          shiny::sliderInput(
            "rng",
            "Time range:",
            value = c(min(data$Time),max(data$Time)),
            min = min(data$Time),
            max = max(data$Time),
            step = data$Time[2]-data$Time[1]
          ),
          shiny::selectInput("error",
                             "Error:", choices = c("None", "Standard Error", "Pointwise Confidence", "Simultaneous Confidence")),
          shiny::conditionalPanel(
            condition = "input.error == 'Pointwise Confidence' | input.error == 'Simultaneous Confidence'",
            shiny::numericInput("conflev", "Confidence level:", 95, min = 0, max = 100)
          )
        )
      ),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(
          1, offset = 0,
          shiny::actionButton("quit", "Quit")
        )
      )),
    server = function(input,
                      output) {
      
      shiny::observe({
        if(input$quit > 0){
          shiny::stopApp(NULL)
        }
      })
      
      granddata <- shiny::reactive({
        gdata <- data %>% filter(Time >= input$rng[1], Time <= input$rng[2])
        gdata$group <- "Grand Average"
        return(gdata)
      })
      cols <-
        shiny::reactive({
          if (input$scale == "Proportions") {
            cols <- input$PCols
          }
          else if (input$scale == "Empirical Logits") {
            cols <- input$ECols
          }
          return(cols)
        })
      SCALE <-
        shiny::reactive({
          SCALE <- input$scale
          return(SCALE)
        })
      ERROR <-
        shiny::reactive({
          ERROR <- input$error
          return(ERROR)
        })
      CONFLEV <-
        shiny::reactive({
          CONFLEV <- input$conflev
          return(CONFLEV)
        })
      YLIM <-
        shiny::reactive({
          if (input$scale == "Proportions") {
            YLIM <- c(0, 1)
          }
          else if (input$scale == "Empirical Logits") {
            YLIM <- c(-4, 4)
          }
          return(YLIM)
        })
      SN <-
        shiny::reactive({
          SN <- c("Time", "group", cols())
          return(SN)
        })
      INDIV <-
        shiny::reactive({
          if (input$type == "Subjects") {
            INDIV <- "Subject"
          }
          else if (input$type == "Items") {
            INDIV <- "Item"
          }
          return(INDIV)
        })
      indivdata <-
        shiny::reactive({
          if (input$type == "Subjects") {
            # data <- data[data$Subject == input$subj, ]
            idata <- filter(data, Subject == input$subj, Time >= input$rng[1], Time <= input$rng[2])
          }
          else if (input$type == "Items") {
            # data <- data[data$Item == input$item, ]
            idata <- filter(data, Item == input$item, Time >= input$rng[1], Time <= input$rng[2])
          }
          idata$group <- "Individual Average"
          return(idata)
        })
      output$Indiv <-
        shiny::renderPlot({
          
          ylim <- YLIM()
          sel_names <- SN()
          sel_names <- enquo(sel_names)
          Cols <- cols()
          Cols <- enquo(Cols)
          Ind <- INDIV()
          #Ind <- enquo(Ind)
          scale <- SCALE()
          error <- ERROR()
          conflev <- CONFLEV()
          
          if (is.null(quo_expr(Cols))) {
            message("Please select interest areas.")
          }
          else {
            avgdat <- bind_rows(granddata(), indivdata())
            xaxis <- unique(avgdat$Time)
            avgdat$group <- as.factor(avgdat$group)
            
            Avg <- avgdat %>% select(UQ(sym(Ind)), !!!sel_names) %>%
              tidyr::gather(key=IA, value = VALUE, !!!Cols, na.rm = FALSE, convert = FALSE) %>%
              group_by(UQ(sym(Ind)), IA, Time, group) %>% 
              summarise(VALUE = mean(VALUE, na.rm = T)) %>% 
              group_by(IA, Time, group)
            
            AvgG <- filter(Avg, group == "Grand Average")
            AvgI <- filter(Avg, group == "Individual Average")
            
            if(scale=="Empirical Logits") {
              AvgG <- AvgG %>% summarise(mean = mean(VALUE, na.rm = T), n=n(), se = sd(VALUE) / sqrt(n()))
              AvgI <- AvgI %>% summarise(mean = mean(VALUE, na.rm = T), n=n(), se = 0)
              if(error=="Pointwise Confidence") {
                t <- 1-(((100-input$conflev)/2)/100)
              } else if (error=="Simultaneous Confidence") {
                t <- 1-(((100-input$conflev)/(2*length(unique(xaxis))))/100)
              }
              if (error=="None" || error=="Standard Error") {
                AvgG <- AvgG
                AvgI <- AvgI
              } else {
                AvgG <- AvgG %>% mutate(ci = stats::qt(t,df=n-1)*se)
                AvgI <- AvgI %>% mutate(ci = 0)
              }
            } else if (scale=="Proportions") {
              AvgG <- AvgG %>% summarise(mean = mean(VALUE, na.rm = T), n=n(), se = sqrt((mean(VALUE)*(1-mean(VALUE)))/n()))
              AvgI <- AvgI %>% summarise(mean = mean(VALUE, na.rm = T), n=n(), se = 0)
              if(error=="Pointwise Confidence") {
                z <- 1-(((100-conflev)/2)/100)
              } else if (error=="Simultaneous Confidence") {
                z <- 1-(((100-conflev)/(2*length(unique(xaxis))))/100)
              }
              if (error=="None" || error=="Standard Error") {
                AvgG <- AvgG
                AvgI <- AvgI
              } else {
                AvgG <- AvgG %>% mutate(ci = stats::qnorm(z)*se)
                AvgI <- AvgI %>% mutate(ci = 0)
              }
            }
            Avg <- bind_rows(AvgG, AvgI) %>% ungroup()
            
            # Setting Error
            if(error!="None") {
              if(error=="Standard Error"){
                Avg$error_lower <- Avg$mean - Avg$se
                Avg$error_upper <- Avg$mean + Avg$se
              } else if(error=="Pointwise Confidence" || error=="Simultaneous Confidence") {
                Avg$error_lower <- Avg$mean - Avg$ci
                Avg$error_upper <- Avg$mean + Avg$ci
              } 
              if(scale=="Proportions") {
                Avg$error_lower <- ifelse(Avg$error_lower < 0, 0, Avg$error_lower)
                Avg$error_upper <- ifelse(Avg$error_upper > 1, 1, Avg$error_upper)
              }
            } else {
              Avg$error_lower <- Avg$mean
              Avg$error_upper <- Avg$mean
            }
            
            # Setting ylim
            if (scale == "Empirical Logits") {
              ylim[1] = min(Avg$error_lower)
              ylim[2] = max(Avg$error_upper)
            } else if (scale == "Proportions") {
              ylim[1] = min(Avg$error_lower)
              ylim[2] = max(Avg$error_upper)
              if (ylim[1] > 0) {
                ylim[1] = 0
              }
              if (ylim[2] < 1) {
                ylim[2] = 1
              }
            }
            
            plt <- ggplot(Avg, aes(x = Time, y = mean, colour = IA)) +
              geom_point(alpha = 0.7) + geom_line(alpha = 0.7)
            if(error!="None") {
              plt <- plt + geom_errorbar(aes(ymin = error_lower, ymax = error_upper), width = 0.3, alpha = 0.7)
            }
            plt + facet_grid(. ~ group) + ylab(scale) +
              scale_y_continuous(limits = c(ylim[1], ylim[2])) + scale_colour_brewer(palette = "Set1") +
              theme_bw() + theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank()
              )
          }
        })
    }
  )
}
