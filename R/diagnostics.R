#' Plots diagnostic plots of the empirical logit transformation.
#' 
#' \code{plot_transformation_app} plots the empirical logit values for a 
#' given number of observations and constant against proportions.
#' 
#' @export
#' @import dplyr
#' @import lazyeval
#' @import ggplot2
#' @import shiny
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
          shiny::numericInput("cons", "Constant", 0.5),
          shiny::actionButton("quit", "Quit")
        ),
        shiny::column(
          8, shiny::htmlOutput("TextVals")
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
          stopApp(NULL)
        }
      })
      
      plotdat <- shiny::reactive({
        obs <- input$obs
        cons <- input$cons
        if(input$obs <= 0 | input$cons <= 0) {
        } else {
        dat <- dat %>% mutate_(elogit = interp(~ elogit(Prop, obs, cons)),
                               wt = interp(~ weight(Prop, obs, cons)))
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
            panel.grid.minor.y = element_blank()
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
            panel.grid.minor.y = element_blank()
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
#' @import dplyr
#' @import lazyeval
#' @import ggplot2
#' @import shiny
#' 
#' @param data A data table object output by either \code{\link{bin_prop}}. 
#' \code{\link{transform_to_elogit}}, or \code{\link{create_binomial}}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # For plotting the grand average with the included theme
#' plot_var_app(data = dat) 
#' }
plot_var_app <- function (data = data)
{
  dat <- data
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("Variability"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("type",
                             "Group:", choices = c("Subjects", "Item")),
          shiny::selectInput(
            "scale",
            "Input:",
            choices = c("Proportions", "Empirical Logits")
          ),
          shiny::conditionalPanel(
            condition = "input.scale == 'Proportions'",
            shiny::selectInput("PCol", "Interest Areas",
                               c(intersect(
                                 grep("_P", names(data), value = TRUE),
                                 grep("IA_", names(data), value = TRUE)
                               ),
                               "Select"), selected = "Select")
          ),
          shiny::conditionalPanel(
            condition = "input.scale == 'Empirical Logits'",
            shiny::selectInput("ECol", "Interest Areas",
                               c(intersect(
                                 grep("_E", names(data), value = TRUE),
                                 grep("IA_", names(data), value = TRUE)
                               ),
                               "Select"), selected = "Select")
          ),
          shiny::sliderInput(
            "mintime",
            "Min Time:",
            value = min(dat$Time),
            min = min(dat$Time),
            max = max(dat$Time)
          ),
          shiny::sliderInput(
            "maxtime",
            "Max Time:",
            value = max(dat$Time),
            min = min(dat$Time),
            max = max(dat$Time)
          )
        ),
        shiny::mainPanel(shiny::plotOutput("Plot"))
      ),
      shiny::actionButton("quit", "Quit")
    ),
    server = function(input, output) {
      
      shiny::observe({
        if (input$quit > 0) {
          stopApp(NULL)
        }
      })
      
      dat <- shiny::reactive({
        data <- data
        return(data)
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
        if (Col == "Select") {
          message("Please select an interest area.")
        }
        else {
          if (input$type == "Item") {
            dat1 <- dat() %>% filter(Time >= input$mintime & Time <= input$maxtime) %>% group_by(Item) %>%
              summarise_(Avg = interp( ~ mean(Col), Col = as.name(Col)), StDev = interp( ~ sd(Col), Col = as.name(Col))) %>%
              ungroup() %>% mutate(., Zscore = (Avg - mean(Avg)) / sd(Avg))
            ggplot(dat1, aes(Item, Zscore)) + 
              geom_segment(aes(x = Item, y = 0, xend = Item, yend = Zscore)) + 
              geom_point(aes(size = StDev), shape = 21, fill = "gray", alpha = 0.75) +
              geom_hline(yintercept = 0) + 
              geom_hline(yintercept = 2.5, color = "gray",  linetype = 2) + 
              geom_hline(yintercept = -2.5, color = "gray",  linetype = 2) + 
              labs(y = "Z-score of looks") +
              scale_size(name = "Within\nItem SD") +
              theme_bw() + theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8)
              )
          }
          else {
            dat1 <- dat() %>% filter(Time >= input$mintime & Time <= input$maxtime) %>% 
              group_by(Subject) %>% 
              summarise_(Avg = interp( ~ mean(Col), Col = as.name(Col)), StDev = interp( ~ sd(Col), Col = as.name(Col))) %>%
              ungroup() %>% mutate(., Zscore = (Avg - mean(Avg)) / sd(Avg))
            ggplot(dat1, aes(Subject, Zscore)) + 
              geom_segment(aes(x = Subject, y = 0, xend = Subject, yend = Zscore)) +
              geom_point(aes(size = StDev), shape = 21, fill = "gray", alpha = 0.75) + 
              geom_hline(yintercept = 0) +
              geom_hline(yintercept = 2.5, color = "gray", linetype = 2) + 
              geom_hline(yintercept = -2.5, color = "gray", linetype = 2) + 
              labs(y = "Z-score of looks") +
              scale_size(name = "Within\nSubject SD") +
              theme_bw() + theme(
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text.x = element_text(angle = 90, vjust = 0.5,size = 8)
              )
          }
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
#' @import dplyr
#' @import tidyr
#' @import lazyeval
#' @import ggplot2
#' @import shiny
#' 
#' @param data A data table object output by either \code{\link{bin_prop}}. 
#' \code{\link{transform_to_elogit}}, or \code{\link{create_binomial}}.
#' @examples
#' \dontrun{
#' library(VWPre)
#' # For plotting the grand average with the included theme
#' plot_indiv_app(data = dat)
#' } 
plot_indiv_app <- function (data = data)
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
            ),
            shiny::actionButton("quit", "Quit")
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
            ),
            shiny::actionButton("quit", "Quit")
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
            "mintime",
            "Min Time:",
            value = min(data$Time),
            min = min(data$Time),
            max = max(data$Time)
          ),
          shiny::sliderInput(
            "maxtime",
            "Max Time:",
            value = max(data$Time),
            min = min(data$Time),
            max = max(data$Time)
          )
        )
      )),
    server = function(input,
                      output) {
      
      shiny::observe({
        if(input$quit > 0){
          stopApp(NULL)
        }
      })
      
      granddata <- shiny::reactive({
        data <- data
        return(data)
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
          SN <- c("Time", cols())
          return(SN)
        })
      INDIV <-
        shiny::reactive({
          if (input$type == "Subjects") {
            INDIV <- input$subj
          }
          else if (input$type == "Items") {
            INDIV <- input$item
          }
          return(INDIV)
        })
      indivdata <-
        shiny::reactive({
          if (input$type == "Subjects") {
            data <- data[data$Subject == input$subj, ]
          }
          else if (input$type == "Items") {
            data <- data[data$Item == input$item, ]
          }
          return(data)
        })
      output$Indiv <-
        shiny::renderPlot({
          ylim <- YLIM()
          sel_names <-
            SN()
          Cols <-
            cols()
          Ind <-
            INDIV()
          scale <-
            SCALE()
          if (is.null(Cols)) {
            message("Please select interest areas.")
          }
          else {
            GrandAvg <- granddata() %>% select(match(sel_names, names(.))) %>% 
              tidyr::gather_("IA", "VALUE", unique(Cols), na.rm = FALSE, convert = FALSE) %>%
              group_by_("IA", "Time") %>% 
              summarise(mean = mean(VALUE, na.rm = T), se = sd(VALUE) / sqrt(length(VALUE))) %>%
              mutate(alpha = 0.25, group = "Grand Average")
            GrandAvg$group <-
              as.factor(GrandAvg$group)
            IndivAvg <-
              indivdata() %>% select(match(sel_names, names(.))) %>% 
              tidyr::gather_("IA", "VALUE", unique(Cols), na.rm = FALSE, convert = FALSE) %>%
              group_by_("IA", "Time") %>% 
              summarise(mean = mean(VALUE, na.rm = T), se = sd(VALUE) / sqrt(length(VALUE))) %>%
              mutate(alpha = 1, group = "Individual Average")
            IndivAvg$group <- as.factor(IndivAvg$group)
            Avg <- rbind(IndivAvg, GrandAvg)
            if (input$scale == "Empirical Logits") {
              ylim[1] <- min(Avg$mean) - 0.25
              ylim[2] <-
                max(Avg$mean) + 0.25
            }
            else {
              ylim <- c(0, 1)
            }
            ggplot(Avg, aes(x = Time, y = mean, colour = IA)) +
              geom_point(alpha = 0.7) + geom_line(alpha = 0.7) +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.3, alpha = 0.7) + 
              facet_grid(. ~ group) + ylab(scale) + scale_x_continuous(limits = c(input$mintime, input$maxtime)) + 
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