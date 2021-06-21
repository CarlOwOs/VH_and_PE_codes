library(shiny)
library(shinyWidgets)
library(DT)
library(shinyjs)
library(quantmod)
library(sqldf)
library(plyr)
library(hrbrthemes)
library(ggplot2)
library(dplyr)
library(highcharter) 
require(lubridate)
library(collections)
library(tidyr)
library(base)
library(png)
library(shinyanimate)
library(xts)
library(forecast)
library(openxlsx)
library(shinyBS)
library(ggalluvial)
library(ggiraph)
library(eq5d)
library(ggiraphExtra)
library(shinydashboard)
library(tsoutliers)
library(shinycssloaders)
library(shinycustomloader)
library(magrittr)
options(device.ask.default = FALSE)

source("./Back_end/Alarms/alarm_tracking.R")

ind_freq <- read.csv("./Back_end/Alarms/alarms_CSVs/ICTUS_PATIENTFREQCARDIO_202104231033.csv")
ind_tox <- read.csv("./Back_end/Alarms/alarms_CSVs/ICTUS_PATIENTOXIMETER_202104231033.csv")
ind_wei <- read.csv("./Back_end/Alarms/alarms_CSVs/ICTUS_PATIENTWEIGHT_202104231033.csv")
ind_pres <- read.csv("./Back_end/Alarms/alarms_CSVs/ICTUS_PATIENTPRESSURE_202104231033.csv")
  
my_autocomplete_list <- c(ind_freq[,1],ind_freq[,1],ind_wei[,1],ind_pres[,1])

print_table <- function(a, b) {
    spaces = "    "
    if (is.na(a) | is.na(b)) paste(spaces,"Try to insert sensitivity and specificity to see the table")
    else NULL
}

nhc_spl1 <- readRDS('./Back_end/cardIA/final_predictions/nhc_spl1.csv')
nhc_spl2 <- readRDS('./Back_end/cardIA/final_predictions/nhc_spl2.csv')

nhc_list <- append(nhc_spl1,nhc_spl2)

total_patients = length(nhc_list)

# nhc not empty
nhc_ne <- function(nhc) {
    if (nhc == "") print("")
    else NULL
}

# num_alarm not empty
num_alarm_ne <- function(n) {
    if (is.na(n)) print("Please introduce the numeric input")
    else NULL
}

# Alarm data not empty
alarm_ne <- function(val){
    if (is.null(val)) HTML("Not enough values to perform the series analysis")
    else NULL
}

# Define UI for application that draws a histogram
ui <- tagList(
    useShinyjs(),
    tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(title = list("Graphics", icon = icon("briefcase-medical", lib = "font-awesome")),
               
    tabPanel("Cardia",
             icon = icon("heartbeat", lib = "font-awesome"),
    fluidPage(
    fluidRow(
    column(12, align = "right",
    dropdown(
    fluidRow(
    column(2,
    div(style="display: inline-block; width: 100px;",
        numericInput("sensitivity_",  label = h6("Sensitivity"), value = 60, min = 0, max = 100, step = 1))),
    column(2,
    div(style="display: inline-block; width: 100px;",
        numericInput("specificity_",  label = h6("Specificity"),value = 40, min = 0, max = 100, step = 1)))),

    fluidRow(
        column(3,
        sidebarLayout(
        sidebarPanel(
            h3("0-1 months", style = "color: #ffba6a;"),
            width = 12,
        
            chooseSliderSkin("Modern"),
            setSliderColor(rep("#ffba6a", 16), c(1:16)),
            div(style="height: 78px;",
            sliderInput("n_SVM_target_1",
                        "Number of SVM:",
                        min = 0,
                        max = 7,
                        step = 1,
                        value = 1)),
            div(style="height: 78px;",
            sliderInput("n_RDA_target_1",
                        "Number of RDA:",
                        min = 0,
                        max = 3,
                        step = 1,
                        value = 0)),
            div(style="height: 78px;",
            sliderInput("n_RF_target_1",
                        "Number of RF:",
                        min = 0,
                        max = 2,
                        step = 1,
                        value = 1)),
            div(style="height: 78px;",
            sliderInput("threshold_target_1",
                        "Threshold:",
                        min = 0,
                        max = 13,
                        step = 1,
                        value = 2,
                        tick = F)),
        ),
        mainPanel( width = 0))),
        column(3,
        sidebarLayout(
        sidebarPanel(
           h3("1-3 months", style = "color: #ffba6a;"),
           width = 12,

           chooseSliderSkin("Modern"),
           div(style="height: 78px;",
           sliderInput("n_SVM_target_6",
                       "Number of SVM:",
                       min = 0,
                       max = 7,
                       step = 1,
                       value = 1)),
           div(style="height: 78px;",
           sliderInput("n_RDA_target_6",
                       "Number of RDA:",
                       min = 0,
                       max = 3,
                       step = 1,
                       value = 0)),
           div(style="height: 78px;",
           sliderInput("n_RF_target_6",
                       "Number of RF:",
                       min = 0,
                       max = 2,
                       step = 1,
                       value = 1)),
           div(style="height: 78px;",
           sliderInput("threshold_target_6",
                       "Threshold:",
                       min = 0,
                       max = 13,
                       step = 1,
                       value = 4,
                       ticks = F)),
       ),
       mainPanel( width = 0))),
       column(3,
       sidebarLayout(
       sidebarPanel(
          h3("3-6 months", style = "color: #ffba6a;"),
          width = 12,
          
          chooseSliderSkin("Modern"),
          div(style="height: 78px;",
              sliderInput("n_SVM_target_12",
                          "Number of SVM:",
                          min = 0,
                          max = 6,
                          step = 1,
                          value = 1)),
          div(style="height: 78px;",
              sliderInput("n_RDA_target_12",
                          "Number of RDA:",
                          min = 0,
                          max = 1,
                          step = 1,
                          value = 1)),
          div(style="height: 78px;",
              sliderInput("n_RF_target_12",
                          "Number of RF:",
                          min = 0,
                          max = 2,
                          step = 1,
                          value = 1)),
          div(style="height: 78px;",
              sliderInput("threshold_target_12",
                          "Threshold:",
                          min = 0,
                          max = 13,
                          step = 1,
                          value = 3,
                          tick = F)),
          ),
          mainPanel( width = 0))),
       column(3,
       sidebarLayout(
       sidebarPanel(
          h3("6-12 months", style = "color: #ffba6a;"),
          width = 12,
          
          chooseSliderSkin("Modern"),
          div(style="height: 78px;",
              sliderInput("n_SVM_target_future",
                          "Number of SVM:",
                          min = 0,
                          max = 7,
                          step = 1,
                          value = 2)),
          div(style="height: 78px;",
              sliderInput("n_RDA_target_future",
                          "Number of RDA:",
                          min = 0,
                          max = 4,
                          step = 1,
                          value = 0)),
          div(style="height: 78px;",
              sliderInput("n_RF_target_future",
                          "Number of RF:",
                          min = 0,
                          max = 2,
                          step = 1,
                          value = 2)),
          div(style="height: 78px;",
              sliderInput("threshold_target_future",
                          "Threshold:",
                          min = 0,
                          max = 13,
                          step = 1,
                          value = 3,
                          tick = F)),
          ),
          mainPanel( width = 0)))),
        
        fluidRow(
        column(12,
        sidebarLayout(
        sidebarPanel(
            width = 12,
            radioGroupButtons(
                inputId = "table_target",
                label = h4("Different configurations (per target)"),
                choices = c("0-1 months", "1-3 months", "3-6 months", "6-12 months"),
                checkIcon = list(
                    yes = icon("ok", lib = "glyphicon"))), align = 'center'),
        mainPanel( width = 0)))),
    
        fluidRow(dataTableOutput("table")),
    
        HTML("<br><br><br><br>"),
        circle = TRUE, status = "warning", icon = icon("gear"), right = TRUE, width = NULL,
        tooltip = tooltipOptions(title = "Click to see configuration !",placement = "left")))),
    
    fluidRow(
        column(12,
               sidebarLayout(
                   sidebarPanel(
                       width = 12,
                       prettyRadioButtons(
                           inputId = "nhc_all",
                           label = "", 
                           choices = c("ALL","Patient ID"),
                           #icon = icon("check"), 
                           icon = icon("arrow-alt-circle-right"),
                           bigger = TRUE,
                           status = "warning",
                           animation = "jelly"
                       ),
                       conditionalPanel(
                           condition = "input.nhc_all == 'Patient ID'",
                           selectizeInput(
                               inputId = 'nhc_predict',
                               #label = h4('NHC'),
                               label = "",
                               choices = nhc_list,
                               selected = NULL,
                               multiple = FALSE, # allow for multiple inputs
                               options = list(create = FALSE, placeholder = 'Please introduce a Patient ID.')))), # if TRUE, allows newly created inputs
                   mainPanel(width = 0)))),

        # Show a plot of the generated distribution
        fluidRow(
        column(3,
        h4("0-1 months", align = "center"),
        uiOutput("res_target1"),
        dataTableOutput('res_target1_all')),
        column(3,
        h4("1-3 months", align = "center"),
        uiOutput("res_target6"),
        dataTableOutput('res_target6_all')),
        column(3,
        h4("3-6 months", align = "center"),
        uiOutput("res_target12"),
        dataTableOutput('res_target12_all')),
        column(3,
        h4("6-12 months", align = "center"),
        uiOutput("res_targetfuture"),
        dataTableOutput('res_targetfuture_all')),
        ),
  
        fluidRow(
          mainPanel(
            fluidRow(column(width = 2,div(style = "height:53.5px"))),
            fluidRow(
              column(11, align = "right",
                     prettyToggle(
                       inputId = "pat.risk",
                       label_on = "Show High Risk Patients",
                       icon_on = icon("check"),
                       status_on = "success",
                       status_off = "danger",
                       label_off = "Show High Risk Patients",
                       icon_off = icon("remove"),
                       value = T
            ))),
            highchartOutput('pat_dist'),
            actionButton("help","", icon = htmltools::browsable(tags$i(class = "far fa-question-circle")), 
                         style="color: #fff; background-color: #ffba6a; border-color: #ffba6a"),
            width = 12,
            align = "right"
          )
        )
    )),
    tabPanel("Alarms",
             icon = icon("bell", lib = "font-awesome"),
             fluidPage(
                 fluidRow (
                     sidebarLayout(
                         sidebarPanel(
                             selectizeInput(
                                 inputId = 'nhc_alarm',
                                 label = h4('Patient ID'),
                                 choices = my_autocomplete_list,
                                 selected = NULL,
                                 multiple = FALSE, # allow for multiple inputs
                                 options = list(create = FALSE, placeholder = 'Please introduce a Patient ID.') # if TRUE, allows newly created inputs
                             ),
                             div(style="display: inline-block; width: 100px;",
                                 numericInput("num_alarm.t",  label = h6("Oximeter"), value = 60)),
                             div(style="display: inline-block; width: 100px;",
                                 numericInput("num_alarm.f",  label = h6("Freq. card"), value = 60)),
                             div(style="display: inline-block; width: 100px;",
                                 numericInput("num_alarm.w",  label = h6("Weight"), value = 60)),
                             div(style="display: inline-block; width: 100px;",
                                 numericInput("num_alarm.p",  label = h6("Pressure"), value = 60)),
                             actionButton("num_alarm_ready", "Done", icon = htmltools::browsable(tags$i(class = "far fa-check-circle")), 
                                          style="color: #fff; background-color: #ffba6a; border-color: #ffba6a"),
                             
                             h4("Type of variation"),
                             prettyCheckbox(
                                 inputId = "AO",
                                 label = "Sudden", 
                                 icon = icon("circle", style = "color: #ffba6a; -webkit-text-stroke-width: 0.5px;
                      -webkit-text-stroke-color: black;"), 
                                 plain = TRUE,
                             ),
                             prettyCheckbox(
                                 inputId = "TC",
                                 label = "Temporal", 
                                 icon = icon("play", style = "transform: rotate(270deg); color: #ffba6a; -webkit-text-stroke-width: 0.5px;
                      -webkit-text-stroke-color: black;", lib = "font-awesome"), 
                                 plain = TRUE,
                             ),
                             prettyCheckbox(
                                 inputId = "LS",
                                 label = "Maintained", 
                                 icon = icon("square", style = "transform: rotate(45deg); color: #ffba6a; -webkit-text-stroke-width: 0.5px;
                      -webkit-text-stroke-color: black;"), 
                                 plain = TRUE,
                             )),
                         
                         mainPanel(
                             tags$head(
                                 tags$style(type = "text/css", "a{color: #ffba6a;}")
                             ),
                             tabsetPanel(id="tabsalarms", type = "tabs",
                                         tabPanel("TOTAL",
                                                  withLoader(verbatimTextOutput("text_alarm"),type = "image", loader = "loading.gif")
                                         ),
                                         tabPanel("Oximeter",
                                                  plotOutput("plot_alarm.t") %>% withSpinner(image="loading.gif", image.width = 400, image.height = 150),
                                                  hr(),
                                                  fluidRow(
                                                      useShinyjs(),
                                                      withAnim(),
                                                      uiOutput("value_alarm.t"),
                                                  )
                                         ),
                                         tabPanel("Freq. card",
                                                  plotOutput("plot_alarm.f") %>% withSpinner(image="loading.gif", image.width = 400, image.height = 150),
                                                  hr(),
                                                  fluidRow(
                                                      useShinyjs(),
                                                      withAnim(),
                                                      uiOutput("value_alarm.f"),
                                                  )
                                         ),
                                         tabPanel("Weight",
                                                  plotOutput("plot_alarm.w")  %>% withSpinner(image="loading.gif", image.width = 400, image.height = 150),
                                                  hr(),
                                                  fluidRow(
                                                      useShinyjs(),
                                                      withAnim(),
                                                      uiOutput("value_alarm.w"),
                                                  )
                                         ),
                                         tabPanel("Pressure",
                                                  plotOutput("plot_alarm.p") %>% withSpinner(image="loading.gif", image.width = 400, image.height = 150),
                                                  hr(),
                                                  fluidRow(
                                                      useShinyjs(),
                                                      withAnim(),
                                                      uiOutput("value_alarm.p"),
                                                  )
                                         )
                             )))))),
    tags$head(
        tags$style(type = 'text/css', 
                   HTML('.navbar { #background-color: blue;}
                          .navbar-default .navbar-brand{color: orange;}
                          .tab-panel{ background-color: red; color: red}
                          .navbar-default .navbar-nav > .active > a, 
                           .navbar-default .navbar-nav > .active > a:focus, 
                           .navbar-default .navbar-nav > .active > a:hover {
                                color: white;
                                background-color: #ffba6a;
                            }')
        )
    )

))

rank_averaging <- function(rda_res, svm_res, rf_res, number_RDA, number_SVM, number_RF) {
    RDA_scores <- rda_res$Scores[,1:max(1, number_RDA)]
    RDA_probabilities <- rda_res$Probabilities[,1:max(1, number_RDA)]
    
    KSVM_scores <- svm_res$Scores[,1:max(1, number_SVM)]
    KSVM_probabilities <- svm_res$Probabilities[,1:max(1, number_SVM)]
    
    RF_scores <- rf_res$Scores[,1:max(1, number_RF)]
    RF_probabilities <- rf_res$Probabilities[,1:max(1, number_RF)]
    
    if (number_SVM == 1 & number_RDA == 0 & number_RF == 0) {
        total_probabilities <- cbind(KSVM_probabilities)
        total_scores <- c(KSVM_scores)
    } else if (number_SVM == 0 & number_RDA == 1 & number_RF == 0) {
        total_probabilities <- cbind(RDA_probabilities)
        total_scores <- c(RDA_scores)
    } else if (number_SVM == 0 & number_RDA == 0 & number_RF == 1) {
        total_probabilities <- cbind(RF_probabilities)
        total_scores <- c(RF_scores)
    } else if (number_SVM == 1 & number_RDA == 1 & number_RF == 0) {
        total_probabilities <- cbind(RDA_probabilities, KSVM_probabilities)
        total_scores <- c(RDA_scores, KSVM_scores)
    } else if (number_SVM == 0 & number_RDA == 1 & number_RF == 1) {
        total_probabilities <- cbind(RDA_probabilities, RF_probabilities)
        total_scores <- c(RDA_scores, RF_scores)
    } else if (number_SVM == 1 & number_RDA == 0 & number_RF == 1) {
        total_probabilities <- cbind(KSVM_probabilities, RF_probabilities)
        total_scores <- c(KSVM_scores, RF_scores)
    } else {
        total_probabilities <- cbind(RDA_probabilities, KSVM_probabilities, RF_probabilities)
        total_scores <- c(RDA_scores, KSVM_scores, RF_scores)
    }
    
    order_scores <- order(total_scores, decreasing = TRUE)
    
    # Extracting rank positions
    result <- c()
    j <- 1
    for (i in order_scores) {
        result[i] <- j
        j <- j+1
    }
    
    # Normalizing  weights according to ranking positions
    result <- result/sum(result)
    
    # Rank averaging the probabilities
    for (col in 1:ncol(total_probabilities)) {
        total_probabilities[,col] <- result[col]*total_probabilities[,col]
    }
    final_probabilities <- rowSums(total_probabilities)
    
    return(final_probabilities)
}



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    nhc <- reactive({ input$nhc_predict })
    
    n_RDA_target_1 <- reactive({ input$n_RDA_target_1 })
    n_SVM_target_1 <- reactive({ input$n_SVM_target_1 })
    n_RF_target_1 <- reactive({ input$n_RF_target_1 })
    threshold_target_1 <- reactive({ input$threshold_target_1 })
    
    n_RDA_target_6 <- reactive({ input$n_RDA_target_6 })
    n_SVM_target_6 <- reactive({ input$n_SVM_target_6 })
    n_RF_target_6 <- reactive({ input$n_RF_target_6 })
    threshold_target_6 <- reactive({ input$threshold_target_6 })
    
    n_RDA_target_12 <- reactive({ input$n_RDA_target_12 })
    n_SVM_target_12 <- reactive({ input$n_SVM_target_12 })
    n_RF_target_12 <- reactive({ input$n_RF_target_12 })
    threshold_target_12 <- reactive({ input$threshold_target_12 })
    
    n_RDA_target_future <- reactive({ input$n_RDA_target_future })
    n_SVM_target_future <- reactive({ input$n_SVM_target_future })
    n_RF_target_future <- reactive({ input$n_RF_target_future })
    threshold_target_future <- reactive({ input$threshold_target_future })
    
    # find number of models ------------------------------------------------------------------------    
    
    targ1 <- read.csv('./Back_end/cardIA/targets/target_1.csv')
    targ6 <- read.csv('./Back_end/cardIA/targets/target_6.csv')
    targ12 <- read.csv('./Back_end/cardIA/targets/target_12.csv')
    targf <- read.csv('./Back_end/cardIA/targets/target_future.csv')
    
    sensitivity_ <- reactive({
        aux = max(0,input$sensitivity_)
        aux = min(aux,100)
        aux = round(aux)
        aux/100
    })
    specificity_ <- reactive({
        aux = max(0,input$specificity_)
        aux = min(aux,100)
        aux = round(aux)
        aux/100
    })
    
    observeEvent (c(sensitivity_(), specificity_()), {
        if (!is.na(sensitivity_()) && (is.na(specificity_())||(last_sens_spec()[1] != sensitivity_() && last_sens_spec()[2] == specificity_()))){
            delay(1500,updateNumericInput(session, "specificity_", value = format((1-sensitivity_())*100, digits = 1)))
        } else if (!is.na(specificity_()) && (is.na(sensitivity_())||(last_sens_spec()[1] == sensitivity_() && last_sens_spec()[2] != specificity_()))){
            delay(1500,updateNumericInput(session, "sensitivity_", value = format((1-specificity_())*100, digits = 1)))
    }})
    
    max_row1.10 <- reactive({ 
        targ1$score = sensitivity_()*targ1$mean_Sensitivity+specificity_()*targ1$mean_Specificity
        targ1 <- targ1[order(targ1$score, decreasing = T),]
        max_row1.10 = targ1[1:10,c(2:7, 9, 11)]
        colnames(max_row1.10)[c(3:8)] <- c("n_SVM","n_RDA", "n_RF", "Mean Accuracy", "Mean Sensitivity","Mean Specificty")
        return(max_row1.10)
    })
    
    # For target 1
    observeEvent ( c(sensitivity_(), specificity_(),tab_idx_t1()), {
        if ((! is.na(sensitivity_()) && ! is.na(specificity_()))) {
            idx = tab_idx_t1()
            max_row1 <- max_row1.10()[idx,]
            n_RDA_target_1.update <- max_row1$n_RDA
            n_SVM_target_1.update <- max_row1$n_SVM
            n_RF_target_1.update <- max_row1$n_RF
            threshold_target_1.update = max_row1$Threshold

            updateSliderInput(session, "n_RDA_target_1", value = n_RDA_target_1.update)
            updateSliderInput(session, "n_SVM_target_1", value = n_SVM_target_1.update)
            updateSliderInput(session, "n_RF_target_1", value = n_RF_target_1.update)
            updateSliderInput(session, "threshold_target_1", value = threshold_target_1.update, min = 0, max = 1, step = 0.05)
        }
    })
    
    max_row6.10 <- reactive({    
        targ6$score = sensitivity_()*targ6$mean_Sensitivity+specificity_()*targ6$mean_Specificity
        targ6 <- targ6[order(targ6$score, decreasing = T),]
        max_row6.10 = targ6[1:10,c(2:7, 9, 11)]
        colnames(max_row6.10)[c(3:8)] <- c("n_SVM","n_RDA", "n_RF", "Mean Accuracy", "Mean Sensitivity","Mean Specificty")
        return(max_row6.10)
    })
    
    # For target 6
    observeEvent ( c(sensitivity_(), specificity_(),tab_idx_t6()), {
        if ((! is.na(sensitivity_()) && ! is.na(specificity_()))) {
            idx = tab_idx_t6()
            max_row6 <- max_row6.10()[idx,]
            n_RDA_target_6.update <- max_row6$n_RDA
            n_SVM_target_6.update <- max_row6$n_SVM
            n_RF_target_6.update <- max_row6$n_RF
            threshold_target_6.update <- max_row6$Threshold
            
            updateSliderInput(session, "n_RDA_target_6", value = n_RDA_target_6.update)
            updateSliderInput(session, "n_SVM_target_6", value = n_SVM_target_6.update)
            updateSliderInput(session, "n_RF_target_6", value = n_RF_target_6.update)
            updateSliderInput(session, "threshold_target_6", value = threshold_target_6.update,min = 0, max = 1, step = 0.05)
        }
    })
    
    max_row12.10 <-reactive({  
        targ12$score = sensitivity_()*targ12$mean_Sensitivity+specificity_()*targ12$mean_Specificity
        targ12 <- targ12[order(targ12$score, decreasing = T),]
        max_row12.10 = targ12[1:10,c(2:7, 9, 11)]
        colnames(max_row12.10)[c(3:8)] <- c("n_SVM","n_RDA", "n_RF", "Mean Accuracy", "Mean Sensitivity","Mean Specificty")
        return(max_row12.10)
    })
    
    # For target 12
    observeEvent ( c(sensitivity_(), specificity_(),tab_idx_t12()), {
        if ((! is.na(sensitivity_()) && ! is.na(specificity_()))) {
            idx = tab_idx_t12()
            max_row12 <- max_row12.10()[idx,]
            n_RDA_target_12.update <- max_row12$n_RDA
            n_SVM_target_12.update <- max_row12$n_SVM
            n_RF_target_12.update <- max_row12$n_RF
            threshold_target_12.update <- max_row12$Threshold
            
            updateSliderInput(session, "n_RDA_target_12", value = n_RDA_target_12.update)
            updateSliderInput(session, "n_SVM_target_12", value = n_SVM_target_12.update)
            updateSliderInput(session, "n_RF_target_12", value = n_RF_target_12.update)
            
            updateSliderInput(session, "threshold_target_12", value = threshold_target_12.update,min = 0, max = 1, step = 0.05)
        }
    })
    
    max_rowfuture.10 <-reactive({    
        targf$score = sensitivity_()*targf$mean_Sensitivity+specificity_()*targf$mean_Specificity
        targf <- targf[order(targf$score, decreasing = T),]
        max_rowfuture.10 = targf[1:10,c(2:7, 9, 11)]
        colnames(max_rowfuture.10)[c(3:8)] <- c("n_SVM","n_RDA", "n_RF", "Mean Accuracy", "Mean Sensitivity","Mean Specificty")
        return(max_rowfuture.10)
    })
    
    # For target future
    observeEvent ( c(sensitivity_(), specificity_(),tab_idx_future()), {
        if ((! is.na(sensitivity_()) && ! is.na(specificity_()))) {
            idx = tab_idx_future()
            max_rowfuture <- max_rowfuture.10()[idx,]
            n_RDA_target_future.update <- max_rowfuture$n_RDA
            n_SVM_target_future.update <- max_rowfuture$n_SVM
            n_RF_target_future.update <- max_rowfuture$n_RF
            threshold_target_future.update <- max_rowfuture$Threshold
            
            updateSliderInput(session, "n_RDA_target_future", value = n_RDA_target_future.update)
            updateSliderInput(session, "n_SVM_target_future", value = n_SVM_target_future.update)
            updateSliderInput(session, "n_RF_target_future", value = n_RF_target_future.update)
            updateSliderInput(session, "threshold_target_future", value = threshold_target_future.update,min = 0, max = 1, step = 0.05)
        }
    })
    
    # TARGET 1 ------------------------------------------------------------------------
    
    output$res_target1_all <- renderDataTable({
      if (input$nhc_all == 'ALL'){
        rda_target_1_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_1_spl1.csv')
        rda_target_1_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_1_spl2.csv')
        rf_target_1_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_1_spl1.csv')
        rf_target_1_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_1_spl2.csv')
        svm_target_1_spl1 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_1_spl1.csv')
        svm_target_1_spl2 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_1_spl2.csv')
        
        ### **Split 1**
        len <- length(rda_target_1_spl1$Classes[,1])
        
        if (n_RDA_target_1() == 0)  {
            conf_RDA_target1_spl1 <- rep(0, len)
        } else if (n_RDA_target_1() == 1 ){
            conf_RDA_target1_spl1 <- rda_target_1_spl1$Classes[,1:n_RDA_target_1()]
        }else{
            conf_RDA_target1_spl1 <- rowSums(rda_target_1_spl1$Classes[,1:n_RDA_target_1()])
        }
        if (n_RF_target_1() == 0)  {
            conf_RF_target1_spl1 <- rep(0, len)
        } else if (n_RF_target_1() == 1){
            conf_RF_target1_spl1 <- rf_target_1_spl1$Classes[,1:n_RF_target_1()]
        }else{
            conf_RF_target1_spl1 <- rowSums(rf_target_1_spl1$Classes[,1:n_RF_target_1()])
        }
        if (n_SVM_target_1() == 0)  {
            conf_SVM_target1_spl1 <- rep(0, len)
        } else if (n_SVM_target_1() == 1){
            conf_SVM_target1_spl1 <- svm_target_1_spl1$Classes[,1:n_SVM_target_1()]
        }else{
            conf_SVM_target1_spl1 <- rowSums(svm_target_1_spl1$Classes[,1:n_SVM_target_1()])
        }
        
        confidence_target_1_spl1 <- conf_RDA_target1_spl1+ conf_RF_target1_spl1+ conf_SVM_target1_spl1
        
        majority_target_1_spl1 <- sapply(confidence_target_1_spl1, function(x) {if (x >= threshold_target_1()) return (1) else return(0)}) 
        probabilities_target_1_spl1 <- rank_averaging(rda_target_1_spl1, svm_target_1_spl1, rf_target_1_spl1, n_RDA_target_1(), n_SVM_target_1(), n_RF_target_1())
        rank_target_1_spl1 <- sapply(probabilities_target_1_spl1, function(x) {if (x >= threshold_target_1()) return (1) else return(0)})
        
        #### **Split 2**
        len <- length(rda_target_1_spl2$Classes[,1])
        
        if (n_RDA_target_1() == 0)  {
            conf_RDA_target1_spl2 <- rep(0, len)
        } else if (n_RDA_target_1() == 1 ){
            conf_RDA_target1_spl2 <- rda_target_1_spl2$Classes[,1:n_RDA_target_1()]
        }else{
            conf_RDA_target1_spl2 <- rowSums(rda_target_1_spl2$Classes[,1:n_RDA_target_1()])
        }
        if (n_RF_target_1() == 0)  {
            conf_RF_target1_spl2 <- rep(0, len)
        } else if (n_RF_target_1() == 1){
            conf_RF_target1_spl2 <- rf_target_1_spl2$Classes[,1:n_RF_target_1()]
        }else{
            conf_RF_target1_spl2 <- rowSums(rf_target_1_spl2$Classes[,1:n_RF_target_1()])
        }
        if (n_SVM_target_1() == 0)  {
            conf_SVM_target1_spl2 <- rep(0, len)
        } else if (n_SVM_target_1() == 1){
            conf_SVM_target1_spl2 <- svm_target_1_spl2$Classes[,1:n_SVM_target_1()]
        }else{
            conf_SVM_target1_spl2 <- rowSums(svm_target_1_spl2$Classes[,1:n_SVM_target_1()])
        }
        
        confidence_target_1_spl2 <- conf_RDA_target1_spl2+ conf_RF_target1_spl2+ conf_SVM_target1_spl2
        
        majority_target_1_spl2 <- sapply(confidence_target_1_spl2, function(x) {if (x >= threshold_target_1()) return (1) else return(0)}) 
        probabilities_target_1_spl2 <- rank_averaging(rda_target_1_spl2, svm_target_1_spl2, rf_target_1_spl2, n_RDA_target_1(), n_SVM_target_1(), n_RF_target_1())
        rank_target_1_spl2 <- sapply(probabilities_target_1_spl2, function(x) {if (x >= threshold_target_1()) return (1) else return(0)})
        
        df1 <- data.frame(nhc_spl1, rank_target_1_spl1, probabilities_target_1_spl1)
        df1 <- df1[df1$rank_target_1_spl1 == 1,]
        colnames(df1) <- c("nhc_spl2" , "rank_target_1_spl2", "probabilities_target_1_spl2")
        
        df2 <- data.frame(nhc_spl2, rank_target_1_spl2, probabilities_target_1_spl2)
        df2 <- df2[df2$rank_target_1_spl2 == 1,]
        
        df <- rbind(df1, df2) 
        df <- df[order(df$probabilities_target_1_spl2, decreasing = T),]
        
        aux = df$probabilities_target_1_spl2
        
        rows_target1 <- (aux >= 0.70)                               
        target_1_ <- df[rows_target1,]
        highrisk_target1(nrow(target_1_))                               
        
        mediumrisk_target1(nrow(df) - highrisk_target1())
        
        lowrisk_target1(total_patients - mediumrisk_target1() - highrisk_target1())
            
        colnames(df)[1] = "Patient ID"
        
        datatable(df, 
                  extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                  rownames = FALSE,
                  selection = 'none',
                  options = list(
                      searching = FALSE,
                      autoWidth = TRUE,
                      scroller = TRUE,
                      scrollX = FALSE,
                      scrollY = "400px",
                      columnDefs = list(list(visible=FALSE, targets=c(1,2)),list(className = 'dt-center', targets = 0)),
                      info = FALSE,
                      class = 'cell-border stripe'
                  )
        ) 
    }})
    
    output$res_target1 <- renderPrint({
        validate(nhc_ne(input$nhc_predict))
        rda_target_1_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_1_spl1.csv')
        rda_target_1_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_1_spl2.csv')
        rf_target_1_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_1_spl1.csv')
        rf_target_1_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_1_spl2.csv')
        svm_target_1_spl1 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_1_spl1.csv')
        svm_target_1_spl2 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_1_spl2.csv')
        
        ### **Split 1**
        len <- length(rda_target_1_spl1$Classes[,1])
        
        
        if (n_RDA_target_1() == 0)  {
            conf_RDA_target1_spl1 <- rep(0, len)
        } else if (n_RDA_target_1() == 1 ){
            conf_RDA_target1_spl1 <- rda_target_1_spl1$Classes[,1:n_RDA_target_1()]
        }else{
            conf_RDA_target1_spl1 <- rowSums(rda_target_1_spl1$Classes[,1:n_RDA_target_1()])
        }
        if (n_RF_target_1() == 0)  {
            conf_RF_target1_spl1 <- rep(0, len)
        } else if (n_RF_target_1() == 1){
            conf_RF_target1_spl1 <- rf_target_1_spl1$Classes[,1:n_RF_target_1()]
        }else{
            conf_RF_target1_spl1 <- rowSums(rf_target_1_spl1$Classes[,1:n_RF_target_1()])
        }
        if (n_SVM_target_1() == 0)  {
            conf_SVM_target1_spl1 <- rep(0, len)
        } else if (n_SVM_target_1() == 1){
            conf_SVM_target1_spl1 <- svm_target_1_spl1$Classes[,1:n_SVM_target_1()]
        }else{
            conf_SVM_target1_spl1 <- rowSums(svm_target_1_spl1$Classes[,1:n_SVM_target_1()])
        }
        
        confidence_target_1_spl1 <- conf_RDA_target1_spl1+ conf_RF_target1_spl1+ conf_SVM_target1_spl1
        
        majority_target_1_spl1 <- sapply(confidence_target_1_spl1, function(x) {if (x >= threshold_target_1()) return (1) else return(0)}) 
        probabilities_target_1_spl1 <- rank_averaging(rda_target_1_spl1, svm_target_1_spl1, rf_target_1_spl1, n_RDA_target_1(), n_SVM_target_1(), n_RF_target_1())
        rank_target_1_spl1 <- sapply(probabilities_target_1_spl1, function(x) {if (x >= threshold_target_1()) return (1) else return(0)})
        
        #### **Split 2**
        len <- length(rda_target_1_spl2$Classes[,1])
        
        if (n_RDA_target_1() == 0)  {
            conf_RDA_target1_spl2 <- rep(0, len)
        } else if (n_RDA_target_1() == 1 ){
            conf_RDA_target1_spl2 <- rda_target_1_spl2$Classes[,1:n_RDA_target_1()]
        }else{
            conf_RDA_target1_spl2 <- rowSums(rda_target_1_spl2$Classes[,1:n_RDA_target_1()])
        }
        if (n_RF_target_1() == 0)  {
            conf_RF_target1_spl2 <- rep(0, len)
        } else if (n_RF_target_1() == 1){
            conf_RF_target1_spl2 <- rf_target_1_spl2$Classes[,1:n_RF_target_1()]
        }else{
            conf_RF_target1_spl2 <- rowSums(rf_target_1_spl2$Classes[,1:n_RF_target_1()])
        }
        if (n_SVM_target_1() == 0)  {
            conf_SVM_target1_spl2 <- rep(0, len)
        } else if (n_SVM_target_1() == 1){
            conf_SVM_target1_spl2 <- svm_target_1_spl2$Classes[,1:n_SVM_target_1()]
        }else{
            conf_SVM_target1_spl2 <- rowSums(svm_target_1_spl2$Classes[,1:n_SVM_target_1()])
        }
        
        confidence_target_1_spl2 <- conf_RDA_target1_spl2+ conf_RF_target1_spl2+ conf_SVM_target1_spl2
        
        majority_target_1_spl2 <- sapply(confidence_target_1_spl2, function(x) {if (x >= threshold_target_1()) return (1) else return(0)}) 
        probabilities_target_1_spl2 <- rank_averaging(rda_target_1_spl2, svm_target_1_spl2, rf_target_1_spl2, n_RDA_target_1(), n_SVM_target_1(), n_RF_target_1())
        rank_target_1_spl2 <- sapply(probabilities_target_1_spl2, function(x) {if (x >= threshold_target_1()) return (1) else return(0)})
        
        if (nhc() %in% nhc_spl1) {
            position_nhc <- which(nhc_spl1 %in% nhc())
            prob = probabilities_target_1_spl1[position_nhc]
            prob_string = toString(base::round(prob,2))
            if (prob >= 0.7) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#e8152a"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #e8152a;">',prob_string,'</pre>'))
            else if (prob < 0.7 & prob >= threshold_target_1()) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#ece205"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #ece205;">',prob_string,'</pre>'))
            else HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#56b83e"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #56b83e;">',prob_string,'</pre>'))
        } else {
            position_nhc <- which(nhc_spl2 %in% nhc())
            prob = probabilities_target_1_spl2[position_nhc]
            prob_string = toString(base::round(prob,2))
            if (prob >= 0.7) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#e8152a"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #e8152a;">',prob_string,'</pre>'))
            else if (prob < 0.7 & prob >= threshold_target_1()) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#ece205"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #ece205;">',prob_string,'</pre>'))
            else HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#56b83e"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #56b83e;">',prob_string,'</pre>'))
        } 
    })
    
    
    # TARGET 6 ------------------------------------------------------------------------
    
    output$res_target6_all <- renderDataTable({
      if (input$nhc_all == 'ALL'){
        rda_target_6_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_6_spl1.csv')
        rda_target_6_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_6_spl2.csv')
        rf_target_6_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_6_spl1.csv')
        rf_target_6_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_6_spl2.csv')
        svm_target_6_spl1 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_6_spl1.csv')
        svm_target_6_spl2 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_6_spl2.csv')


        ### **Split 1**
        len <- length(rda_target_6_spl1$Classes[,1])


        if (n_RDA_target_6() == 0)  {
            conf_RDA_target6_spl1 <- rep(0, len)
        } else if (n_RDA_target_6() == 1 ){
            conf_RDA_target6_spl1 <- rda_target_6_spl1$Classes[,1:n_RDA_target_6()]
        }else{
            conf_RDA_target6_spl1 <- rowSums(rda_target_6_spl1$Classes[,1:n_RDA_target_6()])
        }
        if (n_RF_target_6() == 0)  {
            conf_RF_target6_spl1 <- rep(0, len)
        } else if (n_RF_target_6() == 1){
            conf_RF_target6_spl1 <- rf_target_6_spl1$Classes[,1:n_RF_target_6()]
        }else{
            conf_RF_target6_spl1 <- rowSums(rf_target_6_spl1$Classes[,1:n_RF_target_6()])
        }
        if (n_SVM_target_6() == 0)  {
            conf_SVM_target6_spl1 <- rep(0, len)
        } else if (n_SVM_target_6() == 1){
            conf_SVM_target6_spl1 <- svm_target_6_spl1$Classes[,1:n_SVM_target_6()]
        }else{
            conf_SVM_target6_spl1 <- rowSums(svm_target_6_spl1$Classes[,1:n_SVM_target_6()])
        }

        confidence_target_6_spl1 <- conf_RDA_target6_spl1+ conf_RF_target6_spl1+ conf_SVM_target6_spl1

        majority_target_6_spl1 <- sapply(confidence_target_6_spl1, function(x) {if (x >= threshold_target_6()) return (1) else return(0)})
        probabilities_target_6_spl1 <- rank_averaging(rda_target_6_spl1, svm_target_6_spl1, rf_target_6_spl1, n_RDA_target_6(), n_SVM_target_6(), n_RF_target_6())
        rank_target_6_spl1 <- sapply(probabilities_target_6_spl1, function(x) {if (x >= threshold_target_6()) return (1) else return(0)})

        #### **Split 2**
        len <- length(rda_target_6_spl2$Classes[,1])

        if (n_RDA_target_6() == 0)  {
            conf_RDA_target6_spl2 <- rep(0, len)
        } else if (n_RDA_target_6() == 1 ){
            conf_RDA_target6_spl2 <- rda_target_6_spl2$Classes[,1:n_RDA_target_6()]
        }else{
            conf_RDA_target6_spl2 <- rowSums(rda_target_6_spl2$Classes[,1:n_RDA_target_6()])
        }
        if (n_RF_target_6() == 0)  {
            conf_RF_target6_spl2 <- rep(0, len)
        } else if (n_RF_target_6() == 1){
            conf_RF_target6_spl2 <- rf_target_6_spl2$Classes[,1:n_RF_target_6()]
        }else{
            conf_RF_target6_spl2 <- rowSums(rf_target_6_spl2$Classes[,1:n_RF_target_6()])
        }
        if (n_SVM_target_6() == 0)  {
            conf_SVM_target6_spl2 <- rep(0, len)
        } else if (n_SVM_target_6() == 1){
            conf_SVM_target6_spl2 <- svm_target_6_spl2$Classes[,1:n_SVM_target_6()]
        }else{
            conf_SVM_target6_spl2 <- rowSums(svm_target_6_spl2$Classes[,1:n_SVM_target_6()])
        }

        confidence_target_6_spl2 <- conf_RDA_target6_spl2+ conf_RF_target6_spl2+ conf_SVM_target6_spl2

        majority_target_6_spl2 <- sapply(confidence_target_6_spl2, function(x) {if (x >= threshold_target_6()) return (1) else return(0)})
        probabilities_target_6_spl2 <- rank_averaging(rda_target_6_spl2, svm_target_6_spl2, rf_target_6_spl2, n_RDA_target_6(), n_SVM_target_6(), n_RF_target_6())
        rank_target_6_spl2 <- sapply(probabilities_target_6_spl2, function(x) {if (x >= threshold_target_6()) return (1) else return(0)})

        df1 <- data.frame(nhc_spl1, rank_target_6_spl1, probabilities_target_6_spl1)
        df1 <- df1[df1$rank_target_6_spl1 == 1,]
        colnames(df1) <- c("nhc_spl2" , "rank_target_6_spl2", "probabilities_target_6_spl2")

        df2 <- data.frame(nhc_spl2, rank_target_6_spl2, probabilities_target_6_spl2)
        df2 <- df2[df2$rank_target_6_spl2 == 1,]

        df <- rbind(df1, df2)
        df <- df[order(df$probabilities_target_6_spl2, decreasing = T),]
        
        aux = df$probabilities_target_6_spl2
        
        rows_target6 <- (aux >= 0.70)                               
        target_6_ <- df[rows_target6,]
        highrisk_target6(nrow(target_6_))                               
        
        mediumrisk_target6(nrow(df) - highrisk_target6())                   
        
        lowrisk_target6(total_patients - mediumrisk_target6() - highrisk_target6())
            
        colnames(df)[1] = "Patient ID"
        
        datatable(df, 
                  extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                  rownames = FALSE,
                  selection = 'none',
                  options = list(
                      searching = FALSE,
                      autoWidth = TRUE,
                      scroller = TRUE,
                      scrollX = FALSE,
                      scrollY = "400px",
                      columnDefs = list(list(visible=FALSE, targets=c(1,2)),list(className = 'dt-center', targets = 0)),
                      info = FALSE,
                      class = 'cell-border stripe'
                  )
        ) 
    }})

    output$res_target6 <- renderPrint({
        validate(nhc_ne(input$nhc_predict))
        rda_target_6_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_6_spl1.csv')
        rda_target_6_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_6_spl2.csv')
        rf_target_6_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_6_spl1.csv')
        rf_target_6_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_6_spl2.csv')
        svm_target_6_spl1 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_6_spl1.csv')
        svm_target_6_spl2 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_6_spl2.csv')
        
        ### **Split 1**
        len <- length(rda_target_6_spl1$Classes[,1])
        
        
        if (n_RDA_target_6() == 0)  {
            conf_RDA_target6_spl1 <- rep(0, len)
        } else if (n_RDA_target_6() == 1 ){
            conf_RDA_target6_spl1 <- rda_target_6_spl1$Classes[,1:n_RDA_target_6()]
        }else{
            conf_RDA_target6_spl1 <- rowSums(rda_target_6_spl1$Classes[,1:n_RDA_target_6()])
        }
        if (n_RF_target_6() == 0)  {
            conf_RF_target6_spl1 <- rep(0, len)
        } else if (n_RF_target_6() == 1){
            conf_RF_target6_spl1 <- rf_target_6_spl1$Classes[,1:n_RF_target_6()]
        }else{
            conf_RF_target6_spl1 <- rowSums(rf_target_6_spl1$Classes[,1:n_RF_target_6()])
        }
        if (n_SVM_target_6() == 0)  {
            conf_SVM_target6_spl1 <- rep(0, len)
        } else if (n_SVM_target_6() == 1){
            conf_SVM_target6_spl1 <- svm_target_6_spl1$Classes[,1:n_SVM_target_6()]
        }else{
            conf_SVM_target6_spl1 <- rowSums(svm_target_6_spl1$Classes[,1:n_SVM_target_6()])
        }
        
        confidence_target_6_spl1 <- conf_RDA_target6_spl1+ conf_RF_target6_spl1+ conf_SVM_target6_spl1
        
        majority_target_6_spl1 <- sapply(confidence_target_6_spl1, function(x) {if (x >= threshold_target_6()) return (1) else return(0)}) 
        probabilities_target_6_spl1 <- rank_averaging(rda_target_6_spl1, svm_target_6_spl1, rf_target_6_spl1, n_RDA_target_6(), n_SVM_target_6(), n_RF_target_6())
        rank_target_6_spl1 <- sapply(probabilities_target_6_spl1, function(x) {if (x >= threshold_target_6()) return (1) else return(0)})
        
        #### **Split 2**
        len <- length(rda_target_6_spl2$Classes[,1])
        
        if (n_RDA_target_6() == 0)  {
            conf_RDA_target6_spl2 <- rep(0, len)
        } else if (n_RDA_target_6() == 1 ){
            conf_RDA_target6_spl2 <- rda_target_6_spl2$Classes[,1:n_RDA_target_6()]
        }else{
            conf_RDA_target6_spl2 <- rowSums(rda_target_6_spl2$Classes[,1:n_RDA_target_6()])
        }
        if (n_RF_target_6() == 0)  {
            conf_RF_target6_spl2 <- rep(0, len)
        } else if (n_RF_target_6() == 1){
            conf_RF_target6_spl2 <- rf_target_6_spl2$Classes[,1:n_RF_target_6()]
        }else{
            conf_RF_target6_spl2 <- rowSums(rf_target_6_spl2$Classes[,1:n_RF_target_6()])
        }
        if (n_SVM_target_6() == 0)  {
            conf_SVM_target6_spl2 <- rep(0, len)
        } else if (n_SVM_target_6() == 1){
            conf_SVM_target6_spl2 <- svm_target_6_spl2$Classes[,1:n_SVM_target_6()]
        }else{
            conf_SVM_target6_spl2 <- rowSums(svm_target_6_spl2$Classes[,1:n_SVM_target_6()])
        }
        
        confidence_target_6_spl2 <- conf_RDA_target6_spl2+ conf_RF_target6_spl2+ conf_SVM_target6_spl2
        
        majority_target_6_spl2 <- sapply(confidence_target_6_spl2, function(x) {if (x >= threshold_target_6()) return (1) else return(0)}) 
        probabilities_target_6_spl2 <- rank_averaging(rda_target_6_spl2, svm_target_6_spl2, rf_target_6_spl2, n_RDA_target_6(), n_SVM_target_6(), n_RF_target_6())
        rank_target_6_spl2 <- sapply(probabilities_target_6_spl2, function(x) {if (x >= threshold_target_6()) return (1) else return(0)})
        
        if (nhc() %in% nhc_spl1) {
            position_nhc <- which(nhc_spl1 %in% nhc())
            prob = probabilities_target_6_spl1[position_nhc]
            prob_string = toString(base::round(prob,2))
            if (prob >= 0.7) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#e8152a"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #e8152a;">',prob_string,'</pre>'))
            else if (prob < 0.7 & prob >= threshold_target_6()) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#ece205"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #ece205;">',prob_string,'</pre>'))
            else HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#56b83e"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #56b83e;">',prob_string,'</pre>'))
        } else {
            position_nhc <- which(nhc_spl2 %in% nhc())
            prob = probabilities_target_6_spl2[position_nhc]
            prob_string = toString(base::round(prob,2))
            if (prob >= 0.7) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#e8152a"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #e8152a;">',prob_string,'</pre>'))
            else if (prob < 0.7 & prob >= threshold_target_6()) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#ece205"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #ece205;">',prob_string,'</pre>'))
            else HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#56b83e"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #56b83e;">',prob_string,'</pre>'))
        }
    })
    
    # TARGET 12 ------------------------------------------------------------------------
    
    output$res_target12_all <- renderDataTable({
      if (input$nhc_all == 'ALL'){
        rda_target_12_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_12_spl1.csv')
        rda_target_12_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_12_spl2.csv')
        rf_target_12_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_12_spl1.csv')
        rf_target_12_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_12_spl2.csv')
        svm_target_12_spl1 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_12_spl1.csv')
        svm_target_12_spl2 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_12_spl2.csv')
        
        ### **Split 1**
        len <- length(rda_target_12_spl1$Classes[,1])
        
        
        if (n_RDA_target_12() == 0)  {
          conf_RDA_target12_spl1 <- rep(0, len)
        } else if (n_RDA_target_12() == 1 ){
          conf_RDA_target12_spl1 <- rda_target_12_spl1$Classes[,1:n_RDA_target_12()]
        }else{
          conf_RDA_target12_spl1 <- rowSums(rda_target_12_spl1$Classes[,1:n_RDA_target_12()])
        }
        if (n_RF_target_12() == 0)  {
          conf_RF_target12_spl1 <- rep(0, len)
        } else if (n_RF_target_12() == 1){
          conf_RF_target12_spl1 <- rf_target_12_spl1$Classes[,1:n_RF_target_12()]
        }else{
          conf_RF_target12_spl1 <- rowSums(rf_target_12_spl1$Classes[,1:n_RF_target_12()])
        }
        if (n_SVM_target_12() == 0)  {
          conf_SVM_target12_spl1 <- rep(0, len)
        } else if (n_SVM_target_12() == 1){
          conf_SVM_target12_spl1 <- svm_target_12_spl1$Classes[,1:n_SVM_target_12()]
        }else{
          conf_SVM_target12_spl1 <- rowSums(svm_target_12_spl1$Classes[,1:n_SVM_target_12()])
        }
        
        confidence_target_12_spl1 <- conf_RDA_target12_spl1+ conf_RF_target12_spl1+ conf_SVM_target12_spl1
        
        majority_target_12_spl1 <- sapply(confidence_target_12_spl1, function(x) {if (x >= threshold_target_12()) return (1) else return(0)}) 
        probabilities_target_12_spl1 <- rank_averaging(rda_target_12_spl1, svm_target_12_spl1, rf_target_12_spl1, n_RDA_target_12(), n_SVM_target_12(), n_RF_target_12())
        rank_target_12_spl1 <- sapply(probabilities_target_12_spl1, function(x) {if (x >= threshold_target_12()) return (1) else return(0)})
        
        #### **Split 2**
        len <- length(rda_target_12_spl2$Classes[,1])
        
        if (n_RDA_target_12() == 0)  {
          conf_RDA_target12_spl2 <- rep(0, len)
        } else if (n_RDA_target_12() == 1 ){
          conf_RDA_target12_spl2 <- rda_target_12_spl2$Classes[,1:n_RDA_target_12()]
        }else{
          conf_RDA_target12_spl2 <- rowSums(rda_target_12_spl2$Classes[,1:n_RDA_target_12()])
        }
        if (n_RF_target_12() == 0)  {
          conf_RF_target12_spl2 <- rep(0, len)
        } else if (n_RF_target_12() == 1){
          conf_RF_target12_spl2 <- rf_target_12_spl2$Classes[,1:n_RF_target_12()]
        }else{
          conf_RF_target12_spl2 <- rowSums(rf_target_12_spl2$Classes[,1:n_RF_target_12()])
        }
        if (n_SVM_target_12() == 0)  {
          conf_SVM_target12_spl2 <- rep(0, len)
        } else if (n_SVM_target_12() == 1){
          conf_SVM_target12_spl2 <- svm_target_12_spl2$Classes[,1:n_SVM_target_12()]
        }else{
          conf_SVM_target12_spl2 <- rowSums(svm_target_12_spl2$Classes[,1:n_SVM_target_12()])
        }
        
        confidence_target_12_spl2 <- conf_RDA_target12_spl2+ conf_RF_target12_spl2+ conf_SVM_target12_spl2
        
        majority_target_12_spl2 <- sapply(confidence_target_12_spl2, function(x) {if (x >= threshold_target_12()) return (1) else return(0)}) 
        probabilities_target_12_spl2 <- rank_averaging(rda_target_12_spl2, svm_target_12_spl2, rf_target_12_spl2, n_RDA_target_12(), n_SVM_target_12(), n_RF_target_12())
        rank_target_12_spl2 <- sapply(probabilities_target_12_spl2, function(x) {if (x >= threshold_target_12()) return (1) else return(0)})
        
        df1 <- data.frame(nhc_spl1, rank_target_12_spl1, probabilities_target_12_spl1)
        df1 <- df1[df1$rank_target_12_spl1 == 1,]
        colnames(df1) <- c("nhc_spl2" , "rank_target_12_spl2", "probabilities_target_12_spl2")
        
        df2 <- data.frame(nhc_spl2, rank_target_12_spl2, probabilities_target_12_spl2)
        df2 <- df2[df2$rank_target_12_spl2 == 1,]
        
        df <- rbind(df1, df2) 
        df <- df[order(df$probabilities_target_12_spl2, decreasing = T),]
        
        aux = df$probabilities_target_12_spl2
        
        rows_target12 <- (aux >= 0.70)                               
        target_12_ <- df[rows_target12,]
        highrisk_target12(nrow(target_12_))                               
        
        mediumrisk_target12(nrow(df) - highrisk_target12())                  
        
        lowrisk_target12(total_patients - mediumrisk_target12() - highrisk_target12())
        
        colnames(df)[1] = "Patient ID"
        
        datatable(df, 
                  extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                  rownames = FALSE,
                  selection = 'none',
                  options = list(
                    searching = FALSE,
                    autoWidth = TRUE,
                    scroller = TRUE,
                    scrollX = FALSE,
                    scrollY = "400px",
                    columnDefs = list(list(visible=FALSE, targets=c(1,2)),list(className = 'dt-center', targets = 0)),
                    info = FALSE,
                    class = 'cell-border stripe'
                  )
        ) 
    }})
   
     output$res_target12 <- renderPrint({
        validate(nhc_ne(input$nhc_predict))
        rda_target_12_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_12_spl1.csv')
        rda_target_12_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_12_spl2.csv')
        rf_target_12_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_12_spl1.csv')
        rf_target_12_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_12_spl2.csv')
        svm_target_12_spl1 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_12_spl1.csv')
        svm_target_12_spl2 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_12_spl2.csv')
        
        ### **Split 1**
        len <- length(rda_target_12_spl1$Classes[,1])
        
        
        if (n_RDA_target_12() == 0)  {
            conf_RDA_target12_spl1 <- rep(0, len)
        } else if (n_RDA_target_12() == 1 ){
            conf_RDA_target12_spl1 <- rda_target_12_spl1$Classes[,1:n_RDA_target_12()]
        }else{
            conf_RDA_target12_spl1 <- rowSums(rda_target_12_spl1$Classes[,1:n_RDA_target_12()])
        }
        if (n_RF_target_12() == 0)  {
            conf_RF_target12_spl1 <- rep(0, len)
        } else if (n_RF_target_12() == 1){
            conf_RF_target12_spl1 <- rf_target_12_spl1$Classes[,1:n_RF_target_12()]
        }else{
            conf_RF_target12_spl1 <- rowSums(rf_target_12_spl1$Classes[,1:n_RF_target_12()])
        }
        if (n_SVM_target_12() == 0)  {
            conf_SVM_target12_spl1 <- rep(0, len)
        } else if (n_SVM_target_12() == 1){
            conf_SVM_target12_spl1 <- svm_target_12_spl1$Classes[,1:n_SVM_target_12()]
        }else{
            conf_SVM_target12_spl1 <- rowSums(svm_target_12_spl1$Classes[,1:n_SVM_target_12()])
        }
        
        confidence_target_12_spl1 <- conf_RDA_target12_spl1+ conf_RF_target12_spl1+ conf_SVM_target12_spl1
        
        majority_target_12_spl1 <- sapply(confidence_target_12_spl1, function(x) {if (x >= threshold_target_12()) return (1) else return(0)}) 
        probabilities_target_12_spl1 <- rank_averaging(rda_target_12_spl1, svm_target_12_spl1, rf_target_12_spl1, n_RDA_target_12(), n_SVM_target_12(), n_RF_target_12())
        rank_target_12_spl1 <- sapply(probabilities_target_12_spl1, function(x) {if (x >= threshold_target_12()) return (1) else return(0)})
        
        #### **Split 2**
        len <- length(rda_target_12_spl2$Classes[,1])
        
        if (n_RDA_target_12() == 0)  {
            conf_RDA_target12_spl2 <- rep(0, len)
        } else if (n_RDA_target_12() == 1 ){
            conf_RDA_target12_spl2 <- rda_target_12_spl2$Classes[,1:n_RDA_target_12()]
        }else{
            conf_RDA_target12_spl2 <- rowSums(rda_target_12_spl2$Classes[,1:n_RDA_target_12()])
        }
        if (n_RF_target_12() == 0)  {
            conf_RF_target12_spl2 <- rep(0, len)
        } else if (n_RF_target_12() == 1){
            conf_RF_target12_spl2 <- rf_target_12_spl2$Classes[,1:n_RF_target_12()]
        }else{
            conf_RF_target12_spl2 <- rowSums(rf_target_12_spl2$Classes[,1:n_RF_target_12()])
        }
        if (n_SVM_target_12() == 0)  {
            conf_SVM_target12_spl2 <- rep(0, len)
        } else if (n_SVM_target_12() == 1){
            conf_SVM_target12_spl2 <- svm_target_12_spl2$Classes[,1:n_SVM_target_12()]
        }else{
            conf_SVM_target12_spl2 <- rowSums(svm_target_12_spl2$Classes[,1:n_SVM_target_12()])
        }
        
        confidence_target_12_spl2 <- conf_RDA_target12_spl2+ conf_RF_target12_spl2+ conf_SVM_target12_spl2
        
        majority_target_12_spl2 <- sapply(confidence_target_12_spl2, function(x) {if (x >= threshold_target_12()) return (1) else return(0)}) 
        probabilities_target_12_spl2 <- rank_averaging(rda_target_12_spl2, svm_target_12_spl2, rf_target_12_spl2, n_RDA_target_12(), n_SVM_target_12(), n_RF_target_12())
        rank_target_12_spl2 <- sapply(probabilities_target_12_spl2, function(x) {if (x >= threshold_target_12()) return (1) else return(0)})
        
        if (nhc() %in% nhc_spl1) {
            position_nhc <- which(nhc_spl1 %in% nhc())
            prob = probabilities_target_12_spl1[position_nhc]
            prob_string = toString(base::round(prob,2))
            if (prob >= 0.7) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#e8152a"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #e8152a;">',prob_string,'</pre>'))
            else if (prob < 0.7 & prob >= threshold_target_12()) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#ece205"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #ece205;">',prob_string,'</pre>'))
            else HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#56b83e"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #56b83e;">',prob_string,'</pre>'))
        } else {
            position_nhc <- which(nhc_spl2 %in% nhc())
            prob = probabilities_target_12_spl2[position_nhc]
            prob_string = toString(base::round(prob,2))
            if (prob >= 0.7) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#e8152a"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #e8152a;">',prob_string,'</pre>'))
            else if (prob < 0.7 & prob >= threshold_target_12()) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#ece205"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #ece205;">',prob_string,'</pre>'))
            else HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#56b83e"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #56b83e;">',prob_string,'</pre>'))
        }
    })
     
     # TARGET FUTURE ------------------------------------------------------------------------
     
    output$res_targetfuture_all <- renderDataTable({
      if (input$nhc_all == 'ALL'){
        rda_target_future_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_future_spl1.csv')
        rda_target_future_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_future_spl2.csv')
        rf_target_future_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_future_spl1.csv')
        rf_target_future_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_future_spl2.csv')
        svm_target_future_spl1 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_future_spl1.csv')
        svm_target_future_spl2 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_future_spl2.csv')
        
        ### **Split 1**
        len <- length(rda_target_future_spl1$Classes[,1])
        
        if (n_RDA_target_future() == 0)  {
          conf_RDA_targetfuture_spl1 <- rep(0, len)
        } else if (n_RDA_target_future() == 1 ){
          conf_RDA_targetfuture_spl1 <- rda_target_future_spl1$Classes[,1:n_RDA_target_future()]
        }else{
          conf_RDA_targetfuture_spl1 <- rowSums(rda_target_future_spl1$Classes[,1:n_RDA_target_future()])
        }
        if (n_RF_target_future() == 0)  {
          conf_RF_targetfuture_spl1 <- rep(0, len)
        } else if (n_RF_target_future() == 1){
          conf_RF_targetfuture_spl1 <- rf_target_future_spl1$Classes[,1:n_RF_target_future()]
        }else{
          conf_RF_targetfuture_spl1 <- rowSums(rf_target_future_spl1$Classes[,1:n_RF_target_future()])
        }
        if (n_SVM_target_future() == 0)  {
          conf_SVM_targetfuture_spl1 <- rep(0, len)
        } else if (n_SVM_target_future() == 1){
          conf_SVM_targetfuture_spl1 <- svm_target_future_spl1$Classes[,1:n_SVM_target_future()]
        }else{
          conf_SVM_targetfuture_spl1 <- rowSums(svm_target_future_spl1$Classes[,1:n_SVM_target_future()])
        }
        
        confidence_target_future_spl1 <- conf_RDA_targetfuture_spl1+ conf_RF_targetfuture_spl1+ conf_SVM_targetfuture_spl1
        
        majority_target_future_spl1 <- sapply(confidence_target_future_spl1, function(x) {if (x >= threshold_target_future()) return (1) else return(0)}) 
        probabilities_target_future_spl1 <- rank_averaging(rda_target_future_spl1, svm_target_future_spl1, rf_target_future_spl1, n_RDA_target_future(), n_SVM_target_future(), n_RF_target_future())
        rank_target_future_spl1 <- sapply(probabilities_target_future_spl1, function(x) {if (x >= threshold_target_future()) return (1) else return(0)})
        
        #### **Split 2**
        len <- length(rda_target_future_spl2$Classes[,1])
        if (n_RDA_target_future() == 0)  {
          conf_RDA_targetfuture_spl2 <- rep(0, len)
        } else if (n_RDA_target_future() == 1 ){
          conf_RDA_targetfuture_spl2 <- rda_target_future_spl2$Classes[,1:n_RDA_target_future()]
        }else{
          conf_RDA_targetfuture_spl2 <- rowSums(rda_target_future_spl2$Classes[,1:n_RDA_target_future()])
        }
        if (n_RF_target_future() == 0)  {
          conf_RF_targetfuture_spl2 <- rep(0, len)
        } else if (n_RF_target_future() == 1){
          conf_RF_targetfuture_spl2 <- rf_target_future_spl2$Classes[,1:n_RF_target_future()]
        }else{
          conf_RF_targetfuture_spl2 <- rowSums(rf_target_future_spl2$Classes[,1:n_RF_target_future()])
        }
        if (n_SVM_target_future() == 0)  {
          conf_SVM_targetfuture_spl2 <- rep(0, len)
        } else if (n_SVM_target_future() == 1){
          conf_SVM_targetfuture_spl2 <- svm_target_future_spl2$Classes[,1:n_SVM_target_future()]
        }else{
          conf_SVM_targetfuture_spl2 <- rowSums(svm_target_future_spl2$Classes[,1:n_SVM_target_future()])
        }
        
        confidence_target_future_spl2 <- conf_RDA_targetfuture_spl2+ conf_RF_targetfuture_spl2+ conf_SVM_targetfuture_spl2
        
        majority_target_future_spl2 <- sapply(confidence_target_future_spl2, function(x) {if (x >= threshold_target_future()) return (1) else return(0)}) 
        probabilities_target_future_spl2 <- rank_averaging(rda_target_future_spl2, svm_target_future_spl2, rf_target_future_spl2, n_RDA_target_future(), n_SVM_target_future(), n_RF_target_future())
        rank_target_future_spl2 <- sapply(probabilities_target_future_spl2, function(x) {if (x >= threshold_target_future()) return (1) else return(0)})
        
        df1 <- data.frame(nhc_spl1, rank_target_future_spl1, probabilities_target_future_spl1)
        df1 <- df1[df1$rank_target_future_spl1 == 1,]
        colnames(df1) <- c("nhc_spl2" , "rank_target_future_spl2", "probabilities_target_future_spl2")
        
        df2 <- data.frame(nhc_spl2, rank_target_future_spl2, probabilities_target_future_spl2)
        df2 <- df2[df2$rank_target_future_spl2 == 1,]
        
        df <- rbind(df1, df2) 
        df <- df[order(df$probabilities_target_future_spl2, decreasing = T),]
        
        aux = df$probabilities_target_future_spl2
        
        rows_targetfuture <- (aux >= 0.70)
        
        target_future_ <- df[rows_targetfuture,]
        
        highrisk_targetfuture(nrow(target_future_))                               
        
        mediumrisk_targetfuture(nrow(df) - highrisk_targetfuture())                 
        
        lowrisk_targetfuture(total_patients - mediumrisk_targetfuture() - highrisk_targetfuture())
          
        colnames(df)[1] = "Patient ID"
        
        datatable(df, 
                  extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                  rownames = FALSE,
                  selection = 'none',
                  options = list(
                    searching = FALSE,
                    autoWidth = TRUE,
                    scroller = TRUE,
                    scrollX = FALSE,
                    scrollY = "400px",
                    columnDefs = list(list(visible=FALSE, targets=c(1,2)),list(className = 'dt-center', targets = 0)),
                    info = FALSE,
                    class = 'cell-border stripe'
                  )
        ) 
        
    }})
    
    output$res_targetfuture <- renderPrint({
        validate(nhc_ne(input$nhc_predict))
        rda_target_future_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_future_spl1.csv')
        rda_target_future_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rda_target_future_spl2.csv')
        rf_target_future_spl1 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_future_spl1.csv')
        rf_target_future_spl2 <- readRDS('./Back_end/cardIA/final_predictions/rf_target_future_spl2.csv')
        svm_target_future_spl1 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_future_spl1.csv')
        svm_target_future_spl2 <- readRDS('./Back_end/cardIA/final_predictions/svm_target_future_spl2.csv')
        
        ### **Split 1**
        len <- length(rda_target_future_spl1$Classes[,1])
        
        
        if (n_RDA_target_future() == 0)  {
            conf_RDA_targetfuture_spl1 <- rep(0, len)
        } else if (n_RDA_target_future() == 1 ){
            conf_RDA_targetfuture_spl1 <- rda_target_future_spl1$Classes[,1:n_RDA_target_future()]
        }else{
            conf_RDA_targetfuture_spl1 <- rowSums(rda_target_future_spl1$Classes[,1:n_RDA_target_future()])
        }
        if (n_RF_target_future() == 0)  {
            conf_RF_targetfuture_spl1 <- rep(0, len)
        } else if (n_RF_target_future() == 1){
            conf_RF_targetfuture_spl1 <- rf_target_future_spl1$Classes[,1:n_RF_target_future()]
        }else{
            conf_RF_targetfuture_spl1 <- rowSums(rf_target_future_spl1$Classes[,1:n_RF_target_future()])
        }
        if (n_SVM_target_future() == 0)  {
            conf_SVM_targetfuture_spl1 <- rep(0, len)
        } else if (n_SVM_target_future() == 1){
            conf_SVM_targetfuture_spl1 <- svm_target_future_spl1$Classes[,1:n_SVM_target_future()]
        }else{
            conf_SVM_targetfuture_spl1 <- rowSums(svm_target_future_spl1$Classes[,1:n_SVM_target_future()])
        }
        
        confidence_target_future_spl1 <- conf_RDA_targetfuture_spl1+ conf_RF_targetfuture_spl1+ conf_SVM_targetfuture_spl1
        
        majority_target_future_spl1 <- sapply(confidence_target_future_spl1, function(x) {if (x >= threshold_target_future()) return (1) else return(0)}) 
        probabilities_target_future_spl1 <- rank_averaging(rda_target_future_spl1, svm_target_future_spl1, rf_target_future_spl1, n_RDA_target_future(), n_SVM_target_future(), n_RF_target_future())
        rank_target_future_spl1 <- sapply(probabilities_target_future_spl1, function(x) {if (x >= threshold_target_future()) return (1) else return(0)})
        
        #### **Split 2**
        len <- length(rda_target_future_spl2$Classes[,1])
        if (n_RDA_target_future() == 0)  {
            conf_RDA_targetfuture_spl2 <- rep(0, len)
        } else if (n_RDA_target_future() == 1 ){
            conf_RDA_targetfuture_spl2 <- rda_target_future_spl2$Classes[,1:n_RDA_target_future()]
        }else{
            conf_RDA_targetfuture_spl2 <- rowSums(rda_target_future_spl2$Classes[,1:n_RDA_target_future()])
        }
        if (n_RF_target_future() == 0)  {
            conf_RF_targetfuture_spl2 <- rep(0, len)
        } else if (n_RF_target_future() == 1){
            conf_RF_targetfuture_spl2 <- rf_target_future_spl2$Classes[,1:n_RF_target_future()]
        }else{
            conf_RF_targetfuture_spl2 <- rowSums(rf_target_future_spl2$Classes[,1:n_RF_target_future()])
        }
        if (n_SVM_target_future() == 0)  {
            conf_SVM_targetfuture_spl2 <- rep(0, len)
        } else if (n_SVM_target_future() == 1){
            conf_SVM_targetfuture_spl2 <- svm_target_future_spl2$Classes[,1:n_SVM_target_future()]
        }else{
            conf_SVM_targetfuture_spl2 <- rowSums(svm_target_future_spl2$Classes[,1:n_SVM_target_future()])
        }
        
        confidence_target_future_spl2 <- conf_RDA_targetfuture_spl2+ conf_RF_targetfuture_spl2+ conf_SVM_targetfuture_spl2
        
        majority_target_future_spl2 <- sapply(confidence_target_future_spl2, function(x) {if (x >= threshold_target_future()) return (1) else return(0)}) 
        probabilities_target_future_spl2 <- rank_averaging(rda_target_future_spl2, svm_target_future_spl2, rf_target_future_spl2, n_RDA_target_future(), n_SVM_target_future(), n_RF_target_future())
        rank_target_future_spl2 <- sapply(probabilities_target_future_spl2, function(x) {if (x >= threshold_target_future()) return (1) else return(0)})
        
        if (nhc() %in% nhc_spl1) {
            position_nhc <- which(nhc_spl1 %in% nhc())
            prob = probabilities_target_future_spl1[position_nhc] 
            prob_string = toString(base::round(prob,2))
            if (prob >= 0.7) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#e8152a"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #e8152a;">',prob_string,'</pre>'))
            else if (prob < 0.7 & prob >= threshold_target_future()) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#ece205"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #ece205;">',prob_string,'</pre>'))
            else HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#56b83e"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #56b83e;">',prob_string,'</pre>'))
        } else {
            position_nhc <- which(nhc_spl2 %in% nhc())
            prob = probabilities_target_future_spl2[position_nhc]
            prob_string = toString(base::round(prob,2))
            if (prob >= 0.7) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#e8152a"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #e8152a;">',prob_string,'</pre>'))
            else if (prob < 0.7 & prob >= threshold_target_future()) HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#ece205"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #ece205;">',prob_string,'</pre>'))
            else HTML('<svg viewBox="0 0 50 20"><circle cx="25" cy="10" r="7" stroke="black" stroke-width="0.5" fill="#56b83e"/></svg>',paste0('<pre style="font-size:3rem;width:100%;text-align:center;color: #56b83e;">',prob_string,'</pre>'))
        } 
    })
    
      output$table <- renderDataTable({
        validate(print_table(sensitivity_(), specificity_()))
        if (input$table_target == "0-1 months") {
            datatable(max_row1.10(), 
                      extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                      rownames = FALSE,
                      selection = 'single',
                      options = list(
                          searching = FALSE,
                          autoWidth = TRUE,
                          scroller = TRUE,
                          scrollX = FALSE,
                          scrollY = "200px",
                          columnDefs = list(list(visible=FALSE, targets=c(0,1,2,3,4)),list(className = 'dt-center', targets = c(5,6,7))),
                          info = FALSE,
                          class = 'cell-border stripe'
                      )
            ) %>% formatRound(c(6:8), 2)
        } else if (input$table_target == "1-3 months") {
            datatable(max_row6.10(), 
                      extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                      rownames = FALSE,
                      selection = 'single',
                      options = list(
                          searching = FALSE,
                          autoWidth = TRUE,
                          scroller = TRUE,
                          scrollX = FALSE,
                          scrollY = "200px",
                          columnDefs = list(list(visible=FALSE, targets=c(0,1,2,3,4)),list(className = 'dt-center', targets = c(5,6,7))),
                          info = FALSE,
                          class = 'cell-border stripe'
                      )
            ) %>% formatRound(c(6:8), 2)
        } else if (input$table_target == "3-6 months") {
            datatable(max_row12.10(), 
                      extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
                      rownames = FALSE,
                      selection = 'single',
                      options = list(
                          searching = FALSE,
                          autoWidth = TRUE,
                          scroller = TRUE,
                          scrollX = FALSE,
                          scrollY = "200px",
                          columnDefs = list(list(visible=FALSE, targets=c(0,1,2,3,4)),list(className = 'dt-center', targets = c(5,6,7))),
                          info = FALSE,
                          class = 'cell-border stripe'
                      )
            ) %>% formatRound(c(6:8), 2)
        } else if (input$table_target == "6-12 months") {
            datatable(max_rowfuture.10(), 
                      extensions = c("Scroller"), 
                      rownames = FALSE,
                      selection = 'single',
                      options = list(
                          searching = FALSE,
                          autoWidth = TRUE,
                          scroller = TRUE,
                          scrollX = FALSE,
                          scrollY = "200px",
                          columnDefs = list(list(visible=FALSE, targets=c(0,1,2,3,4)),list(className = 'dt-center', targets = c(5,6,7))),
                          info = FALSE,
                          class = 'cell-border stripe'
                      )
            ) %>% formatRound(c(6:8), 2)
        }
    })
    
    last_sens_spec = reactiveVal(c(9999,9999))
    tab_idx_t1 = reactiveVal(1)
    tab_idx_t6 = reactiveVal(1)
    tab_idx_t12 = reactiveVal(1)
    tab_idx_future = reactiveVal(1)
    
    observeEvent(c(input$table_rows_selected,sensitivity_(),specificity_()),{
        if(!is.na(sensitivity_()) && !is.na(specificity_())){
            if (!all(last_sens_spec() == c(sensitivity_(),specificity_()))){
                idx = 1
                last_sens_spec(c(sensitivity_(),specificity_()))
                tab_idx_t1(idx)
                tab_idx_t6(idx)
                tab_idx_t12(idx)
                tab_idx_future(idx)
            }
            else{
                if(is.null(input$table_rows_selected)){
                    idx = 1
                }
                else if (!is.null(input$table_rows_selected)){
                    idx = input$table_row_last_clicked
                }
                if(input$table_target == "0-1 months") tab_idx_t1(idx)
                else if(input$table_target == "1-3 months") tab_idx_t6(idx)
                else if(input$table_target == "3-6 months") tab_idx_t12(idx)
                else if (input$table_target == "6-12 months") tab_idx_future(idx)
            }
        }
    }, priority = -1, ignoreNULL = F)
    
    observeEvent(input$nhc_all,{
            toggle("res_target1", condition = (input$nhc_all == 'Patient ID'))
            toggle("res_target1_all",condition = (input$nhc_all == 'ALL'))
            toggle("res_target6", condition = (input$nhc_all == 'Patient ID'))
            toggle("res_target6_all",condition = (input$nhc_all == 'ALL'))
            toggle("res_target12", condition = (input$nhc_all == 'Patient ID'))
            toggle("res_target12_all",condition = (input$nhc_all == 'ALL'))
            toggle("res_targetfuture", condition = (input$nhc_all == 'Patient ID'))
            toggle("res_targetfuture_all",condition = (input$nhc_all == 'ALL'))
            toggle("pat_dist",condition = (input$nhc_all == 'ALL'))
            toggle("pat.risk", condition = (input$nhc_all == 'ALL'))
            toggle("help", condition = (input$nhc_all == 'Patient ID'))
    })
    
    # Target 1
    highrisk_target1 = reactiveVal(0)
    mediumrisk_target1 = reactiveVal(0)
    lowrisk_target1 = reactiveVal(0)
    # Target 6
    highrisk_target6 = reactiveVal(0)
    mediumrisk_target6 = reactiveVal(0)
    lowrisk_target6 = reactiveVal(0)
    # Target 12
    highrisk_target12 = reactiveVal(0)
    mediumrisk_target12 = reactiveVal(0)
    lowrisk_target12 = reactiveVal(0)
    # Target Future
    highrisk_targetfuture = reactiveVal(0)
    mediumrisk_targetfuture = reactiveVal(0)
    lowrisk_targetfuture = reactiveVal(0)
    
    output$pat_dist <- renderHighchart({
      if (! input$pat.risk) {
        aux_risk = c("High risk","Medium risk", "Low risk")
        aux_target = rep("0-1 months", times = 3)
        t_1 <- rbind(highrisk_target1(),mediumrisk_target1(),lowrisk_target1())
        t_1 = cbind(t_1,aux_risk,aux_target)
        aux_target = rep("1-3 months", times = 3)
        t_6 <- rbind(highrisk_target6(),mediumrisk_target6(),lowrisk_target6())
        t_6 = cbind(t_6,aux_risk,aux_target)
        aux_target = rep("3-6 months", times = 3)
        t_12 <- rbind(highrisk_target12(),mediumrisk_target12(),lowrisk_target12())
        t_12 = cbind(t_12,aux_risk,aux_target)
        aux_target = rep("6-12 months", times = 3)
        t_fut <- rbind(highrisk_targetfuture(),mediumrisk_targetfuture(),lowrisk_targetfuture())
        t_fut = cbind(t_fut,aux_risk,aux_target)
        all_r_graph <- as.data.frame(rbind(t_1,t_6,t_12,t_fut))
        colnames(all_r_graph) = c("Values","Risk","Time")
        all_r_graph$Values = as.numeric(all_r_graph$Values)
        all_r_graph <- within(all_r_graph, Risk <- factor(Risk, levels=c("High risk","Medium risk","Low risk")))
        all_r_graph$Percentage = base::round(all_r_graph$Values/total_patients*100, 2)
        hchart(all_r_graph, "column", hcaes(x = Time, y = Values, group = Risk),stacking = "normal",tooltip = list(pointFormat = "{point.Risk} : {point.Percentage} %")) %>%
          hc_title(text = "Risk Overview", style = list(fontSize = "24px"), align = "center") %>%
          hc_xAxis(title = list(text = "Period of time to suffer a heart failure",style = list(fontSize = "16px"))) %>%
          hc_yAxis(title = list(text = "Number of patients",style = list(fontSize = "16px")),max = round(total_patients,-2))  %>%
          hc_colors(c('#ff8882', '#ffb037','#91c788'))
      }
      else{
        hr_graph = rbind(highrisk_target1(),highrisk_target6(),highrisk_target12(),highrisk_targetfuture())
        delta_ts = c("0-1 months","1-3 months","3-6 months","6-12 months")
        hr_graph = cbind(hr_graph,delta_ts)
        colnames(hr_graph) = c("Values","Target")
        hr_graph = as.data.frame(hr_graph)
        hr_graph$Values = as.numeric(hr_graph$Values)
        hchart(hr_graph, "column", hcaes(x = Target, y = Values)) %>%
          hc_title(text = "High risk patients", style = list(fontSize = "24px"), align = "center") %>%
          hc_xAxis(title = list(text = "Period of time to suffer a heart failure",style = list(fontSize = "16px"))) %>%
          hc_yAxis(title = list(text = "Number of patients",style = list(fontSize = "16px"))) %>%
          hc_tooltip(enabled = F) %>%
          hc_colors(c("#ff8882"))
      }
    })
    
    observeEvent(input$help, {
      showModal(modalDialog(
        title = HTML('<h3 style="text-align:center;font-family: Lucida Bright,Georgia,serif"><b>Color Help</b></h3>'),
        HTML('<p style="color: #56b83e";>&#9679;<b> Low Risk</b></p> The probability of HF Descompensation is lower than the selected threshold.
              <p></p>
              <p style="color: #ecd40a";>&#9679;<b> Medium Risk</b></p> The probability of HF Descompensation is higher than the selected threshold but lower than 0.7.
              <p></p>
              <p style="color: #e8152a";>&#9679;<b> High Risk</b></p> The probability of HF Descompensation is higher than 0.7'),
        easyClose = TRUE,
        footer = NULL
      ))
    })

    ################################################################################
    
    al_tox <- read.csv("./Back_end/Alarms/alarms_CSVs/ICTUS_PATIENTOXIMETER_202104231033.csv")
    al_freq <- read.csv("./Back_end/Alarms/alarms_CSVs/ICTUS_PATIENTFREQCARDIO_202104231033.csv")
    al_wei <- read.csv("./Back_end/Alarms/alarms_CSVs/ICTUS_PATIENTWEIGHT_202104231033.csv")
    al_pres <- read.csv("./Back_end/Alarms/alarms_CSVs/ICTUS_PATIENTPRESSURE_202104231033.csv")
    
    names(al_tox)[names(al_tox) == 'DPO_VALUE'] <- 'Value'
    names(al_freq)[names(al_freq) == 'DPFC_VALUE'] <- 'Value'
    names(al_wei)[names(al_wei) == 'DPW_VALUE'] <- 'Value'
    names(al_pres)[names(al_pres) == 'DPRE_PULSE'] <- 'Value'
    
    
    al_react <- reactive({
        validate(nhc_ne(input$nhc_alarm))
        nhc  <- input$nhc_alarm
        al_tox <- prep.dd.al(al_tox,nhc)
        al_freq <- prep.dd.al(al_freq,nhc)
        al_wei <- prep.dd.al(al_wei,nhc)
        al_pres <- prep.dd.al(al_pres,nhc)
        ts_tox <- prep.ts(al_tox)
        ts_freq <- prep.ts(al_freq)
        ts_wei <- prep.ts(al_wei)
        ts_pres <- prep.ts(al_pres)
        return(list(ts_tox, ts_freq, ts_wei, ts_pres, al_tox, al_freq, al_wei, al_pres))
    })
    
    v_t <- eventReactive(input$num_alarm_ready, {
        validate(num_alarm_ne(input$num_alarm.t))
        input$num_alarm.t
    })
    
    v_f <- eventReactive(input$num_alarm_ready, {
        validate(num_alarm_ne(input$num_alarm.f))
        input$num_alarm.f
    })
    
    v_w <- eventReactive(input$num_alarm_ready, {
        validate(num_alarm_ne(input$num_alarm.w))
        input$num_alarm.w
    })
    
    v_p <- eventReactive(input$num_alarm_ready, {
        validate(num_alarm_ne(input$num_alarm.p))
        input$num_alarm.p
    })
    
    res.al_tox <- reactive(quote({alarm(al_react()[[1]], v_t())}), quoted = TRUE)
    res.al_freq <- reactive(quote({alarm(al_react()[[2]], v_f())}), quoted = TRUE)
    res.al_wei <- reactive(quote({alarm(al_react()[[3]], v_w())}), quoted = TRUE)
    res.al_pres <- reactive(quote({alarm(al_react()[[4]], v_p())}), quoted = TRUE)
    
    output$text_alarm <- renderPrint({
        print.alarms(res.al_tox()[[1]], res.al_freq()[[1]], res.al_wei()[[1]], res.al_pres()[[1]],
                     al_react()[[5]], al_react()[[6]], al_react()[[7]], al_react()[[8]],v_t(),v_f(),v_w(),v_p())
    })
    
    output$value_alarm.t <-renderPrint({
        if(is.null(res.al_tox()[[2]])) HTML('')
        HTML(res.al_tox()[[2]])
    })
    
    output$plot_alarm.t <- renderPlot({ 
        validate(alarm_ne(res.al_tox()[[1]]))
        outliers <- extract_all_outliers(al_react()[[1]],res.al_tox()[[1]],v_t())
        return(alarm_graphic(al_react()[[1]], outliers, v_t(), input$AO, input$TC, input$LS,1))
    })
    
    output$value_alarm.f <-renderPrint({
        if(is.null((res.al_freq()[[2]])))HTML('')
        HTML(res.al_freq()[[2]])
    })
    
    output$plot_alarm.f <- renderPlot({ 
        validate(alarm_ne(res.al_freq()[[1]]))
        outliers <- extract_all_outliers(al_react()[[2]],res.al_freq()[[1]],v_f())
        return(alarm_graphic(al_react()[[2]], outliers, v_f(), input$AO, input$TC, input$LS,2))
    })
    
    output$value_alarm.w <-renderPrint({
        if(is.null((res.al_wei()[[2]])))HTML('')
        HTML(res.al_wei()[[2]])
    })
    
    output$plot_alarm.w <- renderPlot({ 
        validate(alarm_ne(res.al_wei()[[1]]))
        outliers <- extract_all_outliers(al_react()[[3]],res.al_wei()[[1]],v_w())
        return(alarm_graphic(al_react()[[3]], outliers, v_w(), input$AO, input$TC, input$LS,3))
    })
    
    output$value_alarm.p <-renderPrint({
        if(is.null((res.al_pres()[[2]])))HTML('')
        HTML(res.al_pres()[[2]])
    })
    
    output$plot_alarm.p <- renderPlot({
        validate(alarm_ne(res.al_pres()[[1]]))
        outliers <- extract_all_outliers(al_react()[[4]],res.al_pres()[[1]],v_p())
        return(alarm_graphic(al_react()[[4]], outliers, v_p(), input$AO, input$TC, input$LS,4))
    })
    
    if (interactive()) {
        observeEvent(c(input$num_alarm_ready, input$nhc_alarm), {
            delay(20000, startAnim(session, 'value_alarm.t', 'flash'))
            delay(10000, startAnim(session, 'value_alarm.f', 'flash'))
            delay(1000, startAnim(session, 'value_alarm.w', 'flash'))
            delay(1000, startAnim(session, 'value_alarm.p', 'flash'))
        })
    }
}

# Run the application 
shinyApp(ui = ui, server = server)
