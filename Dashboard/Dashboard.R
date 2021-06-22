
# Load packages ----
library(shiny)
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
library(shinyWidgets)
library(png)
library(shinyanimate)
library(xts)
library(forecast)
library(openxlsx)
library(shinyjs)
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

source("./Back_end/economics.R")
source("./Back_end/variables.R")
source("./Back_end/health.R")
source("./Back_end/alarm_graphics.R")

dd_EQ5DL <- read.csv("./Back_end/CSVs/full_EQ5DL.csv")
list_nhc_EQ5DL <- dd_EQ5DL[,"NHC"]

dd_kansas <- read.csv("./Back_end/CSVs/full_KCCQ12.csv")
list_nhc_kansas <- dd_kansas[,"NHC"]

dateRangeInput2 <- function(inputId, label, minview = "months", maxview = "decades", ...) {
    d <- shiny::dateRangeInput(inputId, label, ...)
    d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
    d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
    d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
    d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
    d
}

# errors en l'interval de la data
date <- function(input, data_max, data_min) {
  if (input[1] > input[2]) print("Start date must be before end date")
  else if (as.character(input[2]) < data_min | as.character(input[1]) > data_max) print("Sorry, there is not data in this interval")
  else NULL
}

# Wellness dataframes not empty for a certain month
dd.kan.nhc.mes.ne <- function(dd, nhc, month, kansas) {
  if (nhc != "" && month != "All" && nrow(dd[dd$NHC == nhc & dd$mesos == month,]) == 0) print (paste0("Patient ID ", nhc, " has no data available for month ", month, sep = " "))
  else NULL
}

dd.eq.nhc.mes.ne <- function(dd, nhc, month) {
  if (nhc == "") print ("EQ5DL does not have an overview for all Patient ID, please select one")
  else if (month != "All" && nrow(dd[dd$NHC == nhc & dd$mesos == month,]) == 0) print (paste0("Patient ID ", nhc, " has no data available for month ", month, sep = " "))
  else NULL
}

ui <- tagList(
  tags$head(tags$script(type="text/javascript", src = "code.js")),
    navbarPage(title = list("Graphics", icon = icon("heartbeat", lib = "font-awesome")),
    tabPanel("Echonomics",
    icon = icon("donate", lib = "font-awesome"),
    fluidPage(
    fluidRow(
        titlePanel("Monthly evolution", br()),
        sidebarLayout(
            sidebarPanel(
                checkboxGroupInput(inputId = "Question",
                                   label = "Plots",
                                   choices = c("Hospital de dia" = "A", 
                                               "Hospitalitzacions" = "B", 
                                               "Urgències" = "C",
                                               "Visites" = "E"),
                                   selected = "E"),
                dateRangeInput2(inputId = "dates", label = "Interval", startview = "year", minview = "months", maxview = "decades", format = 'mm-yyyy', start = "2019-06-01", end = "2020-06-01"),
                tags$div(materialSwitch(inputId = "cost", label = "Frequency", inline = TRUE), tags$span("Cost"))
            ),
            mainPanel(plotOutput("lines"), br(), br())
        )
    ),
    
    fluidRow(
        titlePanel("Grouping by year"),
        mainPanel(
            fluidRow(
              column(11, align = "right",
                     prettyToggle(
                       inputId = "costs.stnd",
                       label_on = "Standarized", 
                       icon_on = icon("check"),
                       status_on = "success",
                       status_off = "danger", 
                       label_off = "Standarized",
                       icon_off = icon("remove")
            ))),
            highchartOutput('barres'),
            width = 12,
        ),
    )
    )
    ),
    
    
    tabPanel("Wellness",
    icon = icon("briefcase-medical", lib = "font-awesome"),         
    fluidPage(
    fluidRow(
        tabsetPanel(type = "tabs",
              tabPanel("Kansas",
                       #titlePanel("Evolució per mesos"),
                       plotOutput("fluvial_kansas"),
                       chooseSliderSkin("Flat"),
                       setSliderColor(c("#ffba6a", "#ffba6a"), c(1,2)),
                       tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                            background: #ffba6a;
                                            border-top: 1px solid #ffba6a ;
                                            border-bottom: 1px solid #ffba6a ;}
                                            .irs-from, .irs-to, .irs-single { background: #ffba6a }'
                       ))),
                       sidebarLayout(
                         sidebarPanel(
                           selectizeInput(
                             inputId = 'nhc_kansas',
                             label = h4('Patient ID'),
                             choices = list_nhc_kansas,
                             selected = NULL,
                             multiple = FALSE, # allow for multiple inputs
                             options = list(create = FALSE, placeholder = 'Please introduce a Patient ID.') # if TRUE, allows newly created inputs
                           ),
                           sliderTextInput(inputId = "mesos_kansas", 
                                           label = "Months:", 
                                           choices = c("All", 0, 1, 6, 12),
                                           selected = 0,
                                           grid = T, 
                                           animate = T),
                         ),
                         mainPanel(
                           girafeOutput("radarchart_kansas"),
                         ))),
                tabPanel("EQ5DL",
                         #titlePanel("Evolució per mesos"),
                         plotOutput("fluvial_EQ5DL"),
                         chooseSliderSkin("Flat", color = "#ffba6a"),
                         setSliderColor(c("#ffba6a", "#ffba6a"), c(1, 2)),
                         tags$head(tags$style(HTML('.js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {
                                              background: #ffba6a;
                                              border-top: 1px solid #ffba6a ;
                                              border-bottom: 1px solid #ffba6a ;}
                                              .irs-from, .irs-to, .irs-single { background: #ffba6a }'
                         ))),
                         sidebarLayout(
                           sidebarPanel(
                             selectizeInput(
                               inputId = 'nhc_EQ5DL',
                               label = h4('Patient ID'),
                               choices = list_nhc_EQ5DL,
                               selected = NULL,
                               multiple = FALSE, # allow for multiple inputs
                               options = list(create = FALSE, placeholder = 'Please introduce a Patient ID.') # if TRUE, allows newly created inputs
                             ),
                             sliderTextInput(inputId = "mesos_EQ5DL", 
                                             label = "Mesos:", 
                                             choices = c("All", 0, 1, 6, 12),
                                             selected = 0,
                                             grid = T, 
                                             animate = T),
                           ),
                           mainPanel(
                             girafeOutput("radarchart_EQ5DL"),
                          )))
        ),
        ),
    )
    ), 
    
    
    tabPanel("Variables",
    icon = icon("bar-chart", lib = "font-awesome"),
    fluidPage(
    fluidRow(
        sidebarLayout(
            sidebarPanel(
                selectInput(inputId = "lm",
                            label = "Variable",
                            choices = c("Cardiac frequency" = "A", 
                                             "IMC" = "B", 
                                             "FEVE" = "C", 
                                             "QRS width" = "D",
                                             "Hemoglobine" = "E",
                                             "Glomerular filtration" = "F",
                                             "Sistolic arterial pressure" = "G",
                                             "Diastolic arterial pressure" = "H"),
                             selected = "A"),
                     
                conditionalPanel(
                    condition = "input.lm == 'A'",
                    radioButtons(inputId = "beta",
                                 label = "Type",
                                 choices = c("All" = "A", 
                                             "Beta" = "B", 
                                             "No Beta" = "C"),
                                 selected = "A"))),
                    mainPanel(plotOutput("plot3")))))),
    
    tabPanel("NORA",
             icon = icon("mobile-alt", lib = "font-awesome"),
             fluidPage(
               fluidRow(column(10, offset = 1, h2("Patients with Multialarm"))),
               fluidRow(
                 column(11, align = "right",
                 prettyToggle(
                   inputId = "app.multial.stand",
                   label_on = "Standarized", 
                   icon_on = icon("check"),
                   status_on = "success",
                   status_off = "danger", 
                   label_off = "Standarized",
                   icon_off = icon("remove")
                 )),
                 fluidRow(plotOutput("app.multialarmes")),
                 
                 hr(),
                  
                 fluidRow(column(10, offset = 1, h2("Alarm Type"))),
                 fluidRow(plotOutput("app.tipus")),
                 
                 hr(),
                 fluidRow(column(10, offset = 1, h2("Wellness Comparison"))),
                 sidebarLayout(
                 sidebarPanel(
                     radioGroupButtons(
                       inputId = "rad.app.incl.type",
                       choices = c("Kansas", "EQ5DL"),
                       checkIcon = list(
                         yes = icon("ok", lib = "glyphicon"))),
                     prettyToggle(
                       inputId = "app.incl",
                       label_on = "App incl", 
                       icon_on = icon("check"),
                       status_on = "success",
                       status_off = "danger", 
                       label_off = "App incl",
                       icon_off = icon("remove")
                     )),
                   mainPanel(
                     girafeOutput("appincl")
                     )),
                 
    ))),
    
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

server <- function(input, output, session) {
    
    events <- read.csv('./Back_end/CSVs/events.csv')
    farmacia <- read.csv('./Back_end/CSVs/farmacia.csv')
    unified <- read.csv('./Back_end/CSVs/unified.csv')
    cursclinic <- read.csv('./Back_end/CSVs/cursclinic.csv')
    urgencies <- read.csv('./Back_end/CSVs/URGENCIES_202104060838.csv')
    hospitalitzacions <- read.csv('./Back_end/CSVs/HOSPITALITZACIONS_202105061742.csv')
    diagnostic <- read.csv('./Back_end/CSVs/diagnostic_ic.csv')
    
    events <- events.f(events)
    hdia <- agrupar(events, 'Requeriment de descongestió endovenosa a H. Dia', 300, 'hdia')
    ingres <- agrupar(events, c('Ingrés per Insuficiència Cardíaca', 'Ingrés per altra causa cardiològica'), c(450,500), 'ingres')
    hospitalitzacions <- hospitalitzacions.f(hospitalitzacions, diagnostic)
    urgencies <- urgencies.f(urgencies)
    farmacia <- farmacia.f(farmacia)
    hdia_cc <- hdia_cc.f(cursclinic)
    visites <- visites.f(unified)
    proves_tipus <- proves_tipus.f(unified)
    proves <- proves.f(proves_tipus)
    
    dataframes <- list(hdia_cc, hospitalitzacions, urgencies, proves, visites, farmacia)
    names <- c('hdia_cc', 'hospitalitzacions', 'urgencies', 'proves', 'visites', 'farmacia')
    dd_linies <- linies (dataframes, names)

    data_max <- max(dd_linies$data)
    data_min <- min(dd_linies$data)
    
    dataInput <- reactive({
        validate(date(input$dates, data_max, data_min))
        input$dates
    })
    
    
    output$lines <- renderPlot({
        choice <- paste(input$Question, collapse = ", ")
        economic_lin(dd_linies, dataInput(), choice, input$cost)
    })
    
    
    # gràfic de barres + la seva interacció 
    
    costs <- rbind (hdia_cc, hospitalitzacions, urgencies, visites)
    costs <- costs.f(costs)
      
    
    output$barres <- renderHighchart({
      if (input$costs.stnd) economic_bar_stnd(costs)
      else economic_bar(costs)
    })
    
    makeReactiveBinding("output_tipus")
    output_tipus <- ''
    
    observeEvent(input$clicked, {
        output_tipus <<- input$clicked
    })
    
    output$proves <- renderHighchart({
        if (output_tipus == "proves") {
            proves_tipus.plot(proves_tipus)
        }
    })
    
    
    ################################################################################
    
    dd_EQ5DL <- dd_EQ5DL.f(dd_EQ5DL) 
    dd_kansas <- dd_kansas.f(dd_kansas)
    
    output$fluvial_EQ5DL <- renderPlot({
        fluv_EQ5DL(dd_EQ5DL, input$nhc_EQ5DL)
    })
    
    output$radarchart_EQ5DL <- renderGirafe({
      validate(dd.eq.nhc.mes.ne(dd_EQ5DL, input$nhc_EQ5DL, input$mesos_EQ5DL))
      radar_graph_individual(dd_EQ5DL, input$nhc_EQ5DL, input$mesos_EQ5DL, "Radar Chart", F)
    })
    
    output$fluvial_kansas <- renderPlot({
      fluv_kansas(dd_kansas, input$nhc_kansas)
    })
    
    output$radarchart_kansas <- renderGirafe({
      validate(dd.kan.nhc.mes.ne(dd_kansas, input$nhc_kansas, input$mesos_kansas))
      radar_graph_individual(dd_kansas, input$nhc_kansas, input$mesos_kansas, "Radar Chart", T)
    })
    
    ################################################################################
    file <- read.csv("./Back_end/CSVs/unified.csv")
    
    output$plot3 <- renderPlot({
      if (input$lm == "A") {
        if (input$beta == "A") freq_card(file)
        else if (input$beta == "B") freq_card_b(file)
        else if (input$beta == "C") freq_card_nb(file)
      }
      else if (input$lm == "B") imc(file)
      else if (input$lm == "C") feve(file)
      else if (input$lm == "D") qrs(file)
      else if (input$lm == "E") hemoglobine(file)
      else if (input$lm == "F") glom_filtr(file)
      else if (input$lm == "G") sap(file)
      else if (input$lm == "H") dap(file)
    })
    
    
    ################################################################################
    
    alarmes <- read.csv("./Back_end/CSVs/ICTUS_CONSTANTALARM_202104231033.csv")
    personas <- read.csv("./Back_end/CSVs/ICTUS_PATIENTWEIGHT_202104231033.csv")
    
    output$app.multialarmes <- renderPlot({
      if(input$app.multial.stand) gr_multialarmes_standard(alarmes, personas)
      else gr_multialarmes(alarmes, personas)
    })
    
    output$app.tipus <- renderPlot({
      gr_alarmes_tipus(alarmes)
    })
    
    output$appincl <- renderGirafe ({
      if (input$app.incl) app <- 1
      else app <- 0
      
      if(input$rad.app.incl.type == 'EQ5DL') radar_graph_app(dd_EQ5DL, app, "Radar Chart", F)
      else radar_graph_app(dd_kansas, app, "Radar Chart", T)
    })
    
}

# Run the application 
shinyApp(ui, server)
