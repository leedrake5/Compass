library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(data.table)
library(shinyWidgets)




options(warn=-1)
assign("last.warning", NULL, envir = baseenv())


shinyUI(navbarPage("Compass Minerals", id="nav", theme = shinytheme("readable"),


tabPanel("MgCl",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(

textInput('projectname', label="Project Name", value="MgClData"),

fileInput('loadvaldata', 'Load Data', multiple=TRUE,
accept=c('text/csv',
'text/comma-separated-values,text/plain',
'.csv')),

tags$hr(),


dropdownButton(
tags$h3("Manual Changes"), icon = icon("gear"),
actionButton('savedefaults', "Save"),
selectInput('traditionalmgcl', "Adjust MgCL", choices=c("Calculated", "Traditional", "All Methods Averaged"), selected=as.character(memory$traditionalmgcl)),
numericInput('adjustmgcl', label=NULL, value=as.numeric(memory$adjustmgcl)),
sliderInput('percentadjustmgcl', label=NULL, min=0.5, max=2, value=as.numeric(memory$percentadjustmgcl), step=0.01),
numericInput('adjustso4', "Adjust SO4", value=as.numeric(memory$adjustso4)),
sliderInput('percentadjustso4', label=NULL, min=0.5, max=2, value=as.numeric(memory$percentadjustso4), step=0.01),
actionButton('restoredefaults', "Restore Defaults"),
actionButton('resetdefaults', "Reset Memory"),
tooltip = tooltipOptions(title = "Click for manual options")
),


checkboxInput('manualoverride', "Manually Choose Calibration", value=FALSE),
uiOutput('selectcal'),
checkboxInput('manualproduct', "Manually Choose Product", value=FALSE),
uiOutput('selectproduct'),
uiOutput('nameproduct'),
uiOutput('mgclmanual'),
uiOutput('so4manual'),


downloadButton('downloadValData', "Results")

),

mainPanel(
tags$style(type="text/css",
".shiny-output-error { visibility: hidden; }",
".shiny-output-error:before { visibility: hidden; }"),
tabsetPanel(
id = 'dataset2',
tabPanel('Pass/Fail', dataTableOutput('qualitytable')),
tabPanel('Molecules', dataTableOutput('moleculetable')),
tabPanel('Elements', dataTableOutput('myvaltable2'))


))))))))









