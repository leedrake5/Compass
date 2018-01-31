library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(data.table)




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
selectInput('traditionalmgcl', "Alternative MgCl Calculation", choices=c("Calculated", "Traditional", "All Methods Averaged"), selected="Traditional"),
numericInput('adjust', "Adjust MgCL", value=0),
sliderInput('percentadjust', label=NULL, min=0.5, max=2, value=1, step=0.01),
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









