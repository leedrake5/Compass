library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(data.table)
library(dtplyr)
library(rhandsontable)
library(Cairo)




options(warn=-1)
assign("last.warning", NULL, envir = baseenv())


shinyUI(navbarPage("Compass Minerals", id="nav", theme = shinytheme("readable"),


tabPanel("MgCl",
div(class="outer",

fluidRow(
sidebarLayout(
sidebarPanel(


fileInput('loadvaldata', 'Load Data', multiple=TRUE,
accept=c('text/csv',
'text/comma-separated-values,text/plain',
'.csv')),

tags$hr(),

checkboxInput('manualoverride', "Manually Choose Calibration", value=FALSE),
uiOutput('selectcal'),

downloadButton('downloadValData', "Results")

),

mainPanel(
tabsetPanel(
id = 'dataset2',
tabPanel('Validation', dataTableOutput('myvaltable2')),
tabPanel('Molecules', dataTableOutput('moleculetable'))


))))))))









