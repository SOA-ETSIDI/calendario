library(rhandsontable)
library(shiny)
library(shinyjs)

source('init.R')
addResourcePath("pdf", tempdir())



## Cabecera con logos
header <- fluidRow(
    column(4, align = 'center', img(src = logoUPM, width = 72)),
    column(4, align = 'center',
           h2("Calendario Académico"),
           h5("Subdirección de Ordenación Académica")),
    column(4, align = 'center', img(src = logoETSIDI, width = 72))
)

editor <- div(id = 'editor',
              fluidRow(
                  column(12,
                         selectInput('curso', "Curso: ",
                                     choices = cursos,
                                     selected = cursoActual))
              ),
              fluidRow(
                  column(12, 
                         rHandsontableOutput('table'))
              ),
              br(),
              fluidRow(
                  column(12,
                         actionButton("update",
                                      "Confirmar",
                                      icon = icon("check"))
                         )
              ))

gradoUI <- fluidRow(column(12,
                         htmlOutput("pdfGrado")
                         ))
masterUI <- fluidRow(column(12,
                         htmlOutput("pdfMaster")
                         ))
etsidiUI <- fluidRow(column(12,
                         htmlOutput("pdfETSIDI")
                         ))


## UI completa
shinyUI(
    fluidPage(
        useShinyjs(),  # Set up shinyjs
        includeCSS("styles.css"),
        header,
        fluidRow(
            column(12,
                   tabsetPanel(
                       tabPanel('Tabla', editor),
                       tabPanel('Grado', gradoUI),
                       tabPanel('Master', masterUI),
                       tabPanel('ETSIDI', etsidiUI)
                   ))
            )
    ))

