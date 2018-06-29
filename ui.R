library(rhandsontable)
library(shiny)
library(shinyjs)

source('init.R')
addResourcePath("pdf", tempdir())


logoUPM <- "http://www.upm.es/sfs/Rectorado/Gabinete%20del%20Rector/Logos/UPM/EscPolitecnica/EscUpmPolit_p.gif"
logoETSIDI <- "http://www.upm.es/sfs/Rectorado/Gabinete%20del%20Rector/Logos/EUIT_Industrial/ETSI%20DISEN%C2%A6%C3%A2O%20INDUSTRIAL%20pqn%C2%A6%C3%A2.png"


## Cabecera con logos
header <- fluidRow(
    column(4, align = 'center', img(src = logoUPM)),
    column(4, align = 'center',
           h2("Calendario Académico"),
           h5("Subdirección de Ordenación Académica")),
    column(4, align = 'center', img(src = logoETSIDI))
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

