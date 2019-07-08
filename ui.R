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

uiCurso <- div(id = 'eligeCurso',
              fluidRow(
                  column(12, align = "center",
                         selectInput('curso', "Curso: ",
                                     choices = cursos,
                                     selected = cursoActual)
                         )),
              fluidRow(
                  column(12, align = "center",
                         actionButton('okCurso', "Aceptar",
                                      icon = icon("check"))
                         ))
              )

editor <- div(id = 'editor',
              fluidRow(
                  column(3,
                         actionButton("update",
                                      "Confirmar",
                                      icon = icon("check"))
                         ),
                  column(3,
                         actionButton("publish",
                                      "Publicar",
                                      icon = icon("check"))
                         )),
              br(),
              fluidRow(
                  column(12, 
                         rHandsontableOutput('table'))
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

tabsUI <- div(id = "tabs",
              fluidRow(column(12,
                              tabsetPanel(
                                  tabPanel('Tabla', editor),
                                  tabPanel('Grado', gradoUI),
                                  tabPanel('Master', masterUI),
                                  tabPanel('ETSIDI', etsidiUI)
                              )
                              )
                       )
              )


## UI completa
shinyUI(
    fluidPage(
        useShinyjs(),  # Set up shinyjs
        includeCSS("styles.css"),
        header,
        uiCurso,
        hidden(tabsUI)
    )
    )
