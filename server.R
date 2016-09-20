library(rhandsontable)
library(shiny)
library(shinyjs)

source('init.R')
addResourcePath("pdf", tempdir())

cal <- leeCalendario()
makeCalPDF(cal)

shinyServer(function(input,output,session){
    disable("update")

    output$table <- renderRHandsontable({
        cal[, Inicio := as.character(Inicio)]
        cal[, Final := as.character(Final)]
        
        hot <- rhandsontable(cal,
                             rowHeaders = NULL,
                             language = 'es')
        ## Las columnas no se editan ni ordenan
        hot <- hot_cols(hot,
                        allowColEdit = FALSE,
                        columnSorting = FALSE)
        hot
    })
    
    output$pdfViewer <- renderUI(
    {
        ## Añado enlace reactivo para que actualice contenido del
        ## iframe si actualizo tabla
        refresh <- input$table
        tags$iframe(style="height:600px; width:100%",
                    src=paste0("pdf/ETSIDI_", cursoActual, 
                               ".pdf#zoom=page-width"))
    })
    ## Refresco PDF
    observeEvent(input$table,
    {
        enable("update")
        df <- hot_to_r(input$table)
        try(makeCalPDF(df))
    })

    ## Grabo datos en csv
    observeEvent(input$update,
    {
        df <- hot_to_r(input$table)
        write.csv(df,
                  file = paste0('calendarioETSIDI_', cursoActual, '.csv'),
                  row.names = FALSE)
        info('Tabla modificada correctamente.')
    })
    
}) 

