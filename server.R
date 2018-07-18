library(rhandsontable)
library(shiny)
library(shinyjs)

source('init.R')
addResourcePath("pdf", "pdf")


shinyServer(function(input,output,session){

    disable("update")

    output$table <- renderRHandsontable({
        ## Lee calendario
        cal <- leeCalendario(input$curso)

        ## Genero por primera vez PDFs si fuese necesario
        pdfCurso <- list.files('pdf', pattern = input$curso)
        if (length(pdfCurso) == 0)
            makeCalPDF(cal, input$curso)

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
    
    output$pdfGrado <- renderUI(
    {
        ## Añado enlace reactivo para que actualice contenido del
        ## iframe si actualizo tabla
        refresh <- input$table
        tags$iframe(style="height:600px; width:100%",
                    src=paste0("pdf/Calendario_Grado_",
                               input$curso, '_v',
                               ".pdf#zoom=page-width"))
    })

    output$pdfMaster <- renderUI(
    {
        ## Añado enlace reactivo para que actualice contenido del
        ## iframe si actualizo tabla
        refresh <- input$table
        tags$iframe(style="height:600px; width:100%",
                    src=paste0("pdf/Calendario_Master_",
                               input$curso, '_v', 
                               ".pdf#zoom=page-width"))
    })

    output$pdfETSIDI <- renderUI(
    {
        ## Añado enlace reactivo para que actualice contenido del
        ## iframe si actualizo tabla
        refresh <- input$table
        tags$iframe(style="height:600px; width:100%",
                    src=paste0("pdf/ETSIDI_", input$curso, 
                               ".pdf#zoom=page-width"))
    })

    ## Refresco PDF
    observeEvent(input$table,
    {
        enable("update")
        df <- hot_to_r(input$table)
        try(makeCalPDF(df, input$curso))
    })

    ## Grabo datos en csv
    observeEvent(input$update,
    {
        df <- hot_to_r(input$table)
        write.csv(df,
                  file = paste0('csv/calendarioETSIDI_',
                                input$curso,
                                '.csv'),
                  row.names = FALSE)
        info('Tabla modificada correctamente.')
    })

    ## Publico PDFs y iCalendar en web
    observeEvent(input$publish,
    {
        ## Vuelca en webdav
        okPDF <- copyWeb(input$curso)
        if (okPDF)
            ## Mensaje para usuario si nada falla
            info('Calendario PDF publicado.')
        else
            info('Error al publicar PDF.')
        makeICS(input$curso, dest = webdav)
        info('Calendario ICS publicado.')
    })
}) 

