library(rhandsontable)
library(shiny)
library(shinyjs)

source('init.R')

cal <- leeCalendario()

shinyServer(function(input,output,session){

    output$table <- renderRHandsontable({
        cal[, Dia := as.character(Dia)]
        cal[, Final := as.character(Dia)]
        
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
                    src=paste0("pdf/", "ETSIDI_2016_2017", 
                               ".pdf#zoom=page-width"))
    })
    ## Refresco PDF
    observeEvent(input$table,
    {
        df <- hot_to_r(input$table)
        calPDF(df)
    })

    ## Grabo datos en csv
    observeEvent(input$update,
    {
        df <- hot_to_r(input$table)
        write.csv(df, file = '/tmp/cal.csv', row.names = FALSE)
        info('Tabla modificada correctamente.')
    })
    
}) 

