library(rhandsontable)
library(shiny)
library(shinyjs)

## source('csv2tt.R')
cal <- leeCalendario()

shinyServer(function(input,output,session){

    output$table <- renderRHandsontable({
        hot <- rhandsontable(cal,
                             rowHeaders = NULL,
                             language = 'es')
        ## Las columnas no se editan ni ordenar
        hot <- hot_cols(hot,
                        allowColEdit = FALSE,
                        columnSorting = FALSE)
        hot <- hot_col(hot, col = "Dia",
                       language = "es",
                       dateFormat = "MM/DD/YYYY")
        hot <- hot_col(hot, col = "Final",
                       language = "es",
                       dateFormat = "MM/DD/YYYY")

        hot
    })
    
    output$pdfViewer <- renderUI(
    {
        ## Añado enlace reactivo para que actualice contenido del
        ## iframe si aprieto botón "refresh"
        refresh <- input$refresh
        tags$iframe(style="height:600px; width:100%",
                    src=paste0("pdfs/", input$grupo,
                               "_", semestre,
                               ".pdf#zoom=page-width"))
    })
    ## Refresco PDF
    observeEvent(input$refresh,
    {
        ## Leo tabla, y grupo y semestre (no incluidos en tabla)
        df <- values$data
        ## csv2tt(df, grupo, semestre,
        ##        dest = '../data/horarios/pdfs/')
        ## }
    })
    ## Grabo datos en csv
    observeEvent(input$update,
    {
        df <- hot_to_r(input$table)
        write.csv(df, file = '/tmp/cal.csv', row.names = FALSE)
        info('Tabla modificada correctamente.')
    })
    
}) 

