library(openssl)
library(data.table)

source('../misc/defs.R')
source('../misc/funciones.R')
source('csv2pdf.R')
source('parseCalendar.R')
source('csv2ics.R')


csvs <- dir(path = 'csv')
filenames <- gsub(pattern = "\\.csv$", "", csvs)
cursos <- gsub(pattern = "calendarioETSIDI_", "", filenames)

webdav <- '/var/www/webdav/cal'


leeCalendario <- function(curso = cursoActual)
{
    calETSIDI <- fread(paste0('csv/calendarioETSIDI_', curso, '.csv'),
                       na.strings = c("", "NA"))
    calETSIDI[, Inicio := as.Date(Inicio)]
    calETSIDI[, Final := as.Date(Final)]
    calETSIDI[is.na(Final), Final := Inicio]
    calETSIDI[, Tipo := factor(Tipo)]
    calETSIDI[, Formato := factor(Formato)]
    setkey(calETSIDI, Inicio)
}


makeCalPDF <- function(x, curso = cursoActual)
{
    formatos <- c('v', 'h')
    withProgress(message = "Actualizando PDFs...", 
    {
        lapply(formatos, function(formato)
        {
            calPDF(x, curso = curso, tipo = 'Grado', formato = formato, dest = 'pdf/')
            incProgress(1/6)
            calPDF(x, curso = curso, tipo = 'Master', formato = formato, dest = 'pdf/')
            incProgress(1/6)
            ## Genero un PDF con los dos calendarios para mostrarlo en el visor de PDFs
            pdfs <- paste0('Calendario_',
                           c('Grado_', 'Master_'),
                           curso,
                           "_", formato, ".pdf",
                           collapse = ' ')
            old <- setwd('pdf/')
            system2('pdftk', args = c(pdfs,
                                      'cat output',
                                      paste0('ETSIDI_', curso, '.pdf')))
            incProgress(1/6)
            setwd(old)
            })
    })
}


copyWeb <- function(curso = cursoActual, from = 'pdf', to = webdav)
{
    formatos <- c('v', 'h')
    ok <- lapply(formatos, function(formato)
    {

        pdfs <- paste0('Calendario_',
                       c('Grado_', 'Master_'),
                       curso,
                       "_", formato, ".pdf")
        file.copy(file.path(from, pdfs),
                  to,
                  overwrite = TRUE)
    })
    ## Si hay algún fallo, el resultado global será FALSE
    ok <- do.call(c, ok)
    all(ok)
}
