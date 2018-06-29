library(data.table)

source('../misc/defs.R')
source('../misc/funciones.R')
source('csv2pdf.R')

csvs <- dir(path = 'csv')
filenames <- gsub(pattern = "\\.csv$", "", csvs)
cursos <- gsub(pattern = "calendarioETSIDI_", "", filenames)

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

makeCalPDF <- function(x, curso = cursoActual, formato = 'v')
{
    calPDF(x, curso = curso, tipo = 'Grado', formato = formato, dest = 'pdf/')
    calPDF(x, curso = curso, tipo = 'Master', formato = formato, dest = 'pdf/')
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
    setwd(old)
}


