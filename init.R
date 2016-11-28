library(data.table)

source('../misc/defs.R')
source('../misc/funciones.R')
source('csv2pdf.R')
cursoActual <- '2016-2017'

leeCalendario <- function(curso = cursoActual)
{
    calETSIDI <- fread(paste0('calendarioETSIDI_', curso, '.csv'),
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
    calPDF(x, curso = curso, tipo = 'Grado', formato = '1p')
    calPDF(x, curso = curso, tipo = 'Master', formato = '1p')
    ## Genero un PDF con los dos calendarios para mostrarlo en el visor de PDFs
    pdfs <- paste0('Calendario_',
                   c('Grado_', 'Master_'),
                   sub('-', '_', curso),
                   "_1p.pdf",
                   collapse = ' ')
    old <- setwd(tempdir())
    system2('pdftk', args = c(pdfs,
                              'cat output',
                              paste0('ETSIDI_', curso, '.pdf')))
    setwd(old)
}


