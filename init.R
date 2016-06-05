library(data.table)

source('../misc/defs.R')
source('../misc/funciones.R')
source('csv2pdf.R')
cursoActual <- '2016_2017'

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
    calPDF(x, curso = curso, tipo = 'Grado')
    calPDF(x, curso = curso, tipo = 'Master')
    pdfs <- paste0('ETSIDI_',
                   c('Grado_', 'Master_'),
                   curso, '.pdf',
                   collapse = ' ')
    old <- setwd(tempdir())
    system2('pdftk', args = c(pdfs,
                              'cat output',
                              paste0('ETSIDI_', curso, '.pdf')))
    setwd(old)
}

