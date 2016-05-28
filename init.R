library(data.table)

source('../aux/defs.R')
source('../aux/funciones.R')

leeCalendario <- function()
{
    calETSIDI <- fread('../data/calendarioETSIDI_2016_2017.csv',
                       na.strings = c("", "NA"))
    calETSIDI[, Dia := as.Date(Dia)]
    calETSIDI[, Final := as.Date(Final)]
    calETSIDI[, Tipo := factor(Tipo)]
    calETSIDI[, Formato := factor(Formato)]
    setkey(calETSIDI, Dia)
}

