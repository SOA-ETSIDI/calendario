source('../aux/funciones.R')

## Es x un día lectivo?
lectivo <- function(x, festivos){
    ## Es de lunes a viernes?
    isWD <- function(x){
        format(x, '%u') %in% as.character(1:5)
    }
    ## Es laborable?
    isLab <- function(x){
        !(x %in% festivos) 
    }
    ## Por tanto, es lectivo?
    x[isWD(x) & isLab(x)]
}

## Calendario exportado de calendarioETSIDI.org
calETSIDI <- read.csv('../data/calendarioETSIDI.csv',
                stringsAsFactors = FALSE,
                na.strings = "")
calETSIDI$Dia <- as.Date(calETSIDI$Dia)
calETSIDI$Final <- as.Date(calETSIDI$Final)

## Días festivos
festivos <- calETSIDI[!calETSIDI$Lectivo, ]
## Periodos vacacionales (Final no es NA)
festPer <- apply(festivos[!is.na(festivos$Final),], 1,
                 function(x) seq(as.Date(x[1]),
                                 as.Date(x[2]),
                                 by = 'days'))
festPer <- do.call(c, festPer)
## Todo junto
festivos <- c(festivos[is.na(festivos$Final), "Dia"],
              festPer)

## Días lectivos, primer y segundo semestre
## GRADO
S1g <- calETSIDI[calETSIDI$Descripcion == "Primer semestre (Grado)",]
S1g <- with(S1g, seq(Dia, Final, by = 'days'))
S1g <- lectivo(S1g, festivos)
S2g <- calETSIDI[calETSIDI$Descripcion == "Segundo semestre (Grado)",]
S2g <- with(S2g, seq(Dia, Final, by = 'days'))
S2g <- lectivo(S2g, festivos)
lDaysG <- c(S1g, S2g)

S1m <- calETSIDI[calETSIDI$Descripcion == "Primer semestre (Máster)",]
S1m <- with(S1m, seq(Dia, Final, by = 'days'))
S1m <- lectivo(S1m, festivos)
S2m <- calETSIDI[calETSIDI$Descripcion == "Segundo semestre (Máster)",]
S2m <- with(S2m, seq(Dia, Final, by = 'days'))
S2m <- lectivo(S2m, festivos)
lDaysM <- c(S1m, S2m)

## Aplicamos los cambios por días especiales
idxCambio <- grep("Horario de", calETSIDI$Descripcion)
dCambios <- calETSIDI[idxCambio, "Dia"]
cambios <- sub("Horario de ", "", calETSIDI[idxCambio, "Descripcion"])
## Grado: día de la semana original (sin cambios)
wd0G <- format(lDaysG, '%A')
idxG <- match(dCambios, lDaysG)
## wd es el día de la semana con cambios
wdG <- wd0G
wdG[idxG] <- cambios

## Máster: día de la semana original (sin cambios)
wd0M <- format(lDaysM, '%A')
idxM <- match(dCambios, lDaysM)
## wd es el día de la semana con cambios
wdM <- wd0M
wdM[idxM] <- cambios

## El resultado es un data.frame donde wd0 es el día de la semana
## "original", y Dia es el día de la semana que le corresponde por el
## cambio
calendarG <- data.frame(
    d = lDaysG,
    wd0 = wd0G,
    Dia = titlecase(wdG),
    Semestre = ifelse(lDaysG %in% S1g, 1, 2),
    stringsAsFactors = FALSE
)

calendarM <- data.frame(
    d = lDaysM,
    wd0 = wd0M,
    Dia = titlecase(wdM),
    Semestre = ifelse(lDaysM %in% S1g, 1, 2),
    stringsAsFactors = FALSE
)
