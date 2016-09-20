library(data.table)

source('../misc/funciones.R')

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

calETSIDI <- fread('calendarioETSIDI_2016-2017.csv',
                na.strings = "")
calETSIDI[, Inicio := as.Date(Inicio)]
calETSIDI[, Final := as.Date(Final)]

## Días festivos
festivos <- calETSIDI[Formato == 'fiesta']
## Días sueltos
festDay <- festivos[isOneDay(Inicio, Final)]
## Periodos vacacionales
festPer <- festivos[!isOneDay(Inicio, Final)]
festPer <- with(festPer,
                mapply(seq, Inicio, Final,
                       MoreArgs = list(by = 'days'))
                )
festPer <- do.call(c, festPer)
## Todo junto
festivos <- c(festDay$Inicio, festPer)

## Días lectivos, primer y segundo semestre
## GRADO
calGrado <- calETSIDI[Tipo %in% c("ETSIDI", "Grado")]
docenciaG <- calGrado[Descripcion == "Docencia"]
docenciaG <- with(docenciaG,
                  mapply(seq, Inicio, Final,
                         MoreArgs = list(by = 'days'))
                  )
docenciaG <- do.call(c, docenciaG)
lDaysG <- lectivo(docenciaG, festivos)

S1g <- calGrado[Descripcion == "Primer semestre"]
S1g <- S1g[, seq(Inicio, Final, by = 'days')]
S2g <- calGrado[Descripcion == "Segundo semestre"]
S2g <- S2g[, seq(Inicio, Final, by = 'days')]

calMaster <- calETSIDI[Tipo %in% c("ETSIDI", "Master")]
docenciaM <- calMaster[Descripcion == "Docencia"]
docenciaM <- with(docenciaM,
                  mapply(seq, Inicio, Final,
                         MoreArgs = list(by = 'days'))
                  )
docenciaM <- do.call(c, docenciaM)
lDaysM <- lectivo(docenciaM, festivos)

S1m <- calMaster[Descripcion == "Primer semestre"]
S1m <- S1m[, seq(Inicio, Final, by = 'days')]
S2m <- calMaster[Descripcion == "Segundo semestre"]
S2m <- S2m[, seq(Inicio, Final, by = 'days')]

## Aplicamos los cambios por días especiales
idxCambio <- grep("Horario de", calETSIDI$Descripcion)
dCambios <- calETSIDI[idxCambio, Inicio]
cambios <- sub("Horario de ", "", calETSIDI[idxCambio, Descripcion])
## Grado: día de la semana original (sin cambios)
wd0G <- format(lDaysG, '%A')
idxG <- match(dCambios, lDaysG)
## wd es el día de la semana con cambios
wdG <- wd0G
wdG[idxG] <- cambios[which(dCambios %in% lDaysM)]

## Máster: día de la semana original (sin cambios)
wd0M <- format(lDaysM, '%A')
## Dado que el calendario de Máster acaba antes, algunos días especiales no entran en sus lectivos
idxM <- match(dCambios, lDaysM, nomatch = 0)
## wd es el día de la semana con cambios
wdM <- wd0M
wdM[idxM] <- cambios[which(dCambios %in% lDaysM)]

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
    Semestre = ifelse(lDaysM %in% S1m, 1, 2),
    stringsAsFactors = FALSE
)
