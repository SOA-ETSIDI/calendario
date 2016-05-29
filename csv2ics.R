library(openssl)
library(data.table)

semestre <- 2

source('../misc/defs.R')
source('../misc/funciones.R')
source('calendar.R')

## Leo los horarios de todos los grupos
## Horarios con aulas
horariosPath <- '../horarios/csv/'
files <- dir(horariosPath, pattern = '.csv')
horarios <- rbindlist(lapply(paste0(horariosPath, files),
                             fread, encoding = 'UTF-8'))
## Filtro por semestre
horarios <- horarios[Semestre %in% (semestre * 1:4),
                     .(Asignatura,
                       Tipo = tipo,
                       Grupo, Titulacion,
                       Dia = factor(Dia, dias),
                       HoraInicio, HoraFinal,
                       Semestre)]
## Horario de grados
horG <- horarios[Titulacion %in% grados]
## Horas Tuthoras
tuthora1 <- horG[, .(Asignatura = "Hora Tuthora", Tipo = "Tutoria",
                    Dia = dias,
                    HoraInicio = "11:15", HoraFinal = "11:45",
                    Titulacion = unique(Titulacion),
                    Semestre = unique(Semestre)),
                by = Grupo]
tuthora2 <- horG[, .(Asignatura = "Hora Tuthora", Tipo = "Tutoria",
                    Dia = dias,
                    HoraInicio = "17:15", HoraFinal = "17:45",
                    Titulacion = unique(Titulacion),
                    Semestre = unique(Semestre)),
                 by = Grupo]
horG <- rbind(horG, tuthora1, tuthora2)
setkey(horG, Dia)
## Horarios de másteres
horM <- horarios[!(Titulacion %in% grados)]
setkey(horM, Dia)

## Calendario generado con calendar.R
calG <- as.data.table(calendarG)
## Ordeno por días y filtro por semestre
calG <- calG[Semestre == semestre,
            .(Dia = factor(Dia, dias),
              d)]
setkey(calG, Dia)

calM <- as.data.table(calendarM)
calM <- calM[Semestre == semestre,
            .(Dia = factor(Dia, dias),
              d)]
setkey(calM, Dia)


## Hago un join de horarios con calendario usando allow.cartesian
jHorG <- horG[calG, allow.cartesian = TRUE]
## Los viernes no hay clase en algunos Másteres
jHorM <- horM[calM, allow.cartesian = TRUE, nomatch = 0]
jHorarios <- rbind(jHorG, jHorM)
## Añado una columna con el UUID que corresponde a cada franja.
## Es importante NO cambiar este UUID aunque cambie algún detalle de los eventos.
## Este UUID es único para cada asignatura, grupo, tipo y *día del año*
jHorarios[, UUID := paste0(md5(paste0(Asignatura, Tipo, Grupo,
                                  d, HoraInicio)),
                       '@subdireccion.oa.etsidi')]

## Escribe los calendarios ICS por grupo
jHorarios[,
{
    ics <- makeCalendar(titulo = Asignatura,
                        descripcion = Tipo,
                        inicio = as.POSIXct(paste(d, HoraInicio),
                                            tz = 'CET'),
                        final = as.POSIXct(paste(d, HoraFinal),
                                           tz = 'CET'),
                        UUID = UUID,
                        calname = Grupo)
    writeLines(ics, paste0('/tmp/', Grupo, '.ics'))
},
by = Grupo]


## Calendario general de la ETSIDI
events <- calETSIDI$Descripcion
inicio <- as.Date(calETSIDI$Dia)
final <- as.Date(calETSIDI$Final)
idxNA <- is.na(final)

## Eventos de 1 sólo día
oneDay <- data.frame(titulo = events[idxNA],
                     inicio = inicio[idxNA],
                     ## The "DTEND" property for a "VEVENT" calendar component specifies
                     ## the non-inclusive end of the event
                     final = inicio[idxNA] + 1)
## Eventos que ocupan un período.
## Día de comienzo
iDays <- data.frame(titulo = paste('Inicio', events[!idxNA]),
                    inicio = inicio[!idxNA],
                    final = inicio[!idxNA] + 1)
## Dia de final
fDays <- data.frame(titulo = paste('Final', events[!idxNA]),
                    inicio = final[!idxNA],
                    final = final[!idxNA] + 1)
## All together now
ETSIDI <- rbind(oneDay, iDays, fDays)

ETSIDI$id <- with(ETSIDI,
                  paste0(md5(paste0(events, inicio, final),
                             '@subdireccion.oa.etsidi')))

icsETSIDI <- with(ETSIDI,
                  makeCalendar(titulo = titulo,
                               descripcion = "",
                               inicio = inicio,
                               final = final,
                               UUID = id,
                               calname = "ETSIDI"))

writeLines(icsETSIDI, '/tmp/ETSIDI.ics')
