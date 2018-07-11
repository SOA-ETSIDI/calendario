library(openssl)
library(data.table)

source('../misc/defs.R')
source('../misc/funciones.R')
source('parseCalendar.R')

## Leo los horarios de todos los grupos
## Horarios con aulas
horariosPath <- '../horarios/csv/'
files <- dir(horariosPath, pattern = '.csv$')
horarios <- rbindlist(lapply(paste0(horariosPath, files),
                             fread,
                             na.string = "", 
                             encoding = 'UTF-8'),
                      fill = TRUE)
## Filtro por semestre
horarios <- horarios[,
                     .(Asignatura,
                       Tipo,
                       Grupo, Titulacion,
                       Dia = factor(Dia, dias),
                       HoraInicio, HoraFinal,
                       Semestre)]
## Horario de grados
horG <- horarios[Titulacion %in% grados]
## Horas Tuthoras
tuthora1 <- horG[, .(Asignatura = "Hora Tuthora", Tipo = "Tutoria",
                    Dia = dias,
                    HoraInicio = tuthoraM[1],
                    HoraFinal = tuthoraM[2]),
                 by = .(Grupo, Titulacion, Semestre)]

tuthora2 <- horG[, .(Asignatura = "Hora Tuthora", Tipo = "Tutoria",
                    Dia = dias,
                    HoraInicio = tuthoraT[1],
                    HoraFinal = tuthoraT[2]),
                 by = .(Grupo, Titulacion, Semestre)]

horG <- rbind(horG, tuthora1, tuthora2)
setkey(horG, Semestre, Dia)
## Horarios de másteres
horM <- horarios[!(Titulacion %in% grados)]
setkey(horM, Semestre, Dia)

## Calendario generado con parseCalendar.R
calG <- as.data.table(calendarG)
## Ordeno por días y semestre
calG <- calG[,
            .(Dia = factor(Dia, dias),
              d,
              Semestre)]
setkey(calG, Semestre, Dia)

calM <- as.data.table(calendarM)
calM <- calM[,
            .(Dia = factor(Dia, dias),
              d,
              Semestre)]
setkey(calM, Semestre, Dia)


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

## Eventos de 1 sólo día
oneDay <- calETSIDI[isOneDay(Inicio, Final),
                    .(titulo = Descripcion,
                      inicio = Inicio,
                      final = Inicio + 1,
                      Tipo)
                    ## The "DTEND" property for a "VEVENT" calendar component specifies the non-inclusive end of the event
                    ]
## Eventos que ocupan un período.
perDay <- calETSIDI[!isOneDay(Inicio, Final)]
## Día de comienzo
iDays <- perDay[,
                .(titulo = paste('Inicio de', Descripcion),
                  inicio = Inicio,
                  final = Inicio + 1,
                  Tipo)
                ]
## Dia de final
fDays <- perDay[,
                .(titulo = paste('Fin de', Descripcion),
                  inicio = Final,
                  final = Final + 1,
                  Tipo)
                ]
## All together now
ETSIDI <- rbind(oneDay, iDays, fDays)
ETSIDIGrado <- ETSIDI[Tipo %in% c('ETSIDI', 'Grado')]
ETSIDIMaster <- ETSIDI[Tipo %in% c('ETSIDI', 'Master')]

ETSIDIGrado[,
             id := paste0(md5(paste0(titulo, inicio, final),
                              '@subdireccion.oa.etsidi'))
            ]
ETSIDIMaster[,
             id := paste0(md5(paste0(titulo, inicio, final),
                               '@subdireccion.oa.etsidi'))
             ]

icsETSIDIGrado <- ETSIDIGrado[,
                              makeCalendar(titulo = titulo,
                                           descripcion = "",
                                           inicio = inicio,
                                           final = final,
                                           UUID = id,
                                           calname = "ETSIDI_Grado")
                              ]
writeLines(icsETSIDIGrado, '/tmp/ETSIDI_Grado.ics')

icsETSIDIMaster <- ETSIDIMaster[,
                              makeCalendar(titulo = titulo,
                                           descripcion = "",
                                           inicio = inicio,
                                           final = final,
                                           UUID = id,
                                           calname = "ETSIDI_Master")
                              ]

writeLines(icsETSIDIMaster, '/tmp/ETSIDI_Master.ics')

