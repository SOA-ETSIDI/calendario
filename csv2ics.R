makeICS <- function(curso, dest = tempdir())
{
    cal <- leeCalendario(curso)
    calParsed <- parseCalendar(cal)
    ## Grado
    calG <- as.data.table(calParsed$grado)
    ## Ordeno por días y semestre
    calG <- calG[,
                 .(Dia = factor(Dia, dias),
                   d,
                   Semestre)]
    setkey(calG, Semestre, Dia)

    ## Master
    calM <- as.data.table(calParsed$master)
    calM <- calM[,
                 .(Dia = factor(Dia, dias),
                   d,
                   Semestre)]
    setkey(calM, Semestre, Dia)

    ## Eventos de 1 sólo día
    oneDay <- cal[isOneDay(Inicio, Final),
                  .(titulo = Descripcion,
                    inicio = Inicio,
                    final = Inicio + 1,
                    Tipo)
                  ## The "DTEND" property for a "VEVENT" calendar component specifies the non-inclusive end of the event
                  ]
    ## Eventos que ocupan un período.
    perDay <- cal[!isOneDay(Inicio, Final)]
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
    writeLines(icsETSIDIGrado, file.path(dest, 'ETSIDI_Grado.ics'))

    icsETSIDIMaster <- ETSIDIMaster[,
                                    makeCalendar(titulo = titulo,
                                                 descripcion = "",
                                                 inicio = inicio,
                                                 final = final,
                                                 UUID = id,
                                                 calname = "ETSIDI_Master")
                                    ]

    writeLines(icsETSIDIMaster, file.path(dest, 'ETSIDI_Master.ics'))
}
