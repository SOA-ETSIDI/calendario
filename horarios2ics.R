## Hace falta ejecutar antes csv2ics...Por ahora esto no se utiliza

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
