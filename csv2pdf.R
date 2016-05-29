preamble <- paste(readLines('preambleCal.tex'), collapse = "\n")
document <- paste(readLines('documentCal.tex'), collapse = "\n")

calPDF <- function(cal, nombre = "ETSIDI_2016_2017", dest = tempdir())
{
    ## Filtro solo grado
    cal <- cal[Tipo %in% c("ETSIDI", "Grado")]
    
    diasHeader <- "\\newcommand{\\calETSIDI}[0]{"
    diasEnd <- ";}\n"

    S1Header <- "\\newcommand{\\primerSemestre}{"
    S1End <- "}\n"
    fechasS1 <- cal[Descripcion == "Primer semestre",
                     .(Dia, Final)]
    S1 <- cal[Dia >= fechasS1$Dia & Final <= fechasS1$Final]
    S2Header <- "\\newcommand{\\segundoSemestre}[0]{"
    S2End <- "}\n"
    fechasS2 <- cal[Descripcion == "Segundo semestre",
                    .(Dia, Final)]
    S2 <- cal[Dia >= fechasS2$Dia & Final <= fechasS2$Final]
    
    dayTex <- function(x, formato)
    {
        paste(
            paste0("if (equals=", x, ") ",
                   " [", formato, "]"),
            collapse = "\n")
    }

    seqTex <- function(start, end, formato)
    {
        paste(
            paste0("if (between = ",
                   start, " and ", end, ") [", formato, "]"),
            collapse = "\n")
    }

    ## Eventos de 1 sólo día
    oneDay <- cal[is.na(Final) & !is.na(Formato),
                  .(Descripcion,
                    Dia,
                    Lectivo,
                    Formato,
                    Tipo)]
    ## Eventos que ocupan un período.
    seqDays <- cal[!is.na(Final) & !is.na(Formato),
                   .(Descripcion,
                     Inicio = Dia,
                     Final,
                     Lectivo,
                     Formato,
                     Tipo)
                   ]

    oneTex <- oneDay[, dayTex(Dia, Formato)]

    seqTex <- seqDays[, seqTex(Inicio, Final, Formato)]

    calTex <- paste(diasHeader,
                  oneTex, seqTex,
                  diasEnd,
                  S1Header, paste(S1[, Descripcion], collapse = "\n\n"), S1End,
                  S2Header, paste(S2[, Descripcion], collapse = "\n\n"), S2End,
                  sep = '\n')
    ## Logos
    file.copy(paste0('../misc/',
                     c('LogoETSIDI.pdf', 'LogoUPM.pdf')),
              dest)
    old <- setwd(dest)
    f <- paste0(nombre, '.tex')
    calTex <- paste(preamble, calTex, document)
    writeLines(calTex, f) 
    system2('pdflatex', f)
    files2clean <- list.files('.', "(log|aux)")
    file.remove(files2clean)
    setwd(old)

    invisible(calTex)
}
