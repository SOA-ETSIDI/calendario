preamble <- paste(readLines('preambleCal.tex'), collapse = "\n")
document <- paste(readLines('documentCal.tex'), collapse = "\n")

event <- function(descripcion, inicio, final = NA)
{
    inicio <- as.Date(inicio)
    final <- as.Date(final)

    isOneDay <- function(inicio, final)
    {
        (is.na(final) | inicio == final)
    }

    dm <- function(x) format(x, "%e de %B")
    
    ifelse(isOneDay(inicio, final),
           paste0("\\item ", dm(inicio), ": ", descripcion),
           paste0("\\item ", " Desde el ", dm(inicio),
                  " hasta el ", dm(final),
                  ": ", descripcion))
}

calPDF <- function(cal, nombre = "ETSIDI_2016_2017", tipo = "Grado",
                   dest = tempdir())
{
    ## Filtro solo grado
    cal <- cal[Tipo %in% c("ETSIDI", tipo)]
    
    diasHeader <- "\\newcommand{\\calETSIDI}[0]{"
    diasEnd <- ";}\n"

    S1Header <- "\\newcommand{\\primerSemestre}{\n\\begin{itemize}\n"
    S1End <- "\n\\end{itemize}}\n"
    fechasS1 <- cal[Descripcion == "Primer semestre",
                     .(Dia, Final)]
    S1 <- cal[Dia >= fechasS1$Dia & Dia <= fechasS1$Final]
    S2Header <- "\\newcommand{\\segundoSemestre}{\n\\begin{itemize}\n"
    S2End <- "\n\\end{itemize}}\n"
    fechasS2 <- cal[Descripcion == "Segundo semestre",
                    .(Dia, Final)]
    S2 <- cal[Dia >= fechasS2$Dia & Dia <= fechasS2$Final]
    
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

    S1Tex <- paste(S1Header,
                   paste(S1[,
                            event(Descripcion, Dia, Final)],
                         collapse = "\n"),
                   S1End,
                   collapse = "\n")
    
    S2Tex <- paste(S2Header,
                   paste(S2[,
                            event(Descripcion, Dia, Final)],
                         collapse = "\n"),
                   S2End,
                   collapse = "\n")

    calTex <- paste(diasHeader,
                  oneTex, seqTex,
                  diasEnd,
                  sep = '\n')
    ## Logos
    file.copy(paste0('../misc/',
                     c('LogoETSIDI.pdf', 'LogoUPM.pdf')),
              dest)
    old <- setwd(dest)
    f <- paste0(nombre, '.tex')
    calTex <- paste(preamble,
                    calTex,
                    S1Tex,
                    S2Tex,
                    document,
                    sep = '\n')
    writeLines(calTex, f) 
    system2('pdflatex', f)
    files2clean <- list.files('.', "(log|aux)")
    file.remove(files2clean)
    setwd(old)

    invisible(calTex)
}
