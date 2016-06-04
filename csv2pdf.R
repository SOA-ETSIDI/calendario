preamble <- paste(readLines('preambleCal.tex'), collapse = "\n")
document <- paste(readLines('documentCal.tex'), collapse = "\n")

isOneDay <- function(inicio, final)
{
    (is.na(final) | inicio == final)
}

event <- function(descripcion, inicio, final = NA)
{
    inicio <- as.Date(inicio)
    final <- as.Date(final)

    dm <- function(x) format(x, "%e de %B")
    
    ifelse(isOneDay(inicio, final),
           paste0("\\item ", "\\textbf{", descripcion, "}: ",
                  dm(inicio)),
           paste0("\\item ", "\\textbf{", descripcion,
                  "}: desde el ", dm(inicio),
                  " hasta el ", dm(final))
           )
}

calPDF <- function(cal, curso = "2016_2017", tipo = "Grado",
                   dest = tempdir())
{
    cal <- as.data.table(cal)
    ## Filtro por grado o Máster
    cal <- cal[Tipo %in% c("ETSIDI", tipo) & !is.na(Formato)]

    Years <- as.numeric(strsplit(curso, "_")[[1]])
    
    oneDayHeader <- "\\newcommand{\\oneDayETSIDI}{"
    oneDayEnd <- "}\n"

    seqHeader <- "\\newcommand{\\seqETSIDI}{"
    seqEnd <- "}\n"

    S1Header <- paste0("\\newcommand{\\primerSemestre}",
                       "{\n{\\LARGE\\textsc{Primer Semestre}",
                       " (", tipo, ")}\n",
                       "\\begin{itemize}\n")
    S1End <- "\n\\end{itemize}}\n"
    S2Header <- paste0("\\newcommand{\\segundoSemestre}",
                       "{\n{\\LARGE\\textsc{Segundo Semestre}",
                       " (", tipo, ")}\n",
                       "\\begin{itemize}\n")
    S2End <- "\n\\end{itemize}}\n"

    fechasS1 <- cal[Descripcion == "Primer semestre",
                    .(Dia, Final)]
    fechasS2 <- cal[Descripcion == "Segundo semestre",
                    .(Dia, Final)]

    S1 <- cal[Dia >= as.Date(paste0(Years[1], '-09-01')) &
              Dia < fechasS2$Dia]
    S2 <- cal[Dia >= fechasS2$Dia &
              Dia <= as.Date(paste0(Years[2], '-07-31'))]
    
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
    oneTex <- cal[isOneDay(Dia, Final),
                  dayTex(Dia, Formato)]
    ## Eventos que ocupan un período.
    seqTex <- cal[!isOneDay(Dia, Final),
                  seqTex(Dia, Final, Formato)]

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

    calTex <- paste(oneDayHeader, oneTex, oneDayEnd,
                    seqHeader, seqTex, seqEnd,
                    sep = '\n')
    ## Logos
    file.copy(paste0('../misc/',
                     c('LogoETSIDI.pdf', 'LogoUPM.pdf')),
              dest)
    old <- setwd(dest)
    f <- paste0("ETSIDI", "_",
                tipo, "_",
                curso, '.tex')
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
