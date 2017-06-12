preamble <- paste(readLines('preambleCal.tex'), collapse = "\n")
document1p <- paste(readLines('documentCal1p.tex'), collapse = "\n")
document2p <- paste(readLines('documentCal2p.tex'), collapse = "\n")

event <- function(descripcion, inicio, final = NA)
{
    inicio <- as.Date(inicio)
    final <- as.Date(final)

    dm <- function(x) format(x, "%e de %B")
    
    ifelse(isOneDay(inicio, final),
           paste0("\\item ", dm(inicio),
                  ": \\textbf{", descripcion, "}"),
           paste0("\\item Desde el ", dm(inicio),
                  " hasta el ", dm(final),
                  ": \\textbf{", descripcion,
                  "}")
           )
}

calPDF <- function(cal, curso = "2016-2017", tipo = "Grado",
                   formato = '1p',
                   dest = tempdir())
{
    cal <- as.data.table(cal)
    ## Filtro por grado o Máster
    cal <- cal[Tipo %in% c("ETSIDI", tipo) & !is.na(Formato)]

    ## Formato: 1 ó 2 páginas
    document <- switch(formato,
                       '1p' = document1p,
                       '2p' = document2p)
    
    Years <- as.numeric(strsplit(curso, "-")[[1]])

    title <- paste0("\\title{\\vspace{-2cm}",
                    "Curso ", curso,
                    " (", tipo, ")",
                    "\\vspace{-1cm}}\n")
    
    oneDayHeader <- "\\newcommand{\\oneDayETSIDI}{"
    oneDayEnd <- "}\n"

    seqHeader <- "\\newcommand{\\seqETSIDI}{"
    seqEnd <- "}\n"

    S1Header <- paste0("\\newcommand{\\primerSemestre}",
                       "{\n{\\large\\textsc{Primer Semestre}}\n",
                       "\\begin{itemize}\n")
    S1End <- "\n\\end{itemize}}\n"
    S2Header <- paste0("\\newcommand{\\segundoSemestre}",
                       "{\n{\\large\\textsc{Segundo Semestre}}\n",
                       "\\begin{itemize}\n")
    S2End <- "\n\\end{itemize}}\n"

    fechasS1 <- cal[Descripcion == "Primer semestre",
                    .(Inicio, Final)]
    fechasS2 <- cal[Descripcion == "Segundo semestre",
                    .(Inicio, Final)]

    S1 <- cal[Inicio >= as.Date(paste0(Years[1], '-09-01')) &
              Inicio < fechasS2$Inicio]
    S2 <- cal[Inicio >= fechasS2$Inicio &
              Inicio <= as.Date(paste0(Years[2], '-07-31'))]
    
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
    oneTex <- cal[isOneDay(Inicio, Final),
                  dayTex(Inicio, Formato)]
    ## Eventos que ocupan un período.
    seqTex <- cal[!isOneDay(Inicio, Final),
                  seqTex(Inicio, Final, Formato)]

    S1Tex <- paste(S1Header,
                   paste(S1[Descripcion != "Primer semestre",
                            event(Descripcion, Inicio, Final)],
                         collapse = "\n"),
                   S1End,
                   collapse = "\n")
    
    S2Tex <- paste(S2Header,
                   paste(S2[Descripcion != "Segundo semestre",
                            event(Descripcion, Inicio, Final)],
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
    f <- paste0("Calendario", "_",
                tipo, "_",
                sub('-', '_', curso),
                "_", formato,
                '.tex')
    calTex <- paste(preamble,
                    calTex,
                    S1Tex,
                    S2Tex,
                    title,
                    document,
                    sep = '\n')
    writeLines(calTex, f) 
    system2('pdflatex', f)
    files2clean <- list.files('.', "(log|aux)")
    file.remove(files2clean)
    setwd(old)

    invisible(calTex)
}
