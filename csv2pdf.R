preamble <- paste(readLines('preambleCal.tex'), collapse = "\n")
document <- paste(readLines('documentCal.tex'), collapse = "\n")

calPDF <- function(cal, nombre = "ETSIDI_2016_2017")
{

    diasHeader <- "\\newcommand{\\holidays}[0]{"
    diasEnd <- ";}\n"

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

    oneGTex <- oneDay[Tipo %in% c("ETSIDI", "Grado"),
                      dayTex(Dia, Formato)]

    seqGTex <- seqDays[Tipo %in% c("ETSIDI", "Grado"),
                       seqTex(Inicio, Final, Formato)]

    texG <- paste(diasHeader,
                  oneGTex, seqGTex,
                  diasEnd,
                  sep = '\n')

    old <- setwd(tempdir())
    f <- paste0(nombre, '.tex')
    writeLines(paste(preamble, texG, document), f) 
    system2('pdflatex', f)
    setwd(old)
}
