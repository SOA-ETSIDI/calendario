source('init.R')

cal <- leeCalendario()
makeCalPDF(cal, formato = 'v')
makeCalPDF(cal, formato = 'h')

