source('init.R')

cal <- leeCalendario()
calPDF(cal, curso = cursoActual, tipo = 'Grado', formato = '1p')
calPDF(cal, curso = cursoActual, tipo = 'Grado', formato = '2p')
calPDF(cal, curso = cursoActual, tipo = 'Master', formato = '1p')
calPDF(cal, curso = cursoActual, tipo = 'Master', formato = '2p')
