library(pdftools)
text <- pdf_subset("D:/Files/Informes/RiesgoIntegral/INFORME_RIESGO_INTEGRAL_SEP23.pdf", 
                   pages = 10:12, output = "D:/Files/Informes/RiesgoIntegral/INFORME_RIESGO_INTEGRAL_SEP23_Liquidez.pdf")
