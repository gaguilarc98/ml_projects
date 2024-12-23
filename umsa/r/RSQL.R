install.packages("sqldf")
library(sqldf)
sqldf("SELECT*
      FROM Orange")
install.packages("RODBC")
library(RODBC)
sqldf("
SELECT age,circumference
FROM Orange
      ")
sqldf("
SELECT age,circumference AS circ
FROM Orange
WHERE tree=1
    ")
#De manera similar en lenguaje R Orange[Orange$Tree==1,2:3]

####LIBRERIA ODBC####
#Cuando se trabaja con bases de datos fuera del entorno de R
#R provee una librería para conectar con una base de datos externa
library(RODBC)
#Ir a Panel de Control buscar Administrative Tools
#Establecer la conexión
canal <- odbcConnect("dbBiblioteca")
sqlTables(canal)
sqlTables(canal,tableType="TABLE")
sqlTables(canal,tableType="VIEW")
sqlColumns(canal, "LIBRO")
#Guardar relaciones
LIBRO <- sqlFetch(canal, "LIBRO")
Ex1 <- sqlFetch(canal, "Ejercicio1")
consulta <- "SELECT * FROM LIBRO WHERE Genero='Narrativo'"
sqlQuery(canal,consulta)
#Consultas con subconsultas
consulta2 <- "SELECT L.Idioma, round(count(L.Codlibro)/(SELECT count(Codlibro) FROM libro),2) AS prop_libros
FROM libro AS L
GROUP BY L.Idioma;
"
sqlQuery(canal,consulta2)
odbcClose(canal)
####Libreria DBI####
install.packages("DBI")
install.packages("odbc")
library(odbc)
library(DBI)
odbc::odbcListDrivers()
file_name = "C:/Users/98acg/OneDrive/Documents/Bases de Datos/Bblioteca.mdb"
con <- dbConnect(odbc::odbc(),
                 .connection_string=paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                           DBQ=",file_name,";"))
#Argumento opcional de dbConnect: encoding="latin1"
#Listado de relaciones
dbListTables(con)
#lista de atributos
dbListFields(con,"LiBRO")
#lista de tuplas
dbReadTable(con,"LIBRO")
#Guardar la relación en un data.frame
LIBRO <- dbReadTable(con,"LIBRO")
#Utilizamos SQL
consulta <- "SELECT nombre, apellido, Libro
FROM USUARIO, REGISTRO, LIBRO
WHERE USUARIO.Codusuario=REGISTRO.codusuario AND
                REGISTRO.Codlibro=LIBRO.Codlibro AND
                Idioma='Español'"
dbGetQuery(con,consulta)
#Cerrar la conexion
odbcClose(canal)
canal2 <- odbcDriverConnect(paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};
                                           DBQ=",file_name,";"))
