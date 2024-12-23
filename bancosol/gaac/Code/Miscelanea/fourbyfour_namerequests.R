df <- readRDS("D:/!bso/girCartera/rds/ec_May2023.rds")
fourbyfour <- read_excel("D:/!bso/requests/fourbyfour_namerequest.xlsx") %>% 
  select(-NOMBRE)
df <- df %>% 
  mutate(NOMBRE = paste(NOMBRE_TIT,PATERNO_TIT,MATERNO_TIT)) %>% 
  select(CTACLIENTE, OPERACION, NOMBRE)

fourbyfour <- fourbyfour %>% 
  left_join(df,by=c("Cuenta"="CTACLIENTE","Operacion"="OPERACION"))
write_xlsx(fourbyfour, "D:/!bso/requests/fourbyfour_May2023.xlsx")
