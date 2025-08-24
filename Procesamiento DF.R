#las sentencias de rlibs son para reorientar en la pc del w las librerias

#R_LIBS_SITE="D:/DM/LIBRERIAS R"
#R_LIBS_USER="D:/DM/LIBRERIAS R"


library(eph)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(vroom)

#opcion 1 cargar datos
#lista.bases <- data_frame(nombre_bases = list.files('C:/Users/Usuario/Desktop/Codigos R/EPH/BasesOriginales/'))
#lista.bases.paths <- lista.bases  %>%
#  mutate(bases_path = paste0("C:/Users/Usuario/Desktop/Codigos R/EPH/BasesOriginales/", nombre_bases))
#BASES <- lista.bases.paths %>%
  # 'do' the function for each row in turn
#  rowwise() %>%
#  do(., vroom(file=.$bases_path))


#library(reactable)
#opcion 2 cargar datos
BASE <- get_microdata(year = 2017:2025,
                      #period = 1:4,
                      type = "individual"
)

#str(BASE$ANO4)
#opcion 3 cargar datos
#BASE1 <- read.table("C:/EPH/INDIVIDUAL/usu_individual_t117.txt", sep = ";", header = T)
#BASE2 <- read.table("C:/EPH/INDIVIDUAL/usu_individual_t118.txt", sep = ";", header = T)
#BASE3 <- read.table("C:/EPH/INDIVIDUAL/usu_individual_t217.txt", sep = ";", header = T)
#BASE4 <- read.table("C:/EPH/INDIVIDUAL/usu_individual_t218.txt", sep = ";", header = T)

#BASE <- rbind2(BASE1,BASE2)
#BASE <- rbind2(BASE,BASE3)
#BASE <- rbind2(BASE,BASE4) 

var.ind <- c("ANO4", "PP03J", "PONDERA","TRIMESTRE", "REGION", "CODUSU", "NRO_HOGAR", "COMPONENTE","CH04", "CH06", "PP07H", "PP04B_COD",
             "NIVEL_ED","ESTADO","CAT_OCUP","CAT_INAC","PP04D_COD", "INTENSI","P47T","P21", "PONDII","CAT_OCUP")


BEPH <- BASE %>% 
  select(all_of(var.ind)) %>% 
  filter(CH06 >= 14 & CH06 <= 64) %>% 
  mutate(Año =  as.character(ANO4),
         Periodo = paste(as.character(ANO4), as.character(TRIMESTRE)),
         Sexo = as.character(CH04),
         Sexo = case_when(Sexo=="1" ~ "Varones",
                          Sexo=="2" ~ "Mujeres"),
         NIVEL_EDUCATIVO = case_when(#NIVEL_ED %in% c(1,2, 3,7) ~ "Primaria",
                                     NIVEL_ED %in% c(1, 7) ~ "SinInst",
                                     NIVEL_ED %in% c(2, 3) ~ "Primaria",
                                     NIVEL_ED %in% c(4, 5) ~ "Secundaria",
                                     NIVEL_ED == 6         ~ "Superior",
                                     NIVEL_ED == 9         ~ "NsNr"),
         CATEGORIA_OCUPACIONAL = case_when(CAT_OCUP == 1 ~ "Patrón",
                                           CAT_OCUP == 2 ~ "Cuenta Propia",
                                           CAT_OCUP == 3 ~ "Obrero u Empleado",
                                           CAT_OCUP == 4 ~ "Trabajador Fliar sin Remuneración",
                                           CAT_OCUP == 9 ~ "NsNr",
                                           CAT_OCUP == 0 ~ "Inactivo"),
         NIVEL_EDUCATIVO = factor(NIVEL_EDUCATIVO, levels = c("SinInst", "Primaria", "Secundaria", "Superior")),
         GRUPO_EDAD = case_when(CH06 >= 14 & CH06 <= 29 ~ "de 14 a 29 años",
                                CH06 >= 30 & CH06 <= 64 ~ "de 30 a 64 años"))


###armo base de indicadores
##primero base trimestral para luego sacar promedio anual
BASE_IND_des <- BEPH %>%
  group_by (Sexo, Año, TRIMESTRE, NIVEL_EDUCATIVO, GRUPO_EDAD) %>%
  summarise(Poblacion         = sum(PONDERA),
            Desocupados       = sum(PONDERA [ESTADO == 2]),
            Ocupados          = sum(PONDERA [ESTADO == 1]),
            PEA               = Ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            Subocupados       = Suboc_demandante + Suboc_no_demand,
            Tasa_Actividad                = round(PEA/Poblacion, 3)*100,
            Tasa_Desocupación               = round(Desocupados/PEA, 3)*100,
            Tasa_Subocupación               = round(Subocupados/PEA, 3)*100,
            Ocupados_Asalariados = sum(PONDERA [ESTADO == 1 & CAT_OCUP==3]),
            Asalariados_NoReg = sum(PONDERA [ESTADO == 1 & CAT_OCUP==3 & PP07H==2]),
            Porcentaje_NoReg = round(Asalariados_NoReg / Ocupados_Asalariados, 3)*100)

BASE_IND_Educ<- BEPH %>%
  group_by (Sexo, Año, TRIMESTRE, NIVEL_EDUCATIVO) %>%
  summarise(GRUPO_EDAD = "Total",
            Poblacion         = sum(PONDERA),
            Desocupados       = sum(PONDERA [ESTADO == 2]),
            Ocupados          = sum(PONDERA [ESTADO == 1]),
            PEA               = Ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            Subocupados       = Suboc_demandante + Suboc_no_demand,
            Tasa_Actividad                = round(PEA/Poblacion, 3)*100,
            Tasa_Desocupación               = round(Desocupados/PEA, 3)*100,
            Tasa_Subocupación               = round(Subocupados/PEA, 3)*100,
            Ocupados_Asalariados = sum(PONDERA [ESTADO == 1 & CAT_OCUP==3]),
            Asalariados_NoReg = sum(PONDERA [ESTADO == 1 & CAT_OCUP==3 & PP07H==2]),
            Porcentaje_NoReg = round(Asalariados_NoReg / Ocupados_Asalariados, 3)*100)

BASE_IND_Edad <- BEPH %>%
  group_by (Sexo, Año, TRIMESTRE, GRUPO_EDAD) %>%
  summarise(NIVEL_EDUCATIVO = "Total",
            Poblacion         = sum(PONDERA),
            Desocupados       = sum(PONDERA [ESTADO == 2]),
            Ocupados          = sum(PONDERA [ESTADO == 1]),
            PEA               = Ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            Subocupados       = Suboc_demandante + Suboc_no_demand,
            Tasa_Actividad                = round(PEA/Poblacion, 3)*100,
            Tasa_Desocupación               = round(Desocupados/PEA, 3)*100,
            Tasa_Subocupación               = round(Subocupados/PEA, 3)*100,
            Ocupados_Asalariados = sum(PONDERA [ESTADO == 1 & CAT_OCUP==3]),
            Asalariados_NoReg = sum(PONDERA [ESTADO == 1 & CAT_OCUP==3 & PP07H==2]),
            Porcentaje_NoReg = round(Asalariados_NoReg / Ocupados_Asalariados, 3)*100)


#armo una categoria total dentro de nivel educativo para que se pueda tener en el filtro
BASE_IND_agr <- BEPH %>%
  group_by (Sexo, Año, TRIMESTRE) %>%
  summarise(NIVEL_EDUCATIVO = "Total",
            GRUPO_EDAD = "Total",
            Poblacion         = sum(PONDERA),
            Desocupados       = sum(PONDERA [ESTADO == 2]),
            Ocupados          = sum(PONDERA [ESTADO == 1]),
            PEA               = Ocupados + Desocupados,
            Suboc_demandante  = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J==1]),
            Suboc_no_demand   = sum(PONDERA[ESTADO == 1 & INTENSI ==1 & PP03J %in% c(2,9)]),
            Subocupados       = Suboc_demandante + Suboc_no_demand,
            Tasa_Actividad                = round(PEA/Poblacion, 3)*100,
            Tasa_Desocupación               = round(Desocupados/PEA, 3)*100,
            Tasa_Subocupación               = round(Subocupados/PEA, 3)*100,
            Ocupados_Asalariados = sum(PONDERA [ESTADO == 1 & CAT_OCUP==3]),
            Asalariados_NoReg = sum(PONDERA [ESTADO == 1 & CAT_OCUP==3 & PP07H==2]),
            Porcentaje_NoReg = round(Asalariados_NoReg / Ocupados_Asalariados, 3)*100
  )
            
BASE_IND <- rbind(BASE_IND_des,BASE_IND_Educ, BASE_IND_Edad, BASE_IND_agr)

#base anual

BASE_IND_ANUAL <- BASE_IND %>%
  group_by (Sexo,Año, NIVEL_EDUCATIVO, GRUPO_EDAD) %>%
  summarise(Poblacion    = mean(Poblacion),
            Desocupados       = mean(Desocupados),
            Ocupados         =  mean(Ocupados),
            PEA               = mean(PEA),
            Suboc_demandante  = mean(Suboc_demandante),
            Suboc_no_demand  = mean(Suboc_no_demand),
            Subocupados            = sum(Suboc_demandante + Suboc_no_demand),
            Tasa_Actividad                = round(PEA/Poblacion , 3)*100,
            Tasa_Desocupación               = round(Desocupados/PEA, 3)*100,
            Tasa_Subocupación               = round(Subocupados/PEA, 3)*100,
            Ocupados_Asalariados = mean(Ocupados_Asalariados),
            Asalariados_NoReg = mean(Asalariados_NoReg),
            Porcentaje_NoReg = round(mean(Porcentaje_NoReg),1)
  )


###armo base de ingresos
##INGRESO TOTAL INDIVIDUAL: base trimestral para luego sacar el promedio anual
ITIdes <- BEPH %>% 
  filter(P47T > 0 & CAT_OCUP %in% c(1:3)) %>% 
  group_by(Sexo, Año, TRIMESTRE, CATEGORIA_OCUPACIONAL, NIVEL_EDUCATIVO) %>% 
  summarise(IngTotInd_Medio= round(weighted.mean(P47T, PONDII), 1))

#armo categoria total dentro de categoria ocupacional Y educacion para que este en el filtro como opcion

ITIcat <- BEPH %>% 
  filter(P47T > 0 & CAT_OCUP %in% c(1:3)) %>% 
  group_by(Sexo, Año, TRIMESTRE, CATEGORIA_OCUPACIONAL) %>% 
  summarise(NIVEL_EDUCATIVO = "Total",
    IngTotInd_Medio= round(weighted.mean(P47T, PONDII), 1))

ITIed <- BEPH %>% 
  filter(P47T > 0 & CAT_OCUP %in% c(1:3)) %>% 
  group_by(Sexo, Año, TRIMESTRE, NIVEL_EDUCATIVO) %>% 
  summarise(CATEGORIA_OCUPACIONAL = "Total",
            IngTotInd_Medio= round(weighted.mean(P47T, PONDII), 1))

ITIagr <- BEPH %>% 
  filter(P47T > 0 & CAT_OCUP %in% c(1:3)) %>% 
  group_by(Sexo, Año, TRIMESTRE) %>% 
  summarise(CATEGORIA_OCUPACIONAL = "Total",
            NIVEL_EDUCATIVO = "Total",
            IngTotInd_Medio= round(weighted.mean(P47T, PONDII), 1))
#Base final trimestral del ITI trimestral
IngTotInd <- rbind(ITIdes, ITIcat, ITIed, ITIagr)


#INGRESO OCUPACION PRINCIPAL: mismos pasos 
IOCdes <- BEPH %>% 
  filter(ESTADO == 1 & CAT_OCUP %in% c(1:3)) %>% 
  group_by(Sexo, Año, TRIMESTRE, CATEGORIA_OCUPACIONAL, NIVEL_EDUCATIVO) %>% 
  summarise(IngOcP_Medio= round(weighted.mean(P21, PONDII), 1))

#armo categoria total dentro de categoria ocupacional Y educacion para que este en el filtro como opcion

IOCcat <- BEPH %>% 
  filter(ESTADO == 1 & CAT_OCUP %in% c(1:3)) %>% 
  group_by(Sexo, Año, TRIMESTRE, CATEGORIA_OCUPACIONAL) %>% 
  summarise(NIVEL_EDUCATIVO = "Total",
            IngOcP_Medio= round(weighted.mean(P21, PONDII), 1))

IOCed <- BEPH %>% 
  filter(ESTADO == 1 & CAT_OCUP %in% c(1:3)) %>% 
  group_by(Sexo, Año, TRIMESTRE, NIVEL_EDUCATIVO) %>% 
  summarise(CATEGORIA_OCUPACIONAL = "Total",
            IngOcP_Medio= round(weighted.mean(P21, PONDII), 1))

IOCagr <- BEPH %>% 
  filter(ESTADO == 1 & CAT_OCUP %in% c(1:3)) %>% 
  group_by(Sexo, Año, TRIMESTRE) %>% 
  summarise(CATEGORIA_OCUPACIONAL = "Total",
            NIVEL_EDUCATIVO = "Total",
            IngOcP_Medio= round(weighted.mean(P21, PONDII), 1))
#Base final trimestral del ITI trimestral
IngOcPpal <- rbind(IOCdes, IOCcat, IOCed, IOCagr)

BASE_INGRESOS <- left_join(IngTotInd,IngOcPpal)


BASE_ING_ANUAL <- BASE_INGRESOS %>%
  group_by (Sexo,Año, CATEGORIA_OCUPACIONAL, NIVEL_EDUCATIVO) %>%
  summarise(IOP_Medio = mean(IngOcP_Medio),
            ITI_Medio = mean(IngTotInd_Medio)
            )


#transpose para graficos

ITI <- BASE_ING_ANUAL %>% 
  select(Sexo, Año, CATEGORIA_OCUPACIONAL, NIVEL_EDUCATIVO, ITI_Medio) %>% 
  spread(., Sexo,ITI_Medio) %>%
  rename(ITI_Mujeres = Mujeres,
         ITI_Varones = Varones) %>%
  mutate(BrechaITI = round(((ITI_Mujeres / ITI_Varones)-1)*100, digits=2))

IOP <- BASE_ING_ANUAL %>% 
  select(Sexo, Año, CATEGORIA_OCUPACIONAL, NIVEL_EDUCATIVO, IOP_Medio) %>% 
  spread(., Sexo,IOP_Medio) %>%
  rename(IOP_Mujeres = Mujeres,
         IOP_Varones = Varones) %>%
  mutate(BrechaIOP = round(((IOP_Mujeres / IOP_Varones)-1)*100, digits=2))

write.table(ITI, "C:/Users/Usuario/Desktop/Codigos R/CURSO_SHINY/MLyGENERO/ITI.txt", sep = ";", dec = ",")

write.table(IOP, "C:/Users/Usuario/Desktop/Codigos R/CURSO_SHINY/MLyGENERO/IOP.txt", sep = ";", dec = ",")

#Hago un transpose para hacer los graficos

TasaAct <- BASE_IND_ANUAL %>% 
  select(Sexo, Año, NIVEL_EDUCATIVO, GRUPO_EDAD, Tasa_Actividad) %>% 
  spread(., Sexo,Tasa_Actividad) %>%
  rename(TA_Mujeres = Mujeres,
         TA_Varones = Varones) 

TasaDes <- BASE_IND_ANUAL %>% 
  select(Sexo, Año, NIVEL_EDUCATIVO, GRUPO_EDAD, Tasa_Desocupación) %>% 
  spread(., Sexo,Tasa_Desocupación) %>%
  rename(TD_Mujeres = Mujeres,
         TD_Varones = Varones) 
 
TasaSub <- BASE_IND_ANUAL %>% 
  select(Sexo, Año, NIVEL_EDUCATIVO, GRUPO_EDAD, Tasa_Subocupación) %>% 
  spread(., Sexo,Tasa_Subocupación) %>%
  rename(TS_Mujeres = Mujeres,
         TS_Varones = Varones) 

TasaNoReg <- BASE_IND_ANUAL %>% 
  select(Sexo, Año, NIVEL_EDUCATIVO, GRUPO_EDAD, Porcentaje_NoReg) %>% 
  spread(., Sexo,Porcentaje_NoReg) %>%
  rename(TNR_Mujeres = Mujeres,
         TNR_Varones = Varones) 

TasasML <- left_join(TasaAct, TasaDes)
TasasML <- left_join(TasasML, TasaSub)
TasasML <- left_join(TasasML, TasaNoReg)

write.table(TasasML, "C:/Users/Usuario/Desktop/Codigos R/CURSO_SHINY/MLyGENERO/TasasML.txt", sep = ";", dec = ",")


 

