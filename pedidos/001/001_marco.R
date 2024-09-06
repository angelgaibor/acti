rm(list = ls())

library(tidyverse)

reem <- readRDS("pedidos/001/directorio_20240318.rds")


marco <- reem %>%
  # 11 563 930 registros
  filter(anio == 2022) %>%
  # 1 242 483 registros
  mutate(tam_acti = case_when(plazas_ult >= 500 | ventas_totales > 5000000 ~ "G4",
                               plazas_ult < 10  ~ "G1",
                               plazas_ult >= 10 & plazas_ult < 50 ~ "G2",
                               plazas_ult >= 50 & plazas_ult < 500 ~ "G3",
                               is.na(plazas_ult) ~ "G0",
                               T ~ "G9")) |> 
  filter(tam_acti %in% c("G2", "G3", "G4"),
         # 32 982 registros
         codigo_seccion %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
                               "K", "L", "M", "N", "P", "Q"),
         # 30 625
         situacion == 1,
         # 30 620
         is.na(empresas_noubicadas)
         # 30 595
         ) |> 
  mutate(dom1 = "1",
         dom2 = codigo_provincia,
         dom3 = codigo_seccion,
         dom4 = tam_acti,
         dom5 = gsectores,
         dom6 = paste0(codigo_provincia, tam_acti, codigo_seccion),
         dom7 = paste0(codigo_provincia, gsectores),
         dom8 = paste0(codigo_seccion, tam_acti))
  
saveRDS(marco, "pedidos/001/marco.rds")

# #sustraigo los 3 digitos para posterioemente sacar los K64, K66, Q88, S94
#   mutate(filtro = substr(codigo_clase, 1, 3), 
#          id_empresa = as.character(id_empresa)) %>%
#   #filtro tamano de empresa mediana a, mediana b y grande empres
#   filter(tamanou_plazas == 3 | tamanou_plazas == 4 | tamanou_plazas == 5) %>%
#   #filtro la rama de actividad A, O, T, U
#   filter(codigo_seccion != "A" & codigo_seccion != "O" & 
#            codigo_seccion != "T" & codigo_seccion != "U") %>%
#   #filtro Actividades de servicio financiero, excepto de seguros (K64), Actividades auxiliares de servicio financiero (K66),
#   #filtro Actividades de Asistencia Social sin alojamiento (Q88), Actividades de Asociaciones (S94)
#   filter(filtro != "K64" & filtro != "K66" &
#            filtro != "Q88" & filtro != "S94") %>%
#   #filtro forma institucional Institucion publica
#   filter(forma_institucional != 7) %>%
#   
#   #filtro Se excluye las empresas Grandes y Medianas “B” con forma institucional “Personas naturales no obligadas a
#   #llevar contabilidad”.
#   mutate(filtro = ifelse((tamanou_plazas == 4 | tamanou_plazas == 5 ) & 
#                            forma_institucional == 2, 1, 0)) %>%
#   filter(filtro == 0) %>%
#   #filtro las empresas no ubicadas
#   #filter(!id_empresa %in% emp_noubi_anion$inec_identificador_empresa) %>%
#   filter(is.na(empresas_noubicadas)) %>% 
#   # se excluyen las empresas con rechazo reitarado de los últimos tres años
#   filter(!id_empresa %in% emp_exc_rec) %>% 
#   mutate(dom_m = paste0(tamanou_plazas, codigo_seccion),
#          id_empresa = as.character(id_empresa))