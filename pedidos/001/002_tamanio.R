rm(list = ls())

library(tidyverse)

marco <- readRDS("pedidos/001/marco.rds")

marco_sin_inc_for <- marco |> 
  filter(tam_acti != "G4")

#------------------------------------------------------------------------------#
#                          Parametros tamaño muestral
#------------------------------------------------------------------------------#


nc=0.95
z=qnorm(nc+(1-nc)/2)
er=0.05

tam1 <- marco_sin_inc_for %>% 
  mutate(ventas_totales = as.numeric(ventas_totales),
         plazas_ult = as.numeric(plazas_ult)) %>% 
  group_by(dom1) %>% 
  summarise(N=n(),
            ventas_sd = sd(ventas_totales, na.rm = T),
            ventas = sum(ventas_totales, na.rm = T),
            empleo_sd = sd(plazas_ult, na.rm = T),
            empleo = sum(plazas_ult, na.rm = T)) %>% 
  mutate(num_ventas = (N*ventas_sd)^2,
         den_ventas =((N-1)/N)*((er*ventas/z)^2)+N*(ventas_sd^2),
         tam_ventas = num_ventas/den_ventas,
         num_empleo = (N*empleo_sd)^2,
         den_empleo =((N-1)/N)*((er*empleo/z)^2)+N*(empleo_sd^2),
         tam_empleo = num_empleo/den_empleo)

sum(tam1$tam_ventas)
sum(tam1$tam_empleo)

tam2 <- marco_sin_inc_for %>% 
  mutate(ventas_totales = as.numeric(ventas_totales),
         plazas_ult = as.numeric(plazas_ult)) %>% 
  group_by(dom2) %>% 
  summarise(N=n(),
            ventas_sd = sd(ventas_totales, na.rm = T),
            ventas = sum(ventas_totales, na.rm = T),
            empleo_sd = sd(plazas_ult, na.rm = T),
            empleo = sum(plazas_ult, na.rm = T)) %>% 
  mutate(num_ventas = (N*ventas_sd)^2,
         den_ventas =((N-1)/N)*((er*ventas/z)^2)+N*(ventas_sd^2),
         tam_ventas = num_ventas/den_ventas,
         num_empleo = (N*empleo_sd)^2,
         den_empleo =((N-1)/N)*((er*empleo/z)^2)+N*(empleo_sd^2),
         tam_empleo = num_empleo/den_empleo)

sum(tam2$tam_ventas)
sum(tam2$tam_empleo)


tam3 <- marco_sin_inc_for %>% 
  mutate(ventas_totales = as.numeric(ventas_totales),
         plazas_ult = as.numeric(plazas_ult)) %>% 
  group_by(dom3) %>% 
  summarise(N=n(),
            ventas_sd = sd(ventas_totales, na.rm = T),
            ventas = sum(ventas_totales, na.rm = T),
            empleo_sd = sd(plazas_ult, na.rm = T),
            empleo = sum(plazas_ult, na.rm = T)) %>% 
  mutate(num_ventas = (N*ventas_sd)^2,
         den_ventas =((N-1)/N)*((er*ventas/z)^2)+N*(ventas_sd^2),
         tam_ventas = num_ventas/den_ventas,
         num_empleo = (N*empleo_sd)^2,
         den_empleo =((N-1)/N)*((er*empleo/z)^2)+N*(empleo_sd^2),
         tam_empleo = num_empleo/den_empleo)

sum(tam3$tam_ventas, na.rm = T)
sum(tam3$tam_empleo, na.rm = T)


tam8 <- marco_sin_inc_for %>% 
  mutate(ventas_totales = as.numeric(ventas_totales),
         plazas_ult = as.numeric(plazas_ult)) %>% 
  group_by(dom8) %>% 
  summarise(N=n(),
            ventas_sd = sd(ventas_totales, na.rm = T),
            ventas = sum(ventas_totales, na.rm = T),
            empleo_sd = sd(plazas_ult, na.rm = T),
            empleo = sum(plazas_ult, na.rm = T)) %>% 
  mutate(num_ventas = (N*ventas_sd)^2,
         den_ventas =((N-1)/N)*((er*ventas/z)^2)+N*(ventas_sd^2),
         tam_ventas = num_ventas/den_ventas,
         num_empleo = (N*empleo_sd)^2,
         den_empleo =((N-1)/N)*((er*empleo/z)^2)+N*(empleo_sd^2),
         tam_empleo = num_empleo/den_empleo)

sum(tam8$tam_ventas, na.rm = T)
sum(tam8$tam_empleo, na.rm = T)




%>% 
  left_join(select(tnr, dominio, tnr_max,tnr_pro, tnr_min, tnr_ult), by="dominio") %>% 
  mutate(tnr_max = ifelse(is.na(tnr_max), 0, tnr_max/100),
         tnr_pro = ifelse(is.na(tnr_pro), 0, tnr_pro/100),
         tnr_min = ifelse(is.na(tnr_min), 0, tnr_min/100),
         tnr_ult = ifelse(is.na(tnr_ult), 0, tnr_ult/100),
         n1 = ceiling(tam/(1-tnr_max)),
         n2 = ifelse(n1>N,N,n1),
         n3 = ceiling(tam/(1-tnr_pro)),
         n4 = ifelse(n3>N,N,n3),
         n5 = ceiling(tam/(1-tnr_min)),
         n6 = ifelse(n5>N,N,n5),
         n7 = ceiling(tam/(1-tnr_ult)),
         n8 = ifelse(n7>N,N,n7))


#sum(tamanio$n1)
sum(tamanio_90_10$n2)