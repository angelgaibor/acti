rm(list = ls())

library(tidyverse)
library(rio)

marco <- readRDS("pedidos/002/marco.rds")

marco_sin_inc_for <- marco |> 
  filter(tam_acti != "G4") |> 
  mutate(estrato = paste0(codigo_provincia, codigo_seccion, tam_acti))

muestra_inc_for <- marco |> 
  filter(tam_acti == "G4")

resumen_muestra_inc_for <- muestra_inc_for |> 
  group_by(codigo_seccion, tam_acti, codigo_provincia, gsectores) |> 
  summarise(Nh = n(),
            nh = n())

#------------------------------------------------------------------------------#
#                          Parametros tamaño muestral ficha 3
#------------------------------------------------------------------------------#

nc=0.95
z=qnorm(nc+(1-nc)/2)
er=0.05

# a)	Nacional
# b)	 Provincial
# c)	 Un dígito (letra) de la CIUU Rev. 4.0.
# d)	 Tamaño de empresa (definido en la sección “Población objetivo) 
# e)	Sector económico (manufactura, servicios, comercio, minas y canteras, agricultura) 
# f)	Provincia, sector económico (manufactura, servicios, comercio, minas y canteras, agricultura) 


# se trabaja con los dominios tamanio, codigo_seccion y provincial-sector económicao

aux_tam <- marco_sin_inc_for |> 
  mutate(ventas_totales = as.numeric(ventas_totales),
         plazas_ult = as.numeric(plazas_ult)) |> 
  group_by(codigo_seccion, tam_acti, codigo_provincia, gsectores) |> 
  summarise(Nh=n(),
            ventas_sdh = sd(ventas_totales, na.rm = T),
            ventash = sum(ventas_totales, na.rm = T),
            empleo_sdh = sd(plazas_ult, na.rm = T),
            empleoh = sum(plazas_ult, na.rm = T)) |> 
  ungroup()

# tamanio
tam1 <- aux_tam %>%
  group_by(tam_acti) |> 
  mutate(N = sum(Nh, na.rm = T)) |> 
  ungroup() |> 
  mutate(ventas_whsh = (Nh/N)*ventas_sdh,
         ventas_whsh2 = (Nh/N)*(ventas_sdh^2),
         empleo_whsh = (Nh/N)*empleo_sdh,
         empleo_whsh2 = (Nh/N)*(empleo_sdh^2)) |> 
  group_by(tam_acti) |> 
  summarise(N = mean(N),
            s_ventas_whsh = sum(ventas_whsh, na.rm = T),
            s_ventas_whsh2 = sum(ventas_whsh2, na.rm = T),
            s_empleo_whsh = sum(empleo_whsh, na.rm = T),
            s_empleo_whsh2 = sum(empleo_whsh2, na.rm = T),
            ventas = sum(ventash, na.rm = T),
            empleo = sum(empleoh, na.rm = T)) |> 
  mutate(num_ventas = (z^2)*(s_ventas_whsh^2),
         den_ventas = (er*ventas/N)^2 + (z^2)*(1/N)*s_ventas_whsh2,
         tam_ventas = num_ventas/den_ventas,
         num_empleo = (z^2)*(s_empleo_whsh^2),
         den_empleo = (er*empleo/N)^2 + (z^2)*(1/N)*s_empleo_whsh2,
         tam_empleo = num_empleo/den_empleo)

sum(tam1$tam_empleo)
sum(tam1$tam_ventas)


# provincia
tam2 <- aux_tam %>%
  group_by(codigo_provincia, gsectores) |> 
  mutate(N = sum(Nh, na.rm = T)) |> 
  ungroup() |> 
  mutate(ventas_whsh = (Nh/N)*ventas_sdh,
         ventas_whsh2 = (Nh/N)*(ventas_sdh^2),
         empleo_whsh = (Nh/N)*empleo_sdh,
         empleo_whsh2 = (Nh/N)*(empleo_sdh^2)) |> 
  group_by(codigo_provincia, gsectores) |> 
  summarise(N = mean(N),
            s_ventas_whsh = sum(ventas_whsh, na.rm = T),
            s_ventas_whsh2 = sum(ventas_whsh2, na.rm = T),
            s_empleo_whsh = sum(empleo_whsh, na.rm = T),
            s_empleo_whsh2 = sum(empleo_whsh2, na.rm = T),
            ventas = sum(ventash, na.rm = T),
            empleo = sum(empleoh, na.rm = T)) |> 
  mutate(num_ventas = (z^2)*(s_ventas_whsh^2),
         den_ventas = (er*ventas/N)^2 + (z^2)*(1/N)*s_ventas_whsh2,
         tam_ventas = num_ventas/den_ventas,
         num_empleo = (z^2)*(s_empleo_whsh^2),
         den_empleo = (er*empleo/N)^2 + (z^2)*(1/N)*s_empleo_whsh2,
         tam_empleo = num_empleo/den_empleo,
         tam_ventas = ifelse(is.nan(tam_ventas), 0, tam_ventas))

sum(tam2$tam_empleo)
sum(tam2$tam_ventas)

# codigo_seccion
tam3 <- aux_tam %>%
  group_by(codigo_seccion) |> 
  mutate(N = sum(Nh, na.rm = T)) |> 
  ungroup() |> 
  mutate(ventas_whsh = (Nh/N)*ventas_sdh,
         ventas_whsh2 = (Nh/N)*(ventas_sdh^2),
         empleo_whsh = (Nh/N)*empleo_sdh,
         empleo_whsh2 = (Nh/N)*(empleo_sdh^2)) |> 
  group_by(codigo_seccion) |> 
  summarise(N = mean(N),
            s_ventas_whsh = sum(ventas_whsh, na.rm = T),
            s_ventas_whsh2 = sum(ventas_whsh2, na.rm = T),
            s_empleo_whsh = sum(empleo_whsh, na.rm = T),
            s_empleo_whsh2 = sum(empleo_whsh2, na.rm = T),
            ventas = sum(ventash, na.rm = T),
            empleo = sum(empleoh, na.rm = T)) |> 
  mutate(num_ventas = (z^2)*(s_ventas_whsh^2),
         den_ventas = (er*ventas/N)^2 + (z^2)*(1/N)*s_ventas_whsh2,
         tam_ventas = num_ventas/den_ventas,
         num_empleo = (z^2)*(s_empleo_whsh^2),
         den_empleo = (er*empleo/N)^2 + (z^2)*(1/N)*s_empleo_whsh2,
         tam_empleo = num_empleo/den_empleo)

sum(tam3$tam_empleo)
sum(tam3$tam_ventas)




# Distribucion

distribucion <- marco_sin_inc_for %>% 
  group_by(codigo_seccion, tam_acti, codigo_provincia, gsectores) %>% 
  summarise(Nh = n(),
            sdh_emp = sd(plazas_ult, na.rm = T),
            sdh_ven = sd(ventas_totales, na.rm = T)) %>% 
  ungroup() %>% 
  # mutate(sdh_emp = 1) |>
  # mutate(sdh_ven = 1) |>
  left_join(tam1 %>% 
              select(tam_acti, n_emp_ta = tam_empleo, n_ven_ta = tam_ventas), 
            by = "tam_acti") %>% 
  group_by(tam_acti) %>% 
  mutate(nh_emp_ta = ceiling(n_emp_ta * Nh * (sdh_emp)/sum(Nh * (sdh_emp), na.rm = T)),
         nh_ven_ta = ceiling(n_ven_ta * Nh * (sdh_ven)/sum(Nh * (sdh_ven), na.rm = T))) %>% 
  ungroup() |> 
  mutate(nh_emp_ta = case_when(is.na(nh_emp_ta) ~ pmin(Nh, 2),
                               nh_emp_ta < 2 ~ pmin(Nh, 2),
                               T ~ pmin(Nh, nh_emp_ta)),
         nh_ven_ta = case_when(is.na(nh_ven_ta) ~ pmin(Nh, 2),
                               nh_ven_ta < 2 ~ pmin(Nh, 2),
                               T ~ pmin(Nh, nh_ven_ta))) |> 
  left_join(tam2 %>% 
              select(codigo_provincia, gsectores, n_emp_cp = tam_empleo, n_ven_cp = tam_ventas), 
            by = c("codigo_provincia", "gsectores")) %>% 
  group_by(codigo_provincia, gsectores) %>% 
  mutate(nh_emp_cp = ceiling(n_emp_cp * Nh * (sdh_emp^ 2)/sum(Nh * (sdh_emp^ 2), na.rm = T)),
         nh_ven_cp = ceiling(n_ven_cp * Nh * (sdh_ven^ 2)/sum(Nh * (sdh_ven^ 2), na.rm = T))) %>% 
  ungroup() |> 
  mutate(nh_emp_cp = case_when(is.na(nh_emp_cp) ~ pmin(Nh, 2),
                               nh_emp_cp < 2 ~ pmin(Nh, 2),
                               T ~ pmin(Nh, nh_emp_cp)),
         nh_ven_cp = case_when(is.na(nh_ven_cp) ~ pmin(Nh, 2),
                               nh_ven_cp < 2 ~ pmin(Nh, 2),
                               T ~ pmin(Nh, nh_ven_cp))) |> 
  left_join(tam3 %>% 
              select(codigo_seccion, n_emp_cs = tam_empleo, n_ven_cs = tam_ventas), 
            by = "codigo_seccion") %>% 
  group_by(codigo_seccion) %>% 
  mutate(nh_emp_cs = ceiling(n_emp_cs * Nh * (sdh_emp^ 2)/sum(Nh * (sdh_emp^ 2), na.rm = T)),
         nh_ven_cs = ceiling(n_ven_cs * Nh * (sdh_ven^ 2)/sum(Nh * (sdh_ven^ 2), na.rm = T))) %>% 
  ungroup() |> 
  mutate(nh_emp_cs = case_when(is.na(nh_emp_cs) ~ pmin(Nh, 2),
                               nh_emp_cs < 2 ~ pmin(Nh, 2),
                               T ~ pmin(Nh, nh_emp_cs)),
         nh_ven_cs = case_when(is.na(nh_ven_cs) ~ pmin(Nh, 2),
                               nh_ven_cs < 2 ~ pmin(Nh, 2),
                               T ~ pmin(Nh, nh_ven_cs))) |> 
  mutate(nh = pmax(nh_emp_ta, nh_ven_ta, nh_emp_cp, nh_ven_cp, nh_emp_cs, nh_ven_cs))

sum(distribucion$nh)

# ajuste por no cobertura

tnr <- readRDS("pedidos/002/tnr.rds")

ajuste <- distribucion |> 
  select(codigo_seccion, tam_acti, codigo_provincia, gsectores, Nh, nh) |> 
  mutate(dominio = paste0(substr(tam_acti, 2, 2), codigo_seccion)) |> 
  left_join(tnr, by = "dominio") |> 
  group_by(tam_acti) |> 
  mutate(tnr_max = max(tnr, na.rm = T)) |> 
  ungroup() |> 
  mutate(tnr = ifelse(is.na(tnr), tnr_max, tnr)/100,
         nhf = pmin(Nh, ceiling(nh/(1-tnr))))

resumen_muestra_final <- resumen_muestra_inc_for |> 
  rbind(ajuste |> select(codigo_seccion, tam_acti, codigo_provincia, gsectores, Nh, nh = nhf))

sum(resumen_muestra_final$nh)

export(resumen_muestra_final |> 
         mutate(codigo_provincia = str_pad(codigo_provincia, 2, "left", "0")), 
       "pedidos/002/ficha2/tamaño_ficha2.xlsx")

