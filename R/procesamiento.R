
# Procesamiento de datos --------------------------------------------------

rm(list=ls())
# Cargar paquetes ---------------------------------------------------------
#install.packages("pacman")
pacman::p_load(tidyverse,
               sjmisc,
               car, 
               sjlabelled)

# Cargar datos ------------------------------------------------------------
fdl <- readRDS(url("https://github.com/fabrica-datos-laborales/fdl-data/raw/main/output/data/fdl.rds"))

# Seleccionar variables ---------------------------------------------------
proc <- fdl %>% 
  select(iso3c,
         year,
         db_wdi,
         ud_ilo_stat,
         mrw_oecd,
         Govint_ictwss,
         hourearn_fem_isco08_total_ilo_stat,
         women_empower_vdem) %>% 
  filter(!is.na(iso3c) & !is.na(year)) %>% 
# Preparar datos ----------------------------------------------------------  
  mutate(Govint_ictwss = car::recode(as.numeric(.$Govint_ictwss),
                                     c("1 = 'No government intervention in wage bargaining';
                                       2 = 'Government provides an institutional framework of consultation and information exchange';
                                       3 = 'Government influences wage bargaining indirectly';
                                       4 = 'The government participates directly in wage bargaining';
                                       5 = 'The government imposes private sector wage settlements, places a ceiling on bargaining outcomes or suspends bargaining';
                                       6 = NA"), as.factor = T,
                                     levels = c('No government intervention in wage bargaining',
                                                'Government provides an institutional framework of consultation and information exchange',
                                                'Government influences wage bargaining indirectly',
                                                'The government participates directly in wage bargaining',
                                                'The government imposes private sector wage settlements, places a ceiling on bargaining outcomes or suspends bargaining')))

proc$Govint_ictwss <- set_label(proc$Govint_ictwss, "Government intervention in wage bargaining")

# Exportar ----------------------------------------------------------------
saveRDS(proc, "output/proc.rds")

