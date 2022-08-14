
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

# Explorar ----------------------------------------------------------------

dim(fdl)
find_var(fdl, "db_wdi") #Ease of doing business score (0 = lowest performance to 100 = best performance)
find_var(fdl, "ud_ilo-stat") #Trade union density rate (%)
find_var(fdl, "mrw_oecd") #Minimum relative to median wages of full-time workers
find_var(fdl, "Govint_ictwss") #Government intervention in wage bargaining 
find_var(fdl, "hourearn_fem_isco08_total_ilo-stat") #Mean nominal hourly earning of women employees (ISCO08 classification)
find_var(fdl, "women_empower_vdem_vdem") #Women political empowerment index

# Seleccionar variables ---------------------------------------------------
proc <- fdl %>% 
  select(iso3c,
         year,
         db_wdi,
         'ud_ilo-stat',
         mrw_oecd,
         Govint_ictwss,
         'hourearn_fem_isco08_total_ilo-stat',
         women_empower_vdem_vdem) %>% 
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

