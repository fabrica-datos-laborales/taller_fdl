
# Procesamiento de datos --------------------------------------------------

rm(list=ls())
# Cargar paquetes ---------------------------------------------------------
#install.packages("pacman")
pacman::p_load(tidyverse,
               sjmisc,
               car)

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
  mutate(Govint_ictwss = as_factor(Govint_ictwss))


proc$Govint_ictwss <- car::recode(proc$Govint_ictwss,
                                  "6 = NA")

# Exportar ----------------------------------------------------------------
saveRDS(proc, "output/proc.rds")

