
# Exploración de datos --------------------------------------------------

rm(list=ls())
# Cargar paquetes ---------------------------------------------------------
#install.packages("pacman")
pacman::p_load(tidyverse,
               sjmisc)

# Cargar datos ------------------------------------------------------------
fdl <- readRDS(url("https://github.com/fabrica-datos-laborales/fdl-data/raw/main/output/data/fdl.rds"))

# Explorar ----------------------------------------------------------------

dim(fdl)

find_var(fdl, "business")
find_var(fdl, "union")
find_var(fdl, "wage")
find_var(fdl, "Government")
find_var(fdl, "government")
find_var(fdl, "fem")
find_var(fdl, "women")

# ¡Nos quedamos con estas!

find_var(fdl, "db_wdi") #Ease of doing business score (0 = lowest performance to 100 = best performance)
find_var(fdl, "ud_ilo_stat") #Trade union density rate (%)
find_var(fdl, "mrw_oecd") #Minimum relative to median wages of full-time workers
find_var(fdl, "Govint_ictwss") #Government intervention in wage bargaining 
find_var(fdl, "hourearn_fem_isco08_total_ilo_stat") #Mean nominal hourly earning of women employees (ISCO08 classification)
find_var(fdl, "women_empower_vdem") #Women political empowerment index


# Descriptivos ------------------------------------------------------------

descr(fdl$db_wdi)
descr(fdl$ud_ilo_stat)
descr(fdl$mrw_oecd)
frq(fdl$Govint_ictwss)
descr(fdl$hourearn_fem_isco08_total_ilo_stat)
descr(fdl$women_empower_vdem)