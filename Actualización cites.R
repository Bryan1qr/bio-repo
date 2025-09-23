#* Automatizando la extracción de apéndices de cites:
library(rcites)
library(tidyverse)
forget_token()
set_token("SLjDDY0MM6EJIGIcXd31lAtt")
# Obtener el objeto raw
taxonomia <- read.csv(file = "data_especies.csv", sep = ";" , fileEncoding = "Latin1")
res <- spp_taxonconcept(query_taxon = "Rhea pennata", raw = TRUE)
source("scripts/descarga_cites.R")
listado_pro <- citesxd(taxonomia$nombre_cientifico, delay = 2, max_per_minute = 30)

View(taxonomia)
openxlsx::write.xlsx(listado_pro %>% distinct(), "cites_listado.xlsx")

pequena <- listado_pro %>% distinct() %>% select(-cites_status)
names(pequena) <- c("nombre_cientifico", "cites_id", "cites_status")

grande_actualizada <- taxonomia %>%
  rows_update(
    pequena,
    by = "nombre_cientifico"
  )

peque <- pequena %>% select(nombre_cientifico) %>% distinct()


library(dplyr)

# Forzar tipos iguales
taxonomia <- taxonomia %>%
  mutate(cites_id = as.integer(cites_id),
         cites_status = as.character(cites_status))

pequena <- pequena %>%
  mutate(cites_id = as.integer(cites_id),
         cites_status = as.character(cites_status))

# Ahora sí actualizar
grande_actualizada <- taxonomia %>%
  rows_update(pequena, by = "nombre_cientifico")

write.table(grande_actualizada, "data_especies.csv",
 sep = ";", row.names = FALSE, fileEncoding = "Latin1")
