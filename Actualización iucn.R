#* Creación de dataset diversidad con APIs internacionales:
remotes::install_github("ropensci/rcites")
remotes::install_github("IUCN-UK/iucnredlist")

#* Initializing IUCN API:
library(iucnredlist)
api <- init_api("UJjX7CaSLakyorMtTKxU4w465HPHMB4EQkjX")

taxonomia <- read.csv("data_especies.csv", fileEncoding = "Latin1", sep = ";")
# función para consultar una especie
iucn_info <- function(x, api){
  # separar género y especie
  partes <- str_split(x, " ", simplify = TRUE)
  genus <- partes[1]
  species <- partes[2]
  
  out <- tryCatch(
    {
      db <- assessments_by_name(api, genus = genus, species = species)
      
      if (is.null(db) || nrow(db) == 0) {
        tibble(
          name = x,
          category = NA,
          year = NA
        )
      } else {
        tibble(
          name = x,
          iucn_id = db$sis_taxon_id[1],
          category = db$red_list_category_code[1],  # puede haber varias, tomo la primera
          year = db$year_published[1]
        )
      }
    },
    error = function(e){
      tibble(
        name = x,
        category = NA,
        year = NA
      )
    }
  )
  return(out)
}
library(tidyverse)
resultados <- map_dfr(taxonomia$nombre_cientifico, iucn_info, api = api)
resultadosxd <- resultados
names(resultadosxd) <- c("nombre_cientifico", "iucn_id", "iucn_status", "iucn_year")
resultados2 <- map_dfr(taxonomia$nombre_cientifico[330:340], iucn_info, api = api)
a <- assessments_by_name(api, genus = "rhea", species = "pennata")


dataset_actualizado <- taxonomia %>% mutate(uicn_status = as.character(uicn_status),
                                            iucn_id = as.integer(iucn_id)) %>%
  rename(iucn_status = "uicn_status") %>%
  rows_update(resultadosxd %>% distinct() %>% select(-iucn_year), by = "nombre_cientifico")


write.table(dataset_actualizado, "data_especies.csv",
 sep = ";", row.names = FALSE, fileEncoding = "Latin1")