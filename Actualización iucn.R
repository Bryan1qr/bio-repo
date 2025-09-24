#* Creación de dataset diversidad con APIs internacionales:
remotes::install_github("ropensci/rcites")
remotes::install_github("IUCN-UK/iucnredlist")

#* Initializing IUCN API:
library(iucnredlist)
api <- init_api("UJjX7CaSLakyorMtTKxU4w465HPHMB4EQkjX")

taxonomia <- read.csv("data_especies.csv", fileEncoding = "Latin1", sep = ";")
# función para consultar una especie
iucn_info <- function(x, api) {
  # separar género y especie (puede haber casos sin especie)
  partes <- str_split(x, "\\s+", simplify = TRUE)
  genus <- ifelse(length(partes) >= 1 && partes[1] != "", partes[1], NA)
  species <- ifelse(length(partes) >= 2 && partes[2] != "", partes[2], NA)
  
  out <- tryCatch(
    {
      # llamada condicional según los campos disponibles
      if (!is.na(genus) && !is.na(species)) {
        db <- assessments_by_name(api, genus = genus, species = species)
      } else if (!is.na(genus)) {
        db <- assessments_by_name(api, genus = genus)
      } else {
        db <- NULL
      }
      
      # delay para respetar el rate limit
      Sys.sleep(2)
      
      # si no se encuentra nada
      if (is.null(db) || nrow(db) == 0) {
        tibble(
          name = x,
          iucn_id = NA,
          category = NA,
          year = NA
        )
      } else {
        tibble(
          name = x,
          iucn_id = dplyr::coalesce(db$sis_taxon_id[1], NA),
          category = dplyr::coalesce(db$red_list_category_code[1], NA),
          year = dplyr::coalesce(db$year_published[1], NA)
        )
      }
    },
    error = function(e) {
      tibble(
        name = x,
        iucn_id = NA,
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


dataset_actualizado <- taxonomia %>%
  rows_update(resultadosxd %>% distinct() %>% select(-iucn_year), by = "nombre_cientifico")


write.table(dataset_actualizado, "data_especies.csv",
 sep = ";", row.names = FALSE, fileEncoding = "Latin1")


# Dataset para looker:
hojas <- readxl::excel_sheets("Nueva_base_de datos avance.xlsx")
estudios <- openxlsx::read.xlsx("Nueva_base_de datos avance.xlsx", sheet = hojas[5], na.strings = c("-", ""))
df_completo <- taxonomia %>%
  left_join(estudios_v2, by = c(nombre_cientifico = "ESPECIES"), relationship = "many-to-many")


openxlsx::write.xlsx(df_completo, "dataset_looker.xlsx")
