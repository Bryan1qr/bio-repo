#* Creación de dataset diversidad con APIs internacionales:
remotes::install_github("ropensci/rcites")
remotes::install_github("IUCN-UK/iucnredlist")

#* Initializing IUCN API:
library(iucnredlist)
library(iucnredlist)

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
          category = db$category[1],  # puede haber varias, tomo la primera
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

# aplicar a toda la tabla (ejemplo con 1000 especies)
resultados <- map_dfr(taxonomia$nombre_cientifico, iucn_info, api = api)