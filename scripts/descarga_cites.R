# Descarga cites:
citesxd <- function(species_vec, delay = 1.5, max_per_minute = 30) {
  # helper para extraer un campo de forma segura
  safe_get <- function(obj, field, default = NA) {
    if (is.null(obj)) return(default)
    if (!field %in% names(obj)) return(default)
    val <- obj[[field]]
    if (is.null(val)) return(default)
    if (is.atomic(val) && length(val) >= 1) return(val[1])
    if (is.list(val) && length(val) == 1 && (is.atomic(val[[1]]) || is.character(val[[1]]))) {
      return(val[[1]])
    }
    return(val)
  }

  n_species <- length(species_vec)
  remaining <- max_per_minute  # contador de consultas restantes en el bloque de minuto

  results <- lapply(seq_along(species_vec), function(i) {
    sp <- species_vec[i]
    Sys.sleep(delay)

    out <- tryCatch({
      res <- spp_taxonconcept(query_taxon = sp, raw = TRUE)

      if (length(res) == 0) {
        tibble(species = sp, id = NA_integer_, name_status = NA_character_, cites_listing = NA_character_)
      } else {
        idx_active <- which(sapply(res, function(z) isTRUE(z$active)))
        especie <- if (length(idx_active) >= 1) res[[idx_active[1]]] else res[[1]]

        tibble(
          species = sp,
          id = safe_get(especie, "id", NA_integer_),
          name_status = safe_get(especie, "name_status", NA_character_),
          cites_listing = safe_get(especie, "cites_listing", NA_character_)
        )
      }
    }, error = function(e) {
      tibble(species = sp, id = NA_integer_, name_status = NA_character_, cites_listing = NA_character_)
    })

    # actualizar y mostrar contador
    remaining <<- remaining - 1
    message(sprintf("[%d/%d] Especie: %s | Consultas restantes en este minuto: %d",
                    i, n_species, sp, remaining))

    # si llega a 0, reiniciar el contador (nuevo minuto)
    if (remaining == 0) {
      message("⚠️  Límite de 30 consultas alcanzado. Reiniciando contador para el siguiente bloque.")
      remaining <<- max_per_minute
    }

    out
  })

  a <- bind_rows(results)
  names(a) <- c("nombre_cientifico", "cites_id", "cites_status", "apendix")
  a
}