# plangue:

xd <- read.csv("nombres_especies_completo.csv")
library(rgbif)


clave <- rgbif::name_backbone(name = "Tillandsia latifolia")$usageKey

# luego usarlo en map_fetch
xd <- rgbif::map_fetch(
  taxonKey = clave,
  srs = "EPSG:4326"
)


library(rgbif)
library(sf)

# 1. obtener el taxonKey
clave <- name_backbone(name = "Tillandsia latifolia")$usageKey

# 2. descargar ocurrencias (ejemplo: máximo 200 registros)
occs <- occ_search(
  taxonKey = clave,
  hasCoordinate = TRUE,
  limit = 200
)

# 3. convertir a sf
pts_sf <- st_as_sf(
  occs$data,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326
)

# revisar
plot(pts_sf["species"])


plot_sf(pts_sf$geometry)

ggplot() +
  geom_sf(data = pts_sf)
