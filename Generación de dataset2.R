#* Automatización de geneación de tablas:

df_modulo <- read.csv("biodiversidad_especies_2025-09-16_22-18-17.csv", sep = ";")

# Reptiles X, aves Y, mamíferos Z, flora F.

library(tidyverse)
library(openxlsx)

hojas <- readxl::excel_sheets("Nueva_base_de datos avance.xlsx")
reptil <- read.xlsx("Nueva_base_de datos avance.xlsx", sheet = hojas[1], na.strings = c("-", ""))
aves <- read.xlsx("Nueva_base_de datos avance.xlsx", sheet = hojas[2], na.strings = c("-", ""))
mamiferos <- read.xlsx("Nueva_base_de datos avance.xlsx", sheet = hojas[3], na.strings = c("-", ""))
flora <- read.xlsx("Nueva_base_de datos avance.xlsx", sheet = hojas[4], na.strings = c("-", ""))
estudios <- read.xlsx("Nueva_base_de datos avance.xlsx", sheet = hojas[5], na.strings = c("-", ""))

# Anfibios y reptiles:
excepciones <- names(reptil[1:9])
names(reptil) <- make.unique(names(reptil))
df1 <- reptil %>% 
  rename_with(
    ~ ifelse(.x %in% excepciones, .x, paste0("X", .x))
  )


base_names <- names(df1) %>% 
  keep(~ str_starts(.x, "X")) %>% 
  str_remove("\\.\\d+$") %>% 
  unique()

# Generar solo las columnas sumadas
df_sumado <- map_dfc(base_names, function(colbase) {
  cols <- df1 %>% select(matches(paste0("^", colbase, "(\\.|$)")))
  
  tibble(!!colbase := rowSums(cols != "" & !is.na(cols), na.rm = TRUE))
})

df_final <- df1 %>% 
  select(-starts_with("X")) %>% 
  bind_cols(df_sumado)

df_final %>%
  pivot_longer(cols = starts_with("X")) %>%
  filter(value > 0) -> db_rept_anf


# Aves:

names(aves) <- make.unique(names(aves))
df2 <- aves %>% 
  rename_with(
    ~ ifelse(.x %in% excepciones, .x, paste0("Y", .x))
  )


base_names2 <- names(df2) %>% 
  keep(~ str_starts(.x, "Y")) %>% 
  str_remove("\\.\\d+$") %>% 
  unique()

# Generar solo las columnas sumadas
df_sumado2 <- map_dfc(base_names2, function(colbase) {
  cols <- df2 %>% select(matches(paste0("^", colbase, "(\\.|$)")))
  
  tibble(!!colbase := rowSums(cols != "" & !is.na(cols), na.rm = TRUE))
})

df_final2 <- df2 %>% 
  select(-starts_with("Y")) %>% 
  bind_cols(df_sumado2)

df_final2 %>%
  pivot_longer(cols = starts_with("Y")) %>%
  filter(value > 0) -> db_aves

# Mamíferos:


names(mamiferos) <- make.unique(names(mamiferos))
df3 <- mamiferos %>% 
  rename_with(
    ~ ifelse(.x %in% excepciones, .x, paste0("Z", .x))
  )


base_names3 <- names(df3) %>% 
  keep(~ str_starts(.x, "Z")) %>% 
  str_remove("\\.\\d+$") %>% 
  unique()

# Generar solo las columnas sumadas
df_sumado3 <- map_dfc(base_names3, function(colbase) {
  cols <- df3 %>% select(matches(paste0("^", colbase, "(\\.|$)")))
  
  tibble(!!colbase := rowSums(cols != "" & !is.na(cols), na.rm = TRUE))
})

df_final3 <- df3 %>% 
  select(-starts_with("Z")) %>% 
  bind_cols(df_sumado3)

df_final3 %>%
  pivot_longer(cols = starts_with("Z")) %>%
  filter(value > 0) -> db_mamiferos


# Flora:

names(flora) <- make.unique(names(flora))
df4 <- flora %>% 
  rename_with(
    ~ ifelse(.x %in% excepciones, .x, paste0("G", .x))
  )


base_names4 <- names(df4) %>% 
  keep(~ str_starts(.x, "G")) %>% 
  str_remove("\\.\\d+$") %>% 
  unique()

# Generar solo las columnas sumadas
df_sumado4 <- map_dfc(base_names4, function(colbase) {
  cols <- df4 %>% select(matches(paste0("^", colbase, "(\\.|$)")))
  
  tibble(!!colbase := rowSums(cols != "" & !is.na(cols), na.rm = TRUE))
})

df_final4 <- df4 %>% 
  select(-starts_with("G")) %>% 
  bind_cols(df_sumado4)

df_final4 %>%
  pivot_longer(cols = starts_with("G")) %>%
  filter(value > 0) -> db_flora


# Unir las tablas:

db_general <- rbind(db_rept_anf, db_aves, db_mamiferos, db_flora)


df_completo <- db_general %>%
  left_join(estudios, by = "name") %>% 
  select(-value)


View(df_modulo)


comparacion <- full_join(
  db_general %>% 
    select(ESPECIES) %>% 
    distinct() %>% 
    mutate(en_df1 = TRUE),
  
  df_modulo %>% 
    select(Nombre.Científico) %>% 
    distinct() %>% 
    mutate(en_df2 = TRUE),
  
  by = c("ESPECIES" = "Nombre.Científico")
)


EJERCICIO <-comparacion %>% filter(is.na(en_df1)| is.na(en_df2))
seguimiento <- EJERCICIO %>% filter(en_df2 == TRUE)

write.xlsx(df_completo, "base_data_especies.xlsx")
write.xlsx(seguimiento, "seguimiento.xlsx")
write.xlsx(db_general %>% select(-c(name, value)) %>% distinct(), "base_solo_especies.xlsx")
write.xlsx(estudios, "base_solo_estudios.xlsx")
