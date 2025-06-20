library(dplyr)
library(sf)

# SLE Admin 3+4 from epicentre outbreak tools geobase sharepoint
shp_dir <- max(fs::dir_ls(fs::path(Sys.getenv("SHAREPOINT_PATH"), "OutbreakTools - GeoBase", "SLE")))
shp_paths <- glue::glue("{shp_dir}/sf/SLE_adm{3:4}.rds")
sf_sle <- purrr::set_names(shp_paths, c("adm3", "adm4")) %>% purrr::map(readr::read_rds)

# simulated ebola outbreak from outbreaks package
df_ll_raw <- tibble(outbreaks::ebola_sim_clean$linelist) %>%
  group_by(gender) %>%
  group_split() %>%
  purrr::map2(c(5, 15), \(x, y) {
    mutate(
      x,
      age = round(abs(rnorm(n(), mean = y, sd = 15))),
      .after = outcome
    )
  }) %>%
  bind_rows()

sf_gps <- df_ll_raw %>%
  select(case_id, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# filter boundaries to those with incidence
sf_sle$adm3 <- sf_sle$adm3 %>% st_filter(sf_gps)
sf_sle$adm4 <- sf_sle$adm4 %>% st_filter(sf_sle$adm3)

# geo join admin 3 and 4 pcodes to linelist for place map module examples
geo_join <- sf_gps %>%
  st_join(select(sf_sle$adm3, adm3_pcode = pcode)) %>%
  st_join(select(sf_sle$adm4, adm4_pcode = pcode)) %>%
  st_drop_geometry()

df_ll_ebola <- df_ll_raw %>% left_join(geo_join, by = "case_id")

usethis::use_data(df_ll_ebola, sf_sle, overwrite = TRUE)
