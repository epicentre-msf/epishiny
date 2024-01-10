library(dplyr)
library(sf)

# SLE Admin 3+4 from epicentre outbreak tools geobase sharepoint
shp_dir <- max(fs::dir_ls(path(Sys.getenv("SHAREPOINT_PATH"), "OutbreakTools - GeoBase", "SLE")))
shp_paths <- glue::glue("{shp_dir}/sf/SLE_adm{3:4}.rds")
sf_sle <- purrr::set_names(shp_paths, c("adm3", "adm4")) %>% purrr::map(readr::read_rds)

# fictional ebola outbreak from outbreaks package, expanded by applied epi handbook team
ll_path <- "https://github.com/appliedepi/epirhandbook_eng/raw/master/data/case_linelists/linelist_cleaned.rds"
df_ll_raw <- readr::read_rds(ll_path)

# geo join admin 3 and 4 pcodes to linelist for place map module examples
geo_join <- df_ll_raw %>%
  select(case_id, lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_join(select(sf_sle$adm3, adm3_pcode = pcode)) %>%
  st_join(select(sf_sle$adm4, adm4_pcode = pcode)) %>%
  st_drop_geometry()

df_ll_ebola <- df_ll_raw %>% left_join(geo_join, by = "case_id")

# geo_data <- list(
#   epishiny::geo_layer(
#     layer_name = "Admin 3",
#     sf = sf_sle$adm3,
#     name_var = "adm3_name",
#     join_by = c("pcode" = "adm3_pcode")
#   ),
#   epishiny::geo_layer(
#     layer_name = "Admin 4",
#     sf = sf_sle$adm4,
#     name_var = "adm4_name",
#     join_by = c("pcode" = "adm4_pcode")
#   )
# )

usethis::use_data(df_ll_ebola, sf_sle, overwrite = TRUE)
