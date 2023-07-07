
leaf_basemap <- function(
    bbox,
    baseGroups = c("Light", "OSM", "OSM HOT"),
    overlayGroups = c("Boundaries", "Taux d'attaque"),
    miniMap = TRUE
) {
  lf <- leaflet::leaflet() %>%
    leaflet::fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) %>%
    leaflet::addMapPane(name = "boundaries", zIndex = 300) %>%
    leaflet::addMapPane(name = "choropleth", zIndex = 310) %>%
    leaflet::addMapPane(name = "circles", zIndex = 410) %>%
    leaflet::addMapPane(name = "region_highlight", zIndex = 420) %>%
    leaflet::addMapPane(name = "place_labels", zIndex = 320) %>%
    leaflet::addProviderTiles("CartoDB.PositronNoLabels", group = "Light") %>%
    leaflet::addProviderTiles(
      "CartoDB.PositronOnlyLabels",
      group = "Light",
      options = leaflet::leafletOptions(pane = "place_labels")
    ) %>%
    leaflet::addProviderTiles("OpenStreetMap", group = "OSM") %>%
    leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM HOT") %>%
    leaflet::addScaleBar(
      position = "bottomright",
      options = leaflet::scaleBarOptions(imperial = FALSE)
    ) %>%
    leaflet.extras::addResetMapButton() %>%
    leaflet::addLayersControl(
      baseGroups = baseGroups,
      overlayGroups = overlayGroups,
      position = "topleft"
    )

  if (miniMap) {
    lf <- lf %>% leaflet::addMiniMap(toggleDisplay = TRUE, position = "bottomleft")
  }

  return(lf)
}

tag_map_title <- tags$style(HTML("
  .leaflet-control.map-title {
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px;
    padding-right: 10px;
    color: rgba(85, 85, 85);
    font-size: 18px;
    font - weight:bold
    background: rgba(255,255,255,0.8);
    box-shadow: 0 0 15px rgba(0,0,0,0.2);
    border-radius: 5px;
  }
"))