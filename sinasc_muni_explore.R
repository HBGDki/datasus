source("_fns.R")
library(ggplot2)
library(ggthemes)

load("data/geo/state_muni_codes.Rdata")
load("data/geo/state_muni_shapefiles.Rdata")
load("data/artifacts/muni_summaries.Rdata")

ggplot(brthwt_muni_region, aes(rank2, mean_bwt, ymin = q1, ymax = q3, color = region_name)) +
  geom_linerange(color = "gray", alpha = 0.2) +
  geom_point() +
  theme_bw() +
  coord_flip()

ggplot(brthwt_muni_region, aes(rank, mean_bwt, color = region_name)) +
  # geom_linerange(aes(ymin = q1, ymax = q3), color = "gray", alpha = 0.2) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ 1, se = FALSE) +
  theme_bw() +
  scale_color_tableau(guide = FALSE) +
  coord_flip() +
  facet_wrap(~ region_name, scales = "free_y", ncol = 1, strip.position = "left") +
  labs(y = "Mean Birthweight", x = "Municipality Rank (within region)")

library(leaflet)

Sys.setenv(MAPBOX_TOKEN = "pk.eyJ1IjoicmhhZmVuIiwiYSI6ImNpdnY5M25oaDAwc24yb281cnFoY3g2YTYifQ.aSlJqMyxuFCtaP6euwu-QA")

pal <- colorFactor(palette = scale_color_tableau()$palette(5),
  levels = levels(brthwt_muni_region$region_code))
labels <- sprintf(
  "<strong>Region: %s</strong><br/>State: %s",
  br_shp$REGIAO, br_shp$ESTADO
) %>% lapply(htmltools::HTML)

leaflet(br_shp) %>%
  setView(
    sum(range(sp::coordinates(br_shp)[, 1])) / 2,
    sum(range(sp::coordinates(br_shp)[, 2])) / 2,
    4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(REGIAO),
    weight = 1,
    opacity = 1,
    color = ~pal(REGIAO),
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addLegend(pal = pal, values = ~REGIAO, opacity = 0.7, title = "Region",
    position = "bottomright")


# rbokeh::widget2gist('
pal <- colorNumeric("viridis", domain = range(muni_shps$mean_bwt, na.rm = TRUE))
pal(muni_shps$mean_bwt)

labels <- sprintf(
  "<strong>%s</strong><br/>State: %s<br/>Mean birth weight: %g<br/>Number of births: %g",
  muni_shps$NOME, muni_shps$state_name, muni_shps$mean_bwt, muni_shps$n
) %>% lapply(htmltools::HTML)

p <- leaflet(muni_shps) %>%
  setView(
    sum(range(sp::coordinates(muni_shps)[, 1])) / 2,
    sum(range(sp::coordinates(muni_shps)[, 2])) / 2,
    4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(mean_bwt),
    weight = 1,
    opacity = 1,
    color = ~pal(mean_bwt),
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  addPolylines(
    data = br_shp,
    weight = 2,
    color = "black"
  ) %>%
  addLegend(pal = pal, values = ~mean_bwt, opacity = 0.7, title = "Birth Weight (g)",
    position = "bottomright")
# ', "Brazil Birth Weight by Municipality")

htmlwidgets::saveWidget(p,
  file = file.path(getwd(), "writeup/maps/muni_brthwt.html"), selfcontained = FALSE)
