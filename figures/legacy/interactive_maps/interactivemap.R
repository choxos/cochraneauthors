library(highcharter)

data(GNI2014, package = "treemap")

hcmap(
  "custom/world-robinson-lowres", 
  data = GNI2014,
  name = "Gross national income per capita", 
  value = "GNI",
  borderWidth = 0,
  nullColor = "#d3d3d3",
  joinBy = c("iso-a3", "iso3")
) %>%
  hc_colorAxis(
    stops = color_stops(colors = viridisLite::inferno(10, begin = 0.1)),
    type = "logarithmic"
  ) 


top_first$country <- as.character(top_first$country)
top_first$country[top_first$country == "United States"] <- "United States of America"
top_first$country[top_first$country == "UK"] <- "United Kingdom"
top_first$country <- as.factor(top_first$country)

hcmap(
  "custom/world-robinson-lowres", 
  data = top_first,
  name = "Total first authors", 
  value = "frequency",
  borderWidth = 0,
  nullColor = "#d3d3d3",
  joinBy = c("name", "country")
) %>%
  hc_colorAxis(
    stops = color_stops(colors = RColorBrewer::brewer.pal(9, "YlOrRd")),
    type = "logarithmic"
  ) 


