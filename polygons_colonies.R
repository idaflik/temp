library(needs)
needs(tidyverse,
      sf,
      ggplot2,
      rnaturalearth,
      osmdata,
      geojsonrewind,
      jsonlite,
      lawn)

## to do: use this tool to rewind everything: https://observablehq.com/@bumbeishvili/rewind-geojson

## to do: add arguin to background countries
## brandenburg gold coast and tianjin are too small; think of solution

data <- read_csv("input/colonies_details.csv")

countries <- ne_countries(returnclass = "sf", scale = "medium") %>%
  dplyr::select(name_long)

fn <- "current_borders.geojson"
if(file.exists(fn)){
  file.remove(fn)
}
write_sf(countries, fn)

# borders_1900 <- st_read("https://raw.githubusercontent.com/aourednik/historical-basemaps/master/geojson/world_1900.geojson")
borders_1914 <- st_read("https://raw.githubusercontent.com/aourednik/historical-basemaps/master/geojson/world_1914.geojson")
# borders_1700 <- st_read("https://raw.githubusercontent.com/aourednik/historical-basemaps/master/geojson/world_1700.geojson")

## 1914
found_1914 <- borders_1914 %>%
  filter(grepl("Togoland|German.*Africa|Kamerun", NAME))%>%
  st_transform(4326)

colonies <- found_1914 %>%
              dplyr::select(name = NAME)

found_today <- ne_countries(returnclass = "sf", scale = "medium") %>%
  filter(grepl("Mariana|Palau|Marshall|Nauru|Samoa|Solomon|Micronesia", admin))%>%
  filter(admin != "American Samoa")%>%
  st_transform(4326)
## Samoa is what used to be Western Samoa (German colony), American Samoa was colonized by US
## Mariana Islands are Northern Mariana Islands (German colony), Southern islands are Guam (US colony)

colonies <- colonies %>%
  bind_rows(found_today %>%
              dplyr::select(name = admin))

## finalize / add info

final <- colonies %>%
  mutate(name = str_replace_all(name,
                                c("German E\\. Africa \\(Tanganyika\\)" = "German East Africa",
                                  "Northern Mariana Islands" = "Mariana Islands",
                                  "Federated States of Micronesia" = "Caroline Islands",
                                  "Solomon Islands" = "North Solomon Islands")))%>%
  left_join(data)%>%
  st_make_valid()

## import pre-rewind manual geometries

shapes <- st_read("pre_rewind.geojson")

final <- final %>%
  bind_rows(shapes)

fn <-  "test_colonies_pre_rewind.geojson"
if(file.exists(fn)){
  file.remove(fn)
}
write_sf(final, fn)
