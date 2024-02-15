library(needs)
needs(tidyverse,
      sf,
      raster,
      fasterize,
      ggplot2,
      readxl,
      rnaturalearth,
      osmdata)

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
  filter(grepl("Togoland|German.*Africa|Kamerun", NAME))

colonies <- found_1914 %>%
              dplyr::select(name = NAME)

found_today <- ne_countries(returnclass = "sf", scale = "medium") %>%
  filter(grepl("Mariana|Palau|Marshall|Nauru|Samoa|Solomon", admin))%>%
  filter(admin != "American Samoa")
## Samoa is what used to be Western Samoa (German colony), American Samoa was colonized by US
## Mariana Islands are Northern Mariana Islands (German colony), Southern islands are Guam (US colony)

colonies <- colonies %>%
  bind_rows(found_today %>%
              dplyr::select(name = admin))

## finalize / add info
data <- read_csv("input/colonies_details.csv")

final <- colonies %>%
  mutate(name = str_replace_all(name,
                                c("German E\\. Africa \\(Tanganyika\\)" = "German East Africa",
                                  "Northern Mariana Islands" = "Mariana Islands",
                                  "Solomon Islands" = "North Solomon Islands")))%>%
  left_join(data)

fn <-  "test_colonies.geojson"
if(file.exists(fn)){
  file.remove(fn)
}
write_sf(final, fn)

# ## open street map
# 
# ## st thomas
# st_thomas <- getbb ("Saint Thomas", format_out = "sf_polygon")%>%
#   ## manually filter out the one whose coords match
#   filter(row_number() == 3)
# 
# colonies <- colonies %>%
#   bind_rows(st_thomas%>%
#   mutate(name = "St. Thomas"))
# 
# ## arguin
# arguin <- getbb ("Arguin", format_out = "sf_polygon")%>%
#   mutate(name = "Arguin")
# 
# colonies <- colonies %>%
#   bind_rows(arguin)
# 
# # ## to do
# # ## Tianjin
# # tianjin <- getbb ("Tianjin", format_out = "sf_polygon")%>%
# #   mutate(name = "Tianjin")%>%
# #   group_by(name)%>%
# #   summarise(geometry = st_union(geometry))
# # 
# # colonies <- colonies %>%
# #  bind_rows(tianjin)
# 
# ## Brandenburg Gold Coast / nowadays Princes Town
# # goldcoast <- getbb ("Awiabo Aiyinase", format_out = "sf_polygon")
# ## coords of fort: 4.79044/-2.13250, get buffer
# ahanta <- getbb ("Ahanta", format_out = "sf_polygon")
# circle <- st_buffer(st_sfc(st_point(c(-2.13332,4.79074)), crs = 4326), 1000)
# goldcoast <- st_intersection(ahanta,
#                              circle)%>%
#   mutate(name = "Brandenburg Gold Coast")
# 
# colonies <- colonies %>%
#   bind_rows(goldcoast)
# 
# ## Jiaozhou
# jiaozhou <- getbb ("Jiaozhou", format_out = "sf_polygon")
# ## to do: try and get "Jiaozhou Bay" instead (key = bay)
# # bb <- getbb('China')
# # x <- opq(bbox = bb) %>%
# #   add_osm_feature(key = 'name:en', value = 'Jiaozhou Bay', value_exact = FALSE) %>%
# #   osmdata_sf()
# ## get approximation with buffer
# region <- getbb ("Qingdao", format_out = "sf_polygon")
# circle <- st_buffer(st_sfc(st_point(c(120.25, 36.1106)), crs = 4326), 20000)
# jiaozhou_bay <- st_intersection(borders_today,
#                                 circle)%>%
#   mutate(name = "Jiaozhou Bay")
# 
# colonies <- colonies %>%
#   bind_rows(jiaozhou_bay%>%dplyr::select(name))
# 
# # ## to do: change to use intersection; st_crop() fucks up geojson
# # 
# # ## wituland
# # box = c(ymin = -2.5377, xmin = 40.3988, ymax = -2.3003, xmax = 40.8382)
# # wituland <- st_crop(borders_today, box)%>%
# #   st_make_valid()%>%
# #   mutate(name = "Wituland")
# # 
# # colonies <- colonies %>%
# #   bind_rows(wituland%>%dplyr::select(name))
# # 
# # ## bismarck archipelago & kaiser wilhelmsland
# # box = c(ymin = -0.769, xmin = 153.984, ymax = -6.490, xmax = 141.086)
# # region <- st_crop(borders_today, box)%>%
# #   st_make_valid()%>%
# #   mutate(id = "Bismarck archipelago")%>%
# #   dplyr::select(id)%>%
# #   ## separate multipolygon into individual ones
# #   st_cast("POLYGON")%>%
# #   mutate(area = st_area(geometry))
# # 
# # archipelago <- region %>%
# #   ## filter out largest area (which is mainland)
# #   filter(!(area %in% max(area)))%>%
# #   mutate(name = "Bismarck Archipelago")%>%
# #   group_by(name)%>%
# #   summarise(geometry = st_union(geometry))
# # 
# # colonies <- colonies %>%
# #   bind_rows(archipelago)
# 
# ## THIS IS A DUMMY to start with, the delineation of region is INCORRECT
# k_wilhelmsland <- region %>%
#   ## filter out largest area (which is mainland)
#   filter(area %in% max(area))%>%
#   mutate(name = "Kaiser-Wilhelmsland")
# 
# colonies <- colonies %>%
#   bind_rows(k_wilhelmsland%>%dplyr::select(name))
# 
# 
# ## caroline islands
# ## CHECK HOW micronesia & palau (today) are distributed between palau (colony) and caroline islands (colony)
# ## DUMMY: using micronesia as caroline islands
# caroline <- st_read("input/micronesia.json")%>%
#   mutate(name = "Caroline Islands")
# 
# colonies <- colonies %>%
#   bind_rows(caroline%>%dplyr::select(name))
# 
# ## this approach not working because world is not available in large resolution (scale = 10)
# # box = c(ymin = 0.09, xmin = 135.70, ymax = 9.28, xmax = 163.74)
# # caroline <- st_crop(world, box)%>%
# #   st_make_valid()%>%
# #   st_cast("POLYGON")
# 
# ## welserland
# ## for now use rough drawing from test data, needs to be redrawn!
# test_separate <- st_read("output/rough_colonies_shapes.geojson")
# welserland <- test_separate%>%
#   filter(row_number() == 10)%>%
#   mutate(name = "Welserland")
# 
# colonies <- colonies %>%
#   bind_rows(welserland%>%dplyr::select(name))
