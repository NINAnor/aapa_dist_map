#
#
#
#
#
#
#
library(tidyverse)
#library(dbConnect)
library(dbplyr)
library(DBI)
library(RPostgres)
library(stars)
library(sf)
library(terra)
library(tmap)
#
#
#
#
#
# the password is stored in a pgpass.conf file
con <- DBI::dbConnect(drv = RPostgres::Postgres(), host = "t2lippgsql03", dbname = "ano_moduler")
#
#
#
#
#
bc <- dplyr::tbl(con, dbplyr::in_schema("helper_variables", "bioclimatic_regions"))

bc <- bc |>
  mutate(geom_wkb = dbplyr::sql("ST_AsBinary(geom)")) |>
  select(-geom) |>
  collect() |>
  mutate(geom = sf::st_as_sfc(geom_wkb, crs = 3035)) |>
  sf::st_as_sf(sf_column_name = "geom")

#
#
#
#
#
reg <- unique(bc$BCregion)
# we exclude boreonemoral and south boreal sones (6SO1-2 and 6SE1)
exl <- grep("6SO-1-2|6SE-1", reg)

# 1 = presence; 0 = absence
bc2 <- bc |>
  mutate(aapa = case_when(
    BCregion %in% reg[exl] ~ 0,
    .default = 1
  ))

# test:
bc2 |> as_tibble() |> count(aapa)
# OK
#
#
#
#
bc2 |>
  slice_sample(prop = 0.2) |>
  tmap::tm_shape() +
  tmap::tm_fill(col = "aapa", style = "cat")
#
#
#
#
temp <- bc2 |>
    mutate(km2 = units::drop_units(st_area(geom))*10^-6)

summary(temp |> as_tibble() |> pull(km2))
# area is 1 km1
#
#
#
# defining a summary function
dplyr_summary <- function(data, var) {
  if (inherits(data, "sf")) {
    data <- sf::st_drop_geometry(data)
  }
  data %>%
    summarise(
      Min    = min({{ var }}, na.rm = TRUE),
      Q1     = quantile({{ var }}, 0.25, na.rm = TRUE),
      Median = median({{ var }}, na.rm = TRUE),
      Mean   = mean({{ var }}, na.rm = TRUE),
      Q3     = quantile({{ var }}, 0.75, na.rm = TRUE),
      Max    = max({{ var }}, na.rm = TRUE)
    )
}

#
#
#
#
grid <- readRDS("data/grid50_kyst_bgr.rds")

# Test that area is 10 km2
grid |>
  mutate(km2 = units::drop_units(st_area(geometry))*10^-6) |>
  dplyr_summary(km2)

# the area varies a bit.
#
#
#
#
#
# set resolution
rr <- 10000
# Get bbox
bb <- st_bbox(grid) 

# create regular grid frm bbox
gr0 <- rast(xmin= floor(bb["xmin"]/rr)*rr, xmax= ceiling(bb["xmax"]/rr)*rr, 
              ymin= floor(bb["ymin"]/rr)*rr, ymax= ceiling(bb["ymax"]/rr)*rr, 
              res= rr, crs= "EPSG:3035")
ret <- rasterize(grid, gr0, cover=T, touches=T) # (for very small polygons touches=TRUE is needed, otherwise FALSE)

class(ret)
#
#
#
#
bc_raster_grid <- rasterize(bc2, ret, fun = "max", field = "aapa") 
res(bc_raster_grid) # OK
#
#
#
plot(bc_raster_grid)
#
#
#
summary(bc_raster_grid)
st_crs(bc_raster) # UTM  - OK

```
#
bc_raster10 <- st_warp(
  bc_raster,
  cellsize = 10000,
  method = "max",
  use_gdal = TRUE
)
plot(bc_raster10)
#
#
#
#
# the area of each pixle should be should 100 km2
temp <- bc_raster10 |>
    st_as_sf() |>
    mutate(km2 = units::drop_units(st_area(geometry))*10^-6)

summary(temp |> as_tibble() |> pull(km2))
# OK
#
#
#
#
saveRDS(bc_raster10, "output/aapa_dist_map.rds")
write_stars(bc_raster10, "output/aapa_dist_map.tif")

# export fines scale raster
saveRDS(bc_raster, "output/aapa_dist_map_1km2.rds")
write_stars(bc_raster, "output/aapa_dist_map_1km2.tif")


saveRDS(bc2, "output/aapa_dist_map_1km2_vector.rds")
#
#
#
