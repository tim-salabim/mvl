library(mapview)
library(sf)
library(slippymath)
library(raster)
library(rgdal)
library(purrr)
library(glue)
library(curl)
library(raster)

# gdalwarp for cube projection
# https://proj4.org/operations/projections/qsc.html

# imagemagick script for 3d box
# http://www.fmwconcepts.com/imagemagick/3Dbox/index.php

# download map tiles

bbox =
  st_bbox(
    c(
      xmin = -179,
      xmax = 179,
      ymin = -85,
      ymax = 85
    ),
    crs = st_crs("+proj=longlat +ellps=WGS84")
  )

bb_tile_query(bbox)
tile_grid = bb_to_tg(bbox, zoom = 3, max_tiles = 64)


images <-
  pmap(tile_grid$tiles,
       function(x, y, zoom){
         outfile <- glue("world_imagery/tiles/tile_{x}_{y}.jpg")
         curl_download(url = glue('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{zoom}/{y}/{x}'),
                       destfile = outfile)
         outfile
       },
       zoom = tile_grid$zoom)



raster_out <- tg_composite(tile_grid, images)

## A convenient wrapper for raster image exports.
raster_to_png(raster_out, "world_imagery/world.png")
