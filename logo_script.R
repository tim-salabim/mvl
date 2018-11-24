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
      ymin = -87.5,
      ymax = 87.5
    ),
    crs = st_crs("+proj=longlat +ellps=WGS84")
  )

bb_tile_query(bbox)
tile_grid = bb_to_tg(bbox, zoom = 3, max_tiles = 80)


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


## create projected cube tiles
# front
system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/front.tiff')

# right
system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=90" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/right.tiff')

# left
system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=-90" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/left.tiff')

# back
system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=180" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/back.tiff')

# top
system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=90 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/top.tiff')

# bottom
system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=-90 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/bottom.tiff')


## create box
system('sudo -kS 3Dbox pan=45 tilt=-45 pef=0 filter=point world_imagery/sides/right.tiff world_imagery/sides/front.tiff world_imagery/sides/top.tiff world_imagery/box.png',
       input = rstudioapi::askForPassword("sudo password"))
