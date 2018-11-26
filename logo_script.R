library(mapview)
library(sf)
library(slippymath)
library(raster)
library(rgdal)
library(purrr)
library(glue)
library(curl)
library(raster)
library(magick)

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
#
# ## A convenient wrapper for raster image exports.
# raster_to_png(raster_out, "world_imagery/world.png")
#
#
# ## create projected cube tiles
# # front
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/front.tiff')
#
# # right
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=90" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/right.tiff')
#
# # left
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=-90" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/left.tiff')
#
# # back
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=180" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/back.tiff')
#
# # top
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=90 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/top.tiff')
#
# # bottom
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=-90 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 world_imagery/world.png world_imagery/sides/bottom.tiff')
#
#
# ## make sure all sides have same dimensions
# fls = list.files("world_imagery/sides", full.names = TRUE)
#
# img_lst = lapply(fls, image_read)
# info = do.call(rbind, lapply(img_lst, image_info))
#
# width = min(info$width)
# height = min(info$height)
#
# xy = min(c(width, height))
#
# # resize all images to min dim
# geom = paste0(xy, "x", xy, "!")
#
# img_lst_smll = lapply(img_lst, image_resize, geometry = geom)
#
# # write resized images to disk
# out_fls = gsub(".tiff", "_smll.tiff", fls)
# lapply(seq(img_lst_smll), function(i) image_write(img_lst_smll[[i]], path = out_fls[i]))

## add text to left and top
# using package magick
left = image_read("world_imagery/sides/left_smll.tiff")
top = image_read("world_imagery/sides/top_smll.tiff")
front = image_read("world_imagery/sides/front_smll.tiff")

xy = image_info(left)$width

left <- image_draw(left)
text(100, 10, adj = c(1, 0.5), "map", font = 2, family = "FreeMono",
     cex = 25, srt = 90, col = "#ffffff55") #"#00ffff55")
dev.off()

top <- image_draw(top)
text(10, 100, adj = c(0, 0.5), "view", font = 2, family = "FreeMono",
     cex = 25, srt = 0, col = "#ffffff55") #"#00ffff55")
dev.off()


# top = image_annotate(top,
#                      text = "view",
#                      gravity = "northwest",
#                      location = "+0+20",
#                      size = 400,
#                      font = "mono",
#                      strokecolor = "black",
#                      color = "#00ffff")
#
# left = image_annotate(left,
#                       text = "map",
#                       gravity = "west",
#                       location = "+200+360",
#                       degrees = 270,
#                       size = 400,
#                       font = "mono",
#                       color = "#00ffff",
#                       strokecolor = "black")

front = image_annotate(front,
                       text = "Leaflet | © OpenStreetMap © CartoDB",
                       gravity = "southwest",
                       location = "+10+20",
                       size = 20,
                       color = "white")

image_write(top, path = "world_imagery/sides/top_smll_txt.tiff")
image_write(left, path = "world_imagery/sides/left_smll_txt.tiff")
image_write(front, path = "world_imagery/sides/front_smll_txt.tiff")

## create box
system('sudo -kS 3Dbox pan=45 tilt=-45 pef=0 filter=point world_imagery/sides/front_smll_txt.tiff world_imagery/sides/left_smll_txt.tiff world_imagery/sides/top_smll_txt.tiff world_imagery/box.png',
       input = rstudioapi::askForPassword("sudo password"))


