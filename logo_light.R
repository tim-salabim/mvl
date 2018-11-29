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

# bbox =
#   st_bbox(
#     c(
#       xmin = -179,
#       xmax = 179,
#       ymin = -85,
#       ymax = 85
#     ),
#     crs = st_crs("+proj=longlat +ellps=WGS84")
#   )
#
# bb_tile_query(bbox)
# tile_grid = bb_to_tg(bbox, zoom = 3, max_tiles = 64)
#
# images <-
#   pmap(tile_grid$tiles,
#        function(x, y, zoom){
#          outfile <- glue("cartodb_positron/tiles/tile_{x}_{y}.png")
#          curl_download(url = glue('https://c.basemaps.cartocdn.com/light_nolabels/{zoom}/{x}/{y}.png'),
#                        destfile = outfile)
#          outfile
#        },
#        zoom = tile_grid$zoom)
#
# png_fls = list.files("cartodb_positron/tiles/", pattern = ".png", full.names = TRUE)
# png_lst = lapply(png_fls, image_read)
# jpg_lst = lapply(png_lst, image_convert, format = "jpg")
#
# lapply(seq(jpg_lst), function(i) {
#   image_write(jpg_lst[[i]], path = gsub(".png", ".jpg", png_fls[i]))
# })
#
# images = gsub(".png", ".jpg", images)
# raster_out <- tg_composite(tile_grid, images)
#
# ## A convenient wrapper for raster image exports.
# raster_to_png(raster_out, "cartodb_positron/world.png")
#
#
# ## create projected cube tiles
# # front
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_positron/world.png cartodb_positron/sides/front.tiff')
#
# # right
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=90" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_positron/world.png cartodb_positron/sides/right.tiff')
#
# # left
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=-90" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_positron/world.png cartodb_positron/sides/left.tiff')
#
# # back
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=180" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_positron/world.png cartodb_positron/sides/back.tiff')
#
# # top
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=90 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_positron/world.png cartodb_positron/sides/top.tiff')
#
# # bottom
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=-90 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_positron/world.png cartodb_positron/sides/bottom.tiff')
#
#
#
# # ## make sure all sides have same dimensions
# fls = list.files("cartodb_positron/sides", full.names = TRUE)
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
right = image_read("cartodb_positron/sides/right_smll.tiff")
bottom = image_read("cartodb_positron/sides/bottom_smll.tiff")
back = image_read("cartodb_positron/sides/back_smll.tiff")

xy = image_info(bottom)$width

# back <- image_draw(back)
# text(10, 100, adj = c(0, 0.5), "view", font = 2, family = "FreeMono",
#      cex = 25, srt = 0, col = "#00ffff")
# dev.off()

bottom <- image_draw(bottom)
lines(x = c(xy / 2, xy / 2), y = c(xy / 2, xy), col = "#d4d9dc", lwd = 2)
rect(xleft = xy / 2 - 45,
     ybottom = xy / 2 - 45,
     xright = xy / 2 + 45,
     ytop = xy / 2 + 45,
     col = "#fafaf8", #"#262626",
     border = "transparent",
     lwd = 0)
# text(60, xy / 2, adj = c(0.5, 0), "view", font = 2, family = "FreeMono",
#      cex = 25, srt = 270, col = "#262626")
dev.off()

bottom = image_annotate(bottom,
                        text = "Leaflet | © OpenStreetMap © CartoDB",
                        gravity = "northwest",
                        location = "+530+40",
                        size = 30,
                        degrees = 180,
                        color = "#fafaf8")

bottom = image_rotate(bottom, 90)

# right = image_draw(right)
# text(x = xy * 1 / 3, y = 100, labels = "m",
#      adj = c(1, 0.5), font = 2, family = "FreeMono",
#      cex = 25, srt = 0, col = "#26262677")
# text(x = xy * 1 / 2, y = 100, labels = "a",
#      adj = c(0.5, 0.5), font = 2, family = "FreeMono",
#      cex = 25, srt = 0, col = "#26262677")
# text(x = xy * 2 / 3, y = 100, labels = "p",
#      adj = c(0, 0.5), font = 2, family = "FreeMono",
#      cex = 25, srt = 0, col = "#26262677")
# dev.off()

right = image_draw(right)
text(x = xy - 20, y = 105, labels = "map",
     adj = c(1, 0.5), font = 2, family = "FreeMono",
     cex = 25, srt = 0, col = "#6666ff")
dev.off()

back = image_draw(back)
text(20, 100, adj = c(0, 0.5), "view", font = 2, family = "FreeMono",
     cex = 25, srt = 0, col = "#6666ff")
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

image_write(bottom, path = "cartodb_positron/sides/bottom_smll_txt.tiff")
image_write(right, path = "cartodb_positron/sides/right_smll_txt.tiff")
image_write(back, path = "cartodb_positron/sides/back_smll_txt.tiff")

## create box
system('sudo -kS 3Dbox bgcolor="#ffffff00" pan=-45 tilt=35 pef=0 filter=point cartodb_positron/sides/right_smll_txt.tiff cartodb_positron/sides/back_smll_txt.tiff cartodb_positron/sides/bottom_smll_txt.tiff cartodb_positron/box_mapview.png',
       input = rstudioapi::askForPassword("sudo password"))

# some post processing
box_mv = image_read("cartodb_positron/box_mapview.png")
box_mv = image_trim(box_mv, fuzz = 80)

x = image_info(box_mv)$width
y = image_info(box_mv)$height

box_mv = image_draw(box_mv)
lines(x = c(1, x / 2), y = c(959, y / 2),
      col = "#fafaf8", lwd = 1)
lines(x = c(x / 2, x - 1), y = c(y / 2, 959),
      col = "#fafaf8", lwd = 1)
lines(x = c(x / 2, x / 2), y = c(0, y / 2),
      col = "#fafaf8", lwd = 1)
dev.off()

# resize and save
wdth = 1.73*300
hght = 2*300

box_mv = image_resize(box_mv, paste0(wdth, "x", hght, "!"))

image_write(box_mv, path = "cartodb_positron/logo_light.png")
