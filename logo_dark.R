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

## function to increase space between letters (as "map" is shorter than "view")
## modified from https://stackoverflow.com/questions/18345196/reduce-the-spacing-between-letters-in-the-text-function
squishtext <- function(x, y, text, squish=1, direction = "x", ...) {
  if (direction == "x") {
    text <- strsplit(text, '')[[1]]
    w <- strwidth(text)
    ww <- cumsum(c(0,head(w,-1)) * squish)
    return(text(x + ww, y, text, ...))
  }
  if (direction == "y") {
    text <- strsplit(text, '')[[1]]
    w <- strwidth(text)
    ww <- cumsum(c(0,head(w,-1)) * squish)
    return(text(x, y + ww, text, ...))
  }
}

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
#          outfile <- glue("cartodb_darkmatter/tiles/tile_{x}_{y}.png")
#          curl_download(url = glue('https://c.basemaps.cartocdn.com/dark_nolabels/{zoom}/{x}/{y}.png'),
#                        destfile = outfile)
#          outfile
#        },
#        zoom = tile_grid$zoom)
#
# png_fls = list.files("cartodb_darkmatter/tiles", pattern = ".png", full.names = TRUE)
# png_lst = lapply(png_fls, image_read)
# jpg_lst = lapply(png_lst, image_convert, format = "jpg", type = "truecolor")
#
# lapply(seq(jpg_lst), function(i) {
#   image_write(jpg_lst[[i]], path = gsub(".png", ".jpg", png_fls[i]))
# })
#
# images = gsub(".png", ".jpg", images)
# raster_out <- tg_composite(tile_grid, images)
#
# ## A convenient wrapper for raster image exports.
# raster_to_png(raster_out, "cartodb_darkmatter/world.png")
#
#
# ## create projected cube tiles
# # front
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_darkmatter/world.png cartodb_darkmatter/sides/front.tiff')
#
# # right
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=90" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_darkmatter/world.png cartodb_darkmatter/sides/right.tiff')
#
# # left
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=-90" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_darkmatter/world.png cartodb_darkmatter/sides/left.tiff')
#
# # back
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=0 +lon_0=180" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_darkmatter/world.png cartodb_darkmatter/sides/back.tiff')
#
# # top
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=90 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_darkmatter/world.png cartodb_darkmatter/sides/top.tiff')
#
# # bottom
# system('gdalwarp -t_srs "+wktext +proj=qsc +units=m +ellps=WGS84  +lat_0=-90 +lon_0=0" -wo SOURCE_EXTRA=100 -wo SAMPLE_GRID=YES -te -6378137 -6378137 6378137 6378137 cartodb_darkmatter/world.png cartodb_darkmatter/sides/bottom.tiff')
#
#
#
# # ## make sure all sides have same dimensions
# fls = list.files("cartodb_darkmatter/sides", full.names = TRUE)
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
left = image_read("cartodb_darkmatter/sides/left_smll.tiff")
top = image_read("cartodb_darkmatter/sides/top_smll.tiff")
front = image_read("cartodb_darkmatter/sides/front_smll.tiff")

xy = image_info(top)$width

# left <- image_draw(left)
# text(x = 96, y = xy * 2 / 3 + 4, labels = "m",
#      adj = c(1, 0.5), font = 2, family = "FreeMono",
#      cex = 25, srt = 90, col = "#4d4d4d")
# text(x = 100, y = xy * 2 / 3, labels = "m",
#      adj = c(1, 0.5), font = 2, family = "FreeMono",
#      cex = 25, srt = 90, col = "#00ffff")
# text(x = 96, y = xy * 1 / 2 + 4, labels = "a",
#      adj = c(0.5, 0.5), font = 2, family = "FreeMono",
#      cex = 25, srt = 90, col = "#4d4d4d")
# text(x = 100, y = xy * 1 / 2, labels = "a",
#      adj = c(0.5, 0.5), font = 2, family = "FreeMono",
#      cex = 25, srt = 90, col = "#00ffff")
# text(x = 96, y = xy * 1 / 3 + 4, labels = "p",
#      adj = c(0, 0.5), font = 2, family = "FreeMono",
#      cex = 25, srt = 90, col = "#4d4d4d")
# text(x = 100, y = xy * 1 / 3, labels = "p",
#      adj = c(0, 0.5), font = 2, family = "FreeMono",
#      cex = 25, srt = 90, col = "#00ffff")
# dev.off()

left <- image_draw(left)
text(x = 100, y = 20, labels = "map",
     adj = c(1, 0.5), font = 2, family = "FreeMono",
     cex = 25, srt = 90, col = "#00ffff")
dev.off()

top <- image_draw(top)
rect(xleft = xy / 2 - 45,
     ybottom = xy / 2 - 45,
     xright = xy / 2 + 45,
     ytop = xy / 2 + 45,
     col = "#262626",
     border = "transparent")
# text(xy / 2 + 4, 96, adj = c(0.5, 0.5), "view", font = 2, family = "FreeMono",
#      cex = 25, srt = 0, col = "#4d4d4d")
text(20, 100, adj = c(0, 0.5), "view", font = 2, family = "FreeMono",
     cex = 25, srt = 0, col = "#00ffff")
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
                       size = 30,
                       color = "#4d4d4d")

image_write(top, path = "cartodb_darkmatter/sides/top_smll_txt.tiff")
image_write(left, path = "cartodb_darkmatter/sides/left_smll_txt.tiff")
image_write(front, path = "cartodb_darkmatter/sides/front_smll_txt.tiff")

## create box
system('sudo -kS 3Dbox bgcolor="#ffffff00" pan=45 tilt=-35 pef=0 filter=point cartodb_darkmatter/sides/front_smll_txt.tiff cartodb_darkmatter/sides/left_smll_txt.tiff cartodb_darkmatter/sides/top_smll_txt.tiff cartodb_darkmatter/box_mapview.png',
       input = rstudioapi::askForPassword("sudo password"))

# some post processing
box_mv = image_read("cartodb_darkmatter/box_mapview.png")
box_mv = image_trim(box_mv, fuzz = 80)

x = image_info(box_mv)$width
y = image_info(box_mv)$height

box_mv = image_draw(box_mv)
lines(x = c(1, x / 2), y = c(319, y * 1 / 2 - 3),
      col = "#090909", lwd = 1)
lines(x = c(x / 2, x - 1), y = c(y * 1 / 2 - 3, 319),
      col = "#090909", lwd = 1)
lines(x = c(x / 2, x / 2), y = c(y * 1 / 2 - 3, y - 1),
      col = "#090909", lwd = 1)
dev.off()

# resize and save
wdth = 1.73*300
hght = 2*300

box_mv = image_resize(box_mv, paste0(wdth, "x", hght, "!"))

image_write(box_mv, path = "cartodb_darkmatter/logo_dark.png")
