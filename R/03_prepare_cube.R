################################################################################
#
#   hexsticker creation for r-package
#
#    _ __ ___   __ _ _ ____   _(_) _____      __
#   | '_ ` _ \ / _` | '_ \ \ / / |/ _ \ \ /\ / /
#   | | | | | | (_| | |_) \ V /| |  __/\ V  V /
#   |_| |_| |_|\__,_| .__/ \_/ |_|\___| \_/\_/
#                   |_|
#
#   Author(s): Tim Appelhans, Christoph Stepper
#   Date:      2018-12-09
#
#   Part 3: prepare cube
#
################################################################################



################################################################################
# 0.install and load packages ----
################################################################################

library(magick)
library(gifski)
library(tidyverse)

################################################################################
# 1. resize all sides to have equal size ----
################################################################################

# see here for help on imagemagick
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html

qsc_sides_png = fs::dir_ls(path = "imagery/qsc_sides/",
                       recursive = TRUE, glob = "*.tiff")

# read tile images
qsc_sides_img = map(.x = qsc_sides_png, .f = image_read)

# get info about images
qsc_sides_info = qsc_sides_img %>%
  map_df(.x = ., .f = image_info)

# extract minimal size
min_size = qsc_sides_info %>% select(width, height) %>% min()

# resize all images
qsc_sides_img_rs = qsc_sides_img %>%
  map(.x = .,
      .f = image_resize, geometry = paste0(min_size, "x", min_size, "!"))

# write resized images to disk
qsc_sides_img_rs %>%
  set_names(x = ., ~ str_replace(string = ., pattern = ".tiff", replacement = "_rs.png")) %>%
  iwalk(.x = ., .f = function(x, y) image_write(image = x, path = y, format = "png"))


################################################################################
# 2. fill gaps on north and south pole and add text ----
################################################################################

left = image_read("imagery/qsc_sides/Esri.WorldImagery_z3/left_rs.png")
top = image_read("imagery/qsc_sides/OpenTopoMap_z3/top_rs.png")
front = image_read("imagery/qsc_sides/CartoDB.DarkMatter_z3/front_rs.png")

xy = image_info(top)$width

# this is currently done manually using paint.net
left <- image_draw(left)
text(x = 100, y = 20, labels = "map",
     adj = c(1, 0.5), font = 2, family = "FreeMono",
     cex = 25, srt = 90, col = "#ffffff88")
dev.off()

top <- image_draw(top)
rect(xleft = xy / 2 - 45,
     ybottom = xy / 2 - 45,
     xright = xy / 2 + 45,
     ytop = xy / 2 + 45,
     col = "#97d2e2",
     border = "transparent")
# text(xy / 2 + 4, 96, adj = c(0.5, 0.5), "view", font = 2, family = "FreeMono",
#      cex = 25, srt = 0, col = "#4d4d4d")
text(20, 100, adj = c(0, 0.5), "view", font = 2, family = "FreeMono",
     cex = 25, srt = 0, col = "#00000088")
dev.off()

front = image_annotate(front,
                       text = "Leaflet | © OpenStreetMap © CartoDB",
                       gravity = "southwest",
                       location = "+10+20",
                       size = 30,
                       color = "#4d4d4d")

image_write(top, path = "imagery/qsc_sides/OpenTopoMap_z3/top_rs_txt.png")
image_write(left, path = "imagery/qsc_sides/Esri.WorldImagery_z3/left_rs_txt.png")
image_write(front, path = "imagery/qsc_sides/CartoDB.DarkMatter_z3/front_rs_txt.png")

################################################################################
# 4. create 3d box ----
################################################################################

# see here for info on the 3Dbox tool
# http://www.fmwconcepts.com/imagemagick/3Dbox/

# see here for how to run IM bash scripts on Windows
# http://im.snibgo.com/cygwin.htm#prebuilt

infiles = c("imagery/qsc_sides/CartoDB.DarkMatter_z3/front_rs_txt.png",
            "imagery/qsc_sides/Esri.WorldImagery_z3/left_rs_txt.png",
            "imagery/qsc_sides/OpenTopoMap_z3/top_rs_txt.png",
            "imagery/qsc_sides/OpenStreetMap_z3/back_rs.png",
            "imagery/qsc_sides/CartoDB.Positron_z3/right_rs.png",
            "imagery/qsc_sides/OpenStreetMap_z3/bottom_rs.png")

outfile = "imagery/3Dbox/box.png"

panvals = c(seq(45, 360, 1), seq(1, 44, 1))

for (i in seq(panvals)) {
  print(i)
  system(
    paste0(
      'bash 3Dbox bgcolor="#ffffff00" pan=', panvals[i],
      ' tilt=-35 pef=0 filter=point format="center" ',
      paste0(infiles, collapse = " "),
      " ",
      gsub(".png", paste0(sprintf("%03.0f", i), ".png"), outfile)
    )
  )
}

png_fls = list.files("imagery/3Dbox", full.names = TRUE)

img1 = image_read(png_fls[1])

x = image_info(img1)$width
y = image_info(img1)$height

gif_file = gifski(png_files = png_fls,
                  gif_file = "imagery/animated/box_anim.gif",
                  width = x * 0.25,
                  height = y * 0.25,
                  delay = 0.05)

# clean up
unlink(png_fls)
unlink(list.files("imagery/healpix", full.names = TRUE))

# I can't get it running on Windows...
# this has to be executed on a Linux machine ...
# system(paste('sudo -kS 3Dbox pan=45 tilt=-35 pef=0 filter=point format="center"',
#               paste0(infiles, collapse = " "), outfile),
#        input = rstudioapi::askForPassword("sudo password"))

# system("sudo -kS bash 3Dbox_anim_pan",
#        input = rstudioapi::askForPassword("sudo password"))
