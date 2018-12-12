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
library(tidyverse)

################################################################################
# 1. resize all sides to have equal size ----
################################################################################

# see here for help on imagemagick
# https://cran.r-project.org/web/packages/magick/vignettes/intro.html

qsc_sides_png = fs::dir_ls(path = "imagery/qsc_sides/",
                       recursive = TRUE, glob = "*.png")

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
  set_names(x = ., ~ str_replace(string = ., pattern = ".png", replacement = "_rs.png")) %>%
  iwalk(.x = ., .f = function(x, y) image_write(image = x, path = y))


################################################################################
# 2. fill gaps on north and south pole ----
################################################################################

# this is currently done manually using paint.net


################################################################################
# 3. add text to the cube sides ----
################################################################################

# tbd - see scripts from Tim :-)


################################################################################
# 4. create 3d box ----
################################################################################

# see here for info on the 3Dbox tool
# http://www.fmwconcepts.com/imagemagick/3Dbox/

# see here for how to run IM bash scripts on Windows
# http://im.snibgo.com/cygwin.htm#prebuilt

infiles = c("imagery/qsc_sides/CartoDB.DarkMatter_z3/front_rs.png",
            "imagery/qsc_sides/OpenStreetMap_z3/left_rs.png.png",
            "imagery/qsc_sides/Esri.WorldImagery_z3/top_rs.png.png",
            "imagery/qsc_sides/CartoDB.DarkMatter_z3/bottom_rs.png",
            "imagery/qsc_sides/OSM.nolabel_z3/right_rs.png.png",
            "imagery/qsc_sides/OpenTopoMap_z3/back_rs.png")

outfile = "imagery/3Dbox/box.gif"

# system(paste0('bash 3Dbox bgcolor="#ffffff00" tilt=-45 pef=0 filter=point format=center ',
#               paste0(infiles, collapse = " "), outfile))

# I can't get it running on Windows...
# this has to be executed on a Linux machine ...
system(paste0('sudo -kS 3Dbox pan=45 tilt=-45 pef=0 filter=point ',
              paste0(infiles, collapse = " "), outfile),
       input = rstudioapi::askForPassword("sudo password"))
