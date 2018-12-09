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
#   Part 2: create projected cube sides
#
################################################################################

# cube projection:
# http://wiki.gis.com/wiki/index.php/Cube_projection

# use gdalwarp: image reprojection and warping utility for this task
# see https://www.gdal.org/gdalwarp.html

# target projection: Quadrilateralized Spherical Cube (QSC)
# https://proj4.org/operations/projections/qsc.html
#
# other projections maybe possible:
# gnomonic projection: https://proj4.org/operations/projections/gnom.html
# HEALpix: https://proj4.org/operations/projections/healpix.html



################################################################################
# 0.install and load packages ----
################################################################################

library(gdalUtils)
library(purrr)

################################################################################
# 1. define functions for cube side creation ----
################################################################################

# function to compute one side
gdal_qsc = function(srcfile,
                    dstfile,
                    side = c("front", "right", "back", "left", "top", "bottom")){

  side = match.arg(side)

  projcenter = switch(side,
                      front = "+lat_0=0 +lon_0=0",
                      right = "+lat_0=0 +lon_0=90",
                      back = "+lat_0=0 +lon_0=180",
                      left = "+lat_0=0 +lon_0=-90",
                      top = "+lat_0=90 +lon_0=0",
                      bottom = "+lat_0=-90 +lon_0=0")

  t_srs = glue::glue("+wktext +proj=qsc +units=m +ellps=WGS84 {projcenter}")

  gdalwarp(srcfile = srcfile, dstfile = as.character(dstfile),
           t_srs = as.character(t_srs),
           te = c(-6378137, -6378137, 6378137, 6378137),
           wo = "SOURCE_EXTRA=1000 SAMPLE_GRID=YES SAMPLE_STEPS=1000"
  )

}


# wrapper function for all sides
gdal_qsc_all = function(srcfile,
                        dir_dstfiles = "imagery/qsc_sides/{provider_name}_{z}",
                        sides = c("front", "right", "back", "left", "top", "bottom")) {

  composit = strsplit(x = fs::path_ext_remove(basename(srcfile)), split = "_")
  provider_name = composit[[1]][1]
  z = composit[[1]][2]

  dir_dstfiles = glue::glue(dir_dstfiles)
  fs::dir_create(dir_dstfiles)

  cat(crayon::bold("creating sides for:"), provider_name, crayon::bold("Output:"), dir_dstfiles, "\n")

  walk(.x = sides, .f = function(side, srcfile, dir_dstfiles) {
    dstfile = glue::glue(dir_dstfiles, "/{side}.png")
    gdal_qsc(srcfile = srcfile, dstfile = dstfile, side = side)
    },
    srcfile = srcfile,
    dir_dstfiles = dir_dstfiles)

}


################################################################################
# 2. reproject world composits ----
################################################################################

composits = fs::dir_ls(path = "imagery/composits/", glob = "*.png$")

#*******************************************************************************
# 2.1 create cube sides for all providers ----
#*******************************************************************************

walk(.x = composits, gdal_qsc_all)


#*******************************************************************************
# 2.2 create healpix for all providers ----
#*******************************************************************************

walk(.x = composits,
     .f = function(x, dir_dstfiles) {
       gdalwarp(srcfile = x,
                dstfile = as.character(here::here(dir_dstfiles, basename(x))),
                t_srs = '+proj=healpix +a=1 +ellps=WGS84 +wktext',
                te = c(-3.14159265, -1.57079632, 3.14159265, 1.57079632),
                wo = "SOURCE_EXTRA=1000 SAMPLE_GRID=YES SAMPLE_STEPS=1000"
       )
     },
     dir_dstfiles = "imagery/healpix"
)
