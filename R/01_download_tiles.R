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
#   Date:      2018-11-29
#
#   Part 1: download tiles from different tile servers as used in mapview for basemaps
#
################################################################################

################################################################################
# 0.install and load packages ----
################################################################################

# devtools::install_github("milesmcbain/slippymath")

library(sf)
library(slippymath)
library(mapview)
library(raster)
library(purrr)
library(magick)


################################################################################
# 1. download grids ----
################################################################################

#*******************************************************************************
# 1.1 bounding box ----
#*******************************************************************************

# define bounding box for download area
bbox = st_bbox(
  c(xmin = -180, xmax = 180, ymin = -90, ymax = 90),
  crs = st_crs("+proj=longlat +ellps=WGS84")
)

# bounding box tile query
# see https://wiki.openstreetmap.org/wiki/Zoom_levels
# for examples of areas to represent
bb_tile_query(bbox = bbox, zoom_levels = 1:6)
#   x_min y_min x_max y_max y_dim x_dim total_tiles zoom
#       0     0     1     1     2     2           4    1
#       0     0     3     3     4     4          16    2
#       0     0     7     7     8     8          64    3
#       0     0    15    15    16    16         256    4
#       0     0    31    31    32    32        1024    5
#       0     0    63    63    64    64        4096    6

# bounding box tile grid

# define zoom to use
zoom = 3

tg = bb_to_tg(bbox = bbox, zoom = zoom)



#*******************************************************************************
# 1.2 providers ----
#*******************************************************************************

# see https://leafletjs.com/reference-1.3.4.html#tilelayer to read more on the
# usage of tile layers in leafletjs

# {z}: zoom level
# {x} and {y}: tile coordinates

# CartoDB.Positron = https://c.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png
# CartoDB.BlackMatter = https://c.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}.png
# OpenStreetMap = https://c.tile.openstreetmap.org/{z}/{x}/{y}.png
# Esri.WorldImagery = https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.png
# OpenTopoMap = https://c.tile.opentopomap.org/{z}/{x}/{y}.png

# OSM.nolabels = https://tiles.wmflabs.org/osm-no-labels/{z}/{x}/{y}.png

# mapview basemaps

providers = mapviewOptions()$basemaps %>%
  purrr::set_names() %>%
  as.list()

providers$CartoDB.Positron = "https://c.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}.png"
providers$CartoDB.DarkMatter = "https://c.basemaps.cartocdn.com/dark_nolabels/{z}/{x}/{y}.png"
providers$OpenStreetMap = "https://c.tile.openstreetmap.org/{z}/{x}/{y}.png"
providers$Esri.WorldImagery = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.png"
providers$OpenTopoMap = "https://c.tile.opentopomap.org/{z}/{x}/{y}.png"

# additional map source - OSM without labels
providers$OSM.nolabel = "tiles.wmflabs.org/osm-no-labels/{z}/{x}/{y}.png"



#*******************************************************************************
# 1.3 download different grids ----
#*******************************************************************************

# function for downloading single tile
download_tile = function(provider_url,
                         path,
                         z = 3,
                         x,
                         y,
                         verbose = FALSE) {
  tile_url = glue::glue(provider_url)
  if (verbose == TRUE) {
    cat(crayon::bold("downloading tile: "), tile_url, "\n")
  }

  if (fs::file_exists(path)) return(path)

  curl::curl_download(url = tile_url,
                      destfile = path)
}

# function for downloading all tiles from one provider
download_provider_tiles = function(provider_name,
                                   provider_url,
                                   tilegrid,
                                   dir_dwnld = "imagery/tiles/{provider_name}_z{z}",
                                   ...) {
  z = tilegrid$zoom

  dir_dwnld = glue::glue(dir_dwnld)
  fs::dir_create(dir_dwnld)

  cat(crayon::bold("downloading tiles from:"), provider_name, crayon::bold("into"), dir_dwnld, "\n")

  pmap(.l = tilegrid$tiles,
        function(provider_url, dir_dwnld, x, y, z) {
          path = glue::glue(dir_dwnld, '/tile_{z}_{x}_{y}.jpg')
          download_tile(provider_url = provider_url,
                        x = x, y = y, z = z, path = path, ...)
          path
        },
        z = z,
        provider_url = provider_url,
        dir_dwnld = dir_dwnld)

}


# download all tiles
tiles = imap(.x = providers, .f = function(x, y){
  download_provider_tiles(provider_name = y, provider_url = x, tilegrid = tg, verbose = FALSE)
  })


# Check tiles
sapply(tiles, length)

################################################################################
# 2. compose tiles ----
################################################################################

# convert image tiles to truecolor rgb
tiles = map(.x = tiles,
            .f = function(x) {
              map(.x = x, .f = function(y){
                y %>%
                  image_read() %>%
                  image_convert(format = "jpg", type = "truecolor", colorspace = "rgb") %>%
                  image_write(path = y)})
            })



# compose tiles
composits = map(.x = tiles, .f = tg_composite, tile_grid = tg)


################################################################################
# 3. save to file ----
################################################################################

fs::dir_create("imagery/composits")
iwalk(.x = composits, .f = function(x, y, z) {
  file_path = glue::glue("imagery/composits/{y}_z{z}_world.png")
  cat(crayon::bold("writing composit to: "), file_path, "\n")
  raster_to_png(tile_raster = x,
                file_path = file_path)
}, z = tg$zoom)


