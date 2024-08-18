#Libraires
library(sf)
library(tigris)
library(tidyverse)
library(stars)
library(rayshader)
library(MetBrewer)
library(colorspace)
library(rgl)

# load kountur data
data <- st_read("C:/Users/kaize/OneDrive/Bureau/Pop_Abidjan/kontur_population_CI_2023.gpkg")
st <- st_read("C:/Users/kaize/OneDrive/Bureau/Pop_Abidjan/Adm_CIV.gpkg")

# load city
Abidjan_ville <- st |>
  filter(NAME_4 == "Abidjan-Ville")

# Visualiser le résultat
Abidjan_ville |>
  ggplot() +
  geom_sf()


# CRS veérification
st_crs(data)
st_crs(Abidjan_ville)

#Transforming Abidjan_ville towards data CRS
Abidjan_ville <- st_transform(Abidjan_ville, st_crs(data))
st_Abidjan_ville <- st_intersection(data, Abidjan_ville)

#define aspect ratio based on bounding box
bb <- st_bbox(st_Abidjan_ville)

bottoml_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(data))

bottoml_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |>
  st_sfc(crs = st_crs(data))

st_Abidjan_ville |>
  ggplot() +
  geom_sf() +
  geom_sf(data = bottoml_left) +
  geom_sf(data = bottoml_right, color = "red")

width <- st_distance(bottoml_left,bottoml_right)

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |>
  st_sfc(crs = st_crs(data))

height <- st_distance(bottoml_left, top_left)

# handle conditions of width or height the longer side
if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ration <- 1
  w_ration <- width / height
}

# convert to raster so we can then convert to matrix
size <- 3000

Abidjan_ville_rast <- st_rasterize(st_Abidjan_ville,
                                   nx = floor(size * w_ratio),
                                   ny = floor(size * h_ratio))

mat <- matrix(Abidjan_ville_rast $population,
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))

# create color palette
color <- met.brewer("Archambault")
swatchplot(color)

texture <- grDevices::colorRampPalette(color, bias = 2)(256)
swatchplot(texture)

# plot that 3d map !
rgl::close3d()

mat |>
  height_shade(texture = texture) |>
  plot_3d(heightmap = mat,
          zscale = 100 / 3,
          solid = TRUE,
          shadowdepth = 0)

render_camera(theta = -20, phi = 45, zoom = .7)
# Afficher la scène 3D dans un widget interactif pour vérifier
rglwidget()

rgl::rgl.snapshot("Save_image/test_pop_Babi3.png")

outfile <- "Save_image/test_pop_Babi2.png"

{
  start_time <- Sys.time()
  cat(crayon::cyan(start_time), "\n")
  if (!file.exists(outfile)) {
    png::writePNG(matrix(1), target = outfile)
  }
render_highquality(
  filename = outfile,
  width = 800,
  height = 600,
  samples = 100,
  interactive = FALSE,
  lightdirection = 280,
  lightaltitude = c(20, 80),
  lightcolor = c(texture[2], "white"),
  lightintensity = c(600, 100)
)
end_time <- Sys.time()
diff <- end_time - start_time
cat(crayon::cyan(diff), "\n")
}
