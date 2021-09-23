#------------------------------------------------------------------------
require(pacman)
pacman::p_load(RColorBrewer, ggspatial, raster,colorspace, ggpubr, sf,openxlsx, tmap)
#------------------------------------------------------------------------
Peru               <- getData('GADM', country='Peru', level=1) %>% st_as_sf()
Cuencas_peru       <- st_read ("SHP/Cuencas_peru.shp")  
Rio_libe           <- st_read ("SHP/RIOS_LA_LIBERTAD_geogpsperu_SuyoPomalia_931381206.shp")  
Cuencas_peru       <- st_transform(Cuencas_peru ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Rio_libe           <- st_transform(Rio_libe ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Cuenca_moche       <- subset(Cuencas_peru , NOMB_UH_N6  == "Moche")
Cuencas_rios       <- st_intersection(Rio_libe, Cuenca_moche)

dem = raster("raster/ASTGTM_S08W079_dem.tif")
dem2 = raster("raster/ASTGTM_S09W079_dem.tif")
dem3 = raster("raster/ASTGTM_S09W080_dem.tif")
DEM_total<- raster::merge(dem, dem2,dem3)

Cuenca_moche_alt     <- crop(DEM_total, Cuenca_moche)
Cuenca_moche_alt     <- Cuenca_moche_alt  <- mask(Cuenca_moche_alt , Cuenca_moche)
plot(Cuenca_moche_alt )

slope = terrain(Cuenca_moche_alt, opt = "slope") 
aspect = terrain(Cuenca_moche_alt, opt = "aspect")
hill = hillShade(slope, aspect, angle = 40, direction = 270)


map = tm_shape(Cuenca_moche_alt ) + 
  tm_raster(style = "quantile", n = 6, title = "Histograma \n(data distribucion)",
            palette = terrain.colors(256), legend.show = T, legend.hist = T, legend.hist.z=0)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "darkgoldenrod1", color.dark = "lightsteelblue4", 
               position = c("left", "bottom"), lwd = 1, color.light= "white")+
  tm_compass(type="radar", position=c("right", "top"), text.color = "white")+
  tm_style("cobalt")+
  tm_layout(title = "Mapa de \nElevacion, Moche", legend.title.size=.8,legend.text.size = 0.5,
            legend.position = c("right", "bottom") ,
            legend.hist.width = 0.2,
            legend.hist.height = 0.2, 
            title.color  = "white",
            title.size = .8,
            legend.title.color = "white",
            legend.text.color = "white",
            bg.color="#022140",
            legend.stack = 'horizontal',
            legend.bg.color = "#416076",
            panel.labels = c("R y RStudio con paquete Tmap"),
            panel.label.color = "darkslateblue",main.title.position = "center",
            main.title = "Terrain analysis based on DEM of Moche. Mapa:R", main.title.size = 0.8)+
  tm_credits("Gorky Florez Castillo", position = c(0.3, .15), col = "white")+
  tm_graticules(ticks =TRUE, lines = TRUE, labels.rot = c(15, 15), col = "azure3", lwd = 1, labels.size = .1)+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 3, position = c(0.70, 0.10))


map1 =tm_shape(aspect, name="Aspecto") + 
  tm_raster(style = "quantile", n = 6, title = "Histograma \n(data distribucion)",
            palette ="Spectral", legend.show = T, legend.hist = T, legend.hist.z=0)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "darkgoldenrod1", color.dark = "lightsteelblue4", 
               position = c("left", "bottom"), lwd = 1, color.light= "white")+
  tm_compass(type="radar", position=c("right", "top"), text.color = "white")+
  tm_style("cobalt")+
  tm_layout(title = "Mapa de \nElevacion, Moche", legend.title.size=.8,legend.text.size = 0.5,
            legend.position = c("right", "bottom") ,
            legend.hist.width = 0.2,
            legend.hist.height = 0.2, 
            title.color  = "white",
            title.size = .8,
            legend.title.color = "white",
            legend.text.color = "white",
            legend.stack = 'horizontal',
            bg.color="#022140",
            legend.bg.color = "#416076",
            panel.labels = c("R y RStudio con paquete Tmap"),
            panel.label.color = "darkslateblue",main.title.position = "center",
            main.title = "Terrain analysis based on DEM of Peru. Mapa:R", main.title.size = 0.8)+
  tm_credits("Gorky Florez Castillo", position = c(0.3, .15), col = "white")+
  tm_graticules(ticks =TRUE, lines = TRUE, labels.rot = c(15, 15), col = "azure3", lwd = 1, labels.size = .1)+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 3, position = c(0.70, 0.10))


cols <-c("#021E4D","#3C4C64","#7E7A77","#C1A972","#F9E945")
map2 =tm_shape(hill) +
  tm_raster(style = "quantile", n = 5, title = "Histograma \n(data distribucion)",
            palette = cols , legend.show = T, legend.hist = T, legend.hist.z=0)+
  tm_shape(hill) +
  tm_raster(palette = cols, style = "cont", legend.show = F)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "darkgoldenrod1", color.dark = "lightsteelblue4", 
               position = c("left", "bottom"), lwd = 1, color.light= "white")+
  tm_compass(type="radar", position=c("right", "top"), text.color = "white")+
  tm_style("cobalt")+
  tm_layout(title = "Mapa de \nElevacion, Moche", legend.title.size=.8,legend.text.size = 0.5,
            legend.position = c("right", "bottom") ,
            legend.hist.width = 0.2,
            legend.hist.height = 0.2, 
            title.color  = "white",
            title.size = .8,
            legend.title.color = "white",
            legend.text.color = "white",
            legend.stack = 'horizontal',
            bg.color="#022140",
            legend.bg.color = "#416076",
            panel.labels = c("R y RStudio con paquete Tmap"),
            panel.label.color = "darkslateblue",main.title.position = "center",
            main.title = "Terrain analysis based on DEM of Moche. Mapa:R", main.title.size = 0.8)+
  tm_credits("Gorky Florez Castillo", position = c(0.3, .15), col = "white")+
  tm_graticules(ticks =TRUE, lines = TRUE, labels.rot = c(15, 15), col = "azure3", lwd = 1, labels.size = .1)+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 3, position = c(0.70, 0.10))


map3 =tm_shape(hill) +
  tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = FALSE)  +
  tm_shape(Cuenca_moche_alt) +
  tm_raster(alpha = 0.5, palette = terrain.colors(25),
            legend.show = FALSE)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "darkgoldenrod1", color.dark = "lightsteelblue4", 
               position = c("left", "bottom"), lwd = 1, color.light= "white")+
  tm_compass(type="radar", position=c("right", "top"), text.color = "white")+
  tm_style("cobalt")+
  tm_layout(title = "Mapa de \nElevacion, Moche", legend.title.size=.8,legend.text.size = 0.5,
            legend.position = c("right", "bottom") ,
            legend.hist.width = 0.2,
            legend.hist.height = 0.2, 
            title.color  = "white",
            title.size = .8,
            legend.title.color = "white",
            legend.text.color = "white",
            legend.stack = 'horizontal',
            bg.color="#022140",
            legend.bg.color = "#416076",
            panel.labels = c("R y RStudio con paquete Tmap"),
            panel.label.color = "darkslateblue",main.title.position = "center",
            main.title = "Terrain analysis based on DEM of Moche. Mapa:R", main.title.size = 0.8)+
  tm_credits("Gorky Florez Castillo", position = c(0.3, .15), col = "white")+
  tm_graticules(ticks =TRUE, lines = TRUE, labels.rot = c(15, 15), col = "azure3", lwd = 1, labels.size = .1)+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 3, position = c(0.70, 0.10))


A=tmap_arrange(map, map1, map2, map3,nrow = 2,ncol =2)
tmap_save(A , "Mpas/cuenca_moche_tmap.png", dpi = 1200, height = 10)

tmap_save(map , "Mpas/cuenca_moche_tmap_dem.png", dpi = 1200, height = 10)
tmap_save(map1 , "Mpas/cuenca_moche_tmap_hill.png", dpi = 1200, height = 10)
tmap_save(map2 , "Mpas/cuenca_moche_tmap_aspecto.png", dpi = 1200, height = 10)
tmap_save(map3 , "Mpas/cuenca_moche_tmap_relieve.png", dpi = 1200, height = 10)
