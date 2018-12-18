rm(list = ls())

#=========================================================================
#= IBEU MAPA  ============================================================ 
#========================================================================
library(ggmap); library(ggplot2); library(dplyr); library(rgdal); 
library(gridExtra); library(dplyr); library(lattice)
library(sp);library(classInt); library(broom);library(tidyr)
library(leaflet);library(RColorBrewer);library(openxlsx)

## instalar 'webshot' package
library(devtools);library(htmlwidgets)
library(webshot)

# importando o shape --------------------------------------------------

library(raster)
shape <- shapefile("E:/Observatório 2018/Shapes/shapeRMMCenso2010PorAeds2/41SEE250GC_RMM.shp")

# merge -------------------------------------------------------------------

dados <- read.csv("E:/Observatório 2018/IBEU/IBEU_APOND_RMM2.csv", sep = ";", dec = ",")

dados$Aponds    <- dados$Aponds%>%as.character()
names(dados)[2] <- "areaPond"

shapeRMM <- merge(shape,dados,by ="areaPond")

# -----------------------------------------------------------------------
# Criando os labels
paleta <-c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba")
cbins  <-c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
cutpal <- colorBin(paleta, bins=cbins, na.color = "gray")

pivs <- paste0("<strong>IBEU: </strong>", shapeRMM$IBEU%>%round(2), " <strong>Setor: </strong> ",shapeRMM$Nome)
pd1  <- paste0("<strong>D1: </strong>", shapeRMM$D1%>%round(2), " <strong>Setor: </strong> ",shapeRMM$Nome)
pd2  <- paste0("<strong>D2: </strong>", shapeRMM$D2%>%round(2), " <strong>Setor: </strong> ",shapeRMM$Nome)
pd3  <- paste0("<strong>D3: </strong>", shapeRMM$D3%>%round(2), " <strong>Setor: </strong> ",shapeRMM$Nome)
pd4  <- paste0("<strong>D4: </strong>", shapeRMM$D4%>%round(2), " <strong>Setor: </strong> ",shapeRMM$Nome)
pd5  <- paste0("<strong>D5: </strong>", shapeRMM$D5%>%round(2), " <strong>Setor: </strong> ",shapeRMM$Nome)

labels <- sprintf(
  "<strong>%s</strong><br/> </strong>",
  shapeRMM$areaPond) %>% lapply(htmltools::HTML)

#-------------------------------------------------------------------------------
# Ocorrências policiais banco RECOP - Lesão Corporal

dir             <-"E:/OBSERVATÓRIO/OBSERVATÓRIO 2016/Violência Ana/lesaopoints.csv"
ocor            <-read.table(dir, sep=";", header = T)
ocor$lat        <-paste0(substr(ocor$lat,1,3),".",substr(ocor$lat,4,12))
ocor$lat        <-as.numeric(ocor$lat)
ocor$lon        <-paste0(substr(ocor$lon,1,3),".",substr(ocor$lon,4,12))
ocor$lon        <-as.numeric(ocor$lon)  
ocor$ocorrencia <- ocor$ocorrencia%>%as.character()
ocor$ocorrencia[ocor$ocorrencia == "Lesão Corporal, Maus Tratos e Tortur"] <- "Lesão Corporal"
ocor2           <- subset(ocor, ocorrencia == "Lesão Corporal")
table(ocor2$ocorrencia)

paste0(substr(ocor$lon,1,3))
ocor2$lat2 <- paste0(substr(ocor2$lat,1,3))
ocor2      <- subset(ocor2, lat2 == "-23")

#-------------------------------------------------------------------------------
# O Mapa --------------------------------------------------------------------------
#--------------------------------------------------------------------------------

mapa <- leaflet() %>% addTiles() %>% 
  # Basemap
  addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Stamen.Toner") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery") %>%
  # Setores
  addPolygons(data = shapeRMM, 
              fillColor = ~CD_GEOCODI,  weight = 1,
              opacity = 0.8, dashArray = "3", color = "white",
              highlight = highlightOptions(
                weight = 5, color = "black",
                dashArray = "",  fillOpacity = 0.7, fillColor = "white", 
                bringToFront = T), label = labels, group = "Aponds",
              labelOptions = labelOptions(noHide = F, direction = "right")) %>%
  # IBEU
  addPolygons(data = shapeRMM, 
              fillColor = ~cutpal(IBEU), weight = 1, color = "white", 
              fillOpacity = 0.8, smoothFactor = 0.6,
              group = "IBEU", popup = pivs,
              highlightOptions = highlightOptions(color = "Black", weight = 3)) %>% 
  # D1
  addPolygons(data = shapeRMM, 
              fillColor = ~cutpal(D1), weight = 1, color = "white", 
              fillOpacity = 0.8, smoothFactor = 0.6,
              group = "D1-Mobilidade", popup = pd1) %>% 
  # D2
  addPolygons(data = shapeRMM, 
              fillColor = ~cutpal(D2), weight = 1, color = "white",  
              fillOpacity = 0.8, smoothFactor = 0.6,
              group = "D2-Ambientais", popup = pd2) %>% 
  # D3
  addPolygons(data = shapeRMM, 
              fillColor = ~cutpal(D3), weight = 1, color = "white",  
              fillOpacity = 0.8, smoothFactor = 0.6,
              group = "D3-Habitacionais", popup = pd3) %>% 
  # D4
  addPolygons(data = shapeRMM, 
              fillColor = ~cutpal(D4), weight = 1, color = "white",  
              fillOpacity = 0.8, smoothFactor = 0.6,
              group = "D4-Serviços", popup = pd4) %>% 
  # D5
  addPolygons(data = shapeRMM, 
              fillColor = ~cutpal(D5), weight = 1, color = "white",  
              fillOpacity = 0.8, smoothFactor = 0.6,
              group = "D5-Infraestrutura", popup = pd5) %>%
  # Ocorrências
  addCircleMarkers(data = ocor2,
             lng = ~lon, lat = ~lat, radius = 1,
             group = "Lesão Corporal", color = "black")%>%
  # Layers
  addLayersControl(
    baseGroups = c("OpenStreetMap","Positron","Stamen.Toner","WorldImagery"),
    overlayGroups = c("Apond","IBEU","D1-Mobilidade", "D2-Ambientais","D3-Habitacionais",
                      "D4-Serviços","D5-Infraestrutura", "Lesão Corporal"),
    options = layersControlOptions(collapsed = F)) %>%
  #Legenda
  #addProviderTiles("CartoDB.Positron") %>% # Fixar google maps
  addLegend("bottomright",  # location
            colors = c("#d7191c","#fdae61","#ffffbf","#abdda4","#2b83ba","gray"),    # palette function
            title = "Indicador",opacity = 1,
            labels = c("0,00 - 0,20", "0,20 - 0,40","0,40 - 0,60","0,60 - 0,80","0,80 - 1","NA"),
            labFormat = labelFormat(prefix = "[", suffix = "]", between = ";"))
mapa

#saveWidget(mapa, "C:/Users/Furriel/Dropbox/IBEU/MapaIBEU/MapaWebIBEU.html")


