
# Introduction: -----------------------------------------------------------
# 
# This script is part of the Data Science Applied to Project Management course (UAH)
# Aims:
#   1) To understand Grammar or Graphics structure of ggplot2
#   2) To visualize the most useful plots for visualizing the most common data structures and patterns
#   3) To translate these tools to your projects

# 1. data loading and manipulation ----
## first we load the packages
library(tidyverse)
library(lubridate)
library(ggthemes)
library(wesanderson)

## we will use two datasets related to  La Palma island's Cumbre Vieja 2021 volcanic
# eruption

# ~~ a) dataset 1 ---- 
# from https://www.ign.es/web/ign/portal/ultimos-terremotos/-/ultimos-terremotos/getAnio 
earthquakes_2021 <- read_csv("earthquakes_spain_2021.csv") %>% 
  slice(-1,-394) %>% #remove intro and ending rows
  slice(which(str_detect(Region,"VILLA DE MAZO") == TRUE |  #filter earthquakes at La Palma localities
                str_detect(Region,"FUENCALIENTE") == TRUE |
                str_detect(Region,"TAZACORTE") == TRUE |
                str_detect(Region,"EL PASO") == TRUE )) %>% 
  mutate(date = dmy(Date), # convert to date
         year = year(date), # extract year
         day = yday(date)) %>%  # extract Day of Year
  filter(year == 2021) %>%  # select only 2021 data
  select(Latitude, Longitude, Depth, Magnitude, `Max. int`, Region, day, date)
# Now we take subsets to manually change the name of localities to a more useful one
mazo <- earthquakes_2021 %>% 
  slice(which(str_detect(Region,"VILLA DE MAZO") == TRUE)) %>% 
  mutate(Region = "Villa de Mazo")
fuencaliente <- earthquakes_2021 %>% 
  slice(which(str_detect(Region,"FUENCALIENTE") == TRUE)) %>% 
  mutate(Region = "Fuencaliente") 
tazacorte <- earthquakes_2021 %>% 
  slice(which(str_detect(Region,"TAZACORTE") == TRUE)) %>% 
  mutate(Region = "Tazacorte") 
paso <- earthquakes_2021 %>% 
  slice(which(str_detect(Region,"EL PASO") == TRUE)) %>% 
  mutate(Region = "El Paso") 
# and ensemble them
earthquakes_2021_canarias <- mazo %>% 
  bind_rows(fuencaliente, tazacorte, paso)
view(earthquakes_2021_canarias)

#now let's collapse air quality data from the same period
# source: https://datos-lapalma.opendata.arcgis.com/datasets/lapalma::registros-calidad-aire-1/explore 
air_quality <- read_csv("registros_calidad_aire.csv") %>% 
  mutate(date = as_date(str_sub(time,1,10)), #date extraction
         year = year(date),
         day = yday(date)) %>% 
  filter(year == 2021, #take only 2021
         SO2 >=0) %>%  #avoid some negative missleading vlaues
  group_by(day) %>% 
  summarise_all(mean) %>% #summarise a mean for each day's SO2 levels 
  select(day, pm1, pm10, pm25, temperature, estacion_ID, CO, NO2, O3, SO2, Humidity, Illuminance)

# and now let's merge both datasets:
data_cumbre_vieja <- inner_join(earthquakes_2021_canarias,air_quality, by = "day") 
view(data_cumbre_vieja)

# ~~ b) dataset 2 ----
## we will modify the data_cumbre_vieja dataset to obtain a daily counting of earthquakes
so2_daily <- data_cumbre_vieja %>%  #let's take an unique value of SO2 for each day and each region
  group_by(date, Region) %>% 
  summarise(so2 = mean(SO2))

count_cumbre_vieja <- data_cumbre_vieja %>%
  group_by(date, Region) %>% 
  count(Magnitude) %>%  # count the number of earthquakes of each magnitude 
  group_by(date, Region,) %>% 
  summarise(number = sum(n)) %>%  # and sum them
  inner_join(so2_daily, by = c("date", "Region")) #bind the previous so2 daily dataset
write_csv(count_cumbre_vieja,"count_cumbre_vieja.csv")

# 2. Structure ------------------------------------------------------------
## general structure:
# -->  ggplot call (what data?) + 
##      geom_anything (what do we want?)+
#       theme (customize)+
#       labs (improve visualization) 
## components:
# -->  aesthetics (aes) is called when we want ggplot to look for something INSIDE our dataset.
##        for example when we want to color our points according to the numeric value of our data
# -->  customization without our data: it uses general values to customize anything (size, color, fill, alpha, etc)
ggplot_object <- ggplot(object, aes(var_x, var_y))+ # aes() is the way we tell R to search something inside our dataset
  geom_typeofplotwewant(alpha = ,
                        size = ,
                        color = ,
                        fill = )+
  geom_smooth()+
  theme_tipodetema()+
  labs(title = "nuestro titulo", #prescindibles!
       subtitle = "nuestro subtitulo", #prescindibles!
       x = "nombre eje x",
       y = "nombre eje y"
       )

#  ~~~~ a) scatterplots ------------------------------------------------------------
# we want to visualize how the daily number of earthquakes have varied along eruption periodhas de la erupción
# geom_point() is our frined
ggplot(data = count_cumbre_vieja, aes(x = date, y = number))+
  geom_point()

# customize
ggplot(data = count_cumbre_vieja, aes(x = date, y = number))+
  geom_point(color = "orangered")+
  theme_few() #add a theme for (better) visualization
# for time series it is useful for data visualization to use lines
ggplot(data = count_cumbre_vieja, aes(x = date, y = number))+
  geom_line(color = "orangered")+
  theme_few()
# better dashed lines and save them into an object
lines <- ggplot(data = count_cumbre_vieja, aes(x = date, y = number))+
  geom_line(color = "orangered", linetype = "dashed")+
  theme_few()
#and combine both
lines_points <- lines+
  geom_point(color = "saddlebrown", alpha = 0.5)
lines_points

# we can also add smooth trend curves
lines_points+
  geom_smooth()
# and customize it
lines_points+
  geom_smooth(color = "orange2")

lines_points+
  geom_smooth(color = "orange2",
              fill = "orange3")
# or convert the smooth to a linear model 
lines_points+
  geom_smooth(color = "orange2",
              fill = "orange3",
              method = "lm")

# now let's compare data between values of a third variable (in this case, the categorical
# factor Region))
colores_region <- ggplot(data = count_cumbre_vieja, aes(x = date, y = number))+
  geom_point(aes(color = Region), 
             size = 4, #bigger points
             alpha = 0.35)+ # set transparency (0 = transparent, 1 = colourful) to avoid overlapping 
  theme_few() 
colores_region
# add lines
colores_region_line <- colores_region+
  geom_line(aes(color = Region), size = 1)
colores_region_line
# add smooths and save
colores_region_line_smooth <- colores_region_line+
  geom_smooth(color = "mediumorchid3",
              fill = "mediumorchid4")
colores_region_line_smooth
# we see that, contrary to lines, only one smooth is displayed.
# to plot one for each region, we input color argument inside an aes()
colores_region_line_smooth <- colores_region_line+
  geom_smooth(color = "mediumorchid3",
              fill = "mediumorchid4")
colores_region_line+
  geom_smooth(aes(color = Region, fill = Region))

# but this plot ios too overlapped. Better divide them into facets
colores_region_line+
  geom_line(aes(color = Region), size = 1)+
  geom_smooth(color = "goldenrod3",
              fill = "goldenrod3")+
  facet_wrap(.~ Region)

## let's examine sulphur dioxide levels (which are associated to eruptive processes)
so2 <- ggplot(data = count_cumbre_vieja, aes(x = date, y = so2))+
  geom_point(color = "turquoise4")

# let's see the trend (geom_smooth) and customize with theme_solarized  
so2_smooth <- so2+
  geom_smooth(method = "gam", #alternatives: gam (generalized additive model), loess or linear model
              fill = "orange2",
              color = "goldenrod3")+
  theme_solarized()
so2_smooth

# and see if a linear pattern is evident
go2_lm <- so2+
  geom_smooth(method = "lm",
              fill = "orange2",
              color = "goldenrod3")+
  theme_solarized()
# ¿existirá correlación entre el so2 y el número de terremotos como indicador de intensidad de erupción?
ggplot(data = count_cumbre_vieja, aes(x = so2, y = number))+
  geom_point(color = "turquoise4")+
  geom_smooth(method = "lm",
              fill = "orange2")+
  theme_solarized()

## another example from the dataset 1 (daily number of earthquakes)
data_cumbre_vieja

# let's see each earthquakes and its magnitude for each date
ggplot(data = data_cumbre_vieja, aes(x = date, y = Magnitude))+
  geom_point(color = "firebrick")+
  theme_few()

# we can paint the points according to their magnitude (or other variable) values with aes(color = Magnitude) inside the geom_point 
ggplot(data = data_cumbre_vieja, aes(x = date, y = Magnitude))+
  geom_point(aes(color = Magnitude), size =2)+
  theme_few()
# and customize with a wesanderson package palette with scale_color_gradientn and a dark theme from ggdark
ggplot(data = data_cumbre_vieja, aes(x = date, y = Magnitude))+
  geom_point(aes(color = Magnitude),size =2)+
  scale_color_gradientn(colors = wes_palette("Zissou1"))+
  ggdark::dark_theme_classic()


# ~~ b) boxplots ----
#let's see magnitude variability
ggplot(data = data_cumbre_vieja, aes(y = Magnitude))+
  geom_boxplot()
# and separate by region
ggplot(data = data_cumbre_vieja, aes(x = Region, y = Magnitude))+
  geom_boxplot()
#customize a bit
ggplot(data = data_cumbre_vieja, aes(x = Region, y = Magnitude))+
  geom_boxplot(color = "lightcoral")
ggplot(data = data_cumbre_vieja, aes(x = Region, y = Magnitude))+
  geom_boxplot(color = "lightcoral",
               fill = "powderblue")+
  theme_classic()
# or colour by region
ggplot(data = data_cumbre_vieja, aes(x = Region, y = Magnitude))+
  geom_boxplot(color = "lightgray",aes(fill = Region))+
  theme_classic()

# let's place the legend in the bottom
ggplot(data = data_cumbre_vieja, aes(x = Region, y = Magnitude))+
  geom_boxplot(color = "lightgray",aes(fill = Region))+
  theme_classic()+
  theme(legend.position = "bottom") # or remove it with "none"

# ~~ c) barplots ----
#vamos a contar el número de terremotos en cada región en el primer dataset

ggplot(data = count_cumbre_vieja, aes(x = Region, y = number))+
  geom_bar(stat = "identity", aes(fill = Region))+
  theme_gdocs()
ggplot(data = count_cumbre_vieja, aes(x = Region, y = number))+
  geom_bar(stat = "identity", aes(fill = Region))+
  scale_fill_manual(values = wes_palette("Darjeeling1",4,"discrete"))+
  theme_minimal()

#  ~~ d) histograms & density curves ----
## to see distribution of our data
ggplot(data = data_cumbre_vieja, aes(x = Magnitude))+
  geom_histogram()+
  theme_classic()

ggplot(data = data_cumbre_vieja, aes(x = Magnitude))+
  geom_histogram(binwidth = 0.1, bins = 100)+ # we can adjust the width and the number of bars
  theme_classic()

# or plot it as a density function (fill argument is not necessary)
ggplot(data = data_cumbre_vieja, aes(x = Magnitude))+
  geom_density(fill = "turquoise4")+
  theme_classic()

# 3. Some maps ----

# y mapear
library(ggmap)
library(leaflet)
library(RColorBrewer)
library(htmlwidgets)

# a) leaflet ----
miscolores <- c(2,3,4,5,6)
mypalette <- colorBin(palette="YlOrBr", 
                      domain=data_cumbre_vieja$Magnitude, 
                      na.color="transparent", 
                      bins=mybins)
mytext <- paste(
  "Date: ", data_cumbre_vieja$date,"<br/>", 
  "Magnitude: ", data_cumbre_vieja$Magnitude, "<br/>", 
  "Depth: ", data_cumbre_vieja$Depth, 
  sep="") %>%
  lapply(htmltools::HTML)

lapalma_earthquakes_maps <- leaflet(data = data_cumbre_vieja) %>% 
                                    addTiles() %>% 
                                    addCircleMarkers(~Longitude, 
                                                     ~Latitude,
                                                     stroke = FALSE,
                                                     fillColor = ~mypalette(Magnitude), 
                                                     fillOpacity = 0.8, 
                                                     color = ~mypalette(Magnitude), 
                                                     popup = ~Magnitude,
                                                     label = mytext,
                                                     labelOptions = labelOptions( 
                                                       style = list("font-weight" = "normal", padding = "3px 8px"), 
                                                       textsize = "13px", 
                                                       direction = "auto"))%>% 
                                    addProviderTiles('Esri.WorldImagery') %>%
                                    addLegend(pal=mypalette, values=~Magnitude, opacity=0.9, title = "Magnitude", position = "bottomleft")
saveWidget(lapalma_earthquakes_maps,
           file=paste0(getwd(),"/lapalma_earthquakes_maps.html"))

# b) ggmap? ----
# library(sf)
# canarias_shp <- read_sf("C:/Users/dario-ssm/Documents/Dario Investigacion/colaboracion_docente/grn/lineas_limite/SIGLIM_Publico_INSPIRE/SHP_WGS84/recintos_provinciales_inspire_canarias_wgs84/recintos_provinciales_inspire_canarias_wgs84.shp", quiet = TRUE)
# canarias_shp %>% 
#   ggplot() +
#   geom_sf() +
#   theme_bw()
library(mapSpain)
country <- esp_get_country()
shape <- esp_get_ccaa_siane(ccaa = "Canarias")
ggplot(shape) +
  geom_point(data = data_cumbre_vieja, aes(x = Longitude, y = Latitude, color = Magnitude))+
  geom_sf(fill = "lightgrey", color = "lightgrey") +
  labs(title = "Map of Santa Cruz de Tenerife Province") +
  theme(panel.border = element_rect(colour = "#887e6a",
                                    fill = NA),
        text = element_text(family = "serif",
      face = "bold"))+
  theme_map()+
  theme(legend.position = "bottom")

gmap <- ggplot(data = data_cumbre_vieja, aes(x = lon, y = lat)) +
  geom_sf(data = france)+
  borders("world", colour = "transparent", fill = "white") +
  geom_point(aes(colour = Magnitude), alpha = 0.35, size = 3)+
  theme_void()+
  theme(legend.position = "bottom",panel.background = element_rect(fill="transparent",color=NA))
france <- worldmap[worldmap$name == 'France',]
geom_sf()


# EXTRA: some resources to dive into data visualizartion in handful ways ----

# plenty of useful and smart ideas to visualize your data
https://www.r-graph-gallery.com/

# ggplot & tidyverse boock (Wickham)
https://link.springer.com/book/10.1007/978-0-387-98141-3 
https://r4ds.had.co.nz/ 
# Graph caveats:
https://www.data-to-viz.com/caveats.html

# How to add uncertainty (error bars, pointranges, etc):
http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization