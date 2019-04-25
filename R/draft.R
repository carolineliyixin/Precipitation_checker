setwd("/Users/carolineli")
library(shiny)
library(dismo)
library(raster)
library(rgdal)
library(gtools)
library(sp)

summary(i)

list.ras <- mixedsort(list.files("data/wc10", full.names = T, 
                                 pattern = ".bil$"))
tmin.all <- stack(list.ras)
a = res(tmin.all)
a[2]

tmin1 <- raster("data/wc10/tmin1.bil")
plot(tmin1)
res(tmin1)
summary(tmin1)
graph_china = shapefile("data/gadm36_CHN_shp/gadm36_CHN_0.shp")
graph_china
graph_china$tmin1 <- extract(x = tmin1, y = graph_china)
summary(graph_china$tmin1)
class(graph_china$tmin1)
new = as.data.frame(graph_china$tmin1)
new

county = shapefile("data/country/country.shp")
summary(county)
res(county)
res()
names(county)
length(county$CNTRY_NAME)
county$CNTRY_NAME

county$CNTRY_NAME[1]
county$tmin1 <- extract(x = tmin1, y = county)
county$tmin1

graph_china$tmin1

values = county$tmin1
summary(values[[1]])

#save(values,file = paste(getwd(),"/Desktop/Shiny/extract_value_county_tmin1.rData", sep = ""))
#writeLines(as.character(my_extract_value), paste(getwd(),"/Desktop/Shiny/extract_value_county_tmin1.txt", sep = ""))
xx = c(1,2)
a = c(12,"22",3,4,xx)
a
class(as.data.frame(a))
b = c(11,21,45,31)
ab = as.data.frame(cbind(a,b))
class(ab)
ab[[1]]
for (i in ab[[1]]){
  class(i)
  print(i)
}
coordinates(ab) = c("a","b")
plot(ab)
summary(ab)

proj4string(ab) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
summary(ab)
plot(ab)


a = as.data.frame(cbind(30,40))
coordinates(a) = c(30,40)
proj4string(a) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
extracted_value <- extract(x = month_to_calculate, y = a)


new_file = read.csv("data/textcase1.csv",header = T, strip.white = T, sep = ",")
a = as.data.frame(new_file[1])
b = as.data.frame(new_file[2])
ab = c(a,b)


if (input$tabs=="Import File"){
  new_file = read.csv(input$input_file,header = T, strip.white = T, sep = ",")
  long = as.data.frame(new_file[1])
  lat = as.data.frame(new_file[2])
  coords = c(long,lat)
  validate(checkData(long,lat))
}



list.ras <- mixedsort(list.files("data/wc10", full.names = T, 
                                 pattern = ".bil$"))
tmin.all <- stack(list.ras)
month = list(new_file[3])
a = as.character(month==1)
month_to_calculate <- tmin.all[[paste("tmin",a, sep = "")]]



new_file = read.csv("x.csv",header = F, sep = ",")
new_file
long = new_file[1]
long
lat = new_file[2]

a <- as.data.frame(cbind(long,lat))
a
coordinates(a) = c("V1","V2")
proj4string(a) = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

month_to_calculate <- tmin.all[[paste("tmin",a, sep = "")]]

a = c(1,2,3)
a[1]

