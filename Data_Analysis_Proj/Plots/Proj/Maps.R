#Following Code reads the shape file of gujarat map and maps enrollment ratios and gender parity index for different years.


#packages need to be loaded
library(rgeos) 
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
library(classInt)
library(ggplot2)
library(maptools)

a.shp = readShapeSpatial(file.choose()) # read actual shape file
a.shp = a.shp[order(a.shp$DISTRICT),] # arranged that file in the alphabetical order of district
a.shp = a.shp[-c(5,8,10,12,14),] # remove rows which are not required: this command removes the rows from shpe files too

# to highlight districts
districtgrp1.shp = a.shp
districtgrp2.shp = a.shp
districtgrp3.shp = a.shp
districtgrp4.shp = a.shp

# to highlight districts
districtgrp1.shp = districtgrp1.shp[c(4,7,12,16,18,25),]
districtgrp2.shp = districtgrp2.shp[c(6,8,10,14,17,22,23),]
districtgrp3.shp = districtgrp3.shp[c(2,5,9,11,15,24,26),]
districtgrp4.shp = districtgrp4.shp[c(1,3,13,19,20,21),]

# to convert shape objects into dataframes from spatial dataframes
districtgrp1.shp.f = fortify(districtgrp1.shp, region = "DISTRICT")
districtgrp2.shp.f = fortify(districtgrp2.shp, region = "DISTRICT")
districtgrp3.shp.f = fortify(districtgrp3.shp, region = "DISTRICT")
districtgrp4.shp.f = fortify(districtgrp4.shp, region = "DISTRICT")

# to plot highlighted districts
highdist.plot1 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group, fill = "white"), color="black", size = 0.25) + coord_equal()+
                  theme_nothing()+ geom_polygon(data = districtgrp1.shp.f, aes(x=long,y=lat,group=group, fill="Yl"), color="black", size=0.25) 
a.shp.f = fortify(a.shp, region = "DISTRICT") # convert spatial polygons dataframe to data frame which can be used by ggplot for mapping
b.df = read.csv(file.choose()) # read Totalenrollment3 file from current folder. It contains enrollment ratio data
#bgpi.df = read.csv(file.choose())
a.shp$DISTRICT 
b.df$id
merge.shp.df = merge(a.shp.f, b.df, by = "id", all.x=T) # merge enrollment data datarame and dataframe used to map using ggplot
#mergegpi.shp.df = merge(a.shp.f, b.df, by = "id", all.x=T)
centroids.df = as.data.frame(coordinates(a.shp)) # find out the centoids of polygons from shape file and convert it into dataframe
names(centroids.df) = c("Longitude", "Latitude") # assign column names to the dataframe containing centroids
centroids.df$id = b.df$id # add a column id in centroids dataframe

label.df = merge(b.df, centroids.df, by = "id", all.x = TRUE ) # merge enrollment data and centroids dataframe
label2.df = label.df
label2.df$Latitude = label2.df$Latitude - 3000

labelgpi.df = merge(bgpi.df, centroids.df, by = "id", all.x = TRUE )

# to change the label position so that it is visible clearly on the map
label.df[1,12] = label.df[1,12] - 2000  
label.df[1,13] = label.df[1,13] + 8000
label.df[24,13] = label.df[24,13] + 12000 
label.df[26,13] = label.df[26,13] + 10000
label.df[2,13] = label.df[2,13] + 12000
label.df[16,13] = label.df[16,13] + 12000
label.df[16,12] = label.df[16,12] - 8000
label.df[19, 13] = label.df[19,13] + 5000
label.df[16,13] = label.df[16,13] - 3000
label2.df = label.df
label2gpi.df = labelgpi.df
label2gpi.df$Latitude = label2gpi.df$Latitude - 8000
label2.df$Latitude = label2.df$Latitude - 3000
label2.df[9,13] = label2.df[9,13] + 1500


# Maps for enrollment Ratios

#map for Enrollment Ratio 2014_15
c.plot10 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group,fill = Year.2014), color="black", size = 0.25) + coord_equal()+
            scale_fill_distiller(name="Enrollment Ratio", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
            theme_nothing(legend = TRUE) + labs(title="Enrollment Ratio 2014_15")
c.plot10 = c.plot10 + geom_text(data = label2.df, aes(label = round(Year.2014, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3.3) 
c.plot10 = c.plot10 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
c.plot10

#map for Enrollment Ratio 2013_14
c.plot9 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group,fill = Year.2013), color="black", size = 0.25) + coord_equal()+
          scale_fill_distiller(name="Enrollment Ratio", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
          theme_nothing(legend = TRUE) + labs(title="Enrollment Ratio 2013_14")  
c.plot9 = c.plot9 + geom_text(data = label2.df, aes(label = round(Year.2013, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3) 
c.plot9 = c.plot9 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
c.plot9

#map for Enrollment Ratio 2012_13
c.plot8 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group,fill = Year.2012), color="black", size = 0.25) + coord_equal()+
          scale_fill_distiller(name="Enrollment Ratio", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
          theme_nothing(legend = TRUE) + labs(title="Enrollment Ratio 2012_13")  
c.plot8 = c.plot8 + geom_text(data = label2.df, aes(label = round(Year.2012, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3) 
c.plot8 = c.plot8 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
c.plot8

#map for Enrollment Ratio 2011_12
c.plot7 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group,fill = Year.2011), color="black", size = 0.25) + coord_equal()+
          scale_fill_distiller(name="Enrollment Ratio", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
          theme_nothing(legend = TRUE) + labs(title="Enrollment Ratio 2011_12")  
c.plot7 = c.plot7 + geom_text(data = label2.df, aes(label = round(Year.2011, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3.3) 
c.plot7 = c.plot7 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
c.plot7

#map for Enrollment Ratio 2010_11
c.plot6 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group,fill = Year.2010), color="black", size = 0.25) + coord_equal()+
          scale_fill_distiller(name="Enrollment Ratio", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
          theme_nothing(legend = TRUE) + labs(title="Enrollment Ratio 2010_11")  
c.plot6 = c.plot6 + geom_text(data = label2.df, aes(label = round(Year.2010, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3) 
c.plot6 = c.plot6 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
c.plot6

#map for Enrollment Ratio 2009_10
c.plot5 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group,fill = Year.2009), color="black", size = 0.25) + coord_equal()+
          scale_fill_distiller(name="Enrollment Ratio", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
          theme_nothing(legend = TRUE) + labs(title="Enrollment Ratio 2009_10") 
c.plot5 = c.plot5 + geom_text(data = label2.df, aes(label = round(Year.2009, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3) 
c.plot5 = c.plot5 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
c.plot5

#map for Enrollment Ratio 2008_09
c.plot4 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group,fill = Year.2008), color="black", size = 0.25) + coord_equal()+
          scale_fill_distiller(name="Enrollment Ratio", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
          theme_nothing(legend = TRUE) + labs(title="Enrollment Ratio 2008_09")  
c.plot4 = c.plot4 + geom_text(data = label2.df, aes(label = round(Year.2008, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3.3) 
c.plot4 = c.plot4 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
c.plot4

#map for Enrollment Ratio 2007_08
c.plot3 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group,fill = Year.2007), color="black", size = 0.25) + coord_equal()+
          scale_fill_distiller(name="Enrollment Ratio", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
          theme_nothing(legend = TRUE) + labs(title="Enrollment Ratio 2007_08") 
c.plot3 = c.plot3 + geom_text(data = label2.df, aes(label = round(Year.2007, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3.3) 
c.plot3 = c.plot3 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
c.plot3

#map for Enrollment Ratio 2006_07
c.plot2 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group,fill = Year.2006), color="black", size = 0.25) + coord_equal()+
          scale_fill_distiller(name="Enrollment Ratio", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
          theme_nothing(legend = TRUE) + labs(title="Enrollment Ratio 2006_07") 
c.plot2 = c.plot2 + geom_text(data = label2.df, aes(label = round(Year.2006, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3.3) 
c.plot2 = c.plot2 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
c.plot2

#map for Enrollment Ratio 2005_06
c.plot1 = ggplot() + geom_polygon(data = merge.shp.df, aes(x=long,y=lat,group = group,fill = Year.2005), color="black", size = 0.25) + coord_equal()+
          scale_fill_distiller(name="Enrollment Ratio", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
          theme_nothing(legend = TRUE) + labs(title="Enrollment Ratio 2005_06") 
c.plot1 = c.plot1 + geom_text(data = label2.df, aes(label = round(Year.2005, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3.3) 
c.plot1 = c.plot1 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
c.plot1

#writes label dataframe into a csv file
write.csv(label.df, "label.csv")

#map for gender parity index
gpi.plot1 = ggplot() + geom_polygon(data = mergegpi.shp.df, aes(x=long,y=lat,group = group,fill = Year.2005), color="black", size = 0.25) + coord_equal()+
            scale_fill_distiller(name="GPI", palette = "YlOrRd", breaks = pretty_breaks(n = 6), trans = "reverse")+
            theme_nothing(legend = TRUE) + labs(title="Gender Parity Index 2005_06") + 
gpi.plot1 = gpi.plot1 + geom_text(data = label2gpi.df, aes(label = round(Year.2014, digits = 3), x = Longitude, y = Latitude, fontface = "bold"), size = 3) 
gpi.plot1 = gpi.plot1 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"))
gpi.plot1


