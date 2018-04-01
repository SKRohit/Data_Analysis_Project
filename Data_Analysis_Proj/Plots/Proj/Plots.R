#Following Code plots enrollment ratio and gender parity index

library(maptools)
library(RColorBrewer)
library(ggplot2)
library(foreign)
library(classInt)
library(rgdal)
library(sp)


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# to plot enrollment ratios of different districts on same graph
enrollment_ratio = read.csv(file.choose(), header = TRUE)
enrollment_ratio$Years = as.character(enrollment_ratio$YEAR)

er1 = ggplot(enrollment_ratio, aes(Years, group = 1))
er1 = er1 + geom_line(aes(y = KACHCHH, color = "KACHCHH"), size = 1) + geom_point(aes(y = KACHCHH), color = "RED", size = 3)
er1 = er1 + geom_line(aes(y = BANASKANTHA, color = "BANASKANTHA"), size = 1) + geom_point(aes(y = BANASKANTHA), color = "VIOLET", size = 3)
er1 = er1 + geom_line(aes(y = PATAN, color = "PATAN"), size = 1) + geom_point(aes(y = PATAN), color = "black", size = 3)
er1 = er1 + geom_line(aes(y = DOHAD, color = "DOHAD"), size = 1) + geom_point(aes(y = DOHAD), color = "GREEN", size = 3)
er1 = er1 + geom_line(aes(y = NAVSARI, color = "NAVSARI"), size = 1) + geom_point(aes(y = NAVSARI), color = "BLUE", size = 3)
er1 = er1 + geom_line(aes(y = VADODARA, color = "VADODARA"), size = 1) + geom_point(aes(y = VADODARA), color = "brown", size = 3)
er1 = er1 + labs(x = "", y = "", title = "Enrollment Ratio") + scale_color_manual(name="Districts",values = c(KACHCHH = "RED", BANASKANTHA = "VIOLET", PATAN = "BLACK", DOHAD = "GREEN", NAVSARI = "BLUE", VADODARA = "BROWN"))
er1 = er1 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

er2 = ggplot(enrollment_ratio, aes(Years, group = 1))
er2 = er2 + geom_line(aes(y = JUNAGADH, color = "JUNAGARH"), size = 1) + geom_point(aes(y = JUNAGADH), color = "RED", size = 3)
er2 = er2 + geom_line(aes(y = AMRELI, color = "AMRELI"), size = 1) + geom_point(aes(y = AMRELI), color = "VIOLET", size = 3)
er2 = er2 + geom_line(aes(y = BHARUCH, color = "BHARUCH"), size = 1) + geom_point(aes(y = BHARUCH), color = "black", size = 3)
er2 = er2 + geom_line(aes(y = NARMADA, color = "NARMADA"), size = 1) + geom_point(aes(y = NARMADA), color = "GREEN", size = 3)
er2 = er2 + geom_line(aes(y = VALSAD, color = "VALSAD"), size = 1) + geom_point(aes(y = VALSAD), color = "BLUE", size = 3)
er2 = er2 + geom_line(aes(y = GANDHINAGAR, color = "GANDHINAGAR"), size = 1) + geom_point(aes(y = GANDHINAGAR), color = "brown", size = 3)
er2 = er2 + geom_line(aes(y = TAPI, color = "TAPI"), size = 1) + geom_point(aes(y = TAPI), color = "GREY", size = 3)
er2 = er2 + labs(x = "", y = "", title = "Enrollment Ratio") + scale_color_manual(name="Districts",values = c(JUNAGARH = "RED", AMRELI = "VIOLET", BHARUCH = "BLACK", NARMADA = "GREEN", VALSAD = "BLUE", GANDHINAGAR = "BROWN", TAPI = "GREY"))
er2 = er2 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

er3 = ggplot(enrollment_ratio, aes(Years, group = 1))
er3 = er3 + geom_line(aes(y = THE.DANGS, color = "DANGS"), size = 1) + geom_point(aes(y = THE.DANGS), color = "RED", size = 3)
er3 = er3 + geom_line(aes(y = JAMNAGAR, color = "JAMNAGAR"), size = 1) + geom_point(aes(y = JAMNAGAR), color = "VIOLET", size = 3)
er3 = er3 + geom_line(aes(y = SURENDRANAGAR, color = "SURENDRANAGAR"), size = 1) + geom_point(aes(y = SURENDRANAGAR), color = "black", size = 3)
er3 = er3 + geom_line(aes(y = SURAT, color = "SURAT"), size = 1) + geom_point(aes(y = SURAT), color = "GREEN", size = 3)
er3 = er3 + geom_line(aes(y = BHAVNAGAR, color = "BHAVNAGAR"), size = 1) + geom_point(aes(y = BHAVNAGAR), color = "BLUE", size = 3)
er3 = er3 + geom_line(aes(y = MAHESANA, color = "MAHESANA"), size = 1) + geom_point(aes(y = MAHESANA), color = "brown", size = 3)
er3 = er3 + geom_line(aes(y = PANCH.MAHALS, color = "PANCHMAHAL"), size = 1) + geom_point(aes(y = PANCH.MAHALS), color = "GREY", size = 3)
er3 = er3 + labs(x = "", y = "", title = "Enrollment Ratio") + scale_color_manual(name="Districts",values = c(DANGS = "RED", JAMNAGAR = "VIOLET", SURENDRANAGAR = "BLACK", SURAT = "GREEN", BHAVNAGAR = "BLUE", MAHESANA = "BROWN", PANCHMAHAL = "GREY"))
er3 = er3 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

er4 = ggplot(enrollment_ratio, aes(Years, group = 1))
er4 = er4 + geom_line(aes(y = SABARKANTHA, color = "SABARKANTHA"), size = 1) + geom_point(aes(y = SABARKANTHA), color = "RED", size = 3)
er4 = er4 + geom_line(aes(y = KHEDA, color = "KHERA"), size = 1) + geom_point(aes(y = KHEDA), color = "VIOLET", size = 3)
er4 = er4 + geom_line(aes(y = PORBANDAR, color = "PORBANDAR"), size = 1) + geom_point(aes(y = PORBANDAR), color = "BLACK", size = 3)
er4 = er4 + geom_line(aes(y = ANAND, color = "ANAND"), size = 1) + geom_point(aes(y = ANAND), color = "GREEN", size = 3)
er4 = er4 + geom_line(aes(y = AHMADABAD, color = "AHMADABAD"), size = 1) + geom_point(aes(y = AHMADABAD), color = "BLUE", size = 3)
er4 = er4 + geom_line(aes(y = RAJKOT, color = "RAJKOT"), size = 1) + geom_point(aes(y = RAJKOT), color = "brown", size = 3)
er4 = er4 + labs(x = "", y = "", title = "Enrollment Ratio") + scale_color_manual(name="Districts",values = c(SABARKANTHA = "RED", KHERA = "VIOLET", PORBANDAR = "BLACK", ANAND = "GREEN", AHMADABAD = "BLUE", RAJKOT = "BROWN"))
er4 = er4 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

# to plot girls enrollment ratios
genrollment_ratio = read.csv(file.choose(), header = TRUE)
genrollment_ratio$Years = as.character(genrollment_ratio$YEAR)

ger1 = ggplot(genrollment_ratio, aes(Years, group = 1))
ger1 = ger1 + geom_line(aes(y = KACHCHH, color = "KACHCHH"), size = 1) + geom_point(aes(y = KACHCHH), color = "RED", size = 3)
ger1 = ger1 + geom_line(aes(y = BANASKANTHA, color = "BANASKANTHA"), size = 1) + geom_point(aes(y = BANASKANTHA), color = "VIOLET", size = 3)
ger1 = ger1 + geom_line(aes(y = PATAN, color = "PATAN"), size = 1) + geom_point(aes(y = PATAN), color = "black", size = 3)
ger1 = ger1 + geom_line(aes(y = DOHAD, color = "DOHAD"), size = 1) + geom_point(aes(y = DOHAD), color = "GREEN", size = 3)
ger1 = ger1 + geom_line(aes(y = NAVSARI, color = "NAVSARI"), size = 1) + geom_point(aes(y = NAVSARI), color = "BLUE", size = 3)
ger1 = ger1 + geom_line(aes(y = VADODARA, color = "VADODARA"), size = 1) + geom_point(aes(y = VADODARA), color = "brown", size = 3)
ger1 = ger1 + labs(x = "", y = "", title = "Girls Enrollment Ratio") + scale_color_manual(name="Districts",values = c(KACHCHH = "RED", BANASKANTHA = "VIOLET", PATAN = "BLACK", DOHAD = "GREEN", NAVSARI = "BLUE", VADODARA = "BROWN"))
ger1 = ger1 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))
ger1

ger2 = ggplot(genrollment_ratio, aes(Years, group = 1))
ger2 = ger2 + geom_line(aes(y = JUNAGADH, color = "JUNAGARH"), size = 1) + geom_point(aes(y = JUNAGADH), color = "RED", size = 3)
ger2 = ger2 + geom_line(aes(y = AMRELI, color = "AMRELI"), size = 1) + geom_point(aes(y = AMRELI), color = "VIOLET", size = 3)
ger2 = ger2 + geom_line(aes(y = BHARUCH, color = "BHARUCH"), size = 1) + geom_point(aes(y = BHARUCH), color = "black", size = 3)
ger2 = ger2 + geom_line(aes(y = NARMADA, color = "NARMADA"), size = 1) + geom_point(aes(y = NARMADA), color = "GREEN", size = 3)
ger2 = ger2 + geom_line(aes(y = VALSAD, color = "VALSAD"), size = 1) + geom_point(aes(y = VALSAD), color = "BLUE", size = 3)
ger2 = ger2 + geom_line(aes(y = GANDHINAGAR, color = "GANDHINAGAR"), size = 1) + geom_point(aes(y = GANDHINAGAR), color = "brown", size = 3)
ger2 = ger2 + geom_line(aes(y = TAPI, color = "TAPI"), size = 1) + geom_point(aes(y = TAPI), color = "GREY", size = 3)
ger2 = ger2 + labs(x = "", y = "", title = "Girls Enrollment Ratio") + scale_color_manual(name="Districts",values = c(JUNAGARH = "RED", AMRELI = "VIOLET", BHARUCH = "BLACK", NARMADA = "GREEN", VALSAD = "BLUE", GANDHINAGAR = "BROWN", TAPI = "GREY"))
ger2 = ger2 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))
ger2

ger3 = ggplot(genrollment_ratio, aes(Years, group = 1))
ger3 = ger3 + geom_line(aes(y = THE.DANGS, color = "DANGS"), size = 1) + geom_point(aes(y = THE.DANGS), color = "RED", size = 3)
ger3 = ger3 + geom_line(aes(y = JAMNAGAR, color = "JAMNAGAR"), size = 1) + geom_point(aes(y = JAMNAGAR), color = "VIOLET", size = 3)
ger3 = ger3 + geom_line(aes(y = SURENDRANAGAR, color = "SURENDRANAGAR"), size = 1) + geom_point(aes(y = SURENDRANAGAR), color = "black", size = 3)
ger3 = ger3 + geom_line(aes(y = SURAT, color = "SURAT"), size = 1) + geom_point(aes(y = SURAT), color = "GREEN", size = 3)
ger3 = ger3 + geom_line(aes(y = BHAVNAGAR, color = "BHAVNAGAR"), size = 1) + geom_point(aes(y = BHAVNAGAR), color = "BLUE", size = 3)
ger3 = ger3 + geom_line(aes(y = MAHESANA, color = "MAHESANA"), size = 1) + geom_point(aes(y = MAHESANA), color = "brown", size = 3)
ger3 = ger3 + geom_line(aes(y = PANCH.MAHALS, color = "PANCHMAHAL"), size = 1) + geom_point(aes(y = PANCH.MAHALS), color = "GREY", size = 3)
ger3 = ger3 + labs(x = "", y = "", title = "Girls Enrollment Ratio") + scale_color_manual(name="Districts",values = c(DANGS = "RED", JAMNAGAR = "VIOLET", SURENDRANAGAR = "BLACK", SURAT = "GREEN", BHAVNAGAR = "BLUE", MAHESANA = "BROWN", PANCHMAHAL = "GREY"))
ger3 = ger3 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))
ger3

ger4 = ggplot(genrollment_ratio, aes(Years, group = 1))
ger4 = ger4 + geom_line(aes(y = SABARKANTHA, color = "SABARKANTHA"), size = 1) + geom_point(aes(y = SABARKANTHA), color = "RED", size = 3)
ger4 = ger4 + geom_line(aes(y = KHEDA, color = "KHERA"), size = 1) + geom_point(aes(y = KHEDA), color = "VIOLET", size = 3)
ger4 = ger4 + geom_line(aes(y = PORBANDAR, color = "PORBANDAR"), size = 1) + geom_point(aes(y = PORBANDAR), color = "BLACK", size = 3)
ger4 = ger4 + geom_line(aes(y = ANAND, color = "ANAND"), size = 1) + geom_point(aes(y = ANAND), color = "GREEN", size = 3)
ger4 = ger4 + geom_line(aes(y = AHMADABAD, color = "AHMADABAD"), size = 1) + geom_point(aes(y = AHMADABAD), color = "BLUE", size = 3)
ger4 = ger4 + geom_line(aes(y = RAJKOT, color = "RAJKOT"), size = 1) + geom_point(aes(y = RAJKOT), color = "brown", size = 3)
ger4 = ger4 + labs(x = "", y = "", title = "Girls Enrollment Ratio") + scale_color_manual(name="Districts",values = c(SABARKANTHA = "RED", KHERA = "VIOLET", PORBANDAR = "BLACK", ANAND = "GREEN", AHMADABAD = "BLUE", RAJKOT = "BROWN"))
ger4 = ger4 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))
ger4


# to plot total enrollments
tot_enrollment = read.csv(file.choose(), header = TRUE)
tot_enrollment$Years = as.character(tot_enrollment$YEAR)

tenr1 = ggplot(tot_enrollment, aes(Years, group = 1))
tenr1 = tenr1 + geom_line(aes(y = KACHCHH, color = "KACHCHH"), size = 1) + geom_point(aes(y = KACHCHH), color = "RED", size = 2.5)
tenr1 = tenr1 + geom_line(aes(y = BANASKANTHA, color = "BANASKANTHA"), size = 1) + geom_point(aes(y = BANASKANTHA), color = "VIOLET", size = 2.5)
tenr1 = tenr1 + geom_line(aes(y = PATAN, color = "PATAN"), size = 1) + geom_point(aes(y = PATAN), color = "black", size = 2.5)
tenr1 = tenr1 + geom_line(aes(y = DOHAD, color = "DOHAD"), size = 1) + geom_point(aes(y = DOHAD), color = "GREEN", size = 2.5)
tenr1 = tenr1 + geom_line(aes(y = NAVSARI, color = "NAVSARI"), size = 1) + geom_point(aes(y = NAVSARI), color = "BLUE", size = 2.5)
tenr1 = tenr1 + geom_line(aes(y = VADODARA, color = "VADODARA"), size = 1) + geom_point(aes(y = VADODARA), color = "brown", size = 2.5)
tenr1 = tenr1 + labs(x = "", y = "", title = "Total Enrollments") + scale_color_manual(name="Districts",values = c(KACHCHH = "RED", BANASKANTHA = "VIOLET", PATAN = "BLACK", DOHAD = "GREEN", NAVSARI = "BLUE", VADODARA = "BROWN"))
tenr1 = tenr1 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

tenr2 = ggplot(tot_enrollment, aes(Years, group = 1))
tenr2 = tenr2 + geom_line(aes(y = JUNAGADH, color = "JUNAGARH"), size = 1) + geom_point(aes(y = JUNAGADH), color = "RED", size = 2.5)
tenr2 = tenr2 + geom_line(aes(y = AMRELI, color = "AMRELI"), size = 1) + geom_point(aes(y = AMRELI), color = "VIOLET", size = 2.5)
tenr2 = tenr2 + geom_line(aes(y = BHARUCH, color = "BHARUCH"), size = 1) + geom_point(aes(y = BHARUCH), color = "black", size = 2.5)
tenr2 = tenr2 + geom_line(aes(y = NARMADA, color = "NARMADA"), size = 1) + geom_point(aes(y = NARMADA), color = "GREEN", size = 2.5)
tenr2 = tenr2 + geom_line(aes(y = VALSAD, color = "VALSAD"), size = 1) + geom_point(aes(y = VALSAD), color = "BLUE", size = 2.5)
tenr2 = tenr2 + geom_line(aes(y = GANDHINAGAR, color = "GANDHINAGAR"), size = 1) + geom_point(aes(y = GANDHINAGAR), color = "brown", size = 2.5)
tenr2 = tenr2 + geom_line(aes(y = TAPI, color = "TAPI"), size = 1) + geom_point(aes(y = TAPI), color = "GREY", size = 3)
tenr2 = tenr2 + labs(x = "", y = "", title = "Total Enrollments") + scale_color_manual(name="Districts",values = c(JUNAGARH = "RED", AMRELI = "VIOLET", BHARUCH = "BLACK", NARMADA = "GREEN", VALSAD = "BLUE", GANDHINAGAR = "BROWN", TAPI = "GREY"))
tenr2 = tenr2 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

tenr3 = ggplot(tot_enrollment, aes(Years, group = 1))
tenr3 = tenr3 + geom_line(aes(y = THE.DANGS, color = "DANGS"), size = 1) + geom_point(aes(y = THE.DANGS), color = "RED", size = 2.5)
tenr3 = tenr3 + geom_line(aes(y = JAMNAGAR, color = "JAMNAGAR"), size = 1) + geom_point(aes(y = JAMNAGAR), color = "VIOLET", size = 2.5)
tenr3 = tenr3 + geom_line(aes(y = SURENDRANAGAR, color = "SURENDRANAGAR"), size = 1) + geom_point(aes(y = SURENDRANAGAR), color = "black", size = 2.5)
tenr3 = tenr3 + geom_line(aes(y = SURAT, color = "SURAT"), size = 1) + geom_point(aes(y = SURAT), color = "GREEN", size = 2.5)
tenr3 = tenr3 + geom_line(aes(y = BHAVNAGAR, color = "BHAVNAGAR"), size = 1) + geom_point(aes(y = BHAVNAGAR), color = "BLUE", size = 2.5)
tenr3 = tenr3 + geom_line(aes(y = MAHESANA, color = "MAHESANA"), size = 1) + geom_point(aes(y = MAHESANA), color = "brown", size = 2.5)
tenr3 = tenr3 + geom_line(aes(y = PANCH.MAHALS, color = "PANCHMAHAL"), size = 1) + geom_point(aes(y = PANCH.MAHALS), color = "GREY", size = 2.5)
tenr3 = tenr3 + labs(x = "", y = "", title = "Total Enrollments") + scale_color_manual(name="Districts",values = c(DANGS = "RED", JAMNAGAR = "VIOLET", SURENDRANAGAR = "BLACK", SURAT = "GREEN", BHAVNAGAR = "BLUE", MAHESANA = "BROWN", PANCHMAHAL = "GREY"))
tenr3 = tenr3 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

tenr4 = ggplot(tot_enrollment, aes(Years, group = 1))
tenr4 = tenr4 + geom_line(aes(y = SABARKANTHA, color = "SABARKANTHA"), size = 1) + geom_point(aes(y = SABARKANTHA), color = "RED", size = 2.5)
tenr4 = tenr4 + geom_line(aes(y = KHEDA, color = "KHERA"), size = 1) + geom_point(aes(y = KHEDA), color = "VIOLET", size = 2.5)
tenr4 = tenr4 + geom_line(aes(y = PORBANDAR, color = "PORBANDAR"), size = 1) + geom_point(aes(y = PORBANDAR), color = "BLACK", size = 2.5)
tenr4 = tenr4 + geom_line(aes(y = ANAND, color = "ANAND"), size = 1) + geom_point(aes(y = ANAND), color = "GREEN", size = 2.5)
tenr4 = tenr4 + geom_line(aes(y = AHMADABAD, color = "AHMADABAD"), size = 1) + geom_point(aes(y = AHMADABAD), color = "BLUE", size = 2.5)
tenr4 = tenr4 + geom_line(aes(y = RAJKOT, color = "RAJKOT"), size = 1) + geom_point(aes(y = RAJKOT), color = "brown", size = 2.5)
tenr4 = tenr4 + labs(x = "", y = "", title = "Total Enrollments") + scale_color_manual(name="Districts",values = c(SABARKANTHA = "RED", KHERA = "VIOLET", PORBANDAR = "BLACK", ANAND = "GREEN", AHMADABAD = "BLUE", RAJKOT = "BROWN"))
tenr4 = tenr4 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))


#to plot total girls enrollment
tot_genrollment = read.csv(file.choose(), header = TRUE)
tot_genrollment$Years = as.character(tot_genrollment$YEAR)

tgenr1 = ggplot(tot_genrollment, aes(Years, group = 1))
tgenr1 = tgenr1 + geom_line(aes(y = KACHCHH, color = "KACHCHH"), size = 1) + geom_point(aes(y = KACHCHH), color = "RED", size = 2.5)
tgenr1 = tgenr1 + geom_line(aes(y = BANASKANTHA, color = "BANASKANTHA"), size = 1) + geom_point(aes(y = BANASKANTHA), color = "VIOLET", size = 2.5)
tgenr1 = tgenr1 + geom_line(aes(y = PATAN, color = "PATAN"), size = 1) + geom_point(aes(y = PATAN), color = "black", size = 2.5)
tgenr1 = tgenr1 + geom_line(aes(y = DOHAD, color = "DOHAD"), size = 1) + geom_point(aes(y = DOHAD), color = "GREEN", size = 2.5)
tgenr1 = tgenr1 + geom_line(aes(y = NAVSARI, color = "NAVSARI"), size = 1) + geom_point(aes(y = NAVSARI), color = "BLUE", size = 2.5)
tgenr1 = tgenr1 + geom_line(aes(y = VADODARA, color = "VADODARA"), size = 1) + geom_point(aes(y = VADODARA), color = "brown", size = 2.5)
tgenr1 = tgenr1 + labs(x = "", y = "", title = "Total Girls Enrollments") + scale_color_manual(name="Districts",values = c(KACHCHH = "RED", BANASKANTHA = "VIOLET", PATAN = "BLACK", DOHAD = "GREEN", NAVSARI = "BLUE", VADODARA = "BROWN"))
tgenr1 = tgenr1 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

tgenr2 = ggplot(tot_genrollment, aes(Years, group = 1))
tgenr2 = tgenr2 + geom_line(aes(y = JUNAGADH, color = "JUNAGARH"), size = 1) + geom_point(aes(y = JUNAGADH), color = "RED", size = 2.5)
tgenr2 = tgenr2 + geom_line(aes(y = AMRELI, color = "AMRELI"), size = 1) + geom_point(aes(y = AMRELI), color = "VIOLET", size = 2.5)
tgenr2 = tgenr2 + geom_line(aes(y = BHARUCH, color = "BHARUCH"), size = 1) + geom_point(aes(y = BHARUCH), color = "black", size = 2.5)
tgenr2 = tgenr2 + geom_line(aes(y = NARMADA, color = "NARMADA"), size = 1) + geom_point(aes(y = NARMADA), color = "GREEN", size = 2.5)
tgenr2 = tgenr2 + geom_line(aes(y = VALSAD, color = "VALSAD"), size = 1) + geom_point(aes(y = VALSAD), color = "BLUE", size = 2.5)
tgenr2 = tgenr2 + geom_line(aes(y = GANDHINAGAR, color = "GANDHINAGAR"), size = 1) + geom_point(aes(y = GANDHINAGAR), color = "brown", size = 2.5)
tgenr2 = tgenr2 + geom_line(aes(y = TAPI, color = "TAPI"), size = 1) + geom_point(aes(y = TAPI), color = "GREY", size = 3)
tgenr2 = tgenr2 + labs(x = "", y = "", title = "Total Girls Enrollments") + scale_color_manual(name="Districts",values = c(JUNAGARH = "RED", AMRELI = "VIOLET", BHARUCH = "BLACK", NARMADA = "GREEN", VALSAD = "BLUE", GANDHINAGAR = "BROWN", TAPI = "GREY"))
tgenr2 = tgenr2 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

tgenr3 = ggplot(tot_genrollment, aes(Years, group = 1))
tgenr3 = tgenr3 + geom_line(aes(y = THE.DANGS, color = "DANGS"), size = 1) + geom_point(aes(y = THE.DANGS), color = "RED", size = 2.5)
tgenr3 = tgenr3 + geom_line(aes(y = JAMNAGAR, color = "JAMNAGAR"), size = 1) + geom_point(aes(y = JAMNAGAR), color = "VIOLET", size = 2.5)
tgenr3 = tgenr3 + geom_line(aes(y = SURENDRANAGAR, color = "SURENDRANAGAR"), size = 1) + geom_point(aes(y = SURENDRANAGAR), color = "black", size = 2.5)
tgenr3 = tgenr3 + geom_line(aes(y = SURAT, color = "SURAT"), size = 1) + geom_point(aes(y = SURAT), color = "GREEN", size = 2.5)
tgenr3 = tgenr3 + geom_line(aes(y = BHAVNAGAR, color = "BHAVNAGAR"), size = 1) + geom_point(aes(y = BHAVNAGAR), color = "BLUE", size = 2.5)
tgenr3 = tgenr3 + geom_line(aes(y = MAHESANA, color = "MAHESANA"), size = 1) + geom_point(aes(y = MAHESANA), color = "brown", size = 2.5)
tgenr3 = tgenr3 + geom_line(aes(y = PANCH.MAHALS, color = "PANCHMAHAL"), size = 1) + geom_point(aes(y = PANCH.MAHALS), color = "GREY", size = 2.5)
tgenr3 = tgenr3 + labs(x = "", y = "", title = "Total Girls Enrollments") + scale_color_manual(name="Districts",values = c(DANGS = "RED", JAMNAGAR = "VIOLET", SURENDRANAGAR = "BLACK", SURAT = "GREEN", BHAVNAGAR = "BLUE", MAHESANA = "BROWN", PANCHMAHAL = "GREY"))
tgenr3 = tgenr3 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

tgenr4 = ggplot(tot_genrollment, aes(Years, group = 1))
tgenr4 = tgenr4 + geom_line(aes(y = SABARKANTHA, color = "SABARKANTHA"), size = 1) + geom_point(aes(y = SABARKANTHA), color = "RED", size = 2.5)
tgenr4 = tgenr4 + geom_line(aes(y = KHEDA, color = "KHERA"), size = 1) + geom_point(aes(y = KHEDA), color = "VIOLET", size = 2.5)
tgenr4 = tgenr4 + geom_line(aes(y = PORBANDAR, color = "PORBANDAR"), size = 1) + geom_point(aes(y = PORBANDAR), color = "BLACK", size = 2.5)
tgenr4 = tgenr4 + geom_line(aes(y = ANAND, color = "ANAND"), size = 1) + geom_point(aes(y = ANAND), color = "GREEN", size = 2.5)
tgenr4 = tgenr4 + geom_line(aes(y = AHMADABAD, color = "AHMADABAD"), size = 1) + geom_point(aes(y = AHMADABAD), color = "BLUE", size = 2.5)
tgenr4 = tgenr4 + geom_line(aes(y = RAJKOT, color = "RAJKOT"), size = 1) + geom_point(aes(y = RAJKOT), color = "brown", size = 2.5)
tgenr4 = tgenr4 + labs(x = "", y = "", title = "Total Girls Enrollments") + scale_color_manual(name="Districts",values = c(SABARKANTHA = "RED", KHERA = "VIOLET", PORBANDAR = "BLACK", ANAND = "GREEN", AHMADABAD = "BLUE", RAJKOT = "BROWN"))
tgenr4 = tgenr4 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

#to plot gender parity index
elemgpi.df = read.csv(file.choose(), header = T)
elemgpi.df$Years = as.character(elemgpi.df$YEAR)

gpi1 = ggplot(elemgpi.df, aes(Years, group = 1))
gpi1 = gpi1 + geom_line(aes(y = KACHCHH, color = "KACHCHH"), size = 1) + geom_point(aes(y = KACHCHH), color = "RED", size = 2.5)
gpi1 = gpi1 + geom_line(aes(y = BANASKANTHA, color = "BANASKANTHA"), size = 1) + geom_point(aes(y = BANASKANTHA), color = "VIOLET", size = 2.5)
gpi1 = gpi1 + geom_line(aes(y = PATAN, color = "PATAN"), size = 1) + geom_point(aes(y = PATAN), color = "black", size = 2.5)
gpi1 = gpi1 + geom_line(aes(y = DOHAD, color = "DOHAD"), size = 1) + geom_point(aes(y = DOHAD), color = "GREEN", size = 2.5)
gpi1 = gpi1 + geom_line(aes(y = NAVSARI, color = "NAVSARI"), size = 1) + geom_point(aes(y = NAVSARI), color = "BLUE", size = 2.5)
gpi1 = gpi1 + geom_line(aes(y = VADODARA, color = "VADODARA"), size = 1) + geom_point(aes(y = VADODARA), color = "brown", size = 2.5)
gpi1 = gpi1 + labs(x = "", y = "", title = "Gender Parity Index") + scale_color_manual(name="Districts",values = c(KACHCHH = "RED", BANASKANTHA = "VIOLET", PATAN = "BLACK", DOHAD = "GREEN", NAVSARI = "BLUE", VADODARA = "BROWN"))
gpi1 = gpi1 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

gpi2 = ggplot(elemgpi.df, aes(Years, group = 1))
gpi2 = gpi2 + geom_line(aes(y = JUNAGADH, color = "JUNAGARH"), size = 1) + geom_point(aes(y = JUNAGADH), color = "RED", size = 2.5)
gpi2 = gpi2 + geom_line(aes(y = AMRELI, color = "AMRELI"), size = 1) + geom_point(aes(y = AMRELI), color = "VIOLET", size = 2.5)
gpi2 = gpi2 + geom_line(aes(y = BHARUCH, color = "BHARUCH"), size = 1) + geom_point(aes(y = BHARUCH), color = "black", size = 2.5)
gpi2 = gpi2 + geom_line(aes(y = NARMADA, color = "NARMADA"), size = 1) + geom_point(aes(y = NARMADA), color = "GREEN", size = 2.5)
gpi2 = gpi2 + geom_line(aes(y = VALSAD, color = "VALSAD"), size = 1) + geom_point(aes(y = VALSAD), color = "BLUE", size = 2.5)
gpi2 = gpi2 + geom_line(aes(y = GANDHINAGAR, color = "GANDHINAGAR"), size = 1) + geom_point(aes(y = GANDHINAGAR), color = "brown", size = 2.5)
gpi2 = gpi2 + geom_line(aes(y = TAPI, color = "TAPI"), size = 1) + geom_point(aes(y = TAPI), color = "GREY", size = 3)
gpi2 = gpi2 + labs(x = "", y = "", title = "Gender Parity Index") + scale_color_manual(name="Districts",values = c(JUNAGARH = "RED", AMRELI = "VIOLET", BHARUCH = "BLACK", NARMADA = "GREEN", VALSAD = "BLUE", GANDHINAGAR = "BROWN", TAPI = "GREY"))
gpi2 = gpi2 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

gpi3 = ggplot(elemgpi.df, aes(Years, group = 1))
gpi3 = gpi3 + geom_line(aes(y = THE.DANGS, color = "DANGS"), size = 1) + geom_point(aes(y = THE.DANGS), color = "RED", size = 2.5)
gpi3 = gpi3 + geom_line(aes(y = JAMNAGAR, color = "JAMNAGAR"), size = 1) + geom_point(aes(y = JAMNAGAR), color = "VIOLET", size = 2.5)
gpi3 = gpi3 + geom_line(aes(y = SURENDRANAGAR, color = "SURENDRANAGAR"), size = 1) + geom_point(aes(y = SURENDRANAGAR), color = "black", size = 2.5)
gpi3 = gpi3 + geom_line(aes(y = SURAT, color = "SURAT"), size = 1) + geom_point(aes(y = SURAT), color = "GREEN", size = 2.5)
gpi3 = gpi3 + geom_line(aes(y = BHAVNAGAR, color = "BHAVNAGAR"), size = 1) + geom_point(aes(y = BHAVNAGAR), color = "BLUE", size = 2.5)
gpi3 = gpi3 + geom_line(aes(y = MAHESANA, color = "MAHESANA"), size = 1) + geom_point(aes(y = MAHESANA), color = "brown", size = 2.5)
gpi3 = gpi3 + geom_line(aes(y = PANCH.MAHALS, color = "PANCHMAHAL"), size = 1) + geom_point(aes(y = PANCH.MAHALS), color = "GREY", size = 2.5)
gpi3 = gpi3 + labs(x = "", y = "", title = "Gender Parity Index") + scale_color_manual(name="Districts",values = c(DANGS = "RED", JAMNAGAR = "VIOLET", SURENDRANAGAR = "BLACK", SURAT = "GREEN", BHAVNAGAR = "BLUE", MAHESANA = "BROWN", PANCHMAHAL = "GREY"))
gpi3 = gpi3 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))

gpi4 = ggplot(elemgpi.df, aes(Years, group = 1))
gpi4 = gpi4 + geom_line(aes(y = SABARKANTHA, color = "SABARKANTHA"), size = 1) + geom_point(aes(y = SABARKANTHA), color = "RED", size = 2.5)
gpi4 = gpi4 + geom_line(aes(y = KHEDA, color = "KHERA"), size = 1) + geom_point(aes(y = KHEDA), color = "VIOLET", size = 2.5)
gpi4 = gpi4 + geom_line(aes(y = PORBANDAR, color = "PORBANDAR"), size = 1) + geom_point(aes(y = PORBANDAR), color = "BLACK", size = 2.5)
gpi4 = gpi4 + geom_line(aes(y = ANAND, color = "ANAND"), size = 1) + geom_point(aes(y = ANAND), color = "GREEN", size = 2.5)
gpi4 = gpi4 + geom_line(aes(y = AHMADABAD, color = "AHMADABAD"), size = 1) + geom_point(aes(y = AHMADABAD), color = "BLUE", size = 2.5)
gpi4 = gpi4 + geom_line(aes(y = RAJKOT, color = "RAJKOT"), size = 1) + geom_point(aes(y = RAJKOT), color = "brown", size = 2.5)
gpi4 = gpi4 + labs(x = "", y = "", title = "Gender Parity Index") + scale_color_manual(name="Districts",values = c(SABARKANTHA = "RED", KHERA = "VIOLET", PORBANDAR = "BLACK", ANAND = "GREEN", AHMADABAD = "BLUE", RAJKOT = "BROWN"))
gpi4 = gpi4 + theme(legend.title = element_text(colour = "black", face = "bold"), plot.title = element_text(colour = "black", face = "bold"), axis.text.x = element_text(colour = "Black", size = 12), axis.text.y = element_text(colour = "Black", size = 12))
 