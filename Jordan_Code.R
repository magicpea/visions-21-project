# load packages
library(tidyverse)
library(ncdf4)
library(IDPmisc)
#library(raster) #not sure if I need this
#library(rgdal) #not sure if I need this

#check variable names
nc_data <- nc_open("~/Downloads/flor_20190616-20190916.nc")
#View(nc_data$var)

#create basic dataframe
fluoro <- NULL
fluoro$fluoro_a <- ncvar_get(nc_data, "fluorometric_chlorophyll_a")
fluoro <- as.data.frame(fluoro)
fluoro$time <- ncvar_get(nc_data, "time")
fluoro$time <- as.POSIXct(fluoro$time,origin="1900-01-02")
fluoro$pressure <- ncvar_get(nc_data, "int_ctd_pressure")

#separate out profiles
??

#for loop that flags potential thin layers
profile_list <- unique(fluoro$profile_separate)
potential_thin_layers <- NULL
for (profile in profile_list){
  print(profile)
  fluoro_small <- subset(fluoro, profile_separate == profile)
  avg_chl <- mean(fluoro_small$fluoro_a)
  peak_chl <- max(fluoro_small$fluoro_a)
  if (peak_chl >= 3*avg_chl){
    peak_width_find <- peaks(x=fluoro_small$fluoro_a, y=fluoro_small$pressure)
    peak_width_find <- subset(peak_width_find, x > 0)
    peak_width <- max(peak_width_find$w)
    if (peak_width < 5){
      to_bind <- NULL
      to_bind$fluoro_a <- fluoro_small$fluoro_a
      to_bind$pressure <- fluoro_small$pressure
      to_bind$time <- fluoro_small$time
      to_bind$profile <- profile
      to_bind$avg_chl <- avg_chl
      to_bind$peak_chl <- peak_chl
      to_bind$peak_width <- peak_width
      potential_thin_layers <- rbind(potential_thin_layers, to_bind)
    }
  }
}

#checking to see if thin chlorophyll peaks last at least 2 profiles
thin_profiles <- unique(potential_thin_layers$profile)
thin_layers <- NULL
for (profile in thin_profiles){
  profile_less <- profile - 1
  profile_more <- profile + 1
  profile_less_test <- subset(potential_thin_layers, profile == profile_less)
  profile_more_test <- subset(potential_thin_layers, profile == profile_more)
  if (is.null(profile_less_test) == FALSE | is.null(profile_more_test) == FALSE){
    to_bind <- NULL
    to_bind$fluoro_a <- fluoro_small$fluoro_a
    to_bind$pressure <- fluoro_small$pressure
    to_bind$time <- fluoro_small$time
    to_bind$profile <- profile
    to_bind$avg_chl <- avg_chl
    to_bind$peak_chl <- peak_chl
    to_bind$peak_width <- peak_width
    to_bind$thin_layer <- TRUE
    thin_layers <- rbind(thin_layers, to_bind)
  }
}
  
#export thin layer list
thin_layers <- write.csv(thin_layers, "~/Downloads/thin_layers.csv")

# #plot all fluoro data
# png("~/Downloads/all_fluoro.png",width=12, height=12, unit="in", res=100)
# a <- ggplot(fluoro)+geom_point(aes(x=fluoro_a,y=pressure))+theme_bw()+scale_y_reverse()+xlab("Chlorophyll a fluorescence (mg/m^3)")+ylab("Depth (m)")
# a
# dev.off()

# #plot month of August
# time1 <- "2019-08-01 00:00:00"
# time2 <- "2019-08-31 23:59:59"
# fluoro_august <- subset(fluoro, time > time1)
# fluoro_august <- subset(fluoro_august, time < time2)
# png("~/Downloads/august_fluoro.png",width=12, height=12, unit="in", res=100)
# b <- ggplot(fluoro_august)+geom_point(aes(x=fluoro_a,y=pressure))+theme_bw()+scale_y_reverse()+xlab("Chlorophyll a fluorescence (mg/m^3)")+ylab("Depth (m)")
# b
# dev.off()

# #plot first week of August
# time3 <- "2019-08-07 23:59:59"
# fluoro_august_wk <- subset(fluoro_august, time < time3)
# png("~/Downloads/august1to7_fluoro.png",width=12, height=12, unit="in", res=100)
# c <- ggplot(fluoro_august_wk)+geom_point(aes(x=fluoro_a,y=pressure))+theme_bw()+scale_y_reverse()+xlab("Chlorophyll a fluorescence (mg/m^3)")+ylab("Depth (m)")
# c
# dev.off()
# 
# #plot August 5
# time4 <- "2019-08-05 00:00:00"
# time5 <- "2019-08-05 23:59:59"
# fluoro_august_5 <- subset(fluoro_august, time > time4)
# fluoro_august_5 <- subset(fluoro_august_5, time < time5)
# png("~/Downloads/august5_line_fluoro.png",width=12, height=12, unit="in", res=100)
# d <- ggplot(fluoro_august_5)+geom_path(aes(x=fluoro_a,y=pressure))+theme_bw()+scale_y_reverse()+xlab("Chlorophyll a fluorescence (mg/m^3)")+ylab("Depth (m)")
# d
# dev.off()

#could bin according to profile number and then use that as the legend
