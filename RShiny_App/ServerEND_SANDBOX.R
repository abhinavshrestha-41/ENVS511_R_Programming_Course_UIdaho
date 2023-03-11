## INPUTS
state <- "Iowa"
clim_var <- "tmax" #prec, tmax
rcp <- "26" #26,45,60,85
month <- "Jun"
  

### Import data with map of US (VECTOR)
usa <- getData('GADM', country='USA', level = 1)

## Extract vector file of only INPUT 'state'
state <- usa[usa$NAME_1 == state,]
plot(state)


### Import raster of CC model with INPUT 'clim_var' and INPUT 'rcp'
#?getData #to refer to the necessary format required to download the climate projection data

CC_data <- getData('CMIP5', #'CMIP5' Phase five of the Coupled Model Intercomparison Project (CMIP5) used to get projected (future) climate data  
                   var = clim_var, #for precipitation data
                   res = 2.5, #CONSTANT
                   rcp = as.numeric(rcp), #projections based on RCP 8.5 (worst case scenario)
                   model = 'CC', #CONSTANT
                   year = 70) #CONSTANT


## Clip raster data to vector file of Montana using 'crop' and 'mask' function
state_CC_data_clip <- crop(CC_data,state) #raster file containing projected precipitation data for the entire world cropped to the extent of the 'montana' vector file 

state_CC_data <- mask(state_CC_data_clip, state)

#renaming raster stack layers
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

for(i in 1:12){
  state_CC_data@data@names[i] <- paste(months[i])
}


#OUTPUT PLOT - Prediction of INPUT 'clim_var' for INPUT 'month' under INPUT 'rcp' conditions: 
month_plot <- subset(state_CC_data, month)

#output plot based on 'clim_var' input, need to divide by 10 for 'tmax' input
if (clim_var == 'tmax'){
  plot(month_plot/10,
       legend.args = list (text = 'Projected Mean Temperature (degrees C)', #'legend.args' used to modify legend elements
                           side = 4, #side 1 starts at the bottom of the element and goes clockwise
                           font = 2, #type of font used
                           line = 2.75, #distance from element
                           cex = 1), #size of font
       main = c(paste("Projected Mean Maximum Temperature for",state$NAME_1,"(2070).",
                      "CMIP5, 'CC' model, RCP",as.numeric(rcp)/10)))
} else {
  plot(month_plot, 
       legend.args = list (text = 'Projected Mean Precipitation (mm)', #'legend.args' used to modify legend elements
                           side = 4, #side 1 starts at the bottom of the element and goes clockwise
                           font = 2, #type of font used
                           line = 2.75, #distance from element
                           cex = 1), #size of font
       main = c(paste("Projected Mean Precipitation for",state$NAME_1,"(2070).",
                      "CMIP5, 'CC' model, RCP",as.numeric(rcp)/10)))
  
}

#Mean INPUT 'clim_var' projection month by month basis 

#Creating an empty data frame to store mean clim_var calculations. 
mean_climVar_df <- data.frame(matrix(ncol=2, nrow=12))
colnames(mean_climVar_df) <- c("Month", "clim_var")
mean_climVar_df$Month <- c(month(ymd(700101) + months(0:11), label = TRUE))

if (clim_var == 'tmax'){
  for(i in 1:12){
    temp_raster <- subset(state_CC_data,i)
    mean_climVar_df$clim_var[i] <- round(((mean(temp_raster@data@values, na.rm=TRUE))/10), 2)
  }
} else {
  for(i in 1:12){
    temp_raster <- subset(state_CC_data,i)
    mean_climVar_df$clim_var[i] <- round(mean(temp_raster@data@values, na.rm=TRUE), 2)
  }
}

highlight_point <- mean_climVar_df[which(mean_climVar_df$Month == month),] 

#creating scatterplot labels based on INPUT 'clim_var'
if (clim_var == 'tmax'){
  ylabel = c("Mean Maximum Temperature (degrees C)")
  maintitle = c(paste("Projected Mean Maximum Temperature for",state$NAME_1,"(2070)"))
  sub_title = c(paste("CMIP5, 'CC' model, RCP",as.numeric(rcp)/10,"| Highlighted point showing data for the month of", month))
} else {
  ylabel = c("Mean Precipitation (mm)")
  maintitle = c(paste("Projected Mean Precipitation for",state$NAME_1,"(2070)"))
  sub_title = c(paste("CMIP5, 'CC' model, RCP",as.numeric(rcp)/10,"| Highlighted point showing data for the month of", month))
}

ggplot(mean_climVar_df, aes(Month,clim_var, group = 1))+
  geom_point(color = 'darkorchid4')+
  geom_line(color = 'darkslategray')+
  geom_point(aes(Month,clim_var, group = 1), data = highlight_point, size = 5, color = 'darkorchid1', fill = 'chartreuse', shape = 21, stroke = 1.1)+
  geom_label_repel(aes(label = clim_var),
                   color = "red",
                   data = highlight_point,
                   box.padding   = 5, 
                   point.padding = 0.5,
                   segment.color = 'black')+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "cornsilk", color = "black"),
        panel.grid.major = element_line(colour = "cornsilk4", linetype = "dotted", size = 0.2),
        panel.grid.minor = element_line(colour = "cornsilk4", linetype = "dotted", size = 0.2))+
  labs(x = "Month", 
       y = ylabel, 
       title = maintitle, 
       subtitle = sub_title)
