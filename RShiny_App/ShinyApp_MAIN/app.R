#############################################################

### Programmer: Abhinav Shrestha
### Contact information: shre9292@vandals.uidaho.edu

### Purpose: ENVS - 511: Assignment 15 [SHINY APP]

### Last update: 11/22/2022

#############################################################

require(raster)
require(sp)
require(sf)
require(shiny)
require(ggplot2)
require(lubridate)
require(ggrepel)
require(rgdal)
require(rsconnect)


ui = fluidPage(
 fluidRow(column(width = 3, titlePanel(title = tags$a(href = "https://www.wcrp-climate.org/wgcm-cmip/wgcm-cmip5", tags$img(src="CMIP.png", title = "click to visit website"))), p(em("The projections on this dashboard are for the United States of America. The model is set to 'CC' with spatial resolution of 2.5 minutes of degrees and the projected year at 2070. Click above for more info."), hr(), strong("Instructions:"), br(), "First use the", strong("Parameters"), "window to set parameters for the climate projection model. Then use the", strong("Comparisions"), "window to compare projection data by month for the chosen US state.")), 
          column(width = 9, titlePanel(title = tags$a(tags$img(src="dashboard.png", title = "Congrats! You found the hidden easter egg, I got nothing to give you tho :)", width = "90%"))))
 ),
 hr(tags$style(HTML("hr {border-top: 1px solid #8c8b8b;}"))),
 titlePanel("Parameters:"),
 p(),
 fluidRow(column(width = 4, selectInput(inputId = "state", label = "Select state", choices = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))),
                
          column(width = 4, radioButtons(inputId = "clim_var", label = "Select climate variable (precipitation or max temperature)", choices = c("prec", "tmax"), inline = TRUE)), 
                
          column(width = 4, radioButtons(inputId = "rcp", label = "Select Representitve Concentration Pathway (RCP)", choices = c("2.6"="26", "4.5" = "45", "6.0" = "60", "8.5" = "85"), inline = TRUE))),
 
 hr(),
 titlePanel("Comparisons:"),
 fluidRow(column(width = 3, selectInput(inputId = "month1", label = "Select 1st month to compare", choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))),
 fluidRow(column(width = 4, plotOutput(outputId = "projected_raster1")),
          column(width = 8, plotOutput(outputId = "projected_plot1"))),
 
 fluidRow(column(width = 3, selectInput(inputId = "month2", label = "Select 2nd month to compare", choices = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))),
 fluidRow(column(width = 4, plotOutput(outputId = "projected_raster2")),
          column(width = 8, plotOutput(outputId = "projected_plot2")))

 )

server = function(input, output){
  
###SETTING REACTIVE VARIABLES-------------------------------------------------------------------------------------------------
  ### Import data with map of US (VECTOR)
  usa <- getData('GADM', country='USA', level = 1)
  
  ## Extract vector file of only INPUT 'state'
  state <- reactive({usa[usa$NAME_1 == input$state,]})
  #plot(state)
  
  
  ### Import raster of CC model with INPUT 'clim_var' and INPUT 'rcp'
  #?getData #to refer to the necessary format required to download the climate projection data
  
  CC_data <- reactive({ getData('CMIP5', #'CMIP5' Phase five of the Coupled Model Intercomparison Project (CMIP5) used to get projected (future) climate data  
                                var = input$clim_var, #for precipitation data
                                res = 2.5, #CONSTANT
                                rcp = as.numeric(input$rcp), 
                                model = 'CC', #CONSTANT
                                year = 70) #CONSTANT
  })
  
  
  ## Clip raster data to vector file of Montana using 'crop' and 'mask' function
  state_CC_data_clip <- reactive({crop(CC_data(),state())}) #raster file containing projected precipitation data for the entire world cropped to the extent of the 'montana' vector file 
  
  state_CC_data <- reactive({mask(state_CC_data_clip(), state())})
  
###PLOTTING THE TWO OUTPUTS MONTH 1----------------------------------------------------------------------------------------------------
  
  output$projected_raster1 = renderPlot({
    
  #OUTPUT PLOT - Prediction of INPUT 'clim_var' for INPUT 'month' under INPUT 'rcp' conditions: 
  #renaming raster stack layers
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
  state_CC_data_Rename <- state_CC_data()
  
  for(i in 1:12){
    state_CC_data_Rename@data@names[i] <- paste(months[i]) 
  }
  
  month_plot <- subset(state_CC_data_Rename, input$month1)
  
  
  #output plot based on 'clim_var' input, need to divide by 10 for 'tmax' input
  
  if (input$clim_var == 'tmax'){
    plot(month_plot/10,
         legend.args = list (text = 'Projected Mean Maximum Temperature (degrees C)', #'legend.args' used to modify legend elements
                             side = 4, #side 1 starts at the bottom of the element and goes clockwise
                             font = 2, #type of font used
                             line = 2.75, #distance from element
                             cex = 1), #size of font
         main = c(paste("Projected Mean Maximum Temperature for",state()$NAME_1,"(2070).",
                        "CMIP5, 'CC' model, RCP",as.numeric(input$rcp)/10),input$month1),
         col = rev(heat.colors(12)))
  } else {
    plot(month_plot, 
         legend.args = list (text = 'Projected Mean Precipitation (mm)', #'legend.args' used to modify legend elements
                             side = 4, #side 1 starts at the bottom of the element and goes clockwise
                             font = 2, #type of font used
                             line = 2.75, #distance from element
                             cex = 1), #size of font
         main = c(paste("Projected Mean Precipitation for",state()$NAME_1,"(2070).",
                        "CMIP5, 'CC' model, RCP",as.numeric(input$rcp)/10),input$month1), 
         col = rev(topo.colors(12)))
    
  }

  })
 
  output$projected_plot1 = renderPlot({
  #Creating an empty data frame to store mean clim_var calculations.
  mean_climVar_df <- data.frame(matrix(ncol=2, nrow=12))
  colnames(mean_climVar_df) <- c("Month", "clim_var")
  mean_climVar_df$Month <- c(month(ymd(700101) + months(0:11), label = TRUE))

  if (input$clim_var == 'tmax'){
    for(i in 1:12){
      temp_raster <- subset(state_CC_data(),i)
      mean_climVar_df$clim_var[i] <- (round(((mean(temp_raster@data@values, na.rm=TRUE))/10), 2))
    }
  } else {
    for(i in 1:12){
      temp_raster <- subset(state_CC_data(),i)
      mean_climVar_df$clim_var[i] <- (round((mean(temp_raster@data@values, na.rm=TRUE)), 2))
    }
  }

  highlight_point <- mean_climVar_df[which(mean_climVar_df$Month == input$month1),]

  #creating scatterplot labels based on INPUT 'clim_var'

  if (input$clim_var == 'tmax'){
    ylabel = c("Mean Maximum Temperature (degrees C)")
    maintitle = c(paste("Projected Mean Maximum Temperature for",state()$NAME_1,"(2070)"))
    sub_title = c(paste("CMIP5, 'CC' model, RCP",(as.numeric(input$rcp)/10),"| Highlighted point showing mean data for the month of", input$month1))
  } else {
    ylabel = c("Mean Precipitation (mm)")
    maintitle = c(paste("Projected Mean Precipitation for",state()$NAME_1,"(2070)"))
    sub_title = c(paste("CMIP5, 'CC' model, RCP",(as.numeric(input$rcp)/10),"| Highlighted point showing mean data for the month of", input$month1))
  }

  
  ggplot(mean_climVar_df, aes(Month,clim_var, group = 1))+
    geom_point(color = 'darkorchid4')+
    geom_line(color = 'darkslategray')+
    geom_point(aes(Month,clim_var, group = 1), data = highlight_point, size = 5, color = 'darkorchid1', fill = 'chartreuse', shape = 21, stroke = 1.1)+
    theme_minimal()+
    theme(panel.background = element_rect(fill = "cornsilk", color = "black"),
          panel.grid.major = element_line(colour = "cornsilk4", linetype = "dotted", size = 0.2),
          panel.grid.minor = element_line(colour = "cornsilk4", linetype = "dotted", size = 0.2))+
    labs(x = "Month",
         y = ylabel,
         title = maintitle,
         subtitle = sub_title)+
    theme(plot.title = element_text(face="bold", size = 16), 
          plot.subtitle = element_text(size = 16), 
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 16))+
    geom_label_repel(aes(label = clim_var),
                     color = "red",
                     data = highlight_point,
                     box.padding   = 5,
                     point.padding = 0.5,
                     segment.color = 'black')

  })
  
  ###PLOTTING THE TWO OUTPUTS MONTH 2 ----------------------------------------------------------------------------------------------------
  
  output$projected_raster2 = renderPlot({
    
    #OUTPUT PLOT - Prediction of INPUT 'clim_var' for INPUT 'month' under INPUT 'rcp' conditions: 
    #renaming raster stack layers
    months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    
    state_CC_data_Rename <- state_CC_data()
    
    for(i in 1:12){
      state_CC_data_Rename@data@names[i] <- paste(months[i]) 
    }
    
    month_plot <- subset(state_CC_data_Rename, input$month2)
    
    
    #output plot based on 'clim_var' input, need to divide by 10 for 'tmax' input
    
    if (input$clim_var == 'tmax'){
      plot(month_plot/10,
           legend.args = list (text = 'Projected Mean Maximum Temperature (degrees C)', #'legend.args' used to modify legend elements
                               side = 4, #side 1 starts at the bottom of the element and goes clockwise
                               font = 2, #type of font used
                               line = 2.75, #distance from element
                               cex = 1), #size of font
           main = c(paste("Projected Mean Maximum Temperature for",state()$NAME_1,"(2070).",
                          "CMIP5, 'CC' model, RCP",as.numeric(input$rcp)/10),input$month2),
           col = rev(heat.colors(12)))
    } else {
      plot(month_plot, 
           legend.args = list (text = 'Projected Mean Precipitation (mm)', #'legend.args' used to modify legend elements
                               side = 4, #side 1 starts at the bottom of the element and goes clockwise
                               font = 2, #type of font used
                               line = 2.75, #distance from element
                               cex = 1), #size of font
           main = c(paste("Projected Mean Precipitation for",state()$NAME_1,"(2070).",
                          "CMIP5, 'CC' model, RCP",as.numeric(input$rcp)/10),input$month2), 
           col = rev(topo.colors(12)))
      
    }
    
  })
  
  output$projected_plot2 = renderPlot({
    #Creating an empty data frame to store mean clim_var calculations.
    mean_climVar_df <- data.frame(matrix(ncol=2, nrow=12))
    colnames(mean_climVar_df) <- c("Month", "clim_var")
    mean_climVar_df$Month <- c(month(ymd(700101) + months(0:11), label = TRUE))
    
    if (input$clim_var == 'tmax'){
      for(i in 1:12){
        temp_raster <- subset(state_CC_data(),i)
        mean_climVar_df$clim_var[i] <- (round(((mean(temp_raster@data@values, na.rm=TRUE))/10), 2))
      }
    } else {
      for(i in 1:12){
        temp_raster <- subset(state_CC_data(),i)
        mean_climVar_df$clim_var[i] <- (round((mean(temp_raster@data@values, na.rm=TRUE)), 2))
      }
    }
    
    highlight_point <- mean_climVar_df[which(mean_climVar_df$Month == input$month2),]
    
    #creating scatterplot labels based on INPUT 'clim_var'
    
    if (input$clim_var == 'tmax'){
      ylabel = c("Mean Maximum Temperature (degrees C)")
      maintitle = c(paste("Projected Mean Maximum Temperature for",state()$NAME_1,"(2070)"))
      sub_title = c(paste("CMIP5, 'CC' model, RCP",(as.numeric(input$rcp)/10),"| Highlighted point showing mean data for the month of", input$month2))
    } else {
      ylabel = c("Mean Precipitation (mm)")
      maintitle = c(paste("Projected Mean Precipitation for",state()$NAME_1,"(2070)"))
      sub_title = c(paste("CMIP5, 'CC' model, RCP",(as.numeric(input$rcp)/10),"| Highlighted point showing mean data for the month of", input$month2))
    }
    
    
    ggplot(mean_climVar_df, aes(Month,clim_var, group = 1))+
      geom_point(color = 'darkorchid4')+
      geom_line(color = 'darkslategray')+
      geom_point(aes(Month,clim_var, group = 1), data = highlight_point, size = 5, color = 'darkorchid1', fill = 'chartreuse', shape = 21, stroke = 1.1)+
      theme_minimal()+
      theme(panel.background = element_rect(fill = "cornsilk", color = "black"),
            panel.grid.major = element_line(colour = "cornsilk4", linetype = "dotted", size = 0.2),
            panel.grid.minor = element_line(colour = "cornsilk4", linetype = "dotted", size = 0.2))+
      labs(x = "Month",
           y = ylabel,
           title = maintitle,
           subtitle = sub_title)+
      theme(plot.title = element_text(face="bold", size = 16), 
            plot.subtitle = element_text(size = 16), 
            axis.text = element_text(size = 16), 
            axis.title = element_text(size = 16))+
      geom_label_repel(aes(label = clim_var),
                       color = "red",
                       data = highlight_point,
                       box.padding   = 5,
                       point.padding = 0.5,
                       segment.color = 'black')
    
  })
}


shinyApp(ui = ui, server = server)