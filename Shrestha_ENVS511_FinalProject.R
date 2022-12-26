library(sp)
library(sf)
library(raster)
library(rLiDAR)
library(lidR)
library(rgdal)
library(terra)
library(ggplot2)
library(rgl)
library(psych)
library(plotly)
library(MASS)
library(gginnards)
library(gridExtra)
library(agricolae)
library(data.table)

# ### Preparing point cloud dataset for each treetype
# ## Clipping manually digitized polygon to site point cloud data set to create point cloud profiles of individual tree types. 
# ### NOT RUN
# ### IMPORTING point cloud as LAS catalog (easier to clip multifeature polygon) -------------------
# las <- readLAScatalog("~\Site_pointcloud.las")
# 
# las$CRS #to check the CRS of the imported las. Returns 6340 -> EPSG code for NAD83(2011) / UTM zone 11N 
# 
# ### Importing shapefile for clipping in spatial 'simple feature' (sf) class -------------------
# sf <- st_read(dsn = "~\TreeType.shp")                                                                                                                                                          
# st_crs(sf) #make sure both point cloud dataset and shapefile are in the same CRS (las$CRS == st_crs(sf))
# 
# ### Clipping PC to shp -------------------
# ## Set destination file path and filenaming rule (based on 'ID' feature of 'sf' dataset) 
# opt_output_files(las) <- "~\Tree_{TreeType}"
# 
# ## Clip PC to shp
# clipped_las <- clip_roi(las,sf)

### Importing Point Cloud as .txt file
topkill <- read.table('C:\\Users\\abhin\\Documents\\GitHub\\ENVS511_FinalProject\\TopKill_Set.txt', sep = "\t", header = TRUE)

graytree <- read.table('C:\\Users\\abhin\\Documents\\GitHub\\ENVS511_FinalProject\\GrayTree_Set.txt', sep = "\t", header = TRUE)

fulltree <- read.table('C:\\Users\\abhin\\Documents\\GitHub\\ENVS511_FinalProject\\FullTree_Set.txt', sep = "\t", header = TRUE)

### Renaming the columns of the imported dataframes
colnames(topkill) <- c("x", "y", "z", "blue", "green", "red", "RedEdge", "NIR", "R", "G", "B")
colnames(graytree) <- c("x", "y", "z", "blue", "green", "red", "RedEdge", "NIR", "R", "G", "B")
colnames(fulltree) <- c("x", "y", "z", "blue", "green", "red", "RedEdge", "NIR", "R", "G", "B")


### Calibration/corrections from Agisoft Metashape Calibration workflow (MicaSense RedEdge MX sensor) stores the point cloud data by multiplying by 10000 (for efficient storage), need to divide by 10000 to get true reflectance values

for (i in 4:8) {
  topkill[i] <- (topkill[i]/10000)
}

for (i in 4:8) {
  graytree[i] <- (graytree[i]/10000)
}

for (i in 4:8) {
  fulltree[i] <- (fulltree[i]/10000)
}


# Initial data exploration 
ft <- ggplot(fulltree)+
        geom_density(aes(x = blue, color = "blue")) +
        geom_density(aes(x = green, color = "green")) +
        geom_density(aes(x = red, color = "red")) +
        geom_density(aes(x = RedEdge, color = "RedEdge")) +
        geom_density(aes(x = NIR, color = "NIR")) +
        scale_color_manual(values = c("blue", "green", "orange", "red", "pink"), 
                           name = "Wavelength") +
        labs(x = "Wavelength (nm)", 
             y = "Density",
             title = "Density distribution: spectral profile of a full green ('healthy') tree")+
        theme(plot.title=element_text(size=10))
  
gt <- ggplot(graytree)+
        geom_density(aes(x = blue, color = "blue")) +
        geom_density(aes(x = green, color = "green")) +
        geom_density(aes(x = red, color = "red")) +
        geom_density(aes(x = RedEdge, color = "RedEdge")) +
        geom_density(aes(x = NIR, color = "NIR")) +
        scale_color_manual(values = c("blue", "green", "orange", "red", "pink"), 
                           name = "Wavelength") +
        labs(x = "Wavelength (nm)", 
             y = "Density",
             title = "Density distribution: spectral profile of a full gray ('dead') tree")+
  theme(plot.title=element_text(size=10))

tk <- ggplot(topkill)+
        geom_density(aes(x = blue, color = "blue")) +
        geom_density(aes(x = green, color = "green")) +
        geom_density(aes(x = red, color = "red")) +
        geom_density(aes(x = RedEdge, color = "RedEdge")) +
        geom_density(aes(x = NIR, color = "NIR")) +
        scale_color_manual(values = c("blue", "green", "orange", "red", "pink"), 
                           name = "Wavelength") +
        labs(x = "Wavelength (nm)", 
             y = "Density",
             title = "Density distribution: spectral profile of a top-kill ('damaged') tree")+
  theme(plot.title=element_text(size=10))

grid.arrange(ft, gt, tk, nrow = 2, ncol = 2)

### Copying imported datasets into a new dummy dataframe to create new column 'TreeType', then aggregate dummy dataframes into a main dataframe with point cloud data from all types of trees. 
fulltree_temp <- fulltree
graytree_temp <- graytree
topkill_temp <- topkill

fulltree_temp$TreeType <- "Full_Tree"
graytree_temp$TreeType <- "Gray_Tree"
topkill_temp$TreeType <- "Top_Kill"

#main dataframe called 'main_data' and setting Tree Type as factor
main_data <- rbind(fulltree_temp,graytree_temp,topkill_temp)
main_data$TreeType <- factor(main_data$TreeType)

# creating a concatenated list of colors for creating plots
mycolors <- c("chartreuse4", "dark gray", "orange") #for green, gray, and top-kill trees respectively. 

#density plots of each band by tree type 

blue <- ggplot(main_data, aes(blue, color = TreeType))+
          geom_density(linewidth = 0.75)+
          scale_color_manual(values = mycolors)+
          labs(x = "Wavelength (nm)", 
               y = "Density", 
               title = "Blue")+
          theme(legend.position = "none")
green <- ggplot(main_data, aes(green, color = TreeType))+
  geom_density(linewidth = 0.75)+
  scale_color_manual(values = mycolors)+
  labs(x = "Wavelength (nm)", 
       y = "Density", 
       title = "Green")+
  theme(legend.position = "none")

red <- ggplot(main_data, aes(red, color = TreeType))+
  geom_density(linewidth = 0.75)+
  scale_color_manual(values = mycolors)+
  labs(x = "Wavelength (nm)", 
       y = "Density", 
       title = "Red")+
  theme(legend.position = "none")

RedEdge <- ggplot(main_data, aes(RedEdge, color = TreeType))+
  geom_density(linewidth = 0.75)+
  scale_color_manual(values = mycolors)+
  labs(x = "Wavelength (nm)", 
       y = "Density", 
       title = "RedEdge")+
  theme(legend.position = "none")

NIR <- ggplot(main_data, aes(NIR, color = TreeType))+
  geom_density(linewidth = 0.75)+
  scale_color_manual(values = mycolors,
                     name = "Tree Type", 
                     labels = c("Green 'Healthy' Tree", "Gray 'Dead' Tree", "Top-kill 'Damaged' Tree"))+
  labs(x = "Wavelength (nm)", 
       y = "Density", 
       title = "NIR")

grid.arrange(arrangeGrob(blue, green, red, ncol=3, nrow=1),
             arrangeGrob(RedEdge, NIR, ncol=2, nrow=1, widths=c(1,2))) 

#Creating new columns for indices and performing calculations
main_data$NDVI <- (main_data$NIR-main_data$red)/(main_data$NIR+main_data$red)
main_data$NDRE <- (main_data$NIR-main_data$RedEdge)/(main_data$NIR+main_data$RedEdge)
main_data$SR <- (main_data$NIR/main_data$red)
main_data$GLI <- ((main_data$green - main_data$red)+(main_data$green - main_data$blue))/((2*main_data$green)+main_data$red+main_data$blue)

#visualizing spectral indices ~ type of tree
NDVI <- ggplot(main_data)+
          geom_density(aes(NDVI, color = TreeType))+
          scale_color_manual(values = mycolors,
                             name = "Tree Type", 
                             labels = c("Green 'Healthy' Tree", "Gray 'Dead' Tree", "Top-kill 'Damaged' Tree"))+
            labs(x = "Wavelength (nm)", 
                 y = "Density", 
                 title = "NDVI")

NDRE <- ggplot(main_data)+
          geom_density(aes(NDRE, color = TreeType))+
          scale_color_manual(values = mycolors,
                             name = "Tree Type", 
                             labels = c("Green 'Healthy' Tree", "Gray 'Dead' Tree", "Top-kill 'Damaged' Tree"))+
          labs(x = "Wavelength (nm)", 
               y = "Density", 
               title = "NDRE")


SR <- ggplot(main_data)+
        geom_density(aes(SR, color = TreeType))+
        scale_color_manual(values = mycolors,
                           name = "Tree Type", 
                           labels = c("Green 'Healthy' Tree", "Gray 'Dead' Tree", "Top-kill 'Damaged' Tree"))+
        labs(x = "Wavelength (nm)", 
             y = "Density", 
             title = "SR")


GLI <- ggplot(main_data)+
        geom_density(aes(GLI, color = TreeType))+
        scale_color_manual(values = mycolors,
                           name = "Tree Type", 
                           labels = c("Green 'Healthy' Tree", "Gray 'Dead' Tree", "Top-kill 'Damaged' Tree"))+
        labs(x = "Wavelength (nm)", 
             y = "Density", 
             title = "GLI")

grid.arrange(NDVI, NDRE, SR, GLI, nrow = 2, ncol = 2)


#ANOVA 

#Normality check 
ggplot(main_data, aes(GLI))+
  geom_density()+
  labs(x = "Wavelength (nm)", 
       y = "Density")

model1 <- aov(GLI~TreeType, data = main_data)
summary(model1)

##Diagnostics on Model1:
par(mfrow = c(2,2))
plot(model1)


res <- residuals(model1)
est <- fitted(model1)

hist(res) #assumption1
plot(est, res); abline(0,0)
qqnorm(res);qqline(res)

#Mutliple comparision using Tukey's HSD: 
HSD1 <- HSD.test(model1, 'TreeType', group = TRUE, console = TRUE)
plot(HSD1)

tukeyLabels <- c("a", "b", "c")
quants <- data.table(main_data)[, list(quant = as.numeric(quantile(GLI)[3])), by = TreeType]

ggplot(main_data, aes(TreeType,GLI, fill = TreeType)) +
  geom_boxplot(size = 0.75) + 
  labs(title = "Tukey's HSD results", 
       x = "Tree Type", 
       y = "Green Leaf Index (GLI)") +
  scale_fill_manual(values = mycolors)+
  scale_x_discrete(labels = c("Green 'Healthy' Tree", "Gray 'Dead' Tree", "Top-kill 'Damaged' Tree"))+
  guides(fill = "none")+
  geom_text(data = quants, aes(x = TreeType, y = quant, label = tukeyLabels), size = 10)

#2D density plot shows there is a potential grouping with GLI index for each point in the point cloud and the z value of each point of on the point cloud. 
ggplot(main_data, aes(GLI, z, color = TreeType))+
  geom_density_2d(linewidth = 1)+
  scale_color_manual(values = mycolors,
                     name = "Tree Type", 
                     labels = c("Green 'Healthy' Tree", "Gray 'Dead' Tree", "Top-kill 'Damaged' Tree"))+
  labs (x = "Green Leaf Index", 
        y = "Elevation (z)", 
        title = "GLI and Elevation density by Tree Type")

# The above shown 2D density plot can also be visualized with color intensity (purple to yellow color scale) using 'geom_density_2d_filled'. Yellower colors represent higher point density. 
ggplot(main_data, aes(GLI, z))+
  geom_density_2d_filled(contour_var = "ndensity")+
  geom_density_2d(aes(color = TreeType))+
  scale_color_manual(values = mycolors,
                     name = "Tree Type", 
                     labels = c("Green 'Healthy' Tree", "Gray 'Dead' Tree", "Top-kill 'Damaged' Tree"))+
  labs (x = "Green Leaf Index", 
        y = "Elevation (z)", 
        title = "GLI and Elevation density by Tree Type")+
  guides(fill = "none")

# The above 2D filled density plot can also be faceted into tree types: 
ggplot(main_data, aes(GLI, z))+
  geom_density_2d_filled(contour_var = "ndensity")+
  geom_density_2d(aes(color = TreeType))+
  facet_wrap(TreeType~.)+
  scale_color_manual(values = mycolors,
                     name = "Tree Type", 
                     labels = c("Green 'Healthy' Tree", "Gray 'Dead' Tree", "Top-kill 'Damaged' Tree"))+
  labs (x = "Green Leaf Index", 
        y = "Elevation (z)", 
        title = "GLI and Elevation density by Tree Type")+
  guides(fill = "none")

# 3D representation of 2D density plots: 

## Using rayshader R-package: 
Density2D <- ggplot(main_data, aes(GLI, z))+
  stat_density_2d_filled(aes(fill = stat(nlevel)))+
  facet_wrap(TreeType~.)+
  labs (x = "Green Leaf Index", 
        y = "Elevation (z)", 
        title = "GLI and Elevation density by Tree Type")+
  scale_fill_viridis_c(option = "A")

# par(mfrow = c(1, 2))
# plot_gg(Density2D, width = 4, height = 4, raytrace = FALSE, preview = TRUE)
plot_gg(Density2D, 
        multicore = TRUE, 
        raytrace = TRUE, 
        width = 4, 
        height = 4, 
        scale = 300, 
        windowsize = c(1200, 960), 
        zoom = 0.5, 
        phi = 50, 
        theta = 35)

Sys.sleep(0.2)
#render_snapshot(clear = TRUE) #un-comment to create a snapshot image of the 3D view (downloadable) 
rgl::rglwidget()

# Another interactive 3D plot created using the [plotly](https://plotly.com/r/3d-charts/) R-package to represent the above 2D density plot:

kd <- kde2d(main_data$GLI, main_data$z)

fig <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()
fig
 




