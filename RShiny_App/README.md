# ENVS 511 R-ShinyApp Repo

## About the R-ShinyApp Repo:
The R-ShinyApp repo contains: 

*  `SERVER_END_MAIN.R` and `ServerEND_SANDBOX.R` R files. These R files are initial R code written for the server end of the shiny app. The `ServerEND_SANDBOX.R` R file was used for testing out various methods and workflows. The `SERVER_END_MAIN.R` R file is the final code used in the shiny app UI-server integrated R file (`app.R`)
* The "ShinyAPP_MAIN" sub directory consists of the `app.R` R file. This R file is the code base for the ShinyApp published online (see link below). The R file integrates the user end (UI) and the logic/database end (SERVER) into a cohesive webapp.

### **To navigate to the Rpubs page with the summary of the work done in R for the final project, please click the following link: [R SHINYAPP (click here)](https://abhinavshrestha.shinyapps.io/Assignment15_ShinyApp/)**  

### Some information about the R-ShinyApp:
* The app is a dashboard-style app that allows users to compare and contrast temperature and precitpitation predictions made using the Coupled Model Intercomparison Project Phase 5 ([CMIP5](https://www.wcrp-climate.org/wgcm-cmip/wgcm-cmip5)) climate prediction model from the World Research Climate Programme ([WRCP](https://www.wcrp-climate.org/)). 
* The projections on this ShinyApp dashboard are for the United States of America. The model is set to 'CC' with spatial resolution of 2.5 minutes of degrees and the projected year at 2070. 
* The user is able to input the US state, temperature or precitpitation variable, month for projection, and representative concentration pathway ([RCP](https://www.ipcc-data.org/guidelines/pages/glossary/glossary_r.html)) as paramters for the output of the projections.
* The outputs are a raster (image) of the state with the projected variable (maximum temperature or average precipitation) and a scatter plot of the projected variable by month with the input month highlighted. 

<hr>
