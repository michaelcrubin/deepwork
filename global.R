# This Script is executed in the beginning and is loaded into the Global Environnment.

# some Options
options( warn = -1 )
options(scipen=999)
rm(list=ls())

Sys.setenv("session_id"=sample(1:9999999, 1))


## LOAD CRAN PACKAGES -------------
load_R_libraries <- function(){
  
  # Shiny Libraries
  library(shiny)
  library(shinydashboardPlus)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyjs)
  library(shinyBS)
  library(shiny.i18n)
  library(shinydashboard)
  library(bs4Dash)
  library(sortable)
  #library(shinyTree)

  
  # Tidyverse stuff
  #library(tidyverse)
  library(dplyr)
  library(purrr)
  library(lubridate) 
  library(magrittr)
  library(readr)
  library(tibble)
  library(tidyr)
  library(stringr)
  
  # Geo Stuff
  # library(leaflet)
  # library(raster)
  # library(leafem)
  # library(sp)
  # library(sf)
  # sf_use_s2(FALSE) # This is important
  #library(rgdal)
  #library(rgeos)
  #library(gstat)
  #library(rmapshaper)
  #library(stars)
 # library(spData)
  # Attention: for this, you have to install the leaflet extras package from the github as below. This is important. The best is to scrap the code 
  # and store it somewhere in our premises
  #remotes::install_github("bhaskarvk/leaflet.extras", ref = remotes::github_pull("184"), force = TRUE)
  #library(leaflet.extras)
  
  # Graphics + Tables
  library(DT)
  # library(xts)
  library(plotly)
  #library(dygraphs)
  
  # others
  library(pool) ## do I need that here or only in package???
  library(zip) ## do i need that??? possibly obly inside map package
  library(configr) #3 will be eliminated as soon as I source system params from DB
  library(sessioninfo)
  library(RCurl)
 # library(Rcpp)
  #library(paws) # For AWS integration
 # library(future) # For Async calls
  #library(promises) # For Async calls
 # plan(multisession)
  #library(curl) # To download stuff
  
 
 # library(data.table) # note: I am not sure if this is used. is this the same as DT?? DT I use for sure.
  #library(knitr) # PROBABLY I dont use knitr, but just kable extra of which knitr is a dependency.
  #library(kableExtra)
  #library(formattable)
  # library(here) # maybe it's not necessary as you said doesn't work in could. need to recode all parts where it's used.
  #library(sortable)
  #library(devtools) # probably I don't use it
  # 
  # 
  # library(DBI)
  # library(RMariaDB) 
  # library(RMySQL)
  # library(readr)
  # library(here)
  # library(pool)
  # library(configr)
  # library(shiny.i18n)
  # library(DT)
  # library(knitr)
  # library(kableExtra)
  # library(rmarkdown)
  # library(tinytex)
  # library(shinydashboardPlus)
  # 
  # library(shinyjs)
  # library(lubridate)
  # library(plotly)

}
load_R_libraries()


# ## LOAD ODAPES PACKAGES LAYER 3 + 4 -------------
# remotes::install_github("ODAPES/OdsDataHelper",ref="master", dependencies = TRUE , force = TRUE)
# remotes::install_github("ODAPES/OdsDBHelper",ref="master", dependencies = TRUE , force = TRUE)
# remotes::install_github("ODAPES/OdsUIHelper",ref="master",  dependencies = TRUE , force = TRUE)
# remotes::install_github("ODAPES/OdsColor",ref="master", dependencies = TRUE , force = TRUE)
# 
# remotes::install_github("ODAPES/OdsUser",ref="main", dependencies = TRUE , force = TRUE)
# remotes::install_github("ODAPES/OdsDataLoad",ref="main", dependencies = TRUE , force = TRUE)
# remotes::install_github("ODAPES/OdsDataStore",ref="master", dependencies = TRUE , force = TRUE)
# remotes::install_github("ODAPES/OdsCommunication",ref="master", dependencies = TRUE , force = TRUE)
# 
# remotes::install_github("ODAPES/OdsUIUX",ref="master", dependencies = TRUE , force = TRUE)
# remotes::install_github("ODAPES/OdsTable",ref="master", dependencies = TRUE , force = TRUE)
# renv::snapshot()
# renv::restore()
# renv::init()

library(OdsDataHelper)
library(OdsDBHelper)
library(OdsUIHelper)
library(OdsColor)

library(OdsUIUX)
library(OdsTable)
#library(OdsDataStore)
#library(OdsAPI)

#library(OdsUser)
library(OdsDataLoad)
library(OdsCommunication)

# unfinished packages
#library(OdsGIS)
#library(OdsAWSIntegration)
#library(OdsDataAnalytics)
# note that the API is only sources once required

## Function to make soruce token url

token_url <- function(lst){
  lst %>% purrr::keep(isTRUE) %>% names() %>% 
    paste0("https://", Sys.getenv("GITHUB_TOKEN"), "@", "raw.githubusercontent.com/ODAPES/SHARED_MODULES/master","/" ,., ".R") 
}



## CREATE DB CONNECTION -------------
client_db_pool <- make_clientdb_pool()

