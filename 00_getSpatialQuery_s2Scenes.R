# Title: 00_getSpatialQuery_s2Scenes.R
# Status: Finished running on 10/16
# Author: Christiana Ade
# Date: 8/10/19
# Modified: 10/16/2019
# 
# Purpose: Save txt files of avaliable S2A&B level 1 and 2 products in the 
# descending node of orbital path 113 (file saved with s2RFN variable namebelow). 
# Products are filtered for less than and equal 75% cloud cover over the 
# 10SFH tile (which covers the majority of the Delta Legal boundary). 
# The results are for tiles: 10SEH, 10SFH, 10SEG, 10SFG (filtered file 
# saved with s2FilFN variable below)
# 
# **Requires** 
# 1) Login information (username and password) for copnericus sci-hub
# 2) Shape file of study area. (Here a shapefile of merged tile extents was used)
####################################################################################
## require packages
#~! which of these do no not need!!
require(raster)
require(rgdal)
require(stringr)
require(lubridate)
require(espa.tools)
require(tidyverse)
require(rgeos)
require(spdplyr)
require(spatstat)
require(getSpatialData)
require(sp)
require(sf)
require(tictoc)
setwd("Z:\\Cade\\DSC_Delta")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
# 1) Region of interest
myAoi <- readOGR("Z:\\Cade\\DSC_Delta\\Data\\Vector\\S2Tiles\\S2Tiles_4merge_dis.shp")
# 2) Login to cophub
login_CopHub(username = "c.ade92") 
# 3) Set Time Range
timeRange = c("2016-01-01", as.character(Sys.Date()))
# 4) Names of Tiles related to the project
projTiles <- c("10SEH","10SFH","10SEG","10SFG")
# 5) Cloud cover percentage threshold
# filter for clouds with this value and below
ccPr <- 75

#### OUTPUT FILES ####
# 1) out directory 
outDir <- "./Data/Ancil/getSpatialQuery"
# 2) Name for all records in this time period filtered for our area, orbital path 113 and descending node
s2RFN <- paste0(outDir,"/","S2records",gsub("-","",as.character(Sys.Date())),'.txt')
# 3) Name for filtered records in the time period above
s2FilFN <- paste0(outDir,"/","S2records",gsub("-","",as.character(Sys.Date())),"_filteredCC.txt")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### 10/16 
# Finished running for 2016-01-01 until output date
# results saved to beanstore and copied to box folder 
# (./Box/Delta-Sentinel/Data/Ancil/S2Scenes)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##################### PART 1: Query Copernicous Records and Filter Datasets #####################################
## Set AOI ##
set_aoi(myAoi) 

#### Step 1: Query records ####
tic("query s2")
## Use getSentinel_query to search for data (using the session AOI)
records <- getSentinel_query(time_range = timeRange, 
                             platform = "Sentinel-2")
toc(log = T) 

##################### PART 2: Filter records for correct tiles and orbital paths #####################################
# SA stands for study area
# Had to refilter by delta tile names because using the extent of the tiles as an Aoi extends the study area
# past ours. 
records_SA <- records %>%
  # Determine the sensor if its S2A or S2B
  mutate(sensorName = str_extract(title,"(S2A|S2B)"),
         # level of processing
         levelProc = str_extract(title,"(L1C|L2A)"),
         # date of processing
         # ~! Should change for when other years are included
         aDate = str_extract(title,"(2016|2017|2018|2019)\\d{4}"),
         # tile code
         tileCode = str_extract(title,"\\d{2}[A-Z]{3}")) %>%
  # filter by the tide codes over our study area
  filter(tileCode %in% projTiles) %>%
  # filter by descending orbit
  filter( orbitdirection == "DESCENDING") %>%
  # filter by correct relative orbit number.
  filter(relativeorbitnumber == 113) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Records ran 10/16/19
# and saved as "./Data/Ancil/getSpatialQuery/S2records20191016.txt"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# write_delim(records_SA,s2RFN)

##################### PART 3: Filter based on cloud cover in 10SFH Image #####################################
# Because 10SFH covers the main portion of the Delta legal boundary, I am filtering 
# this for 75% cloud cover and matching the resulting dates to the other tiles.
# ~! update this section as it pretains to other projects 
records_10SFH <- records_SA %>%
  # filter out any dates where the cloud cover percentage is greater that 75%
  filter(cloudcoverpercentage <= ccPr) %>%
  # fitler by 10SFH tile code 
  filter(tileCode == "10SFH") 

## filter the longer record by the dates availabe in the 10SFH record
records_fil <- records_SA %>%
  filter(aDate %in% unique(records_10SFH$aDate)) 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Records ran 10/16/19
# and saved as "./Data/Ancil/getSpatialQuery/S2records20191016_filtered.txt"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# write_delim(records_fil,s2FilFN)



