# Title: 01_DownloadS2data.R
# Status: PENDING 
# Move filL2A function to a function folder
# Create better error handling for products that are archived on copernicus 
# and can no longer be downloaded (line ~107)
# Author: Christiana Ade
# Date: 8/10/2019
# Modified: 10/16/209 
# Purpose: Download Sentinel-2 Scenes using getSpatialData package.
# If both a level 1c and level 2A product are available, only the level 2a is downloaded
# **Requires** 
# 1) Txt file of scenes user wants to download in the format for the package getSpatialData
# 2) a username and password to copernicus open access hubs
####################################################################################
## require packages
require(tidyverse)
require(stringr)
require(getSpatialData)
require(tictoc)
library(parallel)
library(doParallel)
library(foreach)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START USER DEFINED VARIABLES       #### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The following variables should be defined by the user each time.
#### INPUT FILES ####
# 1) Sentinel-2 records
s2Records <- read_delim('./Data/Ancil/getSpatialQuery/S2records20191016_filteredCC.txt', delim = " ")
# 2) UserName
un <- "c.ade92"
# 3) Password
pw <- "tuft9are"
# 4) Importance of tile downloads 
target <- c("10SFH","10SFG","10SEH","10SFJ")

#### OUTPUT FILES ####
# 1) outdirectory level 1
outL1 <- "./Data/Raster/S2_Level1"

#### USER DEFINED FUNCTIONS ####

# filL2A filters removes any entries that are Level 1C
# that already have Level 2A equivalents availabe on the hub
# The function only takes a tibble or dataframe and must have a 
# levelProc column as created in script 00_getSpatialQuery_s2Scenes.R
filL2A <- function(df){
  # if there is an L2A in the processing level
  if ("L2A" %in% df$levelProc) {
    # determine which entries are L2A products
    l2a_pos <- which(str_detect(df$levelProc,"L2A"))
    # keep these products only
    df_fil <- df %>% slice(l2a_pos)
    return(df_fil) 
  } else {
    return(df)
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             END USER DEFINED VARIABLES        ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####             START SCRIPT                      ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##################### PART 1: Filter Records for L1C Scenes that have already been processed  or downloaded #####################################
# list directories in level1 and level outputfolders
s2List <- list.dirs(outL1, recursive = F, full.names = F)
# add any additional files that might have been downloaded, but not unzipped
s2Zip <- str_extract(list.files(outL1, pattern = ".zip"),".+?(?=\\.zip)")

# filter by previously downloaded names
s2RecordsFil <- s2Records %>% 
  # nest by date and sentinel sensor (S2A or S2B)
  group_by(sensorName,aDate) %>%
  nest() %>%
  # filter out all L1C if there is L2A available
  mutate(data = map(.f=filL2A, .x =data)) %>%
  unnest() %>%
  # remove if already downloaded and unzipped #~!
  filter(!filename %in% s2List) %>%
  # remove if already downloaded, but still zipped 
  filter(!title %in% s2Zip) %>%
  # arrange tiles by importance
  arrange(match(tileCode, target)) %>%
  # arrange by cloud coverage 
  arrange(cloudcoverpercentage) %>% 
  #~!filter out cloud coverage # this should have already worked in the other script, but apparently some were missing
  filter(!cloudcoverpercentage >75)


##################### PART 2: Download New files #####################################

#### Step 1: set up for parallel downloading ####
# detect number of cores
no_cores <- detectCores() - 1
cl <- makeCluster(no_cores, type = "PSOCK")
registerDoParallel(cl)

#### Step 2: Download Records ####

tic("download")
files <- foreach(i = 1:nrow(s2RecordsFil),
                 .combine=c,
                 .packages='getSpatialData') %dopar% {
                   tryCatch(getSentinel_data(s2RecordsFil[i, ], dir_out = outL1,username = un,
                                    password = pw ),error=function(e) NULL)
                   print(s2RecordsFil[i, ])
                 }

toc(log = T)



