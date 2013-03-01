## Compile Tundra and Boreal Fire Metric Data #### for ALF tundra dynamics IEM group 11/16/2012
#  
#  The goal of this script is to extract fire dynamics separately for the boreal and tundra areas of Alaska
#  from simulated runs from ALFRESCO. At minimum you need to output Age maps from 1950-2012.
#
#  A script has already been run on empirical data. An R workspace has been saved with this information
#  already compiled in it, and it is accessed in the script. If for whatever reason you need to find and copy 
#  this R workspace to another location, it is found here: Y:/anna_springsteen/Tundra Model Validation/
#  and is called 'PartI_Empirical_Data.RData'.  You will need to change the pathname to this file in the script
#  at line 36.
#
#  As alluded above, this script is only concerned with years 1950-2012. 
#
#  Before running the script, you will need to make sure to change the two lines with a 'user-specified' comment 
#  at the top of this script to correct values and/or pathnames that match the ALF output 
#  from which you want to compile data.
#
#  When the user-specified changes are made, simply select all, copy, and paste into an R window. 


# USER-SPECIFIED VALUES

# End Rep Number
startrep <- 0       										
endrep <- 199 #199    												# user-specified
reps <- startrep:endrep

options(warn=1)

# Set Workspace 
# (this is the folder to which the ALFRESCO output is saved)
setwd('/big_scratch/apbennett/ALFRESCO-ALEC/TBA_ECHAM')			# user-specified


######################################################################
load('/big_scratch/apbennett/ALFRESCO-ALEC/TBA_ECHAM/PartI_Empirical_Data.RData')
# read in simulated fire files and record:
# 1) the annual area burned (total # of cells with a value of 0)
# 2) # of fires (use clump function)
# 3) fire sizes (similar to 2) above w/ clump function)

# ADD REQUIRED LIBRARIESm
library(grid)
library(sp)
library(raster)
library(rgdal)
library(maptools)
library(shapefiles)
library(igraph)

library(foreach)
library(doParallel)
# library(snow)

cl <- makeCluster(detectCores())
registerDoParallel(cl)
# library(doMC)
# registerDoMC() 

map.tmp <- raster(paste('Maps/Age_1_1990.tif',sep=''))

# create AK-CAN boreal mask
boreal.AKCAN.mask.tmp <- map.tmp
boreal.AKCAN.mask.tmp[!is.na(boreal.AKCAN.mask.tmp)] <- 1
boreal.AKCAN.mask.tmp[is.na(boreal.AKCAN.mask.tmp)] <- 1
boreal.AKCAN.mask <- mask(boreal.AKCAN.mask.tmp, boreal.shp)

# create AK-CAN boreal mask
tundra.AKCAN.mask <- mask(boreal.AKCAN.mask.tmp, tundra.shp)
rm(boreal.AKCAN.mask.tmp)

tundra.aab <- boreal.aab <- tundra.num.fire <- boreal.num.fire <- matrix(NA,length(years -1),length(reps))
tundra.size.fire <- boreal.size.fire <- c()

## ECHAM
# Read in maps # 1-25
endyear=2011
for (y in startyear:endyear){ #Year Loop
  #for (r in (startrep+1):(endrep+1)) { #Rep Loop
  foreach (r=(startrep+1):(endrep+1)) %dopar% { #Rep Loop
  
  print(paste('processing year ',y,' and rep ',(r-1), sep=''))
  map.tmp <- raster(paste('Maps/Age_',(r-1),'_',y,'.tif',sep=''))     
  tundra.map <- mask(map.tmp, tundra.AKCAN.mask)
  boreal.map <- mask(map.tmp, boreal.AKCAN.mask)
  
  tundra.aab[(y-(startyear-1)), r] <- length(tundra.map[tundra.map==0])
  boreal.aab[(y-(startyear-1)), r] <- length(boreal.map[boreal.map==0]) 


  
  tundra0 <- tundra.map == 0
  TundraPatch0 <- clump(tundra0, gaps=FALSE)
  TcellsPerPatch <- freq(TundraPatch0, useNA='no')
  Tnum <- dim(TcellsPerPatch)
  
  boreal0 <- boreal.map == 0
  BorealPatch0 <- clump(boreal0, gaps=FALSE)
  BcellsPerPatch <- freq(BorealPatch0, useNA='no')
  Bnum <- dim(BcellsPerPatch)
  
  tundra.num.fire[(y-(startyear-1)), r] <- Tnum[1]
  boreal.num.fire[(y-(startyear-1)), r] <- Bnum[1]

  tundra.size.fire <- append(tundra.size.fire, TcellsPerPatch[,2])
  boreal.size.fire <- append(boreal.size.fire, BcellsPerPatch[,2])
  
  }
}

# make sure that all of the matrices as the names above
# remove vars are not these matrices

save.image("Calibration_Data.RData")

## DONE# 
