# user inputs to script:
workingDir = "/workspace/UA/malindgren/projects/ALFRESCO/Calibration_Script/"
mapsPath = "/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/CalibrationResults_Maps1950to2009/Maps/"
mcfPath = "/workspace/UA/malindgren/projects/ALFRESCO/Calibration_Script/code_copy/multicore_function_parallel.r"
nreps = 0:1
years = 1950:1951
maskFile = "/workspace/UA/malindgren/projects/ALFRESCO/Calibration_Script/masks/Boreal_Tundra_CombinedDomains.shp"

#  SHOULD ONLY HAVE TO TOUCH THE VARIABLES ABOVE TO GET IT RUNNING ###

# read in some libraries
require(raster)
require(parallel)

# set a working directory
setwd(workingDir)

#some setup
nreps=nreps
years=years
dataPath=mapsPath

if(extension(maskFile) == ".shp"){
	# create the needed mask file using a template file from the data to be processed
	template <- raster(paste(mapsPath, "Age_",nreps[1],"_",years[1],".tif",sep="")) # "/workspace/Shared/ALFRESCO_Jacob_Bird/NEW_CaribouSubregionResults/CalibrationResults_Maps1950to2009/Maps/Age_0_1950.tif"
	# read in the mask shapefile
	shp <- shapefile("/workspace/UA/malindgren/projects/ALFRESCO/Calibration_Script/masks/Boreal_Tundra_CombinedDomains.shp")
	#mask
	boreal.tundra <- rasterize(shp,template, field="Id")		
}else{
	boreal.tundra <- raster(maskFile)
}

# remove the unneeded RAM hoggers
rm(template,shp)

# source in matts parallel setup function
source(mcfPath)#"/workspace/UA/malindgren/projects/ALFRESCO/Calibration_Script/code_copy/multicore_function_parallel.r"

# how many cores to use?
cpu.count <- detectCores()

# func
f <- function(nreps){
	bor.tab <- numeric()
	bor.size <- numeric()
	bor.num <- numeric()
	tun.tab <- numeric()
	tun.size <- numeric()
	tun.num <- numeric()
	for(nrep in nreps){
		for(year in years){
			r <- raster(paste(dataPath,"Age_",nrep,"_",year,".tif",sep=""))
			r[which(values(r) != 0)] <- NA
			r[which(values(r) == 0)] <- 1
			#boreal
			r.clump <- clump(r,gaps=FALSE) #[which(values(boreal.tundra)==1),drop=F]
			boreal.patch <- freq(r.clump[values(boreal.tundra == 1), drop=F], useNA='no')
			bor.tab <- append(bor.tab, sum(boreal.patch[,2]))
			bor.size <- append(bor.size, mean(boreal.patch[,2]))
			bor.num <- append(bor.num, nrow(boreal.patch))
			#tundra
			tundra.patch <- freq(r.clump[values(boreal.tundra == 2), drop=F], useNA='no')
			tun.tab <- append(tun.tab, sum(tundra.patch[,2]))
			tun.size <- append(tun.size, mean(tundra.patch[,2]))
			tun.num <- append(tun.num, nrow(tundra.patch))
		}
	}
	out <- list(bor.tab, bor.size, bor.num, tun.tab, tun.size, tun.num)
}

# run matt's multicore function with the function laid out above
t3 <- mcf(f, iters=nreps, clusters=cpu.count)

# names of the needed output files
table_names = c("boreal.aab","boreal.size.fire","boreal.num.fire","tundra.aab","tundra.size.fire","tundra.num.fire")

#create the needed output tables from the nested list object created above
for(i in 1:6){
	assign(table_names[i], matrix(unlist(lapply(t3,'[[',i)), nrow=length(years)))
}

#lets remove some of the objects in memory
rm(table_names, i, nreps, years)

#save the workspace image
save.image(paste(workingDir,"Calibration_Data.RData",sep=""))
