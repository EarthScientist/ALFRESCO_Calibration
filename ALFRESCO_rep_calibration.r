# user inputs to script:
workingDir = "./"
mapsPath = "./Maps/"
mcfPath = "/home/UA/apbennett/ALFRESCO_Calibration/multicore_function_parallel.r"
nreps = 0:95
years = 1950:2011
maskFile = "/home/UA/apbennett/ALFRESCO_Calibration/Boreal_Tundra_CombinedDomains.shp"


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
	template <- raster(paste(mapsPath, "Age_",nreps[1],"_",years[1],".tif",sep="")) 
	# read in the mask shapefile
	shp <- shapefile(maskFile)
	#mask
	boreal.tundra <- rasterize(shp,template, field="Id")		
}else{
	boreal.tundra <- raster(maskFile)
}

# remove the unneeded RAM hoggers
rm(template,shp)

# source in matts parallel setup function
source(mcfPath) #"/workspace/UA/malindgren/projects/ALFRESCO/Calibration_Script/code_copy/multicore_function_parallel.r"

# how many cores to use?
cpu.count <- detectCores()

# func
f <- function(nreps){
	bor.tab <- numeric()
	bor.size.mean <- numeric()
	bor.num <- numeric()
    bor.size <- numeric()
	tun.tab <- numeric()
	tun.size.mean <- numeric()
	tun.num <- numeric()
    tun.size <- numeric()
	
	for(nrep in nreps){
		for(year in years){
			r <- raster(paste(dataPath,"Age_",nrep,"_",year+1,".tif",sep=""))
			r[which(values(r) != 0)] <- NA
			r[which(values(r) == 0)] <- 1
			#boreal
			r.clump <- clump(r,gaps=FALSE) #[which(values(boreal.tundra)==1),drop=F]
			boreal.patch <- freq(r.clump[values(boreal.tundra == 1), drop=F], useNA='no')
			bor.tab <- append(bor.tab, sum(boreal.patch[,2]))
			bor.size.mean <- append(bor.size, mean(boreal.patch[,2]))
			bor.size <- append(bor.size, boreal.patch[,2])
			bor.num <- append(bor.num, nrow(boreal.patch))
			#tundra
			tundra.patch <- freq(r.clump[values(boreal.tundra == 2), drop=F], useNA='no')
			tun.tab <- append(tun.tab, sum(tundra.patch[,2]))
			tun.size.mean <- append(tun.size, mean(tundra.patch[,2]))
			tun.size <- append(tun.size, tundra.patch[,2])
			tun.num <- append(tun.num, nrow(tundra.patch))
		}
	}
	out <- list(bor.tab, bor.size.mean, bor.num, tun.tab, tun.size.mean, tun.num, bor.size, tun.size)
}

# run matt's multicore function with the function laid out above
t3 <- mcf(f, iters=nreps, clusters=cpu.count)

# names of the needed output files
table_names = c("boreal.aab","boreal.size.fire.mean","boreal.num.fire","tundra.aab","tundra.size.fire.mean","tundra.num.fire","boreal.size.fire","tundra.size.fire")
# table_names = c(boreal.aab,boreal.size.fire,boreal.num.fire,tundra.aab,tundra.size.fire,tundra.num.fire)
#create the needed output tables from the nested list object created above
#for(i in 1:(length(table_names)){
for(i in 1:6){
	assign(as.character(table_names[i]), matrix(unlist(lapply(t3,'[[',i)), nrow=length(years)))
	if(any(is.na(table_names[i])) == TRUE){
		print("TRUE!")
		table_names[i][which(is.na(table_names[i]) == TRUE)] <- 0
	}
}

for(i in 7:8){
	assign(as.character(table_names[i]), unlist(lapply(t3,'[[',i))) 
	if(any(is.na(table_names[i])) == TRUE){
		print("TRUE!")
		table_names[i][which(is.na(table_names[i]) == TRUE)] <- 0
	}
}


#lets remove some of the objects in memory
reps=nreps
rm(table_names, i, nreps, years)

#save the workspace image
save.image(paste(workingDir,"Calibration_Data.RData",sep=""))
