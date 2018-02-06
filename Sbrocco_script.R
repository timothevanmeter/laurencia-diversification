#### OCEAN CLIMATE LAYERS FOR MARINE SPATIAL ECOLOGY -- TOOLS FOR NICHE MODELING  ####

#### esrigrid2ascii function - written by Elizabeth J. Sbrocco, National Evolutionary	####
#### Synthesis Center, Durham, NC  27705 elizabeth.sbrocco@nescent.org - August 2012	####

#### THE FOLLOWING IS A CUSTOM FUNCTION THAT TAKES ADVANTAGE OF THE RASTER PACKAGE TO	####
#### CROP MARSPEC ESRI GRIDS TO YOUR AREA OF INTEREST AND CONVERT IT TO AN ASCII FILE	####
#### FORMAT COMPATIBLE WITH THE MAXENT ECOLOGICAL NICHE MODELING PROGRAM.  AT THIS TIME	####
#### I HAVE ONLY WRITTEN CODE TO HANDLE ONE RASTER AT A TIME, BUT I AM WORKING ON CODE	####
#### TO DO BATCH CONVERSIONS.  FURTHERMORE, IF YOUR AREA OF INTEREST CROSSES THE	####
#### INTERNATIONAL DATE LINE IN THE PACIFIC, THIS CODE WILL NOT WORK - I AM CURRENTLY 	####
#### WORKING ON A TOOL FOR THIS AS WELL. NOTE THIS FUNCTION DEPENDS ON THE RASTER	####
#### PACKAGE, WHICH IN TURN DEPENDS ON THE SP PACKAGE, SO BE SURE THAT THESE ARE 	####
#### DOWNLOADED BEFORE BEGINNING.							####

#### INSTRUCTIONS:
#### AFTER DOWNLOADING THE MARSPEC DATA, UNZIP THE FILES USING 7-ZIP (OR YOUR FAVORITE	####
#### ZIP ARCHIVE UTILITY)AND MOVE THE FILES TO YOUR WORKING DIRECTORY. OPEN R AND USE 	####	
#### "FILE->CHANGE DIR" TO SELECT THE DIRECTORY CONTAINING YOUR MARSPEC GRID FILES.	####
#### EXECUTE THE FOLLOWING CODE BY COPYING AND PASTING INTO THE R CONSOLE WINDOW, OR 	####
#### HIGHLIGHT AND PRESS CTRL-R								####

library(raster)
esrigrid2ascii <- function(inputgrid,outputascii,xmin,xmax,ymin,ymax)
{	x <- raster(inputgrid)
  aoi <- extent(xmin,xmax,ymin,ymax)
  x.crop <- crop(x,aoi)
  writeRaster(x.crop,outputascii,NAflag=-9999)
  "DONE!"
}

#### NOW YOU CAN RUN THE FUNCTION BY TYPING 	esrigrid2ascii(inputgrid,outputascii,xmin,xmax,ymin,ymax)			####
#### WHERE 															####
####	inputgrid = the name of the ESRI grid to be converted									####
####	outputascii = the name of the output ASCII grid to be saved								####
####	xmin, xmax, ymin, ymax = the longitudinal and latitudinal GPS coordinates of your area of interest in decimal degrees	####

#### Example (Assumes you have already downloaded data to your working directory and that you have created a folder called	#### 
#### "output" in that directory) 												####

esrigrid2ascii("Core_Variables_10m/bathy_10m","output/bathy_10m.asc",90,140,-30,30)
