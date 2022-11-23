;-- copy shapefile_utils.ncl from 
;-- https://www.ncl.ucar.edu/Applications/Scripts/shapefile_utils.ncl
load "shapefile_utils.ncl" 

;-- Arguments passed to the script via the command line
;-- ncl tempPath = $tempDir state = "Virginia" cntyfipsname = $nameMask tmaxGrid = $outTMaxArea maskGridMet.ncl

;-- Set shapefile and mask names
;-- shpname  = ".//gpfs/group/kzk10/default/private/ren10/mlisk_collab/counties/shapefiles/Virginia/Accomack_Virginia.51001.shp" 
;-- maskname = "./Accomack_Virginia.51001_mask.nc" 
shpname  = shapePath + "/" + state + "/" + "cnty." + cntyfipsname + ".shp" 
maskname = tempPath + "/" + state + "/" + model + cntyfipsname + "_mask.nc" 

print_shapefile_info(shpname)

;-- open data file to get the grid to be used
;-- f = addfile("/gpfs/group/kzk10/default/private/mdl5548/temp/GridMetintermeds/tmax.day.sub_VA.19790101-20200901.nc","r")
f = addfile(tmaxGrid,"r")

;-- read variable
var                    =  f->air_temperature(0,:,:)
var@lat1d              =  f->lat
var@lon1d              =  f->lon

;-- shapefile mask resources
opt             =  True
opt@return_mask =  True    ;-- this forces the return of a 0s and 1s mask array

;-- create the mask based on the given shapefile
mask_array             =  shapefile_mask_data(var, shpname, opt)
mask_array!0           = "lat" 
mask_array!1           = "lon" 
mask_array@coordinates = "lat lon" 

;-- create new netCDF file and write mask array
system("rm -f " + maskname)
fout = addfile(maskname,"c")

fout->mask_array =  mask_array
fout->lat       =  f->lat
fout->lon      =  f->lon