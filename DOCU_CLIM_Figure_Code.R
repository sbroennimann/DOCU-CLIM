library(stringr)

### Code to generate plots in the paper and to read in the files 

### read file list
lst <- list.files(pattern = "DOCU_CLIM_0")

### read inventory
inp <- read.table("DOCU_CLIM_Inventory.txt",header=T)

#####
# CODE to produce Fig. 6 (type/length/season) from inventory
#####

# top left / types
prec.ptrn <- c("Precip", "precip")
tmp.ptrn <- c("Documentary_", "Temp", "temp")
dry.ptrn <- c("wetness", "SPEI", "drought")
ice.ptrn <- "Ice_phenology"
plt.ptrn <- "Plant_phenology"
win.ptrn <- "Wind"

coor <- inp[str_detect(inp[,3],prec.ptrn[1])|str_detect(inp[,3],prec.ptrn[2]),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
plot(coor[sel,1],coor[sel,2],col="blue",xlim=c(-180,180),ylim=c(-90,90))
coor <- inp[str_detect(inp[,3],tmp.ptrn[1])|str_detect(inp[,3],tmp.ptrn[2])|str_detect(inp[,3],tmp.ptrn[3]),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="red")
coor <- inp[str_detect(inp[,3],dry.ptrn[1])|str_detect(inp[,3],dry.ptrn[2])|str_detect(inp[,3],dry.ptrn[3]),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="brown")
coor <- inp[str_detect(inp[,3],ice.ptrn),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="lightblue")
coor <- inp[str_detect(inp[,3],plt.ptrn),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="lightgreen")
coor <- inp[str_detect(inp[,3],win.ptrn),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="darkgreen")

# top right / start yr
coor <- inp[inp[,11]>=1400&inp[,11]<1500,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
plot(coor[sel,1],coor[sel,2],col="grey",xlim=c(-180,180),ylim=c(-90,90))
coor <- inp[inp[,11]>=1500&inp[,11]<1600,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="blue")
coor <- inp[inp[,11]>=1600&inp[,11]<1700,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="green")
coor <- inp[inp[,11]>=1700&inp[,11]<1800,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="orange")
coor <- inp[inp[,11]>=1800&inp[,11]<1900,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="red")

# bottom left / season
ann.ptrn <- "Annual"
win.ptrn <- c("Winter","winter","earlywinter")
spr.ptrn <- c("Spring","spring","latespring")
sum.ptrn <- c("Summer","summer")
fal.ptrn <- c("Fall","fall")                           

coor <- inp[str_detect(inp[,8],ann.ptrn),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
plot(coor[sel,1],coor[sel,2],col="grey",xlim=c(-180,180),ylim=c(-90,90))
coor <- inp[str_detect(inp[,8],win.ptrn[1])|str_detect(inp[,8],win.ptrn[2])|str_detect(inp[,8],win.ptrn[3]),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="blue")
coor <- inp[str_detect(inp[,8],spr.ptrn[1])|str_detect(inp[,8],spr.ptrn[2])|str_detect(inp[,8],spr.ptrn[3]),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="green")
coor <- inp[str_detect(inp[,8],sum.ptrn[1])|str_detect(inp[,8],sum.ptrn[2]),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="red")
coor <- inp[str_detect(inp[,8],fal.ptrn[1])|str_detect(inp[,8],fal.ptrn[2]),6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="brown")

# bottom right / length
coor <- inp[inp[,13]<50,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
plot(coor[sel,1],coor[sel,2],col="green",xlim=c(-180,180),ylim=c(-90,90))
coor <- inp[inp[,13]>=50&inp[,13]<100,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="yellow")
coor <- inp[inp[,13]>=100&inp[,13]<150,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="orange")
coor <- inp[inp[,13]>=150&inp[,13]<200,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="red")
coor <- inp[inp[,13]>=200&inp[,13]<300,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="darkred")
coor <- inp[inp[,13]>=300&inp[,13]<400,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="purple")
coor <- inp[inp[,13]>=400&inp[,13]<500,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="brown")
coor <- inp[inp[,13]>=500,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="black")

#####
# CODE to produce Fig. 7 time series and calculate total number of values
#####
phen <- c("Start_Date","Grape_Harvest_Date","Flowering_Date","Leafing_Date","Wheat_harvest_date","flowering_date","leaf_colouring_date")
ice <- c("ice_cover","break_up_date","freeze_up_date","SeaIceExtent","first_snow_fall","days_of_freeze","ice_index",
         "first_ship_arrival","first_ship_departure","ship_insurance","start_sailing_season","IceSeverity")
tem <- c("Temperature_Index","Temperature_reconstruction","Temperature_index")
wind <- c("Wind_Onset","Wind_End","MonsoonIndex","WesterlyIndex") 
dro <- c("Drought_Index","SPEI","Dryness")            
pre <- c("Precipitation_Index","Precipitation_Reconstruction")            

n.cat <- array(0,dim=c(622,7))
n.cat[,1] <- 1400:2021
n <- 0
### loop over files
for (i in 1:length(lst)){
  x <- read.table(lst[i],header=T)
  catx <- c(sum(str_detect(phen,x[1,14])),sum(str_detect(ice,x[1,14])),sum(str_detect(tem,x[1,14])),sum(str_detect(wind,x[1,14])),
            sum(str_detect(dro, x[1,14])),sum(str_detect(pre, x[1,14])))
  n.cat[match(x[,9],n.cat[,1]),which(catx==1)+1] <- n.cat[match(x[,9],n.cat[,1]),which(catx==1)+1]+1
  n <- n+sum(!is.na(x[,17]))} 
n
plot(n.cat[,1],n.cat[,2],type="l",col="green",ylim=c(0,200))
lines(n.cat[,1],n.cat[,3],col="lightblue")
lines(n.cat[,1],n.cat[,4],col="red")
lines(n.cat[,1],n.cat[,5],col="black")
lines(n.cat[,1],n.cat[,6],col="orange")
lines(n.cat[,1],n.cat[,7],col="blue")


#####
# CODE to produce Fig. 9 (correlation) ### add more categories if desired
#####
coor <- inp[inp[,18]<0.3,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
plot(coor[sel,1],coor[sel,2],col="bisque1",xlim=c(-180,180),ylim=c(-90,90))
coor <- inp[inp[,18]>=0.3&inp[,13]<0.4,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="yellow")
coor <- inp[inp[,18]>=0.4&inp[,18]<0.5,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="orange")
coor <- inp[inp[,18]>=0.5&inp[,18]<0.6,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="red")
coor <- inp[inp[,18]>=0.6&inp[,18]<0.7,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="darkred")
coor <- inp[inp[,18]>=0.7&inp[,18]<0.8,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="purple")
coor <- inp[inp[,18]>=0.8&inp[,18]<0.9,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="brown")
coor <- inp[inp[,18]>0.9,6:7]
sel <- match(unique(coor[,1]*coor[,2]),coor[,1]*coor[,2])
points(coor[sel,1],coor[sel,2],col="black")

