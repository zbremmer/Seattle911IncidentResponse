# Examination of Seattle's 911 Incident Response data
#
# TODO: 
# Define functions outside of main process?
# Replace map list for loop with laply()?
# Plotly for mapping? library(gridExtra, plotly, ggplot2)

############################
# Load required libraries
############################
library(ggplot2)
library(rgdal)
library(ggmap)
library(GISTools)

############################
# Load 911 Response data
############################
IncidentResponseData <- read.csv("C:/Users/Zach.Bremmer/Desktop/Seattle_Police_Department_911_Incident_Response.csv", header = TRUE)

############################
# Explore the Data
############################
nrow(IncidentResponseData) #find n [1,185,731]
colnames(IncidentResponseData) #view col names
head(IncidentResponseData) #check head
tail(IncidentResponseData) #check tail

head(IncidentResponseData$Event.Clearance.Group) #46 different clearance group levels
table(IncidentResponseData$Event.Clearance.Group) #Summary...have a lot of blank values and null (NULL is entered in data as text, not actually a null value)
head(IncidentResponseData$Initial.Type.Group) #39 different Initial Type Group levels
table(IncidentResponseData$Initial.Type.Group) #Summary...have a lot of blank values

############################
# Munge munge munge...
############################

###
# Need to look at this data more closely. What needs to be a factor? What needs to be a continuous value? Char?
# Format each col into the type it should be in.
# Deal with nulls/NAs in each column. 
###







#Several records have no value recorded for Event.Clearance.Group or Initial.Type.Group. Let's add "Unknown" to this.
IncidentResponseData$Event.Clearance.Group <- as.character(IncidentResponseData$Event.Clearance.Group)
IncidentResponseData$Event.Clearance.Group[IncidentResponseData$Event.Clearance.Group==""] <- "UNKNOWN" # change empty values to UNKNOWN
IncidentResponseData$Event.Clearance.Group[IncidentResponseData$Event.Clearance.Group=="NULL"] <- "UNKNOWN" # change "NULL" to UNKNOWN
IncidentResponseData$Event.Clearance.Group <- as.factor(IncidentResponseData$Event.Clearance.Group)

IncidentResponseData$Initial.Type.Group <- as.character(IncidentResponseData$Initial.Type.Group)
IncidentResponseData$Initial.Type.Group[IncidentResponseData$Initial.Type.Group==""] <- "UNKNOWN"
IncidentResponseData$Initial.Type.Group <- as.factor(IncidentResponseData$Initial.Type.Group)

#convert incident date to POSITXlt format
  ## Need to deal with null/NA values. 
IncidentResponseData$Event.Clearance.Date <- strptime(IncidentResponseData$Event.Clearance.Date, format="%m/%d/%Y %H:%M:%S") 

#Set up some variables
EventClearanceGroup <- data.frame(c(levels(IncidentResponseData$Event.Clearance.Group)))
InitialTypeGroup <- data.frame(c(levels(IncidentResponseData$Initial.Type.Group)))
IncidentResponseDataPoints2016 <- with(IncidentResponseData, IncidentResponseData[(Event.Clearance.Date >= "2016-01-01") & !is.na(Event.Clearance.Date),])

#Set up version of data as SpatialPointsDataFrame object. Deal with NA values in lat/lon 
#xy <- IncidentResponseData[!is.na(IncidentResponseData$Latitude) & !is.na(IncidentResponseData$Longitude),c(13,14)] #get lat/lon values from dataframe
xy <- IncidentResponseData[,c(13,14)]
xy[is.na(xy$Longitude) | is.na(xy$Latitude),] <- 0
IncidentResponseSpatialPoints <- SpatialPointsDataFrame(coords = xy, data = IncidentResponseData, 
                                                        proj4string = CRS("+proj=longlat +datum=WGS84"))

############################
# Exploratory Mapping
############################

# Set up bg map
Seattle <- get_map(location = c(lon = -122.338072,lat =47.625317),color = "color",
                   source="google",maptype="satellite",zoom=11)
SeattleMap <- ggmap(Seattle, extent = "panel")

# Read and prepare data for census tract map 
censusTractShp <- readOGR(dsn="C:/Users/Zach.Bremmer/Desktop/Seattle\ Crime\ Dash/KingCountyCensusTract/kc_tract_10.shp", layer="kc_tract_10")
censusTractShp <- spTransform(censusTractShp,CRS("+proj=longlat +datum=WGS84")) #transform to correct projection
censusTractShp <- fortify(censusTractShp) #transform to dataframe

# Add census tract polygons to background map
SeattleCensusMap <- SeattleMap + geom_polygon(aes(x=long, y=lat, group=group), size=.2,color='blue', data=censusTractShp, alpha=0)

# Plot 2016 incident points on bg map (include test for NA values)
SeattleIncidentMap <- SeattleCensusMap + geom_point(aes(x=Longitude, y=Latitude), data = IncidentResponseDataPoints2016, color="red", size=0.25)+labs(x="",y="",title="2016 Incident Responses")
  #NOTE: Error "Removed k rows containing missing values" likely means they are located outside of the plotted area. Try changing zoom levels on bg map.

############################
# Define multiplot (source: Winston Chang)
############################

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

############################

# Plot points by selected incident type from original data
mapListAll <- list()
j=1
for (i in 1:nrow(EventClearanceGroup)){
  if (nrow(IncidentResponseData[IncidentResponseData[,"Event.Clearance.Group"]==EventClearanceGroup[i,],])>0){
    mapListAll[[j]]<-SeattleCensusMap + geom_point(aes(x=Longitude, y=Latitude), 
                                                    data = IncidentResponseData[IncidentResponseData[,"Event.Clearance.Group"]==EventClearanceGroup[i,],], 
                                                    color="red", size=0.25)+labs(x="",y="",title=EventClearanceGroup[i,])+theme(title=element_text(size=6,face="bold"))
    j<-j+1
  }
}

# There are 45 maps in mapList. I'll do 3x3 plots. 
# Output to PDF so maps don't need to be constantly recreated
pdf(file="AllIncidentsByClearanceGroup.pdf")
multiplot(plotlist=mapListAll[c(1:9)], cols=3)
multiplot(plotlist=mapListAll[c(10:18)], cols=3)
multiplot(plotlist=mapListAll[c(19:27)], cols=3)
multiplot(plotlist=mapListAll[c(28:36)], cols=3)
multiplot(plotlist=mapListAll[c(37:45)], cols=3)
dev.off()

# Plot points by selected incident type from 2016 data
mapList2016 <- list()
j=1
for (i in 1:nrow(EventClearanceGroup)) {
  if (nrow(IncidentResponseDataPoints2016[IncidentResponseDataPoints2016[,"Event.Clearance.Group"]==EventClearanceGroup[i,],])>0){
    mapList2016[[j]]<-SeattleCensusMap + geom_point(aes(x=Longitude, y=Latitude), 
                                                data = IncidentResponseDataPoints2016[IncidentResponseDataPoints2016[,"Event.Clearance.Group"]==EventClearanceGroup[i,],], 
                                                color="red", size=0.25)+labs(x="",y="",title=paste0("2016 ", EventClearanceGroup[i,]))+theme(title=element_text(size=6,face="bold"))
    j<-j+1
  }
}  

# There are 39 maps in mapList. I'll do 3x3 plots. 
# Output to PDF so maps don't need to be constantly recreated
pdf(file="2016IncidentsByClearanceGroup.pdf")
multiplot(plotlist=mapList2016[c(1:9)], cols=3)
multiplot(plotlist=mapList2016[c(10:18)], cols=3)
multiplot(plotlist=mapList2016[c(19:27)], cols=3)
multiplot(plotlist=mapList2016[c(28:36)], cols=3)
multiplot(plotlist=mapList2016[c(37:39)], cols=3)
dev.off()

############################
# Kernel Density Mapping
############################
 


####
# Prediction / ML
# Ideas for engineered features:
# -Day of Week    -Weekday/Weekend  -Before/After Dark
# -Type (violent, non-violent, etc.)  
# -Holiday  -Census Block   -Zip code -Neighborhood
# -Time of day  -Pop density (avail by block?) -Income by census ZIP
# -is park?     -Lat/lon from Incident.Location is more accurate than Lat/Lon but needs splitting  
####

# See what kind of maps seattle has - if has park map for ex. - can can do intersection to find if park, etc.

############################
# Build some features
############################

# Not sure if this is the best way to do this!

# List weekday for Event.Clearance.Date
IncidentResponseData$Day <- factor(weekdays(IncidentResponseData$Event.Clearance.Date))
# Is day a weekend?
IncidentResponseData$Weekend <- IncidentResponseData$Day == "Saturday" | IncidentResponseData$Day == "Sunday"


# Output updated csv
write.csv(IncidentResponseData, file="Seattle911IncidentResponseDataFeatures.csv")



