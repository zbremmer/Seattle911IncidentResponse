# Examination of Seattle's 911 Incident Response data
#
# TODO: Add timestamp column to IncidentResponseData BEFORE formatting date
#
#

############################
# Load required libraries
############################
library(ggplot2)
library(rgdal)
library(ggmap)

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
# Munge
############################

#Several records have no value recorded for Event.Clearance.Group or Initial.Type.Group. Let's add "Unknown" to this.
IncidentResponseData$Event.Clearance.Group <- as.character(IncidentResponseData$Event.Clearance.Group)
IncidentResponseData$Event.Clearance.Group[IncidentResponseData$Event.Clearance.Group==""] <- "UNKNOWN" # change empty values to UNKNOWN
IncidentResponseData$Event.Clearance.Group[IncidentResponseData$Event.Clearance.Group=="NULL"] <- "UNKNOWN" # change "NULL" to UNKNOWN
IncidentResponseData$Event.Clearance.Group <- as.factor(IncidentResponseData$Event.Clearance.Group)

IncidentResponseData$Initial.Type.Group <- as.character(IncidentResponseData$Initial.Type.Group)
IncidentResponseData$Initial.Type.Group[IncidentResponseData$Initial.Type.Group==""] <- "UNKNOWN"
IncidentResponseData$Initial.Type.Group <- as.factor(IncidentResponseData$Initial.Type.Group)

#convert incident date to date format and add timestamp column
IncidentResponseData$Event.Clearance.Date <- as.Date(IncidentResponseData$Event.Clearance.Date, format="%m/%d/%Y") 

#Set up some variables
EventClearanceGroup <- data.frame(c(levels(IncidentResponseData$Event.Clearance.Group)))
InitialTypeGroup <- data.frame(c(levels(IncidentResponseData$Initial.Type.Group)))
IncidentResponseDataPoints2016 <- with(IncidentResponseData, IncidentResponseData[(Event.Clearance.Date >= "2016-01-01") & !is.na(Event.Clearance.Date),])
IncidentResponseDataPoints2015 <- with(IncidentResponseData, IncidentResponseData[(Event.Clearance.Date >= "2015-01-01") & (Event.Clearance.Date < "2016-01-01") &!is.na(Event.Clearance.Date),])
  #Can break out data by year if needed. May not have to. 

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
multiplot(plotlist=mapListAll[c(1:9)], cols=3)
multiplot(plotlist=mapListAll[c(10:18)], cols=3)
multiplot(plotlist=mapListAll[c(19:27)], cols=3)
multiplot(plotlist=mapListAll[c(28:36)], cols=3)
multiplot(plotlist=mapListAll[c(37:45)], cols=3)

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

multiplot(plotlist=mapList2016[c(1:9)], cols=3)
multiplot(plotlist=mapList2016[c(10:18)], cols=3)
multiplot(plotlist=mapList2016[c(19:27)], cols=3)
multiplot(plotlist=mapList2016[c(28:36)], cols=3)
multiplot(plotlist=mapList2016[c(37:39)], cols=3)







####
# Prediction / ML
# Ideas for engineered features:
# -Day of Week    -Weekday/Weekend  -Before/After Dark
# -Type (violent, non-violent, etc.)  
# -Holiday  -
####


