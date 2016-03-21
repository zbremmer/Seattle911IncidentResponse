# Examination of Seattle's 911 Incident Response data

# Load required libraries
library(ggplot2)
library(rgdal)

# Load data
IncidentResponseData <- read.csv("C:/Users/Zach.Bremmer/Desktop/Seattle_Police_Department_911_Incident_Response.csv", header = TRUE)

# Exploratory Data Analysis
nrow(IncidentResponseData) #find n 
colnames(IncidentResponseData) #view col names
head(IncidentResponseData$Event.Clearance.Group) #46 different clearance group levels
table(IncidentResponseData$Event.Clearance.Group) #Summary...have a lot of blank values
head(IncidentResponseData$Initial.Type.Group) #39 different Initial Type Group levels
table(IncidentResponseData$Initial.Type.Group) #Summary...have a lot of blank values

#Several records have no value recorded for Event.Clearance.Group or Initial.Type.Group. Let's add "Unknown" to this.
IncidentResponseData$Event.Clearance.Group <- as.character(IncidentResponseData$Event.Clearance.Group)
IncidentResponseData$Event.Clearance.Group[IncidentResponseData$Event.Clearance.Group==""] <- "UNKNOWN"
IncidentResponseData$Event.Clearance.Group <- as.factor(IncidentResponseData$Event.Clearance.Group)
table(IncidentResponseData$Event.Clearance.Group)

IncidentResponseData$Initial.Type.Group <- as.character(IncidentResponseData$Initial.Type.Group)
IncidentResponseData$Initial.Type.Group[IncidentResponseData$Initial.Type.Group==""] <- "UNKNOWN"
IncidentResponseData$Initial.Type.Group <- as.factor(IncidentResponseData$Initial.Type.Group)
table(IncidentResponseData$Initial.Type.Group)

#convert incident date to date format
IncidentResponseData$Event.Clearance.Date <- as.Date(IncidentResponseData$Event.Clearance.Date, format="%m/%d/%Y") 


# Set up bg map
Seattle <- get_map(location = c(lon = -122.338072,lat =47.625317),color = "color",
                   source="google",maptype="satellite",zoom=11)
SeattleMap <- ggmap(Seattle, extent = "panel")

# Read and prepare data for census tract map 
censusTract <- readOGR(dsn="C:/Users/Zach.Bremmer/Desktop/Seattle\ Crime\ Dash/KingCountyCensusTract/kc_tract_10.shp", layer="kc_tract_10")
censusTract <- spTransform(censusTract,CRS("+proj=longlat +datum=WGS84")) #transform to correct projection
censusTract <- fortify(censusTract) #transform to dataframe

# Add census tract polygons to background map
SeattleCensusMap <- SeattleMap + geom_polygon(aes(x=long, y=lat, group=group), size=.2,color='blue', data=censusTract, alpha=0)

# Plot 2016 incident points on bg map (include test for NA values)
IncidentResponseDataPoints <- with(IncidentResponseData, IncidentResponseData[(Event.Clearance.Date >= "2016-01-01") & !is.na(Event.Clearance.Date),])
SeattleIncidentMap <- SeattleCensusMap + geom_point(aes(x=Longitude, y=Latitude), data = IncidentResponseDataPoints, color="red", size=0.25)
  #NOTE: Error "Removed k rows containing missing values" likely means they are located outside of the plotted area. Try changing zoom levels on bg map.

# Plot points by incident type