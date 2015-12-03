library(XML)
library(RCurl)
latlon2ft <- function(origin,destination){
  xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
  xmlfile <- xmlParse(getURL(xml.url))
  dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
  distance <- as.numeric(sub(" km","",dist))
  ft <- distance/1000 # FROM METER TO KM
  print("Call")
  return(ft)
}

earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}
####latlon2ft(origin=paste(b[2],b[1],sep = ","),destination=paste(b[46],b[45],sep = ","))

######Reverse geocoding

reverseGeoCode <- function(latlng) {
  latlngStr <-  gsub(' ','%20', paste(latlng, collapse=","))#Collapse and Encode URL Parameters
  library("RJSONIO") #Load Library
  #Open Connection
  connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&latlng=',latlngStr, sep="")
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  #Flatten the received JSON
  data.json <- unlist(data.json)
  if(data.json["status"]=="OK")
    address <- data.json["results.formatted_address"]
  return (address)
}
##address <- reverseGeoCode(c(37.4418834, -122.1430195))

head(as.POSIXct(as.numeric(data2$TIMESTAMP),origin="1970-01-01",tz="GMT"))

data2[,"Time"] <- as.POSIXct(as.POSIXct(as.numeric(data2$TIMESTAMP),origin="1970-01-01",tz="GMT"), format="%H:%M:%S",origin="1970-01-01",tz="GMT")

data2[,"HOUR"] <- unlist(as.POSIXlt(data2[,"Time"]))["hour"]

data2$HOUR <- lapply(data2$Time,function(x) unlist(as.POSIXlt(x))["hour"])
data2$HOUR1 <- lapply(data2$TIMESTAMP,function(x) unlist(as.POSIXlt(x,origin="1970-01-01",tz="GMT"))["hour"])

## counting the number of Call types in each category
table(data1$CALL_TYPE)
##attaching a Row Id Coloumn for subsetting index
data1$rowID <- c(1:1710670)
#Take Sample from each call type(1:4 Ratio)
b <- sample(data1$rowID[data1$CALL_TYPE=='A'],91192)
c <- sample(data1$rowID[data1$CALL_TYPE=='B'],204470)
d <- sample(data1$rowID[data1$CALL_TYPE=='C'],132005)

#Subsetting with selected RowID
data3 <- subset(data1,rowID %in% b)
data4 <- subset(data1,rowID %in% c)
data5 <- subset(data1,rowID %in% d)

#merging 3 subsets
dataSmall <- rbind.data.frame(data3,data4)
dataSmall <- rbind.data.frame(dataSmall,data5)

#Resuffling the sampled data
dataSmall <- dataSmall[sample(nrow(dataSmall)),]

##Load the location Data
locData <- read.csv(file = "metaData_taxistandsID_name_GPSlocation.csv")

##Removin missing data 
dataSmall <- dataSmall[dataSmall$MISSING_DATA!='True',]
dataSmall$POLYLINE <- as.character(dataSmall$POLYLINE)






## Indentifying nearest landmarks/Stands of Starting and Ending 
dataSmall$Start <-""
dataSmall$End <- ""
dataSmall$DIST <- 0
dataSmall$StartDev <- 0
dataSmall$EndDev <- 0

library(rjson)
library(XML)
library(RCurl)
for(j in 34289:149999){
  unlistedLoc <- unlist(fromJSON(dataSmall[j,"POLYLINE"]))
  print(j)
  #print(unlistedLoc)
  if(!is.null(unlistedLoc))
  {
    #print(dataSmall[j,"ID"])
    strtLat <- unlistedLoc[2]
    strtLon <- unlistedLoc[1]
    endLat  <- unlistedLoc[length(unlistedLoc)]
    endLon  <- unlistedLoc[length(unlistedLoc)-1]
    
    minStart <- .Machine$integer.max
    minEnd <- .Machine$integer.max
    minStrtIndx <- 0
    minEndIndx <- 0
    for(i in 1:63)
    {
      
      distStart <- as.numeric(earth.dist(as.numeric(strtLat),as.numeric(strtLon),locData[i,"Latitude"],locData[i,"Longitude"]))
      distEnd <- as.numeric(earth.dist(as.numeric(endLat),as.numeric(endLon),locData[i,"Latitude"],locData[i,"Longitude"]))
      if(distStart<minStart){
        minStart <- distStart
        minStrtIndx <- i
      }
      if(distEnd <- minEnd){
        minEndIndx <- i
      }
    }
    #print(minStrtIndx)
    #print(minEndIndx)
    #print(dataSmall["ID"])
    #print(dataSmall["Start"])
    library(Imap)
    dataSmall[j,"Start"] <- as.character(locData[minStrtIndx,"Descricao"])
    dataSmall[j,"End"] <- as.character(locData[minEndIndx,"Descricao"])
    dataSmall[j,"StartDev"] <- gdist(lat.1=as.numeric(strtLat), lon.1=as.numeric(strtLon), lat.2=locData[minStrtIndx,"Latitude"], lon.2=locData[minStrtIndx,"Longitude"], units="km")
    
    
    dataSmall[j,"EndDev"] <- gdist(lat.1=as.numeric(endLat), lon.1=as.numeric(endLon), lat.2=locData[minEndIndx,"Latitude"], lon.2=locData[minEndIndx,"Longitude"], units="km")
    
    
    
  }
}
View(data2)

ReplaceLowerOrUpperTriangle <- function(m, triangle.to.replace){
  # If triangle.to.replace="lower", replaces the lower triangle of a square matrix with its upper triangle.
  # If triangle.to.replace="upper", replaces the upper triangle of a square matrix with its lower triangle.
  
  if (nrow(m) != ncol(m)) stop("Supplied matrix must be square.")
  if      (tolower(triangle.to.replace) == "lower") tri <- lower.tri(m)
  else stop("triangle.to.replace must be set to 'lower' or 'upper'.")
  m[tri] <- t(m)[tri]
  return(m)
}
GeoDistanceInMetresMatrix <- function(df.geopoints){
  GeoDistanceInMetres <- function(g1, g2){
    DistM <- function(g1, g2){
      require("Imap")
      #gdist(lat.1=g1$Latitude, lon.1=g1$Longitude, lat.2=g2$Latitude, lon.2=g2$Longitude, units="m")
      #latlon2ft(origin=paste(g1$Latitude,g1$Longitude,sep = ","),destination=paste(g2$Latitude,g2$Longitude,sep = ","))
      return(ifelse(g1$index > g2$index, 0, gdist(lat.1=as.numeric(g1$Latitude), lon.1=as.numeric(g1$Longitude), lat.2=as.numeric(g2$Latitude), lon.2=as.numeric(g2$Longitude), units="km")))
    }
    return(mapply(DistM, g1, g2))
  }
  
  n.geopoints <- nrow(df.geopoints)
  
  # The index column is used to ensure we only do calculations for the upper triangle of points
  df.geopoints$index <- 1:n.geopoints
  
  # Create a list of lists
  list.geopoints <- by(df.geopoints[,c("index", "Latitude", "Longitude")], 1:n.geopoints, function(x){return(list(x))})
  
  # Get a matrix of distances (in metres)
  mat.distances <- ReplaceLowerOrUpperTriangle(outer(list.geopoints, list.geopoints, GeoDistanceInMetres), "lower")
  
  # Set the row and column names
  rownames(mat.distances) <- df.geopoints$Descricao
  colnames(mat.distances) <- df.geopoints$Descricao
  
  return(mat.distances)
}
distanceMatrix <- GeoDistanceInMetresMatrix(locData[,-1])

## yet to be implemented
##Distance of the journey 
for(j in 1:427663){
  unlistedLoc <- unlist(fromJSON(dataSmall[j,"POLYLINE"]))
  print(j)
  #print(unlistedLoc)
  if(!is.null(unlistedLoc))
  {
    #print(dataSmall[j,"ID"])
    strtLat <- unlistedLoc[2]
    strtLon <- unlistedLoc[1]
    endLat  <- unlistedLoc[length(unlistedLoc)]
    endLon  <- unlistedLoc[length(unlistedLoc)-1]
    library(Imap)
    dataSmall[j,"DIST"] <- gdist(lat.1=as.numeric(strtLat), lon.1=as.numeric(strtLon), lat.2=as.numeric(endLat), lon.2=as.numeric(endLon), units="km")
  }
}
## First check the tours which is more than 50 km and also start & end is 50 km away from any closer taxi stand land mark
##Check the tours which arbitarily started and crossing distance thresold.. greater Distance
##Check the the tours starting from stand but deviating larger than thresold and journey is bigger
##Only journey very big than thresold
FiftykmDist <- subset(dataSmall,(CALL_TYPE=='C' & ((DIST>50 & StartDev>50) | EndDev>50)) & StartDev<=250 & EndDev<=250)
FiftykmDist_A <- dataSmall[dataSmall$CALL_TYPE=='A' & dataSmall$DIST>50,]
FiftykmDist_B <- dataSmall[dataSmall$CALL_TYPE=='B' & dataSmall$DIST>50,]


## take those co ordinates by dividing the routes and make cluster

SecondLocData <- data.frame(Descricao=character(),Latitude=double(),Longitude=double())

#This loop will find the co ordinates in almost 45 km interval, as it is asume a cab can go 50 km at average without taking any fuel
for(i in 1:257){
  print(i)
  ployLineVector <- fromJSON(FiftykmDist[i,"POLYLINE"])
  lengthPolyLine <- length(ployLineVector)
  if(lengthPolyLine>1){
    distant <- FiftykmDist[i,"DIST"]
    intervals <- as.integer(distant/45)
    skipPoints <- as.integer(lengthPolyLine/distant*45)
    primIndex <- 1
    while(primIndex<lengthPolyLine){
      newLat <- as.numeric(unlist(ployLineVector[[primIndex]])[2])
      newLon <- as.numeric(unlist(ployLineVector[[primIndex]])[1])
      SecondLocData <- rbind(SecondLocData,data.frame(Descricao=as.character(reverseGeoCode(c(newLat,newLon))),Latitude=newLat,Longitude=newLon))
      primIndex <- primIndex+skipPoints
      
    }
  }
  Sys.sleep(0.5) ##To overcome the Google API timeout
}

#Strip the country name from the names
SecondLocData$Descricao <- lapply(SecondLocData$Descricao,function(x) substr(x,0,nchar(x)-10))


#Find the duplicate place name from the SecondLocation Data Table and store the count for candidature of cluster centroid
SecondLocData <- SecondLocData[order(SecondLocData$Descricao),]

SecondLocDataTrimmed <- data.frame(Descricao=character(),Latitude=double(),Longitude=double())
PlaceCount <- data.frame(Descricao=character(),Count=integer())

prevStr <- SecondLocData[1,"Descricao"]     #If a placename has multiple occurence under different LatLon, mean of them were taken.
count <- 1                               
latSum <- SecondLocData[1,"Latitude"]
longSum <- SecondLocData[1,"Longitude"]
for( i in 2:773){
  print(i)
  if(SecondLocData[i,"Descricao"]==prevStr){
    print("In if")
    count <- count+1
    latSum <- latSum+SecondLocData[i,"Latitude"]
    longSum <- longSum+SecondLocData[i,"Longitude"]
  }
  else
  {
    print("In else")
    PlaceCount <- rbind(PlaceCount,data.frame(Descricao=prevStr,Count=count))
    SecondLocDataTrimmed <- rbind(SecondLocDataTrimmed,data.frame(Descricao=prevStr,Latitude=latSum/count,Longitude=longSum/count))
    count <- 1
    latSum <- SecondLocData[i,"Latitude"]
    longSum <- SecondLocData[i,"Longitude"]
    prevStr <- SecondLocData[i,"Descricao"]
    
  }
}

rm(prevStr,count,i,a,latSum,longSum)

#Remove the rows where occurence of the location is less than 5 and Sort the data frame with decreasing order of count
PlaceCount <- PlaceCount[PlaceCount$Count>=5,]
#Make the distance matrix from the new Locations
SecondLocDataTrimmed$Descricao <- as.character(SecondLocDataTrimmed$Descricao)
distanceMatrixFarLoc <- GeoDistanceInMetresMatrix(SecondLocDataTrimmed)

## Number of Medoids will be choosen by Cluster validation tool of clValid
## number of cluster reference:- arxiv.org/pdf/1205.1117
library(clValid)
internalValidator <-  clValid(distanceMatrixFarLoc, 14:22, clMethods=c("pam"),validation="internal")

stabilityValidator <- clValid(distanceMatrixFarLoc, 14:16, clMethods=c("pam"),validation="stability",verbose = T)

#Make Cluster
ClusterOutskirts <- pam(distanceMatrixFarLoc,16)
library(ggplot2)
plot(ClusterOutskirts)

#The cluster medoids for the primary locations of Fuel pumps
FuelLocPrimary <- merge(data.frame(Clusterid=1:16,Descricao=cbind(row.names(ClusterOutskirts$medoids))),SecondLocDataTrimmed,by.y = "Descricao",by.x = "Descricao")
FuelLocPrimary <- FuelLocPrimary[order(FuelLocPrimary$Clusterid),]
row.names(FuelLocPrimary) <- 1:16

#Extract the cluster member ship
ClusterMembers <- data.frame(ClusterNumber=ClusterOutskirts$clustering)
ClusterMembers$Descricao <- row.names(ClusterMembers)
row.names(ClusterMembers) <- 1:513
#Arranging Cluster members for fast searching
library(plyr)
ClusterMembers <-arrange(ClusterMembers,ClusterNumber,Descricao)

#Finding the important land mark using the Placecount
BetterChoiceOfFuelPump <- merge(PlaceCount,ClusterMembers,by.x = "Descricao",by.y="Descricao")
BetterChoiceOfFuelPump <- merge(BetterChoiceOfFuelPump,SecondLocDataTrimmed,by.y = "Descricao",by.x = "Descricao")

#Validation of Taxi Stand and predefined landmark cluster
StandInternalValidator <-  clValid(distanceMatrix, 6:8, clMethods=c("pam"),validation="internal",verbose = T)
StandStabilityValidator <-  clValid(distanceMatrix, 6:8, clMethods=c("pam"),validation="stability",verbose = T)
#From the validator it seems 6 clusters are the perfect number
clusterStands <- pam(distanceMatrix,6)
FuelLocStation <- merge(data.frame(Clusterid=1:6,Descricao=cbind(row.names(clusterStands$medoids))),locData,by.y = "Descricao",by.x = "Descricao")
FuelLocStation <- FuelLocStation[,-c(3)]
#Plot the Fuel pump location data on map to see the spread
library(ggplot2)
library(ggmap)
testmap <- get_googlemap(c(lon=-8.6166667,lat=41.15) ,zoom=7, 
                         xlim=c(-6.203549,-9.332479), ylim=c(38.64142,42.17828))


#plotting
ggmap(testmap) + geom_point(aes(x= Longitude, y= Latitude), data=FuelLocPrimary, colour="red", size=5)+geom_point(aes(x= Longitude, y= Latitude), data=BetterChoiceOfFuelPump, colour="blue", size=4)+geom_point(aes(x= Longitude, y= Latitude), data=FuelLocStation, colour="green", size=4)


