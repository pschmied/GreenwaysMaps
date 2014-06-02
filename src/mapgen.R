# Map generation for greenways

# Required libraries
# install.packages(c("rgdal", "XML", "ggmap", "rgeos", "extrafont"))
library(rgdal) # For loading all spatial data
library(XML) # Web scraping for school addresses
library(ggmap) # For mapping
library(rgeos) # for constructing buffers
library(extrafont) # for school symbol
font_import(pattern="Symbola", prompt=FALSE)

# Read our spatial datasets
routes <- readOGR(dsn="./data/", layer="routes")
crossings <- readOGR(dsn="./data/", layer="crossings")

getschoolsaddress <- function() {
  # Get our POI addresses
  SPSurl <- "http://goo.gl/GoJu3U"
  SPSaddresses <- readHTMLTable(SPSurl, which=3:8, header=TRUE) # addresses table
  SPSdf <- subset(do.call(rbind.data.frame, SPSaddresses), select=c(1, 3))
  names(SPSdf) <- c("Name", "Address")
  SPSdf$Name <- gsub("\\s+", " ", gsub("Â", "", SPSdf$Name))
  SPSdf$Address <- gsub("\\s+$", "", gsub("(Â|\r\n.+$)", "", SPSdf$Address))
  row.names(SPSdf) <- NULL
  SPSdf
  # Uncomment to purge "Skills Center" results
  # SPSdf[SPSdf$Name == "Skills Center", ]
}

geocodepoi <- function() {
  # Calls getschooladdress for a dataframe,
  # Return a SpatialPointsDataFrame with Name, Address in @data slot
  schooldf <- getschoolsaddress()
  gstring <- paste(schooldf$Name, schooldf$Address, "Seattle, WA", sep=", ")
  latlon <- geocode(gstring)
  spdf <- cbind(schooldf, latlon)
  coordinates(spdf) <- ~lon+lat
  spdf@proj4string <- CRS("+init=EPSG:4326")
  spdf
}

bufferpoi <- function(spatialpointsdf, dist) {
  # Takes a SPointsDF (one record, probably) and returns
  # a buffer of the desired distance in meters
  spdf.proj <- spTransform(spatialpointsdf, CRS("+init=EPSG:3857"))
  buff <- gBuffer(spdf.proj, width=dist, quadsegs=100)
  spTransform(buff, CRS("+init=EPSG:4326"))
}

  
  
plotpoi <- function(allpoi, poi, routes, crossings) {
    # takes an SPointsDF with all poi (because context) and a single focal poi
    # returns a ggmap object with the requisite features
    
    # buffer our POIs at .25mi, .5mi, and useful zoom level
    m.25mi <- fortify(bufferpoi(poi, 402.336))
    m.50mi <- fortify(bufferpoi(poi, 804.672))
    m.zoom <- fortify(bufferpoi(poi, 1005.84))
    
    # Fortify and split routes
    routes.fort <- fortify(routes)
    routes@data$id <- row.names(routes)
    routes.fort <- merge(x=routes.fort, y=subset(routes@data, select=c("id", "Priority")), by="id")
    routes.fort$Priority <- as.factor(routes.fort$Priority)
    
    # Fake fort
    crossings.fort <- data.frame(crossings@coords, Rank=as.character(crossings@data$Rank))
    
    # Additional crop factor
    cfactor <- .001
    
    gmapdata <- get_map(location=poi@coords, zoom=15, scale=2, maptype="roadmap")
    mp <- ggmap(gmapdata, extent="normal", maprange=FALSE) +
      geom_polygon(data=m.25mi, aes(x=long, y=lat, group=group),
                   linetype=3, colour="grey", alpha=.05, size=1.1) +
      geom_polygon(data=m.50mi, aes(x=long, y=lat, group=group),
                   linetype=1, colour="darkgrey", alpha=.05, size=1.1) +
      geom_line(data=routes.fort, mapping=aes(x=long, y=lat, group=id, linetype=Priority),
                size=1, colour="darkgreen") +
      geom_point(data=crossings.fort, mapping=aes(x=coords.x1, y=coords.x2, shape=Rank),
                 color="red", size=4) +
      geom_text(data=data.frame(poi@coords),
                aes(x=lon, y=lat, label="∆", family="Symbola"),
                size=16) +
      coord_map(projection="mercator",
                xlim=c(min(m.zoom$long), max(m.zoom$long)),
                ylim=c(min(m.zoom$lat), max(m.zoom$lat))) +
      theme_nothing()
    mp
}
