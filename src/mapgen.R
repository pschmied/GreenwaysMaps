# Map generation for greenways

# Required libraries
# install.packages(c("rgdal", "XML", "ggmap", "rgeos", "extrafont"))
library(rgdal) # For loading all spatial data
library(XML) # Web scraping for school addresses
library(ggmap) # For mapping
library(rgeos) # for constructing buffers
library(extrafont) # for school symbol
loadfonts(quiet=TRUE)

getspsschoolsaddress <- function() {
  # Get our POI addresses
  SPSurl <- "http://goo.gl/GoJu3U"
  SPSaddresses <- readHTMLTable(SPSurl, which=3:8, header=TRUE) # addresses table
  SPSdf <- subset(do.call(rbind.data.frame, SPSaddresses), select=c(1, 3))
  names(SPSdf) <- c("Name", "Address")
  SPSdf$Name <- gsub("\\s+", " ", gsub("Â", "", SPSdf$Name))
  SPSdf$Address <- gsub("\\s+$", "", gsub("(Â|\r\n.+$)", "", SPSdf$Address))
  row.names(SPSdf) <- NULL
  SPSdf$id <- as.numeric(row.names(SPSdf))
  SPSdf$type <- "school"
  SPSdf
  # Uncomment to purge "Skills Center" results
  # SPSdf[SPSdf$Name == "Skills Center", ]
}

geocodepoi <- function() {
  # Calls getschooladdress for a dataframe,
  # Return a SpatialPointsDataFrame with Name, Address in @data slot
  schooldf <- getspsschoolsaddress()
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

readroutes <- function(dsn, layer) {
  # Read and project the layer
  routes <- readOGR(dsn, layer)
  routes <- spTransform(routes, CRS("+init=EPSG:4326"))
  # Fortify the routes into a set of points
  routes.fort <- fortify(routes)
  routes@data$id <- row.names(routes)
  routes.fort <- merge(x=routes.fort, y=routes@data, by="id")
  # Recode our route priorities from messy data
  routes.fort$Priority <- NA # base case
  routes.fort$Priority[routes.fort$Existing == "N"] <- "Future"
  routes.fort$Priority[routes.fort$SNG == 1] <- "Okay"
  routes.fort$Priority[routes.fort$SNG_best == 1] <- "Best"
  routes.fort$Priority <- as.factor(routes.fort$Priority)
  routes.fort
}

readcrossings <- function(dsn, layer) {
  # Read and project the layer
  crossings <- readOGR(dsn, layer)
  crossings <- spTransform(crossings, CRS("+init=EPSG:4326"))
  # Fortify the layer into a set of points
  # Use SymbolID instead of Rank, because it seems to be consistent
  crossings.fort <-
    data.frame(crossings@coords, Rank=as.factor(crossings@data$SymbolID))
  crossings.fort
}


plotpoi <- function(allpoi, poi, fortifiedroutes, fortifiedcrossings) {
  # takes an SPointsDF with all poi (because context) and a single focal poi
  # returns a ggmap object with the requisite features
  
  # buffer our POIs at .25mi, .5mi, and useful zoom level
  m.25mi <- fortify(bufferpoi(poi, 402.336))
  m.50mi <- fortify(bufferpoi(poi, 804.672))
  m.zoom <- fortify(bufferpoi(poi, 1005.84))
  
  # Build the plot object
  gmapdata <- get_map(location=poi@coords, zoom=15, scale=2, maptype="roadmap")
  mp <- ggmap(gmapdata, extent="normal", maprange=FALSE) +
    geom_polygon(data=m.25mi, aes(x=long, y=lat, group=group),
                 linetype=3, colour="grey", alpha=.05, size=1.1) +
    geom_polygon(data=m.50mi, aes(x=long, y=lat, group=group),
                 linetype=1, colour="darkgrey", alpha=.05, size=1.1) +
    geom_line(data=fortifiedroutes, mapping=aes(x=long, y=lat, group=id, linetype=Priority),
              size=1, colour="darkgreen") +
    geom_point(data=fortifiedcrossings, mapping=aes(x=coords.x1, y=coords.x2, shape=Rank),
               color="red", size=8) +
    scale_shape_manual(values=c(16, 17)) +
    geom_text(data=data.frame(poi@coords),
              aes(x=lon, y=lat, label="∆", family="Symbola"),
              size=16) +
    geom_text(data=data.frame(subset(allpoi, Name != poi$Name)@coords),
              aes(x=lon, y=lat, label="∆", family="Symbola"),
              size=10, alpha=.5) +
    coord_map(projection="mercator",
              xlim=c(min(m.zoom$long), max(m.zoom$long)),
              ylim=c(min(m.zoom$lat), max(m.zoom$lat))) +
    theme_nothing()
  mp
}

writemap <- function(geocodedpois, routes, crossings) {
  for(i in geocodedpois@data$id) {
    mp <- plotpoi(geocodedpois, geocodedpois[i,], routes, crossings)
    fname <- paste("./output/", i, ".png", sep="")
    ggsave(filename=fname, mp, width=6, height=6, units="in", dpi=300)
  }
}

writelatex <- function(geocodedpois) {
  pre <- readLines("./src/atlaspreamble.tex")
  minipg <- readLines("./src/atlasminipage.tex")
  post <- readLines("./src/atlaspost.tex")
  mid <- c()
  for(i in geocodedpois@data$id) {
    s <- gsub("SCHOOL", geocodedpois@data$Name[i], minipg)
    s <- gsub("ADDRESS", geocodedpois@data$Address[i], s)
    s <- gsub("MAPNUMBER", geocodedpois@data$id[i], s)
    mid <- c(mid, s)
  }
  writeLines(c(pre,mid,post), "./output/altas.tex")
}

main <- function() {
  # Read our spatial datasets
  routes <- readroutes(dsn="./data/", layer="bmp")
  crossings <- readcrossings(dsn="./data/Intersections.kml", layer="Greenway2014collapsed.xlsx")
  
  # Geocode our schools and maybe other POIs
  pois <- geocodepoi()
  
  # Output our maps to .png format
  writemap(pois, routes, crossings)
  
  # Emit LaTeX for compiling into a .PDF
  writelatex(pois)
}

main()