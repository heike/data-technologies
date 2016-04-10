library("readr")
library("ggplot2")
library("dplyr")
library("rworldmap")
library("purrr")

#rsurvey <- read_delim("data/pnas.1506047112.sd01.csv", delim=";")

#head(rsurvey$v_253)
#table(rsurvey$v_253)

datraw <- read.csv2("data/pnas.1506047112.sd01.csv", dec = ".")
table(datraw$v_253)
#datraw$v_253[datraw$v_253 == 204] <- NA

# Match names to country code
countrynames <- read.table("data/pnas.1506047112.country.txt", stringsAsFactors = FALSE)
colnames(countrynames) <- c("indx", "name")
countrynames <- rbind(countrynames, c(204, "unknown"))

datraw$country <- countrynames$name[datraw$v_253]
datraw$country[is.na(datraw$country)] <- "unknown"
table(datraw$country)

world <- getMap(resolution = "low")
extractPolys <- function(p) {
  polys <- NULL
  for (i in 1:length(p)) {
    for (j in 1:length(p[[i]]@Polygons)) {
      x <- p[[i]]@Polygons[[j]]@coords
      polys$lon <- c(polys$lon, x[,1])
      polys$lat <- c(polys$lat, x[,2])
      polys$ID <- c(polys$ID, rep(p[[i]]@ID, nrow(x)))
      polys$region <- c(polys$region, rep(paste(p[[i]]@ID, j, sep="_"), nrow(x)))
      polys$order <- c(polys$order, 1:nrow(x))
    }
  }
  return(data.frame(polys))
}
polys <- extractPolys(world@polygons)
  
qplot(lon, lat, data=polys, group=region, geom="path")
qplot(lon, lat, data=polys, group=region, geom="polygon")

# Match country names to map names
cntrynames <- unique(datraw$country)
polynames <- unique(polys$ID)
setdiff(cntrynames, polynames)

# Tabulate the countributing countries
cntry_count <- datraw %>% group_by(country) %>% tally()

# Join to map
polys_cntry <- merge(polys, cntry_count, by.x="ID", by.y="country", all.x=TRUE)
polys_cntry <- polys_cntry %>% arrange(region, order)
qplot(lon, lat, data=polys_cntry, group=region, geom="polygon", fill=n) + coord_equal()


