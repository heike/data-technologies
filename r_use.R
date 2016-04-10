library("readr")
library("ggplot2")
library("dplyr")
library("rworldmap")
library("purrr")

rsurvey <- read_delim("data/pnas.1506047112.sd01.csv", delim=";")

head(rsurvey$v_253)
table(rsurvey$v_253)

datraw <- read.csv2("data/pnas.1506047112.sd01.csv", dec = ".")
table(datraw$v_253)
datraw$v_253[datraw$v_253 == 204] <- NA

countrynames <- read.table("data/pnas.1506047112.country.txt", stringsAsFactors = FALSE)
colnames(countrynames) <- c("indx", "name")

datraw$country <- countrynames$name[datraw$v_253]

world <- getMap(resolution = "low")

length(world@polygons)

world %>% group_by()

# Need to modify this function
extractPolygons <- function(shapes) {
  
  dframe <- ldply(1:length(shapes@polygons), function(i) {
    ob <- shapes@polygons[[i]]@Polygons
    dframe <- ldply(1:length(ob), function(j) {
      x <- ob[[j]]
      co <- x@coords
      data.frame(co, order=1:nrow(co), group=j)
    })
    dframe$region <- i
    dframe$name <- shapes@polygons[[i]]@ID
    dframe
  })
  # construct a group variable from both group and polygon:
  dframe$group <- interaction(dframe$region, dframe$group)
  
  dframe
}


