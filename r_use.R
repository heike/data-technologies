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

# Set up map
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
  
# Map theme
theme_map <- theme_bw()
theme_map$line <- element_blank()
theme_map$strip.text <- element_blank()
theme_map$axis.text <- element_blank()
theme_map$plot.title <- element_blank()
theme_map$axis.title <- element_blank()
theme_map$panel.border <- element_rect(colour = "grey90", size=1, fill=NA)

#qplot(lon, lat, data=polys, group=region, geom="path") + 
#  theme_map + coord_equal()
#qplot(lon, lat, data=polys, group=region, geom="polygon") + 
#  theme_map + coord_equal()

# Match country names to map names
cntrynames <- unique(datraw$country)
polynames <- unique(polys$ID)
setdiff(cntrynames, polynames)

# Tabulate the countributing countries
cntry_count <- datraw %>% group_by(country) %>% tally()

# Join to map
polys_cntry <- merge(polys, cntry_count, by.x="ID", by.y="country", all.x=TRUE)
polys_cntry <- polys_cntry %>% arrange(region, order)
ggplot(data=polys_cntry, aes(x=lon, y=lat)) + 
  geom_polygon(aes(group=region, fill=n), color="grey90", size=0.1) + 
  scale_fill_gradient("", low="#e0f3db", high="#43a2ca", na.value="white") + 
  scale_x_continuous(expand=c(0,0)) + scale_y_continuous(expand=c(0,0)) +
  coord_equal() + theme_map 

# Examine gender
table(datraw$v_216)
datraw$v_216 <- factor(datraw$v_216, levels=c(1,2), labels=c("male","female"))
qplot(v_216, data=datraw, geom="bar") + xlab("Gender")

# Age
summary(datraw$v_215)
head(datraw$v_215)
qplot(v_215, data=datraw, geom="histogram", binwidth=5) + xlab("Age")

# Education
table(datraw$v_217)
table(datraw$v_218)
table(datraw$v_219)
table(datraw$v_220)

# Combine ed categories
datraw$ed <- "Not given"
datraw$ed[datraw$v_217 == 1] <- "HS"
datraw$ed[datraw$v_219 == 1] <- "BS/BA/MS/MBA..."
datraw$ed[datraw$v_220 == 1] <- "PhD"
datraw$ed <- factor(datraw$ed, levels=c("Not given", "HS", "BS/BA/MS/MBA...", "PhD"))
qplot(ed, data=datraw, geom="bar") + xlab("Education")

# Ed area
table(datraw$v_222)
table(datraw$v_223)
table(datraw$v_224)
table(datraw$v_225)
table(datraw$v_226)
table(datraw$v_227)
table(datraw$v_228)

# Look at proportions
