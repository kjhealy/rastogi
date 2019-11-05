library(ggplot2)
library(maptools)
library(mapproj)
library(rgeos)


library(rgdal)
library(gdalUtils)

library(scales)

theme_set(theme_minimal())

## Make a "figures" subdirectory if one doesn't exist
ifelse(!dir.exists(file.path("figures")),
       dir.create(file.path("figures")),
       FALSE)

###--------------------------------------------------
### Data 
###--------------------------------------------------


## US County data per the book / Bob Rudis
ogrInfo("us_counties/geojson/gz_2010_us_050_00_5m.json")
us_counties <- readOGR(dsn="us_counties/geojson/gz_2010_us_050_00_5m.json")


## 2017 TIGER/Line state shapefile from Minnesota Pop Center
ogrListLayers("shapefiles")

us_raw <- readOGR(dsn = "shapefiles", layer = "US_state_2017")

aea_proj  <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
+lon_0=-96 +datum=WGS84 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

state_aea <- spTransform(us_raw, aea_proj)

## Simplify polygons
state_aea_sim <- rmapshaper::ms_simplify(state_aea, keep = 0.01)

raster::crs(state_aea_sim)
raster::extent(state_aea_sim)
bbox(state_aea_sim)


p <- ggplot(data=state_aea_sim,
            aes(x=long, y=lat, group=group)) 

p + geom_polygon(color="gray80",
                   size=0.2) + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma,
                       breaks = c(-7e6, -6e6, -4e6, -2e6,
                                  0, 2e6, 3e6))

raster::crs(us_raw)
raster::extent(us_raw)
bbox(us_raw)

us_sim <- rmapshaper::ms_simplify(us_raw)
raster::crs(us_sim)
raster::extent(us_sim)

p <- ggplot(data=us_sim,
            aes(x=long, y=lat, group=group)) 

p + geom_polygon(color="gray80",
                 size=0.2) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma)


aea_proj  <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23
+lon_0=-96 +datum=WGS84 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

state_aea  <-  spTransform(us_sim, aea_proj)

p <- ggplot(data=state_aea,
            aes(x=long, y=lat, group=group))

p + geom_polygon(color="gray80",
                   size=0.2) + 
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma, ,
                       breaks = c(-7e6, -6e6, -4e6, -2e6, 0,
                                                          2e6, 3e6))

raster::crs(state_aea)
raster::extent(state_aea)


## us_sim <- spTransform(us_sim, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +a=6370997 +b=6370997 +units=m +no_defs"))

us_sim <- spTransform(us_sim, aea_proj)

raster::crs(us_sim)
raster::extent(us_sim)



## us_raw_json <- geojson_json(us_raw)
## us_json_sim <- rmapshaper::ms_simplify(us_raw_json)

## class(us_json_sim)

## geojson_write(us_json_sim, file = "data/us_state_sim.geojson")

## ## Now start from the geojson file
## us_states <- st_read("data/us_state_sim.geojson")

## Albers projection
us_states <- us_sim
us_states@data$id <- rownames(us_states@data)

# Extract, then rotate, shrink & move alaska (and reset projection)
# need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- us_states[us_states$STATEFP=="02",]
alaska <- maptools::elide(alaska, rotate=-50)
alaska <- maptools::elide(alaska,
                           scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- maptools::elide(alaska, shift=c(-2.5e6, -0.1e6))
proj4string(alaska) <- proj4string(us_states)



# extract, then rotate & shift hawaii
hawaii <- us_states[us_states$STATEFP=="15",]
hawaii <- maptools::elide(hawaii, rotate=-35)
hawaii <- maptools::elide(hawaii, shift=c(5.4e6, -1.4e6))
proj4string(hawaii) <- proj4string(us_states)

# remove old states and put new ones back in; note the different order
# we're also removing puerto rico in this example but you can move it
# between texas and florida via similar methods to the ones we just used
us_states <- us_states[!us_states$STATEFP
                             %in% c("02", "15", "72", "66", "78", "60", "69",
                                    "64", "68", "70", "74", "81", "84", "86",
                                    "87", "89", "71", "76", "95", "79"),]

us_states <- rbind(us_states, alaska, hawaii)

png(filename = "figures/test.png", width = 1024, height = 768)
p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat, group = group))
p + geom_polygon(color="white",
             size=0.2) +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma)
dev.off()

