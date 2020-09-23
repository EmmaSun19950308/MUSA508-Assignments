# Question:
# 
# Percentage Bachelor legend 


# Question:
# 4. When and how to submit assignment? 
# 5. Graduated symbol plot must need longitude and latitude?
#    How to understand geometry? Relation between XY coordinate, between lon&lat?
new_buffer <- 
  st_transform(D_Stops,('EPSG:32138')) %>%
  st_union(st_buffer(D_Stops, 2640)) %>%
  st_sf() %>%
  dplyr::select(STA_NAME)

join_data <- 
  st_join(tracts17, new_buffer) %>%
  filter(!is.na(STA_NAME))

popByStation <-
  join_data %>% 
  group_by(STA_NAME) %>%
  summarize(sumPop = sum(TotalPop)) 

Dallas <- st_read('https://www.dallasopendata.com/api/geospatial/mn9w-m2tp?method=export&format=GeoJSON')

ggplot() + 
  geom_sf(data = Dallas, fill = 'grey') +
  geom_sf(data = popByStation, aes(size = sumPop)) 

# 6. Why only looked at tracts09 when combining transit data？
D_Stops <- 
  st_read('https://opendata.arcgis.com/datasets/65b609c50cba44c19cfbaa2a0251a8ae_0.geojson') %>% 
  mutate(Line = "Dallas Transit") %>%
  select(STA_NAME) %>%
  st_transform(st_crs(tracts09))  

# Same question on select centroid
selectCentroids <-
  st_centroid(tracts09)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts09, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

# So for graduated symbol plot, should I use tracts17 or tracts09?

# 8. How to understand st_drop_geometry() and st_sf()
selectCentroids <-
  st_centroid(tracts09)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts09, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")

# Markdown output has R console and from Line 375 markdown cannot be knitted to html
# Policy brief inside markdown


# load Libraries
knitr::opts_chunk$set(echo = TRUE)
options(scipen=999)


library(tidyverse)
library(tidycensus)
library(sf)
library(gridExtra)
library(grid)
library(knitr)
library(kableExtra)
library(rmarkdown)
library(ggplot2)

# ---- Load Styling options -----
# create style functions 

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}


# Load hexadecimal color palette

palette5 <- c("#CCCCFF","#CC99FF","#9966CC","#663399","#330066")

# Load census API key

census_api_key("a7edab3d7c3df571998caab5a3cc12a4ec8d8b61" , overwrite  = TRUE)

# Load multipleRingBuffer Function

multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
{
  #create a list of distances that we'll iterate through to create each ring
  distances <- seq(0, maxDistance, interval)
  #we'll start with the second value in that list - the first is '0'
  distancesCounter <- 2
  #total number of rings we're going to create
  numberOfRings <- floor(maxDistance / interval)
  #a counter of number of rings
  numberOfRingsCounter <- 1
  #initialize an otuput data frame (that is not an sf)
  allRings <- data.frame()
  
  #while number of rings  counteris less than the specified nubmer of rings
  while (numberOfRingsCounter <= numberOfRings) 
  {
    #if we're interested in a negative buffer and this is the first buffer
    #(ie. not distance = '0' in the distances list)
    if(distances[distancesCounter] < 0 & distancesCounter == 2)
    {
      #buffer the input by the first distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #different that buffer from the input polygon to get the first ring
      buffer1_ <- st_difference(inputPolygon, buffer1)
      #cast this sf as a polygon geometry type
      thisRing <- st_cast(buffer1_, "POLYGON")
      #take the last column which is 'geometry'
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add a new field, 'distance' so we know how far the distance is for a give ring
      thisRing$distance <- distances[distancesCounter]
    }
    
    
    #otherwise, if this is the second or more ring (and a negative buffer)
    else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
    {
      #buffer by a specific distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create the next smallest buffer
      buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #This can then be used to difference out a buffer running from 660 to 1320
      #This works because differencing 1320ft by 660ft = a buffer between 660 & 1320.
      #bc the area after 660ft in buffer2 = NA.
      thisRing <- st_difference(buffer2,buffer1)
      #cast as apolygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #get the last field
      thisRing <- as.data.frame(thisRing$geometry)
      #create the distance field
      thisRing$distance <- distances[distancesCounter]
    }
    
    #Otherwise, if its a positive buffer
    else 
    {
      #Create a positive buffer
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create a positive buffer that is one distance smaller. So if its the first buffer
      #distance, buffer1_ will = 0. 
      buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #difference the two buffers
      thisRing <- st_difference(buffer1,buffer1_)
      #cast as a polygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #geometry column as a data frame
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add teh distance
      thisRing$distance <- distances[distancesCounter]
    }  
    
    #rbind this ring to the rest of the rings
    allRings <- rbind(allRings, thisRing)
    #iterate the distance counter
    distancesCounter <- distancesCounter + 1
    #iterate the number of rings counter
    numberOfRingsCounter <- numberOfRingsCounter + 1
  }
  
  #convert the allRings data frame to an sf data frame
  allRings <- st_as_sf(allRings)
}


# ---- Year 2009 tracts -----

# Notice this returns "long" data - let's examine it
View(load_variables(2009,'acs5',cache = TRUE))

tracts09 <-  
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2009, state= 48, county= 113, geometry=T) %>% 
  st_transform('EPSG:32138')

# Let's "spread" the data into wide form
# each row is tract, each column is variable
tracts09 <- 
  tracts09 %>%
  dplyr::select( -NAME, -moe) %>%
  spread(variable, estimate) %>%
  dplyr::select(-geometry) %>%
  rename(TotalPop = B25026_001, 
         Whites = B02001_002,
         FemaleBachelors = B15001_050, 
         MaleBachelors = B15001_009,
         MedHHInc = B19013_001, 
         MedRent = B25058_001,
         TotalPoverty = B06012_002)

# st_drop_geometry drops the geometry of its argument, and reclasses it accordingly
st_drop_geometry(tracts09)[1:3,]

# Let's create new rate variables using mutate

tracts09 <- 
  tracts09 %>%
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop), 0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2009") %>%
  dplyr::select(-Whites,-FemaleBachelors,-MaleBachelors,-TotalPoverty)


# Tracts 2009 is now complete. Let's grab 2017 tracts and do a congruent
# set of operations

# ---- 2017 Census Data -----

# Notice that we are getting "wide" data here in the first place
# This saves us the trouble of using "spread"

tracts17 <- 
  get_acs(geography = "tract", variables = c("B25026_001E","B02001_002E","B15001_050E",
                                             "B15001_009E","B19013_001E","B25058_001E",
                                             "B06012_002E"), 
          year=2017, state= 48, county= 113, geometry=T, output="wide") %>%
  st_transform('EPSG:32138') %>%
  rename(TotalPop = B25026_001E, 
         Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
         MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
         MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  dplyr::select(-NAME, -starts_with("B")) %>% #-starts_with("B") awesome!
  mutate(pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty) 

# --- Combining 09 and 17 data ----

allTracts <- rbind(tracts09,tracts17)



# ---- Wrangling Transit Open Data -----

D_Stops <- 
  st_read('https://opendata.arcgis.com/datasets/65b609c50cba44c19cfbaa2a0251a8ae_0.geojson') %>% 
      mutate(Line = "Dallas Transit") %>%
      select(STA_NAME) %>%
  st_transform(st_crs(tracts09))  


ggplot() + 
  geom_sf(data=st_union(tracts09)) +
  geom_sf(data=D_Stops, 
          aes(colour = 'orange'), 
          show.legend = "point", size= 2) +
  scale_colour_manual(values = c("orange")) +
  labs(title="Dallas Transit Stops", 
       subtitle=" ", 
       caption="Figure 1.1") +
  mapTheme()

# --- Relating Dallas Stops and Tracts ----

# Create buffers (in feet - note the CRS) around Dallas stops -
# Both a buffer for each stop, and a union of the buffers...
# and bind these objects together

D_Buffers <- 
  rbind(
    st_buffer(D_Stops, 2640) %>%
      mutate(Legend = "Buffer") %>%
      dplyr::select(Legend),
    st_union(st_buffer(D_Stops, 2640)) %>%
      st_sf() %>%
      mutate(Legend = "Unioned Buffer"))

# Let's examine both buffers by making a small multiple
# "facet_wrap" plot showing each

ggplot() +
  geom_sf(data=D_Buffers) +
  geom_sf(data=D_Stops, show.legend = "point") +
  facet_wrap(~Legend) + 
  labs(caption = "Figure 2.6") +
  mapTheme()

# ---- Spatial operations ----

# Consult the text to understand the difference between these three types of joins
# and discuss which is likely appropriate for this analysis

# Create an sf object with ONLY the unioned buffer
buffer <- filter(D_Buffers, Legend=="Unioned Buffer")

# Do a centroid-in-polygon join to see which tracts have their centroid in the buffer
# Note the st_centroid call creating centroids for each feature
selectCentroids <-
  st_centroid(tracts09)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(tracts09, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")



# ---- Indicator Maps ----

# We do our centroid joins as above, and then do a "disjoin" to get the ones that *don't*
# join, and add them all together.
# Do this operation and then examine it.
# What represents the joins/doesn't join dichotomy?
# Note that this contains a correct 2009-2017 inflation calculation
# 还需要加上variable本身的不同
allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD")) %>%
  mutate(MedRent.inf = ifelse(year == "2009", MedRent * 1.14, MedRent))  # inflation


ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = TOD)) +
  labs(title = "Time/Space Groups") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))


ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedRent.inf))) +
  geom_sf(data = buffer, fill = "transparent", color = "red" )+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))


ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(pctWhite))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctWhite"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))


ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(pctBachelors))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "pctBachelors"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))


ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedHHInc))) +
  geom_sf(data = buffer, fill = "transparent", color = "red")+
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedHHInc"),
                    name = "Rent\n(Quintile Breaks)") +
  labs(title = "Median Rent 2009-2017", subtitle = "Real Dollars") +
  facet_wrap(~year)+
  mapTheme() + 
  theme(plot.title = element_text(size=22))

# --- TOD Indicator Tables ----

allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
  group_by(year, TOD) %>%
  summarize(Rent = mean(MedRent, na.rm = T),
            Population = mean(TotalPop, na.rm = T),
            Percent_White = mean(pctWhite, na.rm = T),
            Percent_Bach = mean(pctBachelors, na.rm = T),
            Percent_Poverty = mean(pctPoverty, na.rm = T),
            Median_Household_Income = mean(MedHHInc, na.rm = T))

allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value) %>%
  kable() %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2.3")

# --- TOD Indicator Plots ------

# Let's create small multiple plots
# We use the "gather" command (look this one up please)
# To go from wide to long
# Why do we do this??
# Notice we can "pipe" a ggplot call right into this operation!

allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Variable, scales = "free", ncol=5) +
  scale_fill_manual(values = c("#debae4", "#800694")) +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + 
  theme(legend.position="bottom")

# Q5

new_buffer <- 
  st_transform(D_Stops,('EPSG:32138')) %>%
  st_union(st_buffer(D_Stops, 2640)) %>%
  st_sf() %>%
  dplyr::select(STA_NAME)

join_data <- 
  st_join(tracts17, new_buffer) %>%
  filter(!is.na(STA_NAME))

popByStation <-
  join_data %>% 
  group_by(STA_NAME) %>%
  summarize(sumPop = sum(TotalPop)) 
  
Dallas <- st_read('https://www.dallasopendata.com/api/geospatial/mn9w-m2tp?method=export&format=GeoJSON')

ggplot() + 
  geom_sf(data = Dallas, fill = 'grey') +
  geom_sf(data = popByStation, aes(size = sumPop)) 



# Q6 
allTracts.rings <-
  st_join(st_centroid(dplyr::select(allTracts, GEOID, year)), 
          multipleRingBuffer(st_union(D_Stops), 47520, 2640)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(allTracts, GEOID, MedRent, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate(distance = distance / 5280) 

allTracts.rings <- na.omit(allTracts.rings)

mean_RENT <- allTracts.rings %>%
  group_by(year, distance) %>%
  summarize(mean_RENT = mean(MedRent)) 

ggplot(data = mean_RENT) +
  geom_line(aes(x = distance, y = mean_RENT, col = year)) +
  labs(title = " Rent as a function of distance to subway stations (Figure x.x.x)") +
  plotTheme() + 
  theme(legend.position="bottom")


# Q7
# -------- Crime Data ----------
crime_dat <- read.csv('Dallas_Crime_Data.CSV')

crime_dat_point <- st_as_sf(crime_dat, coords = c("Longitude", "Latitude"), 
                  crs = 4326, agr = "constant") %>%
  select(Type, geometry)

# burglary vs. rents
ggplot(allTracts.group) + 
geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = q5(MedRent.inf))) +
  geom_sf(data = crime_dat_point, 
          aes(color = Type),
          show.legend = 'point',size = 1) +
  scale_fill_manual(values = palette5,
                    labels = qBr(allTracts.group, "MedRent.inf"),
                    name = "Rent\n(Quintile Breaks)")  +
  labs(title = "Indicator differences across time and space") +
  plotTheme() + theme(legend.position="bottom")


# burglary vs. Transit
ggplot(allTracts.group)+
  geom_sf(data = st_union(tracts09))+
  geom_sf(aes(fill = TOD)) +
  scale_fill_manual(values = palette5) +
            
  geom_sf(data = crime_dat_point, 
          aes(color = Type),
          show.legend = 'point',size = 1) +
  labs(title = "Burglary vs. Transit") +
  plotTheme() + 
  theme(legend.position="bottom")
  mapTheme() + 
  theme(plot.title = element_text(size=22))



# -------- Crime Data ----------
# http://www.slmpd.org/Crimereports.shtml
crime_dat <- read.csv('May2019.csv')

# spatial reference list 
# https://www.spatialreference.org/ref/?search=Missouri
library(proj4)
proj4string <- "+proj=tmerc +lat_0=32.784 +lon_0=-96.79300000000001 +k=1 +x_0=24500 +y_0=24500 +ellps=WGS84 +units=m +no_defs"

# Transformed data
pj <- data.frame(project(cbind(crime_dat$X.Coordinate,crime_dat$Y.Coordinate), proj4string,inv = TRUE))
crime_dat$Latitude <- pj$X2
crime_dat$Longitude <- pj$X1

leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(~Longitude, ~Latitude, 
                   data = crime_dat,
                   radius = 2, 
                   weight = 1,
                   stroke = TRUE)



