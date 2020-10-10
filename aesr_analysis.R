library(tidyverse)
library(ggplot2)
library(stringr)
library(jsonlite)
read.csv("aser/ASER2016GSchool.csv")
school <- read.csv("aser/ASER2016GSchool.csv")

read.csv()


head(school)
child <- read.csv("aser/ASER2016Child.csv")

RegionName <- c("2" = "Panjab", 
                "3" = "Sindh", 
                "4" = "Balochistan", 
                "5" = "Khyber Pakhtunkhwa", 
                "6" = "Gilgit-Baltistan", 
                "7" = "Azad Jammu and Kashmir", 
                "8" = "Islamabad - ICT", 
                "9" = "Federally Administrated Tribal Areas")
Gender <- c("0" = "Male",
            "-1" = "Female")
# Total samples
length(unique(child$CID))

# sample size hunza
child %>% 
  filter(DID == 266) %>% 
  summarize(N_hunza = length(unique(CID)))

# Gender proportion
child %>% 
  filter(DID == 266) %>% 
  summarize(gender_rate = mean(C002))

# Eduation status
# 1 = never enrolled; 2 = drop-out; 3 = currently enrolled
child %>% 
  filter(DID == 266) %>% 
  ggplot(aes(C003)) +
  geom_histogram(bins = 3) 

# Education status by gender
child %>% 
  filter(DID == 266) %>% 
  ggplot(aes(C003)) +
  geom_histogram(bins = 3, binwidth = 1) +
  facet_grid(~C002, labeller = labeller(C002 = Gender))


# Plotting the enrollment rate by gender
child %>% 
  filter(DID == 266) %>% 
  group_by(C002) %>% 
  summarize(enrollment_rate = mean(C003 == 3)) %>% 
  ungroup() %>% 
  ggplot(aes(C002, enrollment_rate)) +
  geom_col() +
  scale_y_continuous() +
  geom_label(aes(label = enrollment_rate))
  

# Comparison between other districts
child %>% 
  group_by(DID) %>% 
  mutate(avg = round(mean(C003 == 3), digits = 2)) %>% 
  ungroup() %>% 
  ggplot(aes(avg)) +
  geom_histogram() +
  facet_grid(~RID, labeller = labeller(RID = RegionName)) +
  labs(title = "Current Enrollment Rate by Region")


# Within Gilgit-Baltistan
child %>% 
  filter(RID == 6) %>% 
  group_by(DID) %>% 
  mutate(Current_Enrollment_Rate = mean(C003 == 3)) %>% 
  ggplot(aes(DID, Current_Enrollment_Rate)) +
  geom_count() +
  scale_x_continuous(breaks = 260:266, labels = c("Gilgit", "Diamer", "Skardu", "Ghanshe", "Astore", "Ghizer", "Hunza-Nagar"))

# Importing map information
  
# Importing map information

map <- read.csv("map/csvData.csv")
map$location <- str_remove(map$location, "https://www.google.com/maps/")
map$location <- str_remove(map$location, "^\\D")
map$location <- str_remove(map$location, "q=")
map

provDist <- read.csv("aser/ASER2016ProvDist.csv")

head(provDist)

child <- child %>% right_join(provDist[-1], by = "DID")

map$location <- str_split(map$location, pattern = ",")
col_names <- c("long", "lat")
a <- t(as.data.frame(map$location))
colnames(a) <- col_names
class(a)
lat <- a[,1]
long <- a[,2]
long <- as.vector(long)

long <- as.numeric(long)
lat <- as.vector(lat)
lat <- as.numeric(lat)
map <- map %>% 
  mutate(long = long, lat = lat) %>% 
  select(-location)

map

height <- max(map$lat) - min(map$lat)
width <- max(map$long) - min(map$long)
pak_borders <- c(bottom  = min(map$lat)  - 0.1 * height, 
                 top     = max(map$lat)  + 0.1 * height,
                 left    = min(map$long) - 0.1 * width,
                 right   = max(map$long) + 0.1 * width)

sum(map$name == child$DNAME)
length(map$name)
length(child$DNAME)
# map_a <- get_stamenmap(pak_borders, zoom = 10, maptype = "toner-lite")

# ggmap(map_a)
