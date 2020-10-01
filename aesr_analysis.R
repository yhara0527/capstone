library(tidyverse)
library(ggplot2)
school <- read.csv("E:/R Data/Capstone/aser/ASER2016GSchool.csv")
head(school)
child <- read.csv("E:/R Data/Capstone/aser/ASER2016Child.csv")

RegionName <- c("2" =  "Panjab", 
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
    
  
