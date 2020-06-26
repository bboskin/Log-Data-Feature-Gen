library(tidyverse)
library(anytime)
library(Hmisc)
library(lubridate)
library(tibbletime)

#PAPER THAT USES THIS DATA: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.6586&rep=rep1&type=pdf

# Load data
nasa <- read_csv("/Users/Jack/Documents/data.csv")

# Data transformations
nasa <- nasa[c(1:2000),]

nasa <- nasa %>%
  select(-X1, -method, -url) %>%
  filter(bytes<3e+05) %>%
  mutate(time = anytime(time)) %>%
  mutate(day = weekdays(as.Date(time)))

nasa <- nasa[,c(1,4,2,5,3)]

nasa_grouped <- nasa %>% 
  group_by(host) %>% 
  summarise(avg_bytes = mean(bytes), 
            total_bytes = sum(bytes))

nasa <- merge(nasa, 
              nasa_grouped, 
              by = "host")

nasa_noIP<-nasa[grep("\\D$", nasa$host),]


# Adding site_type --------------------------------------------------------

dk <-
  data.frame(grep("dk$", nasa_noIP$host), rep("dk", length(grep("dk$", nasa_noIP$host))))

net <-
  data.frame(grep("net$", nasa_noIP$host), rep("net", length(grep("net$", nasa_noIP$host))))

com <-
  data.frame(grep("com$", nasa_noIP$host), rep("com", length(grep("com$", nasa_noIP$host))))

au <-
  data.frame(grep("au$", nasa_noIP$host), rep("au", length(grep("au$", nasa_noIP$host))))

edu <-
  data.frame(grep("edu$", nasa_noIP$host), rep("edu", length(grep("edu$", nasa_noIP$host))))

de <-
  data.frame(grep("de$", nasa_noIP$host), rep("de", length(grep("de$", nasa_noIP$host))))

se <-
  data.frame(grep("se$", nasa_noIP$host), rep("se", length(grep("se$", nasa_noIP$host))))

org <-
  data.frame(grep("org$", nasa_noIP$host), rep("org", length(grep("org$", nasa_noIP$host))))

gov <-
  data.frame(grep("gov$", nasa_noIP$host), rep("gov", length(grep("gov$", nasa_noIP$host))))

ca <-
  data.frame(grep("ca$", nasa_noIP$host), rep("ca", length(grep("ca$", nasa_noIP$host))))

uk <-
  data.frame(grep("uk$", nasa_noIP$host), rep("uk", length(grep("uk$", nasa_noIP$host))))

nl <-
  data.frame(grep("nl$", nasa_noIP$host), rep("nl", length(grep("nl$", nasa_noIP$host))))

pl <-
  data.frame(grep("pl$", nasa_noIP$host), rep("pl", length(grep("pl$", nasa_noIP$host))))

colnames(dk) <- c("index", "name")
colnames(net) <- c("index", "name")
colnames(com) <- c("index", "name")
colnames(au) <- c("index", "name")
colnames(edu) <- c("index", "name")
colnames(de) <- c("index", "name")
colnames(se) <- c("index", "name")
colnames(org) <- c("index", "name")
colnames(gov) <- c("index", "name")
colnames(ca) <- c("index", "name")
colnames(uk) <- c("index", "name")
colnames(nl) <- c("index", "name")
colnames(pl) <- c("index", "name")


sites<-
  bind_rows(dk, net, com, au, edu, de, se, org, gov, ca, uk, nl, pl)

sites <- sites %>%
  arrange(index)
  
nasa_noIP <-
  data.frame(nasa_noIP, sites[,2])

colnames(nasa_noIP)[6] <- "site_type"




# Adding time of day ------------------------------------------------------

nasa_noIP<- 
  nasa_noIP %>%
  mutate(date = as.Date(time)) %>%
  mutate(time = strftime(time, "%H:%M:%S"))

nasa_noIP <-
  nasa_noIP[,c(1,2,7,3,4,6)]

nasa_noIP<-
  nasa_noIP[order(nasa_noIP$date)]

nasa_tbl_time_date<-as_tbl_time(nasa_noIP, index = date)
nasa_tbl_time_date<-nasa_tbl_time_date[order(nasa_tbl_time_date$date),]


filter_time(nasa_tbl_time, time_formula = '1' ~ '12:00:00')

write.csv(nasa_noIP, "/Users/Jack/Desktop/Nasa_date_time_split.csv")


# Grouping nasa_noIP ------------------------------------------------------

nasa_noIP_grouped <- nasa_noIP %>%
  group_by(host) %>%
  summarise(site_type = first(site_type))


# For automata ------------------------------------------------------------

nasa_noIP_for_automata <-
  nasa_noIP[,c(1:4)]

write.csv(nasa_noIP_for_automata, "/Users/Jack/Documents/GitHub/Log-Data-Feature-Gen/input-automata-csv/nasa_input.csv", row.names = FALSE)

# Visualizations
nasa_noIP %>% ggplot(aes(x=time)) +
  geom_histogram()
