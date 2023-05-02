## Import libraries
library(tidyverse)
library(dplyr)
library(ggmap)
library(mapview)
library(scales)
options(scipen=999)

## load the meter read data
meterreads <- read.csv("MeterReads.csv")
View(meterreads)

## summarise the meters to find average monthly consumption, based on the
## number of monthly reads in the data for the meters
meterread_avgs <- meterreads %>%
  group_by(meterno, zoning_desc) %>%
  summarise(avg_meter_consumption = mean(consumption), n_meter_reads = n())
View(meterread_avgs)

meterread_plot <- ggplot(meterread_avgs, aes(x=zoning_desc, y=avg_meter_consumption)) + geom_boxplot() + geom_jitter()
meterread_plot

## Find the outlier by it's meter number
outlier_meter <- meterread_avgs %>%
  filter(avg_meter_consumption > 2500000) %>%
  pull(meterno)
#View(outlier_meter)

## Find the address associated with the meter
outlier_address <- meterreads %>%
  filter(meterno == outlier_meter) %>%
  group_by(address) %>%
  pull(address)
#View(outlier_address)


## Map the outlier on a map:
map_view <- mutate_geocode(head(as.data.frame(outlier_address),1), location = outlier_address, output = "latlona")
View(map_view)

## Here is a dummy data.frame containing the lat/long, should the above API call fail:
#map_view <- data.frame(outlier_address = c("1400 W MARS HLL RD"),
#                       lon = c(-111.6648),
#                       lat = c(35.20196))

## Plot the map:
mapview(map_view, xcol="lon",ycol="lat", crs=4269, grid=FALSE)




## Filter the outlier and plot again:
meterread_avgs_filter <- meterread_avgs %>%
  filter(meterno != outlier_meter)

## Create a plot now that the outlier has been filtered out:
meterread_plot_filter <- ggplot(meterread_avgs_filter, aes(x=zoning_desc, y=avg_meter_consumption)) + geom_boxplot() + geom_jitter()
meterread_plot_filter



## Filter one more time to get a better idea of values, with outliers highlighted:
meterread_plot_filter_under_500000 <- ggplot(meterread_avgs_filter, aes(x=zoning_desc, y=avg_meter_consumption)) +
  geom_boxplot(outlier.shape = 21, outlier.color = "red")  +
  ylim(0,500000) +
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2))
meterread_plot_filter_under_500000




## Get count per zoning category
zoning_count <- meterread_avgs_filter %>%
  group_by(zoning_desc) %>%
  count(zoning_desc) %>%
  arrange(n)
View(zoning_count)

## Find the zoning categories that have fewer than 10 occurrences, or are blank.
zoning_excludes <- zoning_count %>%
  filter(n < 10 | zoning_desc == '') %>%
  pull("zoning_desc")
View(zoning_excludes)



## Filter out possible entry errors to get a good plot and final dataset.
meterread_zoning_filter <- meterread_avgs_filter %>%
  filter(!zoning_desc %in% zoning_excludes)
View(meterread_zoning_filter)



## Plot without the empty and low count zoning categories, and set the scale
## appropriately
meterread_zoning_filter_plot <- ggplot(meterread_zoning_filter, aes(x=zoning_desc, y=avg_meter_consumption)) +
  geom_boxplot(outlier.shape = 21, outlier.color = "red")  +
  ylim(0,10000) +
  theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
  labs(y= "Average Monthly Meter Consumption (gal)", x = "Zoning Description")
meterread_zoning_filter_plot



## Find mean and median meter consumption per zoning type
zoning_average <- meterread_zoning_filter %>%
  group_by(zoning_desc) %>%
  summarize(avg_month_zoning_consumption_by_zoning = mean(avg_meter_consumption),
            median_month_meter_consumption_by_zoning = median(avg_meter_consumption),
            n_meters_per_zone = n())
View(zoning_average)


zoning_categories <- data.frame(zoning_desc = c("Central Business",
                                       "Commercial Service",
                                       "Community Commercial",
                                       "Estate Residential",
                                       "Heavy Industrial",
                                       "High Density Residential",
                                       "Highway Commercial",
                                       "Light Industrial",
                                       "Light Industrial Open",
                                       "Manufactured Housing",
                                       "Medium Density Residential",
                                       "Public Facility",
                                       "Research and Development",
                                       "Rural Residential",
                                       "Single-family Residential",
                                       "Single-family Residential Neighborhood",
                                       "Suburban Commercial",
                                       "Transect Zone"),
                       zoning_category = c("Commercial",
                                           "Commercial",
                                           "Commercial",
                                           "Residential",
                                           "Industrial",
                                           "Residential",
                                           "Commercial",
                                           "Industrial",
                                           "Industrial",
                                           "Residential",
                                           "Residential",
                                           "Public and Open Space",
                                           "Commercial",
                                           "Residential",
                                           "Residential",
                                           "Residential",
                                           "Commercial",
                                           "Transect")
)
#View(zoning_categories)


## Join zoning categories to the zoning description
zoning_joined <- zoning_average %>%
  left_join(zoning_categories, by=join_by(zoning_desc)) %>%
  arrange(zoning_category)
View(zoning_joined)


## Plot and sort by zoning category
category_plot <- ggplot(data=zoning_joined, aes(x=fct_reorder(zoning_desc, zoning_category),
                                                y=avg_month_zoning_consumption_by_zoning, fill=zoning_category)) +
                        geom_bar(stat="identity")  +
                        ylim(0,60000) +
                        theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
                        labs(y= "Average Monthly Meter Consumption (gal)", x = "Zoning Description", fill="Zoning Category" )
category_plot


## Filter only to residential zones
residential_zones <- zoning_joined %>%
  filter(zoning_category == "Residential")
View(residential_zones)


## plot the residential zones
residential_plot <- ggplot(data=residential_zones, aes(x=reorder(zoning_desc, avg_month_zoning_consumption_by_zoning),
                                                y=avg_month_zoning_consumption_by_zoning, fill=zoning_category)) +
                          geom_bar(stat="identity")  +
                          ylim(0,10000) +
                          theme(axis.text.x = element_text(angle = 90, hjust=0.95,vjust=0.2)) +
                          labs(y= "Average Monthly Meter Consumption (gal)", x = "Zoning Description", fill="Zoning Category" )
residential_plot


## Separate out only the Rural Residential and Single-family Residential to compare
meterread_SFR_RR_avgs <- meterreads %>%
  group_by(meterno, zoning_desc) %>%
  summarise(avg_meter_consumption = mean(consumption), n_meter_reads = n()) %>%
  filter(zoning_desc == "Single-family Residential" | zoning_desc == "Rural Residential")
View(meterread_SFR_RR_avgs)


library(ggpubr)
library(rstatix)
# Add p-value and significance levels
SFR_values <- meterread_SFR_RR_avgs %>%
  filter(zoning_desc == "Single-family Residential") %>%
  pull(avg_meter_consumption)
SFR_values
RR_values  <- meterread_SFR_RR_avgs %>%
  filter(zoning_desc == "Rural Residential") %>%
  pull(avg_meter_consumption)
RR_values


# Perform the T-Test
stat_test <- t.test(SFR_values, RR_values)
stat_test

## Create boxplot to compare Rural Residential and Single Family Residential
boxplot <- ggboxplot(
  meterread_SFR_RR_avgs, x = "zoning_desc", y = "avg_meter_consumption", 
  ylab = "avg_meter_consumption", xlab = "zoning_desc", add = "jitter") +
  coord_cartesian(ylim = c(0, 15000)) +
  labs(y= "Average Monthly Meter Consumption", x = "Zoning Description") + 
  stat_compare_means(method = "t.test", label.x = 1.375, label.y = -500)

boxplot
