# This code file focuses on performing exploratory data analysis to gather insights from the data


library(sf)
library(ggplot2)
library(tidyverse)
library(mapview)
library(RColorBrewer)
library(leaflet.extras2) 
library(lubridate)
library(corrplot)






new_df <- read.csv("C:\\Users\\Sinjini\\Documents\\Semester 2\\Data Mining\\California Wildfires Project\\wildfires_updated.csv", header=TRUE, sep=",") 

View(new_df)



new_sf <- st_as_sf(new_df, coords = c("longitude", "latitude"), crs = 4326)  # WGS84 projection


new_sf$damage <- factor(new_sf$damage, 
                        levels = c("No Damage", "Affected (1-9%)", "Destroyed (>50%)", 
                                   "Inaccessible", "Major (26-50%)", "Minor (10-25%)"))

# Define colors
category_colors <- c("gray", "lightgreen", "red", "blue", "orange", "yellow")

# Plot the combined map 
mapview(new_sf, 
        zcol = "damage", 
        layer.name = "Damage Categories Combined",
        col.regions = category_colors)







# Plot the map for "No Damage" category
mapview(sf_nodamage, 
        layer.name = "Damage Category: No Damage", 
        col.regions = "gray")

# Plot the map for "Affected (1-9%)" category
mapview(sf_affected, 
        layer.name = "Damage Category: Affected (1-9%)", 
        col.regions = "lightgreen")

# Plot the map for "Destroyed (>50%)" category
mapview(sf_destroyed, 
        layer.name = "Damage Category: Destroyed (>50%)", 
        col.regions = "red")

# Plot the map for "Inaccessible" category 
mapview(sf_inaccessible, 
        layer.name = "Damage Category: Inaccessible", 
        col.regions = "blue")

# Plot the map for "Major (26-50%)" category
mapview(sf_major, 
        layer.name = "Damage Category: Major (26-50%)", 
        col.regions = "orange")

# Plot the map for "Minor (10-25%)" category
mapview(sf_minor, 
        layer.name = "Damage Category: Minor (10-25%)", 
        col.regions = "yellow")






# Top 10 counties with most damage
top_counties <- new_df %>%
  group_by(county) %>%
  summarise(damage_count = n()) %>%
  arrange(desc(damage_count)) %>%  
  slice_head(n = 10) %>%           
  pull(county)  

county_damage <- new_df %>%
  filter(county %in% top_counties) %>%  
  group_by(county) %>%
  summarise(damage_count = n())  


ggplot(county_damage, aes(x = reorder(county, damage_count), y = damage_count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Total Damage Count by County", x = "County", y = "Number of Damages") +
  coord_flip() +
  theme_minimal()


# Distribution of damage types
ggplot(new_df, aes(x = damage, fill = damage)) +
  geom_bar() +
  scale_fill_manual(values = c("No Damage" = "gray", 
                               "Affected (1-9%)" = "lightgreen", 
                               "Destroyed (>50%)" = "red", 
                               "Inaccessible" = "blue", 
                               "Major (26-50%)" = "orange", 
                               "Minor (10-25%)" = "yellow")) +
  labs(title = "Distribution of Damage Types", x = "Damage Category", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#How has the number of fire incidents changed over time?
new_df <- new_df %>%
  mutate(incident_start_date = mdy_hms(incident_start_date))


new_df$incident_start_date <- as.POSIXct(new_df$incident_start_date, format = "%m/%d/%Y %I:%M:%S %p")

str(new_df$incident_start_date)
head(new_df$incident_start_date)

new_df$Year <- year(new_df$incident_start_date)

incident_counts_by_year <- new_df %>%
  count(Year) %>%
  arrange(Year)


ggplot(incident_counts_by_year, aes(x = Year, y = n)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Number of Fire Incidents Over Time",
       x = "Year",
       y = "Number of Incidents") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(incident_counts_by_year$Year, na.rm = TRUE), 
                                  max(incident_counts_by_year$Year, na.rm = TRUE), by = 1))
 scale_x_continuous(breaks = seq(min(incident_counts_by_year$Year, na.rm = TRUE), 
                                  max(incident_counts_by_year$Year, na.rm = TRUE), by = 1))

 
# Is there a seasonal dependence? 

new_df$Month <- factor(month(new_df$incident_start_date, label = TRUE, abbr = FALSE), 
                        levels = rev(month.name))  
 
incident_counts_by_month <- new_df %>% count(Month)
 
ggplot(incident_counts_by_month, aes(x = n, y = Month)) +
   geom_bar(stat = "identity", fill = "orange") +
   labs(title = "Number of Fire Incidents by Month",
        x = "Number of Incidents",
        y = "Month") +
   theme_minimal()
 


# Which structure types are most affected?
damage_counts <- new_df %>%
  count(structure_type, sort = TRUE) %>%
  top_n(10, n)

ggplot(damage_counts, aes(x = n, y = reorder(structure_type, n))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Structure Types Most Affected by Fires",
       x = "Number of Fire Incidents",
       y = "Structure Type") +
  theme_minimal()

damage_counts <- new_df %>%
  count(structure_category, sort = TRUE) %>%
  top_n(10, n)

ggplot(damage_counts, aes(x = n, y = reorder(structure_category, n))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 10 Structure Categories Most Affected by Fires",
       x = "Number of Fire Incidents",
       y = "Structure Category") +
  theme_minimal()










# Which counties suffered the most economic loss?

new_df$assessed_improved_value_parcel <- as.numeric(new_df$assessed_improved_value_parcel)


new_df <- new_df%>% filter(!is.na('assessed_improved_value_parcel'))

new_df$assessed_improved_value_parcel <- as.numeric(gsub("[^0-9.-]", "", new_df$assessed_improved_value_parcel))




economic_loss_by_county <- new_df %>%
  group_by(county) %>%
  summarise(total_loss = sum(assessed_improved_value_parcel)) %>%
  arrange(desc(total_loss)) %>%
  slice_head(n = 10)



ggplot(economic_loss_by_county, aes(x = reorder(county, -total_loss), y = total_loss)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Top 10 Counties with the Most Economic Loss",
    x = "County",
    y = "Total Assessed Property Value Loss"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Which fire unit responded to the highest number of incidents

fire_unit_counts <- new_df %>%
  group_by(cal_fire_unit) %>%
  summarise(incident_count = n()) %>%
  arrange(desc(incident_count)) %>%
  slice_head(n = 10)

ggplot(fire_unit_counts, aes(x = reorder(cal_fire_unit, -incident_count), y = incident_count)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Top 10 CAL FIRE Units by Number of Incidents",
    x = "CAL FIRE Unit",
    y = "Number of Incidents"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Property value and damage

agg_data <- new_df %>%
  filter(county %in% top_counties) %>%
  group_by(damage) %>%
  summarise(avg_assessed_value = mean(assessed_improved_value_parcel, na.rm = TRUE))

ggplot(agg_data, aes(x = damage, y = avg_assessed_value, fill = avg_assessed_value)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colors = c("blue", "red")) +  # Equivalent to "coolwarm" in seaborn
  labs(title = "Average Assessed Improved Value by Damage Type (Top Counties)",
       x = "Damage Type",
       y = "Average Assessed Improved Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# Correlation between numerical features
numeric_cols <- new_df %>% select(where(is.numeric))

numeric_cols <- na.omit(numeric_cols)

corr_matrix <- cor(numeric_cols, use = "complete.obs")

short_names <- c("unitsinstructure", "damagedoutbuildings", "nondamagedoutbuildings", "assessedpropertyvalue", "latitude", "longitude", "X", "Y", "year")
colnames(corr_matrix) <- short_names
rownames(corr_matrix) <- short_names


corrplot(corr_matrix, method = "color", col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.col = "black", tl.srt = 90, tl.cex = 0.7, cl.align.text = "l",
         number.cex = 0.6,  addCoef.col = "black", cl.cex = 0.7, mar = c(1,1,1,1), addgrid.col = "black")


# Damage and defense actions taken

new_df <- new_df %>%
  mutate(defense_actions_taken = ifelse(trimws(defense_actions_taken) == "", "Unknown", defense_actions_taken))


new_df <- new_df %>%
  mutate(defense_actions_taken = ifelse(trimws(defense_actions_taken) == "Both", "Combination of Actions", defense_actions_taken))


ggplot(new_df, aes(x = damage, fill = defense_actions_taken)) +
  geom_bar(position = "dodge") +  
  ggtitle("Damage Distribution by Defense Actions") +
  xlab("Damage") +
  ylab("Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(size = 14),
        axis.title = element_text(size = 12)) +
  scale_fill_brewer(palette = "Paired")


