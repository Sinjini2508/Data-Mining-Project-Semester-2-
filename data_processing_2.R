library(data.table)

# This code file focuses on viewing the distribution of categorical columns and deleting the columns that convey no useful information

#import data
df <- read.csv("C:\\Users\\Sinjini\\Documents\\Semester 2\\Data Mining\\California Wildfires Project\\wildfires_updated.csv", header=TRUE, sep=",") 
View(df)

# Viewing the amount of missing data
missing_values <- colSums(is.na(df))
sorted_missing_values <- sort(missing_values, decreasing = TRUE)
View(sorted_missing_values)


# Value Counts for categorical data columns
value_counts_damage <- table(df$damage)
View(value_counts_damage)

value_counts_streetname <- table(df$street_name)
View(value_counts_streetname)

value_counts_streettype <- table(df$street_type)
View(value_counts_streettype)

value_counts_streetsuffix <- table(df$street_suffix)
View(value_counts_streetsuffix)

value_counts_city <- table(df$city)
View(value_counts_city)

value_counts_state <- table(df$state)
View(value_counts_state)

value_counts_calfireunit <- table(df$cal_fire_unit)
View(value_counts_calfireunit)

value_counts_county <- table(df$county)
View(value_counts_county)

value_counts_community <- table(df$community)
View(value_counts_community)

value_counts_battalion <- table(df$battalion)
View(value_counts_battalion)

value_counts_incidentname <- table(df$incident_name)
View(value_counts_incidentname)

value_counts_incidentnumber <- table(df$incident_number)
View(value_counts_incidentnumber)

value_counts_incidentname <- table(df$incident_name)
View(value_counts_incidentname)

value_counts_incidentstartdate <- table(df$incident_start_date)
View(value_counts_incidentstartdate)

value_counts_hazardtype <- table(df$hazard_type)
View(value_counts_hazardtype)

value_counts_ifaffectedwheredidfirestart <- table(df$if_affected_1_9_where_did_fire_start)
View(value_counts_ifaffectedwheredidfirestart)

value_counts_ifaffectedwhatstartedfire <- table(df$if_affected_1_9_what_started_fire)
View(value_counts_ifaffectedwhatstartedfire)

value_counts_defenseactionstaken <- table(df$defense_actions_taken)
View(value_counts_defenseactionstaken)

value_counts_structurecategory <- table(df$structure_category)
View(value_counts_structurecategory)


value_counts_structuretype <- table(df$structure_type)
View(value_counts_structuretype)


value_counts_roofconstruction <- table(df$roof_construction)
View(value_counts_roofconstruction)


value_counts_xeaves <- table(df$x..eaves)
View(value_counts_xeaves)


value_counts_ventscreen <- table(df$vent_screen)
View(value_counts_ventscreen)

value_counts_exteriorsiding <- table(df$exterior_siding)
View(value_counts_exteriorsiding)

value_counts_windowpane <- table(df$window_pane)
View(value_counts_windowpane)

value_counts_deck_porch_on_grade <- table(df$deck_porch_on_grade)
View(value_counts_deck_porch_on_grade)

value_counts_deck_porch_elevated <- table(df$deck_porch_elevated)
View(value_counts_deck_porch_elevated)

value_counts_patio_cover_carport_attached <- table(df$patio_cover_carport_attached)
View(value_counts_patio_cover_carport_attached)

value_counts_fence_attached_to_structure <- table(df$fence_attached_to_structure)
View(value_counts_fence_attached_to_structure)

value_counts_distance_propane_tank <- table(df$distance_propane_tank)
View(value_counts_distance_propane_tank)

value_counts_distance_residence_utility <- table(df$distance_residence_utility)
View(value_counts_distance_residence_utility)

value_counts_fire_name_secondary <- table(df$fire_name_secondary)
View(value_counts_fire_name_secondary)


# Deleting certain columns

remove_cols <- c('id', 'objectid', 'state', 'community', 'battalion', 'incident_name', 'incident_number', 'hazard_type', 'globalid', 'site_address_parcel', 'APN_parcel', 'year_built_parcel', 'distance_propane_tank', 'distance_residence_utility', 'street_number', 'street_suffix', 'zip_code', 'incident_start_date')

new_df = subset(df, select = !(names(df) %in% remove_cols)) 

cleaned_file_path <- "cleaned_dataset.csv"
fwrite(new_df, cleaned_file_path)

