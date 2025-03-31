library("dplyr")
# This whole file is just for renaming columns, nothing deeper. 

#import data
df <- read.csv("C:\\Users\\Sinjini\\Documents\\Semester 2\\Data Mining\\California Wildfires Project\\wildfires.csv", header=TRUE, sep=",") 

#view structure of data
View(df)
dim(df)

# Checking the data type of each column (feature)
View(sapply(df, class))

# Renaming the columns
colnames(df) <- tolower(colnames(df))
colnames(df)

names(df)[1] <- "id"
names(df)
names(df)[3] <- "damage"
names(df)[4] <- "street_number"
names(df)[5] <- "street_name"
names(df)[6] <- "steet_type"
names(df)[7] <- "street_suffix"
names(df)[8] <- "city"
names(df)[10] <- "zip_code"
names(df)[11] <- "cal_fire_unit"
names(df)[15] <- "incident_name"
names(df)[16] <- "incident_number"
names(df)[17] <- "incident_start_date"
names(df)[18] <- "hazard_type"
names(df)[19] <- "if_affected_1_9_where_did_fire_start"
names(df)[20] <- "if_affected_1_9_what_started_fire"
names(df)[21] <- "defense_actions_taken"
names(df)[22] <- "structure_type"
names(df)[23] <- "structure_category"
names(df)[24] <- "number_of_units_in_structure_if_multi_unit"
names(df)[25] <- "number_of_damaged_outbuildings_120_ft"
names(df)[26] <- "number_of_nondamaged_outbuildings_120_ft"
names(df)[27] <- "roof_construction"
names(df)[29] <- "vent_screen"
names(df)[30] <- "exterior_siding"
names(df)[31] <- "window_pane"
names(df)[32] <- "deck_porch_on_grade"
names(df)[33] <- "deck_porch_elevated"
names(df)[34] <- "patio_cover_carport_attached"
names(df)[35] <- "fence_attached_to_structure"
names(df)[36] <- "distance_propane_tank"
names(df)[37] <- "distance_residence_utility"
names(df)[38] <- "fire_name_secondary"
names(df)[39] <- "APN_parcel"
names(df)[40] <- "assessed_improved_value_parcel"
names(df)[41] <- "year_built_parcel"
names(df)[42] <- "site_address_parcel"


# writing the updates column names to a new dataset
write.csv(df, "C:\\Users\\Sinjini\\Documents\\Semester 2\\Data Mining\\California Wildfires Project\\wildfires_updated.csv", row.names=FALSE)
