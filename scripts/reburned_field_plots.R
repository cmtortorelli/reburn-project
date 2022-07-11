
# title: "reburned_field_plots"
# date: '2022-07-11'


#### Purpose: Compile existing plot data from multiple data sources; extract data for evaluating plots such as management history, disturbance history.


library(plyr)
library(tidyverse)
library(here)
library(sf)
library(readxl)
library(terra)


### Setup: define a function `datadir()` that points to your local data directory 
# The root of the data directory
data_dir = readLines(here("data_dir.txt"), n=1)
source(here("scripts/convenience_functions.R"))

#### Load plot data

#### Permanently-marked (rebarred) plots:

##Main CSE database compiled by Clark Richter
richter_plots = read_excel(datadir("raw-data/CSEs/richter-db-export/Plot Data.xlsx"))

#add additional powers plots with UTMs instead of Lat Long coords
power_plots = read_excel(datadir("raw-data/CSEs/richter-additional/power/Plot Data Total.xlsx"))

#change plot ID to match other PWR plots
power_plots_cor = power_plots %>%
  #to match the plot ID from PWR tables to the Plot_ID column from the compiled regen data, prefix them with PWR1400[…]. You need to pad the number of zeros so there are 7 digits total. 
  #If the plot number has a suffix letter like A or B, this does not count in the 7 digits.
  mutate(PWR = "PWR14") %>% #add column with prefix for merging
  separate(`Plot ID`, c("Plot_IDnum","Plot_IDletter"), "(?<=[0-9])(?=[A-Z])") %>% #separate letters from numbers
  mutate(Plot_IDletter = replace_na(Plot_IDletter, "")) %>% #replace NAs with blanks
  mutate(Plot_IDpad = str_pad(Plot_IDnum, 5, side = 'left', pad = '0')) %>% #pad with 0s to 5 digits
  unite("Plot ID", c("PWR", "Plot_IDpad", "Plot_IDletter"), remove = TRUE, sep = "") %>%
  mutate(FIRE_ID = "PWR") %>%
  subset(Sample_Year == 2015) %>%
  select(-(c(Plot_IDnum)))

#replace PWR lat-long values in richter_plots with UTM values from power_plots (power plots sampled in 2015)
richter_plots_cor = richter_plots %>%
  filter(!(FIRE_ID == "PWR" & Sample_Year == 2015)) %>%
  rbind.fill(., power_plots_cor)

# replace FRD coordinats with corrected coordinates

#read in corrected FRDs coordinats - NAD83 UTM zone 10
freds_coords_cor = read_excel(datadir("raw-data/CSEs/richter-additional/freds/2012+2013_CSEPlots_withCorrectedandUncorrectedUTM.xlsx")) %>%
  select(c(Plot_ID, Easting, Northing, Status)) %>%
  mutate(Plot_ID = str_replace_all(Plot_ID, "FRN", "FRD")) %>%
  rename(`Plot ID` = Plot_ID)

freds_plots_cor = richter_plots_cor %>%
  filter(FIRE_ID == "FRD") %>%
  mutate(`Plot ID` = str_replace_all(`Plot ID`, "FRD_12_", "FRD1200")) %>% #change plot ID format to match coordinates
  mutate(`Plot ID` = str_replace_all(`Plot ID`, "1200356", "120356")) %>% #change plot ID to match coordinates
  select(-c(Easting, Northing)) %>%
  left_join(., freds_coords_cor) %>% #replace with corrected easting and northing for 2012. 2013 may not have needed correcting
  relocate(c(Easting, Northing), .after = Observers) #reorder columns for binding
  
#replace FRD in richter database with updated FRD plot data
richter_plots_cor = richter_plots_cor %>%
  filter(FIRE_ID != "FRD") %>%
  bind_rows(., freds_plots_cor)

#TODO: To get Bassetts data: use CSE plot list from Richter tables, merged with ?? for plot locs (originally thought welch DB but those seem to be different plots--waiting for Kevin to respond on this.)

# Pendola fire plots from Richter tables
pendola_plots = read_excel(datadir("raw-data/CSEs/richter-additional/pendola/Plot_Data.xlsx"))

# gondola and showers fires from hugh. Fires burned 2002. Initial survey 08-09. Revisit 2019.
gond_show_plots = read_excel(datadir("raw-data/CSEs/showers-gondola/access-export/Plot_data.xlsx"))


#### Non-rebarred (non-CSE) plots
## Welch and Young data
welch_young_plots = read_csv(datadir("raw-data/non-CSEs/young-welch-summarized/plot_level_pub.csv"))

## Young/Latimer JFSP data
jfsp_plots = read_excel(datadir("raw-data/non-CSEs/latimer-young-summarized/Data_template_DJNY-3.xlsx"),sheet = 3)

## Cleveland CSE data
clev_plots = read_excel(datadir("raw-data/CSEs/Cleveland/ClevelandFire_NoTreatment.xlsx"),sheet = "Plot_Data")


#### Merge the relevant plot sets:
## Richter DB: prep the right columns in the right format
richter_premerge = richter_plots_cor %>%
  mutate(survey_year = str_sub(Date,1,4)) %>%
  mutate(fire_sev = recode(FIRE_SEV, "Control" = "0", "Low" = "1", "Med" = "3", "High" = "5", "Unburned" = "0") %>% as.numeric) %>%
  select(FIRE_ID, Plot_ID = `Plot ID`, Sample_Year, Easting, Northing, fire_sev, NOTES) #there's only notes available for MNL and FRD

# ## Need to pull in the FRD 2013 surveys coords, which are missing from the main Richter CSE database - *did this above with updated coordinates!
# frd_missing_coords = read_excel(datadir("CSEs/richter-additional/freds/CSE_Missing_Coordinates_CJR.xlsx")) %>%
#   rename("Plot_ID" = `Plot ID`, "Easting_missing" = "Easting", "Northing_missing" = "Northing")

richter_premerge_2 = richter_premerge %>%
  # left_join(richter_premerge,frd_missing_coords,by="Plot_ID") %>%
  # mutate(Easting = ifelse(is.na(Easting), Easting_missing, Easting),
  #        Northing = ifelse(is.na(Northing), Northing_missing, Northing)) %>%
  # select(-Easting_missing, -Northing_missing) %>%
  mutate(permanent = "Permanent", revisited = "Not-revisited", source = "richter-cse-db")

## Mark the Angora Fire plots as revisited
richter_premerge_2[richter_premerge_2$FIRE_ID == "ANG", "revisited"] = "Revisited"


## Need to pull in the FRD 2012 fire sevs (they are missing from the main Richter CSE database)
# First fix the Frd 2012 plot ID formatting: FRD_12_395 -> FRD120395 
richter_premerge_2 = richter_premerge_2 %>%
  mutate(Plot_ID = str_replace(Plot_ID,pattern = fixed("FRD120"),replacement = "FRD12")) %>%
  mutate(Plot_ID = str_replace_all(Plot_ID, "12356", "120356")) #change plot ID to match fire severity

frd_missing_sevs = read_excel(datadir("raw-data/CSEs/richter-additional/freds/FredsCSE_DataEntryFinal_2012+2013.xlsx")) %>%
  select(Plot_ID = "Plot ID", fire_sev_missing = FIRE_SEV)

richter_premerge_3 =
  left_join(richter_premerge_2,frd_missing_sevs,by="Plot_ID") %>%
  mutate(fire_sev = ifelse(is.na(fire_sev), fire_sev_missing, fire_sev) %>% as.numeric) %>%
  select(-fire_sev_missing)


##Pendola Fire data from Clark Richter: prep the right columns in the right format
pendola_premerge = pendola_plots %>%
  mutate(Sample_Year = str_sub(Date,1,4) %>% as.numeric,
         FIRE_ID = str_sub(Regen_Plot,1,3)) %>%
  select(FIRE_ID, Plot_ID = Regen_Plot, Sample_Year, Easting, Northing, fire_sev = FIRE_SEV, NOTES) %>%
  mutate(permanent = "Permanent", revisited = "Not-revisited", source="richter-cse-additional")

## Filter to just those Pendola plots that are in the Richter sheets "Species Composition" -- to make sure that we only sample the CSEs since there were also standalone regen-only plots that were not rebarred.
species_plumas_2010 = read_excel(datadir("raw-data/CSEs/richter-additional/pendola/Pendola_Species_Composition.xlsx"),sheet="PNF_2010") %>%
  rename(PlotID = "Plot ID") %>%
  mutate(PlotID_full = paste0("PNP10",str_pad(PlotID,5,side="left",pad="0")))
species_plumas_2011 = read_excel(datadir("raw-data/CSEs/richter-additional/pendola/Pendola_Species_Composition.xlsx"),sheet="PNF_2011") %>%
  rename(PlotID = "Plot ID") %>%
  mutate(PlotID_full = paste0("PNP111",str_pad(PlotID,4,side="left",pad="0")))
species_tahoe_2010 = read_excel(datadir("raw-data/CSEs/richter-additional/pendola/Pendola_Species_Composition.xlsx"),sheet="TNF_2010") %>%
  rename(PlotID = "Plot ID") %>%
  mutate(PlotID_full = paste0("PNT10",str_pad(PlotID,5,side="left",pad="0")))
species_tahoe_2011 = read_excel(datadir("raw-data/CSEs/richter-additional/pendola/Pendola_Species_Composition.xlsx"),sheet="TNF_2011") %>%
  rename(PlotID = "Plot ID") %>%
  mutate(PlotID_full = paste0("PNT11",str_pad(PlotID,5,side="left",pad="0")))
pendola_species = bind_rows(species_plumas_2010,
                            species_plumas_2011,
                            species_tahoe_2010,
                            species_tahoe_2011)

pendola_premerge = pendola_premerge %>%
  filter(Plot_ID %in% pendola_species$PlotID_full)


## Showers & Gondola: prep the right columns in the right format
gond_show_premerge = gond_show_plots %>%
  mutate(FIRE_ID = str_sub(Regen_Plot_Mast,1,3)) %>%
  filter(Year <= 2009) %>% # keep only those plots sampled in 08 and 09 (bc DB also includes plots resampled at a later date, but don't want those to look like additional plots)
  select(FIRE_ID, Plot_ID = Regen_Plot_Mast, Sample_Year = Year, Easting, Northing, fire_sev = FIRE_SEV, NOTES)

## Pull in missing Showers plots locs

show_missing_coords = read_excel(datadir("raw-data/CSEs/showers-gondola/PlotLocations_Gondola&Showers.xlsx"),sheet=2) %>%
  select(Plot_ID = Object_ID, Northing_missing = Northing, Easting_missing = Easting)

gond_show_premerge_2 = gond_show_premerge %>%
  left_join(show_missing_coords, by="Plot_ID") %>%
  mutate(Easting = ifelse(is.na(Easting), Easting_missing, Easting) %>% as.numeric,
         Northing = ifelse(is.na(Northing), Northing_missing, Northing) %>% as.numeric) %>%
  select(-Easting_missing, -Northing_missing) %>%
  mutate(permanent = "Permanent", revisited = "Revisited", source = "safford-gon-show-cse")


## Welch-Young (flag that non-CSE): prep the right columns in the right format

welch_young_premerge = welch_young_plots %>%
  mutate(FIRE_ID = str_sub(Regen_Plot,1,3)) %>%
  select(FIRE_ID,Plot_ID = Regen_Plot, Sample_Year = Year, Easting, Northing, fire_sev = FIRE_SEV, fire_year = Year.of.Fire) %>% #no notes or comments column
  mutate(permanent = "Non-permanent", revisited = "Not-revisited", source = "welch-young-regen")

## Latimer-Young (flag that non-CSE): prep the right columns in the right format

jfsp_premerge = jfsp_plots %>%
  mutate(FIRE_ID = str_sub(plot_id,1,1)) %>%
  select(FIRE_ID, Plot_ID = plot_id, Sample_Year = sample_year, longitude, latitude, fire_year) %>% #no notes or comments column
  mutate(fire_sev = 5, permanent = "Non-permanent", revisited = "Not-revisited", source = "latimer-young-jfsp")

## Cleveland plots - clean up for merging - change col names
clev_premerge = clev_plots %>%
  mutate(FIRE_ID = "CLV", Plot_ID = `Plot #`, Sample_Year = 2013, 
         Easting = UTM_Easting, Northing = UTM_Northing, fire_sev = BurnSeverity,
         permanent = "Permanent", revisited = "Not-revisited", 
         source = "Gabrielle N. Bohlman", fire_year = 1992) %>%
  mutate(Plot_ID = as.character(Plot_ID)) %>%
  select(FIRE_ID, Plot_ID, Sample_Year, Easting, Northing, fire_sev, 
      permanent, revisited, source, fire_year, NOTES = Notes) 


## Merge everything that's in UTMs and convert to lat-long

plots_merged_utm = bind_rows(richter_premerge_3, pendola_premerge, gond_show_premerge_2, welch_young_premerge, clev_premerge)

#! Some plots have weird coords (all Star fire plots and some Power). These are in lat/long: e.g., 1203931 means 120 deg, 39" 31" W. But the precision is too low (1 second of lat/long = ~30 m)
# Only keep those plots that have coords that make sense
plots_merged_utm = plots_merged_utm %>%
  filter(between(Easting,580000,1210000),
         between(Northing,1e06,10000000)) # this ends up throwing out all STA plots

# All these plots appear to be in UTM 10N
# Make them into a spatial object (points)
plots_10n_sf = st_as_sf(plots_merged_utm,coords = c("Easting","Northing"), crs = 26910)

# Project to lat/long and extract coords
plots_sf_latlong = st_transform(plots_10n_sf, 4326)
coords = st_coordinates(plots_sf_latlong)
plots_sf_latlong$longitude = coords[,1]
plots_sf_latlong$latitude = coords[,2]
st_geometry(plots_sf_latlong) = NULL

# Merge these plots with the latimer-young-jfsp plots, which were already in lat/long
plots = bind_rows(plots_sf_latlong, jfsp_premerge)

# what fire years need to be populated?
table(plots$FIRE_ID, plots$fire_year)

## Populate missing fire years
plots = plots %>%
  mutate(fire_year_missing = recode(FIRE_ID, ANG = 2007, GON = 2002, MNL = 2007, PNP = 1999, PNT = 1999, SHR = 2002, FRD = 2004, PWR = 2004, RCH = 2008, .default = 0)) %>%
  mutate(fire_year = ifelse(is.na(fire_year), fire_year_missing, fire_year)) %>%
  select(-fire_year_missing)


#### Filter out plots that burned after the survey

# Load fire permi database. It only goes through 2020
fire_perims = vect(datadir("raw-data/fire-perims/fire20_1.gdb"))
fire_perims$YEAR_ = as.numeric(fire_perims$YEAR_)
fire_perims = fire_perims[fire_perims$YEAR_ > 1991,]

# Rasterize it for easy extraction. When two fires overlap, keep the year of the most recent
r = rast(fire_perims,resolution=50,crs="+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
mostrecent_fire = rasterize(fire_perims,r, field="YEAR_", fun=max)


## Bring in the important 2021 fires (Caldor and Dixie), rasterize them too
caldor = vect(datadir("raw-data/fire-perims/ca3858612053820210815_20201011_20211016_ravg_data/ca3858612053820210815_20201011_20211016/CA3858612053820210815.kml"))
caldor = project(caldor,r)
caldor$year = 2021
caldor_rast = rasterize(caldor,r,field="year", fun=max)
dixie = vect(datadir("raw-data/fire-perims/ca3987612137920210714_20201012_20211015_ravg_data/ca3987612137920210714_20201012_20211015/CA3987612137920210714.kml"))
dixie = project(dixie,r)
dixie$year = 2021
dixie_rast = rasterize(dixie,r,field="year", fun=max)

## Stack the fires and get the most recent year of fire for each pixel, accounting for 2021 fires too
fire_hist = c(mostrecent_fire,caldor_rast,dixie_rast)
fire_hist = max(fire_hist, na.rm=TRUE)

# temp export for checking
# writeRaster(fire_hist,datadir("temp/mostrecent_fire.tif"), overwrite=TRUE)

## Extract the year of most recent fire at each plot
plots_sf = st_as_sf(plots,coords = c("longitude","latitude"), crs = 4326) %>% st_transform(3310)
plots_vect = vect(plots_sf)
mostrecent_fire = extract(fire_hist,plots_vect)
plots_sf$mostrecent_fire = mostrecent_fire[,2]

# label reburned plots - Did plots burn after the initial fire?
plots_sf$reburned = plots_sf$fire_year < plots_sf$mostrecent_fire ## new coordinates for PWR do not fall within the PWR burn perim? mostrecent_fire = NaN

#plot pwr and utms to check locations

# plot(plots_sf[190:199,1], col = 'red')
# plot(fire_perims[fire_perims$FIRE_NAME == 'POWER' & fire_perims$YEAR_ == 2004,], add = TRUE)


##!! Exclude non-reburned plots
plots_sf = plots_sf %>%
  filter(reburned == TRUE)

# temp export for checking
# st_write(plots_sf,datadir("temp/plots.gpkg"), delete_dsn=TRUE)


#### Add management data

## Load FACTS (USFS forest management database)
facts = st_read(datadir("facts/compiled/facts_merged.gpkg"))

## Check for reporting discrepancies in reported size vs. actual size
# remove those units where reported and GIS acres do not match. Check either NBR_UNTIS or SUBUNIT_S and allow either to match
# making sure GIS acres is not much larger than reported acres
# Doing this because have found that some units cover far more area spatially than were really reported to be treated
facts <- facts %>%
  mutate( reporting_discrepancy = ! (((((NBR_UNITS_ > (GIS_ACRES*.75)) ) | ((NBR_UNITS_ > (GIS_ACRES - 25)) ))) |
                                       ((((SUBUNIT_SI > (GIS_ACRES*.75)) ) | ((SUBUNIT_SI > (GIS_ACRES - 25)) )))) )

# If completed date is blank, use awarded date. otherwise keep completed date as it was.
facts$DATE_COMPOSITE <- ifelse(is.na(facts$DATE_COMPL),facts$DATE_AWARD %>% as.character,facts$DATE_COMPL %>% as.character)
facts$year <- as.numeric(substr(facts$DATE_COMPOSITE,1,4))
# Remove management that was not actually performed (e.g., just planned)
facts <- facts[!is.na(facts$year),]

# Drop units managed before any of the focal fires (to make the dataset smaller and easier to work with)
facts <- facts[facts$year >= min(plots_sf$fire_year %>% as.numeric),]

# Defin the different management types that we care about
planting <- c("Plant Trees")
salvage <- c("Stand Clearcut (w/ leave trees) (EA/RH/FH)", "Salvage Cut (intermediate treatment, not regeneration)","Stand Clearcut (EA/RH/FH)","Patch Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)","Shelterwood Removal Cut (EA/NRH/FH)", "Shelterwood Preparatory Cut (EA/NRH/NFH)", "Improvement Cut", "Shelterwood Establishment Cut (with or without leave trees) (EA/RH/NFH)", "Seed-tree Final Cut (EA/NRH/FH)")
prep <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging","Site Preparation for Planting - Mechanical","Site Preparation for Planting - Manual","Site Preparation for Planting - Burning","Site Preparation for Planting - Other","Site Preparation for Natural Regeneration - Manual","Site Preparation for Natural Regeneration - Burning","Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels", "Slashing - Pre-Site Preparation")
release <- c("Tree Release and Weed","Control of Understory Vegetation")
thin <- c("Precommercial Thin","Commercial Thin","Thinning for Hazardous Fuels Reduction","Single-tree Selection Cut (UA/RH/FH)")
replant <- c("Fill-in or Replant Trees")
prune <- c("Pruning to Raise Canopy Height and Discourage Crown Fire","Prune")
fuel <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging","Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels","Underburn - Low Intensity (Majority of Unit)","Broadcast Burning - Covers a majority of the unit")

manage <- c(planting,salvage,prep,release,thin,replant,prune,fuel)

# Filter to only the management types we care about
facts = facts %>%
  filter(ACTIVITY %in% manage)

# Extract FACTS history at each plot (this is just a matrix the lists the indices of each FACTS polygon that intersects each plot. Need to futher process it in the lines below to get the actual managmenet history)
activities = st_intersects(plots_sf %>% st_transform(st_crs(facts)),facts)

# Initiate new columns to store FACTS management history data
plots_sf$facts_managed = NA
plots_sf$facts_year_mostrecent = NA
plots_sf$facts_activity = NA
plots_sf$facts_date_award = NA
plots_sf$facts_date_compl = NA
plots_sf$facts_reporting_discrepancy = NA

# For each plot, get the activities that happened after the fire year
for(i in 1:nrow(plots_sf)) {
  
  plot = plots_sf[i,]
  
  # which facts polygons overlapped the plot
  facts_indices = activities[[i]]
  facts_overlap = facts[facts_indices,]
  
  # get the most recent year of management out of all the facts polygons that overlapped
  facts_year_mostrecent = max(facts_overlap$year)
  facts_year_mostrecent = ifelse(facts_year_mostrecent == -Inf, NA, facts_year_mostrecent)
  
  # if the most recent year of management was before the fire, ignore the management records
  if(is.na(facts_year_mostrecent) | (facts_year_mostrecent < plot$fire_year)) next()
  
  # otherwise, record all the management that happened there
  plots_sf[i,which(names(plots_sf) == "facts_managed")] = TRUE
  plots_sf[i,which(names(plots_sf) == "facts_year")] = paste(facts_overlap$year,collapse=", ")
  plots_sf[i,which(names(plots_sf) == "facts_year_mostrecent")] = facts_year_mostrecent
  plots_sf[i,which(names(plots_sf) == "facts_activity")] = paste(facts_overlap$ACTIVITY,collapse=", ")
  plots_sf[i,which(names(plots_sf) == "facts_date_award")] = paste(facts_overlap$DATE_AWARD,collapse=", ")
  plots_sf[i,which(names(plots_sf) == "facts_date_compl")] = paste(facts_overlap$DATE_COMPL,collapse=", ")
  plots_sf[i,which(names(plots_sf) == "facts_reporting_discrepancy")] = paste(facts_overlap$reporting_discrepancy,collapse=", ")
  
}

# Check to see if management occurred after the inital sampling visit
plots_sf$managed_after_visit = plots_sf$facts_year_mostrecent >= plots_sf$Sample_Year

### Make sure plots are on FS land
sf_use_s2(FALSE)
fsland = st_read(datadir("ownership/fsland_ca.gpkg")) %>% st_buffer(0) %>% st_union
on_fs = st_intersects(plots_sf %>% st_transform(st_crs(fsland)),fsland, sparse=FALSE)
plots_sf$fs_land = on_fs[,1]

plots_sf = plots_sf %>%
  filter(fs_land == TRUE | FIRE_ID == "GON") # make a temporary exception for the Gondola fire because it's just over the border in NV and our FS ownership and FACTS layer doesn't yet extend there. Assume it wasn't managed and it's on FS land.


### Extract climate data at plot locations
precip = rast(datadir("raw-data/prism/PRISM_ppt_30yr_normal_800mM3_annual_bil/PRISM_ppt_30yr_normal_800mM3_annual_bil.bil"))
tmean = rast(datadir("raw-data/prism/PRISM_tmean_30yr_normal_800mM3_annual_bil/PRISM_tmean_30yr_normal_800mM3_annual_bil.bil"))

plots_vect = vect(plots_sf %>% st_transform(crs(precip)))

precip_extr = extract(precip,plots_vect)
tmean_extr = extract(tmean,plots_vect)

plots_sf$precip = precip_extr[,2]
plots_sf$tmean = tmean_extr[,2]


### Compute # years since first survey and reburn
plots_sf = plots_sf %>%
  mutate(yrs_sample_to_reburn = mostrecent_fire - Sample_Year,
         yrs_since_first_survey = 2022 - Sample_Year)


### Change the FACTS managed column to have descriptive values
plots_sf = plots_sf %>%
  mutate(facts_managed = ifelse(!is.na(facts_managed), "Managed", "Unmanaged"))



### Save compiled plot data (spatial), ------------ 
#plus separate shapefiles for permanently marked and non-permanently marked plots

# Remove unneeded and potentially confusing columns
plots_sf = plots_sf %>%
  select(-facts_reporting_discrepancy, -fs_land)


## correct fire IDs
plots_sf = plots_sf %>% 
  mutate(FIRE_ID_cor = FIRE_ID) %>%
  mutate(FIRE_ID = replace(FIRE_ID, FIRE_ID == "MOO", "MNL")) %>%
  mutate(FIRE_ID_cor = replace(FIRE_ID_cor, FIRE_ID_cor == "A", "MNL")) %>%
  mutate(FIRE_ID_cor = replace(FIRE_ID_cor, FIRE_ID_cor == "B", "PWR")) %>%
  mutate(FIRE_ID_cor = replace(FIRE_ID_cor, FIRE_ID_cor == "C", "AMR")) %>%
  mutate(FIRE_ID_cor = replace(FIRE_ID_cor, FIRE_ID_cor == "PNP", "PNT"))

plots_sf %>%
  group_by(FIRE_ID_cor) %>% count()


st_write(plots_sf, datadir("raw-data/plot-data-compiled/reburn-plots_compiled.gpkg"), delete_dsn = TRUE)


## Prelim visualization
ggplot(plots_sf, aes(x=precip,y=tmean,color=FIRE_ID_cor)) +
  geom_point() +
  theme_bw(15)

ggplot(plots_sf, aes(x=yrs_sample_to_reburn,y=tmean,color=Sample_Year)) +
  geom_point() +
  theme_bw(15)

ggplot(plots_sf, aes(x=as.factor(yrs_sample_to_reburn),y=mostrecent_fire, color= fire_sev)) +
  geom_jitter(width = 0.5) +
  geom_point(color = "red")+
  theme_bw(15)

ggplot(plots_sf, aes(x=as.factor(fire_year),y=mostrecent_fire, color= fire_sev)) +
  geom_jitter(width = 0.5) +
  geom_point(color = "red")+
  theme_bw(15)


