##########################################################################
# This script re-constructs alldata.csv and alldatahierarchies.csv per 6/16 call with Ada by:
# A. Reading in the cleaned city data and making clean-up changes as needed
# B. Adding the weapons columns
# C. Adding city region column
# D. Exporting as csv
#
# Exports: 
# 1. alldata_Azavea.csv
# 2. alldatahierarchies_Azavea.csv
#
# To-do:
# 1. add `size` column data
# 
##########################################################################

#### Admin ----
source("~scripts/00 - Admin.R")
ind_cities_path <- file.path(data_dir, "Individual_City_Dataset/")
col_names <- c("incidentID", "reportdate", "occurdate", "desc", "weapon", "loc", "lat", "lon", "city", "state")

#### A. Read in city data ----

#### 1. Atlanta, GA ----
atl <- read_csv(file.path(ind_cities_path, "Atlanta_Georgia/clean_data/atlanta_allOffense_09_20.csv")) 

atl <- atl %>% 
  dplyr::select(incidentID = `Report Number`,
                reportdate = `Report Date`,
                occurdate = `Occur Date`,
                desc = `UCR Literal`,
                loc = `Location`,
                lat = `Latitude`,
                lon = `Longitude`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         city = "Atlanta",
         state = "Georgia",
         reportdate = as.character(reportdate),
         occurdate = as.character(occurdate)) %>% 
  dplyr::select(all_of(col_names))

#### 2. Auburn, WA ----
auburn <- read_csv(file.path(ind_cities_path, "Auburn_Washington/clean_data/auburn_allOffense_17_20.csv")) 

auburn <- auburn %>% 
  dplyr::select(incidentID = `CASENUMBER`,
                reportdate = `REPORTED`,
                # occurdate = `Occur Date`,
                desc = `OFFENSENAME`,
                loc = `Location`,
                lat = `Y`,
                lon = `X`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         occurdate = NA_character_,
         city = "Auburn",
         state = "Washington",
         reportdate = as.character(reportdate)) %>% 
  dplyr::select(all_of(col_names))

#### 3. Baltimore, MD ----
balt <- read_csv(file.path(ind_cities_path, "Baltimore_Maryland/clean_data/baltimore_allOffense_14_20.csv")) 

balt <- balt %>% 
  dplyr::select(incidentID = `ID`,
                # reportdate = `REPORTED`,
                # occurdate = `Occur Date`,
                desc = `Description`,
                weapon = `Weapon`,
                loc = `Location`,
                lat = `Latitude`,
                lon = `Longitude`,
                `CrimeDate`,
                `CrimeTime`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         city = "Baltimore",
         # weapon = NA_character_,
         reportdate = NA_character_,
         occurdate = paste(CrimeDate, CrimeTime),
         state = "Maryland") %>% 
  dplyr::select(all_of(col_names))

#### 4. Baton Rouge, LA ----
baton <- read_csv(file.path(ind_cities_path, "BatonRouge_Louisiana/clean_data/batonrouge_allOffense_11_20.csv")) 
  
baton <- baton %>% 
  dplyr::select(incidentID = `FILE NUMBER`,
                # reportdate = `REPORTED`,
                # occurdate = `Occur Date`,
                desc = `OFFENSE DESCRIPTION`,
                loc = `FULL ADDRESS`,
                lat = `Y`,
                lon = `X`,
                `OFFENSE DATE`,
                `OFFENSE TIME`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         occurdate = paste(`OFFENSE DATE`, `OFFENSE TIME`),
         reportdate = NA_character_,
         city = "Baton Rouge",
         state = "Louisiana") %>% 
  dplyr::select(all_of(col_names))

#### 5. Boston, MA ----
bos12_15 <- read_csv(file.path(ind_cities_path, "Boston_Massachusetts/clean_data/boston_allOffense_12_15.csv")) %>% 
  mutate(FROMDATE = as.character(FROMDATE))
bos15_20 <- read_csv(file.path(ind_cities_path, "Boston_Massachusetts/clean_data/boston_allOffense_15_20.csv")) %>% 
  mutate(OCCURRED_ON_DATE = as.character(OCCURRED_ON_DATE))

bos12_15 <- bos12_15 %>% 
  dplyr::select(incidentID = `COMPNOS`,
                # reportdate = `REPORTED`,
                occurdate = `FROMDATE`,
                desc = `INCIDENT_TYPE_DESCRIPTION`,
                loc = `STREETNAME`,
                # lat = `Y`,
                # lon = `X`,
                weapon = `WEAPONTYPE`, 
                `Location`
                  ) %>% 
  mutate(incidentID = as.character(incidentID),
         lon = str_match(Location, "[:digit:], (.*?)\\)")[, 2],
         lat = str_match(Location, "\\((.*?),")[, 2],
         # weapon = NA_character_,
         # occurdate = paste(`OFFENSE DATE`, `OFFENSE TIME`),
         lat = as.character(lat),
         lon = as.character(lon),
         reportdate = NA_character_,
         city = "Boston",
         state = "Massachusetts") %>% 
  dplyr::select(all_of(col_names))

bos15_20 <- bos15_20 %>% 
  dplyr::select(incidentID = `INCIDENT_NUMBER`,
                # reportdate = `REPORTED`,
                occurdate = `OCCURRED_ON_DATE`,
                desc = `OFFENSE_DESCRIPTION`,
                loc = `STREET`,
                lat = `Lat`,
                lon = `Long`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         reportdate = NA_character_,
         city = "Boston", 
         state = "Massachusetts") %>% 
  dplyr::select(all_of(col_names))

bos <- rbind(bos12_15,
             bos15_20)

#### 6. Chicago, IL ----
chi <- vroom(file.path(ind_cities_path, "Chicago_Illinois/raw_data/Crimes_-_2001_to_present.csv")) %>% 
  mutate(Date = as.character(Date))

chi <- chi %>% 
  dplyr::select(incidentID = `ID`,
                # reportdate = `REPORTED`,
                occurdate = `Date`,
                desc = `Description`,
                loc = `Block`,
                lat = `Latitude`,
                lon = `Longitude`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         reportdate = NA_character_,
         city = "Chicago",
         state = "Illinois") %>% 
  dplyr::select(all_of(col_names))

#### 7. Cincinnati, OH ----
cin <- vroom(file.path(ind_cities_path, "Cincinnati_Ohio/clean_data/cincinnati_allOffense_10_20.csv")) %>% 
  mutate(DATE_REPORTED = as.character(DATE_REPORTED),
         DATE_FROM = as.character(DATE_FROM))

cin <- cin %>% 
  dplyr::select(incidentID = `INCIDENT_NO`,
                reportdate = `DATE_REPORTED`,
                occurdate = `DATE_FROM`,
                desc = `OFFENSE`,
                loc = `ADDRESS_X`,
                lat = `LATITUDE_X`,
                lon = `LONGITUDE_X`) %>% 
  mutate(incidentID = as.character(incidentID),
         weapon = NA_character_,
         lat = as.character(lat),
         lon = as.character(lon),
         # occurdate = NA_character_,
         # reportdate = NA_character_,
         city = "Cincinnati",
         state = "Ohio") %>% 
  dplyr::select(all_of(col_names))

#### 8. Columbia, SC ----
columbia <- read_csv(file.path(ind_cities_path, "Columbia_SouthCarolina/clean_data/columbia_allOffense_16_19.csv")) %>% 
  mutate(Arrest_Date = as.character(Arrest_Date))

columbia <- columbia %>% 
  dplyr::select(incidentID = `Inc_Case`,
                reportdate = `Arrest_Date`,
                # occurdate = `DATE_FROM`,
                desc = `Offense_Description`,
                loc = `Address`,
                lat = `Y`,
                lon = `X`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         occurdate = NA_character_,
         # reportdate = NA_character_,
         city = "Columbia",
         state = "South Carolina") %>% 
  dplyr::select(all_of(col_names))

#### 9. Dallas, TX ----
# did not include Police_Bulk_Data bc it didn't have any coordinate info
dallas_incidents <- vroom(file.path(ind_cities_path, "Dallas_Texas/raw_data/Police_Incidents.csv")) %>% 
  mutate(`Date of Report` = as.character(`Date of Report`),
         `Date1 of Occurrence` = as.character(`Date1 of Occurrence`))

dallas <-  dallas_incidents %>% 
  dplyr::select(incidentID = `Incident Number w/year`,
                reportdate = `Date of Report`,
                occurdate = `Date1 of Occurrence`,
                desc = `UCR Offense Description`,
                loc = `Location1`,
                weapon = `Weapon Used`) %>% 
  mutate(incidentID = as.character(incidentID),
         lon = str_match(loc, "[:digit:], (.*?)\\)")[, 2],
         lat = str_match(loc, "\\((.*?),")[, 2],
         lat = as.character(lat),
         lon = as.character(lon),
         city = "Dallas",
         state = "Texas") %>% 
  dplyr::select(all_of(col_names))

#### 10. Denver, CO ----
den <- read_csv(file.path(ind_cities_path, "Denver_Colorado/clean_data/denver_allOffense_14_20.csv")) %>% 
  mutate(REPORTED_DATE = as.character(REPORTED_DATE),
         FIRST_OCCURRENCE_DATE = as.character(FIRST_OCCURRENCE_DATE),
         LAST_OCCURRENCE_DATE = as.character(LAST_OCCURRENCE_DATE))

den <- den %>% 
  dplyr::select(incidentID = `OFFENSE_ID`,
                reportdate = `REPORTED_DATE`,
                occurdate = `FIRST_OCCURRENCE_DATE`,
                desc = `OFFENSE_TYPE_ID`,
                loc = `INCIDENT_ADDRESS`,
                lat = `GEO_LAT`,
                lon = `GEO_LON`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         # reportdate = NA_character_,
         city = "Denver",
         state = "Colorado") %>% 
  dplyr::select(all_of(col_names))

#### 11. Detroit, MI ----
det_11_14 <- read_csv(file.path(ind_cities_path, "Detroit_Michigan/clean_data/detroit_allOffense_11_14.csv")) %>% 
  mutate(INCIDENTDATE = as.character(INCIDENTDATE))
det_15_20 <- read_csv(file.path(ind_cities_path, "Detroit_Michigan/clean_data/detroit_allOffense_15_20.csv")) %>% 
  mutate(incident_timestamp = as.character(incident_timestamp))


det_11_14 <- det_11_14 %>% 
  dplyr::select(incidentID = `INCINO`,
                # reportdate = `REPORTED_DATE`,
                occurdate = `INCIDENTDATE`,
                desc = `OFFENSEDESCRIPTION`,
                loc = `ADDRESS`,
                lat = `LAT`,
                lon = `LON`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         reportdate = NA_character_,
         city = "Detroit",
         state = "Michigan") %>% 
  dplyr::select(all_of(col_names))

det_15_20 <- det_15_20 %>% 
  dplyr::select(incidentID = `oid`,
                # reportdate = `REPORTED_DATE`,
                occurdate = `incident_timestamp`,
                desc = `offense_description`,
                loc = `address`,
                lat = `latitude`,
                lon = `longitude`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         reportdate = NA_character_,
         city = "Detroit",
         state = "Michigan") %>% 
  dplyr::select(all_of(col_names))

det <- rbind(det_11_14,
             det_15_20)

#### 12. Gainesville, FL ----
gaines <- read_csv(file.path(ind_cities_path, "Gainesville_Florida/clean_data/gainesville_allOffense_11_20.csv")) %>% 
  mutate(`Report Date` = as.character(`Report Date`),
         `Offense Date` = as.character(`Offense Date`))

gaines <- gaines %>% 
  dplyr::select(incidentID = `ID`,
                reportdate = `Report Date`,
                occurdate = `Offense Date`,
                desc = `Incident Type`,
                loc = `Address`,
                lat = `Latitude`,
                lon = `Longitude`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         # reportdate = NA_character_,
         city = "Gainesville",
         state = "Florida") %>% 
  dplyr::select(all_of(col_names))

#### 13. Hartford, CT ----
hart <- read_csv(file.path(ind_cities_path, "Hartford_Connecticut/clean_data/hartford_allOffense_05_20.csv")) %>% 
  mutate(Date = as.character(Date),
         Time_24HR = as.character(Time_24HR))

hart <- hart %>% 
  dplyr::select(incidentID = `Case_Number`,
                # reportdate = `Report Date`,
                # occurdate = `Offense Date`,
                desc = `UCR_1_Description`,
                loc = `Address`,
                lat = `Y`,
                lon = `X`,
                Date, 
                Time_24HR) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         occurdate = paste(Date, Time_24HR),
         reportdate = NA_character_,
         city = "Hartford",
         state = "Connecticut") %>% 
  dplyr::select(all_of(col_names))

#### 14. Indianapolis, IN ----
ind <- read_csv(file.path(ind_cities_path, "Indianapolis_Indiana/clean_data/indy_allOffense_07_19_geocoded.csv")) %>% 
  mutate(DATE_ = as.character(DATE_),
         TIME = as.character(TIME))

ind <- ind %>% 
  dplyr::select(incidentID = `OBJECTID`,
                # reportdate = `Report Date`,
                # occurdate = `Offense Date`,
                desc = `CRIME`,
                loc = `ADDRESS`,
                lat = `Y`,
                lon = `X`,
                DATE_, 
                TIME) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         occurdate = paste(DATE_, TIME),
         reportdate = NA_character_,
         city = "Indianapolis",
         state = "Indiana") %>% 
  dplyr::select(all_of(col_names))

#### 15. Kansas City, MO ----
kc <- vroom(file.path(ind_cities_path, "KansasCity_Missouri/clean_data/kansascity_allOffense_09_20.csv"))

kc <- kc %>% 
  dplyr::select(incidentID = `Report_No`,
                # reportdate = `Report Date`,
                # occurdate = `Offense Date`,
                desc = `Description`,
                loc = `Address`,
                lat = `Latitude`,
                lon = `Longitude`,
                Reported_Date, 
                Reported_Time,
                weapon = `Firearm Used Flag`,
                From_Date,
                From_Time) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         occurdate = paste(Reported_Date, Reported_Time),
         reportdate = paste(From_Date, From_Time),
         city = "Kansas City",
         state = "Missouri") %>% 
  dplyr::select(all_of(col_names))

#### 16. Lincoln, NE ----
linc_all <- read_csv(file.path(ind_cities_path, "Lincoln_Nebraska/clean_data/lincoln_allOffense_13_20.csv")) %>% 
  mutate(DATE = as.character(DATE),
         TIME = as.character(TIME),
         FROM_DATE = as.character(FROM_DATE),
         FROM_TIME = as.character(FROM_TIME))

linc_gun_geocode <- read_csv(file.path(ind_cities_path, 
                                       "Lincoln_Nebraska/clean_data/lincoln_firearm_13_20_regeocoded.csv")) %>% 
  dplyr::select(CASE_NUMBER,
                lon,
                lat) %>% 
  mutate(lon = as.character(lon),
         lat = as.character(lat))

linc <- linc_all %>% 
  dplyr::select(incidentID = `CASE_NUMBER`,
                # reportdate = `Report Date`,
                # occurdate = `Offense Date`,
                desc = `CALL_TYPE`,
                loc = `ADDRESS`,
                # lat = `Y`,
                # lon = `X`,
                DATE, 
                TIME,
                FROM_DATE,
                FROM_TIME) %>% 
  mutate(incidentID = as.character(incidentID),
         weapon = NA_character_,
         occurdate = paste(FROM_DATE, FROM_TIME),
         reportdate = paste(DATE, TIME),
         city = "Lincoln",
         state = "Nebraska") %>% 
  left_join(linc_gun_geocode,
            by = c("incidentID" = "CASE_NUMBER")) %>% 
  dplyr::select(all_of(col_names))

#### 17. Little Rock, AR ----
lrock <- read_csv(file.path(ind_cities_path, "LittleRock_Arkansas/clean_data/littlerock_allOffense_14_20.csv")) %>% 
  mutate(IncidentDate = as.character(IncidentDate))

lrock <- lrock %>% 
  dplyr::select(incidentID = `IncidentNumber`,
                # reportdate = `Report Date`,
                occurdate = `IncidentDate`,
                desc = `offenseDesc1`,
                loc = `IncidentAddress`,
                lat = `Y`,
                lon = `X`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         reportdate = NA_character_,
         city = "Little Rock",
         state = "Arkansas") %>% 
  dplyr::select(all_of(col_names))

#### 18. Los Angeles, CA ----
LA <- vroom(file.path(ind_cities_path, "LosAngeles_California/raw_data/Crime_Data_from_2010_to_2019.csv")) %>% 
  mutate(`DATE OCC` = as.character(`DATE OCC`),
         `DATE OCC` = str_split(.$`DATE OCC`,
                                pattern = " ",
                                simplify = TRUE)[,1],
         `Date Rptd` = as.character(`Date Rptd`),
         `Date Rptd` = str_split(.$`Date Rptd`,
                                pattern = " ",
                                simplify = TRUE)[,1],)

LA <- LA %>% 
  dplyr::select(incidentID = `DR_NO`,
                reportdate = `Date Rptd`,
                # occurdate = `IncidentDate`,
                desc = `Crm Cd Desc`,
                loc = `LOCATION`,
                lat = `LAT`,
                lon = `LON`,
                weapon = `Weapon Desc`,
                `DATE OCC`,
                `TIME OCC`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         occurdate = paste(`DATE OCC`,`TIME OCC`),
         # reportdate = NA_character_,
         city = "Los Angeles",
         state = "California") %>% 
  dplyr::select(all_of(col_names))

#### 19. Louisville, KY ----
lou_all <- vroom(file.path(ind_cities_path, "Louisville_Kentucky/clean_data/louisville_allOffense_03_20.csv")) %>% 
  mutate(ID = as.character(ID),
         DATE_REPORTED = as.character(DATE_REPORTED),
         DATE_OCCURED = as.character(DATE_OCCURED))

lou_gun_geocode <- read_csv(file.path(ind_cities_path, 
                                       "Louisville_Kentucky/clean_data/louisville_firearm_03_20_geocoded.csv")) %>% 
  dplyr::select(incidentID = ID,
                lon,
                lat) %>% 
  mutate(incidentID = as.character(incidentID),
         lon = as.character(lon),
         lat = as.character(lat))

lou <- lou_all %>% 
  dplyr::select(incidentID = `ID`,
                reportdate = `DATE_REPORTED`,
                occurdate = `DATE_OCCURED`,
                desc = `UOR_DESC`,
                loc = `BLOCK_ADDRESS`
                # lat = `Y`,
                # lon = `X`,
                ) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = paste(FROM_DATE, FROM_TIME),
         # reportdate = paste(DATE, TIME),
         city = "Louisville",
         state = "Kentucky") %>% 
  left_join(lou_gun_geocode,
            by = c("incidentID")) %>% 
  dplyr::select(all_of(col_names))

#### 20. Madison, WI ----
mad <- read_csv(file.path(ind_cities_path, 
                          "Madison_Wisconsin/clean_data/madison_allOffense_05_20_regeocoded.csv")) %>% 
  mutate(IncidentDate = as.character(IncidentDate))

mad <- mad %>% 
  dplyr::select(incidentID = `IncidentID`,
                # reportdate = `Report Date`,
                occurdate = `IncidentDate`,
                desc = `Details`,
                loc = `Address`,
                lat = `lat`,
                lon = `lon`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         reportdate = NA_character_,
         city = "Madison",
         state = "Wisconsin") %>% 
  dplyr::select(all_of(col_names))

#### 21. Minneapolis, MN ----
mpls <- read_csv(file.path(ind_cities_path, 
                          "Minneapolis_Minnesota/clean_data/minneapolis_allOffense_10_20.csv")) %>% 
  mutate(ReportedDate = as.character(ReportedDate),
         BeginDate = as.character(BeginDate))

mpls <- mpls %>% 
  dplyr::select(incidentID = `FID`,
                reportdate = `ReportedDate`,
                occurdate = `BeginDate`,
                desc = `Description`,
                loc = `PublicAddress`,
                lat = `Lat`,
                lon = `Long`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         # reportdate = NA_character_,
         city = "Minneapolis",
         state = "Minnesota") %>% 
  dplyr::select(all_of(col_names))

#### 22. Nashville, TN ----
nash <- vroom(file.path(ind_cities_path, 
                           "Nashville_Tennessee/clean_data/nashville_allOffense_13_20.csv")) %>% 
  mutate(`Incident Occurred` = as.character(`Incident Occurred`),
         `Incident Reported` = as.character(`Incident Reported`))

nash <- nash %>% 
  dplyr::select(incidentID = `Primary Key`,
                reportdate = `Incident Reported`,
                occurdate = `Incident Occurred`,
                desc = `Offense Description`,
                weapon = `Weapon Description`,
                loc = `Incident Location`,
                lat = `Latitude`,
                lon = `Longitude`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         city = "Nashville",
         state = "Tennessee") %>% 
  dplyr::select(all_of(col_names))

#### 23. New York, NY ----
nyc <- vroom(file.path(ind_cities_path,
                       "NewYork/raw_data/NYPD_Complaint_Data_Historic.csv")) %>% 
  mutate(`RPT_DT` = as.character(`RPT_DT`),
         `CMPLNT_FR_DT` = as.character(`CMPLNT_FR_DT`),
         `CMPLNT_FR_TM` = as.character(`CMPLNT_FR_TM`))

nyc <- nyc %>% 
  dplyr::select(incidentID = `CMPLNT_NUM`,
                reportdate = `RPT_DT`,
                # occurdate = `Incident Occurred`,
                desc = `PD_DESC`,
                # weapon = `Weapon Description`,
                # loc = `Incident Location`,
                lat = `Latitude`,
                lon = `Longitude`,
                `CMPLNT_FR_DT`,
                `CMPLNT_FR_TM`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         loc = NA_character_,
         occurdate = paste(`CMPLNT_FR_DT`,
                           `CMPLNT_FR_TM`),
         # reportdate = NA_character_,
         city = "New York",
         state = "New York") %>% 
  dplyr::select(all_of(col_names))
  
#### 24. Philadelphia, PA ----
phl <- vroom(file.path(ind_cities_path, 
                          "Philadelphia_Pennsylvania/clean_data/philly_allOffense_06_20.csv")) %>% 
  mutate(dispatch_date_time = as.character(dispatch_date_time))

phl <- phl %>% 
  dplyr::select(incidentID = `dc_key`,
                reportdate = `dispatch_date_time`,
                # occurdate = `IncidentDate`,
                desc = `text_general_code`,
                loc = `location_block`,
                lat = `point_y`,
                lon = `point_x`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         occurdate = NA_character_,
         # reportdate = NA_character_,
         city = "Philadelphia",
         state = "Pennsylvania") %>% 
  dplyr::select(all_of(col_names))

#### 25. Phoenix, AZ ----
pho_all <- vroom(file.path(ind_cities_path, 
                       "Phoenix_Arizona/raw_data/crimestat.csv")) %>% 
  mutate(`OCCURRED ON` = as.character(`OCCURRED ON`),
         `INC NUMBER` = as.character(`INC NUMBER`))

pho_gun_geocode <- read_excel(file.path(ind_cities_path, 
                                      "Phoenix_Arizona/clean_data/phoenix_firearm_15_20_geocoded.xlsx")) %>% 
  dplyr::select(incidentID = `INC NUMBER`,
                lat = Latitude,
                lon = Longitude) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon))

pho <- pho_all %>% 
  dplyr::select(incidentID = `INC NUMBER`,
                # reportdate = `DATE_REPORTED`,
                occurdate = `OCCURRED ON`,
                desc = `UCR CRIME CATEGORY`,
                loc = `100 BLOCK ADDR`
                # lat = `Y`,
                # lon = `X`,
  ) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         reportdate = NA_character_,
         city = "Phoenix",
         state = "Arizona") %>% 
  left_join(pho_gun_geocode,
            by = c("incidentID")) %>% 
  dplyr::select(all_of(col_names))

#### 26. Portland, OR ----
port <- vroom(file.path(ind_cities_path, 
                       "Portland_Oregon/clean_data/portland_allOffense_15_20.csv")) %>% 
  mutate(OccurDate = as.character(OccurDate),
         ReportDate = as.character(ReportDate))

port <- port %>% 
  dplyr::select(incidentID = `CaseNumber`,
                reportdate = `ReportDate`,
                occurdate = `OccurDate`,
                desc = `OffenseType`,
                loc = `Address`,
                lat = `OpenDataLat`,
                lon = `OpenDataLon`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         # reportdate = NA_character_,
         city = "Portland",
         state = "Oregon") %>% 
  dplyr::select(all_of(col_names))
  
#### 27. Raleigh, NC ----
ral <- vroom(file.path(ind_cities_path, 
                               "Raleigh_NorthCarolina/clean_data/raleigh_allOffense_05_20.csv")) %>% 
  mutate(INC_DATETIME = as.character(INC_DATETIME))
  
ral <- ral %>% 
  dplyr::select(incidentID = `INC_NO`,
                # reportdate = `ReportDate`,
                occurdate = `INC_DATETIME`,
                desc = `CRIME_DESC`,
                # loc = `Address`,
                lat = `LATITUDE`,
                lon = `LONGITUDE`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         loc = NA_character_,
         reportdate = NA_character_,
         city = "Raleigh",
         state = "North Carolina") %>% 
  dplyr::select(all_of(col_names))

#### 28. Sacramento County, CA ----
sac <- vroom(file.path(ind_cities_path, 
                       "SacramentoCounty_California/clean_data/sacramento_allOffense_07_20.csv")) %>% 
  mutate(ReportDate = as.character(ReportDate),
         OccurenceStartDate = as.character(OccurenceStartDate))

sac <- sac %>% 
  dplyr::select(incidentID = `FID`,
                reportdate = `ReportDate`,
                occurdate = `OccurenceStartDate`,
                desc = `PrimaryViolation`,
                loc = `OccurenceLocation`,
                lat = `Y`,
                lon = `X`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         # loc = NA_character_,
         # reportdate = NA_character_,
         city = "Sacramento County",
         state = "California") %>% 
  dplyr::select(all_of(col_names))

#### 29. Saint Paul, MN ----
sp_all <- vroom(file.path(ind_cities_path, 
                       "SaintPaul_Minnesota/raw_data/Crime_Incident_Report_-_Dataset.csv")) %>% 
  mutate(DATE = as.character(DATE),
         TIME = as.character(TIME),
         `CASE NUMBER` = as.character(`CASE NUMBER`))

sp_gun_geocode <- read_excel(file.path(ind_cities_path, 
                                        "SaintPaul_Minnesota/clean_data/saintpaul_firearm_14_20_geocoded.xlsx")) %>% 
  dplyr::select(incidentID = `CASE NUMBER`,
                lat = Latitude,
                lon = Longitude) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon))

sp <- sp_all %>% 
  dplyr::select(incidentID = `CASE NUMBER`,
                # reportdate = `DATE_REPORTED`,
                # occurdate = `OCCURRED ON`,
                desc = `INCIDENT TYPE`,
                loc = `BLOCK`,
                DATE,
                TIME
                # lat = `Y`,
                # lon = `X`,
  ) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         occurdate = paste(DATE, TIME),
         reportdate = NA_character_,
         city = "Saint Paul",
         state = "Minnesota") %>% 
  left_join(sp_gun_geocode,
            by = c("incidentID")) %>% 
  dplyr::select(all_of(col_names))

#### 30. Salt Lake City, UT ----
slc <- vroom(file.path(ind_cities_path, 
                       "SaltLakeCity_Utah/clean_data/saltlakecity_allOffense_07_20_geocoded.csv")) %>% 
  mutate(REPORT_DATE = as.character(REPORT_DATE),
         OCCUR_DATE = as.character(OCCUR_DATE))

slc <- slc %>% 
  dplyr::select(incidentID = `CASE`,
                reportdate = `REPORT_DATE`,
                occurdate = `OCCUR_DATE`,
                desc = `UCR_DESCRIPTION`,
                loc = `LOCATION`,
                lat = `Y`,
                lon = `X`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = NA_character_,
         # loc = NA_character_,
         # reportdate = NA_character_,
         city = "Salt Lake City",
         state = "Utah") %>% 
  dplyr::select(all_of(col_names))

#### 31. San Francisco, CA ----
sf <- vroom(file.path(ind_cities_path, 
                       "SanFrancisco_California/clean_data/sanfrancisco_allOffense_03_20.csv")) %>% 
  mutate(Date = as.character(Date),
         Time = as.character(Time))

sf <- sf %>% 
  dplyr::select(incidentID = `IncidntNum`,
                # reportdate = `REPORT_DATE`,
                # occurdate = `OCCUR_DATE`,
                desc = `Descript`,
                loc = `Location`,
                lat = `Y`,
                lon = `X`,
                Date,
                Time) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         occurdate = paste(Date, Time),
         # loc = NA_character_,
         reportdate = NA_character_,
         city = "San Francisco",
         state = "California") %>% 
  dplyr::select(all_of(col_names))

#### 32. St. Louis County, MO ----
stl <- vroom(file.path(ind_cities_path, 
                      "StLouisCounty_Missouri/clean_data/stlouis_allOffense_15_19.csv")) %>% 
  mutate(D_OCCURRED = as.character(D_OCCURRED),
         DT_CALLREC = as.character(DT_CALLREC))

stl <- stl %>% 
  dplyr::select(incidentID = `COMPLAINTNUM`,
                reportdate = `DT_CALLREC`,
                occurdate = `D_OCCURRED`,
                desc = `UCR_OFFENSE`,
                loc = `ADDRESS`,
                lat = `Y`,
                lon = `X`) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = as.character(lat),
         lon = as.character(lon),
         weapon = NA_character_,
         # occurdate = paste(Date, Time),
         # loc = NA_character_,
         # reportdate = NA_character_,
         city = "St Louis County",
         state = "Missouri") %>% 
  dplyr::select(all_of(col_names))

#### 33. Tucson, AZ ----
tuc <- vroom(file.path(ind_cities_path, 
                       "Tucson_Arizona/clean_data/tucson_allOffense_09_20.csv")) %>% 
  mutate(DATE_OCCU = as.character(DATE_OCCU),
         DATE_REPT = as.character(DATE_REPT))

tuc <- tuc %>% 
  dplyr::select(incidentID = `INCI_ID`,
                reportdate = `DATE_REPT`,
                occurdate = `DATE_OCCU`,
                weapon = `WEAPON1DESC`,
                desc = `STATUTDESC`,
                loc = `ADDRESS_PUBLIC`,
                geometry) %>% 
  mutate(incidentID = as.character(incidentID),
         lat = str_match(geometry, "[:digit:], (.*?)\\)")[, 2],
         lon = str_match(geometry, "\\((.*?),")[, 2],
         lat = as.character(lat),
         lon = as.character(lon),
         city = "Tucson",
         state = "Arizona") %>% 
  dplyr::select(all_of(col_names))

#### 34. Virginia Beach, VA ----
va <- vroom(file.path(ind_cities_path, 
                       "VirginiaBeach_Virginia/raw_data/Police+Incident+Reports.csv")) %>% 
  mutate(`Date Reported` = as.character(`Date Reported`),
         `Date Occured` = as.character(`Date Occured`))

va <- va %>% 
  dplyr::select(incidentID = `Police Case Number`,
                reportdate = `Date Reported`,
                occurdate = `Date Occured`,
                desc = `Offense Description`,
                loc = `Block Address`) %>% 
  mutate(incidentID = as.character(incidentID),
         weapon = NA_character_,
         # occurdate = paste(Date, Time),
         # loc = NA_character_,
         # reportdate = NA_character_,
         lat = NA_character_,
         lon = NA_character_,
         city = "Virginia Beach",
         state = "Virginia") %>% 
  dplyr::select(all_of(col_names))

#### Combine cities ----
alldata <- bind_rows(atl,
             auburn,
             balt,
             baton,
             bos,
             chi,
             cin,
             columbia,
             dallas,
             den,
             det,
             gaines,
             hart,
             ind,
             kc,
             linc,
             lrock,
             LA,
             lou,
             mad,
             mpls,
             nash,
             nyc,
             phl,
             pho,
             port,
             ral,
             sac,
             sp,
             slc,
             sf,
             stl,
             tuc,
             va)

#### B. Add weapons columns ----
adagun_desc <- c("GUN", "FIREARM", "WEAPON", "MURDER",
              "MANSLAUGHTER", "HANDGUN", "RIFLE", "SHOTGUN", 
              "SHOOTING", "SHOT", "HOMICIDE")
adagun_desc <- paste(adagun_desc, collapse = "|")

weapgun_desc <- c("GUN", "FIREARM", "RIFLE", "REVOLVER",
                  "PISTOL", "SHOTGUN", "Y", "1")
weapgun_desc <- paste(weapgun_desc, collapse = "|")

alldata <- alldata %>% 
  mutate(desc = str_to_upper(desc),
         adagun = ifelse(str_detect(desc, adagun_desc), 1, 0),
         weapon = str_to_upper(weapon),
         weapgun = ifelse(str_detect(weapon, weapgun_desc), 1, 0),
         allgun = ifelse(adagun == 1 | weapgun == 1, 1, 0))

#### C. Add `region` and `size` columns ----
west <- c("Auburn",
          "Denver",
          "Los Angeles",
          "Phoenix",
          "Portland",
          "Sacramento County",
          "Salt Lake City",
          "San Francisco",
          "Tucson")
midwest <- c("Chicago",
             "Cincinnati",
             "Detroit",
             "Indianapolis",
             "Kansas City",
             "Lincoln",
             "Madison",
             "Minneapolis",
             "Saint Paul",
             "St Louis County")
south <- c("Atlanta",
           "Baltimore",
           "Baton Rouge",
           "Columbia",
           "Dallas",
           "Gainesville",
           "Little Rock",
           "Louisville",
           "Nashville",
           "Raleigh",
           "Virginia Beach")
northeast <- c("Boston",
               "Hartford",
               "New York",
               "Philadelphia")

alldata_hierachy <- alldata %>% 
  mutate(size = NA_character_,
         region = as.factor(case_when(city %in% west ~ "West",
                                      city %in% midwest ~ "Midwest",
                                      city %in% south ~ "South",
                                      city %in% northeast ~ "Northeast",
                                      TRUE ~ NA_character_)))

#### D. Export as csv ----
# fwrite(alldata,
#        file = file.path(data_dir, "Full_City_Dataset/alldata_Azavea.csv"))
# fwrite(alldata_hierarchy,
#        file = file.path(data_dir, "Full_City_Dataset/alldatahierarchies_Azavea.csv"))