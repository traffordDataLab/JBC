# Population picker: pre-processing code #

library(tidyverse) ; library(lubridate) ; library(sf)

# Create a string object with the name of the local authority
id <- "Liverpool"

# ------------------------------------------------------------------------------

# Area lookups
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/
administrative_lookup <- read_csv("https://opendata.arcgis.com/datasets/e169bb50944747cd83dcfb4dd66555b1_0.csv")
statistical_lookup <- read_csv("https://opendata.arcgis.com/datasets/6ecda95a83304543bc8feedbd1a58303_0.csv")

# Local authority code
la_code <- pull(distinct(filter(administrative_lookup, LAD19NM == id), LAD19CD))
# Electoral ward codes
ward_codes <- pull(distinct(filter(administrative_lookup, LAD19NM == id), WD19CD))
# MSOA codes
msoa_codes <- pull(distinct(filter(statistical_lookup, LAD11NM == id), MSOA11CD))
# LSOA codes
lsoa_codes <- pull(distinct(filter(statistical_lookup, LAD11NM == id), LSOA11CD))
# OA codes
oa_codes <- pull(distinct(filter(statistical_lookup, LAD11NM == id), OA11CD))

# MSOA names
# Source: House of Commons Library
# URL: https://visual.parliament.uk/msoanames
msoa_names <- read_csv("https://visual.parliament.uk/msoanames/static/MSOA-Names-1.7.csv") %>% 
  select(msoa11cd, msoa11hclnm)

# ------------------------------------------------------------------------------

# Mid-2019 population estimates # 
# Source: Nomis
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/annualmidyearpopulationestimates/mid2019
# Licence: Open Government Licence v3.0

# Nomis API key needed to return over 25,000 rows
api_key <- ""

# Local authority
la <- read_csv(paste0("http://www.nomisweb.co.uk/api/v01/dataset/NM_2002_1.data.csv?geography=", la_code, "&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name")) %>% 
  mutate(geography = "Local authority")

# Electoral ward
# !Needs to be done manually until API call debugged!
ward <- read_csv("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=1656750747...1656750768,1656750770,1656750769,1656750771...1656750776&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name") %>% 
  mutate(geography = "Electoral ward")

# ward <- read_csv(paste0("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=395", paste0(ward_codes, collapse = ","), "&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name")) %>% 
#   mutate(geography = "Electoral ward")

# Middle-layer Super Output Area
msoa <- read_csv(paste0("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?geography=", paste0(msoa_codes, collapse = ","), "&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name")) %>% 
  left_join(msoa_names, by = c("GEOGRAPHY_CODE" = "msoa11cd")) %>%
  mutate(GEOGRAPHY_NAME = msoa11hclnm,
         geography = "MSOA") %>% 
  select(-msoa11hclnm)

# Lower-layer Super Output Area
lsoa <- read_csv(paste0("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?uid=", api_key, "&geography=", paste0(lsoa_codes, collapse = ","), "&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name")) %>% 
  mutate(geography = "LSOA")

# Output Area
oa <- read_csv(paste0("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?uid=", api_key, "&geography=", paste0(oa_codes, collapse = ","), "&date=latest&gender=0...2&c_age=101...191&measures=20100&select=date_name,geography_name,geography_code,gender_name,c_age_name,measures_name,obs_value,obs_status_name")) %>% 
  mutate(geography = "OA")

all_geographies <- bind_rows(la, ward, msoa, lsoa, oa) %>% 
  select(period = DATE_NAME,
         area_code = GEOGRAPHY_CODE,
         area_name = GEOGRAPHY_NAME,
         gender = GENDER_NAME,
         age = C_AGE_NAME,
         count = OBS_VALUE,
         geography) %>% 
  mutate(period = ymd(str_c(period, "06-30", sep = "-")),
         gender = fct_recode(gender, "Females" = "Female" , "Males" = "Male", "Persons" = "Total"),
         age = as.integer(str_trim(str_replace_all(age, "Age.|\\+", ""))))  %>% 
  spread(age, count) %>% 
  gather(age, n, -period, -area_code, -area_name, -geography, -gender) %>%
  mutate(age = as.integer(age))

write_csv(all_geographies, "mid-2019_population_estimates_all_geographies.csv")

# ------------------------------------------------------------------------------

# Administrative and statistical boundaries #
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/
# Licence: Open Government Licence v3.0

# Local authority
st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Local_Authority_Districts_December_2019_Boundaries_UK_BFC/MapServer/0/query?where=lad19cd%20like%20'%25", la_code, "%25'&outFields=lad19cd,lad19nm,long,lat&outSR=4326&f=geojson"), quiet = TRUE) %>% 
  select(area_code = lad19cd, area_name = lad19nm) %>% 
  st_write("la.geojson")

# Electoral ward
st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2019_Boundaries_GB_BFC/MapServer/0/query?where=", 
                        URLencode(paste0("wd19cd IN (", paste(shQuote(ward_codes), collapse = ", "), ")")), 
                        "&outFields=wd19cd,wd19nm&outSR=4326&f=geojson")) %>% 
  select(area_code = wd19cd, area_name = wd19nm) %>% 
  st_write("ward.geojson")

# Middle-layer Super Output Area
st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Middle_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=", 
                       URLencode(paste0("msoa11cd IN (", paste(shQuote(msoa_codes), collapse = ", "), ")")), 
                       "&outFields=msoa11cd,msoa11nm&outSR=4326&f=geojson")) %>% 
  left_join(msoa_names, by = "msoa11cd") %>% 
  select(area_code = msoa11cd, area_name = msoa11hclnm) %>% 
  st_write("msoa.geojson")

# Lower-layer Super Output Area
st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Lower_Super_Output_Areas_December_2011_Boundaries/MapServer/2/query?where=UPPER(lsoa11nm)%20like%20'%25", URLencode(toupper(id), reserved = TRUE), "%25'&outFields=lsoa11cd,lsoa11nm&outSR=4326&f=geojson")) %>% 
  select(area_code = lsoa11cd, area_name = lsoa11nm) %>% 
  st_write("lsoa.geojson")

# Output Area
st_read(paste0("https://ons-inspire.esriuk.com/arcgis/rest/services/Census_Boundaries/Output_Area_December_2011_Boundaries/MapServer/2/query?where=UPPER(lad11cd)%20like%20'%25", la_code, "%25'&outFields=oa11cd&outSR=4326&f=geojson")) %>% 
  select(area_code = oa11cd) %>% 
  st_write("oa.geojson")

