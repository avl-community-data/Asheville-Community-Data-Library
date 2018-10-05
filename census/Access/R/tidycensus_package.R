# This code uses tidycensus to access census data
# https://censusreporter.org/topics/table-codes/
library(here)
library(tidyverse)

library(tidycensus)
options(tigris_use_cache = TRUE)

library(scales)
library(leaflet)
library(cowplot)
library(units)
library(sf)

## Null Theme
theme_null <- function() {
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.line = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_line(colour = 'transparent'),
        panel.grid.minor = element_blank())
}

counties <- read.csv(here::here("census", "Access", "R", "nc_counties.csv"))
counties <- counties %>% filter(WNC == 1)

## You'll need to get your own API key
census_api_key("KEY GOES HERE", install = TRUE)

## See variables
acs_vars <- load_variables(2016, "acs5", cache = TRUE)


get_vars <- data.frame( name = c(
  "B01003_001E", # Total Population
  
  "B02001_001E", # Race/Ethnicity
  "B03002_003E",
  "B03002_004E",
  "B03002_005E",
  "B03002_006E",
  "B03002_007E",
  "B03002_008E",
  "B03002_010E",
  "B03002_011E",
  "B03002_012E",
  
  "B07204_002E", # Different house in United States 1 year ago
  
  "B08301_001E", # MEANS OF TRANSPORTATION TO WORK TOTAL
  "B08301_010E", # Public transportation (excluding taxicab)
  
  "B08601_001E", # MEANS OF TRANSPORTATION TO WORK GEOGRAPHY) TOTAL
  "B08601_010E", # Public transportation (excluding taxicab)
  
  "B15003_001E", # Education
  "B15003_002E",
  "B15003_003E",
  "B15003_004E",
  "B15003_005E",
  "B15003_006E",
  "B15003_007E",
  "B15003_008E",
  "B15003_009E",
  "B15003_010E",
  "B15003_011E",
  "B15003_012E",
  "B15003_013E",
  "B15003_014E",
  "B15003_015E",
  "B15003_016E",
  "B15003_017E",
  "B15003_018E",
  'B15003_019E',
  'B15003_020E',
  'B15003_021E',
  'B15003_022E',
  'B15003_023E',
  'B15003_024E',
  'B15003_025E',
  
  'B17026_001E', # RATIO OF INCOME TO POVERTY LEVEL OF FAMILIES IN THE PAST 12 MONTHS
  'B17026_002E',
  'B17026_003E',
  'B17026_004E',
  'B17026_005E',
  'B17026_006E',
  'B17026_007E',
  'B17026_008E',
  'B17026_009E',
  'B17026_010E',
  'B17026_011E',
  'B17026_012E',
  'B17026_013E',
  
  'B19001_001E', # HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2016 INFLATION-ADJUSTED DOLLARS)
  'B19001_002E',
  'B19001_003E',
  'B19001_004E',
  'B19001_005E',
  'B19001_006E',
  'B19001_007E',
  'B19001_008E',
  'B19001_009E',
  'B19001_010E',
  'B19001_011E',
  'B19001_012E',
  'B19001_013E',
  'B19001_014E',
  'B19001_015E',
  'B19001_016E',
  'B19001_017E',
  
  "B19013_001E", #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2016 INFLATION-ADJUSTED DOLLARS)
  
  "B23025_001E", # EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER
  "B23025_002E",
  "B23025_003E",
  "B23025_005E",
  "B23025_006E",
  "B23025_007E",
  
  "B25001_001E", # HOUSING UNITS
  "B25002_001E",
  "B25002_002E",
  "B25002_003E",
  
  "B25003_001E", # HOUSING TENURE
  "B25003_002E",
  "B25003_003E",
  "B25035_001E",

  "C17002_001E", # RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS
  "C17002_002E",
  "C17002_003E",
  "C17002_004E",
  "C17002_005E",
  "C17002_006E",
  "C17002_007E",
  "C17002_008E"
))

get_vars <- get_vars %>%
  left_join(acs_vars, by = "name")

# Clean up
get_vars$label <- gsub("Estimate|!|,|\\$", "", get_vars$label)
get_vars$label <- ifelse(get_vars$label != "Total", gsub("Total", "", get_vars$label), get_vars$label)
get_vars$label <- gsub("\\(([^\\)]+)\\)||Civilian labor force", "", get_vars$label)
get_vars$concept <- gsub("\\(([^\\)]+)\\)", "", get_vars$concept)
get_vars$label <- tolower(get_vars$label)
get_vars$concept <- tolower(get_vars$concept)
get_vars$label <- gsub(" ", "_", get_vars$label)
get_vars$concept <- gsub(" ", "_", get_vars$concept)
get_vars$label <- paste(get_vars$concept, get_vars$label, sep = "_")
get_vars$concept <- NULL

## ZCTA
# Close to Zip Code
zcta_county_match <- read.table(here::here("census", "Access", "R", "zcta_county_rel_10.txt"), # Get this file from census website
                                header = TRUE, sep = ",")

zcta_county_match <- zcta_county_match[zcta_county_match$STATE == 37 &
                                         zcta_county_match$COUNTY %in% counties$COUNTYFP, ]

start_time <- Sys.time()
wnc_zcta_data <- get_acs(geography = "zip code tabulation area",
                               variables = get_vars$name,
                               geometry = TRUE,
                               output = "wide")
Sys.time() - start_time
# 2 mins

wnc_zcta_data <- wnc_zcta_data[wnc_zcta_data$GEOID %in% zcta_county_match$ZCTA5,
                                           names(wnc_zcta_data) %in% c("NAME", "GEOID", "geometry", get_vars$name)]

## Counties
wnc_county_data <- reduce(
  map(counties$COUNTY, function(x) {
    get_acs(geography = "county", variables = get_vars$name,
            state = "NC", county = x, geometry = TRUE, output = "wide")
  }),
  rbind
)

wnc_county_data <- wnc_county_data[, names(wnc_county_data) %in% c("NAME", "GEOID", "geometry", get_vars$name)]

### Investigate

# Base Maps
ggplot() +
  geom_sf(data = wnc_county_data, col = "darkblue", fill = NA, lwd = 1.5) +
  geom_sf(data = wnc_zcta_data, col = "darkred", fill = NA) +
  ggtitle("Base Map") +
  theme_null()

leaflet() %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(data = wnc_county_data,
              popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = TRUE,
              smoothFactor = .5,
              fillColor = "white",
              color = "darkblue",
              weight = 2) %>%
  addPolygons(data = wnc_zcta_data,
              popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = TRUE,
              smoothFactor = .5,
              fillColor = "white",
              color = "darkred",
              weight = 1)

## Total Population
# Add in Density
wnc_county_data <- wnc_county_data %>%
  mutate(area = as.numeric(set_units(st_area(geometry), mi^2)),
  density = as.numeric(B01003_001E / set_units(st_area(geometry), mi^2)))

wnc_zcta_data <- wnc_zcta_data %>%
  mutate(area = as.numeric(set_units(st_area(geometry), mi^2)),
         density = as.numeric(B01003_001E / set_units(st_area(geometry), mi^2)))

# County
county_1 <- ggplot() +
  geom_sf(data = wnc_county_data, aes(col = B01003_001E, fill = B01003_001E)) +
  scale_color_continuous(name = "Total Population",
                         labels = comma) +
  scale_fill_continuous(name = "Total Population",
                        labels = comma) +
  theme(legend.position = c(.25, .75)) +
  theme_null()

county_2 <- ggplot() +
  geom_sf(data = wnc_county_data, aes(col = density, fill = density)) +
  scale_color_continuous(name = "Population Density",
                         labels = comma) +
  scale_fill_continuous(name = "Population Density",
                        labels = comma) +
  theme(legend.position = c(.25, .75)) +
  theme_null()

plot_grid(county_1, county_2)

zcta_1 <- ggplot() +
  geom_sf(data = wnc_zcta_data, aes(col = B01003_001E, fill = B01003_001E)) +
  scale_color_continuous(name = "Total Population",
                         labels = comma) +
  scale_fill_continuous(name = "Total Population",
                        labels = comma) +
  theme(legend.position = c(.25, .75)) +
  theme_null()

zcta_2 <- ggplot() +
  geom_sf(data = wnc_zcta_data, aes(col = density, fill = density)) +
  scale_color_continuous(name = "Population Density",                         
                         labels = comma) +  
  scale_fill_continuous(name = "Population Density",
                        labels = comma) +  
  theme(legend.position = c(.25, .75)) +  
  theme_null()

plot_grid(zcta_1, zcta_2)

## Race/Ethnicity Break Out
# Explain Race vs Ethnicity
race_eths <- wnc_county_data[, names(wnc_county_data) %in%
                                     c("NAME", "GEOID", "geometry",
                                       "B02001_001E", "B03002_003E", "B03002_004E", "B03002_005E",
                                       "B03002_006E", "B03002_007E", "B03002_008E", "B03002_010E",
                                       "B03002_011E", "B03002_012E")]

race_eths_lookup <- data.frame(variable = c("B02001_001E", "B03002_003E", "B03002_004E", "B03002_005E",
                                            "B03002_006E", "B03002_007E", "B03002_008E", "B03002_010E",
                                            "B03002_011E", "B03002_012E"),
                               race_eth = c("Total", "White", "Black", "Native_American",
                                            "Asian", "Other", "Other", "Other", "Other",
                                            "Hispanic"), stringsAsFactors = FALSE)
race_eths <- race_eths %>%
  gather(variable, estimate, B02001_001E:B03002_012E) %>%
  inner_join(., race_eths_lookup, by = "variable") %>%
  group_by(GEOID, NAME, race_eth) %>%  
  summarize(estimate = sum(estimate)) %>%
  filter(race_eth != 'Total') %>%
  mutate(estimate_prop = estimate / sum(estimate),
         race_eth = factor(race_eth, levels = c("Asian", "Black", "Hispanic", "Native_American", "White", "Other")))

# Race/Ethnicity Totals
ggplot() +
  geom_sf(data = race_eths, aes(col = estimate, fill = estimate)) +
  scale_color_continuous(name = "Race/Ethnicity Totals",                     
                         labels = comma) +
  scale_fill_continuous(name = "Race/Ethnicity Totals",
                        labels = comma) +
  facet_wrap(~race_eth) +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 25),
         color = guide_colorbar(barwidth = 25)) +
  theme_null()

# Race/Ethnicity Proportion within County
ggplot() +
  geom_sf(data = race_eths, aes(col = estimate_prop, fill = estimate_prop)) +
  scale_color_continuous(name = "Race/Ethnicity Proportion within County") +
  scale_fill_continuous(name = "Race/Ethnicity Proportion within County") +
  facet_wrap(~race_eth) +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 25),
         color = guide_colorbar(barwidth = 25)) +
  theme_null()

# County Proportion within Race/Ethnicity
race_eths <- race_eths %>%
  group_by(race_eth) %>%
  mutate(county_prop = estimate / sum(estimate))

ggplot() +
  geom_sf(data = race_eths, aes(col = county_prop, fill = county_prop)) +
  scale_color_continuous(name = "County Proportion within Race/Ethnicity", limits = c(0, .4)) +
  scale_fill_continuous(name = "County Proportion within Race/Ethnicity", limits = c(0, .4)) +
  facet_wrap(~race_eth) +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 25),
         color = guide_colorbar(barwidth = 25)) +
  theme_null()

# Stack bar
ggplot(data = race_eths, aes(NAME, estimate_prop, fill = race_eth)) +
  geom_bar(stat = 'identity') +
  xlab("County") +
  ylab("Proportion") +
  guides(fill = guide_legend(title = "Race/Ethnicity")) +
  coord_flip()

## High School Education
education <- wnc_zcta_data[, names(wnc_zcta_data) %in%
                                   c("NAME", "GEOID", "geometry",
                                     "B15003_001E", "B15003_002E", "B15003_003E", "B15003_004E",
                                     "B15003_005E", "B15003_006E", "B15003_007E", "B15003_008E",
                                     "B15003_009E", "B15003_010E", "B15003_011E", "B15003_012E",
                                     "B15003_013E", "B15003_014E", "B15003_015E", "B15003_016E",
                                     "B15003_017E", "B15003_018E", 'B15003_019E', 'B15003_020E',
                                     "B15003_021E", "B15003_022E", 'B15003_023E', 'B15003_024E',
                                     'B15003_025E')]
education <- education %>%
  mutate(Total = B15003_001E,
         under_HS = B15003_002E + B15003_003E + B15003_004E + B15003_005E +
           B15003_006E + B15003_007E + B15003_008E + B15003_009E + B15003_010E +
           B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
           B15003_016E,
         HS = B15003_017E + B15003_018E,
         over_HS = B15003_019E + B15003_020E + B15003_021E + B15003_022E +
           B15003_023E + B15003_024E + B15003_025E) %>%
  select(NAME, GEOID, geometry, under_HS, HS, over_HS, Total) %>%
  gather(education, level, c(under_HS, HS, over_HS)) %>%
  mutate(education = factor(education, levels = c("under_HS", "HS", "over_HS")),
         level_proportion = level / Total)

ggplot() +
  geom_sf(data = education, aes(col = level, fill = level)) +
  scale_color_continuous(name = "Education Levels Total") +
  scale_fill_continuous(name = "Education Levels Total") +
  facet_wrap(~education) +
  theme_null()

ggplot() +
  geom_sf(data = education, aes(col = level_proportion, fill = level_proportion)) +
  scale_color_continuous(name = "Education Levels Proportion", limits = c(0, 1)) +
  scale_fill_continuous(name = "Education Levels Proportion", limits = c(0, 1)) +
  facet_wrap(~education) +
  theme_null()

# County
education <- wnc_county_data[, names(wnc_county_data) %in%
                                     c("NAME", "GEOID", "geometry",
                                       "B15003_001E", "B15003_002E", "B15003_003E", "B15003_004E",
                                       "B15003_005E", "B15003_006E", "B15003_007E", "B15003_008E",
                                       "B15003_009E", "B15003_010E", "B15003_011E", "B15003_012E",
                                       "B15003_013E", "B15003_014E", "B15003_015E", "B15003_016E",
                                       "B15003_017E", "B15003_018E", 'B15003_019E', 'B15003_020E',
                                       "B15003_021E", "B15003_022E", 'B15003_023E', 'B15003_024E',
                                       'B15003_025E')]
education <- education %>%
  mutate(Total = B15003_001E,
         under_HS = B15003_002E + B15003_003E + B15003_004E + B15003_005E +
           B15003_006E + B15003_007E + B15003_008E + B15003_009E + B15003_010E +
           B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E +
           B15003_016E,
         HS = B15003_017E + B15003_018E,
         over_HS = B15003_019E + B15003_020E + B15003_021E + B15003_022E +
           B15003_023E + B15003_024E + B15003_025E) %>%
  select(NAME, GEOID, geometry, under_HS, HS, over_HS, Total)

library(ggtern)
ggtern(data = education, aes(x = under_HS, y = HS, z = over_HS)) +
  geom_point() +
  theme_showarrows()
# Lines follow tick marks

education <- education %>%
  gather(education, level, c(under_HS, HS, over_HS)) %>%
  mutate(education = factor(education, levels = c("under_HS", "HS", "over_HS")),
         level_proportion = level / Total)

education$NAME <- factor(education$NAME,
                         levels = education$NAME[order(education[education$education=='HS',]$level_proportion)])
ggplot(data = education, aes(x = level_proportion,
                             y = NAME, color = education)) +
  scale_x_continuous(limits = c(0, 1)) +
  geom_point()
# under_HS and HS move together against over_HS
# due math due to HS always between under_HS and over_HS

education$NAME <- factor(education$NAME,
                         levels = education$NAME[order(education[education$education=='HS',]$Total)])
ggplot(data = education, aes(x = level, y = NAME, color = education)) +
  geom_point()

# Ratio of Income to Poverty Level in the Past 12 Months
income_poverty <- wnc_county_data[, names(wnc_county_data) %in%
                                          c("NAME", "GEOID", "geometry",
                                            "C17002_001E", "C17002_002E", "C17002_003E",
                                            "C17002_004E", "C17002_005E", "C17002_006E", "C17002_007E",
                                            "C17002_008E")]
income_poverty <- income_poverty %>%
  mutate(under_1 = (C17002_002E + C17002_003E),
         or_over_1 = (C17002_004E +C17002_005E + C17002_006E + C17002_007E + C17002_008E),
         under_1_prop = (C17002_002E + C17002_003E) / C17002_001E,
         or_over_1_prop = (C17002_004E +C17002_005E + C17002_006E + C17002_007E + C17002_008E) / C17002_001E,
         area = as.numeric(set_units(st_area(geometry), mi^2)),
         density = as.numeric((C17002_002E + C17002_003E) / set_units(st_area(geometry), mi^2))) %>%
  select(GEOID, NAME, under_1, or_over_1, under_1_prop, or_over_1_prop,
         area, density, geometry)
ggplot() +
  geom_sf(data = income_poverty, aes(col = under_1, fill = under_1)) +
  scale_color_continuous(name = "Ratio of Income to Poverty Level Under 1") +
  scale_fill_continuous(name = "Ratio of Income to Poverty Level Under 1") +
  theme_null()

ggplot(data = income_poverty, aes(density, under_1_prop)) +
  geom_point() +
  geom_text(aes(label = gsub(" County, North Carolina", "", NAME),
                vjust = 1)) +
  ylab("Proportion of Individuals with Ratio of Income to Poverty Level Under 1") +
  xlab("Density of Individuals with Ratio of Income to Poverty Level Under 1")

## Median Household Income
ggplot(data = wnc_zcta_data, aes(col = B19013_001E, fill = B19013_001E)) +
  scale_color_continuous(name = "Median Household Income") +
  scale_fill_continuous(name = "Median Household Income") +
  geom_sf() +
  theme_null()

## Unemployed
unemployed <- wnc_county_data[, names(wnc_county_data) %in%
                                      c("NAME", "GEOID", "geometry",
                                        "B23025_001E", "B23025_002E", "B23025_003E",
                                        "B23025_005E", "B23025_006E", "B23025_007E")]
unemployed <- unemployed %>%
  mutate(in_force = (B23025_002E - B23025_006E),
         in_force_prop = (B23025_002E - B23025_006E) / (B23025_001E - B23025_006E),
         unemployed = B23025_005E,
         unemployed_prop = B23025_005E / B23025_003E)
county_1 <- ggplot() +
  geom_sf(data = unemployed, aes(col = unemployed_prop, fill = unemployed_prop)) +
  scale_color_continuous(name = "Unemployed in civilian labor force Proportion",
                         limits = c(0, .31)) +
  scale_fill_continuous(name = "Unemployed in civilian labor force Proportion",
                        limits = c(0, .31)) +
  theme_null()

unemployed <- wnc_zcta_data[, names(wnc_zcta_data) %in%
                                    c("NAME", "GEOID", "geometry",
                                      "B23025_001E", "B23025_002E", "B23025_003E",
                                      "B23025_005E", "B23025_006E", "B23025_007E")]
unemployed <- unemployed %>%
  mutate(in_force = (B23025_002E - B23025_006E),
         in_force_prop = (B23025_002E - B23025_006E) / (B23025_001E - B23025_006E),
         unemployed = B23025_005E,
         unemployed_prop = B23025_005E / B23025_003E)

zcta_1 <- ggplot() +
  geom_sf(data = unemployed, aes(col = unemployed_prop, fill = unemployed_prop)) +
  scale_color_continuous(name = "Unemployed in civilian labor force Proportion",
                         limits = c(0, .31)) +
  scale_fill_continuous(name = "Unemployed in civilian labor force Proportion",
                        limits = c(0, .31)) +
  theme_null()

plot_grid(county_1, zcta_1)
