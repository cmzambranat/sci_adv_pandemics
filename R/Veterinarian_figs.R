library(here)
library(tidyverse)
library(readxl)
library(lmodel2)
library(scales)
library(ggrepel)
library(countrycode)
library(styler)

# Global options
theme_set(
  theme_bw(base_size = 18)
)


# read data
# World Bank data country classification
wb_class = read_xls(here("data/worldbank_classification.xls"), sheet = "Groups") %>%
  filter(GroupCode == 'HIC' | GroupCode == 'LIC' | GroupCode == 'LMC' | GroupCode == 'UMC')

# Data provided by A. Dobson
# Add "latin1" encoding to remove unicode characters
# Remove . from column name
# Convert upper case to lower case in Country column

vet_data =  read_csv(here('data/VetPerXNation2Reduced.csv'),
                     locale = readr::locale(encoding = "latin1")) %>%
  mutate(plot_name = str_replace_all(Name, "[.]", "")) %>%
  mutate(
    Country = str_to_title(Country),
    stand_country = str_to_title(rangeBuilder::standardizeCountry(Country, fuzzyDist = 25)),
    is_bold = case_when(
      stand_country == 'Spain' ~ 'bold',
      stand_country == 'Uruguay' ~ 'bold',
      stand_country == 'Falkland Islands' ~ 'bold',
      stand_country == 'United States' ~ 'bold',
      stand_country == 'United Kingdom' ~ 'bold',
      stand_country == 'France' ~ 'bold',
      stand_country == 'Venezuela' ~ 'bold',
      stand_country == 'Canada' ~ 'bold',
      stand_country == 'Mongolia' ~ 'bold',
      stand_country == 'Cuba' ~ 'bold',
      TRUE ~ 'plain'),
    stand_country = case_when(
      Country == 'Sthelena' ~ 'Saint Helena',
      Country == 'Dr Congo' ~ 'Democratic Republic of the Congo',
      TRUE ~ stand_country
    ),
    continent = countrycode(stand_country, origin = 'country.name', destination = 'continent'),
    region = countrycode(stand_country, origin = 'country.name', destination = 'region'),
    iso3c = countrycode(stand_country, origin = 'country.name', destination = 'iso3c'),
    new_country = case_when(
      stand_country == 'Spain' ~ '(1) Spain',
      stand_country == 'Uruguay' ~ '(2) Uruguay',
      stand_country == 'Falkland Islands' ~ '(3) Falkland Islands',
      stand_country == 'United States' ~ '(4) USA',
      stand_country == 'United Kingdom' ~ '(5) UK',
      stand_country == 'France' ~ '(6) France',
      stand_country == 'Venezuela' ~ '(7) Venezuela',
      stand_country == 'Canada' ~ '(8) Canada',
      stand_country == 'Mongolia' ~ '(9) Mongolia',
      stand_country == 'Cuba' ~ '(10) Cuba',
      stand_country == 'Bosnia And Herzegovina' ~ 'BIH',
      TRUE ~ stand_country)
    ) %>%
  left_join(wb_class, by = c('iso3c' = 'CountryCode')) %>%
  mutate(GroupName = case_when(
    iso3c == 'FLK' ~ 'High income',
    iso3c == 'GUF' ~ 'High income',
    iso3c == 'GLP' ~ 'High income',
    iso3c == 'MTQ' ~ 'High income',
    iso3c == 'MYT' ~ 'High income',
    iso3c == 'REU' ~ 'High income',
    iso3c == 'SHN' ~ 'High income',
    TRUE ~ GroupName
  ))
    

# Fit MA Type II regression
# Code provided by A. Dobson

VetxPopFit2 <- lmodel2(log(VetsStaff) ~ log(Pop20), data = vet_data, nperm = 99 )
VetxPopFit2
##
MAFit <- numeric(300)
MAFitU <- numeric(300)
MAFitL <- numeric(300)
MAFit <- (10^-2.6578) * vet_data$Pop20^0.8551

# Plot Number of Veterinarians, Population

vetxpop =
ggplot(vet_data) +
  geom_point(aes(Pop20, VetsStaff, colour = 'black')) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  geom_line(aes(Pop20, MAFit, colour = 'red')) +
  xlab("Population") +
  ylab("Number of Veterinary Staff") +
  scale_color_brewer(type = "qual",
                     palette = 'Set1',
                     labels = c("Observed data", "Type II: 0.855")) +
  theme(legend.position = c(0.8, 0.2),
        legend.title = element_blank(),
        legend.key = element_blank(),
        legend.background = element_blank(),
        text = element_text(
          face = 'bold',
          family = 'Helvetica'
        )
  ) +
  annotation_logticks()

# 
ggsave(here("figures/vet_pop_size.png"), vetxpop, 
       device = 'png', width = 2, height = 2, dpi = 300, scale = 4)


# Vets of the world plot
# Uses development version ggrepel 0.9.0
# Need to print directly on disk, otherwise will produce an error

vets_world = 
vet_data %>%
  filter(Country != "Sint Maarten") %>%
  ggplot(aes(Area, VetsStaff / Pop20)) +
  geom_point(aes(color = GroupName)) +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  scale_y_log10(
    breaks = trans_breaks("log10", function(x) 10 ^ x),
    labels = trans_format("log10", math_format(10 ^ .x))
  ) +
  geom_text_repel(
    aes(label = new_country, fontface = is_bold),
    segment.color = 'grey50',
    max.overlaps = 11,
    #segment.curvature = -1e-20,
    size = 3,
    family = "Helvetica",
    point.padding = 0.05
  ) +
  xlab(bquote(bold("Area (km" ^ 2 ~ ")"))) +
  ylab("Veterinary Staff /  Citizen") +
  theme(
    text = element_text(
      face = 'bold',
      family = 'Helvetica'
    ),
    legend.position = c(0.2, 0.12),
    legend.title = element_blank()
  ) +
  annotation_logticks() +
  scale_color_brewer(
    type = "qual",
    palette = 'Set1',
    breaks = c(
      "Low income",
      "Lower middle income",
      "Upper middle income",
      "High income"
    ),
    labels = c(
      "Low-income economies",
      "Lower-middle-income economies",
      "Upper-middle-income economies",
      "High-income economies"
    )
  )

# Supplementary information, full size page
ggsave(here("figures/vet_citizen.png"), vets_world, 
       device = 'png', width = 11, height = 8.5, dpi = 300, scale = 1)
