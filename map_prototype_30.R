#rm(list = ls())

#### Libraries ####
library(tidyverse)
library(data.table)
library(readxl)
library(countrycode)
library(sf)
library(rnaturalearth)
library(patchwork)
library(viridis)
library(cowplot)
library(forcats)

####Set up working environment ####
setwd('/Users/Guest_Aanensen/Documents/prototype_maps_30by30/')
glass_data <- read_excel('NAP_Glass_data.xlsx')
NAP_data <- read_excel('National Genomic Landscape.xlsx')
all_genomic_data <- read.csv('all_priority_genomes.csv')
critical_genomic_data <- read.csv('critical_priority_genomes.csv')
klebsiella_genomic_data <- read.csv('klebsiella_genomes.csv')
tracss_data <- read.csv('TrACSS-2025-Data-export-01102025.csv')

####Pull shapefiles from natural earth ####
world_sf <- rnaturalearth::ne_countries(scale = 'medium', returnclass = 'sf')
africa_sf <- filter(world_sf, continent == "Africa")
europe_sf <- filter(world_sf, continent == "Europe")
asia_sf   <- filter(world_sf, continent == "Asia")
central_south_america_sf <- filter(world_sf, continent == "South America" | subregion == "Central America") #Using this method - French Guiana is removed
fleming_sf <- filter(world_sf, continent == "Africa" | continent == "Asia") #So we can map the flaming fund countries

# Test the shapefiles - ggplot(asia_sf) + geom_sf() + ggtitle("Natural Earth: countries (50m)")

#### Set up Iso codes for country matching ####

get_iso3_from_df <- function(df) {
  iso_cols <- c("iso3","ISO3","iso_a3","iso_a3c","ISO_A3","ISO_A3C")
  for (cname in iso_cols) if (cname %in% names(df)) {
    v <- toupper(as.character(df[[cname]]))
    v[nchar(v) != 3] <- NA
    return(v)
  }
  # try country name columns
  name_cols <- c("country","country_name","Country","admin","COUNTRY")
  for (cname in name_cols) if (cname %in% names(df)) {
    return(countrycode(df[[cname]], origin = "country.name", destination = "iso3c"))
  }
  # fallback: NA
  rep(NA_character_, nrow(df))
}

# Add iso3 columns if missing
glass_data <- glass_data %>% mutate(iso3 = get_iso3_from_df(.))
NAP_data   <- NAP_data   %>% mutate(iso3 = get_iso3_from_df(.))
all_genomic_data <- all_genomic_data %>% mutate(iso3 = get_iso3_from_df(.))
critical_genomic_data <- critical_genomic_data %>% mutate(iso3 = get_iso3_from_df(.))
klebsiella_genomic_data <- klebsiella_genomic_data %>% mutate(iso3 = get_iso3_from_df(.))
tracss_data <- tracss_data %>% mutate(iso3 = get_iso3_from_df(.))


#check for missing ISO values
missing_iso_glass <- glass_data %>% filter(is.na(iso3)) %>% head(10)
if (nrow(missing_iso_glass) > 0) {
  message("Some rows in glass_data have missing ISO3 — inspect 'missing_iso_glass' in your environment.")
  print(missing_iso_glass %>% select(matches("country|name"), everything()) )
}


#Allocate a manual code for Kosovo in all datasets - natural earth uses 'XKX' for KOSOVO
add_kosovo_iso3 <- function(df) {
  # find a country name column
  country_cols <- c("country", "country_name", "admin", "name","Country")
  country_col <- intersect(country_cols, names(df))[1]
  
  if (!is.na(country_col)) {
    df <- df %>%
      mutate(
        iso3 = if_else(
          tolower(.data[[country_col]]) %in% c("kosovo", "republic of kosovo"),
          "KOS",
          iso3
        )
      )
  }
  df
}

glass_data   <- add_kosovo_iso3(glass_data)
NAP_data     <- add_kosovo_iso3(NAP_data)
all_genomic_data <- add_kosovo_iso3(all_genomic_data)
critical_genomic_data <- add_kosovo_iso3(critical_genomic_data)
klebsiella_genomic_data <- add_kosovo_iso3(klebsiella_genomic_data)
tracss_data <- add_kosovo_iso3(tracss_data)

#Check whether the filter has worked
glass_data %>% filter(iso3 == "KOS") %>% select(matches("country"), iso3) %>% head()
NAP_data   %>% filter(iso3 == "KOS") %>% select(matches("country"), iso3) %>% head()
all_genomic_data %>% filter(iso3 == "KOS") %>% select(matches("country_name"), iso3) %>% head()
tracss_data %>% filter(iso3 == "KOS") %>% select(matches("Country"), iso3) %>% head()

##### Bring input datasets into one - using the NAP_data to join - I only want country information included if they are in the NAP_data ####
# Change column names to lower case for harmonisation
names(glass_data) <- tolower(names(glass_data))
names(NAP_data)   <- tolower(names(NAP_data))
names(all_genomic_data) <- tolower(names(all_genomic_data))
names(tracss_data) <- tolower(names(tracss_data))
names(critical_genomic_data) <- tolower(names(critical_genomic_data))
names(klebsiella_genomic_data) <- tolower(names(klebsiella_genomic_data))

#Combine and rename genomic data 

all_genomic_data <- all_genomic_data %>% 
  left_join(critical_genomic_data, by = 'iso3') %>% 
  left_join(klebsiella_genomic_data, by = 'iso3') %>% 
  select(-c('country_name.y','country_name')) %>% 
  rename_with(~c('country','number_of_priority_genomes','iso3','number_of_critical_genomes','number_of_klebsiella_genomes'))

#Join data together using the ISO code matching - join data onto the nap_data - remove duplica
#Select columns needed from each dataframe before joining 

NAP_data <- NAP_data %>% 
  select(c('row id', 'country', 'iso3','who region','latitude', 'longitude', 'glass_amr_enrolled', 'amr action plan'))

glass_data <- glass_data %>% 
  select(c('country','iso3','who_region', 'amr_enrolled', 'glass_status', 'nap_status'))

#genomic data doesn't need specific columns selected

tracss_data <- tracss_data %>% 
  select(c(1:3,21,22,124,130)) %>% 
  rename_with(~c('nap_status','nap_type','surveillance_network','national_reference_lab'), .cols = c(4,5,6,7))


combined_data <- NAP_data %>% 
  left_join(glass_data, by = 'iso3') %>% 
  select(-country.y) %>% 
  rename(country = country.x) %>% 
  left_join(all_genomic_data, by = 'iso3') %>% 
  select(-country.y) %>% 
  left_join(tracss_data, by = 'iso3') %>% 
  select(-c('country','who_region.y','who region')) %>% 
  rename_with(~c('country','who_region','nap_status_glass','nap_status_tracss'),.cols = c(2,8,11,15)) %>% 
  select(-c('amr_enrolled')) #Added after checking concordance and validity of the two AMR_enrolled columns

#### Clean combined data and generate metrics ####

#Create a metric column for each map 
combined_data <- combined_data %>% 
  mutate(
    glass_status_metric = case_when(
      str_detect(glass_status, "Not Enrolled") ~ "No",
      str_detect(glass_status, "Enrolled") ~ "Yes",
      TRUE ~ NA_character_
    )
  ) 

combined_data <- combined_data %>% 
  mutate(
    glass_data_metric = case_when(
      str_detect(glass_status, "Not Enrolled") ~ "Not Enrolled",
      str_detect(glass_status, "Enrolled") & 
        str_detect(glass_status, "Data submitted") ~ "Yes",
      str_detect(glass_status, "Enrolled") & 
        str_detect(glass_status, "No data submitted") ~ "No",
      TRUE ~ NA_character_
    )
  )

combined_data <- combined_data %>% 
  mutate(
    national_reference_lab_metric = case_when(
      str_detect(national_reference_lab, "^Yes") ~ "Yes",
      str_detect(national_reference_lab, "^No")  ~ "No",
      TRUE                                       ~ NA_character_
    )
  )

combined_data <- combined_data %>% 
  mutate(
    surveillance_network_metric = case_when(
      str_detect(surveillance_network, "^[AB]") ~ "No",
      str_detect(surveillance_network, "^C")     ~ "Partial",
      str_detect(surveillance_network, "^[DE]")  ~ "Yes",
      TRUE                                       ~ NA_character_
    )
  )

make_genome_metric <- function(x) {
  num <- x %>%
    str_trim() %>%
    na_if("") %>%
    { gsub(",", "", .) } %>%         # remove commas
    { gsub("[^0-9]", "", .) } %>%    # keep only digits
    as.numeric()
  
  # Create AMR.Watch style bins - have not included single genomes
  case_when(
    is.na(num)                         ~ NA_character_,
    num == 0                           ~ NA_character_,   # preserve your previous behaviour: treat 0 as NA
    num >= 1     & num <= 50           ~ "1-50",
    num >= 51    & num <= 100          ~ "50-100",
    num >= 101   & num <= 500          ~ "100-500",
    num >= 501   & num <= 1000         ~ "500-1000",
    num >= 1001  & num <= 5000         ~ "1000-5000",
    num >= 5001  & num <= 10000        ~ "5000-10000",
    num >= 10001 & num <= 50000        ~ "10000-50000",
    num >= 50001                        ~ "50000+",
    TRUE                                ~ NA_character_
  )
}

# Apply to the three genome columns in the combined data
combined_data <- combined_data %>%
  mutate(
    number_of_priority_genomes_metric = make_genome_metric(number_of_priority_genomes),
    number_of_critical_genomes_metric = make_genome_metric(number_of_critical_genomes),
    number_of_klebsiella_genomes_metric = make_genome_metric(number_of_klebsiella_genomes)
  )

#### MAPPING SECTION ####
##### Join the shapefile and combined data and ensure correct mapping of country codes ####
# Fix the missing joins and check for missing ISO codes
combined_data <- combined_data %>%
  mutate(iso3 = toupper(trimws(as.character(iso3))))

world_sf <- world_sf %>%
  mutate(adm0_a3 = toupper(trimws(as.character(adm0_a3))))

#Check iso codes for differences
world_iso3 <- sort(unique(world_sf$adm0_a3))
data_iso3  <- sort(unique(combined_data$iso3))

# Check the codes in the shapefile that aren't in the data 
setdiff(world_iso3, data_iso3) %>% head(30) %>% print()

#  Check the codes in the data that aren't in the shapefile 
setdiff(data_iso3, world_iso3) %>% head(30) %>% print()

#Change codes that are inconsistent in the shapefile
#Change codes that are inconsistent in the shapefile
world_sf <- world_sf %>%
  mutate(
    iso3 = case_when(
      adm0_a3 == "SDS" ~ "SSD",  # South Sudan
      adm0_a3 == "PSX" ~ "PSE",  # Palestine
      TRUE ~ adm0_a3
    )
  )

world_data <- world_sf %>%
  left_join(combined_data, by = "iso3")

####Create mapping themes, folders and colour palettes  ####
# Ensure output folders exist

# Make a cropped Europe object (only for European panels) 
# Filter to Europe first (so cropping won't accidentally keep e.g. Americas),
# then crop to a bbox with xmin = -40 (and sensible lat/lon bounds to keep islands).
europe_continents <- c("Europe")
europe_bbox <- st_bbox(c(xmin = -40, ymin = 20, xmax = 60, ymax = 85),
                       crs = st_crs(world_data))

# world_data_europe: first filter by continent, then crop geometry to bbox
world_data_europe <- world_data %>%
  filter(continent %in% europe_continents) %>%
  st_make_valid() %>%          # defensive: fix any invalid geometries
  st_crop(europe_bbox)

#Define the fleming countries
fleming_countries <- c(
  "Senegal","Sierra Leone","Ghana","Nigeria","Eswatini","Malawi","Kenya","Rwanda",
  "Tanzania","Uganda","Zambia","Zimbabwe","Bangladesh","Bhutan","India","Nepal",
  "Pakistan","Sri Lanka","Timor-Leste","Indonesia","Laos","Papua New Guinea","Vietnam"
)

# 1) Greys for maps (global + Fleming Fund specifics)
na_colour <- "grey85"
fleming_bg_colour <- "grey92"
fleming_na_colour <- "grey70"

colours <- list(
  glass = c("Yes" = "#1B9E77", "No" = "#D95F02"),
  glass_sub = c("Yes" = "#1B9E77", "No" = "#FDAE61","Not Enrolled" = "#F8766D"),
  nrl   = c("Yes" = "#2B8CBE", "No" = "#E41A1C"),
  net   = c("Yes" = "#4292C6", "Partial" = "#FDAE61", "No" = "#F8766D"),
  genomes = c(
    "1-50"="#F7FBFF", "50-100"="#DEEBF7", "100-500"="#C6DBEF", "500-1000"="#9ECAE1", 
    "1000-5000"="#6BAED6", "5000-10000"="#3182BD", "10000-50000"="#08519C", "50000+"="#08306B"
  )
)

# 5. Mapping Theme
theme_map <- function() {
  theme_void() +
    theme(
      plot.background   = element_rect(fill = "white", colour = NA),
      legend.position   = "right",
      plot.title        = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.margin       = margin(5,5,5,5, "mm")
    )
}

# 6. Helper: Match column to palette
match_pal <- function(col) {
  case_when(
    str_detect(col, "glass_status") ~ "glass", 
    str_detect(col, "glass_data") ~ "glass_sub", 
    str_detect(col, "national_reference") ~ "nrl", 
    str_detect(col, "surveillance") ~ "net", 
    TRUE ~ "genomes"
  )
}

# 7. Updated make_map with Dynamic "Non-Fleming" Legend
# 7. Updated make_map with Fixed Dynamic Legend Logic
make_map <- function(data, fill_col, title, palette) {
  df <- data %>% mutate(.temp_fill = as.character(.data[[fill_col]]))
  df$.temp_fill[is.na(df$.temp_fill)] <- "NA"
  
  # Define Levels
  if (fill_col %in% c("number_of_priority_genomes_metric", "number_of_critical_genomes_metric", "number_of_klebsiella_genomes_metric")) {
    levels_vec <- c("1-50", "50-100", "100-500", "500-1000", "1000-5000", "5000-10000", "10000-50000", "50000+", "CONTEXT", "NA")
  } else if (fill_col == "surveillance_network_metric") {
    levels_vec <- c("Yes", "Partial", "No", "CONTEXT", "NA")
  } else if (fill_col == "glass_data_metric") {
    levels_vec <- c("Yes", "No", "Not Enrolled", "CONTEXT", "NA")
  } else {
    levels_vec <- c("Yes", "No", "CONTEXT", "NA")
  }
  
  df$.temp_fill <- factor(df$.temp_fill, levels = levels_vec)
  
  # Ghost Geometry Logic
  missing_levels <- setdiff(levels_vec, unique(as.character(df$.temp_fill)))
  if (length(missing_levels) > 0) {
    phantom_sf <- st_sf(data.frame(.temp_fill = factor(missing_levels, levels = levels_vec)), 
                        geometry = st_sfc(lapply(missing_levels, function(x) st_polygon()), crs = st_crs(df)))
    df <- bind_rows(df, phantom_sf)
  }
  
  # --- FIXED LEGEND LOGIC ---
  is_fleming_map <- grepl("Fleming", title)
  
  # Define display labels
  label_vec <- setNames(levels_vec, levels_vec)
  label_vec["CONTEXT"] <- "Non-Fleming"
  
  # Ensure breaks and labels match in length
  if (is_fleming_map) {
    plot_breaks <- levels_vec
    plot_labels <- label_vec
  } else {
    # Remove CONTEXT from both breaks and labels for non-Fleming maps
    plot_breaks <- setdiff(levels_vec, "CONTEXT")
    plot_labels <- label_vec[plot_breaks] 
  }
  # --------------------------
  
  final_pal <- palette
  if (!"CONTEXT" %in% names(final_pal)) final_pal["CONTEXT"] <- fleming_bg_colour
  if (!"NA" %in% names(final_pal))      final_pal["NA"]      <- na_colour
  
  ggplot(df) +
    geom_sf(aes(fill = .temp_fill), colour = "grey20", linewidth = 0.1) +
    scale_fill_manual(
      values = final_pal, 
      breaks = plot_breaks, # Dynamically includes or excludes CONTEXT
      labels = plot_labels, # Matches length of plot_breaks
      drop = FALSE,
      guide = guide_legend(override.aes = list(colour = "grey20", linewidth = 0.2))
    ) +
    labs(title = title, fill = NULL) + 
    theme_map()
}
# 8. Panel Functions
make_panel <- function(data, region_name, palettes) {
  plots <- list(
    make_map(data, "glass_status_metric", "Enrolled in GLASS", palettes$glass),
    make_map(data, "glass_data_metric", "GLASS Data Submitted", palettes$glass_sub),
    make_map(data, "national_reference_lab_metric", "National Ref Lab", palettes$nrl),
    make_map(data, "surveillance_network_metric", "Sentinel Network", palettes$net)
  )
  plot_grid(
    ggdraw() + draw_label(region_name, fontface = "bold", size = 18), 
    plot_grid(plotlist = plots, ncol = 2), 
    ncol = 1, rel_heights = c(0.08, 1)
  )
}

make_genomics_panel <- function(data, region_name, palettes) {
  plots <- list(
    make_map(data, "number_of_priority_genomes_metric", "Priority Pathogen Genomes", palettes$genomes),
    make_map(data, "number_of_critical_genomes_metric", "Critical Pathogen Genomes", palettes$genomes)
  )
  plot_grid(
    ggdraw() + draw_label(paste(region_name, "Genomics"), fontface = "bold", size = 18), 
    plot_grid(plotlist = plots, ncol = 2), 
    ncol = 1, rel_heights = c(0.12, 1)
  )
}

# 9. EXECUTION LOOP
regions <- list(
  "Europe"="Europe", 
  "Asia"="Asia", 
  "Africa"="Africa", 
  "Fleming"=c("Africa","Asia"), 
  "Central & South America"=c("South America", "Central America", "Caribbean")
)
all_regions <- c("Worldwide", names(regions))

metrics_capacity <- list(
  list(col="glass_status_metric", title="Enrolled"), 
  list(col="glass_data_metric", title="Data"), 
  list(col="national_reference_lab_metric", title="Ref Lab"), 
  list(col="surveillance_network_metric", title="Network")
)

for (r in all_regions) {
  # Subset data
  plot_data <- if(r == "Worldwide") {
    world_data 
  } else if(r == "Europe") {
    world_data_europe 
  } else {
    world_data %>% filter(continent %in% regions[[r]])
  }
  
  # Fleming Logic: Mask non-funded countries as CONTEXT
  if (r == "Fleming") {
    plot_data <- plot_data %>% 
      mutate(across(ends_with("_metric"), ~if_else(iso3 %in% fleming_iso3_list, as.character(.x), "CONTEXT")))
  }
  
  # Save Individual Capacity Maps
  for (m in metrics_capacity) {
    p <- make_map(plot_data, m$col, paste(r, m$title), colours[[match_pal(m$col)]])
    ggsave(paste0("individual maps/", r, "_", m$col, ".png"), p, width = 8, height = 5)
  }
  
  # Save Regional Panels
  ggsave(paste0("regional panels/", r, "_panel.png"), make_panel(plot_data, r, colours), width = 14, height = 10, bg = "white")
  ggsave(paste0("regional panels/", r, "_genomics.png"), make_genomics_panel(plot_data, r, colours), width = 14, height = 6, bg = "white")
}



