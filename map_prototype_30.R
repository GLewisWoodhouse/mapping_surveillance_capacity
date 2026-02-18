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
glass_data <- read.csv('/Users/Guest_Aanensen/Downloads/GLASS data 2023.csv', check.names = FALSE)

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
if ("Iso3" %in% names(glass_data)) {
  names(glass_data)[names(glass_data) == "Iso3"] <- "iso3"
}
if ("ISO3" %in% names(glass_data)) {
  names(glass_data)[names(glass_data) == "ISO3"] <- "iso3"
}

if ("iso3" %in% names(glass_data)) {
  glass_data <- glass_data %>% mutate(iso3 = toupper(as.character(iso3)))
} else {
  glass_data <- glass_data %>% mutate(iso3 = get_iso3_from_df(.))
}


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

#Check whether the filter has worked
glass_data %>% filter(iso3 == "KOS") %>% select(matches("country"), iso3) %>% head()

#### Clean data and generate 2023 capacity metrics ####
names(glass_data) <- tolower(names(glass_data))
glass_data_2023 <- glass_data %>%
  filter(year == 2023)

combined_data <- glass_data_2023 %>%
  mutate(
    iso3 = toupper(trimws(as.character(iso3))),
    amr_ncc_metric = case_when(
      amr_ncc == "Established" ~ "Yes",
      amr_ncc == "Establishment in progress" ~ "Partial",
      amr_ncc == "Not established" ~ "No",
      amr_ncc == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    ),
    amr_nrl_metric = case_when(
      amr_nrl == "Established" ~ "Yes",
      amr_nrl == "Not established" ~ "No",
      amr_nrl == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    ),
    eqa_to_nrl_metric = case_when(
      eqa_to_nrl == "Provided" ~ "Yes",
      eqa_to_nrl == "Not provided" ~ "No",
      eqa_to_nrl == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    ),
    amr_ast_standards_metric = case_when(
      amr_ast_standards %in% c("CLSI", "EUCAST", "EUCAST|CLSI", "O") ~ "Yes",
      amr_ast_standards == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    ),
    amr_eqa_glass_labs_metric = case_when(
      amr_eqa_glass_labs == "Provided to all laboratories" ~ "Yes",
      amr_eqa_glass_labs == "Not provided to all laboratories" ~ "Partial",
      amr_eqa_glass_labs == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    iso3, year,
    amr_ncc_metric,
    amr_nrl_metric,
    eqa_to_nrl_metric,
    amr_ast_standards_metric,
    amr_eqa_glass_labs_metric
  )

#### MAPPING SECTION ####
##### Join the shapefile and combined data and ensure correct mapping of country codes ####
# Fix the missing joins and check for missing ISO codes
combined_data <- combined_data %>%
  mutate(iso3 = toupper(trimws(as.character(iso3))))

world_sf <- world_sf %>%
  mutate(adm0_a3 = toupper(trimws(as.character(adm0_a3))))

#Check ISO3 codes for differences between the two files before joining 
world_iso3 <- sort(unique(world_sf$adm0_a3))
data_iso3  <- sort(unique(combined_data$iso3))
setdiff(world_iso3, data_iso3) %>% head(30) %>% print() # Check the codes in the shapefile that aren't in the data 
setdiff(data_iso3, world_iso3) %>% head(30) %>% print()  #Check the codes in the data that aren't in the shapefile 

#Change codes that are inconsistent in the shapefile with the combined data file 
world_sf <- world_sf %>%
  mutate(
    iso3 = case_when(
      adm0_a3 == "SDS" ~ "SSD",  # South Sudan
      adm0_a3 == "PSX" ~ "PSE",  # Palestine
      TRUE ~ adm0_a3
    )
  )

# 1. Define Fleming ISO3 list
fleming_iso3_list <- c(
  "SEN", "SLE", "GHA", "NGA", "SWZ", "MWI", "KEN", "RWA", 
  "TZA", "UGA", "ZMB", "ZWE", "BGD", "BTN", "IND", "NPL", 
  "PAK", "LKA", "TLS", "IDN", "LAO", "PNG", "VNM"
)

# 2. Prepare Map Data
world_data <- world_sf %>% 
  mutate(iso3 = case_when(
    adm0_a3 == "SDS" ~ "SSD", 
    adm0_a3 == "PSX" ~ "PSE", 
    TRUE ~ adm0_a3
  )) %>%
  left_join(combined_data %>% mutate(iso3 = toupper(trimws(iso3))), by = "iso3")

# 3. Define Cropped Europe
europe_bbox <- st_bbox(c(xmin = -40, ymin = 20, xmax = 60, ymax = 85), crs = st_crs(world_data))
world_data_europe <- world_data %>% 
  filter(continent == "Europe") %>% 
  st_make_valid() %>%
  st_crop(europe_bbox)

#### Create mapping themes, folders and colour palettes  ####
# 4. Global Color Definitions
na_colour <- "grey85"
fleming_bg_colour <- "grey92" #Non flemming countries
fleming_na_colour <- "grey50"

colours <- list(
  ncc = c("Yes" = "#1B9E77", "Partial" = "#E6AB02", "No" = "#D95F02", "Not Enrolled" = "#7570B3"),
  nrl = c("Yes" = "#1B9E77", "No" = "#D95F02", "Not Enrolled" = "#7570B3"),
  eqa_nrl = c("Yes" = "#1B9E77", "No" = "#D95F02", "Not Enrolled" = "#7570B3"),
  ast = c("Yes" = "#1B9E77", "No" = "#D95F02", "Not Enrolled" = "#7570B3"),
  eqa_glass = c("Yes" = "#1B9E77", "Partial" = "#E6AB02", "No" = "#D95F02", "Not Enrolled" = "#7570B3")
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

#### Match column to palette function definition ####
match_pal <- function(col) {
  case_when(
    str_detect(col, "amr_ncc") ~ "ncc",
    str_detect(col, "amr_nrl") ~ "nrl",
    str_detect(col, "eqa_to_nrl") ~ "eqa_nrl",
    str_detect(col, "amr_ast_standards") ~ "ast",
    str_detect(col, "amr_eqa_glass_labs") ~ "eqa_glass",
    TRUE ~ "ncc"
  )
}

#### Create Make Map Function ####
make_map <- function(data, fill_col, title, palette, is_fleming = FALSE) {
  df <- data %>% mutate(.temp_fill = as.character(.data[[fill_col]]))
  
  if (is_fleming) {
    df <- df %>%
      mutate(.temp_fill = case_when(
        .temp_fill == "CONTEXT" ~ "CONTEXT",
        (is.na(.temp_fill) | .temp_fill == "NA") & (iso3 %in% fleming_iso3_list) ~ "NA_FLEMING",
        is.na(.temp_fill) | .temp_fill == "NA" ~ "NA",
        TRUE ~ .temp_fill
      ))
    levels_vec <- c("Yes", "Partial", "No", "Not Enrolled", "CONTEXT", "NA_FLEMING", "NA")
  } else {
    df <- df %>%
      mutate(.temp_fill = case_when(
        is.na(.temp_fill) | .temp_fill == "NA" ~ "NA",
        TRUE ~ .temp_fill
      ))
    levels_vec <- c("Yes", "Partial", "No", "Not Enrolled", "NA")
  }
  
  df$.temp_fill <- factor(df$.temp_fill, levels = levels_vec)
  
  # 3. Phantom Geometry
  missing_levels <- setdiff(levels_vec, unique(as.character(df$.temp_fill)))
  if (length(missing_levels) > 0) {
    phantom_sf <- st_sf(data.frame(.temp_fill = factor(missing_levels, levels = levels_vec)), 
                        geometry = st_sfc(lapply(missing_levels, function(x) st_polygon()), crs = st_crs(df)))
    df <- bind_rows(df, phantom_sf)
  }
  
  # 4. Color Assignment
  final_pal <- palette
  if (is_fleming) {
    final_pal["CONTEXT"] <- "#F5F5F5"
    final_pal["NA_FLEMING"] <- "#707070"
  }
  final_pal["NA"] <- "#E0E0E0"
  
  # 5. Legend Display Logic
  label_vec <- setNames(levels_vec, levels_vec)
  if (is_fleming) {
    label_vec["CONTEXT"] <- "Non-Fleming"
    label_vec["NA_FLEMING"] <- "No Data"
  }
  
  if (is_fleming) {
    plot_breaks <- setdiff(levels_vec, "NA") 
  } else {
    plot_breaks <- levels_vec
  }
  plot_labels <- label_vec[plot_breaks]
  
  # 6. Plot
  ggplot(df) +
    geom_sf(aes(fill = .temp_fill), colour = "grey20", linewidth = 0.1) +
    scale_fill_manual(
      values = final_pal, 
      breaks = plot_breaks, 
      labels = plot_labels, 
      drop = FALSE,
      guide = guide_legend(override.aes = list(colour = "grey20", linewidth = 0.2))
    ) +
    labs(title = title, fill = NULL) + 
    theme_map()
}


#### Create Panel Functions ####
make_panel <- function(data, region_name, palettes, is_fleming = FALSE) {
  plots <- list(
    make_map(data, "amr_ncc_metric", "AMR NCC", palettes$ncc, is_fleming),
    make_map(data, "amr_nrl_metric", "AMR NRL", palettes$nrl, is_fleming),
    make_map(data, "eqa_to_nrl_metric", "EQA To NRL", palettes$eqa_nrl, is_fleming),
    make_map(data, "amr_ast_standards_metric", "AMR AST Standards", palettes$ast, is_fleming),
    make_map(data, "amr_eqa_glass_labs_metric", "AMR EQA (GLASS Labs)", palettes$eqa_glass, is_fleming)
  )
  plot_grid(
    ggdraw() + draw_label(region_name, fontface = "bold", size = 18), 
    plot_grid(plotlist = plots, ncol = 3), 
    ncol = 1, rel_heights = c(0.08, 1)
  )
}
#### Set up the Execution loop ####
regions <- list(
  "Europe"="Europe", 
  "Asia"="Asia", 
  "Africa"="Africa", 
  "Fleming"=c("Africa","Asia"), 
  "Central & South America"=c("South America", "Central America", "Caribbean")
)
all_regions <- c("Worldwide", names(regions))

metrics_capacity <- list(
  list(col = "amr_ncc_metric", title = "AMR_NCC"), 
  list(col = "amr_nrl_metric", title = "AMR_NRL"), 
  list(col = "eqa_to_nrl_metric", title = "EQA_to_NRL"), 
  list(col = "amr_ast_standards_metric", title = "AMR_AST_standards"),
  list(col = "amr_eqa_glass_labs_metric", title = "AMR_EQA_GLASS_labs")
)

#### Run execution loop ####
output_root <- "GLASS_2023_capacity_maps"
individual_dir <- file.path(output_root, "individual_maps")
panel_dir <- file.path(output_root, "capacity_panels")
dir.create(individual_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(panel_dir, recursive = TRUE, showWarnings = FALSE)

for (r in all_regions) {
  
  # 1. First, subset the data as you currently do
  if (r == "Worldwide") {
    plot_data <- world_data
  } else if (r == "Europe") {
    plot_data <- world_data_europe
  } else if (r == "Central & South America") {
    plot_data <- world_data %>% 
      filter(continent == "South America" | subregion == "Central America" | subregion == "Caribbean")
  } else {
    plot_data <- world_data %>% filter(continent %in% regions[[r]])
  }
  
  current_is_fleming <- (r == "Fleming")
  
  if (current_is_fleming) {
    # This turns all non-Fleming countries in Africa/Asia to "CONTEXT"
    plot_data <- plot_data %>% 
      mutate(across(ends_with("_metric"), ~if_else(iso3 %in% fleming_iso3_list, as.character(.x), "CONTEXT")))
  }
  
  # Save Individual Capacity Maps
  for (m in metrics_capacity) {
    p <- make_map(plot_data, m$col, paste(r, "–", m$title), colours[[match_pal(m$col)]], 
                  is_fleming = current_is_fleming)
    ggsave(file.path(individual_dir, paste0(r, "_", m$col, ".png")), p, width = 8, height = 5)
  }
  
  # Save Regional Panels
  ggsave(file.path(panel_dir, paste0(r, "_capacity_panel.png")), 
         make_panel(plot_data, r, colours, is_fleming = current_is_fleming),
         width = 14, height = 10, bg = "white")
}
