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
      amr_ncc == "Established" ~ "Established",
      amr_ncc == "Establishment in progress" ~ "In Progress",
      amr_ncc == "Not established" ~ "Not Established",
      amr_ncc == "Not_enrolled" ~ "Not Enrolled",
      amr_ncc == "Not reported" ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    amr_nrl_metric = case_when(
      amr_nrl == "Established" ~ "Established",
      amr_nrl == "Not established" ~ "Not Established",
      amr_nrl == "Not_enrolled" ~ "Not Enrolled",
      amr_nrl == "Not reported" ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    eqa_to_nrl_metric = case_when(
      eqa_to_nrl == "Provided" ~ "Provided",
      eqa_to_nrl == "Not provided" ~ "Not Provided",
      eqa_to_nrl == "Not_enrolled" ~ "Not Enrolled",
      eqa_to_nrl == "Not reported" ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    amr_ast_standards_metric = case_when(
      amr_ast_standards == "CLSI" ~ "CLSI",
      amr_ast_standards == "EUCAST" ~ "EUCAST",
      amr_ast_standards == "EUCAST|CLSI" ~ "EUCAST and CLSI",
      amr_ast_standards == "O" ~ "Other Standard",
      amr_ast_standards == "Not_enrolled" ~ "Not Enrolled",
      amr_ast_standards == "Not reported" ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    amr_eqa_glass_labs_metric = case_when(
      amr_eqa_glass_labs == "Provided to all laboratories" ~ "Provided to All Laboratories",
      amr_eqa_glass_labs == "Not provided to all laboratories" ~ "Not Provided to All Laboratories",
      amr_eqa_glass_labs == "Not_enrolled" ~ "Not Enrolled",
      amr_eqa_glass_labs == "Not reported" ~ "Not Reported",
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
  ncc = c(
    "Established" = "#1B9E77",
    "In Progress" = "#E6AB02",
    "Not Established" = "#D95F02",
    "Not Enrolled" = "#7570B3",
    "Not Reported" = "#9E9E9E"
  ),
  nrl = c(
    "Established" = "#1B9E77",
    "Not Established" = "#D95F02",
    "Not Enrolled" = "#7570B3",
    "Not Reported" = "#9E9E9E"
  ),
  eqa_nrl = c(
    "Provided" = "#1B9E77",
    "Not Provided" = "#D95F02",
    "Not Enrolled" = "#7570B3",
    "Not Reported" = "#9E9E9E"
  ),
  ast = c(
    "EUCAST" = "#1B9E77",
    "CLSI" = "#2C7FB8",
    "EUCAST and CLSI" = "#66A61E",
    "Other Standard" = "#E6AB02",
    "Not Enrolled" = "#7570B3",
    "Not Reported" = "#9E9E9E"
  ),
  eqa_glass = c(
    "Provided to All Laboratories" = "#1B9E77",
    "Not Provided to All Laboratories" = "#E6AB02",
    "Not Enrolled" = "#7570B3",
    "Not Reported" = "#9E9E9E"
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

#### Create Make Map Function ####
make_map <- function(data, fill_col, title, palette, levels_vec, label_map = NULL, is_fleming = FALSE) {
  df <- data %>% mutate(.temp_fill = as.character(.data[[fill_col]]))
  
  if (is_fleming) {
    df <- df %>%
      mutate(.temp_fill = case_when(
        .temp_fill == "CONTEXT" ~ "CONTEXT",
        (is.na(.temp_fill) | .temp_fill == "NA") & (iso3 %in% fleming_iso3_list) ~ "NA_FLEMING",
        is.na(.temp_fill) | .temp_fill == "NA" ~ "NA",
        TRUE ~ .temp_fill
      ))
    levels_vec <- c(levels_vec, "CONTEXT", "NA_FLEMING", "NA")
  } else {
    df <- df %>%
      mutate(.temp_fill = case_when(
        is.na(.temp_fill) | .temp_fill == "NA" ~ "NA",
        TRUE ~ .temp_fill
      ))
    levels_vec <- c(levels_vec, "NA")
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
  if (!is.null(label_map)) {
    common_labs <- intersect(names(label_map), names(label_vec))
    label_vec[common_labs] <- label_map[common_labs]
  }
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
make_panel <- function(data, region_name, palettes, metric_definitions, is_fleming = FALSE) {
  plots <- lapply(metric_definitions, function(m) {
    make_map(
      data = data,
      fill_col = m$col,
      title = m$title,
      palette = palettes[[m$palette]],
      levels_vec = m$levels,
      is_fleming = is_fleming
    )
  })
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
  list(
    col = "amr_ncc_metric",
    title = "AMR National Coordinating Centre (NCC)",
    palette = "ncc",
    levels = c("Established", "In Progress", "Not Established", "Not Enrolled", "Not Reported")
  ),
  list(
    col = "amr_nrl_metric",
    title = "AMR National Reference Laboratory (NRL)",
    palette = "nrl",
    levels = c("Established", "Not Established", "Not Enrolled", "Not Reported")
  ),
  list(
    col = "eqa_to_nrl_metric",
    title = "External Quality Assessment Provided to NRL",
    palette = "eqa_nrl",
    levels = c("Provided", "Not Provided", "Not Enrolled", "Not Reported")
  ),
  list(
    col = "amr_ast_standards_metric",
    title = "AST Standards Used for AMR Testing",
    palette = "ast",
    levels = c("EUCAST", "CLSI", "EUCAST and CLSI", "Other Standard", "Not Enrolled", "Not Reported")
  ),
  list(
    col = "amr_eqa_glass_labs_metric",
    title = "EQA Coverage Across GLASS Laboratories",
    palette = "eqa_glass",
    levels = c("Provided to All Laboratories", "Not Provided to All Laboratories", "Not Enrolled", "Not Reported")
  )
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
    p <- make_map(
      data = plot_data,
      fill_col = m$col,
      title = paste(r, "–", m$title),
      palette = colours[[m$palette]],
      levels_vec = m$levels,
      is_fleming = current_is_fleming
    )
    ggsave(file.path(individual_dir, paste0(r, "_", m$col, ".png")), p, width = 8, height = 5)
  }
  
  # Save Regional Panels
  ggsave(file.path(panel_dir, paste0(r, "_capacity_panel.png")), 
         make_panel(plot_data, r, colours, metrics_capacity, is_fleming = current_is_fleming),
         width = 14, height = 10, bg = "white")
}
