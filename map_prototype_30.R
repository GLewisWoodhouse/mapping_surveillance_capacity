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
glass_data <- read_excel('NAP_Glass_data.xlsx')
tracss_data <- read.csv('TrACSS-2025-Data-export-01102025.csv', stringsAsFactors = FALSE)
all_genomic_data <- read.csv('all_priority_genomes.csv', stringsAsFactors = FALSE)
critical_genomic_data <- read.csv('critical_priority_genomes.csv', stringsAsFactors = FALSE)

glass_2023_path <- '/Users/Guest_Aanensen/Downloads/GLASS data 2023.csv'
if (file.exists(glass_2023_path)) {
  glass_data_2023 <- read.csv(glass_2023_path, check.names = FALSE, stringsAsFactors = FALSE)
} else {
  message("GLASS data 2023.csv not found in Downloads. NCC/GLASS-NRL metrics may be missing.")
  glass_data_2023 <- data.frame()
}

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
tracss_data <- tracss_data %>% mutate(iso3 = get_iso3_from_df(.))
all_genomic_data <- all_genomic_data %>% mutate(iso3 = get_iso3_from_df(.))
critical_genomic_data <- critical_genomic_data %>% mutate(iso3 = get_iso3_from_df(.))

if (nrow(glass_data_2023) > 0) {
  if ("Iso3" %in% names(glass_data_2023)) {
    names(glass_data_2023)[names(glass_data_2023) == "Iso3"] <- "iso3"
  }
  if ("ISO3" %in% names(glass_data_2023)) {
    names(glass_data_2023)[names(glass_data_2023) == "ISO3"] <- "iso3"
  }
  if ("iso3" %in% names(glass_data_2023)) {
    glass_data_2023 <- glass_data_2023 %>% mutate(iso3 = toupper(as.character(iso3)))
  } else {
    glass_data_2023 <- glass_data_2023 %>% mutate(iso3 = get_iso3_from_df(.))
  }
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

glass_data <- add_kosovo_iso3(glass_data)
tracss_data <- add_kosovo_iso3(tracss_data)
all_genomic_data <- add_kosovo_iso3(all_genomic_data)
critical_genomic_data <- add_kosovo_iso3(critical_genomic_data)
if (nrow(glass_data_2023) > 0) {
  glass_data_2023 <- add_kosovo_iso3(glass_data_2023)
}

#Check whether the filter has worked
glass_data %>% filter(iso3 == "KOS") %>% select(matches("country"), iso3) %>% head()

#### Clean data and generate combined capacity metrics ####
names(glass_data) <- tolower(names(glass_data))
if (nrow(glass_data_2023) > 0) {
  names(glass_data_2023) <- tolower(names(glass_data_2023))
}

glass_core <- glass_data %>%
  select(any_of(c("iso3", "glass_status", "data_sumbitted", "amr_enrolled"))) %>%
  mutate(iso3 = toupper(trimws(as.character(iso3))))

tracss_core <- tracss_data %>%
  select(c(1:3, 21, 22, 124, 130), iso3) %>%
  rename_with(~ c("nap_status", "nap_type", "surveillance_network", "national_reference_lab"), .cols = c(4, 5, 6, 7)) %>%
  select(iso3, surveillance_network, national_reference_lab) %>%
  mutate(iso3 = toupper(trimws(as.character(iso3))))

if (nrow(glass_data_2023) > 0) {
  glass_2023_core <- glass_data_2023 %>%
    filter(year == 2023) %>%
    select(any_of(c("iso3", "amr_ncc", "amr_nrl", "amr_eqa_glass_labs"))) %>%
    mutate(iso3 = toupper(trimws(as.character(iso3))))
} else {
  glass_2023_core <- data.frame(
    iso3 = character(),
    amr_ncc = character(),
    amr_nrl = character(),
    amr_eqa_glass_labs = character()
  )
}

combined_data <- glass_core %>%
  full_join(glass_2023_core, by = "iso3") %>%
  full_join(tracss_core, by = "iso3") %>%
  mutate(
    iso3 = toupper(trimws(as.character(iso3))),
    has_glass_profile = !is.na(glass_status) | !is.na(data_sumbitted) | !is.na(amr_enrolled),
    glass_enrollment_metric = case_when(
      str_detect(glass_status, "Not Enrolled") ~ "Not Enrolled",
      str_detect(glass_status, "Enrolled") ~ "Enrolled",
      str_detect(amr_enrolled, "^Y") ~ "Enrolled",
      str_detect(amr_enrolled, "^N") ~ "Not Enrolled",
      has_glass_profile ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    glass_data_submission_metric = case_when(
      str_detect(glass_status, "Not Enrolled") ~ "Not Enrolled",
      str_detect(glass_status, "Data submitted") ~ "Submitted",
      str_detect(glass_status, "No data submitted") ~ "No Submission",
      data_sumbitted == "submitted_data" ~ "Submitted",
      has_glass_profile ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    ncc_metric = case_when(
      amr_ncc == "Established" ~ "Established",
      amr_ncc == "Establishment in progress" ~ "In Progress",
      amr_ncc == "Not established" ~ "Not Established",
      amr_ncc == "Not_enrolled" ~ "Not Enrolled",
      amr_ncc == "Not reported" ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    nrl_glass_metric = case_when(
      amr_nrl == "Established" ~ "Established",
      amr_nrl == "Not established" ~ "Not Established",
      amr_nrl == "Not_enrolled" ~ "Not Enrolled",
      amr_nrl == "Not reported" ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    nrl_tracss_metric = case_when(
      str_detect(national_reference_lab, "^Yes") ~ "Established",
      str_detect(national_reference_lab, "^No") ~ "Not Established",
      str_detect(national_reference_lab, "^Unknown") ~ "Not Reported",
      TRUE ~ NA_character_
    ),
    nrl_metric = coalesce(nrl_glass_metric, nrl_tracss_metric),
    eqa_glass_metric = case_when(
      amr_eqa_glass_labs == "Provided to all laboratories" ~ "Provided to All Laboratories",
      amr_eqa_glass_labs == "Not provided to all laboratories" ~ "Not Provided to All Laboratories",
      amr_eqa_glass_labs == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    ),
    surveillance_network_metric = case_when(
      str_detect(surveillance_network, "^[AB]") ~ "No",
      str_detect(surveillance_network, "^C") ~ "Partial",
      str_detect(surveillance_network, "^[DE]") ~ "Yes",
      str_detect(surveillance_network, "^\\s*$") ~ "Not Reported",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    iso3,
    glass_enrollment_metric,
    glass_data_submission_metric,
    ncc_metric,
    nrl_metric,
    eqa_glass_metric,
    surveillance_network_metric
  )

make_genome_metric <- function(x) {
  num <- x %>%
    as.character() %>%
    str_trim() %>%
    na_if("") %>%
    { gsub(",", "", .) } %>%
    { gsub("[^0-9]", "", .) } %>%
    as.numeric()
  
  case_when(
    is.na(num) ~ NA_character_,
    num == 0 ~ NA_character_,
    num >= 1 & num <= 50 ~ "1-50",
    num >= 51 & num <= 100 ~ "50-100",
    num >= 101 & num <= 500 ~ "100-500",
    num >= 501 & num <= 1000 ~ "500-1000",
    num >= 1001 & num <= 5000 ~ "1000-5000",
    num >= 5001 & num <= 10000 ~ "5000-10000",
    num >= 10001 & num <= 50000 ~ "10000-50000",
    num >= 50001 ~ "50000+",
    TRUE ~ NA_character_
  )
}

genomics_data <- all_genomic_data %>%
  rename(number_of_priority_genomes = number_of_genomes) %>%
  left_join(
    critical_genomic_data %>% rename(number_of_critical_genomes = number_of_genomes) %>% select(iso3, number_of_critical_genomes),
    by = "iso3"
  ) %>%
  mutate(
    iso3 = toupper(trimws(as.character(iso3))),
    number_of_priority_genomes_metric = make_genome_metric(number_of_priority_genomes),
    number_of_critical_genomes_metric = make_genome_metric(number_of_critical_genomes)
  ) %>%
  select(iso3, number_of_priority_genomes_metric, number_of_critical_genomes_metric)

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

world_data_genomics <- world_sf %>% 
  mutate(iso3 = case_when(
    adm0_a3 == "SDS" ~ "SSD",
    adm0_a3 == "PSX" ~ "PSE",
    TRUE ~ adm0_a3
  )) %>%
  left_join(genomics_data %>% mutate(iso3 = toupper(trimws(iso3))), by = "iso3")

# 3. Define Cropped Europe
europe_bbox <- st_bbox(c(xmin = -40, ymin = 20, xmax = 60, ymax = 85), crs = st_crs(world_data))
world_data_europe <- world_data %>% 
  filter(continent == "Europe") %>% 
  st_make_valid() %>%
  st_crop(europe_bbox)

world_data_genomics_europe <- world_data_genomics %>% 
  filter(continent == "Europe") %>% 
  st_make_valid() %>%
  st_crop(europe_bbox)

#### Create mapping themes, folders and colour palettes  ####
# 4. Global Color Definitions
na_colour <- "grey85"
fleming_bg_colour <- "grey92" #Non flemming countries
fleming_na_colour <- "grey50"

colours <- list(
  enrollment = c(
    "Enrolled" = "#1B9E77",
    "Not Enrolled" = "#D95F02"
  ),
  submission = c(
    "Submitted" = "#1B9E77",
    "No Submission" = "#E6AB02",
    "Not Enrolled" = "#D95F02"
  ),
  ncc = c(
    "Established" = "#1B9E77",
    "In Progress" = "#E6AB02",
    "Not Established" = "#D95F02",
    "Not Enrolled" = "#7570B3"
  ),
  nrl = c(
    "Established" = "#1B9E77",
    "Not Established" = "#D95F02",
    "Not Enrolled" = "#7570B3"
  ),
  eqa_glass = c(
    "Provided to All Laboratories" = "#1B9E77",
    "Not Provided to All Laboratories" = "#E6AB02",
    "Not Enrolled" = "#7570B3"
  ),
  network = c(
    "Yes" = "#1B9E77",
    "Partial" = "#E6AB02",
    "No" = "#D95F02"
  ),
  genomics = c(
    "1-50" = "#F7FBFF",
    "50-100" = "#DEEBF7",
    "100-500" = "#C6DBEF",
    "500-1000" = "#9ECAE1",
    "1000-5000" = "#6BAED6",
    "5000-10000" = "#3182BD",
    "10000-50000" = "#08519C",
    "50000+" = "#08306B"
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
make_map <- function(data, fill_col, title, palette, levels_vec, label_map = NULL, is_fleming = FALSE, show_legend = TRUE) {
  df <- data %>% mutate(.temp_fill = as.character(.data[[fill_col]]))
  is_genomics <- str_detect(fill_col, "genomes")
  df <- df %>%
    mutate(.temp_fill = case_when(
      .temp_fill == "Not Reported" ~ "NA",
      TRUE ~ .temp_fill
    ))
  
  if (is_fleming) {
    if (is_genomics) {
      df <- df %>%
        mutate(.temp_fill = case_when(
          .temp_fill == "CONTEXT" ~ "CONTEXT",
          is.na(.temp_fill) | .temp_fill == "NA" ~ "NA",
          TRUE ~ .temp_fill
        ))
      levels_vec <- c(levels_vec, "CONTEXT", "NA")
    } else {
      df <- df %>%
        mutate(.temp_fill = case_when(
          .temp_fill == "CONTEXT" ~ "CONTEXT",
          (is.na(.temp_fill) | .temp_fill == "NA") & (iso3 %in% fleming_iso3_list) ~ "NA_FLEMING",
          is.na(.temp_fill) | .temp_fill == "NA" ~ "NA",
          TRUE ~ .temp_fill
        ))
      levels_vec <- c(levels_vec, "CONTEXT", "NA_FLEMING", "NA")
    }
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
    if (!is_genomics) {
      final_pal["NA_FLEMING"] <- "#707070"
    }
    final_pal["NA"] <- "#8F8F8F"
  } else {
    final_pal["NA"] <- "#E0E0E0"
  }
  
  # 5. Legend Display Logic
  label_vec <- setNames(levels_vec, levels_vec)
  if (!is.null(label_map)) {
    common_labs <- intersect(names(label_map), names(label_vec))
    label_vec[common_labs] <- label_map[common_labs]
  }
  if (is_fleming) {
    label_vec["CONTEXT"] <- "Non-Fleming"
    if (!is_genomics) {
      label_vec["NA_FLEMING"] <- "No Data"
    }
  }
  
  if (is_fleming) {
    if (is_genomics) {
      plot_breaks <- levels_vec
    } else {
      plot_breaks <- setdiff(levels_vec, "NA")
    }
  } else {
    plot_breaks <- levels_vec
  }
  plot_labels <- label_vec[plot_breaks]
  
  # 6. Plot
  legend_guide <- if (show_legend) {
    guide_legend(override.aes = list(colour = "grey20", linewidth = 0.2))
  } else {
    "none"
  }
  
  ggplot(df) +
    geom_sf(aes(fill = .temp_fill), colour = "grey20", linewidth = 0.1) +
    scale_fill_manual(
      values = final_pal, 
      breaks = plot_breaks, 
      labels = plot_labels, 
      drop = FALSE,
      guide = legend_guide
    ) +
    labs(title = title, fill = NULL) + 
    theme_map()
}


#### Create Panel Functions ####
make_panel <- function(data, region_name, palettes, metric_definitions, ncol = 3, is_fleming = FALSE) {
  plots <- lapply(metric_definitions, function(m) {
    make_map(
      data = data,
      fill_col = m$col,
      title = m$title,
      palette = palettes[[m$palette]],
      levels_vec = m$levels,
      is_fleming = is_fleming,
      show_legend = FALSE
    )
  })
  plot_grid(
    ggdraw() + draw_label(region_name, fontface = "bold", size = 18), 
    plot_grid(plotlist = plots, ncol = ncol), 
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
    col = "glass_data_submission_metric",
    title = "Data Submission to GLASS",
    palette = "submission",
    levels = c("Submitted", "No Submission", "Not Enrolled")
  ),
  list(
    col = "nrl_metric",
    title = "AMR National Reference Lab",
    palette = "nrl",
    levels = c("Established", "Not Established", "Not Enrolled")
  ),
  list(
    col = "eqa_glass_metric",
    title = "EQA at GLASS Labs",
    palette = "eqa_glass",
    levels = c("Provided to All Laboratories", "Not Provided to All Laboratories", "Not Enrolled")
  ),
  list(
    col = "surveillance_network_metric",
    title = "Surveillance Network",
    palette = "network",
    levels = c("Yes", "Partial", "No")
  )
)

metrics_genomics <- list(
  list(
    col = "number_of_priority_genomes_metric",
    title = "Priority Pathogen Genomes",
    palette = "genomics",
    levels = c("1-50", "50-100", "100-500", "500-1000", "1000-5000", "5000-10000", "10000-50000", "50000+")
  ),
  list(
    col = "number_of_critical_genomes_metric",
    title = "Critical Priority Pathogen Genomes",
    palette = "genomics",
    levels = c("1-50", "50-100", "100-500", "500-1000", "1000-5000", "5000-10000", "10000-50000", "50000+")
  )
)

#### Run execution loops ####
output_root <- "reworked capacity metrics"
individual_dir <- file.path(output_root, "individual_maps")
panel_dir <- file.path(output_root, "capacity_panels")
individual_svg_dir <- file.path(output_root, "individual_maps_svg")
panel_svg_dir <- file.path(output_root, "capacity_panels_svg")
genomics_panel_dir <- file.path(output_root, "genomics_panels")
genomics_panel_svg_dir <- file.path(output_root, "genomics_panels_svg")
dir.create(individual_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(panel_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(individual_svg_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(panel_svg_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(genomics_panel_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(genomics_panel_svg_dir, recursive = TRUE, showWarnings = FALSE)

for (r in all_regions) {
  
  # 1. First, subset the data as you currently do
  if (r == "Worldwide") {
    plot_data <- world_data
    plot_data_genomics <- world_data_genomics
  } else if (r == "Europe") {
    plot_data <- world_data_europe
    plot_data_genomics <- world_data_genomics_europe
  } else if (r == "Central & South America") {
    plot_data <- world_data %>% 
      filter(continent == "South America" | subregion == "Central America" | subregion == "Caribbean")
    plot_data_genomics <- world_data_genomics %>% 
      filter(continent == "South America" | subregion == "Central America" | subregion == "Caribbean")
  } else {
    plot_data <- world_data %>% filter(continent %in% regions[[r]])
    plot_data_genomics <- world_data_genomics %>% filter(continent %in% regions[[r]])
  }
  
  current_is_fleming <- (r == "Fleming")
  
  if (current_is_fleming) {
    # This turns all non-Fleming countries in Africa/Asia to "CONTEXT"
    plot_data <- plot_data %>% 
      mutate(across(ends_with("_metric"), ~if_else(iso3 %in% fleming_iso3_list, as.character(.x), "CONTEXT")))
    plot_data_genomics <- plot_data_genomics %>% 
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
    ggsave(
      file.path(individual_svg_dir, paste0(r, "_", m$col, ".svg")),
      p,
      width = 8,
      height = 5,
      device = svglite::svglite
    )
  }
  
  # Save Regional Panels
  panel_plot <- make_panel(plot_data, r, colours, metrics_capacity, ncol = 2, is_fleming = current_is_fleming)
  ggsave(file.path(panel_dir, paste0(r, "_capacity_panel.png")), panel_plot, width = 14, height = 10, bg = "white")
  ggsave(
    file.path(panel_svg_dir, paste0(r, "_capacity_panel.svg")),
    panel_plot,
    width = 14,
    height = 10,
    bg = "white",
    device = svglite::svglite
  )
  
  genomics_plot <- make_panel(
    plot_data_genomics,
    paste(r, "Genomics"),
    colours,
    metrics_genomics,
    ncol = 2,
    is_fleming = current_is_fleming
  )
  ggsave(file.path(genomics_panel_dir, paste0(r, "_genomics_panel.png")), genomics_plot, width = 14, height = 7, bg = "white")
  ggsave(
    file.path(genomics_panel_svg_dir, paste0(r, "_genomics_panel.svg")),
    genomics_plot,
    width = 14,
    height = 7,
    bg = "white",
    device = svglite::svglite
  )
}
