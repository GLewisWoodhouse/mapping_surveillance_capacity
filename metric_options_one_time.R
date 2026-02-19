# One-time metric options atlas for colleague review.
# Produces regional panels (one map per metric) and per-metric description PDFs.

library(tidyverse)
library(readxl)
library(countrycode)
library(sf)
library(rnaturalearth)
library(cowplot)

get_iso3_from_df <- function(df) {
  iso_cols <- c("iso3", "ISO3", "Iso3", "iso_a3", "iso_a3c", "ISO_A3", "ISO_A3C")
  for (cname in iso_cols) {
    if (cname %in% names(df)) {
      v <- toupper(as.character(df[[cname]]))
      v[nchar(v) != 3] <- NA
      return(v)
    }
  }
  name_cols <- c("country", "country_name", "Country", "admin", "COUNTRY")
  for (cname in name_cols) {
    if (cname %in% names(df)) {
      return(countrycode(df[[cname]], origin = "country.name", destination = "iso3c"))
    }
  }
  rep(NA_character_, nrow(df))
}

add_kosovo_iso3 <- function(df) {
  country_cols <- c("country", "country_name", "admin", "name", "Country")
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

glass_profile <- read_excel("NAP_Glass_data.xlsx") %>%
  mutate(iso3 = get_iso3_from_df(.)) %>%
  add_kosovo_iso3() %>%
  rename_with(tolower)

tracss <- read.csv("TrACSS-2025-Data-export-01102025.csv", stringsAsFactors = FALSE) %>%
  mutate(iso3 = get_iso3_from_df(.)) %>%
  add_kosovo_iso3() %>%
  select(c(1:3, 124, 130), iso3) %>%
  rename_with(~ c("surveillance_network", "national_reference_lab"), .cols = c(4, 5))

glass_2023_path <- "/Users/Guest_Aanensen/Downloads/GLASS data 2023.csv"
glass_2023 <- if (file.exists(glass_2023_path)) {
  g23 <- read.csv(glass_2023_path, check.names = FALSE, stringsAsFactors = FALSE)
  names(g23) <- make.unique(tolower(names(g23)))
  g23 <- g23 %>%
    mutate(iso3 = get_iso3_from_df(.)) %>%
    add_kosovo_iso3() %>%
    filter(year == 2023)
  g23
} else {
  data.frame(iso3 = character())
}

metric_data <- glass_profile %>%
  select(any_of(c("iso3", "glass_status", "data_sumbitted", "amr_enrolled"))) %>%
  full_join(glass_2023 %>% select(any_of(c("iso3", "amr_ncc", "amr_nrl", "eqa_to_nrl", "amr_ast_standards", "amr_eqa_glass_labs"))), by = "iso3") %>%
  full_join(tracss %>% select(iso3, surveillance_network, national_reference_lab), by = "iso3") %>%
  mutate(
    iso3 = toupper(trimws(as.character(iso3))),
    glass_enrollment_metric = case_when(
      str_detect(glass_status, "Not Enrolled") ~ "Not Enrolled",
      str_detect(glass_status, "Enrolled") ~ "Enrolled",
      str_detect(amr_enrolled, "^Y") ~ "Enrolled",
      TRUE ~ NA_character_
    ),
    glass_submission_metric = case_when(
      str_detect(glass_status, "Not Enrolled") ~ "Not Enrolled",
      str_detect(glass_status, "Data submitted") ~ "Submitted",
      str_detect(glass_status, "No data submitted") ~ "No Submission",
      data_sumbitted == "submitted_data" ~ "Submitted",
      TRUE ~ NA_character_
    ),
    ncc_glass_metric = case_when(
      amr_ncc == "Established" ~ "Yes",
      amr_ncc == "Establishment in progress" ~ "Partial",
      amr_ncc == "Not established" ~ "No",
      amr_ncc == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    ),
    nrl_glass_metric = case_when(
      amr_nrl == "Established" ~ "Yes",
      amr_nrl == "Not established" ~ "No",
      amr_nrl == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    ),
    eqa_to_nrl_glass_metric = case_when(
      eqa_to_nrl == "Provided" ~ "Yes",
      eqa_to_nrl == "Not provided" ~ "No",
      eqa_to_nrl == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    ),
    ast_standards_glass_metric = case_when(
      amr_ast_standards %in% c("CLSI", "EUCAST", "EUCAST|CLSI", "O") ~ "Yes",
      amr_ast_standards == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    ),
    eqa_glass_labs_metric = case_when(
      amr_eqa_glass_labs == "Provided to all laboratories" ~ "Yes",
      amr_eqa_glass_labs == "Not provided to all laboratories" ~ "Partial",
      amr_eqa_glass_labs == "Not_enrolled" ~ "Not Enrolled",
      TRUE ~ NA_character_
    ),
    nrl_tracss_metric = case_when(
      str_detect(national_reference_lab, "^Yes,.*all the 12") ~ "Yes",
      str_detect(national_reference_lab, "^Yes,.*some") ~ "Partial",
      str_detect(national_reference_lab, "^No") ~ "No",
      TRUE ~ NA_character_
    ),
    surveillance_network_metric = case_when(
      str_detect(surveillance_network, "^[AB]") ~ "No",
      str_detect(surveillance_network, "^C") ~ "Partial",
      str_detect(surveillance_network, "^[DE]") ~ "Yes",
      TRUE ~ NA_character_
    )
  )

metric_definitions <- list(
  list(col = "glass_enrollment_metric", title = "Enrollment in GLASS", source = "GLASS", levels = c("Enrolled", "Not Enrolled"), palette = c("Enrolled" = "#1B9E77", "Not Enrolled" = "#D95F02"), description = "Country enrollment status in GLASS AMR surveillance."),
  list(col = "glass_submission_metric", title = "Data Submission to GLASS", source = "GLASS", levels = c("Submitted", "No Submission", "Not Enrolled"), palette = c("Submitted" = "#1B9E77", "No Submission" = "#E6AB02", "Not Enrolled" = "#D95F02"), description = "Whether enrolled countries have submitted GLASS data."),
  list(col = "ncc_glass_metric", title = "National Coordinating Centre", source = "GLASS 2023", levels = c("Yes", "Partial", "No", "Not Enrolled"), palette = c("Yes" = "#1B9E77", "Partial" = "#E6AB02", "No" = "#D95F02", "Not Enrolled" = "#7570B3"), description = "Status of AMR national coordinating centre."),
  list(col = "nrl_glass_metric", title = "AMR National Reference Lab", source = "GLASS 2023", levels = c("Yes", "No", "Not Enrolled"), palette = c("Yes" = "#1B9E77", "No" = "#D95F02", "Not Enrolled" = "#7570B3"), description = "Status of AMR national reference laboratory."),
  list(col = "eqa_to_nrl_glass_metric", title = "EQA to NRL", source = "GLASS 2023", levels = c("Yes", "No", "Not Enrolled"), palette = c("Yes" = "#1B9E77", "No" = "#D95F02", "Not Enrolled" = "#7570B3"), description = "External quality assessment provision to the national reference laboratory."),
  list(col = "ast_standards_glass_metric", title = "AST Standards in Use", source = "GLASS 2023", levels = c("Yes", "Not Enrolled"), palette = c("Yes" = "#1B9E77", "Not Enrolled" = "#7570B3"), description = "Whether AST standards are defined/reported."),
  list(col = "eqa_glass_labs_metric", title = "EQA at GLASS Labs", source = "GLASS 2023", levels = c("Yes", "Partial", "Not Enrolled"), palette = c("Yes" = "#1B9E77", "Partial" = "#E6AB02", "Not Enrolled" = "#7570B3"), description = "Extent of EQA coverage across GLASS laboratories."),
  list(col = "nrl_tracss_metric", title = "AMR National Reference Lab", source = "TrACSS", levels = c("Yes", "Partial", "No"), palette = c("Yes" = "#1B9E77", "Partial" = "#E6AB02", "No" = "#D95F02"), description = "TrACSS-reported NRL AST capacity (all organisms, some organisms, or none)."),
  list(col = "surveillance_network_metric", title = "Surveillance Network", source = "TrACSS", levels = c("Yes", "Partial", "No"), palette = c("Yes" = "#1B9E77", "Partial" = "#E6AB02", "No" = "#D95F02"), description = "Maturity of national AMR surveillance network in TrACSS.")
)

world_sf <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
  mutate(
    iso3 = case_when(
      adm0_a3 == "SDS" ~ "SSD",
      adm0_a3 == "PSX" ~ "PSE",
      TRUE ~ adm0_a3
    )
  )

world_data <- world_sf %>%
  left_join(metric_data %>% mutate(iso3 = toupper(trimws(iso3))), by = "iso3")

fleming_iso3_list <- c(
  "SEN", "SLE", "GHA", "NGA", "SWZ", "MWI", "KEN", "RWA",
  "TZA", "UGA", "ZMB", "ZWE", "BGD", "BTN", "IND", "NPL",
  "PAK", "LKA", "TLS", "IDN", "LAO", "PNG", "VNM"
)

theme_map <- function() {
  theme_void() +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      legend.position = "right",
      plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
      plot.margin = margin(4, 4, 4, 4, "mm")
    )
}

make_map <- function(data, fill_col, title, palette, levels_vec, is_fleming = FALSE) {
  df <- data %>% mutate(.temp_fill = as.character(.data[[fill_col]]))
  if (is_fleming) {
    df <- df %>%
      mutate(.temp_fill = if_else(iso3 %in% fleming_iso3_list, .temp_fill, "CONTEXT"))
    levels_vec <- c(levels_vec, "CONTEXT", "NA")
    palette <- c(palette, "CONTEXT" = "#F2F2F2")
  } else {
    levels_vec <- c(levels_vec, "NA")
  }
  df <- df %>%
    mutate(.temp_fill = case_when(
      is.na(.temp_fill) | .temp_fill == "" ~ "NA",
      TRUE ~ .temp_fill
    ))
  palette <- c(palette, "NA" = "#D9D9D9")
  df$.temp_fill <- factor(df$.temp_fill, levels = levels_vec)
  
  ggplot(df) +
    geom_sf(aes(fill = .temp_fill), colour = "grey25", linewidth = 0.1) +
    scale_fill_manual(
      values = palette,
      breaks = levels_vec,
      drop = FALSE,
      guide = guide_legend(override.aes = list(colour = "grey25", linewidth = 0.2))
    ) +
    labs(title = title, fill = NULL) +
    theme_map()
}

make_panel <- function(data, region_name, metric_defs, ncol = 3, is_fleming = FALSE) {
  plots <- lapply(metric_defs, function(m) {
    p_full <- make_map(data, m$col, paste0(m$title, " (", m$source, ")"), m$palette, m$levels, is_fleming = is_fleming)
    p_map <- make_map(data, m$col, paste0(m$title, " (", m$source, ")"), m$palette, m$levels, is_fleming = is_fleming) +
      theme(legend.position = "none")
    p_leg <- cowplot::get_legend(p_full)
    plot_grid(p_map, ggdraw(p_leg), ncol = 2, rel_widths = c(1, 0.4))
  })
  plot_grid(
    ggdraw() + draw_label(region_name, fontface = "bold", size = 18),
    plot_grid(plotlist = plots, ncol = ncol),
    ncol = 1,
    rel_heights = c(0.06, 1)
  )
}

write_description_rmd <- function(rmd_path, metric_defs) {
  lines <- c(
    "---",
    "title: \"Metric Options and Descriptions\"",
    "output:",
    "  pdf_document:",
    "    toc: true",
    "    number_sections: false",
    "geometry: margin=1in",
    "---",
    "",
    "This booklet lists each available metric, its data source, description, and simplified labels.",
    ""
  )
  for (m in metric_defs) {
    lines <- c(
      lines,
      paste0("## ", m$title, " (", m$source, ")"),
      "",
      paste0("**Description:** ", m$description),
      "",
      paste0("**Simplified labels:** ", paste(m$levels, collapse = ", ")),
      ""
    )
  }
  writeLines(lines, con = rmd_path)
}

regions <- list(
  "Worldwide" = NULL,
  "Europe" = "Europe",
  "Asia" = "Asia",
  "Africa" = "Africa",
  "Fleming" = c("Africa", "Asia"),
  "Central & South America" = c("South America", "Central America", "Caribbean")
)

out_root <- "metric_options_one_time"
panel_dir <- file.path(out_root, "regional_panels_png")
desc_dir <- file.path(out_root, "metric_descriptions_pdf")
dir.create(panel_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(desc_dir, recursive = TRUE, showWarnings = FALSE)

for (nm in names(regions)) {
  if (nm == "Worldwide") {
    plot_data <- world_data
  } else if (nm == "Central & South America") {
    plot_data <- world_data %>% filter(continent == "South America" | subregion == "Central America" | subregion == "Caribbean")
  } else if (nm == "Fleming") {
    plot_data <- world_data %>% filter(continent %in% regions[[nm]])
  } else {
    plot_data <- world_data %>% filter(continent == regions[[nm]])
  }
  panel <- make_panel(plot_data, nm, metric_definitions, ncol = 3, is_fleming = (nm == "Fleming"))
  ggsave(file.path(panel_dir, paste0(nm, "_metric_options_panel.png")), panel, width = 22, height = 16, bg = "white")
}

unlink(file.path(desc_dir, "*.pdf"))
rmd_path <- file.path(desc_dir, "all_metric_descriptions.Rmd")
write_description_rmd(rmd_path, metric_definitions)

if (!requireNamespace("rmarkdown", quietly = TRUE)) {
  stop("Package 'rmarkdown' is required. Install with install.packages('rmarkdown').")
}

if (!rmarkdown::pandoc_available()) {
  rstudio_pandoc_dir <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
  if (file.exists(file.path(rstudio_pandoc_dir, "pandoc"))) {
    Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc_dir)
  }
}

if (!rmarkdown::pandoc_available()) {
  stop("Pandoc not found. Install Pandoc or run from RStudio where Pandoc is bundled.")
}

rmarkdown::render(
  input = rmd_path,
  output_format = "pdf_document",
  output_file = "all_metric_descriptions.pdf",
  output_dir = desc_dir,
  quiet = TRUE
)

message("Done. Outputs written to: ", out_root)
