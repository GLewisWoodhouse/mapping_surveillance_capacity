# Prototype AMR Surveillance & Genomics Maps – README

## Overview

This folder contains data inputs, processed datasets, R scripts, and output maps used to generate prototype country‑level and regional visualisations of antimicrobial resistance (AMR) surveillance capacity and pathogen genomics coverage.

The work brings together **surveillance system metrics** (WHO GLASS, TrACSS) and **genomic sequencing data** (AMR.Watch), harmonised at country level (ISO3) and visualised as static maps for internal analysis and stakeholder communication.

The primary purpose of this handover is to:

* Explain what each dataset and map represents
* Document how key metrics were derived
* Enable colleagues to reproduce, interpret, or extend the outputs

---

## Folder structure (high level)

* **Raw / intermediate data**

  * `NAP_Glass_data.xlsx` – WHO GLASS enrolment and reporting status
  * `TrACSS-2025-Data-export-*.csv` – TrACSS surveillance capacity indicators
  * `National Genomic Landscape.xlsx` – contextual national genomics information
  * `all_priority_genomes.csv`, `critical_priority_genomes.csv`, `klebsiella_genomes.csv` – processed genomic counts from AMR.Watch
  * `aggregated_genomes.csv`, `combined_data.csv`, `combined_global_data.csv` – harmonised country‑level datasets used for mapping

* **Scripts**

  * `testing_script.R` / `map_prototype_*.R` – data cleaning, harmonisation, and mapping scripts (R)
  * `glass_data_pull.R` – GLASS data extraction and preparation

* **Outputs**

  * `individual_maps/` – single‑metric maps by region
  * `region_panels/` – multi‑panel regional figures (surveillance capacity and genomics)

---

## Data sources

### Genomic data

* **AMR.Watch**

  * Source of raw genome counts by organism and country
  * Counts reflect publicly available sequencing data indexed in AMR.Watch at time of extraction

* **Data processing pipeline**

  * Genomic data were processed using a reproducible **data.flo** pipeline:

    * [https://www.data-flo.io/run/7nm2xeWxGWCbs1MfPcM75r](https://www.data-flo.io/run/7nm2xeWxGWCbs1MfPcM75r)
  * The pipeline:

    * Filters records to priority pathogens
    * Aggregates genome counts at country level
    * Outputs per‑organism and combined genome count tables

### Surveillance and system metrics

* **WHO GLASS** (Global Antimicrobial Resistance and Use Surveillance System)

  * Country enrolment status
  * Data submission status

* **TrACSS** (Tracking AMR Country Self‑Assessments)

  * National AMR surveillance capacity indicators

---

## Metrics and how they were created

All metrics are constructed at **country level** and joined using ISO3 country codes before mapping.

### 1. Enrolled in GLASS

**Source:** WHO GLASS

* Binary indicator of whether a country is formally enrolled in GLASS
* Categories:

  * `Yes` – country enrolled in GLASS
  * `No` – country not enrolled
  * `NA` – status not available

Used directly as provided by WHO GLASS metadata.

---

### 2. GLASS data submitted

**Source:** WHO GLASS

* Applied **only to countries enrolled in GLASS**
* Indicates whether AMR data have been submitted
* Categories:

  * `Yes` – enrolled and has submitted data
  * `No` – enrolled but no data submission recorded
  * `Not enrolled` – not enrolled in GLASS
  * `NA` – missing or unclear status

This metric is derived by combining enrolment status with submission flags from GLASS.

---

### 3. National Reference Laboratory

**Source:** TrACSS

* Indicates the presence of a national reference laboratory for AMR
* Categories:

  * `Yes` – national reference laboratory reported
  * `No` – no national reference laboratory reported
  * `NA` – information not available

Values are taken directly from TrACSS country self‑assessment responses.

---

### 4. National Sentinel Surveillance Network

**Source:** TrACSS

* Describes the extent of a national AMR sentinel surveillance network
* Categories:

  * `Yes` – established national sentinel network
  * `Partial` – limited or sub‑national coverage
  * `No` – no sentinel surveillance network
  * `NA` – information not available

This metric reflects TrACSS categorical responses and is mapped as an ordered factor.

---

### 5. Priority pathogen genome counts

**Source:** AMR.Watch (processed via data.flo)

* Genome counts aggregated by country across **WHO priority pathogens**
* Counts represent the number of publicly available genomes associated with each country

For visualisation, counts are binned into the following categories:

* 1–50
* 50–100
* 100–500
* 500–1,000
* 1,000–5,000
* 5,000–10,000
* 10,000–50,000
* 50,000+

Separate maps are produced for:

* **All priority pathogens**
* **Critical priority pathogens only**

---

## Definition of critical and priority pathogens

Pathogen priority status follows WHO classifications. The table below lists organisms included in the analysis and their WHO priority status.

| organismId | Organism name                  | WHO priority status |
| ---------: | ------------------------------ | ------------------- |
|        470 | *Acinetobacter baumannii*      | Critical            |
|        562 | *Escherichia coli*             | Critical            |
|        573 | *Klebsiella pneumoniae*        | Critical            |
|        287 | *Pseudomonas aeruginosa*       | Critical            |
|     354276 | *Enterobacter cloacae* complex | Critical            |
|        197 | *Campylobacter jejuni*         | High                |
|        195 | *Campylobacter coli*           | High                |
|       1352 | *Enterococcus faecium*         | High                |
|        485 | *Neisseria gonorrhoeae*        | High                |
|       1280 | *Staphylococcus aureus*        | High                |
|     149539 | *Salmonella Enteritidis*       | High                |
|      90370 | *Salmonella Typhi*             | High                |
|      90371 | *Salmonella Typhimurium*       | High                |
|      28901 | *Salmonella enterica*          | High                |
|       1639 | *Listeria monocytogenes*       | High                |
|        727 | *Haemophilus influenzae*       | Medium              |
|       1313 | *Streptococcus pneumoniae*     | Medium              |
|        621 | *Shigella boydii*              | Medium              |
|        622 | *Shigella dysenteriae*         | Medium              |
|        623 | *Shigella flexneri*            | Medium              |
|        624 | *Shigella sonnei*              | Medium              |

Only organisms classified as **Critical** are included in the “critical priority pathogen” genomic maps.

---

## Map interpretation notes

* Grey (`NA`) indicates missing, unavailable, or non‑applicable data
* Genomic counts reflect **data availability**, not true burden or incidence
* Surveillance capacity indicators are self‑reported and may vary in interpretation between countries

---

## Reproducibility

All maps can be regenerated using the R scripts in this folder, assuming the same input datasets. The mapping scripts:

* Harmonise country identifiers (ISO3)
* Join surveillance and genomics datasets
* Apply consistent colour palettes and binning
* Output regional and global figures as static images

---

## Contact / ownership

For questions about data sources, methods, or interpretation, please contact the project author or CGPS AMR surveillance team.
