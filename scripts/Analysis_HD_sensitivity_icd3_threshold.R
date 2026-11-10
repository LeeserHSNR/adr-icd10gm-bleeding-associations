# ---
# Project: ADRED Paper 1
# Script:  Sensitivity analysis (ICD-3 threshold) – primary diagnoses (HD)
# Purpose: Evaluate how different relative frequency thresholds translate
#          into MIN_TOPLEVEL_COUNT and affect inclusion/exclusion of ICD-3 categories
# Input:   ../ADRED_Daten_R.xlsx  (sheet: T_ICD_semik_mod_para_21)
# Output:  output/
#          - Plots: Plot_HD_sensitivity_icd3_threshold_*.jpg
#          - Table: Table_HD_sensitivity_icd3_threshold.docx
# Notes:   This script uses HD only (Diagnoseart == "HD")
# ---

# ---- 00 References (methodology / threshold selection) ----

# 1. Piironen J, Vehtari A. Sparsity information and regularization in the horseshoe and other shrinkage priors.
#    Electronic Journal of Statistics, 2017; 11(2): 5018-5051.
#    DOI: 10.1214/17-EJS1337SI
#    Link: https://doi.org/10.1214/17-EJS1337SI
# 2. Schneeweiss S, Rassen JA, Glynn RJ, Avorn J, Mogun H, Brookhart MA. High-dimensional propensity score adjustment in studies of treatment effects using health care claims data.
#    Epidemiology, 2011; 22(3): 334-344.
#    DOI: 10.1097/EDE.0b013e3181f2e3e6
#    Link: https://doi.org/10.1097/EDE.0b013e3181f2e3e6
# 3. Gelman A, Carlin JB, Stern HS, Dunson DB, Vehtari A, Rubin DB. Bayesian Data Analysis. 3rd edition. CRC Press; 2014.
# 4. Harrell FE Jr. Regression Modeling Strategies. 2nd edition. Springer; 2015.
# 5. Govaert S, Lombaert N. Performance of disproportionality analysis methods for signal detection in spontaneous reporting systems with hierarchical structure.
#    Pharmacoepidemiology and Drug Safety, 2020; 29(1): 68-77.
#    DOI: 10.1002/pds.4956
#    Link: https://doi.org/10.1002/pds.4956


# ---- 01 Setup: packages, options ----
# Install packages
# install.packages("flextable")
# install.packages("officer")   # Word-Export

# Load required libraries
library(readxl)
library(tidyr)
library(dplyr)
library(stringi)
library(ggplot2)
options(dplyr.summarise.inform = FALSE)
library(flextable)
library(officer)

# ---- 02 Paths: input data (Excel) ----
DATA_FILE <- file.path("..", "ADRED_Daten_R.xlsx")
if (!file.exists(DATA_FILE)) stop("Data file not found: ", normalizePath(DATA_FILE, winslash = "/"))

# ---- 03 Data import: ICD table (T_ICD_semik_mod_para_21) ----
# The data are contained in a set of Excel tables, linked by the key case_id
srcdata <- read_excel(DATA_FILE, sheet = "T_ICD_semik_mod_para_21")

# Create variables pat_id and presentation from case_id
srcdata$pat_id <- sub("_\\d+", "", srcdata$MANU_case_id, perl = TRUE)
srcdata$presentation <- as.numeric(sub("\\d+_", "", srcdata$MANU_case_id, perl = TRUE))

# Keep only inpatient cases
srcdata <- srcdata %>% filter(Aufnahmetyp == "stationär")

# Filter only Hauptdiagnosen (HD)
srcdata_HD <- srcdata %>% filter(Diagnoseart == "HD")  

# ---- 04 Sensitivity analysis: thresholds -> MIN_TOPLEVEL_COUNT ----
# thresholds and case counts
thresholds <- c(0.01, 0.025, 0.05, 0.1, 0.25, 0.5)  # in percent
total_cases <- nrow(srcdata_HD)

sensitivity_results <- data.frame()

for (t in thresholds) {
  min_count <- ceiling((t / 100) * total_cases)
  
  ICDcount_test <- srcdata_HD %>%
    group_by(ICD_Dreisteller) %>%
    summarize(count = n(), .groups = "drop")
  
  badICD <- ICDcount_test %>% filter(count < min_count) %>% pull(ICD_Dreisteller)
  
  removed_groups <- length(badICD)
  kept_groups <- nrow(ICDcount_test) - removed_groups
  
  removed_rows <- sum(srcdata_HD$ICD_Dreisteller %in% badICD)
  kept_rows <- total_cases - removed_rows
  
  sensitivity_results <- rbind(sensitivity_results,
                               data.frame(
                                 Threshold = t,
                                 MIN_TOPLEVEL_COUNT = min_count,
                                 Kept_Groups = kept_groups,
                                 Removed_Groups = removed_groups,
                                 Kept_Cases = kept_rows,
                                 Removed_Cases = removed_rows
                               )
  )
}

# Show table
print(sensitivity_results)

# ---- 05 Visualization: category counts (kept vs removed) ----
sensitivity_long <- pivot_longer(sensitivity_results,
                                 cols = c(Kept_Groups, Removed_Groups),
                                 names_to = "Type", values_to = "Count")

p1 <- ggplot(sensitivity_long, aes(x = Threshold, y = Count, color = Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Sensitivity Analysis: Threshold vs. Category Selection",
    x = "Threshold (% of total cases)",
    y = "Number of three-character categories"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("darkgreen", "red3"),
                     labels = c("Kept", "Removed"),
                     name = NULL) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

print(p1)

# ---- 06 Visualization: case counts (kept vs removed) ----
sensitivity_cases <- sensitivity_results %>%
  select(Threshold, Kept_Cases, Removed_Cases) %>%
  tidyr::pivot_longer(-Threshold, names_to = "Type", values_to = "Count")

p2 <- ggplot(sensitivity_cases, aes(x = Threshold, y = Count, color = Type)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Sensitivity Analysis: Impact on Case Counts",
    x = "Threshold (% of total cases)",
    y = "Number of cases"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("steelblue", "orange"),
                     labels = c("Kept", "Removed"),
                     name = NULL) +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

print(p2)

# ---- 07 Output: file names and output directory ----
base_path  <- file.path("output")
base_name  <- "Plot_HD_sensitivity_icd3_threshold"
table_name <- "Table_HD_sensitivity_icd3_threshold"

if (!dir.exists(base_path)) {
  dir.create(base_path, recursive = TRUE)
}

# ---- 08 Output: save plots (JPG) ----
ggsave(
  filename = file.path(base_path, paste0(base_name, "_categories_counts.jpg")),
  plot = p1,
  device = "jpeg",
  width = 8, height = 6, units = "in",
  dpi = 300,
  bg = "white"
)

ggsave(
  filename = file.path(base_path, paste0(base_name, "_case_counts.jpg")),
  plot = p2,
  device = "jpeg",
  width = 8, height = 6, units = "in",
  dpi = 300,
  bg = "white"
)


# ---- 09 Output: format results table (for Word export) ----
out_word <- sensitivity_results %>%
  mutate(
    Threshold = sprintf("%.3f%%", Threshold),     # e.g., "0.010%"
    MIN_TOPLEVEL_COUNT = as.integer(MIN_TOPLEVEL_COUNT),
    Kept_Groups   = as.integer(Kept_Groups),
    Removed_Groups= as.integer(Removed_Groups),
    Kept_Cases    = as.integer(Kept_Cases),
    Removed_Cases = as.integer(Removed_Cases)
  ) %>%
  rename(
    `Threshold` = Threshold,
    `Min. count` = MIN_TOPLEVEL_COUNT,
    `Kept` = Kept_Groups,
    `Removed` = Removed_Groups,
    `Kept ` = Kept_Cases,        # intentionally different name for later header labels
    `Removed ` = Removed_Cases
  )

# ---- 10 Output: Word table (flextable formatting + export) ----
# Flextable
ft <- flextable(out_word)

# Create a two-level header: "Three-character categories" over Kept/Removed, and "Cases" over Kept/Removed
ft <- add_header_row(
  ft,
  values = c("", "", "Three-character categories", "Cases"),
  colwidths = c(1, 1, 2, 2)
)

# Make the lower header row descriptive
ft <- set_header_labels(
  ft,
  Threshold = "Threshold",
  `Min. count` = "MIN_TOPLEVEL_COUNT",
  Kept = "Kept",
  Removed = "Removed",
  `Kept ` = "Kept",
  `Removed ` = "Removed"
)

ft <- theme_vanilla(ft)

# Grey header + bold (applies to both header rows)
ft <- bg(ft, bg = "#D9D9D9", part = "header")
ft <- bold(ft, part = "header")

# Typography and alignment
ft <- fontsize(ft, size = 10, part = "all")
ft <- align(ft, align = "left", part = "all")
ft <- valign(ft, valign = "top", part = "all")
ft <- autofit(ft)

# Prevent table centering and use a fixed layout
ft <- set_table_properties(ft, layout = "fixed", width = 0.98)

# (Optional) Set column widths in inches — adjust if needed
ft <- width(ft, j = 1, width = 1.0)  # Threshold
ft <- width(ft, j = 2, width = 1.3)  # min count
ft <- width(ft, j = 3:6, width = 1.0)

# Borders (inner grid + outer frame)
inner <- fp_border(color = "grey", width = 0.5)
outer <- fp_border(color = "grey", width = 0.8)

ft <- border_inner_h(ft, border = inner, part = "all")
ft <- border_inner_v(ft, border = inner, part = "all")
ft <- border_outer(ft, border = outer, part = "all")

# Caption/legend + Word export
caption_text <- "Table X. Sensitivity analysis: impact of prevalence thresholds on the inclusion of three-character ICD-10-GM categories and cases (principal diagnoses)."
legend_text <- paste0(
  "The threshold (%) defines the minimum frequency required per three-character ICD-10-GM category. ",
  "MIN_TOPLEVEL_COUNT = ceiling((threshold/100) × N), where N is the total number of cases. ",
  "Kept/Removed report the numbers of three-character categories and cases retained versus excluded."
)

dir.create(base_path, recursive = TRUE, showWarnings = FALSE)

doc <- read_docx()

# Caption above the table (left-aligned)
doc <- body_add_fpar(
  doc,
  fpar(
    ftext(caption_text, fp_text(bold = FALSE, font.size = 11)),
    fp_p = fp_par(text.align = "left")
  )
)
doc <- body_add_par(doc, "", style = "Normal")

# Insert the table (force left alignment)
doc <- body_add_flextable(doc, ft, align = "left")
doc <- body_add_par(doc, "", style = "Normal")

# Legend below the table (left-aligned)
doc <- body_add_fpar(
  doc,
  fpar(
    ftext(legend_text, fp_text(font.size = 9, italic = TRUE)),
    fp_p = fp_par(text.align = "left")
  )
)

print(doc, target = file.path(base_path, paste0(table_name, ".docx")))