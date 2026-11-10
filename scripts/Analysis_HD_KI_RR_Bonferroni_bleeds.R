# ---
# Project: ADRED Paper 1
# Script:  Relative risks (RR) for bleeding by primary diagnosis (HD)
# Purpose: Compute RR + 95% CI for IN_Blutung by HD (ICD-3), excluding defining ICD-5 codes.
#          Publication focus: Bonferroni-adjusted confidence intervals.
# Input:   ../ADRED_Daten_R.xlsx
#          - T_ICD_semik_mod_para_21
#          - T_fall_z01_03_05_06
# Output:  output/
#          - Table: Table_HD_KI_RR_CIlower_gt1.docx
#          - Plots: Plot_HD_KI_RR_BF_bleeds.jpg
#                   Plot_Bleeding_Age_Sex_Boxplot.jpg
# ---


# ---- 00 Overview / notes ----
# Relative risk and confidence intervals for primary diagnoses (HD) in IN_Blutung (internal bleeding)
# Important: Results shown/used for publication are based on Bonferroni-adjusted 95% confidence intervals.
# (Non-adjusted estimates are not reported.)


# ---- 01 Setup: packages, options ----
# Install packages
install.packages("openxlsx")
library(openxlsx)

# Load required libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(patchwork)
library(flextable)
library(officer)
library(tidyr)
library(readxl)


# ---- 02 Paths: input data + output directory ----
# Paths (project root = R_Promotion_Paper1) 
DATA_FILE <- file.path("..", "ADRED_Daten_R.xlsx")
if (!file.exists(DATA_FILE)) stop("Data file not found: ", normalizePath(DATA_FILE, winslash = "/"))

EXPORT_DIR <- file.path("output")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 03 Data import: ICD table (T_ICD_semik_mod_para_21) ----
# Read the diagnosis table containing primary (HD) and secondary (ND) diagnoses
Excel_HD_ND <- readxl::read_excel(DATA_FILE, sheet = "T_ICD_semik_mod_para_21")


# ---- 04 Preprocessing: select variables (df_ICD) ----
# Create the data frame df_ICD for diagnoses and convert variables to factors
df_ICD <- Excel_HD_ND %>%
  mutate_if(is.character, as_factor)

# Rename variables and drop columns that are not needed
df_ICD <- Excel_HD_ND %>%
  select(ID=MANU_case_id,
         Diagnoseart,
         ICD=ICD_Dreisteller,
         ICD_5=ICD_Fünfsteller,
         Aufnahmetyp)

## View
df_ICD
print(df_ICD, n=50)


# ---- 05 Outcome definition: dummy IN_Blutung ----
# Create the dummy variable "IN_Blutung" in df_ICD
# IN_Blutung = I85.0, I98.3, K22.81, K25.0, K25.2, K26.0, K26.2, K26.4, K27.2, K28.0, K31.82, K55.22, K55.32, K57.11,
#              K57.31, K57.33, K92.2, N42.1, N93.8, N93.9, R04.1, R04.8, R04.9, R58

df_ICD_Dummy <- df_ICD %>%
  mutate(IN_Blutung = case_when(ICD_5 == "I85.0"|ICD_5 =="I98.3"|ICD_5 =="K22.81"|ICD_5 =="K25.0"|ICD_5 =="K25.2"|ICD_5 =="K26.0"|ICD_5 =="K26.2"
                                |ICD_5 =="K26.4"|ICD_5 =="K27.2"|ICD_5 =="K28.0"|ICD_5 =="K31.82"|ICD_5 =="K55.22"|ICD_5 =="K55.32"|ICD_5 =="K57.11"
                                |ICD_5 =="K57.31"|ICD_5 =="K57.33"|ICD_5 =="K92.2"|ICD_5 =="N42.1"|ICD_5 =="N93.8"|ICD_5 =="N93.9"|ICD_5 =="R04.1"
                                |ICD_5 =="R04.8"|ICD_5 =="R04.9"|ICD_5 =="R58"~ 1, TRUE ~ 0))

# Display patients with IN_Blutung
# Selected by ID: if a patient has IN_Blutung = 1, then all other diagnoses for that patient are also set to 1
df_ICD_Dummy_Ja <- df_ICD_Dummy %>%
  filter(IN_Blutung == 1) %>%
  select(ID,
         Diagnoseart,
         ICD,
         Aufnahmetyp) %>%
  group_by(ID, ICD)

df_ICD_Dummy_Nein <- df_ICD_Dummy %>%
  filter(IN_Blutung == 0) %>%
  select(ID,
         Diagnoseart,
         ICD,
         Aufnahmetyp) %>%
  group_by(ID, ICD)


## Print the counts of IN_Blutung by ICD code
table(df_ICD_Dummy_Ja$ICD)


# Add the dummy variable "IN_Blutung" to the data frame (case-level)
df_ICD_Dummy <- df_ICD_Dummy %>%
  mutate(IN_Blutung = ifelse(df_ICD_Dummy$ID %in% df_ICD_Dummy_Ja$ID,1,0))


# ---- 06 Build wide tables: HD/ND selection (exclude defining ICD-5 codes) ----
# Separate primary and secondary diagnoses into separate objects and filter by admission type (inpatient)
# Keep patient ID to link records later; exclude defining group ICD-5 codes

Hauptdiagnosen_ICD_5 <- df_ICD %>%
  filter(Diagnoseart == "HD", Aufnahmetyp == "stationär") %>%
  filter(!ICD_5 %in% c("I85.0", "I98.3", "K22.81", "K25.0", "K25.2", "K26.0", "K26.2", "K26.4",
                     "K27.2", "K28.0", "K31.82", "K55.22", "K55.32", "K57.11", "K57.31", "K57.33",
                     "K92.2", "N42.1", "N93.8", "N93.9", "R04.1", "R04.8", "R04.9", "R58")) %>%
  select(ID, ICD_5, ICD, Diagnoseart, Aufnahmetyp)



Nebendiagnosen_ICD_5 <- df_ICD %>%
  filter(Diagnoseart == "ND", Aufnahmetyp == "stationär") %>%
  filter(!ICD_5 %in% c("I85.0", "I98.3", "K22.81", "K25.0", "K25.2", "K26.0", "K26.2", "K26.4",
                     "K27.2", "K28.0", "K31.82", "K55.22", "K55.32", "K57.11", "K57.31", "K57.33",
                     "K92.2", "N42.1", "N93.8", "N93.9", "R04.1", "R04.8", "R04.9", "R58")) %>%
  select(ID, ICD_5, ICD,Diagnoseart, Aufnahmetyp)


## View
Hauptdiagnosen_ICD_5
Nebendiagnosen_ICD_5

# Separate primary and secondary diagnoses into separate objects (ICD-3-digit codes
# Exclude ICD-3 predictors used to define the bleeding outcome (avoid outcome leakage))
bleeding_predictors <- c(
  "I85", "I98", "K22", "K25", "K26", "K27", "K28",
  "K31", "K55", "K57", "K92", "N42", "N93", "R04", "R58"
)

Hauptdiagnosen <- Hauptdiagnosen_ICD_5 %>%
  filter(Diagnoseart == "HD", Aufnahmetyp == "stationär") %>%
  select(ID, HD = ICD) %>%
  filter(!HD %in% bleeding_predictors)

Nebendiagnosen <- Nebendiagnosen_ICD_5 %>%
  filter(Diagnoseart == "ND", Aufnahmetyp == "stationär") %>%
  select(ID, ND = ICD)%>%
  filter(!ND %in% bleeding_predictors)

## View
Hauptdiagnosen
Nebendiagnosen

# ---- 07 Construct HD table with bleeding status (one row per ID) ----
# Attach the primary diagnosis to each IN_Blutung dummy (ID occurs only once)
# Note: df_Dummy1 still contains pre-inpatient cases.
df_Dummy1 <- df_ICD_Dummy %>%
  select (ID, IN_Blutung) %>%
  group_by(ID) %>%
  slice(1)

HDNew <- Hauptdiagnosen %>%
  inner_join(df_Dummy1, by = "ID") 

HDNew %>%
  count(HD, IN_Blutung)



# ---- 08 Wide summary table: counts per HD by bleeding (Nein/Ja) ----
breiteTabelle <- HDNew %>%
  count(HD, IN_Blutung) %>%
  pivot_wider(names_from = IN_Blutung,
              values_from = n,
              values_fill = 0) %>%
  rename_with(~c("Nein(0)", "Ja(1)"), c("0", "1"))

## View
breiteTabelle

# ---- 09 Frequencies + filtering threshold (Summe > 7) ----
# Add frequencies of primary diagnoses
table(Hauptdiagnosen$HD)
as.data.frame(table(Hauptdiagnosen$HD))

HauptHaeufig <- as.data.frame(table(Hauptdiagnosen$HD))

left_join(breiteTabelle, HauptHaeufig,
          join_by(HD == Var1)) %>%
  select(HD, Freq, everything()) %>%
  arrange(desc(Freq))


ergebnis <- left_join(breiteTabelle, HauptHaeufig,
                      join_by(HD == Var1)) %>%
  select(HD, Summe=Freq, everything()) %>%
  arrange(desc(Summe))%>%
  filter(Summe >= 7)


# BADHD: excluded primary diagnoses (Summe < 7)
BADHD <- left_join(breiteTabelle, HauptHaeufig, join_by(HD == Var1)) %>%
  select(HD, Summe = Freq, everything()) %>%
  filter(Summe < 7)

anzahl_betroffene <- sum(BADHD$Summe)


# ---- 10 Analysis: Bonferroni-corrected RR + CI (publication results) ----
# Definition of variables

# a <- tab[1]  # Exposed, event --> HD and IN_bleeding
a <- ergebnis$`Ja(1)`

# b <- tab[2]  # Exposed, no event --> HD and no IN_bleeding

b <- ergebnis$`Nein(0)`

# c <- tab[3]  # Not exposed, event --> Not the HD and IN_bleeding

c <- sum(ergebnis$`Ja(1)`) - ergebnis$`Ja(1)`

# d <- tab[4]  # Not exposed, no event --> Not the HD and no IN_bleeding

d <- sum(ergebnis$`Nein(0)`)- ergebnis$`Nein(0)`

# RR
# RR <- (a / (a + b)) / (c / (c + d)) --> (75/75+225)/(1252/(1252+3887)

RR <- (a / (a + b)) / (c / (c + d))

# Standard error of log(RR)
#SE_logRR <- sqrt((1/a) - (1/(a+b)) + (1/c) - (1/(c+d)))

SE_logRR <- sqrt((1/a) - (1/(a+b)) + (1/c) - (1/(c+d)))

# CI (95%)
# z <- 1.96  # z-Wert für 95% CI
# logRR <- log(RR)
# CI_lower <- exp(logRR - z * SE_logRR)
# CI_upper <- exp(logRR + z * SE_logRR)

z <- 1.96  # z-Wert for 95% CI
logRR <- log(RR)
CI_lower <- exp(logRR - z * SE_logRR)
CI_upper <- exp(logRR + z * SE_logRR)


# Bonferroni parameters
m <- nrow(ergebnis)  # number of tests
print(sprintf("-- Bonferroni: m = %d tests", m))
alpha <- 0.05  # original significance level
alphaM <- alpha / m
zM <- qnorm(1 - alphaM / 2)

# Bonferroni-adjusted CI
CI_lowerM <- exp(logRR - zM * SE_logRR)
CI_upperM <- exp(logRR + zM * SE_logRR)

## Output
list(
  RR = RR,
  CI_adjusted = c(CI_lowerM, CI_upperM),
  z_adjusted = zM
)

# Create results table with Bonferroni-adjusted CI
ergebnisNewM <- ergebnis %>%
  mutate(
    RR = RR,
    CI_lowerM = CI_lowerM,
    CI_upperM = CI_upperM
  ) %>%
  filter(RR > 0)

# ---- 11 Output: Word table (Bonferroni-adjusted; Lower > 1)- without ICD-10 Description ----
# Output directory (relative to project root = R_Promotion_Paper1)
EXPORT_DIR <- file.path("output")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

# File name for the Word table (independent from plot names)
table_name <- "Table_HD_KI_RR_CIlower_gt1"
word_file  <- file.path(EXPORT_DIR, paste0(table_name, ".docx"))


# Check required columns
need <- c("HD", "Summe", "Ja(1)", "RR", "CI_lowerM", "CI_upperM")
stopifnot(all(need %in% names(ergebnisNewM)))

# Prepare table (filter: Lower > 1)
out_rr <- ergebnisNewM %>%
  transmute(
    HD = HD,
    `Total (n)`    = suppressWarnings(as.integer(as.character(Summe))),
    `Bleeding (n)` = suppressWarnings(as.integer(as.character(`Ja(1)`))),
    RR_num    = suppressWarnings(as.numeric(as.character(RR))),
    Lower_num = suppressWarnings(as.numeric(as.character(CI_lowerM))),
    Upper_num = suppressWarnings(as.numeric(as.character(CI_upperM)))
  ) %>%
  filter(is.finite(Lower_num), Lower_num > 1) %>%        # ONLY Lower > 1
  filter(is.finite(RR_num), is.finite(Upper_num)) %>%
  arrange(desc(RR_num)) %>%
  mutate(
    # Force EXACTLY two decimals (robust)
    RR    = sprintf("%.2f", RR_num),
    Lower = sprintf("%.2f", Lower_num),
    Upper = sprintf("%.2f", Upper_num)
  ) %>%
  select(
    `Primary diagnosis (ICD-10-GM, 3-digit)` = HD,
    `Total (n)`,
    `Bleeding (n)`,
    RR, Lower, Upper
  )

# Flextable
ft <- flextable(out_rr)

# shorten the first column header
ft <- set_header_labels(ft,
                        `Primary diagnosis (ICD-10-GM, 3-digit)` = "Primary diagnosis"
)

# Two-level header
ft <- add_header_row(
  ft,
  values = c("", "Counts", "RR", "Bonferroni-adjusted 95% CI"),
  colwidths = c(1, 2, 1, 2)
)

ft <- theme_vanilla(ft)
ft <- bg(ft, bg = "#D9D9D9", part = "header")
ft <- bold(ft, part = "header")

# Alignment / typography
ft <- fontsize(ft, size = 10, part = "all")
ft <- align(ft, align = "left", part = "all")
ft <- align(ft, j = c("Total (n)", "Bleeding (n)", "RR", "Lower", "Upper"),
            align = "right", part = "all")
ft <- valign(ft, valign = "top", part = "all")

# Fit on page (PORTRAIT): fixed layout + controlled column widths
ft <- set_table_properties(ft, layout = "fixed", width = 1)

# Column widths (inches) tuned for portrait
ft <- width(ft, j = 1, width = 1.8)          # diagnosis
ft <- width(ft, j = 2:3, width = 1.0)        # counts
ft <- width(ft, j = 4, width = 0.65)         # RR
ft <- width(ft, j = 5:6, width = 0.75)       # CI

# Borders
inner <- fp_border(color = "grey", width = 0.5)
outer <- fp_border(color = "grey", width = 0.8)
ft <- border_inner_h(ft, border = inner, part = "all")
ft <- border_inner_v(ft, border = inner, part = "all")
ft <- border_outer(ft, border = outer, part = "all")

# --- Caption/legend + Word export (PORTRAIT) 
caption_text <- "Table X. Relative risks (RRs) with Bonferroni-adjusted 95% confidence intervals for bleeding by principal diagnosis (ICD-10-GM, three-character level)."
legend_text  <- "Legend: Total (n) is the number of cases per diagnosis; Bleeding (n) is the number of bleeding cases. Lower/Upper denote the bounds of the Bonferroni-adjusted 95% confidence interval. Only diagnoses whose lower bound exceeds 1 are shown."

doc <- read_docx()

doc <- body_add_fpar(
  doc,
  fpar(
    ftext(caption_text, fp_text(bold = FALSE, font.size = 11)),
    fp_p = fp_par(text.align = "left")
  )
)
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_flextable(doc, ft, align = "left")
doc <- body_add_par(doc, "", style = "Normal")
doc <- body_add_fpar(
  doc,
  fpar(
    ftext(legend_text, fp_text(font.size = 9, italic = TRUE)),
    fp_p = fp_par(text.align = "left")
  )
)

print(doc, target = word_file)

# ---- 11 Output: Word table (Bonferroni-adjusted; Lower > 1) ----

if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("stringr", quietly = TRUE)) install.packages("stringr")

library(xml2)
library(dplyr)
library(stringr)
library(flextable)
library(officer)

# Output directory (relative to project root = R_Promotion_Paper1)
EXPORT_DIR <- file.path("output")
dir.create(EXPORT_DIR, recursive = TRUE, showWarnings = FALSE)

# File name for the Word table (independent from plot names)
table_name <- "Table_HD_KI_RR_CIlower_gt1"
word_file  <- file.path(EXPORT_DIR, paste0(table_name, ".docx"))


# ---- 11a ICD-10 3-digit label lookup (written-out diagnoses) ----
# Note: for readability, English 3-digit category descriptions from the
# WHO ICD-10 (2019 edition, ClaML format) are used as labels for the German
# ICD-10-GM 3-digit codes analysed here (categories are equivalent at the
# 3-digit level between ICD-10-GM and WHO ICD-10).

zip_url  <- "https://icdcdn.who.int/icd10/claml/icd102019en.xml.zip"
zip_file <- "icd102019en.xml.zip"

if (!file.exists(zip_file)) {
  download.file(zip_url, destfile = zip_file, mode = "wb")
}

xml_file <- unzip(zip_file, list = TRUE)$Name[1]
if (!file.exists(xml_file)) {
  unzip(zip_file, exdir = ".")
}

xml_doc <- read_xml(xml_file)

# Only actual diagnosis categories (kind="category"), excluding chapters/blocks
class_nodes <- xml_find_all(xml_doc, ".//*[local-name()='Class'][@kind='category']")

icd10_who <- tibble(
  code = xml_attr(class_nodes, "code"),
  description = map_chr(class_nodes, function(cl) {
    lab <- xml_find_first(cl, ".//*[local-name()='Rubric'][@kind='preferred']/*[local-name()='Label']")
    txt <- tryCatch(xml_text(lab), error = function(e) NA_character_)
    if (length(txt) == 0) NA_character_ else txt
  })
) %>%
  mutate(code = str_trim(code)) %>%
  filter(!is.na(code), !is.na(description))

message(sprintf("Parsed %d ICD-10-WHO entries.", nrow(icd10_who)))
stopifnot(nrow(icd10_who) > 0)

# Keep ONLY 3-character ICD-10 categories
icd10_labels <- icd10_who %>%
  mutate(
    code = str_extract(code, "^[A-Z][0-9]{2}")
  ) %>%
  filter(!is.na(code)) %>%
  distinct(code, .keep_all = TRUE)

# Add ICD-10-GM-specific categories not present in the WHO base classification
missing_labels <- tibble::tribble(
  ~code, ~description,
  "D90", "Immunodeficiency following radiation, chemotherapy or other immunosuppressive procedures",
  "U50", "Motor impairment",
  "U99", "Special procedures for testing for SARS-CoV-2"
)

icd10_labels <- bind_rows(
  icd10_labels,
  missing_labels
) %>%
  distinct(code, .keep_all = TRUE)


# ---- 11b Check required columns ----
# (unverändert)


# ---- 11b Check required columns ----

need <- c("HD", "Summe", "Ja(1)", "RR", "CI_lowerM", "CI_upperM")
stopifnot(all(need %in% names(ergebnisNewM)))


# ---- 11c Prepare table (filter: Lower > 1) + join written-out diagnoses ----

out_rr <- ergebnisNewM %>%
  
  transmute(
    HD = HD,
    `Total (n)`    = suppressWarnings(as.integer(as.character(Summe))),
    `Bleeding (n)` = suppressWarnings(as.integer(as.character(`Ja(1)`))),
    RR_num    = suppressWarnings(as.numeric(as.character(RR))),
    Lower_num = suppressWarnings(as.numeric(as.character(CI_lowerM))),
    Upper_num = suppressWarnings(as.numeric(as.character(CI_upperM)))
  ) %>%
  
  filter(is.finite(Lower_num), Lower_num > 1) %>%        # ONLY Lower > 1
  filter(is.finite(RR_num), is.finite(Upper_num)) %>%
  arrange(desc(RR_num)) %>%
  
  mutate(
    # Force EXACTLY two decimals (robust)
    RR    = sprintf("%.2f", RR_num),
    Lower = sprintf("%.2f", Lower_num),
    Upper = sprintf("%.2f", Upper_num)
  ) %>%
  
  # join written-out diagnosis descriptions by 3-digit ICD code
  left_join(
    icd10_labels,
    by = c("HD" = "code")
  ) %>%
  
  mutate(
    description = ifelse(is.na(description), "", description)
  ) %>%
  
  select(
    `Primary diagnosis (ICD-10-GM, 3-digit)` = HD,
    `Diagnosis description` = description,
    `Total (n)`,
    `Bleeding (n)`,
    RR, Lower, Upper
  )


# ---- 11d Flextable ----

ft <- flextable(out_rr)

# shorten the first column header
ft <- set_header_labels(
  ft,
  `Primary diagnosis (ICD-10-GM, 3-digit)` = "ICD-Code",
  `Diagnosis description` = "Description"
)

# Two-level header (note: one extra column for the description now)
ft <- add_header_row(
  ft,
  values = c("", "", "Counts", "RR", "Bonferroni-adjusted 95% CI"),
  colwidths = c(1, 1, 2, 1, 2)
)

ft <- theme_vanilla(ft)
ft <- bg(ft, bg = "#D9D9D9", part = "header")
ft <- bold(ft, part = "header")

# Alignment / typography
ft <- fontsize(ft, size = 10, part = "all")
ft <- align(ft, align = "left", part = "all")
ft <- align(
  ft,
  j = c("Total (n)", "Bleeding (n)", "RR", "Lower", "Upper"),
  align = "right",
  part = "all"
)
ft <- valign(ft, valign = "top", part = "all")

# Fit on page (PORTRAIT): fixed layout + controlled column widths
ft <- set_table_properties(ft, layout = "fixed", width = 1)

# Column widths (inches) tuned for portrait (7 columns now)
ft <- width(ft, j = 1, width = 1.1)          # diagnosis code
ft <- width(ft, j = 2, width = 2.2)          # diagnosis description
ft <- width(ft, j = 3:4, width = 0.85)       # counts
ft <- width(ft, j = 5, width = 0.6)          # RR
ft <- width(ft, j = 6:7, width = 0.7)        # CI

# Borders
inner <- fp_border(color = "grey", width = 0.5)
outer <- fp_border(color = "grey", width = 0.8)
ft <- border_inner_h(ft, border = inner, part = "all")
ft <- border_inner_v(ft, border = inner, part = "all")
ft <- border_outer(ft, border = outer, part = "all")


# ---- 11e Caption/legend + Word export (PORTRAIT) ----

caption_text <- "Table X. Relative risks (RRs) with Bonferroni-adjusted 95% confidence intervals for bleeding by principal diagnosis (ICD-10-GM, three-character level)."

legend_text  <- "Legend: Total (n) is the number of cases per diagnosis; Bleeding (n) is the number of bleeding cases. Lower/Upper denote the bounds of the Bonferroni-adjusted 95% confidence interval. Only diagnoses whose lower bound exceeds 1 are shown. 
Diagnosis descriptions correspond to the English three-character ICD-10 category labels; the underlying analysis was performed using German ICD-10-GM codes."

doc <- read_docx()

doc <- body_add_fpar(
  doc,
  fpar(
    ftext(caption_text, fp_text(bold = FALSE, font.size = 11)),
    fp_p = fp_par(text.align = "left")
  )
)

doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_flextable(doc, ft, align = "left")

doc <- body_add_par(doc, "", style = "Normal")

doc <- body_add_fpar(
  doc,
  fpar(
    ftext(legend_text, fp_text(font.size = 9, italic = TRUE)),
    fp_p = fp_par(text.align = "left")
  )
)

print(doc, target = word_file)

# ---- 12 Output: RR plot (Bonferroni-adjusted CI) ----
p_rr <- ergebnisNewM %>%
  ggplot(aes(x = reorder(HD, RR), y = RR, ymin = CI_lowerM, ymax = CI_upperM)) +
  
  # reference line in the background (draw first)
  geom_hline(yintercept = 1, color = "grey50", linewidth = 0.5, alpha = 0.5) +
  
  geom_pointrange(shape = 4) +
  geom_errorbar(aes(ymin = CI_lowerM, ymax = CI_upperM),
                width = 0.5, linewidth = 0.5) +
  
  scale_y_continuous(breaks = seq(0, 300, by = 20)) +
  coord_flip() +
  labs(y = "RR ± 95% CI", x = NULL) +
  theme_classic(base_size = 10) +
  theme(
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.line  = element_line(color = "grey50", linewidth = 0.8),
    axis.ticks = element_line(color = "grey50", linewidth = 0.8),
    panel.grid.major.x = element_line(color = "grey80", linewidth = 0.4),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor   = element_blank(),

# xis labels/text color
    axis.title = element_text(color = "black"),
# move the x-axis title further down
# (because of coord_flip(), your labs(y=...) becomes the x-axis title)
axis.title.x = element_text(margin = margin(t = 12)),

# optional: add a bit more space at the bottom if needed
plot.margin = margin(t = 5, r = 5, b = 15, l = 5)
  )


ggsave(
  filename = file.path(EXPORT_DIR, "Plot_HD_KI_RR_BF_bleeds.jpg"),
  plot = p_rr,
  units = "mm", width = 250, height = 400, dpi = 300, bg = "white"
)



