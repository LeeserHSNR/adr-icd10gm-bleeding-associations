# ---
# Project: ADRED Paper 1
# Script:  Preprocessing ADRED data (quasi-hierarchical model)
# Purpose: Create analysis-ready wide table `data` (predictors + outcomes + covariates)
# Input:   ../ADRED_Daten_R.xlsx
#          - T_ICD_semik_mod_para_21
#          - T_fall_z01_03_05_06
#          - T_Symptome_ohne_Duplikate
# Output:  R object in session: `data`
# Notes:   Preprocessing concept is documented in Section 00 below.
# Authors: Lea Leeser and Roberto Viviani
# date:    05/2025 - 11/2026 
# ---
  
# ---- 00 Overview: data sources + preprocessing steps ----
# The ADRED data are in an Excel file from which individual tables were saved
# separately, and perhaps further processed. These tables are:
#
# Data Sources:
# - T_Medikation_Aufnahmetyp_Anzahl:  Medications per patient presentation
# - T_ICD_semik_mod_para_21:          Side diagnoses (ICD-10, primarily 3-digit level)
# - T_fall_z01_03_05_06:              Demographic and patient-specific data
# - T_Symptome_ohne_Duplikate:        Documented symptoms
#
# The main source of data is T_ICD_semik_mod_para_21, which contains a row per medication,
# and an index variable case_id which conflates a patient id and the presentation number.
# In the present analysis, we only look at first presentations, so that after filtering
# presentations case_id is the same as a patient id.
#
# Each medication instance (one per row) is described by:
#  * ICD code, 5 digits (code and name)
#  * ICD code, 3 digits (code and name)
#  * Aufnahmetyp (stationär/vorstationär) and Diagnoseart (HD/ND)
#
# Diagnoses are extracted from ICD-10 codes at the 3-digit level and used as predictors.
# Side-effect outcomes are constructed as composite categories (e.g., “bleeding”, “fall”).
#
# Preprocessing proceeds in 6 steps:
# 1) In T_ICD_semik_mod_para_21, discard observations that should not be included.
# 2) Identify top-level predictors too rare to model; store as count_others and discard them.
# 3) Build crosstabs: one row per case, indicator columns for predictors; drop rare predictors.
# 4) Construct side-effect outcomes (bleeding, fall) and merge into predictors table.
# 5) Load patient-specific information (age, sex etc.) and standardize.
# 6) Join everything into a single analysis table.



# ---- 01 Setup: packages, options, paths, parameters ----
# Load required libraries
library(tidyr)
library(dplyr)
library(readxl)
library(stringi)
options(dplyr.summarise.inform = FALSE)

# Data path: the ADRED Excel file is located one level above the project root
# (R_Promotion_Paper1). Works whether this script is run from the project root
# (source/Source button) or from scripts/ (Rmd, knit, source with chdir = TRUE).
proj_root <- if (basename(getwd()) == "scripts") normalizePath("..", winslash = "/") else getwd()
data_root <- normalizePath(file.path(proj_root, ".."), winslash = "/", mustWork = FALSE)

candidates <- c(
  file.path(data_root, "ADRED_Daten_R.xlsx"),
  file.path(data_root, "ADRED_DATEN_R.xlsx"),
  file.path(data_root, "ADRED_DATEN_R", "ADRED_Daten_R.xlsx"),
  file.path(data_root, "ADRED_DATEN_R", "ADRED_DATEN_R.xlsx")
)

file_path <- candidates[file.exists(candidates)][1]

if (is.na(file_path)) {
  stop(
    "ADRED Excel not found. Looked for:\n- ",
    paste(candidates, collapse = "\n- "),
    "\n\nFiles in data_root (", data_root, "):\n",
    paste(list.files(data_root), collapse = ", ")
  )
}

DATA_FILE <- file_path

# MIN_TOPLEVEL_COUNT sets the minimum number of diagnosis entries for a
# three-character category to be retained. Categories below this count are collapsed into UNK.
MIN_TOPLEVEL_COUNT = 64

# VERBOSE controls the display of predictors that are eliminated during preprocessing.
VERBOSE = FALSE

# RELEVANT_HD_CODES: bleeding-associated principal diagnoses (ICD-10-GM, 3-digit).
# Used as a binary covariate (relevant_HD = 1 if any of these codes is present).
RELEVANT_HD_CODES <- c("D12", "K63", "C15", "K62", "K21", "D61", "N32")

# BLEEDING_ICD5_CODES: ICD-10-GM codes defining the bleeding outcome
# (bleeding = 1 if any of these codes is present in the case).
BLEEDING_ICD5_CODES <- c("I85.0", "I98.3", "K22.81", "K25.0", "K25.2", "K26.0", "K26.2",
                         "K26.4", "K27.2", "K28.0", "K31.82", "K55.22", "K55.32", "K57.11",
                         "K57.31", "K57.33", "K92.2", "N42.1", "N93.8", "N93.9", "R04.1",
                         "R04.8", "R04.9", "R58")

# BLEEDING_ICD3_EXCLUDE: 3-digit categories containing outcome-defining codes.
# They are removed from the predictor set to avoid outcome leakage. Only categories
# above MIN_TOPLEVEL_COUNT are actually present and removed (see console output).
BLEEDING_ICD3_EXCLUDE <- c("I85", "I98", "K22", "K25", "K26", "K27", "K28",
                           "K31", "K55", "K57", "K92", "N42", "N93", "R04", "R58")

# The data are contained in a set of Excel tables, linked by the key case_id.


# ---- 02 Import: ICD table (T_ICD_semik_mod_para_21) + basic variables ----
# load ICD data
srcdata <- read_excel(DATA_FILE, sheet = "T_ICD_semik_mod_para_21")


# create variables pat_id and presentation from MANU_case_id
# pat_id is not used later; after filtering to first presentations it is synonymous with case_id
srcdata$pat_id <- sub("_\\d+", "", srcdata$MANU_case_id, perl = T)
srcdata$presentation <- as.numeric(sub("\\d+_", "", srcdata$MANU_case_id, perl = T))


# create ultrashort ICD category indicator (first character of ICD_Dreisteller)
srcdata <- srcdata %>%
  mutate(ICD_Group = substr(ICD_Dreisteller, 1, 1))



# ---- 03 Derived variables: relevant_HD (case-level marker) ----
# Create a dummy per MANU_case_id (case-level)
dummy_table <- srcdata %>%
  group_by(MANU_case_id) %>%
  summarize(relevant_HD = as.integer(any(ICD_Dreisteller %in% RELEVANT_HD_CODES)), .groups = "drop")


# Join the dummy back to the original data (optional)
srcdata <- srcdata %>%
  left_join(dummy_table, by = "MANU_case_id")


# ---- 04 Filtering step 1: exclude unwanted rows (presentations, UNK, etc.) ----
# First filtering:
# - keep first presentation only
# - drop ICD codes "UNK" and too-short codes
# - drop Aufnahmetyp "vorstationär"
# - drop Diagnoseart "HD"

srcdata <- filter(srcdata, ICD_Dreisteller != "UNK", stri_length(ICD_Dreisteller) > 2, 
                  presentation == 1,Aufnahmetyp != "vorstationär",Diagnoseart !="HD")


# ---- Filtering step 2: collapse rare three-character categories into UNK + count_others ----
# Identify rare three-character categories (below MIN_TOPLEVEL_COUNT) and collapse them into 'UNK'
ICDcount <- srcdata %>% group_by(ICD_Dreisteller) %>% summarize(count = n())
badICD <- ICDcount[ICDcount$count < MIN_TOPLEVEL_COUNT,]
print(sprintf("-- Total three-character categories: %d; categories to be dropped: %d",
              nrow(ICDcount), nrow(badICD)))
if (VERBOSE) {
  print("Dropped three-character categories:")
  print(badICD$ICD_Dreisteller)
}
print("The dropped three-character categories are in variable badICD")
for (i in 1 : nrow(badICD))
  srcdata$ICD_Dreisteller[srcdata$ICD_Dreisteller == badICD$ICD_Dreisteller[i]] <- "UNK"
rm(list = c("i", "ICDcount"))

# Count UNK observations per case_id (stored as count_others)
count_others <- srcdata %>% group_by(MANU_case_id) %>% 
  summarize(count_others = sum(ICD_Dreisteller == "UNK"), dplyr.summarise.inform = F) %>% 
  rename(case_id = MANU_case_id)

# Drop UNK rows after counting them
srcdata <- filter(srcdata, ICD_Dreisteller != "UNK")



# ---- 06 Predictors: crosstab ICD-3 (wide table) + drop rare predictors ----
# Form crosstabs for ICD-3-digit predictors (one row per case; 0/1 indicators)
ICDtable <- srcdata %>%
  group_by(MANU_case_id, ICD_Dreisteller, relevant_HD) %>%
  summarize(n = 1, .groups = "drop") %>%
  spread(ICD_Dreisteller, n, fill = 0)


# helper: keep columns with at least n cases
idcols <- function(tbl, n) {
  cases <- select(tbl, -MANU_case_id) %>% sapply(sum)
  print(sprintf("There are %d variables with less than %d cases",
                sum(cases < n), n))
  if (VERBOSE) {
    print(sprintf("Variables with less than %d cases:", n))
    print(names(cases[cases < n]))
  }
  colnames <- names(cases[cases >= n])
  colnames
}


print("-- Dropping ICD 3 codes variables with few observations")
ICDtable <- select(ICDtable, MANU_case_id, any_of(idcols(ICDtable, MIN_TOPLEVEL_COUNT)))

print("-- Dropping medication variables starting with '*'")
ICDtable <- select(ICDtable, MANU_case_id, !starts_with("*"))


print(sprintf("-- Total cases: %d", nrow(ICDtable)))
print(sprintf("-- Total ICD 3-digit codes (Prädiktoren): %d", ncol(ICDtable) - 1))


# 07 Predictors: crosstab initial letter (first character) + drop rare predictors ----

ICDgrouptable <- srcdata %>%
  group_by(MANU_case_id, ICD_Group) %>%
  summarize(n = 1, .groups = "drop") %>%
  spread(ICD_Group, n, fill = 0)


# helper (duplicated in original code): keep columns with at least n cases
idcols <- function(tbl, n) {
  cases <- select(tbl, -MANU_case_id) %>% sapply(sum)
  print(sprintf("There are %d variables with less than %d cases",
                sum(cases < n), n))
  if (VERBOSE) {
    print(sprintf("Variables with less than %d cases:", n))
    print(names(cases[cases < n]))
  }
  colnames <- names(cases[cases >= n])
  colnames
}


print("-- Dropping initial-letter variables with few observations")
ICDgrouptable <- select(ICDgrouptable, MANU_case_id, any_of(idcols(ICDgrouptable, MIN_TOPLEVEL_COUNT)))

print("-- Dropping medication variables starting with '*'")
ICDgrouptable <- select(ICDgrouptable, MANU_case_id, !starts_with("*"))


print(sprintf("-- Total cases: %d", nrow(ICDgrouptable)))
print(sprintf("-- Total initial letters (first-character level): %d", ncol(ICDgrouptable) - 1))

# Rename ID column in ICDgrouptable for consistency before merging
ICDgrouptable <- ICDgrouptable %>% rename(case_id = MANU_case_id)

# ---- 08 Outcomes: build bleeding/fall side-effect indicators and merge ----
# Load ICD data with relevant columns and filter for inpatient admissions only
ICD_Code <- read_excel(DATA_FILE, sheet = "T_ICD_semik_mod_para_21") %>%
  select(case_id = MANU_case_id, ICD_Fünfsteller, ICD_Fünsteller_Text, Admission_Type = Aufnahmetyp, Diagnosis_Type = Diagnoseart, ICD_Dreisteller, ICD_Dreisteller_Text) %>%
  filter(Admission_Type == "stationär") %>%
  # Create a binary cross-tab indicating presence (1) or absence (0) of each ICD 5-digit code per case
  group_by(ICD_Fünfsteller, case_id) %>%
  summarize(n = 1) %>%
  spread(ICD_Fünfsteller, n, fill = 0)

# Bleeding outcome indicator (ICD-5 list)
tmp <- select(ICD_Code, any_of(BLEEDING_ICD5_CODES))

ICD_Code$bleeding <- as.numeric(rowSums(tmp) > 0)  # 1 = bleeding diagnosis present, 0 = absent

# Fall outcome indicator (pattern-based selection)
tmp <- select(ICD_Code, matches("^R09|^R26|^R27|^R40|^R41|^R42|^R44|^R55|^I95"))
ICD_Code$fall <- as.numeric(apply(tmp, 1, sum) > 0)

rm(tmp)

# Exclude ICD-3 predictors used to define the bleeding outcome (avoid outcome leakage)
raus <- intersect(BLEEDING_ICD3_EXCLUDE, names(ICDtable))
print(sprintf("-- Entfernte Blutungscodes: %d (%s)",
              length(raus), paste(raus, collapse = ", ")))

ICDtable <- ICDtable %>%
  select(-any_of(BLEEDING_ICD3_EXCLUDE))

# Rename ID column in ICDtable for consistency before merging
ICDtable <- ICDtable %>%
  rename(case_id = MANU_case_id)

# Create unique side-effect table and merge into ICDtable
side_effects <- ICD_Code %>%
  select(case_id, bleeding, fall) %>%
  distinct()

ICDtable <- ICDtable %>%
  left_join(side_effects, by = "case_id")



# ---- 09 Covariates: patient information (age/sex) ----
# patient anagraphical data: age, sex, and morbidity score
patdata <- read_excel(DATA_FILE, sheet = "T_fall_z01_03_05_06") %>%
  select(case_id=MANU_case_id, Z_Alter, Z_Geschlecht, Aufnahmetyp) %>%
  filter(Aufnahmetyp == "stationär") %>%   
  mutate(Age.z=scale(Z_Alter)[,1], Sex=Z_Geschlecht-1,
         Z_Alter=NULL, Z_Geschlecht=NULL)


# ---- 10 Symptoms: crosstab + drop rare symptom variables ----
# helper: keep symptom columns with at least `threshold` cases
idcols <- function(df, threshold) {
  col_sums <- colSums(df[, setdiff(names(df), "case_id")] == 1, na.rm = TRUE)
  names(col_sums)[col_sums >= threshold]
}

symptoms <- read_excel(DATA_FILE, sheet = "T_Symptome_ohne_Duplikate") %>% 
  select(case_id, pt_text) %>%  # pt_text is a symptom text
  group_by(pt_text, case_id) %>% 
  summarize(n = 1, .groups = "drop") %>% 
  spread(pt_text, n, fill = 0)

print("-- Dropping symptom variables with few observations")
cols_to_keep <- intersect(idcols(symptoms, 60), names(symptoms))
symptoms <- select(symptoms, case_id, any_of(cols_to_keep))

# ---- 11 Diagnostics: variable overview (first/last names) ----
printcols <- function(tbl) {
  colnames <- tbl %>% select(-case_id) %>% names()
  print(c(colnames[1], colnames[length(colnames)]))
}

print("")
print("-- Available variable ranges:")
printcols(ICDtable)
printcols(ICDgrouptable)
printcols(patdata)
printcols(symptoms)
printcols(count_others)


# ---- 12 Join: create final analysis table `data` ----
data <- ICDtable %>%
  inner_join(ICDgrouptable, by = "case_id") %>%
  inner_join(symptoms, by = "case_id") %>%
  inner_join(count_others,by = "case_id") %>%
  inner_join(patdata, by = "case_id") %>%
  {
    all_cols <- colnames(.)
    case_id_col <- "case_id"
    # delete bleeding, fall and relevant_HD from the middle
    other_cols <- setdiff(all_cols, c(case_id_col, "bleeding", "fall", "relevant_HD"))
    select(., all_of(case_id_col), all_of(other_cols), bleeding, fall, relevant_HD)
  }


# ---- 99 Cleanup ----
rm(list = c("ICD_Code", "patdata", "symptoms"))

