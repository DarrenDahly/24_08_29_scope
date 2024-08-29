

  library(readxl)
  library(tidyverse)
  library(janitor)
  library(testthat)
  
# Meta data --------------------------------------------------------------------
  
  file_   <- "data/OPTIMATE_excel_export_20230703110937.xlsx"
  sheets_ <- excel_sheets(path = file_)
  
  # Study variables
  study_vars    <- read_excel(
    file_, sheet = which(sheets_ == "Study variable list")
  ) |> clean_names()
  
  study_vars$variable_name <- make_clean_names(study_vars$variable_name)
  
  # Report variables
  report_vars   <- read_excel(
    file_, sheet = which(sheets_ == "Report variable list")
  ) |> clean_names()
  
  # Survey variables
  # survey_vars   <- read_excel(file_, sheet = which(sheets_ == "Survey variable list")) |> clean_names()
  
  # Response options and labels
  field_options <- read_excel(
    file_, sheet = which(sheets_ == "Field options")
  ) |> clean_names()
  
# Data -------------------------------------------------------------------------
  
  ## Study data ----
  study_data <- read_excel(file_, sheet = which(sheets_ == "Study results")) |>
    clean_names() |>
    remove_empty() |>
    filter(!is.na(randomization_group)) |> # Remove 2 TEST IDs
    rename(
      site = site_abbreviation
    )
  
  # print(distinct(select(study_vars, visit_name, form_name)), n = 23)
  # 1  Recruitment      Informed Consent Process                         Sheet 1
  # 2  Recruitment      Randomization
  # 3  Recruitment      Enrolment information pre-Clinical Visit
  # 4  Baseline         Previous interventions
  # 5  Baseline         Basic cancer diagnosis data (1)
  # 6  Baseline         Basic cancer diagnosis data (2)
  # 7  Baseline         Surgery
  # 8  Baseline         Chemotherapy
  # 9  Baseline         Endocrine therapy
  # 10 Baseline         Radiotherapy
  # 11 Baseline         Family history cancer
  # 12 Baseline         Nurse patient Reports
  # 13 Baseline         Nutrition Assessment (1)
  # 14 Baseline         Nutrition Assessment (2)
  # 15 Baseline         WCRF/AICR
  # 16 Baseline         Survivorship Clinical Visit - Baseline
  # 17 Follow-Up        Follow-up checklist
  # 18 Follow-Up        Survivorship Clinical Visit - Follow-up
  # 19 End of the Study End of the study checklist
  # 20 End of the Study Nutrition Assessment (1)
  # 21 End of the Study Nutrition Assessment (2)
  # 22 End of the Study WCRF/AICR
  # 23 End of the Study Survivorship Clinical Visit - End of the study
  
  ### Variables ----
  
  # Numeric
  tar <- study_vars$variable_name[study_vars$exported_field_type == "numeric"]
  tar <- names(study_data)[names(study_data) %in% tar]
  study_data[tar] <- map_df(study_data[tar], as.numeric)
  
  study_data$post_bmi <- as.numeric(study_data$post_bmi)
  study_data$post_waist_aver <- as.numeric(study_data$post_waist_aver)
  
  study_data$bmi <- as.numeric(study_data$bmi)
  study_data$waist_aver <- as.numeric(study_data$waist_aver)
  
  study_data$age <- as.numeric(study_data$age)
  study_data$age_dg <- as.numeric(study_data$age_dg)
  study_data$treat_end_days <- as.numeric(study_data$treat_end_days)
  
  study_data$randomized_on <- as.POSIXct(
    study_data$randomized_on, format = "%Y-%m-%d %H:%M:%OS"
  )
  
  
  # Dates/Times
  tar <- c(names(study_data)[grepl("date", names(study_data))], "dob")
  study_data[tar] <- map_df(
    study_data[tar], function(x) as.POSIXct(x, format = "%d-%m-%Y")
  )
  
  study_data$ic_sig_lag <- as.numeric(difftime(
    study_data$date_inc, study_data$ic_date, units = "days"
  ))
  
  # Corrections
  study_data$height[study_data$height == 165.00 & !is.na(study_data$height)] <- 1.65
  
  
  # New
  # survey count
  study_data <- left_join(
    study_data,
    study_data |>
      select(
        participant_id, (starts_with("surv_m") & contains("date"))
      ) |>
      pivot_longer(
        starts_with("surv_m"), names_to = "Month", values_to = "Date"
      ) |>
      na.omit() |>
      group_by(participant_id) |>
      count(participant_id) |>
      rename(surv_count = n),
    by = "participant_id"
  )
  
  # Handgrip averages
  study_data$post_handgrip_l <- rowMeans(
    study_data[c("post_handgr_1l", "post_handgr_2l", "post_handgr_3l")]
  )
  study_data$post_handgrip_r <- rowMeans(
    study_data[c("post_handgr_1r", "post_handgr_2r", "post_handgr_3r")]
  )
  study_data$handgrip_l <- rowMeans(
    study_data[c("handgr_1l", "handgr_2l", "handgr_3l")]
  )
  study_data$handgrip_r <- rowMeans(
    study_data[c("handgr_1l", "handgr_2l", "handgr_3l")]
  )
  
  ### Add variable labels ----
  # Labelling function for existing variables
  add_labs <- function(data, labs){
    lab_names <- names(data)
    df <- deparse(substitute(data))
    for(i in 1:length(lab_names)){
      lab <- labs$field_label[labs$variable_name == lab_names[i]]
      lab <- lab[!is.na(lab)]
      attr(data[[lab_names[i]]], "label") <- lab
    }
    print(paste(df, "labels added"))
    return(data)
  }
  
  study_data <- add_labs(study_data, study_vars)
  
  # Label new variables
  attr(study_data$ic_sig_lag, "label") <- "Days between IC and signature"
  attr(study_data$randomization_group, "label") <- "Study arm at randomization"
  attr(study_data$surv_count, "label") <- "Surveys completed"
  attr(study_data$group, "label") <- "Study arm noted at follow-up"
  attr(study_data$post_handgrip_l, "label") <- "Mean handgrip L"
  attr(study_data$post_handgrip_r, "label") <- "Mean handgrip R"
  attr(study_data$handgrip_l, "label") <- "Mean handgrip L (Post)"
  attr(study_data$handgrip_r, "label") <- "Mean handgrip R (Post)"
  
  # map(study_data, attr, "label")
  
## Report data ----
  
  # print(distinct(select(report_vars, repeating_data_name, form_name)), n = 43)
  
  # 1 N.  Comorbidities                  Comorbidities (Nurse)           Sheet 2
  # 2 N.  Medication                     Medication                            3
  # 3 N.  Use of resources               Use of resource                       4
  # 4 N.  Pathway Symptoms               Fatigue                               5
  # 5 N.  Pathway Symptoms               Insomnia
  # 6 N.  Pathway Symptoms               Problems with concentration
  # 7 N.  Pathway Symptoms               Problems with memory
  # 8 N.  Pathway Symptoms               General Pain
  # 9 N.  Pathway Symptoms               Joint pain
  # 10 N. Pathway Symptoms               Abdominal pain
  # 11 N. Pathway Symptoms               Swelling
  # 12 N. Pathway Symptoms               Hot Flashes
  # 13 N. Pathway Symptoms               Vaginal Dryness
  # 14 N. Pathway Symptoms               Decreased libido
  # 15 N. Pathway Symptoms               Vaginal Discomfort
  # 16 N. Pathway Symptoms               Urinary urgency
  # 17 N. Pathway Symptoms               Urinary frequency
  # 18 N. Pathway Symptoms               Change in colour of urine
  # 19 N. Pathway Symptoms               Urinary incontinence
  # 20 N. Pathway Symptoms               Constipation
  # 21 N. Pathway Symptoms               Diarrhoea
  # 22 N. Pathway Symptoms               Faecal incontinence
  # 23 N. Pathway Symptoms               Emotional distress- anxiety
  # 24 N. Pathway Symptoms               Emotional distress- depression
  # 25 N. Pathway Symptoms               Fear of recurrence
  # 26 N. Pathway Symptoms               1. OTHER SYMPTOM
  # 27 N. Pathway Symptoms               2. OTHER SYMPTOM
  # 28 N. Pathway Symptoms               3. OTHER SYMPTOM
  # 29 N. Pathway Symptoms               4. OTHER SYMPTOM
  # 30 N. Pathway Symptoms               5. OTHER SYMPTOM
  # 31 N. Pathway Symptoms               6. OTHER SYMPTOM
  # 32 N. Pathway Symptoms               7. OTHER SYMPTOM
  # 33 N. Pathway Symptoms               8. OTHER SYMPTOM
  # 34 N. Pathway Symptoms               9. OTHER SYMPTOM
  # 35 N. Pathway Symptoms               10. OTHER SYMPTOM
  # 36 N. Nurse Visits                   Nurse visit                           6
  # 37 IR. Adverse Event                 Adverse Event                         7
  # 38 IR. Drop out                      Drop out                              8
  # 39 N. Medical Review Required Report Medical Review Required               9
  # 40 N. Referral                       Referral                             10
  # 41 D. Dietitian visit                Dietitian visit                      11
  # 42 D. Dietitian visit                Clinic NCP (Personalised nutritio…   11
  # 43 N. Patient hospital visit         Patient hospital visit report        12
  
  comorbid <- read_excel(file_, sheet = 2)  |> clean_names() |> remove_empty()
  meds     <- read_excel(file_, sheet = 3)  |> clean_names() |> remove_empty()
  resource <- read_excel(file_, sheet = 4)  |> clean_names() |> remove_empty()
  symptoms <- read_excel(file_, sheet = 5)  |> clean_names() |> remove_empty()
  nurse    <- read_excel(file_, sheet = 6)  |> clean_names() |> remove_empty()
  # aes      <- read_excel(file_, sheet = 7)  |> clean_names() |> remove_empty()
  drops    <- read_excel(file_, sheet = 8)  |> clean_names() |> remove_empty()
  med_rev  <- read_excel(file_, sheet = 9)  |> clean_names() |> remove_empty()
  referal  <- read_excel(file_, sheet = 10) |> clean_names() |> remove_empty()
  diet_vst <- read_excel(file_, sheet = 11) |> clean_names() |> remove_empty()
  hosp_vst <- read_excel(file_, sheet = 12) |> clean_names() |> remove_empty()
  
  # Note: The sheet for adverse events is blank
  
  ### Add variable labels ----
  comorbid <- add_labs(comorbid, report_vars)
  meds     <- add_labs(meds,     report_vars)
  resource <- add_labs(resource, report_vars)
  symptoms <- add_labs(symptoms, report_vars)
  nurse    <- add_labs(nurse,    report_vars)
  # aes      <- add_labs(aes,      report_vars)
  drops    <- add_labs(drops,    report_vars)
  med_rev  <- add_labs(med_rev,  report_vars)
  referal  <- add_labs(referal,  report_vars)
  diet_vst <- add_labs(diet_vst, report_vars)
  hosp_vst <- add_labs(hosp_vst, report_vars)
  
  # map(symptoms, attr, "label")
  
  
## Survey data ----
  
  # print(distinct(select(survey_vars, survey_name, form_name)), n = 20)
  # 1 Ongoing survey                                                          13
  # Symptoms
  # 2 Ongoing survey
  # Fear of recurrence
  # 3 Ongoing survey
  # Hormonal adherence
  # 4 Ongoing survey
  # Dietetic assessment
  # 5 PART II: Patient Experience Survey (Control)                            14
  # PART II: Patient Experience Survey (Control)
  # 6 Quality of Life - specific block for Breast Cancer patients             15
  # EORTC QLQ - BR23
  # 7 Quality of Life Survey                                                  16
  # EORTC QLQ-C30
  # 8 Quality of Life Survey
  # EQ-5D-5L
  # 9 Quality of Life Survey
  # Self-Care Agency-R
  # 10 Changes to report                                                      17
  # Medication changes
  # 11 Changes to report
  # Use of resources/services
  # 12 Changes to report
  # Medical conditions changes
  # 13 Changes to report
  # Hospital visit
  # 14 Baseline sociodemographic data                                         18
  # Baseline sociodemographics
  # 15 End of the study sociodemographic data                                 19
  # End of the study sociodemographic data
  # 16 Quality of Life - specific block for Cervical Cancer patients          20
  # EORTC QLQ – CX24
  # 17 Quality of Life - specific block for Endometrial Cancer patients       21
  # EORTC QLQ – EN24
  # 18 PART II: Patient Experience Survey (intervention)                      22
  # PART II: Patient Experience Survey (intervention)
  # 19 PART I: Usability and Satisfaction Survey                              23
  # PART I
  # 20 PART III: Patient Suggestions (Open questions)
  # PART III: Patient Suggestions (Open questions)                          24
  
  survey   <- read_excel(file_, sheet = 13) |> clean_names() |> remove_empty()
  pex_con  <- read_excel(file_, sheet = 14) |> clean_names() |> remove_empty()
  qol_bc   <- read_excel(file_, sheet = 15) |> clean_names() |> remove_empty()
  qol      <- read_excel(file_, sheet = 16) |> clean_names() |> remove_empty()
  changes  <- read_excel(file_, sheet = 17) |> clean_names() |> remove_empty()
  demo_bl  <- read_excel(file_, sheet = 18) |> clean_names() |> remove_empty()
  demo_eos <- read_excel(file_, sheet = 19) |> clean_names() |> remove_empty()
  qol_cc   <- read_excel(file_, sheet = 20) |> clean_names() |> remove_empty()
  qol_ec   <- read_excel(file_, sheet = 21) |> clean_names() |> remove_empty()
  pex_int  <- read_excel(file_, sheet = 22) |> clean_names() |> remove_empty()
  use_sat  <- read_excel(file_, sheet = 23) |> clean_names() |> remove_empty()
  suggs    <- read_excel(file_, sheet = 24) |> clean_names() |> remove_empty()
  
  survey$survey_completed_on <- as.POSIXct(
    survey$survey_completed_on,
    format = "%d-%m-%Y %H:%M:%OS"
  )
  
  survey   <- add_labs(survey,   survey_vars)
  pex_con  <- add_labs(pex_con,  survey_vars)
  qol_bc   <- add_labs(qol_bc,   survey_vars)
  qol      <- add_labs(qol,      survey_vars)
  changes  <- add_labs(changes,  survey_vars)
  demo_bl  <- add_labs(demo_bl,  survey_vars)
  demo_eos <- add_labs(demo_eos, survey_vars)
  qol_cc   <- add_labs(qol_cc,   survey_vars)
  qol_ec   <- add_labs(qol_ec,   survey_vars)
  pex_int  <- add_labs(pex_int,  survey_vars)
  use_sat  <- add_labs(use_sat,  survey_vars)
  suggs    <- add_labs(suggs,    survey_vars)
  
  # map(survey, attr, "label")
  
# QC ---------------------------------------------------------------------------
  
  # Consent data before randomization date?
  expect_equal(TRUE, all(study_data$ic_date < study_data$randomized_on))
  
# Save -------------------------------------------------------------------------
  
  survey_list <- Hmisc::llist(
    survey, pex_con, qol_bc, qol, changes, demo_bl, demo_eos, qol_cc, qol_ec,
    pex_int, use_sat, suggs
  )
  
  report_list <- Hmisc::llist(
    comorbid, meds, resource, symptoms, nurse, aes, drops, med_rev, referal,
    diet_vst, hosp_vst
  )
  
  save(
    study_data, report_list, survey_list,
    field_options, study_vars, survey_vars,
    file = "data/data.RData"
  )
  
  print("data.R all done")
  
  rm(list = ls())



 
# Haven labelled variables
  # Convert the haven_labelled variables into factors
  tar <- map_lgl(patients, is.labelled)
  patients[tar] <- map(patients[tar], as_factor)

  tar <- map_lgl(meals, is.labelled)
  meals[tar] <- map(meals[tar], as_factor)

  # Get rid of the other SPSS attributes
  patients <- zap_formats(patients)
  meals    <- zap_formats(meals)


  colnames(data) <- tolower(colnames(data))
  colnames(data) <- gsub("^\\s+|\\s+$", "", colnames(data)) # trailing/leading space

  colnames(data) <- gsub("[[:punct:]]", "", colnames(data))
  colnames(data) <- gsub(" ", ".", colnames(data))

  colnames(data) <- gsub("^[[:digit:]]", "", colnames(data))

  colnames(data) <- gsub(" ", ".", colnames(data))
  colnames(data) <- gsub("\\/", ".", colnames(data))
  colnames(data) <- gsub("\\,", "", colnames(data))
  colnames(data) <- gsub("\\?", "", colnames(data))

  as.character(gsub("^\\s+|\\s+$", "", colnames(data))) # lead, trailing white space
  colnames(data) <- make.names(colnames(data), unique = TRUE)

  sub("^([0-9])(.+)", "\\2\\1", colnames(data)) # Move digit from front to end

  data$x[grepl("XXXXX",  data$x)]    <- "xxxxxxxx"

# This captures a number of any length \\d+ in the () as \\1, and then puts . in
# front what was captured.
  colnames(data) <- sub("(\\d+)", "\\.\\1", colnames(data))

# Remove . at end of string
  colnames(data) <- gsub("\\.$", "", colnames(data))

# keep only the digit from a string
  gregexpr("[[:digit:]]+", data$x) %>%
  regmatches(data$x, .) %>%
  unlist() %>%
  as.numeric()

  readr::parse_number() # "Parse numbers, flexibly."

# select values that match string in column
  data$x[grepl("x",  data$x)]    <- "X"

# Keep only the first digit

  data$x <- sub("([0-9]{1}).*", "\\1", data$x)

# Keep a match
# Keep everything after a given character

  data$x <-  regmatches(data$x, regexpr("([^XXX]*$)", data$x))

# Add a separator to a pattern and then use separate to split it
  ecg1$bp_exam <- sub("(BP:\\s*\\d+/\\d{,2})", "\\1\\SEPP", ecg1$bp_exam)
  ecg1 <- separate(ecg1, bp_exam, c("bp_exam", "exam"), sep = "SEPP")

# Keep just the pattern you want
  ecg1$bp_exam <-  sub(".*?(\\d+/\\d{,2}).*", "\\1", ecg1$bp_exam)

# Remove digit from position

# Rouding while keeping trailing zeros

  formatC(round(x, 2), format = "f", 2)

# Finding duplicated column names

  full.1 <- full[, colnames(full) %in%
                   unique(colnames(full)[ duplicated(colnames(full))])]

  full.2 <- full[, !(colnames(full) %in%
                       unique(colnames(full)[ duplicated(colnames(full))]))]

# Repeats values to fill in NAs
# http://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
  repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
    ind = which(!is.na(x))        # get positions of nonmissing values
    if(is.na(x[1]))               # if it begins with a missing, add the
      ind = c(1,ind)              # first position to the indices
    rep(x[ind], times = diff(     # repeat the values at these indices
      c(ind, length(x) + 1) ))    # diffing the indices + length yields how often
  }

# Find the matching set of colnames among a set of files

  form.cols <- function(target = getwd()) {

  	require(readxl)

  	form <- read_excel(target, skip = 1)

  	colnames(form)  <- gsub("[[:punct:]]", "", colnames(form))

  	return(colnames(form))

  }

  forms <- list("data/final.splitsets/senato40.xls",
  							"data/final.splitsets/senato41.xls",
  							"data/final.splitsets/senato42.xls",
  							"data/final.splitsets/senato43.xls",
  							"data/final.splitsets/senato44.xls",
  							"data/final.splitsets/senato45.xls",
  							"data/final.splitsets/senato46.xls",
  							"data/final.splitsets/senato47.xls",
  							"data/final.splitsets/senato48.xls",
  							"data/final.splitsets/senato49.xls",
  							"data/final.splitsets/senato50.xls",
  							"data/final.splitsets/senato51.xls",
  							"data/final.splitsets/senato52.xls")

  colnamesList <- lapply(forms, form.cols)

# Get the column names shared across datasets, and add the new one for type
  shared.vars <- c(Reduce(intersect, colnamesList), "adr.type")

# Keep columns with less missing data

  data <- data[colSums(!is.na(data)) > x]

# Index columns whose name includes a given string
  x <- data[, grepl("xxx", names(data))]

  data[, grepl("xxxx", names(data))] <-  x[colSums(!is.na(x)) > 0]


# Use attributes in a factor label

  data$x <- factor(data$x,
                  labels = attributes(attributes(data$x)$labels)$names)

# Add variable labels ####

  label(data) <- lapply(names(varlabs),
                        function(x) label(data[, x]) = varlabs[x])

# Save labels from Haven

  label.list <- list()

  for (i in seq_along(data)){
    label.list[[i]] <- attributes(data[[i]])$label
  }

# Remove labelled class from Haven
  is_labelled <- function(x) {
    if (length(class(x)) > 1) return(any(class(x) == "labelled"))
    return(class(x) == "labelled")
  }

  unlabel <- function(x) {
    if (is.data.frame(x) || is.matrix(x)) {
      for (i in 1:ncol(x)) {
        if (is_labelled(x[[i]])) x[[i]] <- unclass(x[[i]])
      }
    }
    else {
      # remove labelled class
      if (is_labelled(x)) x <- unclass(x)
    }
    return(x)
  }

# Convert lablled to factors, or remove the labels

  data <- mutate_if(data, is.labelled, as_factor)
  data <- mutate_if(data, is.labelled, zap_label)


# Tidy character values ####

  View(data[, sapply(data, class) == 'character'])


# Remove leading and trailing white space from characters

  trim <- function(x) {
    if (is.character(x) == TRUE) {
      x <- as.character(gsub("^\\s+|\\s+$", "", x))
    }
    else {
      x <- x
    }
  }

  data <- as.data.frame(lapply(data, trim), stringsAsFactors = FALSE)

  data[, c()] <- apply(data[, c()] , 2,
                       function(x) gsub("\\\"", "", x))


# Gets rid of characters from what should be numeric values. (Surely an easier
# way?)

  x <- gregexpr("[[:digit:]]+", x) %>%
		   regmatches(x, .) %>%
		   unlist() %>%
		   as.numeric()
# Tidy factors ####

  bi.factor <- function(x, ...){
    x <- factor(x, levels = c(1, 2), labels = c("Yes", "No"))
  }

# Find variables that look like factors because min = 1 and max = 2
  lapply(data[, sapply(data, max, na.rm = TRUE) == 2 &
                sapply(data, min, na.rm = TRUE) == 1 ],
         bi.factor,
         stringsAsFactors = FALSE) %>%
    as.data.frame() -> data[, sapply(data, max, na.rm = TRUE) == 2 &
                              sapply(data, min, na.rm = TRUE) == 1 ]


# Tidy time ####

  data$x <- paste(data$date, data$time, sep = " ") %>%
            as.POSIXct(format = "%d/%m/%Y %H:%M:%S", tz = "GMT")

# Add ":" from 2 spots back
  data$time <- gsub("(.{2})$", ":\\1",  data$time)

# Time from excel sheep (e.g. 41202)

  data$x <- as.POSIXct(as.numeric(data$x) * 60 * 60 * 24,
             origin = "1899-12-30")

  as.POSIXct(x, origin = "1970-01-01", tz = "GMT")

# Add time to date variables

  x <- unlist(lapply(x, function(x){
  	                      return(as.character(seq(as.Date(x),
  																								length = 2,
  																				        by = "-100 years")[2]))
  								      }))

# Create decimel hours from HH:MM:SS

  x <- sapply(strsplit(x, ":"), function(x) {
  															  x <- as.numeric(x)
  																x[1] + (x[2] / 60) + (x[3] / 3600)
  															})


# Duplicates ####

  allDup <- function(value){
    duplicated(value) | duplicated(value, fromLast = TRUE)
  }

  data[allDup(data_frame(A = data$A,
                         B = data$B)), ]


# Missing values ####

# Recode missings to NA

  replMiss <- function(x) {
    if (is.character(x) == TRUE) {
      ifelse(x == "99.000000" | x == "999.000000", NA, x)
    }
    else {
      x <- x
    }
  }

  data <- as.data.frame(lapply(data, replMiss), stringsAsFactors = FALSE)

  propMiss(data)

# Do these data match the existing data?
  a <- select(clin, id, sex, age, surgery, arm, procedure) %>%
       mutate(surgery = factor(surgery))
  b <- select(rec,  id, sex, age, surgery, arm, procedure)

  comparison <- compare(a, b)

  View(comparison$tM)

  ids <- anti_join(a, b)$id

  filter(a, id %in% ids) %>% View()
  filter(b, id %in% ids) %>% View()

# NaN

  data[sapply(data, is.numeric)] <- apply(data[sapply(data, is.numeric)], 2,
                                          function(x) ifelse(is.nan(x), NA, x))



# Haven



