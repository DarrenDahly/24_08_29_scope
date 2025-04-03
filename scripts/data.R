
  library(readxl)
  library(tidyverse)
  library(janitor)
  library(testthat)

# Data -------------------------------------------------------------------------

  file_   <- "data/Factors_Influencing_Use_&_Choice_of_COS_OMIs_13.8.csv"

  # Create a metadatafile with the "clean" variable names. Don't re-run this.
  # x <- read_csv(file_) |> clean_names() # Read in the dataset
  # write_csv( # Make a new dataframe with the names and labels from the raw data
  #   data_frame(var_name = names(x), labels = unlist(x[1, ])), file = "meta_data.csv"
  #   ); rm(x)
  # Note: I deleted the 2nd row from the raw data that contained the data labels
  # to stop it throwing off the variable classes then they are read in below

  data <- read_csv(file_, na = c("", "-99")) |>
    clean_names() |>
    remove_empty() |>
    select(
      -contains("recipient"), -contains("location"), -ip_address
      ) |>
    mutate_if(is.character, utf8::utf8_encode) |>
    arrange(start_date) |>
    mutate(pid = row_number())

  # Retain these for labelling at the end of the script
  original_vars <- names(data)

## Variables --------------------------------------------------------------------

  # Fix dates and convert to posixct
  tar <- names(select(data, contains("Date")))
  data[tar] <- map_df(
    data[tar],
    function(x) as.POSIXct(gsub("^0", "", x), format = "%m/%d/%Y %H:%M")
    )

  data$progress <- as.numeric(data$progress)

  data$duration_in_minutes <- data$duration_in_seconds / 60

  data$finished <- factor(data$finished, labels = c("No", "Yes"))

  data$q2 <- factor(
    data$q2, levels = levels(factor(data$q2))[c(5, 1:4)]
    )

  tar <- names(select(data, starts_with("q8_")))
  levs <- levels(factor(data$q8_1))[c(3, 2, 1, 4)]
  data[tar] <- map_df(data[tar], function(x) factor(x, levels = levs))

  data$q19_1_1[data$q19_1_1 == 0] <- NA
  data$q19_1_2[data$q19_1_2 == 0] <- NA
  data$q19_1_3[data$q19_1_3 == 0] <- NA
  data$q19_1_4[data$q19_1_4 == 0] <- NA
  data$q19_1_5[data$q19_1_5 == 0] <- NA

  data$q19_2_1[data$q19_2_1 == 0] <- NA
  data$q19_2_2[data$q19_2_2 == 0] <- NA
  data$q19_2_3[data$q19_2_3 == 0] <- NA
  data$q19_2_4[data$q19_2_4 == 0] <- NA
  data$q19_2_5[data$q19_2_5 == 0] <- NA

  tar <- names(select(data, starts_with("q19_3")))
  data[tar] <- map_df(data[tar], function(x)as.character(x))
  data$q19_3_1[data$q19_3_1 == 0] <- NA
  data$q19_3_2[data$q19_3_2 == 0] <- NA
  data$q19_3_3[data$q19_3_3 == 0] <- NA
  data$q19_3_4[data$q19_3_4 == 0] <- NA
  data$q19_3_5[data$q19_3_5 == 0] <- NA

  data$q19_4_1[data$q19_4_1 == 0] <- NA
  data$q19_4_2[data$q19_4_2 == 0] <- NA
  data$q19_4_3[data$q19_4_3 == 0] <- NA
  data$q19_4_4[data$q19_4_4 == 0] <- NA
  data$q19_4_5[data$q19_4_5 == 0] <- NA

  data$q19_5_1[data$q19_5_1 == 0] <- NA
  data$q19_5_2[data$q19_5_2 == 0] <- NA
  data$q19_5_3[data$q19_5_3 == 0] <- NA
  data$q19_5_4[data$q19_5_4 == 0] <- NA
  data$q19_5_5[data$q19_5_5 == 0] <- NA

  data$q20_1_1[data$q20_1_1 == 0] <- NA
  data$q20_1_2[data$q20_1_2 == 0] <- NA
  data$q20_1_3[data$q20_1_3 == 0] <- NA
  data$q20_1_4[data$q20_1_4 == 0] <- NA
  data$q20_1_5[data$q20_1_5 == 0] <- NA

  data$q20_2_1[data$q20_2_1 == 0] <- NA
  data$q20_2_2[data$q20_2_2 == 0] <- NA
  data$q20_2_3[data$q20_2_3 == 0] <- NA
  data$q20_2_4[data$q20_2_4 == 0] <- NA
  data$q20_2_5[data$q20_2_5 == 0] <- NA

  data$q20_3_1[data$q20_3_1 == 0] <- NA
  data$q20_3_2[data$q20_3_2 == 0] <- NA
  data$q20_3_3[data$q20_3_3 == 0] <- NA
  data$q20_3_4[data$q20_3_4 == 0] <- NA
  data$q20_3_5[data$q20_3_5 == 0] <- NA

  data$q20_4_1[data$q20_4_1 == 0] <- NA
  data$q20_4_2[data$q20_4_2 == 0] <- NA
  data$q20_4_3[data$q20_4_3 == 0] <- NA
  data$q20_4_4[data$q20_4_4 == 0] <- NA
  data$q20_4_5[data$q20_4_5 == 0] <- NA

  data$q20_5_1[data$q20_5_1 == 0] <- NA
  data$q20_5_2[data$q20_5_2 == 0] <- NA
  data$q20_5_3[data$q20_5_3 == 0] <- NA
  data$q20_5_4[data$q20_5_4 == 0] <- NA
  data$q20_5_5[data$q20_5_5 == 0] <- NA

  data$q20_6_1[data$q20_6_1 == 0] <- NA
  data$q20_6_2[data$q20_6_2 == 0] <- NA
  data$q20_6_3[data$q20_6_3 == 0] <- NA
  data$q20_6_4[data$q20_6_4 == 0] <- NA
  data$q20_6_5[data$q20_6_5 == 0] <- NA

  tar <- names(select(data, starts_with("q21_")))
  levs <- c(
    "Very important", "Important", "Neutral", "Not important",
    "Not important at all"
    )

  map(data[tar], function(x) table(is.na(x)))
  data[tar] <- map_dfr(data[tar], factor, levels = levs)


# Labels -----------------------------------------------------------------------

  meta_data <- read_csv("data/meta_data.csv") |>
    mutate_if(is.character, utf8::utf8_encode)

  for(i in 1:length(original_vars)){
    attr(data[[original_vars[i]]], "label") <-
      meta_data$labels[meta_data$var_name == original_vars[i]]
    }

# Save -------------------------------------------------------------------------

  save(data, meta_data, file = "data/data.RData")

  print("All done! :P")


