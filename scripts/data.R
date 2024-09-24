
  library(readxl)
  library(tidyverse)
  library(janitor)
  library(testthat)

# Pharmacy ---------------------------------------------------------------------

## Data ------------------------------------------------------------------------

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
      )

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

# Labels -----------------------------------------------------------------------

  meta_data <- read_csv("meta_data.csv")
  var_labs <- meta_data$labels # Update to new_label if needed
  for(i in 1:length(original_vars)){
    attr(data[[original_vars[i]]], "label") <- var_labs[i]
    }

# GP ---------------------------------------------------------------------------


# Save -------------------------------------------------------------------------

  save(data, meta_data, file = "data/data.RData")

  print("All done! :P")


