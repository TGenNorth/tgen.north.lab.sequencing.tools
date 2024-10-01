#' Create Robot Input for Integra
#'
#' @description Generates the CSV file required by the Integra robot for automated sample pooling.
#' @param create_pools_df A data frame from the function create_pools() specifying pool and pooling information,
#' @param Theoretical_Pools A data frame summarizing the final volume and theoretical concentration of each pool.
#' @param Date_Project A string representing the name of the project.
#' @import dplyr
#' @return No return value; this function saves a dataframe file for Integra.

create_integra_input <- function(create_pools_df, Theoretical_Pools, Date_Project) {

  # Check for conditions:
  # - Less than X unique plates
  # - Less than X pools
  # - The maximum volume in Theoretical_Pools is less than 1500 μL
  # - The maximum volume per sample in create_pools_df is less than or equal to 10 μL
  if (length(unique(create_pools_df$Plate)) < 2 &&
      length(unique(create_pools_df$Pool)) < 48 &&
      max(Theoretical_Pools$Final_Volume) < 2000 &&
      max(create_pools_df$Vol_Pool) <= 300) {

    # Create a dataframe 'Destination' with 24 rows for 24 destination wells, with Pool values from 1 to 24.
    Destination <- data.frame(Destination = "C", Pool = 1:48)

    # Create a dataframe 'Source' for the source plate labels and assign sequential 'Rack' numbers.
    Source <- data.frame(Source_Label = unique(create_pools_df$Plate))
    Source$Rack <- "B"

    # Merge 'create_pools_df' with 'Source' based on matching Plate and Source_Label values.
    create_pools_df <- create_pools_df %>%
      left_join(Source, by = c("Plate" = "Source_Label")) %>%
      left_join(Destination, by = "Pool") %>%
      # Add new columns for Rack_dest (destination rack number) and Tool (e.g., pipetting tool).
      mutate(Rack_dest = "C")

    # Define the file path for saving the EPMO output CSV file.
    savepath <- paste0("./", Date_Project, "/", Date_Project, "_Robot_Output.csv")

    # Select the relevant columns for the Epmo dataframe for robot instruction.
    Epmo <- create_pools_df %>%
      select(Plate, Rack, Position, Rack_dest, Destination, Vol_Pool, Tool)

    # Rename the columns to match the format expected by the robot:
    # Plate = Source Plate, Rack = Source Rack, Position = Source Well, Rack_dest = Destination Rack,
    # Destination = Destination Well, Vol_Pool = Volume, Tool = Pipetting Tool
    names(Epmo) <- c("Plate", "Rack", "Source", "Rack_dest", "Destination", "Volume", "Tool")

    return(Epmo)

  } else {
    # If the conditions are not met, stop the function and display an error message.
    stop("Error in Integra program. Ensure less than 2 plates, less than 48 pools, max pool size <2000 μL, and max volume <300 μL per sample!")
  }
}

## Testing the function
# library(tidyverse)
# Max_Volume = 30 ###### Edit me max volume to use
# Min_Volume = 2 ###### Edit me min volume to use
# Date_Project = "Test_Function"
# Quant <- read.csv("~/Dropbox/TGen Projects/20220318_Pooling_Script/Test_Dataset.csv") ###### Edit me!!!!!! Data to read in
#
# Quant <- filter(Quant, Plate == "Plate 1")
#
# Out <- create_pools(Quant = Quant, Max_Volume = Max_Volume, Min_Volume = Min_Volume)
# Theoretical_Pools <- Out %>%
#   group_by(Pool) %>%
#   summarise(n_samples = n(), Final_Volume = sum(Vol_Pool), Theoretical_Conc = sum(Vol_Pool * Quantification) / sum(Vol_Pool))
#
# create_pools_df <- Out
#
# create_integra_input(Out, Theoretical_Pools, Date_Project)
