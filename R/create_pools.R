#' Create Pools
#'
#' @description This function takes a data frame of sample quantification data and pools the samples based on their quantification values. It ensures that pool volumes are between the specified minimum and maximum values.
#' @param Quant A data frame with columns "Sample Name", "Quantification", "Position", "Plate", and "Group". "Group" is optional and allows separate pooling within each group.
#' @param Max_Volume The maximum volume allowed for a sample in a pool (default = 8 μL).
#' @param Min_Volume The minimum volume allowed for a sample in a pool (default = 2 μL).
#' @return A data frame containing the pooled samples, including pool volumes and pool numbers.
#' @import dplyr
#' @import tidyr

create_pools <- function(Quant, Max_Volume = 8, Min_Volume = 2) {
  Pool_n = 0 # Initialize pool counter
  Out <- data.frame() # Create an empty data frame to store output

  # Ensure each sample belongs to a group. If 'Group' is missing, assign all samples to Group "1".
  if (is.null(Quant$Group)) {
    Quant$Group <- "1"
  } else {
    Quant$Group <- as.character(Quant$Group) # Convert Group to character to handle it consistently
  }

  # Loop over each unique group in the data and create pools
  for (GROUP in unique(Quant$Group)) {
    Group_Quant <- Quant %>%
      filter(Group == GROUP) # Select data for the current group

    # Determine the minimum quantification value in this group
    Min_Quant <- min(Group_Quant$Quantification)

    # Increment pool number for each new group
    Pool_n <- Pool_n + 1

    # Print the current pool number
    cat("Updating pool number to: ", Pool_n, "\n")

    # Calculate pool volumes as a fraction of the Min_Quant, with the Max_Volume as the reference
    Group_Quant <- Group_Quant %>%
      mutate(Vol_Pool = (Min_Quant / Quantification) * Max_Volume, Pool = Pool_n)

    # Get the smallest volume in the pool
    Min_Pool <- min(Group_Quant$Vol_Pool)

    # If any volumes are smaller than Min_Volume, we need to reassign those samples to a new pool
    while (Min_Pool < Min_Volume) {
      # Select samples that have a volume greater than the minimum allowed
      Temp <- Group_Quant %>%
        filter(Vol_Pool > Min_Volume)

      # Add these samples to the output dataframe
      Out <- rbind(Out, Temp)

      # Remove the samples that have been processed from the group
      Group_Quant <- Group_Quant %>%
        filter(!Vol_Pool > Min_Volume)

      # Update the minimum quantification for the remaining samples
      Min_Quant <- min(Group_Quant$Quantification)

      # Increment pool number for the new pool
      Pool_n <- Pool_n + 1

      # Print the current pool number
      cat("Updating pool number to: ", Pool_n, "\n")

      # Recalculate pool volumes for the remaining samples
      Group_Quant <- Group_Quant %>%
        mutate(Vol_Pool = (Min_Quant / Quantification) * Max_Volume, Pool = Pool_n)

      # Get the smallest volume in the new pool
      Min_Pool <- min(Group_Quant$Vol_Pool)
    }

    # Once all samples in the group are processed, add them to the output
    Out <- rbind(Out, Group_Quant)
  }

  # Round the final pool volumes to two decimal places for clarity
  Out$Vol_Pool <- round(Out$Vol_Pool, 2)

  # Get pooled concentrations
  Out <- Out %>%
    mutate(Pooled_Conc = round(Vol_Pool*Quantification,2))

  # Return the dataframe containing pooled samples
  return(Out)
}

## Testing the function
# library(tidyverse)
# Max_Volume = 10 ###### Edit me max volume to use
# Min_Volume = 2 ###### Edit me min volume to use
# Quant <- read.csv("~/Dropbox/TGen Projects/20220318_Pooling_Script/Test_Dataset.csv") ###### Edit me!!!!!! Data to read in
# create_pools(Quant = Quant, Max_Volume = 10, Min_Volume = 2)

