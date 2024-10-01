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
    Destination <- data.frame(Destination = "C", Pool = 1:48, Pool_well =c("A01","A02","A03","A04","A05","A06",
           "B01","B02","B03","B04","B05","B06",
           "C01","C02","C03","C04","C05","C06",
           "D01","D02","D03","D04","D05","D06",
           "E01","E02","E03","E04","E05","E06",
           "F01","F02","F03","F04","F05","F06",
           "G01","G02","G03","G04","G05","G06",
           "H01","H02","H03","H04","H05","H06"))

    # Create a dataframe 'Source' for the source plate labels and assign sequential 'Rack' numbers.
    Source <- data.frame(Source_Label = unique(create_pools_df$Plate))
    Source$Rack <- "B"

    # Merge 'create_pools_df' with 'Source' based on matching Plate and Source_Label values.
    create_pools_df <- create_pools_df %>%
      left_join(Source, by = c("Plate" = "Source_Label")) %>%
      left_join(Destination, by = "Pool")

    # Select the relevant columns for the integra dataframe for robot instruction.
    Integra <- create_pools_df %>%
      mutate("Integra" = paste(Sample.Name,Rack, Position, Destination, Pool_well, Vol_Pool, sep = ";")) %>%
      select("Integra")

    names(Integra) <- c("SampleID;SourceDeckPosition;SourceWell;TargetDeckPosition;TargetWell;TransferVolume [µl]")

    return(Integra)

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
# Quant <- filter(Quant, Plate == "Plate 1")
# Out <- tgen.north.lab.sequencing.tools:::create_pools(Quant = Quant, Max_Volume = Max_Volume, Min_Volume = Min_Volume)
# Theoretical_Pools <- Out %>%
#   group_by(Pool) %>%
#   summarise(n_samples = n(), Final_Volume = sum(Vol_Pool), Theoretical_Conc = sum(Vol_Pool * Quantification) / sum(Vol_Pool))
# create_integra_input(Out, Theoretical_Pools, Date_Project)
