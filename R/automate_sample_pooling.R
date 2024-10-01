#' Automate Sample Pooling
#'
#' @description Automates sample pooling, creates plots, and generates Epmo robot output. This function wraps around the internal pool creation, plotting, and robot output functions.
#' @param Quant A data frame with columns: Sample Name, Quantification, Position, Plate, Group (optional).
#' @param Date_Project A string representing the name of the project (used for output directories and file names).
#' @param Max_Volume The maximum volume allowed for each sample in the pool (default = 8 μL).
#' @param Min_Volume The minimum volume allowed for each sample in the pool (default = 2 μL).
#' @export  automate_sample_pooling
#' @import tidyr
#' @import tidyverse
#' @import ggplot2
#' @import dplyr

automate_sample_pooling <- function(Quant, Date_Project, Max_Volume = 8, Min_Volume = 2) {

  # Create Folder to save to
  dir.create(paste("./", Date_Project, sep = ""))

  # Create pools
  Out <- create_pools(Quant, Max_Volume, Min_Volume)

  #Save pool information
  write.csv(Out, paste("./", Date_Project,"/", Date_Project, "_Pooling_Output.csv", sep = ""))

  #Generate theoretical
  Theoretical_Pools <- Out %>%
    group_by(Pool) %>%
    summarise(n_samples = n(), Final_Volume = sum(Vol_Pool), Theoretical_Conc = sum(Vol_Pool * Quantification) / sum(Vol_Pool))

  #Save theoretical pool information
  write.csv(Theoretical_Pools, paste("./", Date_Project,"/", Date_Project, "_Theoretical_Output.csv", sep = ""))

  #Generate plots
  generate_plate_maps(Out, Date_Project)

  cat("Starting to create Epmo Input file...")

  #Generate Epmo Output
  Epmo <- create_epmo_input(Out, Theoretical_Pools, Date_Project)

  # Write the final Epmo dataframe to a CSV file without row names.
  write.csv(Epmo, paste("./", Date_Project,"/", Date_Project, "_Epmo_Input.csv", sep = ""), row.names = FALSE)

  cat("Starting to create Integra Input file...")
  #Generate Epmo Output
  Integra <- create_integra_input(Out, Theoretical_Pools, Date_Project)

  # Write the final Epmo dataframe to a CSV file without row names.
  write.csv(Integra, paste("./", Date_Project,"/", Date_Project, "_Integra_Input.csv", sep = ""), row.names = FALSE)
}

## Testing the function
# library(tidyverse)
# Max_Volume = 15 ###### Edit me max volume to use
# Min_Volume = 10 ###### Edit me min volume to use
# Date_Project = "Test_Function_Integra"
# Quant <- read.csv("~/Dropbox/TGen Projects/20220318_Pooling_Script/Test_Dataset.csv") ###### Edit me!!!!!! Data to read in
# Quant <- filter(Quant, Plate == "Plate 1")
# automate_sample_pooling(Quant = Quant, Max_Volume = Max_Volume, Min_Volume = Min_Volume, Date_Project = Date_Project)
