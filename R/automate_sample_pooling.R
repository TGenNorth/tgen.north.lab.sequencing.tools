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

  # Validate input
  cat(validate_automate_sample_pooling_input(Quant, Date_Project, Max_Volume, Min_Volume))

  # Create Folder to save to
  dir.create(paste("./", Date_Project, sep = ""))
  dir.create(paste("./", Date_Project, "/Pools_Map/", sep = "")) #Saves some pngs to file

  # Document session info
  session_info <- capture.output(sessionInfo())
  writeLines(session_info, paste("./", Date_Project, "/", Date_Project, "_SessionInfo.txt", sep = ""))

  # Create pools
  Out <- create_pools(Quant, Max_Volume, Min_Volume)

  #Save pool information
  write.csv(Out, paste("./", Date_Project,"/", Date_Project, "_Pooling_Output.csv", sep = ""))

  #Generate theoretical
  Theoretical_Pools <- Out %>%
    group_by(Pool) %>%
    summarise(n_samples = n(), Final_Volume = sum(Vol_Pool), Theoretical_Conc = sum(Vol_Pool * Quantification) / sum(Vol_Pool))

  #Cat warning if there are low concentrations samples.
  if (min(Theoretical_Pools$Theoretical_Conc) < 4) {
    cat("WARNING: One pool has a theoretical concentration <4 (nMol?), this may impact sequencing performance of samples. Check sample concentrations and consider grouping poor performing samples.\n")
  }

  #Save theoretical pool information
  write.csv(Theoretical_Pools, paste("./", Date_Project,"/", Date_Project, "_Theoretical_Output.csv", sep = ""))

  #Generate plots
  generate_plate_maps(Out, Date_Project)

  cat("Starting to create Epmo Input file...\n")

  #Generate Epmo Output
  Epmo <- create_epmo_input(Out, Theoretical_Pools, Date_Project)

  # Write the final Epmo dataframe to a CSV file without row names.
  write.csv(Epmo, paste("./", Date_Project,"/", Date_Project, "_Epmo_Input.csv", sep = ""), row.names = FALSE)

  # Write file that says loding positions
  Load_Positions <- Epmo %>%
    group_by(`Plate (DELETE ME!)`, Rack_source) %>%
    tally()

  #Save loading positions
  write.csv(Load_Positions, paste("./", Date_Project,"/", Date_Project, "_Epmo_Loading_Positions.csv", sep = ""), row.names = FALSE)

  cat("Starting to create Integra Input file...\n")
  #Generate Epmo Output
  Integra <- create_integra_input(Out, Theoretical_Pools, Date_Project)

  # Write the final Epmo dataframe to a CSV file without row names.
  write.csv(Integra, paste("./", Date_Project,"/", Date_Project, "_Integra_Input.csv", sep = ""), row.names = FALSE)
  cat("RUN COMPLETE, PLEASE DOWNLOAD THE DATA FILE...\n")
}
#
# # ## Testing the function
# library(tgen.north.lab.sequencing.tools)
# library(tidyverse)
# Max_Volume = 8 ###### Edit me max volume to use
# Min_Volume = 1 ###### Edit me min volume to use
# Date_Project = "Test_Function_Integra"
# Quant <- read.csv("~/Dropbox/TGen Projects/20220318_Pooling_Script/Test_Dataset.csv") ###### Edit me!!!!!! Data to read in
# # Quant <- filter(Quant, Plate == "Plate 1")
# # validate_automate_sample_pooling_input(Quant, Date_Project, Max_Volume, Min_Volume)
# # automate_sample_pooling(Quant = Quant, Max_Volume = Max_Volume, Min_Volume = Min_Volume, Date_Project = Date_Project)
# Out <- tgen.north.lab.sequencing.tools:::create_pools(Quant, Max_Volume, Min_Volume)
# Epmo <- tgen.north.lab.sequencing.tools:::create_epmo_input(create_pools_df = Out, Theoretical_Pools = Theoretical_Pools, Date_Project)
