#' Generate Pooling Plate Maps
#'
#' @description This function creates and saves pooling plots for each plate. It generates a general map for each plate and pool/plate-specific maps.
#' @param create_pools_df A data frame from the function create_pools() specifying pool and pooling information,
#' @param Date_Project A string representing the name of the project (used to name the output directory and files).
#' @import ggplot2
#' @import dplyr
#' @return No return value; this function saves plot files as PNG images.
#'
generate_plate_maps <- function(create_pools_df, Date_Project) {

  # Create a mock plate to represent all possible well positions (A01 to H12).
  MockPlate = data.frame(c("A01","A02","A03","A04","A05","A06","A07","A08","A09","A10","A11","A12",
                           "B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12",
                           "C01","C02","C03","C04","C05","C06","C07","C08","C09","C10","C11","C12",
                           "D01","D02","D03","D04","D05","D06","D07","D08","D09","D10","D11","D12",
                           "E01","E02","E03","E04","E05","E06","E07","E08","E09","E10","E11","E12",
                           "F01","F02","F03","F04","F05","F06","F07","F08","F09","F10","F11","F12",
                           "G01","G02","G03","G04","G05","G06","G07","G08","G09","G10","G11","G12",
                           "H01","H02","H03","H04","H05","H06","H07","H08","H09","H010","H11","H12"))

  # Assign the column name "Position" to this mock plate.
  names(MockPlate) <- c("Position")

  # Add 'Row' and 'Column' to MockPlate for plotting purposes.
  MockPlate <- mutate(MockPlate,
                      Row=as.numeric(match(toupper(substr(Position, 1, 1)), LETTERS)),
                      Column=as.numeric(substr(Position, 2, 5)))

  # Similarly, add 'Row' and 'Column' to the actual plate map for positioning in plots.
  PlateMap <- mutate(create_pools_df,
                     Row=as.numeric(match(toupper(substr(Position, 1, 1)), LETTERS)),
                     Column=as.numeric(substr(Position, 2, 5)))

  # Convert the 'Pool' column in PlateMap to a factor, which will be used for coloring.
  PlateMap$Pool <- as.factor(PlateMap$Pool)

  # Loop through each unique plate in the PlateMap dataset.
  for (j in unique(PlateMap$Plate)) {

    # Subset the PlateMap for the current plate.
    Temp = filter(PlateMap, Plate == j)

    # Define the save path for the general map of the current plate.
    savepath <- paste("./", Date_Project, "/", Date_Project, "_General_Map_", j, ".png", sep = "")

    # Create the general plate map using ggplot2.
    ggplot(data=Temp, aes(x=Column, y=Row)) +
      # Add a background grid of grey circles representing empty wells.
      geom_point(data=expand.grid(seq(1, 12), seq(8, 1)), aes(x=Var1, y=Var2), color="grey90", shape=21) +
      coord_fixed(ratio=(13/12)/(9/8), xlim=c(-0.5, 13.5), ylim=c(9.5, .5)) + # Fix aspect ratio to match a 96-well plate
      scale_y_continuous(breaks=seq(1,8), labels=LETTERS[1:8]) + # Label the rows with letters
      scale_x_continuous(breaks=seq(1, 12)) + # Label the columns with numbers
      geom_point(aes(colour = Pool), alpha = 0.9, size = 14) + # Plot the pooled samples with colors by 'Pool'
      ylab("") + # Remove y-axis label
      xlab("") + # Remove x-axis label
      theme(legend.position = c(0.5, 1.05), legend.direction="vertical") + # Position the legend above the plot
      geom_text(aes(label=paste(Sample.Name,"\n", Vol_Pool, " μL", sep = "")), fontface = "bold", size = 2.5) + # Add sample name and volume
      guides(color = guide_legend(override.aes = list(size = 3))) + # Adjust legend size
      guides(colour=guide_legend(ncol=1)) + # Set legend to one column
      theme_bw() + # Use a clean white background theme
      ggtitle(savepath) # Set the title to the save path

    # Save the general plate map as a PNG file.
    ggsave(savepath, width = 1.3 * 8.5, height = 1.3 * 5.5)

    # Now, create pool-specific maps for each unique pool in the current plate.
    for (n in unique(Temp$Pool)) {

      # Subset the data for the current pool within the current plate.
      Temp_Plot <- filter(Temp, Pool == n)

      # Join the current pool data with the mock plate to ensure all positions are represented, even if empty.
      Temp_Plot <- full_join(Temp_Plot, MockPlate, by = c("Row", "Column"))

      # Define the save path for the pool-specific map.
      savepath <- paste("./", Date_Project, "/", Date_Project, "_Pool_Map_", j, "_Pool", n, ".png", sep = "")

      # Create the pool-specific map using ggplot2.
      ggplot(data=Temp_Plot, aes(x=Column, y=Row)) +
        geom_point(data=expand.grid(seq(1, 12), seq(8, 1)), aes(x=Var1, y=Var2), color="grey90", shape=21) + # Background grid
        coord_fixed(ratio=(13/12)/(9/8), xlim=c(-0.5, 13.5), ylim=c(9.5, .5)) + # Fix aspect ratio
        scale_y_continuous(breaks=seq(1, 8), labels=LETTERS[1:8]) + # Row labels
        scale_x_continuous(breaks=seq(1, 12)) + # Column labels
        geom_point(aes(colour = Pool), alpha = 0.9, size = 14) + # Plot the pooled samples
        ylab("") + # Remove y-axis label
        xlab("") + # Remove x-axis label
        theme(legend.position = c(0.5, 1.05), legend.direction="vertical") + # Position legend
        geom_text(aes(label= paste(Position.x,"\n", Vol_Pool, " μL", sep = "")), fontface = "bold", size = 2.5) + # Add labels
        guides(color = guide_legend(override.aes = list(size = 3))) + # Adjust legend size
        guides(colour=guide_legend(ncol=1)) + # Set legend to one column
        theme_bw() + # Use clean white background
        theme(legend.position = "none") + # Remove legend
        scale_color_manual(values=c("indianred2", "white", "white", "cyan2")) + # Set manual colors
        ggtitle(savepath) # Set the title to the save path

      # Save the pool-specific map as a PNG file.
      ggsave(savepath, width = 0.9 * 10, height = 0.9 * 6.5)
    }
  }
}

# ## Testing the function
# library(tidyverse)
# Max_Volume = 10 ###### Edit me max volume to use
# Min_Volume = 2 ###### Edit me min volume to use
# Date_Project = "Test_Function"
# Quant <- read.csv("~/Dropbox/TGen Projects/20220318_Pooling_Script/Test_Dataset.csv") ###### Edit me!!!!!! Data to read in
# Out <- create_pools(Quant = Quant, Max_Volume = 10, Min_Volume = 2)
# generate_plate_maps(create_pools_df = Out, Date_Project = Date_Project)
