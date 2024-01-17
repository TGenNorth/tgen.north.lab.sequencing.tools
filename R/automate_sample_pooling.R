#' Automate Sample Pooling
#'
#' @description This function is meant to automate sample pooling for sequencing. The process will produce several files including pooling maps, theoretical concentrations, and a script that can be added to the Epmo for completely automated pooling.
#' @param Quant A csv file with four columns named "Sample Name", "Quantification" "Position", and "Plate". Sample name can be anything. Quantification should be sample quantification value from quant. Position should be the position within the 96 well plate (A1-H12). Plate should identify which plate the sample is on ("Plate 1"-"Plate N").
#' @param Date_Project A character string to specify the name of the pooling run. This will create a new directory with that name and name the files accordingly.
#' @param Max_Volume A number specifying the max volume of a sample to be included in a pool (Default = 8ul).
#' @param Min_Volume A number specifying the minimim volume of a sample to be included in the pool (Default = 2ul).
#' @export automate_sample_pooling
#' @import tidyr
#' @import tidyverse
#' @import dplyr
#' @import ggplot2
#' @examples
#' # basic usage of automate_sample_pooling
#' #For example Usage use:
#' #For real usage use: Quant_DF <- read.csv(PATH TO CSV FILE)
#' Quant_DF <- tgen.north.lab.sequencing.tools::Example_Quant_Data
#' automate_sample_pooling(Quant = Quant_DF, Date_Project = "20240117_Testing", Max_Volume = 10, Min_Volume = 1.6)


automate_sample_pooling <- function(Quant, Date_Project, Max_Volume = 8, Min_Volume = 2){

  # Find minimum quantification value
  Min_Quant = min(Quant$Quantification)

  #Calculate Vol to pool
  Quant <- Quant %>%
    mutate(Vol_Pool = (Min_Quant / Quantification)*Max_Volume,
           Pool = 1)

  #Find minimum pool value
  Min_Pool <- min(Quant$Vol_Pool)

  #Create out Quant
  Out  <- data.frame()

  #While Min_pool is less than min_volume
  while (Min_Pool < Min_Volume ) {


    Temp <- Quant %>%
      filter(Vol_Pool > Min_Volume )

    Out <- rbind(Out, Temp)

    Quant <- Quant %>%
      filter(!Vol_Pool > Min_Volume )

    Min_Quant = min(Quant$Quantification)

    Quant <- Quant %>%
      mutate(Vol_Pool = (Min_Quant / Quantification)*Max_Volume,
             Pool = Pool + 1)

    Min_Pool = min(Quant$Vol_Pool)

  }

  #Join Out and Quant
  Out <- rbind(Out, Quant)

  Out$Max_Available <- NULL
  Out$Min_Available <- NULL

  Out$Vol_Pool <- round(Out$Vol_Pool, 2)

  Out <- Out %>%
    mutate(Pooled_Conc = Vol_Pool*Quantification )

  Theoretical_Pools <- Out %>%
    group_by(Pool) %>%
    summarise(n_samples = n(),
              Final_Volume = sum(Vol_Pool),
              Theoretical_Conc = (sum(Pooled_Conc)/sum(Vol_Pool)))

  Out$Vol_Pool <- round(Out$Vol_Pool, 2)


  #Create Mock Plate for plotting NA values
  MockPlate = data.frame(c("A01","A02","A03","A04","A05","A06","A07","A08","A09","A10","A11","A12","B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12","C01","C02","C03","C04","C05","C06","C07","C08","C09","C10","C11","C12","D01","D02","D03","D04","D05","D06","D07","D08","D09","D10","D11","D12","E01","E02","E03","E04","E05","E06","E07","E08","E09","E10","E11","E12","F01","F02","F03","F04","F05","F06","F07","F08","F09","F10","F11","F12","G01","G02","G03","G04","G05","G06","G07","G08","G09","G10","G11","G12","H01","H02","H03","H04","H05","H06","H07","H08","H09","H010","H11","H12"))

  names(MockPlate) <- c("Position")

  MockPlate <- mutate(MockPlate,
                      Row=as.numeric(match(toupper(substr(Position, 1, 1)), LETTERS)),
                      Column=as.numeric(substr(Position, 2, 5)))



  # Write Excel Output


  dir.create(paste("./", Date_Project, sep = ""))

  write.csv(Out, paste("./", Date_Project,"/", Date_Project, "_Pooling_Output.csv", sep = ""))

  write.csv(Theoretical_Pools, paste("./", Date_Project,"/", Date_Project, "_Theoretical_Output.csv", sep = ""))

  PlateMap <- mutate(Out,
                     Row=as.numeric(match(toupper(substr(Position, 1, 1)), LETTERS)),
                     Column=as.numeric(substr(Position, 2, 5)))

  PlateMap$Pool <- as.factor(PlateMap$Pool)

  for(j in unique(PlateMap$Plate)){

    Temp = filter(PlateMap, Plate == j)

    savepath <- paste("./", Date_Project,"/", Date_Project, "_General_Map_", j, ".png", sep = "")


    ggplot(data=Temp, aes(x=Column, y=Row)) +
      geom_point(data=expand.grid(seq(1, 12), seq(8, 1)), aes(x=Var1, y=Var2),
                 color="grey90", shape=21) +
      coord_fixed(ratio=(13/12)/(9/8), xlim=c(-0.5, 13.5), ylim=c(9.5, .5)) +
      scale_y_continuous(breaks=seq(1,8), labels=LETTERS[1:8]) +
      scale_x_continuous(breaks=seq(1, 12)) +
      geom_point(aes(colour =Pool), alpha = 0.9, size = 14)+
      ylab("")+
      xlab("")+
      theme(legend.position = c(0.5, 1.05), legend.direction="vertical")+
      geom_text(
        aes(label=paste(Sample.Name,"\n", Vol_Pool, " μL", sep = "")), fontface = "bold", size = 2.5)+
      guides(color = guide_legend(override.aes = list(size = 3)))+
      guides(colour=guide_legend(ncol=1))+
      theme_bw()+
      ggtitle(savepath)

    ggsave(savepath,  width = 1.3*8.5, height=1.3*5.5)


    for(n in unique(Temp$Pool)){

      Temp_Plot <- filter(Temp, Pool == n)

      Temp_Plot <- full_join(Temp_Plot, MockPlate, by = c("Row", "Column"))

      savepath <- paste("./", Date_Project,"/", Date_Project, "_Pool_Map_", j,"_Pool",n, ".png", sep = "")

      ggplot(data=Temp_Plot, aes(x=Column, y=Row)) +
        geom_point(data=expand.grid(seq(1, 12), seq(8, 1)), aes(x=Var1, y=Var2),
                   color="grey90", shape=21) +
        coord_fixed(ratio=(13/12)/(9/8), xlim=c(-0.5, 13.5), ylim=c(9.5, .5)) +
        scale_y_continuous(breaks=seq(1,8), labels=LETTERS[1:8]) +
        scale_x_continuous(breaks=seq(1, 12)) +
        geom_point(aes(colour =Pool), alpha = 0.9, size = 14)+
        ylab("")+
        xlab("")+
        theme(legend.position = c(0.5, 1.05), legend.direction="vertical")+
        geom_text(
          aes(label= paste(Position.x,"\n", Vol_Pool, " μL", sep = "")), fontface = "bold", size = 2.5)+
        guides(color = guide_legend(override.aes = list(size = 3)))+
        guides(colour=guide_legend(ncol=1))+
        theme_bw()+
        theme(legend.position = "none")+
        scale_color_manual(values=c("indianred2", "white", "white", "cyan2"))+
        ggtitle(savepath)

      ggsave(savepath,   width = 0.9*10, height=0.9*6.5)
    }
  }


  if (length(unique(PlateMap$Plate))<4 & length(unique(PlateMap$Pool)) <5){

    Temp = PlateMap

    Destination = data.frame(Destination = c(1:24), Pool = c(1:24))

    Source = data.frame(Source_Label = unique(Temp$Plate))

    Source$Rack <- seq.int(nrow(Source))

    Temp$Pool <- as.numeric(as.character(Temp$Pool))

    Temp = left_join(Temp, Source, by = c("Plate" = "Source_Label"))

    Temp = left_join(Temp, Destination, by = "Pool")

    Temp$Rack.dest <- "1"

    savepath <- paste("./", Date_Project,"/", Date_Project, "_Robot_Output.csv", sep = "")

    Epmo = Temp %>%
      select(Plate,Rack, Position, Rack.dest, Destination, Vol_Pool)

    Epmo$Tool <- "TS_10"

    names(Epmo) <- c("Plate (DELETE ME!)", "Rack", "Source", "Rack", "Destination", "Volume", "Tool")

    write.csv(Epmo, savepath, row.names = F)

  } else { print("Error in Epmo program... Check to make sure you have less than 4 plates and less than 24 pools!")}

}

## Testing the package
# Max_Volume = 10 ###### Edit me max volume to use
# Min_Volume = 1.6 ###### Edit me min volume to use
# Date_Project = "20240117_Testing" ###### Edit me!!!!!!! Directory for stuff to be saved
# Example_Quant_Data <- read.csv("~/Dropbox/TGen Projects/20220318_Pooling_Script/Test_Dataset.csv") ###### Edit me!!!!!! Data to read in
# usethis::use_data(Example_Quant_Data)
#
# ?automate_sample_pooling
#
# automate_sample_pooling(Quant = read.csv("~/Dropbox/TGen Projects/20220318_Pooling_Script/Test_Dataset.csv"), Date_Project = "20240117_Testing", Max_Volume = 10, Min_Volume = 1.6)
