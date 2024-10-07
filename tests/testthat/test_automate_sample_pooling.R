library(testthat)
library(tgen.north.lab.sequencing.tools)

# Load the test dataset
load("./testdata/Example_Quant_Data.rda")  # Ensure this contains the Example_Quant_Data.rda

# Test suite for automate_sample_pooling function
test_that("automate_sample_pooling works correctly with Example_Quant_Data.rda", {
  # Ensure Quant is available from the loaded dataset
  expect_true(exists("Example_Quant_Data"), "Quant data frame should be loaded from the RDA file.")

  # Set parameters for the test
  Quant <- Example_Quant_Data
  Date_Project <- "Test_Project"
  Max_Volume <- 8
  Min_Volume <- 2

  # Clean up any existing project directory
  if (dir.exists(Date_Project)) {
    unlink(Date_Project, recursive = TRUE)
  }

  # Run the function
  automate_sample_pooling(Quant, Date_Project, Max_Volume, Min_Volume)

  # Check if output directories were created
  expect_true(dir.exists(Date_Project), "Output directory should be created.")

  # Check if output files were created
  expect_true(file.exists(file.path(Date_Project, paste(Date_Project, "_Pooling_Output.csv", sep = ""))), "Pooling_Output.csv should be created.")
  expect_true(file.exists(file.path(Date_Project, paste(Date_Project, "_Theoretical_Output.csv", sep = ""))), "Theoretical_Output.csv should be created.")
  expect_true(file.exists(file.path(Date_Project, paste(Date_Project, "_Epmo_Input.csv", sep = ""))), "Epmo_Input.csv should be created.")
  expect_true(file.exists(file.path(Date_Project, paste(Date_Project, "_Integra_Input.csv", sep = ""))), "Integra_Input.csv should be created.")

  # Load the created outputs to check their content
  pool_output <- read.csv(file.path(Date_Project, paste(Date_Project, "_Pooling_Output.csv", sep = "")))
  theoretical_output <- read.csv(file.path(Date_Project, paste(Date_Project, "_Theoretical_Output.csv", sep = "")))
  epmo_input <- read.csv(file.path(Date_Project, paste(Date_Project, "_Epmo_Input.csv", sep = "")))
  integra_input <- read.csv(file.path(Date_Project, paste(Date_Project, "_Integra_Input.csv", sep = "")))

  # Check if the generated pool output is not empty
  expect_gt(nrow(pool_output), 0, "Pooling output should have at least one row.")

  # Check if the theoretical output has the correct structure
  expect_equal(nrow(epmo_input), 192) #"Epmo has 192 rows"
  expect_equal(nrow(integra_input), 192) #"Integra has 192 rows"
  expect_equal(nrow(theoretical_output), 4)

  # Clean up output files after testing
  unlink(Date_Project, recursive = TRUE)
})
