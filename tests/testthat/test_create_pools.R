# Load necessary libraries
library(testthat)  # For writing and running unit tests
library(dplyr)     # For data manipulation, used in the function and test preparation

# Sample dataset for testing purposes
test_data <- data.frame(
  `Sample Name` = c("Sample1", "Sample2", "Sample3", "Sample4"),  # Sample identifiers
  Quantification = c(5, 10, 15, 20),  # Quantification values for each sample
  Position = c("A1", "B1", "C1", "D1"),  # Sample positions (e.g., in a plate)
  Plate = c("P1", "P1", "P2", "P2"),     # Plate identifiers
  Group = c("G1", "G1", "G2", "G2")      # Groups for pooling samples
)

# Test case 1: Ensure the function returns a data frame
test_that("create_pools returns a data frame", {
  result <- create_pools(Quant = test_data)  # Run the function
  expect_s3_class(result, "data.frame")  # Check if the output is a data frame
})

# Test case 2: Ensure pool volumes are within the specified Min_Volume and Max_Volume range
test_that("create_pools ensures pool volumes are within the specified range", {
  result <- create_pools(Quant = test_data, Max_Volume = 8, Min_Volume = 2)  # Run with specified volume limits
  expect_true(all(result$Vol_Pool <= 8))  # Verify that no pool exceeds Max_Volume
  expect_true(all(result$Vol_Pool >= 2))  # Verify that no pool is below Min_Volume
})

# Test case 3: Ensure the function handles missing Group column properly
test_that("create_pools handles missing Group column", {
  test_data_no_group <- test_data %>% select(-Group)  # Remove 'Group' column
  result <- create_pools(Quant = test_data_no_group)  # Run the function
  expect_true(all(result$Group == "1"))  # Check that the Group column is assigned to "1"
})

# Test case 4: Verify pooled concentration calculation
test_that("create_pools recalculates pooled concentration correctly", {
  result <- create_pools(Quant = test_data, Max_Volume = 8, Min_Volume = 2)  # Run the function
  expect_true(all(result$Pooled_Conc == round(result$Vol_Pool * result$Quantification, 2)))  # Verify concentration
})

# Test case 5: Handle edge cases where there's only one sample in a group
test_that("create_pools handles edge cases with one sample per group", {
  single_sample_data <- data.frame(
    `Sample Name` = "Sample1",  # Single sample
    Quantification = 5,  # Quantification value
    Position = "A1",  # Position
    Plate = "P1",  # Plate
    Group = "G1"  # Group
  )
  result <- create_pools(Quant = single_sample_data, Max_Volume = 8, Min_Volume = 2)  # Run the function
  expect_equal(nrow(result), 1)  # Ensure only one row is returned
  expect_equal(result$Vol_Pool, 8)  # Check that volume is set correctly (as max volume)
})

# Test case 6: Ensure pool numbers are assigned uniquely across groups
test_that("create_pools assigns unique pool numbers", {
  result <- create_pools(Quant = test_data)  # Run the function
  expect_equal(length(unique(result$Pool)), length(unique(test_data$Group)))  # Pool numbers should match group count
})

# Test case 7: Check end to end performance on complex df
test_that("Check output unsing Example_Test_Dataset_2.rda for standard use", {
  load("./testdata/Example_Test_Dataset_2.rda")
  result <- create_pools(Quant = Test_Dataset_2)  # Run the function
  expect_equal(length(unique(result$Pool)), 10)  # Output should have 10 pools
  expect_equal(sum(result$Vol_Pool), 2682.47) # 2682.47ul should be pooled
})

# Test case 8: Check end to end performance on complex df with altered parameters creating less pools
test_that("Check output using Example_Test_Dataset_2.rda for a few pools", {
  load("./testdata/Example_Test_Dataset_2.rda")
  result <- create_pools(Quant = Test_Dataset_2, Max_Volume = 25, Min_Volume = 1)  # Run the function
  expect_equal(length(unique(result$Pool)), 6)  # Output should have 10 pools
  expect_equal(sum(result$Vol_Pool), 5846.02) # 2682.47ul should be pooled
})

# Test case 9: Check end to end performance on complex df with altered parameters creating more pools
test_that("Check output using Example_Test_Dataset_2.rda for many pools", {
  load("./testdata/Example_Test_Dataset_2.rda")
  result <- create_pools(Quant = Test_Dataset_2, Max_Volume = 25, Min_Volume = 20)  # Run the function
  expect_equal(length(unique(result$Pool)), 36)  # Output should have 10 pools
  expect_equal(sum(result$Vol_Pool), 13021.2) # 2682.47ul should be pooled
})
