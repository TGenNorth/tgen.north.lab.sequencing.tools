library(testthat)

# Test suite for the validate_automate_sample_pooling_input function
test_that("validate_automate_sample_pooling_input works correctly", {

  # Create a sample data frame with valid inputs
  valid_data <- data.frame(
    Sample.Name = c("Sample1", "Sample2"),
    Quantification = c(10, 20),  # Numeric values
    Position = c("A1", "A2"),
    Plate = c("Plate1", "Plate2")
  )

  # Test 1: Valid inputs
  # Ensure that the function processes valid input and prints a confirmation message
  expect_equal(validate_automate_sample_pooling_input(valid_data, "Project_1", 100, 10),
                "Input passed validation...")

  # Test 2: Invalid Quant (not a data frame)
  # Ensure that an error is raised when Quant is not a data frame
  expect_error(validate_automate_sample_pooling_input(list(), "Project_1", 100, 10),
               "Quant must be a data frame.")

  # Test 3: Missing required columns
  # Create an invalid data frame by removing a required column
  invalid_data <- valid_data[, -3]  # Remove the Position column
  expect_error(validate_automate_sample_pooling_input(invalid_data, "Project_1", 100, 10),
               "The following required columns are missing from Quant: Position")

  # Test 4: Non-numeric Quantification
  # Create an invalid data frame with non-numeric values in the Quantification column
  invalid_data <- data.frame(
    Sample.Name = c("Sample1", "Sample2"),
    Quantification = c("Ten", "Twenty"),  # Non-numeric values
    Position = c("A1", "A2"),
    Plate = c("Plate1", "Plate2")
  )
  expect_error(validate_automate_sample_pooling_input(invalid_data, "Project_1", 100, 10),
               "The 'Quantification' column must contain numeric values.")

  # Test 5: Invalid Max_Volume
  # Ensure that an error is raised when Max_Volume is not a positive numeric value
  expect_error(validate_automate_sample_pooling_input(valid_data, "Project_1", -10, 5),
               "Max_Volume must be a positive numeric value.")

  # Test 6: Invalid Min_Volume
  # Ensure that an error is raised when Min_Volume is greater than or equal to Max_Volume
  expect_error(validate_automate_sample_pooling_input(valid_data, "Project_1", 100, 110),
               "Min_Volume must be a positive numeric value smaller than Max_Volume.")

  # Test 7: Invalid Date_Project
  # Ensure that an error is raised when Date_Project is an empty string
  expect_error(validate_automate_sample_pooling_input(valid_data, "", 100, 10),
               "Date_Project must be a non-empty string.")

  # Test 8: Check Group column handling (default value)
  # Verify that if the Group column is not provided, it defaults to "1"
  result <- valid_data
  result$Group <- NULL  # Remove Group column
  expect_equal(validate_automate_sample_pooling_input(result, "Project_1", 100, 10),
                "Input passed validation...")  # Ensure the function runs without error
})
