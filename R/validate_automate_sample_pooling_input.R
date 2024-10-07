#' Validate Input for Automate Sample Pooling Function
#' @description This function validates the input parameters for the `automate_sample_pooling` function.
#' It checks that the provided data frame has the required columns, ensures numeric constraints on volume parameters,
#' and verifies that the project date is a valid non-empty string.
#' @param Quant A data frame containing sample information. It must include the columns:
#'   "Sample Name", "Quantification", "Position", "Plate". The "Group" column is optional.
#' @param Date_Project A string representing the project name, used for output file naming.
#' @param Max_Volume A numeric value indicating the maximum volume allowed in the pools. Must be greater than 0.
#' @param Min_Volume A numeric value indicating the minimum volume allowed in the pools. Must be greater than 0 and less than Max_Volume.
#' @return A validated data frame, possibly with a default "Group" column added.
#' @keywords internal

validate_automate_sample_pooling_input <- function(Quant, Date_Project, Max_Volume, Min_Volume) {
  # Validate that Quant is a data frame
  if (!is.data.frame(Quant)) {
    stop("Quant must be a data frame.")
  }

  # Ensure required columns are present
  required_columns <- c("Sample.Name", "Quantification", "Position", "Plate")
  missing_columns <- setdiff(required_columns, colnames(Quant))
  if (length(missing_columns) > 0) {
    stop(paste("The following required columns are missing from Quant:", paste(missing_columns, collapse = ", ")))
  }

  # Ensure Quantification column contains only numeric values
  if (!is.numeric(Quant$Quantification)) {
    stop("The 'Quantification' column must contain numeric values.")
  }

  # Ensure Max_Volume and Min_Volume are numeric and logical
  if (!is.numeric(Max_Volume) || Max_Volume <= 0) {
    stop("Max_Volume must be a positive numeric value.")
  }

  if (!is.numeric(Min_Volume) || Min_Volume <= 0 || Min_Volume >= Max_Volume) {
    stop("Min_Volume must be a positive numeric value smaller than Max_Volume.")
  }

  # Ensure Date_Project is a non-empty string
  if (!is.character(Date_Project) || Date_Project == "") {
    stop("Date_Project must be a non-empty string.")
  }

  # Check if Group column is optional but convert to character if it exists
  if (!("Group" %in% colnames(Quant))) {
    Quant$Group <- "1"
  } else {
    Quant$Group <- as.character(Quant$Group)
  }

  return("Input passed validation...")  # Return the possibly modified data frame
}
