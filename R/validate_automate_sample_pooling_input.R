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

  # Define valid positions
  valid_positions <- c(
    "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12",
    "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09", "B10", "B11", "B12",
    "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12",
    "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10", "D11", "D12",
    "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12",
    "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12",
    "G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12",
    "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12",
    "A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9",# Include positions A1 to H9
    "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9",
    "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9",
    "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9",
    "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9",
    "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9",
    "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9",
    "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9"
  )


  # Ensure Position column contains only valid positions
  if (!all(Quant$Position %in% valid_positions)) {
    invalid_positions <- unique(Quant$Position[!Quant$Position %in% valid_positions])
    stop(paste("The following Position values are invalid:", paste(invalid_positions, collapse = ", ")))
  }

  return("Input passed validation...")
}
