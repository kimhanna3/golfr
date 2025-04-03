#' Reformat Group Assignment Output
#'
#' @description
#' This function takes the output from `MakeGroups()` and transposes it so that
#' each row represents a round and each column represents a student.
#'
#' @param groups_df A data frame produced by `MakeGroups()`, where rows represent students
#'        and columns represent group assignments across multiple rounds.
#'
#' @return A transformed data frame where rows represent rounds and columns represent students.
#' @export
#'
#' @examples
#' exdf <- GenerateData(9)
#' initial_mat <- initmat(exdf)
#' groups_df <- MakeGroups(exdf, 3, 3, initial_mat)
#' ReformatGroups(groups_df)
ReformatGroups <- function(groups_df) {
  # Ensure the input is valid
  if (!("Student" %in% colnames(groups_df))) {
    stop("The input data frame must contain a 'Student' column.")
  }

  # Extract round columns (everything except "Student")
  rounds_only <- groups_df[, -1, drop = FALSE] #use lapply for each round in isolation

  # Transpose the data: make rounds the rows and students the columns
  reformatted_df <- as.data.frame(t(rounds_only))

  # Assign proper row and column names
  colnames(reformatted_df) <- groups_df[[Student]]
  rownames(reformatted_df) <- colnames(rounds_only)

  return(reformatted_df)
}
