#' Generalized function to identify the students column and assign students into groups (MakeGroups)
#'
#' @description
#' This function automatically detects the column containing student names
#' in a dataset and assigns students into groups using `MakeGroups`.
#'
#' @param data A data frame containing at least one column with student names.
#' @param students_per_group A positive integer specifying the number of students per group.
#' @param iterations A positive integer specifying the number of iterations.
#' @param initial_matrix A matrix generated with `initmat()`.
#'
#' @return A data frame containing student group assignments across multiple rounds.
#' @export
#'
#' @examples
#' # Example dataset where the student column is not predefined
#' example_data <- data.frame(Name = LETTERS[1:9], Score = rnorm(9))
#' M <- initmat(example_data$Name)
#' is.students(example_data, 3, 3, M)
is.students <- function(data, students_per_group, iterations, initial_matrix) {
  # Step 1: Identify potential student column(s)
  char_cols <- sapply(data, function(col) is.character(col) || is.factor(col))  # Find character/factor columns
  unique_counts <- sapply(data, function(col) length(unique(col)) / length(col)) # Compute uniqueness ratio

  # Step 2: Select the most likely student column
  candidate_cols <- names(data)[char_cols & unique_counts > 0.8]  # Filter columns with mostly unique values

  if (length(candidate_cols) == 0) {
    stop("Could not automatically detect a student name column. Please make sure your dataset contains a column with unique student names.")
  }

  # Choose the first matching column (most likely student identifier)
  student_col <- candidate_cols[1]

  # Step 3: Extract student names
  student_names <- data[[student_col]]

  # Step 4: Create standardized data frame for MakeGroups
  standardized_data <- data.frame(Student = student_names, stringsAsFactors = FALSE)

  # Step 5: Call the original MakeGroups function
  result <- MakeGroups(standardized_data, students_per_group, iterations, initial_matrix)

  return(result)
}
