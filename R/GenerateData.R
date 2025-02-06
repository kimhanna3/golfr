#' Generates test data frame of the students with unique ID (uppercase letters)
#'
#' @param num_students a positive integer.
#'
#' @return a data frame
#' @export
#'
#' @examples
#' # five students
#' GenerateData(5)
GenerateData <- function(num_students) {

  Names <- c(LETTERS, paste(LETTERS))[seq_len(num_students)]
  student_df <- data.frame(Student = Names, stringsAsFactors = FALSE)

  return(student_df)
}
