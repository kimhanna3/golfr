#' Generates initial matrix
#'
#' @param students a column of a dataset that has students' unique IDs
#'
#' @return matrix
#' @export
#'
#' @examples
#' # Initial matrix 5x5
#' data <- GenerateData(5)
#' initmat(data$Student)
initmat <- function(students) {
  n <- length(students)
  initmat <- matrix(0, nrow = n, ncol = n, dimnames = list(students, students))
  diag(initmat) <- -99
  return(initmat)
}
