#'  Assign every student into groups with set number of students per group
#'
#' @description
#'  Assign every student into groups with set number of students per group,
#'  with set number of iterations (overlap not considered)
#'
#' @param data a data frame
#' @param students_per_group a positive integer
#' @param iterations a positive integer
#' @param initial_matrix matrix generated with `initmat()`
#'
#' @return data frame
#' @export
#'
#' @examples
#' # Assign 9 students into 3 groups of 3, with 3 iterations
#' data <- GenerateData(9)
#' M <- initmat(data$Student)
#' MakeGroups(data, 3, 3, M)
MakeGroups <- function(data, students_per_group, iterations, initial_matrix) {
  num_students <- nrow(data)
  # ceiling or floor?
  num_groups <- ceiling(num_students / students_per_group)
  # Initialize a list to store group assignments for each iteration
  group_assignments_list <- list()
  group_assignments_list <- vector("list", length = iterations)

  # Initialize the matrix for each iteration
  initial_matrix <- initmat(data$Student)

  for (i in seq_len(iterations)) {
    shuffled_students <- sample(data$Student)

    # Create initial group assignments
    group_assignments <- rep(seq_len(num_groups),
                             each = students_per_group,
                             length.out = num_students)

    ## This will always put extra students in existing groups
    ## Maybe want to create a new group if rem_stud/n_group > 1/2
    # Distribute any remaining students among the existing groups
    remaining_students <- num_students %% students_per_group
    if (remaining_students != 0) {
      extra_indices <- (num_students - remaining_students + 1):num_students
      extra_groups <- sample(seq_len(num_groups-1), remaining_students)
      group_assignments[extra_indices] <- extra_groups
    }

    #initial_matrix <- updatemat(initial_matrix, shuffled_students, group_assignments)
    # Update the matrix
    for (g in seq_len(num_groups)) {
      groupMembers <- shuffled_students[group_assignments == g]
      for (j in seq_along(groupMembers)) {
        for (k in seq_along(groupMembers)) {
          if (j != k) {
            rn <- which(rownames(initial_matrix) == groupMembers[j])
            cn <- which(colnames(initial_matrix) == groupMembers[k])
            initial_matrix[rn, cn] <- 1
          }
        }
      }
    }
    # Add group assignments to the list
    iteration_groups <- data.frame(Student = shuffled_students,
                                   round = group_assignments)
    names(iteration_groups) <- c("Student", paste0("Round", i))
    group_assignments_list[[i]] <- iteration_groups
  }

  # Combine all data frames for each round into a single data frame
  combined_data <- Reduce(function(x, y) merge(x, y, by = "Student", all = TRUE), group_assignments_list, init = data.frame(Student = data$Student))

  return(combined_data)
}
