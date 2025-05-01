# Integrated version of `MakeGroups`

MakeGroups2 <- function(df, students_per_group, iterations, student_col) {
  # Extract student names from specified column
  student_names <- df[[student_col]]
  num_students <- length(student_names)
  num_groups <- ceiling(num_students / students_per_group)

  # Initialize interaction matrix
  initial_matrix <<- initmat(student_names)  # Global so testoverlap() can access it
  conflict_scores <- numeric(iterations)
  group_assignments_list <- vector("list", length = iterations)

  for (i in seq_len(iterations)) {
    valid_assignment_found <- FALSE
    attempt <- 0

    while (!valid_assignment_found) {
      attempt <- attempt + 1
      shuffled_students <- sample(student_names)

      group_assignments <- rep(seq_len(num_groups),
                               each = students_per_group,
                               length.out = num_students)

      # Handle leftover students
      remaining_students <- num_students %% students_per_group
      if (remaining_students != 0) {
        extra_indices <- (num_students - remaining_students + 1):num_students
        extra_groups <- sample(seq_len(num_groups - 1), remaining_students)
        group_assignments[extra_indices] <- extra_groups
      }

      # Check for overlap violations
      all_groups <- split(shuffled_students, group_assignments)
      group_check_results <- sapply(all_groups, nooverlap)

      if (sum(group_check_results) == 0) {
        valid_assignment_found <- TRUE
      }
    }

    # Replaced nested for-loop that manually updated the matrix
    # → Now handled by `updatemat()`
    previous_matrix <- initial_matrix
    initial_matrix <- updatemat(initial_matrix, group_assignments, shuffled_students)

    # Add conflict score using `conflict_score()`
    conflict_scores[i] <- conflict_score(initial_matrix, previous_matrix)

    # Store group assignment for this round
    iteration_groups <- data.frame(Student = shuffled_students,
                                   round = group_assignments)
    names(iteration_groups) <- c("Student", paste0("Round", i))
    group_assignments_list[[i]] <- iteration_groups
  }

  # Merge all round data frames
  combined_data <- Reduce(function(x, y) merge(x, y, by = "Student", all = TRUE),
                          group_assignments_list,
                          init = data.frame(Student = student_names))

  # Final output
  return(list(
    groups = combined_data,
    conflict_scores = conflict_scores
  ))
}

# example execution
# df <- GenerateData(9)
# results <- MakeGroups(df, 3, 3, student_col = "Student")
# results$groups           # Group assignments
# results$conflict_scores  # Conflict score for each round
