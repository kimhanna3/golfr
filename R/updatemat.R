# updatemat
## Takes matrix, vector and integer as inputs, returns matrix
### Updates the results of first round to the initial matrix

updatemat <- function(initialmat, group_assignments, students) {
  numGroups <- length(unique(group_assignments))
  
  for (g in seq_len(numGroups)) {
    groupMembers <- students[group_assignments == g]
    for (j in seq_along(groupMembers)) {
      for (k in seq_along(groupMembers)) {
        if (j != k) {
          rn <- which(rownames(initialmat) == groupMembers[j])
          cn <- which(colnames(initialmat) == groupMembers[k])
          initialmat[rn, cn] <- 1 + initialmat[rn, cn]
        }
      }
    }
  }
  
  return(initialmat)
}