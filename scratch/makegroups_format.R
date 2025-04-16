
## How to format current return object from MakeGroups

exdf <- GenerateData(9)
gps <- MakeGroups(exdf, 3, 3, student_col = "Student")

## Get student names
stud <- gps$Student

## Lapply all of the group columns, not the stud column
lapply(gps[, -1], function(x) { 
  df <- data.frame(stud, x) # create just student and assign
  df[order(df$x), ]$stud %>% 
    matrix(byrow = TRUE, nrow = sqrt(nrow(df)))
  })
