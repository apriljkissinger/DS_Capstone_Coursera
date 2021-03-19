# List the library paths
# The issue is likely to be in the first directory
paths = .libPaths()

## Try and detect bad files
list.files(paths, 
           pattern = "^00LOCK*|*\\.rds$|*\\.RDS$",
           full.names = TRUE)

## List files of size 0
l = list.files(paths, full.names = TRUE)
l[sapply(l, file.size) == 0]