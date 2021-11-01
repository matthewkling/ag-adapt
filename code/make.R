functions <- list.files("code/production", pattern = "\\.R", full.names = T)
x <- sapply(functions, source)
make(plan)