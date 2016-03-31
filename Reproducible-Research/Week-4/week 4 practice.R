install.packages("cacher")
library(cacher)
clonecache(id = "092dcc7dda4b93e42f23e038a60e1d44dbec7b3f")
clonecache(id = "092d")  ## effectively the same as above
# output: created cache directory '.cache'
showfiles() # show files stored in cache
# output: [1] "top20.R"
sourcefile("top20.R") # load R script

code() # examine the content of the code
# output:
# source file: top20.R
# 1  cities <- readLines("citylist.txt")
# 2  classes <- readLines("colClasses.txt")
# 3  vars <- c("date", "dow", "death",
# 4  data <- lapply(cities, function(city) {
# 5  names(data) <- cities
# 6  estimates <- sapply(data, function(city) {
# 7  effect <- weighted.mean(estimates[1,
# 8  stderr <- sqrt(1/sum(1/estimates[2,

graphcode() # generate graph showing structure of code
