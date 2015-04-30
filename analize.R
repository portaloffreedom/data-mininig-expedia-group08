# Parameters
FILENAME <- "training_set_VU_DM_2014.csv"

# load the file - is gonna take some time
dataset <- read.csv(FILENAME)

# create a minified version
SELECTION_SIZE <- as.integer(nrow(train)/10 + .5)
selection <- sample(seq(1,nrow(dataset)), SELECTION_SIZE)
dataset_min <- dataset[selection,]
write.csv(dataset_min, "min.csv")

# parse datetime example
strptime(as.character(dataset[1,2]), format = "%Y-%m-%d %H:%M:%S")
