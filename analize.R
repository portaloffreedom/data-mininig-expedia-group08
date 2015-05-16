# Parameters
FILENAME <- "training_set_VU_DM_2014.csv"

# load the file - is gonna take some time
dataset <- read.csv(FILENAME)

# # create a minified version | per line version
# SELECTION_SIZE <- as.integer(nrow(dataset)/10 + .5)
# selection <- sample(seq(1,nrow(dataset)), SELECTION_SIZE)
# dataset_min <- dataset[selection,]
# write.csv(dataset_min, "min.csv")

# create a minified version | per search_id version
searches = as.factor(dataset$srch_id)
searches_ints = as.numeric(searches)
num_searches = max(searches_ints)

SELECTION_SIZE <- as.integer(num_searches/10 + .5)
selection <- sample(seq(1,num_searches), SELECTION_SIZE)
dataset_min <- c()
for (search_id in searches) {
    dataset_min <- rbind(dataset_min, dataset[dataset$srch_id == search_id,])
}

colnames(dataset_min) <- dataset_min(dataset)

write.csv(dataset_min, "min.csv")

# parse datetime example
strptime(as.character(dataset[1,2]), format = "%Y-%m-%d %H:%M:%S")
