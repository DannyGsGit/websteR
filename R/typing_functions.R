
## Load sample dataset
my.data <- read.csv("./Data/AmesHousing.csv", stringsAsFactors = FALSE)




get_feature_types <- function(dataset) {
  # Generates a data frame of the data types for each column of a dataset
  #
  # Args:
  #   dataset: A [DATA FRAME] to be analyzed
  #
  # Returns:
  #   A [DATA FRAME] with two columns to describe the type for each feature
  
  types <- data.frame(feature = colnames(dataset), original.type = sapply(dataset, class))
  row.names(types) <- NULL
  return(types)
}

# zz <- get_feature_types(my.data)
