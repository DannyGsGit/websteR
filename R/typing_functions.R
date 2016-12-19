# Extract classes from all columns of a data frame
#
# This function will extract the column classes from a data frame, returning a new data frame.
#
# @param dataset A data frame of the data for which we are building a dictionary.
# @return types A dataframe listing column names and their respective classes.
get_feature_types <- function(dataset) {
  types <- data.frame(feature = colnames(dataset), type = sapply(dataset, class))
  row.names(types) <- NULL
  return(types)
}



# Decorate the bare dicionary
#
# Adds decorators to the barebones dictionary. Adds Keep, Description, Notes columns
#
# @param dictionary A bare dictionary, in dataframe format, including column names and classes
#
# @return dictionary Decorated dictionary
decorate_dictionary <- function(dictionary) {
  dictionary$keep <- TRUE
  dictionary$description <- ""
  dictionary$notes <- ""

  return(dictionary)
}


#' Compile a data dictionary for a data frame
#'
#' Compiles a data dictionary for a data frame input
#'
#' @param dataset The dataframe to generate the dictionary
#' @param name The filename for the dictionary
#' @param path The filepath to locate the dictionary
#'
#' @return dict.temp The dictionary dataframe (Note: Not returned, the function will save the csv dictionary to the desired path)
#'
#' @examples
#' my.dictionary <- websteR::compile_dictionary(iris)
#'
#' @export
compile_dictionary <- function(dataset, name = "Data_Dictionary.csv", path = "./") {
  dict.temp <- get_feature_types(dataset)
  dict.temp <- decorate_dictionary(dict.temp)

  dict.temp$feature <- as.character(dict.temp$feature)
  dict.temp$type <- as.character(dict.temp$type)

  utils::write.csv(dict.temp, file = paste(path, name, sep = "/"))

  return(dict.temp)
}




#' Apply data dictionary to dataset
#'
#' Applies typing and filtering rules from a data dictionary to a dataset
#'
#' @param data Dataset to be processed
#' @param dictionary.path Filepath to the data dictionary to be enforced
#' @param enforce.keep When enforce.keep = TRUE (Default), filters out columns with a "keep" value of FALSE in the dictionary
#'
#' @return data Data frame with dictionary rules applied
#'
#' @export
enforce_dictionary <- function(data, dictionary.path, enforce.keep = TRUE) {
  # Import dictionary
  dictionary <- utils::read.csv(dictionary.path, stringsAsFactors = FALSE)

  # Filter out unwanted columns
  if(enforce.keep == TRUE) {
    # Drop columns not explicitly labeled as keep = TRUE
    filter.list <- dictionary$feature[which(dictionary$keep == TRUE)]
    data <- data[, filter.list]

    # Drop the rows from the dictionary in this session
    dictionary <- dictionary[which(dictionary$keep == TRUE), ]
  }


  # If any columns in data are factors, convert to character before applying any operations
  data.factor.features <- get_feature_types(data)
  data.factor.features <- as.character(data.factor.features$feature[which(data.factor.features$type == "factor")])

  data[, data.factor.features] <- sapply(data[, data.factor.features], as.character)


  # Apply typing to columns
  ## Factors
  factor.features <- dictionary$feature[which(dictionary$type == "factor")]
  data[, factor.features] <- lapply(data[, factor.features], as.factor)

  ## Numerics
  numeric.features <- dictionary$feature[which(dictionary$type == "numeric")]
  data[, numeric.features] <- lapply(data[, numeric.features], as.numeric)

  ## Characters
  character.features <- dictionary$feature[which(dictionary$type == "character")]
  data[, character.features] <- lapply(data[, character.features], as.character)

  ## Integers
  integer.features <- dictionary$feature[which(dictionary$type == "integer")]
  data[, integer.features] <- lapply(data[, integer.features], as.integer)

  return(data)

}
