# websteR

An R package for creating and consuming data dictionaries for data science projects.  

The data dictionary is used to document and apply the following to a *raw* dataset:
* Column typing
* Flag column for removal
* Description
* Additional notes

The dictionary is exported as a human-readable CSV file and used as documentation for the data pipeline. Furthermore, the dictionary can be consumed by websteR to apply typing and column removal in an R script; ensuring data manipulation and documentation are synchronized.
