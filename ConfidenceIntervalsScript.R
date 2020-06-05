library(magrittr) # To use pipes (%>%)

design <- readRDS("DataFiles/design_factors.Rds") # Populated from Excel spreadsheet in package
df <- readRDS("DataFiles/Table 9.2.Rds")          # This is the table after columns and variables have been renamed, and any unwanted columns removed, but before the main processing has been done.

# Table 9.2 is a time series/type 1 table, the code below is based on code in process_table_type_1.R
# The main 'variable' column is called 'blank' as in the final tables the column name is hidden.

df <- tidyr::gather(df,
                    key = `Year`, 
                    value = `Percent`, 
                    `2007/2008`, 
                    `2009/2010`, 
                    `2011`, 
                    `2012`, 
                    `2013`, 
                    `2014`, 
                    `2015`, 
                    `2016`, 
                    `2017`, 
                    `2018`) %>%
  dplyr::mutate(`blank` = factor(`blank`, levels = c("Health services - Satisfied", 
                                                     "Health services - Neither nor", 
                                                     "Health services - Dissatisfied", 
                                                     "Healt services - No opinion", 
                                                     "Health services - All", 
                                                     "Health services - Base", 
                                                     "Schools - Satisfied", 
                                                     "Schools - Neither nor", 
                                                     "Schools - Dissatisfied", 
                                                     "Schools - No opinion", 
                                                     "Schools - All", 
                                                     "Schools - Base", 
                                                     "Public Transport - Satisfied", 
                                                     "Public Transport - Neither nor", 
                                                     "Public Transport - Dissatisfied", 
                                                     "Public Transport - No Opinion", 
                                                     "Public transport - All", 
                                                     "Public transport - Base", 
                                                     "Composite measre - Satisfied", 
                                                     "Composite measure - Neither nor", 
                                                     "Composite measure - Dissatisfied", 
                                                     "Composite measure - No opinion", 
                                                     "Composite measure - All", 
                                                     "Composite measure - Base"))) %>%
  
  dplyr::group_by(Council, Year)

# At this point, the table is fine for this stage in the processing.
# The group_by function above means that the following code applies to each combination of Council and Year

View(df)

df <- dplyr::mutate(df, 
                    Base = Percent[`blank` == 'Health services - Base'])

# This is the part that needs changed, lines 52/53 are adding a column called Base, 
# populated with the 'Health services - Base' value.

# It should be populated with 'Health services - Base' if the 'blank' value starts with 'Health Services', 
# 'Schools - Base' if it starts with 'Schools' etc.

# You can see what effect it has:

View(df)

# These lines shouldn't need changed as they calculate the confidence intervals from the Base value.
# If that's calculated correctly in lines 52/53, this should still run okay:

df <- merge(df, design, by = 'Year') %>%
dplyr::mutate(sig_value = 1.96 * as.numeric(Factor) * (sqrt((as.numeric(Percent) / 100) * (1 - (as.numeric(Percent) / 100)) / as.numeric(Base))), 
                    sig_lower = as.numeric(Percent) - (100 * sig_value), 
                    sig_lower = round(as.numeric(sig_lower), 1), 
                    sig_upper = as.numeric(Percent) + (100 * sig_value), 
                    sig_upper = round(as.numeric(sig_upper), 1), 
                    Percent = dplyr::if_else(Percent > 0, as.character(round(as.numeric(Percent), 1)), as.character(Percent))) %>%

dplyr::ungroup()

View(df)

