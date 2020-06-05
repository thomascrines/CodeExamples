# The eval function evaluates a line of string as r code:

example_string <- "print(\"Example text\")" # (To include " in the string, you need to escape with \, i.e \")

print(example_string)

eval(parse(text = example_string))



# You can then use paste0 to make longer strings, with variables that can change:

additional_text <- " plus additional text"
example_string <- paste0("print(\"Example text", additional_text, "\")")

eval(parse(text = example_string))



# In the app, string are made by passing values to functions
# This function in in app/source/functions.R in the main project
# It takes a list of column names and rounds those columns' values

round_comparison_df_values <- function(variable_column_names) {
  
  round_string <- "dplyr::mutate(df,"
  
  for (variable_column_name in variable_column_names) {
    if (!grepl("_l", variable_column_name) & !grepl("_u", variable_column_name)) {
      round_string  = paste0(round_string , " `", variable_column_name, "` = ifelse(`", variable_column_name, "` > 0, suppressWarnings(as.character(round(as.numeric(`", variable_column_name, "`,  0)))), `", variable_column_name, "`),")
    }
  }
  round_string  <- (substr(round_string, 1, nchar(round_string ) - 1))
  round_string  <- paste0(round_string, ")")

  round_string
}



# You can then pass variables to return a full string:

variable_column_names_1 <- c("2007/2008", "2009/2010", "2011")

print(round_comparison_df_values(variable_column_names = variable_column_names_1))

# Then run using eval:

df <- readRDS("DataFiles/Table 9.2.Rds")

eval(parse(text = round_comparison_df_values(variable_column_names = variable_column_names_1)))

df # Columns "2007/2008", "2009/2010", "2011" are rounded



# Passing in different values will round different columns:

variable_column_names_2 <- c("2012", "2013", "2014", "2015", "2016", "2017")

print(round_comparison_df_values(variable_column_names = variable_column_names_1))

df <- readRDS("DataFiles/Table 9.2.Rds")

eval(parse(text = round_comparison_df_values(variable_column_names = variable_column_names_2)))

df # Columns "2012", "2013", "2014", "2015", "2016", "2017" are rounded




