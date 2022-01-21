source("02_code/Sourcer.R")
sourceAll()


# clean ####
clean(inPath = 
        "01_data/00_initial_data/Popularity Index Analysis (Responses) - Form Responses 1.csv")

# prepare ####
prepare(inPath = "01_data/02_cleaned_data/dt_cleaned.csv")

