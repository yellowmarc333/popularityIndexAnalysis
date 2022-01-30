source("02_code/Sourcer.R")
sourceAll()


# import community data ####
# users: do not execute
import(inPath =
         "01_data/00_initial_data/Popularity Index Analysis (Responses) - Form Responses 3.csv",
       outPath = "01_data/01_imported_data/dt_com_imported.csv",
       format = c("%m/%d/%Y %H:%M:%S"),
       time_col = c("Timestamp"))

# import MS data ####
import(inPath =
         "01_data/00_initial_data/Popularity_Scores_MS.csv",
       outPath = "01_data/01_imported_data/dt_MS_imported.csv",
       format = c("%d.%m.%Y"),
       time_col = c("Timestamp"))

# integrate ####
integrate_data(inPath = "01_data/01_imported_data/dt_com_imported.csv",
          inPath2 = "01_data/01_imported_data/dt_MS_imported.csv", 
          outPath = "01_data/03_integrated_data/dt_integrated.csv",
          time_col = "Timestamp",
          id_cols = c("Artist Name","Song Name"))

# clean ####
clean(inPath = "01_data/03_integrated_data/dt_integrated.csv",
      outPath = "01_data/02_cleaned_data/dt_cleaned.csv")

# prepare ####
prepare(inPath = "01_data/02_cleaned_data/dt_cleaned.csv")

# model ####
model(inPath = "01_data/04_prepared_data/dt_prepared_filtered.csv",
      target_var = "PopularityIndex",
      rm_cols = c("PopularityIndex", "ArtistSongId"),
      order_col = "DaysSinceRelease",
      nrounds = 35,
      outPath = paste0("01_data/05_model_data/model_data_",
                       gsub(date(), pattern = "[ ,:]", replacement = ""),
                       ".rds"))

# deployment ####





# todos####
# include timestamp, where discover data was implemented.
# integrate non anonoumized prepared data
# if PI <1 multiplicate by 100
# xxadd new own data
# xxintegrate own old data
# clean: check/ convert blogs better
# todo: preparedata: musicstax/sfd autoadjustment of timestamp.
# overall: adjust outpaths
# prepare: indicate if 28days mean < 7day mean (factor)

