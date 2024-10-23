####
### Randomly select and chop audio files to be analyzed
## J. Green 18/10/2024

#install nescessary packages

install.packages("remotes")
remotes::install_github("ABbiodiversity/wildrtrax") #Install the WildRTrax package from GitHub

library(wildrtrax)
library(tidyverse)
library(lubridate)
library(vroom)
library(tuneR)

###### Step 1: create a list of all recordings in dataset (minus rain/corrupted files)
# wt_audio_scanner function will throw errors if corrupt files are present. Use ParaScanner to identify and
# isolate corrupt files

# call in corrupted files

corrupted_files <- read.csv("E:/ARU Data/2024/corrupted_files.csv")
corrupted_files_vector <- corrupted_files$corrupted_files

## We can remove these files from our dataset if it hasn't already been done

removed_files <- file.remove(corrupted_files_vector)

# Now that corrupt files have been removed from the dataset,create a list of recordings to choose from (clean data)

rec.list <-  wt_audio_scanner(path = "E:/ARU Data/2024/2024_ARU_Recordings_Complete", file_type = "wav", extra_cols = T) 

# add columns for time of recording

rec.list <- rec.list %>%
  mutate(hour = hour(recording_date_time), minute = minute(recording_date_time))

# add column for plot number

rec.list.tibble <- tibble(rec.list) %>%
  separate(location, c("plot"), sep = "-", remove=F)

write.csv(rec.list.tibble, file = "E:/ARU Data/2024/RecordingList.csv") #Write the recording list as a .csv

# now that our list is generated, let's load in rain_files.csv to exclude these files from selection

## The harRain output didn't include pathnames or.wav extension in file name outputs. Let's add it! (skip if path name is present)

# Create a function to add the full path and .wav extension
add_full_path <- function(file_name) {
  # Ensure the filename ends with .wav
  if (!grepl("\\.wav$", file_name)) {
    file_name <- paste0(file_name, ".wav")
  }
  
  # Extract the first four characters (the folder number)
  folder_number <- substr(file_name, 1, 4)
  
  # Construct the full path using the folder number
  full_path <- file.path("E:/ARU Data/2024/2024_ARU_Recordings_Complete", folder_number, "Data", file_name)
  
  return(full_path)
}

# Apply the function to the filenames in the CSV
rain_files$full_path <- sapply(rain_files$rain_files, add_full_path)

# Write the .csv
write_csv(rain_files, "E:/ARU Data/2024/rain_files_with_paths.csv")

#read in the files
rain_files <- read_csv("E:/ARU Data/2024/rain_files_with_paths.csv")


# set seed for reproducibility 
set.seed(612)

data.analysis <- rec.list.tibble %>%
  anti_join(rain_files, by = c("location" = "rain_files")) %>%
  filter(length_seconds > 180) %>%  # Only keep recordings longer than 180 seconds
  group_by(row_number()) %>%
  mutate(rando = runif(1)) %>%
  group_by(plot) %>%
  filter(julian <= (min(julian) + 14), hour >= 5 & hour <= 23) %>%
  ungroup() %>%
  group_by(plot, julian) %>%
  filter(rando == min(rando)) %>%
  ungroup() %>%
  group_by(plot) %>%
  arrange(plot, rando) %>%
  mutate(ord.num = row_number()) %>%
  filter(ord.num < 8) %>%
  ungroup()


# confirm the number of recordings for each plot (there should be 7/plot) 
data.check <- data.analysis %>%
  group_by(plot) %>%
  summarize(rec.count = length(hour), first.date = min(julian), last.date = max(julian))

# write out the recordings selected for analysis to a .csv

write.csv(data.analysis, file = "E:/ARU Data/2024/Recordings_Analyzed.csv")

##### Step 3: Chop files to proper length for upload to wildtrax


# Finding files longer than 3 minutes
filestochop <- data.analysis %>%
  filter(length_seconds > 180)

# Convert recording_date_time to POSIXct format
rec.list.tibble$recording_date_time <- as.POSIXct(rec.list.tibble$recording_date_time, format="%Y-%m-%d %H:%M:%S")
filestochop$recording_date_time <- as.POSIXct(filestochop$recording_date_time, format="%Y-%m-%d %H:%M:%S")

#Create output for 3 minute clips
output_folder = "E:/ARU Data/2024/wildtrax_recordings"

# Loop through the file list and chop recordings down to 3 minutes

for (i in 1:nrow(filestochop)) {
  # Get the file path of the current audio file
  file_path <- filestochop$file_path[i]  
  
  # Check if the file exists
  if (file.exists(file_path)) {
    # Read the audio file
    audio <- readWave(file_path)
    
    # Calculate the number of samples for 3 minutes
    samples_to_keep <- min(180 * audio@samp.rate, length(audio@left))  # Use length(audio@left) instead of nframes
    
    # Extract the first 3 minutes of audio
    trimmed_audio <- extractWave(audio, from = 1, to = samples_to_keep)
    
    # Define the new file name (keeping the same structure but in the output folder)
    output_file_name <- file.path(output_folder, basename(file_path))
    
    # Write the trimmed audio to a new file
    writeWave(trimmed_audio, output_file_name)
    
    print(paste("Created trimmed file:", output_file_name))
  } else {
    print(paste("File not found:", file_path))
  }
}


#list chopped files

rec.list.chopped <- wt_audio_scanner(path = "E:/ARU Data/2024/wildtrax_recordings",
                                  file_type = "wav", extra_cols = T)

#Add hour and minute columns

rec.list.chopped <- rec.list.chopped %>%
  mutate(hour = hour(recording_date_time), minute = minute(recording_date_time))

# add plot column
rec.list.chopped <- tibble(rec.list) %>%
  separate(location, c("plot"), sep = "-", remove=F)

# write .csv for the final, clean, chopped recordings to be sent off for analysis
write.csv(rec.list.chopped, file = "E:/ARU Data/2024/ChoppedRecordingList.csv")


