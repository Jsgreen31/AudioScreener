
### Script to detect and isolate corrupted .wav files

# J. Green 16/10/2024

# Load necessary packages
library(tuneR)
library(dplyr)
library(fs)
library(doSNOW)
library(foreach)

# Function to check if an audio file is valid
is_valid_audio_file <- function(file) {
  result <- tryCatch({
    wave <- readWave(file)  # read the WAV file
    TRUE  # return TRUE if reading is successful
  }, error = function(e) {
    FALSE  # return FALSE if an error occurs
  })
  return(result)
}

# Define the main directory
main_directory <- "/users/Jorda/Desktop/ARU2024"
folder_list <- fs::dir_ls(main_directory, type = "directory")

# Initialize a vector to store corrupted files
corrupted_files <- c()

# List all audio files across all folders (plots)
all_audio_files <- unlist(lapply(folder_list, function(folder) {
  fs::dir_ls(file.path(folder, "Data"), glob = "*.wav")
}))

# Calculate total number of audio files for progress bar
total_files <- length(all_audio_files)

# Set up parallel cluster
num_cores <- detectCores() - 1  # Leave one core free
cl <- makeCluster(num_cores, type = "SOCK")
registerDoSNOW(cl)

# Create progress bar
pb <- txtProgressBar(min = 0, max = total_files, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Run the checks in parallel

corrupted_files <- foreach(file = all_audio_files, .combine = c, .packages = 'tuneR', .options.snow = opts) %dopar% {
  valid <- is_valid_audio_file(file)  # Check if the file is valid
  
  # Return the corrupted file if it's not valid
  if (!valid) {
    return(file)  
  } else {
    return(NULL)  # Return NULL for valid files
  }
}

# Close the progress bar
close(pb)

# Stop the cluster
stopCluster(cl)

# Filter out NULL values to get the list of corrupted files
corrupted_files <- corrupted_files[!sapply(corrupted_files, is.null)]

# Write the list of corrupted files to a CSV
write.csv(data.frame(corrupted_files), file = "/users/Jorda/Desktop/corrupted_files.csv", row.names = FALSE)
cat("Number of corrupted files found:", length(corrupted_files), "\n")
