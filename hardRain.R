###
#hardRain script to screen out noisy recordings containing rain
#
# J.Green 29-09-24
#
# hardRain will freeze and keep spinning while classifying recordings if any of the .wav files are corrupt. 
# This script was written assuming all corrupt files have been cleaned from the dataset.
# If you need to detect and isolate corrupt files in your data set, see ParaScanner Script in github repository. 

#Installation

install.packages("devtools")
devtools::install_github("https://github.com/Cdevenish/hardRain", force = TRUE)
install.packages("tuneR")
install.packages("ggplot2")
install.packages("caret")
install.packages("doParallel")

#Call in packages

library(hardRain)
library(tuneR)
library(ggplot2)
library(caret)
library(parallel)

# Function to split WAV files

split_wav_file <- function(input_file, t.step, output_dir) {
  wav <- readWave(input_file)
  sample_rate <- wav@samp.rate
  samples_per_segment <- t.step * sample_rate
  
  num_segments <- ceiling(length(wav@left) / samples_per_segment)
  
  # Extract base file name without extension
  base_name <- tools::file_path_sans_ext(basename(input_file))
  
  # Reset segment index for each file
  for (i in 1:num_segments) { 
    start_sample <- (i - 1) * samples_per_segment + 1
    end_sample <- min(i * samples_per_segment, length(wav@left))
    
    segment <- wav[start_sample:end_sample]
    
    # Create a unique output filename
    output_file <- file.path(output_dir, paste0(base_name, "_segment_", i, ".wav")) 
    
    tryCatch({
      writeWave(segment, output_file)
      message("Written: ", output_file)
    }, error = function(e) {
      message("Error writing file", output_file, "\n", e$message)
    })
  }
}


# Create input directories for rain/non-rain clips
#training input directory

rain_dir_train <- "/users/Jorda/Desktop/Rain_train"
non_rain_dir_train <- "/users/Jorda/Desktop/Non_rain_train"


# Create output directory for clipped 15s segments
#training output directory

rain_output_dir_train <- "/users/Jorda/Desktop/rainclipstrain"
nonrain_output_dir_train <- "/users/Jorda/Desktop/nonrainclipstrain"


# Set length of each segment in seconds
t.step <- 15

# List .wav files from the input directories
rain_files_train <- list.files(rain_dir_train, pattern = "\\.wav$", full.names = TRUE)
non_rain_files_train <- list.files(non_rain_dir_train, pattern = "\\.wav$", full.names = TRUE)


# Loop through rain directory and split each file
for (file in rain_files_train) {
  split_wav_file(file, t.step, rain_output_dir_train)
}

#Loop through non-rain directory and split each file
for (file in non_rain_files_train) {
  split_wav_file(file, t.step, nonrain_output_dir_train)
}


#List the audio files for training

train.BR <- list.files(path= rain_output_dir_train, pattern = "\\.wav$", full.names = TRUE)
train.MCR <- list.files(path = nonrain_output_dir_train, pattern = "\\.wav$", full.names = TRUE)

# Calculate Thresholds

trBR <- getThreshold(train.BR)
write.csv(trBR, file = "trBR.csv")

trMCR <- getThreshold(train.MCR)
write.csv(trMCR, file = "trMCR.csv")

# Inspect Metrics

metBR <- getMetrics(train.BR)
metMCR <- getMetrics(train.MCR)


# Combine into a dataframe

psd_s2n <- data.frame(
    filename = c(rownames(metBR), rownames(metMCR)),
    rbind(metBR, metMCR),
    loc = c(rep("BR", length(train.BR)), rep("MCR", length(train.MCR))))


psd_s2n$rain <- ifelse(grepl("rain", psd_s2n$filename), TRUE, FALSE)

### Visualize data
# Create boxplots - Power Spectral Density(PSD)

# Band.1.psd
ggplot(psd_s2n, aes(x = factor(rain), y = band.1.psd, fill = loc)) +
  geom_boxplot() +
  labs(title = "Power Spectral Density (Band 1) by Rain Status and Location") +
  theme_minimal()

#Band.2.psd
ggplot(psd_s2n, aes(x = factor(rain), y = band.2.psd, fill = loc)) +
  geom_boxplot() +
  labs(title = "Power Spectral Density (Band 2) by Rain Status and Location") +
  theme_minimal()

# Boxplots - Signal to Noise Ratio (S2N)

#Band.1.S2N
ggplot(psd_s2n, aes(x = factor(rain), y = band.1.s2n, fill = loc)) +
  geom_boxplot() +
  labs(title = "Signal to Noise Ratio (Band 1) by Rain Status and Location") +
  theme_minimal()

#Band.2.S2N
ggplot(psd_s2n, aes(x = factor(rain), y = band.2.s2n, fill = loc)) +
  geom_boxplot() +
  labs(title = "Signal to Noise Ratio (Band 2) by Rain Status and Location") +
  theme_minimal()

# Begin model testing

#Create input directories for test files

rain_dir_test <- "/users/Jorda/Desktop/Rain_test"
non_rain_dir_test <- "/users/Jorda/Desktop/non_rain_test"

#Create output directory for clipped test files

rain_output_dir_test <- "/users/Jorda/Desktop/rainclipstest"
non_rain_output_dir_test <- "/users/Jorda/Desktop/nonrainclipstest"


# List .wav files from the input directories
rain_files_test <- list.files(rain_dir_test, pattern = "\\.wav$", full.names = TRUE)
non_rain_files_test <- list.files(non_rain_dir_test, pattern = "\\.wav$", full.names = TRUE)

#split test files to 15s segments

# Loop through rain directory and split each file
for (file in rain_files_test) {
  split_wav_file(file, t.step, rain_output_dir_test)
}

#Loop through non-rain directory and split each file
for (file in non_rain_files_test) {
  split_wav_file(file, t.step, non_rain_output_dir_test)
}

#Load test files

test.BR <- list.files(path = rain_output_dir_test, pattern = "\\.wav$", full.names = TRUE)  
test.MCR <- list.files(path = non_rain_output_dir_test, pattern = "\\.wav$", full.names = TRUE)


# Classify test files


resBR <- classifyRain(test.BR, thresh.vals = trBR, threshold = "Q2")
resMCR <- classifyRain(test.MCR, thresh.vals = trMCR, threshold = "Q2")

#Evaluate classification results

tapply(resBR$value, list(resBR$threshold), table)
tapply(resMCR$value, list(resMCR$threshold), table)

table(resBR)
table(resMCR)

# Evaluate performance with confusion matrices

# Define truth filenames
truth_filenames <- c(
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_1.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_2.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_3.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_4.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_5.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_6.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_12.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_11.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_10.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_9.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_8.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_7.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_13.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_14.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_15.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_16.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_17.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_18.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_4.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_3.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_2.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_1.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_20.wav",
  "~/Desktop/rainclipstest/1253_20240713_204500_segment_19.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_5.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_6.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_7.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_8.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_9.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_10.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_16.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_15.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_14.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_13.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_12.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_11.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_17.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_18.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_19.wav",
  "~/Desktop/rainclipstest/1262_20240528_061200_segment_20.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_20.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_19.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_18.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_17.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_11.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_12.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_13.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_14.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_15.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_16.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_10.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_9.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_8.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_7.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_6.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_5.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_19.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_20.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_1.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_2.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_3.wav",
     "~/Desktop/nonrainclipstest/1212_20240529_064100_segment_4.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_18.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_17.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_16.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_15.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_14.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_13.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_7.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_8.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_9.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_10.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_11.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_12.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_6.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_5.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_4.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_3.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_2.wav",
     "~/Desktop/nonrainclipstest/1246_20240520_054900_segment_1.wav")


# Define truth values (ensure they match the number of files)
truth_values <- c(
  rep(TRUE, 40),   # First 40 are TRUE (for rain)
  rep(FALSE, 40)   # Next 40 are FALSE (for non-rain)
)

# Check lengths
if (length(truth_filenames) != length(truth_values)) {
  stop("The number of filenames and truth values must match.")
}

# Create the data frame
truth_df_test <- data.frame(
  filename = truth_filenames,
  truth_column = truth_values
)


# Predictions from the BR and MCR models
predictions_BR <- resBR$value  
predictions_MCR <- resMCR$value  

# Combine predictions and truth values into a single data frame
combined_predictions <- data.frame(
  filename = c(resBR$filename, resMCR$filename),
  predicted = c(predictions_BR, predictions_MCR),
  truth = c(truth_df_test$truth_column)  # Use your truth column from the data frame
)

# Ensure lengths match
if (nrow(combined_predictions) != nrow(truth_df_test)) {
  stop("Number of predictions must match number of truth values.")
}

# Create a confusion matrix
confusion_matrix <- confusionMatrix(
  as.factor(combined_predictions$predicted), 
  as.factor(combined_predictions$truth)
)

# Print the confusion matrix
print(confusion_matrix)

# metrics:
accuracy <- confusion_matrix$overall['Accuracy']
sensitivity <- confusion_matrix$byClass['Sensitivity']
specificity <- confusion_matrix$byClass['Specificity']

# Print metrics
cat("Accuracy:", accuracy, "\n")
cat("Sensitivity:", sensitivity, "\n")
cat("Specificity:", specificity, "\n")


############### Step 3: Classify Data set

# The raw 5 minute .wav files take very long to run (approximately 8 mins processing time/5-min of audio)
# This is prohibitive for large data sets due to long processing times. However, hardRain is efficient at 
# classifying shorter clips (1 min). Segmenting the dataset into 1-min clips significantly reduces processing time
# (5 seconds/5 mins of audio). While classifying the data set, if at least one segment of a recording is classified 
# as rain, the remaining segments of that recording will also be excluded as rain. 
#

# Load necessary packages
library(tuneR)
library(dplyr)
library(fs)
library(doSNOW)
library(parallel)
library(future)
library(future.apply)


# Define the base directory containing the .wav files
dataset_dir <- "E:/ARU Data/2024/2024_ARU_Recordings_Complete"

#Create output directory for clipped files
clipped_dataset <- "E:/ARU Data/2024/Clipped"

# List files in dataset
Raw_files <- list.files(dataset_dir, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)

# Load the list of corrupted files
corrupted_files_df <- read.csv("E:/ARU Data/2024/corrupted_files.csv")  
corrupted_files <- corrupted_files_df$corrupted_files  # Adjust the column name as needed

# Exclude corrupted files
Raw_files <- Raw_files[!Raw_files %in% corrupted_files]  # Exclude by file name

#Function to split .wav files

split_wav_file <- function(input_file, t.step, output_dir) {
  wav <- readWave(input_file)
  sample_rate <- wav@samp.rate
  samples_per_segment <- t.step * sample_rate
  
  num_segments <- ceiling(length(wav@left) / samples_per_segment)
  
  # Extract base file name without extension
  base_name <- tools::file_path_sans_ext(basename(input_file))
  
  # Reset segment index for each file
  for (i in 1:num_segments) { 
    start_sample <- (i - 1) * samples_per_segment + 1
    end_sample <- min(i * samples_per_segment, length(wav@left))
    
    segment <- wav[start_sample:end_sample]
    
    # Create a unique output filename
    output_file <- file.path(output_dir, paste0(base_name, "_segment_", i, ".wav")) 
    
    tryCatch({
      writeWave(segment, output_file)
      message("Written: ", output_file)
    }, error = function(e) {
      message("Error writing file", output_file, "\n", e$message)
    })
  }
}


#Set segment length to 60 seconds

t.step <- 60


# Set up parallel processing to split recordings

num_cores <- availableCores() - 2  # Leave a couple cores free
plan(multisession, workers = num_cores)


# Use future_lapply to process files in parallel
results_all <- future_lapply(Raw_files, function(file) {
  # Split the file
  tryCatch({
    split_wav_file(file, t.step, clipped_dataset)
    return(paste("Processed:", file))  # Return success message
  }, error = function(e) {
    message(paste("Error processing file:", file, ":", e$message))
    return(NULL)  # Return NULL in case of error
  })
})


# List all clipped files
clipped_files <- list.files(clipped_dataset, pattern = "\\.wav$", full.names = TRUE, recursive = TRUE)



## If trBR was calculated in another session and you are uploading a .csv, you may run into data structure issues
# Here is my fix

trBR <- data.frame(
  band.1.psd = c(0.000105477, 0.000395490),
  band.2.psd = c(0.000034800, 0.000383828),
  band.1.s2n = c(1.259287364, 1.895601603),
  band.2.s2n = c(1.188718756, 1.771311239),
  row.names = c("min", "Q2")
)

# Set trBR as a matrix 
trBR <- as.matrix(trBR)

# Number of cores to use for parallel processing
num_cores <- detectCores() - 2 # Leave a few cores free

# Set up the cluster
cl <- makeCluster(num_cores, type = "SOCK")
registerDoSNOW(cl)

# Initialize a progress bar
total <- length(clipped_files)
pb <- txtProgressBar(min = 0, max = total, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

# Use foreach to process files in parallel
results_all <- foreach(i = seq_along(clipped_files), .packages = 'hardRain', .combine = rbind, .options.snow = opts) %dopar% {
  # Classify the file
  result <- tryCatch({
    classifyRain(
      wav = clipped_files[i],
      thresh.vals = trBR,
      threshold = "Q2"  # Use Q2 (more conservative)
    )
  }, error = function(e) {
    message(paste("Error processing file:", clipped_files[i], ":", e$message))
    return(NULL)  # Return NULL in case of error
  })
  
  # Return result
  result
}

# Close the progress bar
close(pb)

# Stop the cluster
stopCluster(cl)

# Convert classification to logical (T/F)
results_all$classification <- as.logical(results_all$classification)

write.csv(results_all, file = "results_all.csv")

# extract base file names without segment number

base_filenames <- sub("_segment_\\d+\\.wav$,", "", results_all$filename)


# identify base file names with at least one TRUE classification

has_rain <- tapply(results_all$value, base_filenames, any)




# Correct the sub() pattern (removing the trailing comma and ensuring it matches the filename structure correctly)
base_filenames <- sub("_segment_\\d+\\.wav$", "", results_all$filename)

# Check the length of base_filenames and results_all$classification to ensure they match
length(base_filenames)  

# Now apply tapply correctly
has_rain <- tapply(results_all$value, base_filenames, any)

# Continue with the filtering process:
rain_files <- names(has_rain[has_rain])

# Filter out segments belonging to recordings with rain
filtered_results <- results_all[!base_filenames %in% rain_files, ]
# Save the filtered results to a CSV file
write.csv(filtered_results, "filtered_results.csv", row.names = FALSE)


# create a data frame for files containing rain
rain_files_df <- data.frame(filename = rain_files)
write.csv(rain_files_df, "rain_files.csv", row.names = FALSE)
















