# script to rename files

#J. Green 17/10/2024



# Set the folder path where the audio files are located
folder_path <- "E:/ARU Data/2024/2024_ARU_Recordings_Complete/1144 (files incorrectly show plot 1188 but they are indeed 1144)/Data"

# List all files in the folder
files <- list.files(path = folder_path, full.names = TRUE)

# Loop through each file and rename it
for (file in files) {
  # Get the file name (without the full path)
  file_name <- basename(file)
  
  # Check if the file starts with '1188'
  if (startsWith(file_name, "1188")) {
    
    # Replace '1188' with '1144' only in the first four characters
    new_file_name <- sub("^1188", "1144", file_name)
    
    # Create the full path for the new file name
    new_file_path <- file.path(folder_path, new_file_name)
    
    # Rename the file
    file.rename(file, new_file_path)
  }
}

# Check the results
print("Plot numbers renamed successfully!")