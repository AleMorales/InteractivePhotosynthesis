

# Function to write the selected scenario into a file. Use ad-hoc (human readable) format.
writeScenario <- function(fileName, Name, PhotosType, CurveType, tableFixedInputs, tableVariableInputs, tableOutputs) {
  dest = file(description = fileName, open = "w")
  writeLines(text = c("","METADATA",""), con = dest)
  writeLines(text = paste0("Name = ", Name), con = dest)
  writeLines(text = paste0("PhotosType = ", PhotosType), con = dest)
  writeLines(text = paste0("CurveType = ", CurveType), con = dest)
  writeLines(text = c("","FIXED INPUTS",""), con = dest)
  write.table(x = tableFixedInputs, file = dest, row.names = FALSE, col.names = TRUE, append = TRUE, sep = ",")
  writeLines(text = c("","VARIABLE INPUTS",""), con = dest)
  write.table(x = tableVariableInputs, file = dest, row.names = FALSE, col.names = TRUE, append = TRUE, sep = ",")
  writeLines(text = c("","OUTPUTS",""), con = dest)
  write.table(x = tableOutputs, file = dest, row.names = FALSE, col.names = TRUE, append = TRUE, sep = ",")
  close(dest)
}


readScenario = function(file) {
  # Extract lines from the source file
  source = file(description = file, open = "r")
  lines = readLines(con = source)
  close(source)
  
  # Id lines before the start of each section
  startMetadata = grep(pattern = "^METADATA", x = lines) + 1
  startFixedInputs = grep(pattern = "^FIXED INPUTS", x = lines) + 1
  startVariableInputs = grep(pattern = "^VARIABLE INPUTS", x = lines) + 1
  startOutputs = grep(pattern = "^OUTPUTS", x = lines) + 1
  
  # Calculate length of variable and fixed inputs
  lengthFixedInputs = startVariableInputs - startFixedInputs - 4
  lengthVariableInputs = startOutputs - startVariableInputs - 4
  
  # Extract name
  nameLine = grep(pattern = "^Name = ",x = lines[1:startFixedInputs], value = TRUE)
  Name = strsplit(x = nameLine, split = " = ", fixed = TRUE)[[1]][2]
  photosLine = grep(pattern = "^PhotosType = ",x = lines[1:startFixedInputs], value = TRUE)
  PhotosType = strsplit(x = photosLine, split = " = ", fixed = TRUE)[[1]][2]
  curveLine = grep(pattern = "^CurveType = ",x = lines[1:startFixedInputs], value = TRUE)
  CurveType = strsplit(x = curveLine, split = " = ", fixed = TRUE)[[1]][2]
  
  # Read the tables
  Outputs = read.csv(file = file, header = TRUE, skip = startOutputs)
  FixedInputs = read.csv(file = file, header = TRUE, skip = startFixedInputs, nrows = lengthFixedInputs)
  VariableInputs = read.csv(file = file, header = TRUE, skip = startVariableInputs, nrows = lengthVariableInputs)
  Inputs = vector(mode = "list", length = nrow(FixedInputs) + ncol(VariableInputs))
  n = nrow(FixedInputs)
  for(i in 1:n) {
    Inputs[[i]] = FixedInputs[i,"Value"]
  }
  for(i in 1:ncol(VariableInputs)) {
    Inputs[[i + n]] = VariableInputs[,i]
  }
  names(Inputs) =  c(FixedInputs$Input, colnames(VariableInputs))
  
  return(list(
    Name = Name,
    PhotosType = PhotosType,
    CurveType = CurveType,
    Inputs = Inputs,
    Outputs = Outputs
  ))
}

