#########################################
##                                     ##
## SCRIPT TO DETERMINE BEST SIMULATION ##
##                                     ##
#########################################

#################
# OBJECT SCRIPT #
#################
# write a script to find the data with largest p-value
folder_path = "C:/Users/Deon T. Benton/Documents/projects/spelkeSaxeSimulations/postMS/final/JEPGeneral/revision1/simulations/spelkePhillipsWoodward1995/SRN/4mosModelData/Object"

files = list.files(path = folder_path, pattern = ".txt", full.names = TRUE)

min_pvalue = -Inf
best_file = ""
mean_same_side = NULL
mean_different_side = NULL

for (file in files) {
  options(scipen=999)
  file_name = basename(file)
  
  data = read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  data$ID = rep(1:32, each = 2)
  data$event = rep(c("Collision", "No Collision"), each = 1, times = 32)
  data$event = as.factor(data$event)
  
  data$looking_time = data$V3
  
  data = data[,-c(1:3)]
  
  test = t.test(data$looking_time[data$event=="Collision"],
                data$looking_time[data$event=="No Collision"], 
                paired = TRUE)
  
  # Calculate largest p-value
  res = test$p.value
  
  # Check if this is the best model so far
  if (res > min_pvalue) {
    min_pvalue = res
    best_file = file
    mean_collision = mean(data$looking_time[data$event=="Collision"])
    mean_no_collision = mean(data$looking_time[data$event=="No Collision"])
  }
}

# Print the best file
print(paste("Best file: ", best_file))

# Print the best model metrics
print(paste("min p-value: ", min_pvalue))

# Print the mean values for the best file
print(paste("Mean for Collision: ", mean_collision))
print(paste("Mean for No Collision: ", mean_no_collision))

# best file:
# 7mosObject80060.030.060.00025.txt
# OR: 7mosObject60060.070.080.0001.txt

#################
# PEOPLE SCRIPT #
#################
# write a script to find the data with largest p-value
folder_path = "C:/Users/Deon T. Benton/Documents/projects/spelkeSaxeSimulations/postMS/final/JEPGeneral/revision1/simulations/spelkePhillipsWoodward1995/SRN/7mosModelData/People"

files = list.files(path = folder_path, pattern = ".txt", full.names = TRUE)

max_pvalue = -Inf
best_file = ""
mean_same_side = NULL
mean_different_side = NULL

for (file in files) {
  options(scipen=999)
  file_name = basename(file)
  
  data = read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  data$ID = rep(1:32, each = 2)
  data$event = rep(c("Collision", "No Collision"), each = 1, times = 32)
  data$event = as.factor(data$event)
  
  data$looking_time = data$V3
  
  data = data[,-c(1:3)]
  
  test = t.test(data$looking_time[data$event=="Collision"],
                data$looking_time[data$event=="No Collision"], 
                paired = TRUE)
  
  # Calculate largest p-value
  res = test$p.value
  
  # Check if this is the best model so far
  if (res > max_pvalue) {
    max_pvalue = res
    best_file = file
    mean_collision = mean(data$looking_time[data$event=="Collision"])
    mean_no_collision = mean(data$looking_time[data$event=="No Collision"])
  }
}

# Print the best file
print(paste("Best file: ", best_file))

# Print the best model metrics
print(paste("max p-value: ", max_pvalue))

# Print the mean values for the best file
print(paste("Mean for Collision: ", mean_collision))
print(paste("Mean for No Collision: ", mean_no_collision))

# BEST MODEL: 7mosPeople60060.070.080.0001.txt

#######################
# PEOPLEOBJECT SCRIPT #
#######################
# write a script to find the data with largest p-value
folder_path = "C:/Users/bentod2/Documents/projects/current/spelkeSaxeSimulations/postMS/final/JEPGeneral/simulations/spelkePhillipsWoodward1995/SRN/7mosModelData/peopleObjects"

files = list.files(path = folder_path, pattern = ".txt", full.names = TRUE)

max_pvalue = Inf
best_file = ""
mean_same_side = NULL
mean_different_side = NULL

for (file in files) {
  options(scipen=999)
  file_name = basename(file)
  
  data = read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  data$ID = rep(1:32, each = 6)
  data$event = rep(c("Collision", "No Collision"), each = 3, times = 32)
  data$event = as.factor(data$event)
  
  data$looking_time = data$V3
  
  data = data[,-c(1:3)]
  
  test = t.test(data$looking_time[data$event=="Collision"],
                data$looking_time[data$event=="No Collision"], 
                paired = TRUE)
  
  # Calculate largest p-value
  res = test$p.value
  
  # Check if this is the best model so far
  if (res < max_pvalue) {
    max_pvalue = res
    best_file = file
    mean_collision = mean(data$looking_time[data$event=="Collision"])
    mean_no_collision = mean(data$looking_time[data$event=="No Collision"])
  }
}

# Print the best file
print(paste("Best file: ", best_file))

# Print the best model metrics
print(paste("min p-value: ", max_pvalue))

# Print the mean values for the best file
print(paste("Mean for Collision: ", mean_collision))
print(paste("Mean for No Collision: ", mean_no_collision))

# BEST MODEL: 7mosPeopleObject80040.030.080.0001.txt


########################
# OBJECTSPEOPLE SCRIPT #
########################
# write a script to find the data with largest p-value
folder_path = "C:/Users/bentod2/Documents/projects/current/spelkeSaxeSimulations/postMS/final/JEPGeneral/simulations/spelkePhillipsWoodward1995/SRN/7mosModelData/objectPeople"

files = list.files(path = folder_path, pattern = ".txt", full.names = TRUE)

max_pvalue = -Inf
best_file = ""
mean_same_side = NULL
mean_different_side = NULL

for (file in files) {
  options(scipen=999)
  file_name = basename(file)
  
  data = read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  data$ID = rep(1:32, each = 6)
  data$event = rep(c("Collision", "No Collision"), each = 3, times = 32)
  data$event = as.factor(data$event)
  
  data$looking_time = data$V3
  
  data = data[,-c(1:3)]
  
  test = t.test(data$looking_time[data$event=="Collision"],
                data$looking_time[data$event=="No Collision"], 
                paired = TRUE)
  
  # Calculate largest p-value
  res = test$p.value
  
  # Check if this is the best model so far
  if (res > max_pvalue) {
    max_pvalue = res
    best_file = file
    mean_collision = mean(data$looking_time[data$event=="Collision"])
    mean_no_collision = mean(data$looking_time[data$event=="No Collision"])
  }
}

# Print the best file
print(paste("Best file: ", best_file))

# Print the best model metrics
print(paste("min p-value: ", max_pvalue))

# Print the mean values for the best file
print(paste("Mean for Collision: ", mean_collision))
print(paste("Mean for No Collision: ", mean_no_collision))

# BEST MODEL: 7mosObjectPeople80040.050.040.0001.txt