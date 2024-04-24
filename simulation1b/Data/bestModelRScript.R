#########################################
##                                     ##
## SCRIPT TO DETERMINE BEST SIMULATION ##
##                                     ##
#########################################

#################
# OBJECT SCRIPT #
#################
# folder_path = "C:/Users/Deon T. Benton/Documents/projects/spelkeSaxeSimulations/postMS/final/JEPGeneral/revision1/simulations/spelkePhillipsWoodward1995/SRN/7mosModelData/Object"
folder_path = "C:/Users/bentod2/Documents/projects/current/spelkeSaxeSimulations/postMS/final/JEPGeneral/revision1/simulations/spelkePhillipsWoodward1995/SRN/3Contact3NoContactTestEvents/7mosModelData/Object"

files = list.files(path = folder_path, pattern = ".txt", full.names = TRUE)

# Initialize an empty dataframe to store the results
results_df1 <- data.frame(FileName = character(),
                         PValue = numeric(),
                         MeanCollision = numeric(),
                         MeanNoCollision = numeric(),
                         stringsAsFactors = FALSE)

for (file in files) {
  options(scipen=999)
  file_name = basename(file)
  
  data = read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  data$ID = rep(1:20, each = 40)
  data$event = rep(c("Collision", "No Collision"), each = 2, times = 200)
  data$event = as.factor(data$event)
  
  data$looking_time = data$V3
  data = data[,-c(1:3)]
  
  test = t.test(data$looking_time[data$event=="Collision"],
                data$looking_time[data$event=="No Collision"], 
                paired = TRUE)
  
  # Calculate p-value and mean values
  p_value = test$p.value
  mean_collision = mean(data$looking_time[data$event=="Collision"])
  mean_no_collision = mean(data$looking_time[data$event=="No Collision"])
  
  # Append results to the dataframe
  results_df1 <- rbind(results_df1, data.frame(FileName = file_name, 
                                             PValue = p_value,
                                             MeanCollision = mean_collision,
                                             MeanNoCollision = mean_no_collision))
}

# Sort the dataframe by PValue in ascending order
results_df1 <- results_df1[order(results_df1$PValue), ]

# Select the top three simulations based on the smallest p-values
top_three <- head(results_df1, 3)

# Print the top three results
cat("Top Three Simulations:\n")
print(top_three)

# Print the overall best simulation based on the smallest p-value
best_simulation <- top_three[1, ]
cat("Best File:", best_simulation$FileName, "\n")
cat("Min p-value:", best_simulation$PValue, "\n")
cat("Mean for Collision:", best_simulation$MeanCollision, "\n")
cat("Mean for No Collision:", best_simulation$MeanNoCollision, "\n")

# best file:
# 7mosObject80060.030.060.00025.txt
# OR: 7mosObject60060.070.080.0001.txt

#################
# PEOPLE SCRIPT #
#################
# write a script to find the data with largest p-value
# folder_path = "C:/Users/Deon T. Benton/Documents/projects/spelkeSaxeSimulations/postMS/final/JEPGeneral/revision1/simulations/spelkePhillipsWoodward1995/SRN/7mosModelData/People"
folder_path = "C:/Users/bentod2/Documents/projects/current/spelkeSaxeSimulations/postMS/final/JEPGeneral/revision1/simulations/spelkePhillipsWoodward1995/SRN/3Contact3NoContactTestEvents/7mosModelData/People"

files = list.files(path = folder_path, pattern = ".txt", full.names = TRUE)

# Initialize an empty dataframe to store the results
results_df2 <- data.frame(FileName = character(),
                          PValue = numeric(),
                          MeanCollision = numeric(),
                          MeanNoCollision = numeric(),
                          stringsAsFactors = FALSE)

for (file in files) {
  options(scipen=999)
  file_name = basename(file)
  
  data = read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  data$ID = rep(1:20, each = 40)
  data$event = rep(c("Collision", "No Collision"), each = 2, times = 200)
  data$event = as.factor(data$event)
  
  data$looking_time = data$V3
  data = data[,-c(1:3)]
  
  test = t.test(data$looking_time[data$event=="Collision"],
                data$looking_time[data$event=="No Collision"], 
                paired = TRUE)
  
  # Calculate p-value and mean values
  p_value = test$p.value
  mean_collision = mean(data$looking_time[data$event=="Collision"])
  mean_no_collision = mean(data$looking_time[data$event=="No Collision"])
  
  # Append results to the dataframe
  results_df2 <- rbind(results_df2, data.frame(FileName = file_name, 
                                               PValue = p_value,
                                               MeanCollision = mean_collision,
                                               MeanNoCollision = mean_no_collision))
}

# Sort the dataframe by PValue in ascending order
results_df2 <- results_df2[order(results_df2$PValue, decreasing = TRUE), ]

# Select the top three simulations based on the smallest p-values
top_three <- head(results_df2, 3)

# Print the top three results
cat("Top Three Simulations:\n")
print(top_three)

# Print the overall best simulation based on the smallest p-value
best_simulation <- top_three[1, ]
cat("Best File:", best_simulation$FileName, "\n")
cat("Min p-value:", best_simulation$PValue, "\n")
cat("Mean for Collision:", best_simulation$MeanCollision, "\n")
cat("Mean for No Collision:", best_simulation$MeanNoCollision, "\n")



#######################
# PEOPLEOBJECT SCRIPT #
#######################
# write a script to find the data with largest p-value
folder_path = "C:/Users/Deon T. Benton/Documents/projects/spelkeSaxeSimulations/postMS/final/JEPGeneral/revision1/simulations/spelkePhillipsWoodward1995/SRN/7mosModelData/PeopleObject"

files = list.files(path = folder_path, pattern = ".txt", full.names = TRUE)

# Initialize an empty dataframe to store the results
results_df3 <- data.frame(FileName = character(),
                          PValue = numeric(),
                          MeanCollision = numeric(),
                          MeanNoCollision = numeric(),
                          stringsAsFactors = FALSE)

for (file in files) {
  options(scipen=999)
  file_name = basename(file)
  
  data = read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  data$ID = rep(1:20, each = 6)
  data$event = rep(c("Collision", "No Collision"), each = 3, times = 20)
  data$event = as.factor(data$event)
  
  data$looking_time = data$V3
  data = data[,-c(1:3)]
  
  test = t.test(data$looking_time[data$event=="Collision"],
                data$looking_time[data$event=="No Collision"], 
                paired = TRUE)
  
  # Calculate p-value and mean values
  p_value = test$p.value
  mean_collision = mean(data$looking_time[data$event=="Collision"])
  mean_no_collision = mean(data$looking_time[data$event=="No Collision"])
  
  # Append results to the dataframe
  results_df3 <- rbind(results_df3, data.frame(FileName = file_name, 
                                               PValue = p_value,
                                               MeanCollision = mean_collision,
                                               MeanNoCollision = mean_no_collision))
}

# Sort the dataframe by PValue in ascending order
results_df3 <- results_df3[order(results_df3$PValue), ]

# Select the top three simulations based on the smallest p-values
top_three <- head(results_df3, 3)

# Print the top three results
cat("Top Three Simulations:\n")
print(top_three)

# Print the overall best simulation based on the smallest p-value
best_simulation <- top_three[1, ]
cat("Best File:", best_simulation$FileName, "\n")
cat("Min p-value:", best_simulation$PValue, "\n")
cat("Mean for Collision:", best_simulation$MeanCollision, "\n")
cat("Mean for No Collision:", best_simulation$MeanNoCollision, "\n")


########################
# OBJECTSPEOPLE SCRIPT #
########################
# write a script to find the data with largest p-value
# folder_path = "C:/Users/Deon T. Benton/Documents/projects/spelkeSaxeSimulations/postMS/final/JEPGeneral/revision1/simulations/spelkePhillipsWoodward1995/SRN/7mosModelData/ObjectPeople"
folder_path = "C:/Users/bentod2/Documents/projects/current/spelkeSaxeSimulations/postMS/final/JEPGeneral/revision1/simulations/spelkePhillipsWoodward1995/SRN/3Contact3NoContactTestEvents/7mosModelData/ObjectPeople"


files = list.files(path = folder_path, pattern = ".txt", full.names = TRUE)

# Initialize an empty dataframe to store the results
results_df4 <- data.frame(FileName = character(),
                          PValue = numeric(),
                          MeanCollision = numeric(),
                          MeanNoCollision = numeric(),
                          stringsAsFactors = FALSE)

for (file in files) {
  options(scipen=999)
  file_name = basename(file)
  
  data = read.table(file, header = FALSE, stringsAsFactors = FALSE)
  
  data$ID = rep(1:20, each = 40)
  data$event = rep(c("Collision", "No Collision"), each = 2, times = 200)
  data$event = as.factor(data$event)
  
  data$looking_time = data$V3
  data = data[,-c(1:3)]
  
  test = t.test(data$looking_time[data$event=="Collision"],
                data$looking_time[data$event=="No Collision"], 
                paired = TRUE)
  
  # Calculate p-value and mean values
  p_value = test$p.value
  mean_collision = mean(data$looking_time[data$event=="Collision"])
  mean_no_collision = mean(data$looking_time[data$event=="No Collision"])
  
  # Append results to the dataframe
  results_df4 <- rbind(results_df4, data.frame(FileName = file_name, 
                                               PValue = p_value,
                                               MeanCollision = mean_collision,
                                               MeanNoCollision = mean_no_collision))
}

# Sort the dataframe by PValue in ascending order
results_df4 <- results_df4[order(results_df4$PValue, decreasing = TRUE), ]

# Select the top three simulations based on the smallest p-values
top_three <- head(results_df4, 3)

# Print the top three results
cat("Top Three Simulations:\n")
print(top_three)

# Print the overall best simulation based on the smallest p-value
best_simulation <- top_three[1, ]
cat("Best File:", best_simulation$FileName, "\n")
cat("Min p-value:", best_simulation$PValue, "\n")
cat("Mean for Collision:", best_simulation$MeanCollision, "\n")
cat("Mean for No Collision:", best_simulation$MeanNoCollision, "\n")

## combine dataframes and then save to csv
combine_df = rbind(results_df2,results_df4)
#write.csv(combine_df, "C:\\Users\\Deon T. Benton\\Desktop\\spelke_bm_results.csv", row.names=FALSE)
write.csv(combine_df, "C:\\Users\\bentod2\\Desktop\\spelke_people_results.csv", row.names=FALSE)

