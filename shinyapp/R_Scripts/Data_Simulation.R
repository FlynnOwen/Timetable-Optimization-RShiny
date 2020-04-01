###### Simulating data for the timetable App #######

# Creating the matrix for storing values
student <- matrix(nrow = 250, ncol = 4)

# Sampling random values between 1 and 50
for(i in 1:250){
  student[i,] <- sample(1:30,4,replace = FALSE, prob = c(0.2,0.15,0.03,0.03,0.02,0.015,0.05,0.015,0.01,0.04,0.01,0.03,0.015,0.015,0.015,0.04,0.01,0.01,0.01,0.01,0.01,0.015,0.02,0.015,0.05,0.02,0.03,0.015,0.02,0.08))  
}

# Renaming the simluated values to be classes
for (i in 1:nrow(student)){
  for (j in 1:ncol(student)){
    if(student[i,j] == 1){
      student[i,j] <- "Math"
    }
    else if(student[i,j] == 2){
      student[i,j] <- "English"
    }
    else if(student[i,j] == 3){
      student[i,j] <- "Economics"
    }
    else if(student[i,j] == 4){
      student[i,j] <- "Business"
    }
    else if(student[i,j] == 5){
      student[i,j] <- "P.E"
    }
    else if(student[i,j] == 6){
      student[i,j] <- "Drama"
    }
    else if(student[i,j] == 7){
      student[i,j] <- "Statistics"
    }
    else if(student[i,j] == 8){
      student[i,j] <- "Geology"
    }
    else if(student[i,j] == 9){
      student[i,j] <- "Accounting"
    }
    else if(student[i,j] == 10){
      student[i,j] <- "Media Studies"
    }
    else if(student[i,j] == 11){
      student[i,j] <- "Psychology"
    }
    else if(student[i,j] == 12){
      student[i,j] <- "Chemistry"
    }
    else if(student[i,j] == 13){
      student[i,j] <- "Physics"
    }
    else if(student[i,j] == 14){
      student[i,j] <- "Biology"
    }
    else if(student[i,j] == 15){
      student[i,j] <- "Politics"
    }
    else if(student[i,j] == 16){
      student[i,j] <- "History"
    }
    else if(student[i,j] == 17){
      student[i,j] <- "Classics"
    }
    else if(student[i,j] == 18){
      student[i,j] <- "Painting"
    }
    else if(student[i,j] == 19){
      student[i,j] <- "Drawing"
    }
    else if(student[i,j] == 20){
      student[i,j] <- "Design"
    }
    else if(student[i,j] == 21){
      student[i,j] <- "Woodwork"
    }
    else if(student[i,j] == 22){
      student[i,j] <- "Mechanics"
    }
    else if(student[i,j] == 23){
      student[i,j] <- "Laws"
    }
    else if(student[i,j] == 24){
      student[i,j] <- "Cooking"
    }
    else if(student[i,j] == 25){
      student[i,j] <- "Maori"
    }
    else if(student[i,j] == 26){
      student[i,j] <- "Japanese"
    }
    else if(student[i,j] == 27){
      student[i,j] <- "French"
    }
    else if(student[i,j] == 28){
      student[i,j] <- "Spanish"
    }
    else if(student[i,j] == 29){
      student[i,j] <- "Engineering"
    }
    else {
      student[i,j] <- "IT"
    }
  }
}

# Creating a dataframe and adding an ID variable
student <- as.data.frame(student)
write.csv(student, file = "Timetable_data2")

######### Simulating data for a smaller highschool ############

# Creating the matrix for storing values
student1 <- matrix(nrow = 130, ncol = 4)

# Sampling random values between 1 and 50
for(i in 1:130){
  student1[i,] <- sample(1:20,4,replace = FALSE, prob = c(0.20,0.20,0.03,0.02,0.03,0.01,0.08,0.03,0.02,0.02,0.025,0.05,0.035,0.04,0.02,0.05,0.03,0.04,0.05,0.02))  
}

# Renaming the simluated values to be classes
for (i in 1:nrow(student1)){
  for (j in 1:ncol(student1)){
    if(student1[i,j] == 1){
      student1[i,j] <- "Math"
    }
    else if(student1[i,j] == 2){
      student1[i,j] <- "English"
    }
    else if(student1[i,j] == 3){
      student1[i,j] <- "Economics"
    }
    else if(student1[i,j] == 4){
      student1[i,j] <- "Business"
    }
    else if(student1[i,j] == 5){
      student1[i,j] <- "P.E"
    }
    else if(student1[i,j] == 6){
      student1[i,j] <- "Drama"
    }
    else if(student1[i,j] == 7){
      student1[i,j] <- "Statistics"
    }
    else if(student1[i,j] == 8){
      student1[i,j] <- "Geology"
    }
    else if(student1[i,j] == 9){
      student1[i,j] <- "Accounting"
    }
    else if(student1[i,j] == 10){
      student1[i,j] <- "Media Studies"
    }
    else if(student1[i,j] == 11){
      student1[i,j] <- "Psychology"
    }
    else if(student1[i,j] == 12){
      student1[i,j] <- "Chemistry"
    }
    else if(student1[i,j] == 13){
      student1[i,j] <- "Physics"
    }
    else if(student1[i,j] == 14){
      student1[i,j] <- "Biology"
    }
    else if(student1[i,j] == 15){
      student1[i,j] <- "Politics"
    }
    else if(student1[i,j] == 16){
      student1[i,j] <- "History"
    }
    else if(student1[i,j] == 17){
      student1[i,j] <- "Classics"
    }
    else if(student1[i,j] == 18){
      student1[i,j] <- "Painting"
    }
    else if(student1[i,j] == 19){
      student1[i,j] <- "Drawing"
    }
    else {
      student1[i,j] <- "Design"
  }
  }
}

# Creating a dataframe and adding an ID variable
student1 <- as.data.frame(student1)

DataDir <- '../Data'
save(student1, file = file.path(DataDir,"Timetable_data_small.csv"))

##########################################################

