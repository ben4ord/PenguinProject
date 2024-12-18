
library(ggplot2)
library(Rtsne)
library(RANN)
library(Iso)
library(tidyverse)
library(RSpectra)
library(palmerpenguins)
library(caret)
library(nnet)
library(MLmetrics)

data <-penguins_raw
#only choosing necessary data 
data <- data |> select(Species,Island,`Culmen Length (mm)`,`Culmen Depth (mm)`,`Flipper Length (mm)`,`Body Mass (g)`,Sex)

#cleaning data
data <- data |> drop_na()
data <- data |> mutate(Species = str_replace(Species, " .*",""))

#seperating species
AdeliePenguin <- data |> filter(Species == "Adelie") |> select(-Species)

ChinstrapPenguin <- data |> filter(Species == "Chinstrap") |> select(-Species)

GentooPenguin <- data |> filter(Species == "Gentoo") |> select(-Species)

#Body Mass by Species

data |> ggplot() + geom_dotplot(aes(`Body Mass (g)`, fill = Species), binwidth = 125) +
  labs(x ="Penguin Mass (G)", color = "Species") + 
  scale_fill_manual(values=c("#b2b2b2", "#7854ff", "#ffa53f"))

#breaking up penguins by gender
AdeliePenguinFemale <- AdeliePenguin |> filter(Sex == "FEMALE")
AdeliePenguinMale <- AdeliePenguin |> filter(Sex == "MALE")

ChinstrapPenguinFemale <- ChinstrapPenguin |> filter(Sex == "FEMALE")
ChinstrapPenguinMale <- ChinstrapPenguin |> filter(Sex == "MALE")

GentooPenguinFemale <- GentooPenguin |> filter(Sex == "FEMALE")
GentooPenguinMale <- GentooPenguin |> filter(Sex == "MALE")


#Creating artificial Penguin :)
#-------------------------------------------------------------------------------------
#Function [selectPenguin]

  #Purpose:   This function creates and artificial penguin by sampling from a data set
#             It allows us to create a new penguin with reasonable data points
  

#  Parameters:
#      penguin -- this penguin parater is what penguin you want to artifically create.
#                 So you send in all of the penguin combined or send threw seperated data
#                 by species and sex
#  Returns:   A "fake penguin" is returned to be added to a new data set
#-------------------------------------------------------------------------------------
artificialPenguin <- function(penguin){
  
  Island <- sample(penguin$Island,1)
  gender <- sample(penguin$`Sex`,1)
  
  # This nested function does all of the sampling for a penguin 
  # the N value is how many samples you want the greater the N value the more
  # normal a penguin is. I found a sample of 2-3 to be the best outcome
  # we used 2 for our project
  penAvg<- function(N){
    cLength <- sample(penguin$`Culmen Length (mm)`, N, replace = TRUE)
    cDepth  <- sample(penguin$`Culmen Depth (mm)`, N, replace = TRUE)
    fLength <- sample(penguin$`Flipper Length (mm)`, N, replace = TRUE)
    bMass <- sample(penguin$`Body Mass (g)`, N, replace = TRUE)
    
    cDepth <-  mean(cDepth)
    cLength <- mean(cLength)
    fLength <-  mean(fLength)
    bMass <- mean(bMass)
    
    sizeData <- data.frame(cLength,cDepth,fLength,bMass)
    colnames(sizeData) <- c("Culmen Length (mm)","Culmen Depth (mm)","Flipper Length (mm)","Body Mass (g)")
    sizeData
  }
  
  penguinSize <- penAvg(2) #<- change number to get a different average of penguin data
  # as N grows the penguin will be more average 
  penguinData <- data.frame(Island,penguinSize,gender)
  penguinData
}


#Fixing data types
charToNum <- function(data){
  data <- data |> mutate(`Culmen Length (mm)` = as.double(`Culmen Length (mm)`)) |>
    mutate(`Culmen Depth (mm)` = as.double(`Culmen Depth (mm)`)) |>
    mutate(`Flipper Length (mm)` = as.double(`Flipper Length (mm)`)) |>
    mutate(`Body Mass (g)` = as.double(`Body Mass (g)`))
  
  data
}


#-------------------------------------------------------------------------------------
#Function [makeArtifcalPenguin]

#Purpose:   This function replicated the desired number of selected penguins while turning it into a 
#           data frame and fixing structure and data types.


#  Parameters:
#      penguin -- this penguin parameter is what penguin you want to artificially create.
#                 So you send in all of the penguin combined or send threw separated data
#                 by species and sex

#      num--      The desired amount of artificial penguins 

#  Returns:   A data frame of as many penguins as you want :)
#-------------------------------------------------------------------------------------
makeArtificialPenguin <- function(num,penguin){
  
  artPenData <- replicate(num,artificialPenguin(penguin))
  artPenData <- matrix(unlist(artPenData),ncol =6,byrow = TRUE)
  artPenData <- as_tibble(artPenData) |> set_names(c("Island","Culmen Length (mm)",
                                                     "Culmen Depth (mm)","Flipper Length (mm)",
                                                     "Body Mass (g)","Sex"))
  artPenData <- as.data.frame(artPenData)
  artPenData <- charToNum(artPenData)
  artPenData
  
}


# creating fake penguins and adding species back into the data frame
AdelieFemaleRepdata <- makeArtificialPenguin(1000,AdeliePenguinFemale)
AdelieMaleRepdata <- makeArtificialPenguin(1000,AdeliePenguinMale)
AdelieMaleRepdata <- AdelieMaleRepdata |> mutate(Species = "Adelie")
AdelieFemaleRepdata <- AdelieFemaleRepdata |> mutate(Species = "Adelie")


ChinstrapFemaleRepData <-makeArtificialPenguin(1000,ChinstrapPenguinFemale)
ChinstrapMaleRepData <-makeArtificialPenguin(1000,ChinstrapPenguinMale)
ChinstrapFemaleRepData <- ChinstrapFemaleRepData |> mutate(Species = "Chinstrap")
ChinstrapMaleRepData <- ChinstrapMaleRepData |> mutate(Species = "Chinstrap")


GentooFemaleRepData <-makeArtificialPenguin(1000,GentooPenguinFemale)
GentooMaleRepData <-makeArtificialPenguin(1000,GentooPenguinMale)
GentooFemaleRepData <- GentooFemaleRepData |> mutate(Species = "Gentoo")
GentooMaleRepData <- GentooMaleRepData |> mutate(Species = "Gentoo")

#binding all penguins back together 
artificialData <- bind_rows(AdelieFemaleRepdata,AdelieMaleRepdata,GentooFemaleRepData,GentooMaleRepData,ChinstrapFemaleRepData,ChinstrapMaleRepData)
artificialData <- bind_rows(artificialData) 

artificialData <- artificialData |> drop_na()
#Holy moly thats alot of data

artificialData |> ggplot() + geom_dotplot(aes(`Body Mass (g)`, fill = Species), binwidth = 45) +
  labs(x ="Penguin Mass (G)", fill = "Sex") +  facet_grid(cols = vars(Sex))  + 
  scale_fill_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

artificialData |> ggplot() + geom_dotplot(aes(`Culmen Length (mm)`, fill = Species), binwidth = 0.34) +
  labs(x ="Penguin Beak Length (mm)", fill = "Sex") +  facet_grid(cols = vars(Sex))  + 
  scale_fill_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

artificialData |> ggplot() + geom_dotplot(aes(`Flipper Length (mm)`, fill = Species), binwidth = .6) +
  labs(x ="Flipper Length (mm)", fill = "Sex") +  facet_grid(cols = vars(Sex))  + 
  scale_fill_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

artificialData |> ggplot() + geom_boxplot(aes(`Body Mass (g)`, fill = Species)) +
  labs(x ="Penguin Mass (G)", fill = "Sex")+ facet_grid(cols = vars(Sex))

artificialData |> ggplot() + geom_density(aes(`Body Mass (g)`, fill = Species)) +
  labs(x ="Penguin Mass (G)", fill = "Sex")+ facet_grid(cols = vars(Sex))

artificialData |> ggplot() + geom_violin(aes(`Body Mass (g)`,`Culmen Length (mm)`, fill = Species)) +
  labs(x ="Penguin Mass (G)", fill = "Sex") +
  facet_grid(cols =vars(Sex))


summary(artificialData)


#PREDICTION USING MULTINOMIAL LOGISTIC REGRESSION ON ARTIFICIAL DATA

model <- multinom(Species ~ `Body Mass (g)` + `Flipper Length (mm)` + `Culmen Depth (mm)` + `Culmen Length (mm)`, data = artificialData)

predictions <- predict(model, newdata = data, type = "class")

# Compare predictions with actual values
#Creating a dataframe to store the table (shows the actual vs predicted outcomes)
art_table <- data.frame(table(data$Species, predictions)) |>
  rename(Actual = Var1)
art_table <- art_table |>
  pivot_wider(names_from = predictions, values_from = Freq, values_fill = 0)

art_table

#Finding the metrics for our prediction data
art_accuracy <- Accuracy(data$Species, predictions)

art_precision <- Precision(data$Species, predictions)

art_f1 <- F1_Score(data$Species, predictions)

#Creating a dataframe to store this data
art_metrics_df <- data.frame(
  Dataset = "Artificial Data",
  Accuracy = art_accuracy,
  Precision = art_precision,
  `F1 Score` = art_f1
)

#View the result
art_metrics_df

#Original plot based on body mass and species
artificialData |> ggplot(aes(x = `Body Mass (g)`, y = Species, color = Species)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Penguin Species by Body Mass",
       x = "Body Mass (g)", y = "Species") + 
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

#original plot bases on Culmen length of species
artificialData |> ggplot(aes(x = `Culmen Length (mm)`, y = Species, color = Species)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Penguin Species by Culmen Length",
       x = "Culmen Length (mm)", y = "Species") + 
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

#original plot bases on Flipper length of species
artificialData |> ggplot(aes(x = `Flipper Length (mm)`, y = Species, color = Species)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Penguin Species by Flipper Length",
       x = "Flipper Length (mm)", y = "Species") + 
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))


# Add predictions to the dataset
artificialData$predictions <- predict(model, newdata = artificialData, type = "class")

# Plot actual vs predicted
artificialData |> ggplot(aes(x = `Culmen Length (mm)`, y = Species, color = predictions)) +
  geom_jitter(width = 0.3, height = 0.3, size = .6) +
  labs(title = "Predicted vs Actual Penguin Species",
       x = "Culmen Length (mm)", y = "Species",
       color = "Predicted Species")  + 
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

artificialData |> ggplot(aes(x = `Flipper Length (mm)`, y = Species, color = predictions)) +
  geom_jitter(width = 0.3, height = 0.3, size = .6) +
  labs(title = "Predicted vs Actual Penguin Species",
       x = "Flipper Length (mm)", y = "Species",
       color = "Predicted Species")  + 
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

artificialData |> ggplot(aes(x = `Body Mass (g)`, y = Species, color = predictions)) +
  geom_jitter(width = 0.3, height = 0.3, size = .6) +
  labs(title = "Predicted vs Actual Penguin Species",
       x = "Body Mass (g)", y = "Species",
       color = "Predicted Species")  + 
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))


#PREDICTION USING MULTINOMIAL LOGISTIC REGRESSION ON ORGINIAL DATA

model <- multinom(Species ~ `Body Mass (g)` + `Flipper Length (mm)` + `Culmen Depth (mm)` + `Culmen Length (mm)`, data = data)

predictions <- predict(model, newdata = artificialData, type = "class")

# Compare predictions with actual values
#Creating a dataframe to store the table (shows the actual vs predicted outcomes)
orig_table <- data.frame(table(artificialData$Species, predictions)) |>
  rename(Actual = Var1)
orig_table <- orig_table |>
  pivot_wider(names_from = predictions, values_from = Freq, values_fill = 0)

orig_table

#Metrics for our predicted data
orig_accuracy <- Accuracy(artificialData$Species, predictions)

orig_precision <- Precision(artificialData$Species, predictions)

orig_f1 <- F1_Score(artificialData$Species, predictions)

#Creating a dataframe to store the metrics
orig_metrics_df <- data.frame(
  Dataset = "Real Data",
  Accuracy = orig_accuracy,
  Precision = orig_precision,
  `F1 Score` = orig_f1
)

# Add to the existing art_metrics data frame
combined_metrics_df <- bind_rows(art_metrics_df, orig_metrics_df)

# View result to compare results from the metrics
combined_metrics_df



#Original plot based on body mass and species
data |> ggplot(aes(x = `Body Mass (g)`, y = Species, color = Species)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Penguin Species by Body Mass",
       x = "Body Mass (g)", y = "Species") 


# Add predictions to the dataset
data$predictions <- predict(model, newdata = data, type = "class")

# Plot actual vs predicted
data |> ggplot(aes(x = `Body Mass (g)`, y = Species, color = predictions)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Predicted vs Actual Penguin Species",
       x = "Body Mass (g)", y = "Species",
       color = "Predicted Species")+  
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

data |> ggplot(aes(x = `Culmen Length (mm)`, y = Species, color = predictions)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Predicted vs Actual Penguin Species",
       x = "Culmen Length (mm)", y = "Species",
       color = "Predicted Species")+ 
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

data |> ggplot(aes(x = `Flipper Length (mm)`, y = Species, color = predictions)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Predicted vs Actual Penguin Species",
       x = "Flipper Length (mm)", y = "Species",
       color = "Predicted Species")+ 
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))



#PREDICTING BASED ON SEX
model <- multinom(Sex ~ `Body Mass (g)` + `Flipper Length (mm)` + `Culmen Depth (mm)` + `Culmen Length (mm)`, data = artificialData)

predictions <- predict(model, newdata = artificialData, type = "class")


Accuracy(artificialData$Sex, predictions)

Precision(artificialData$Sex, predictions)

F1_Score(artificialData$Sex, predictions)


#Original plot based on body mass and Sex
artificialData |> ggplot(aes(x = `Body Mass (g)`, y = Sex, color = Species)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Penguin Sex by Body Mass",
       x = "Body Mass (g)", y = "Sex",
       color = "Sex") +
  facet_grid(cols = vars(Species)) + 
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

#Plot actual vs predicted
artificialData |> ggplot(aes(x = `Body Mass (g)`, y = Sex, color = predictions)) +
  geom_jitter(width = 0.3, height = 0.3, size = 1) +
  labs(title = "Predicted vs Actual Penguin Sex",
       x = "Body Mass (g)", y = "Sex",
       color = "Predicted Sex") +
  facet_grid(cols = vars(Species)) + 
  scale_color_manual(values=c("#ff3131", "#7854ff", "#ffa53f"))

