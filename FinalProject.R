
library(dimRed)
library(ggplot2)
library(Rtsne)
library(RANN)
library(Iso)
library(tidyverse)
library(RSpectra)
library(igraph)
library(palmerpenguins)
library(caret)
library(nnet)
library(MLmetrics)

data <-penguins_raw
data <- data |> select(Species,Island,`Culmen Length (mm)`,`Culmen Depth (mm)`,`Flipper Length (mm)`,`Body Mass (g)`,Sex)
data <- data |> drop_na()
data <- data |> mutate(Species = str_replace(Species, " .*",""))

AdeliePenguin <- data |> filter(Species == "Adelie") |> select(-Species)

ChinstrapPenguin <- data |> filter(Species == "Chinstrap") |> select(-Species)

GentooPenguin <- data |> filter(Species == "Gentoo") |> select(-Species)

#Body Mass by Species

ggplot() + geom_dotplot(aes(data$`Body Mass (g)`, fill = data$Species), binwidth = 125) +
  labs(x ="Penguin Mass (G)", fill = "Species")

#breaking up penguins by gender
AdeliePenguinFemale <- AdeliePenguin |> filter(Sex == "FEMALE")
AdeliePenguinMale <- AdeliePenguin |> filter(Sex == "MALE")

ChinstrapPenguinFemale <- ChinstrapPenguin |> filter(Sex == "FEMALE")
ChinstrapPenguinMale <- ChinstrapPenguin |> filter(Sex == "MALE")

GentooPenguinFemale <- GentooPenguin |> filter(Sex == "FEMALE")
GentooPenguinMale <- GentooPenguin |> filter(Sex == "MALE")


#getting artifical Penguin :)
selectPenguin <- function(penguin){
  
  Island <- sample(penguin$Island,1)
  gender <- sample(penguin$`Sex`,1)
  
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


artificialPenguin <-function(penguin){
  pen <- selectPenguin(penguin)
  #penguinData <- bind_rows(penguinData,data.frame(pen))
  pen
}

#repilcated penguins to be put combined with other data
charToNum <- function(data){
  data <- data |> mutate(`Culmen Length (mm)` = as.double(`Culmen Length (mm)`)) |>
    mutate(`Culmen Depth (mm)` = as.double(`Culmen Depth (mm)`)) |>
    mutate(`Flipper Length (mm)` = as.double(`Flipper Length (mm)`)) |>
    mutate(`Body Mass (g)` = as.double(`Body Mass (g)`))
  
  data
}


#function to make artificial penguins
makeArtificialPenguin <- function(num,penguin){
  
  artPenData <- replicate(num,artificialPenguin(penguin))
  artPenData <- matrix(unlist(artPenData),ncol =6,byrow = TRUE)
  artPenData <- as_tibble(artPenData) |> set_names(c("Island","Culmen Length (mm)","Culmen Depth (mm)","Flipper Length (mm)","Body Mass (g)","Sex"))
  artPenData <- as.data.frame(artPenData)
  artPenData <- charToNum(artPenData)
  artPenData
  
}

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

#note: in the data there is no species so must add species back into data 
artificialData <- bind_rows(AdelieFemaleRepdata,AdelieMaleRepdata,GentooFemaleRepData,GentooMaleRepData,ChinstrapFemaleRepData,ChinstrapMaleRepData)
artificialData <- bind_rows(artificialData)

artificialData <- artificialData |> drop_na()
#Holy moly thats alot of data

artificialData |> ggplot() + geom_dotplot(aes(`Body Mass (g)`, fill = Species), binwidth = 50) +
  labs(x ="Penguin Mass (G)", fill = "Sex") + facet_grid(cols = vars(Sex))

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
table(data$Species, predictions)

Accuracy(data$Species, predictions)

Precision(data$Species, predictions)

F1_Score(data$Species, predictions)



#Original plot based on body mass and species
artificialData |> ggplot(aes(x = `Body Mass (g)`, y = Species, color = Species)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Penguin Species by Body Mass",
       x = "Body Mass (g)", y = "Species") 


# Add predictions to the dataset
artificialData$predictions <- predict(model, newdata = artificialData, type = "class")

# Plot actual vs predicted
artificialData |> ggplot(aes(x = `Culmen Length (mm)`, y = Species, color = predictions)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Predicted vs Actual Penguin Species",
       x = "Body Mass (g)", y = "Species",
       color = "Predicted Species")

#PREDICTION USING MULTINOMIAL LOGISTIC REGRESSION ON ORGINIAL DATA

model <- multinom(Species ~ `Body Mass (g)` + `Flipper Length (mm)` + `Culmen Depth (mm)` + `Culmen Length (mm)`, data = data)

predictions <- predict(model, newdata = artificialData, type = "class")

# Compare predictions with actual values
table(artificialData$Species, predictions)

Accuracy(artificialData$Species, predictions)

Precision(artificialData$Species, predictions)

F1_Score(artificialData$Species, predictions)



#Original plot based on body mass and species
data |> ggplot(aes(x = `Body Mass (g)`, y = Species, color = Species)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Penguin Species by Body Mass",
       x = "Body Mass (g)", y = "Species") 


# Add predictions to the dataset
data$predictions <- predict(model, newdata = data, type = "class")

# Plot actual vs predicted
data |> ggplot(aes(x = `Culmen Length (mm)`, y = Species, color = predictions)) +
  geom_jitter(width = 0.1, height = 0.1, size = 3) +
  labs(title = "Predicted vs Actual Penguin Species",
       x = "Body Mass (g)", y = "Species",
       color = "Predicted Species")


