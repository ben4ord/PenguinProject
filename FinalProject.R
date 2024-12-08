
library(dimRed)
library(ggplot2)
library(Rtsne)
library(RANN)
library(Iso)
library(tidyverse)
library(RSpectra)
library(igraph)
library(palmerpenguins)

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


artificalPenguin <-function(penguin){
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

  artPenData <- replicate(num,artificalPenguin(penguin))
  artPenData <- matrix(unlist(artPenData),ncol =6,byrow = TRUE)
  artPenData <- as_tibble(artPenData) |> set_names(c("Island","Culmen Length (mm)","Culmen Depth (mm)","Flipper Length (mm)","Body Mass (g)","Sex"))
  artPenData <- as.data.frame(artPenData)
  artPenData <- charToNum(artPenData)
  artPenData
  
}

AdelineFemaleRepdata <- makeArtificialPenguin(1000,AdeliePenguinFemale)
AdelineMaleRepdata <- makeArtificialPenguin(1000,AdeliePenguinMale)

ChinstrapFemaleRepData <-makeArtificialPenguin(1000,ChinstrapPenguinFemale)
ChinstrapMaleRepData <-makeArtificialPenguin(1000,ChinstrapPenguinMale)

GentooFemaleRepData <-makeArtificialPenguin(1000,GentooPenguinFemale)
GentooMaleRepData <-makeArtificialPenguin(1000,GentooPenguinMale)

#note: in the data there is no species so must add species back into data 
artificalData <- bind_rows(AdelineFemaleRepdata,AdeliePenguinMale,GentooFemaleRepData,GentooMaleRepData,ChinstrapFemaleRepData,ChinstrapMaleRepData)

#Holy moly thats alot of data
artificalData |> ggplot() + geom_dotplot(aes(`Body Mass (g)`, fill = Sex), binwidth = 20) +
  labs(x ="Penguin Mass (G)", fill = "Sex")

artificalData |> ggplot() + geom_boxplot(aes(`Body Mass (g)`, fill = Sex)) +
  labs(x ="Penguin Mass (G)", fill = "Sex")




