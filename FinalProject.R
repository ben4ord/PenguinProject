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
    sizeData
   }
   
   penguinSize <- penAvg(100) #<- change number to get a different average of penguin data
                              # as N grows the penguin will be more average 
   penguinData <- data.frame(Island,penguinSize,gender)
   penguinData
}

#pulling penguin
test<- selectPenguin(AdeliePenguinFemale)

artificalPenguin <-function(n,penguin){
  df <- data.frame(Island=character(),
                   `Culmen Length (mm)`=num(),
                   `Culmen Depth (mm)`=num(),
                   `Flipper Length (mm)`=num(),
                   `Body Mass (g)`=num(),
                   Sex=chrcharacter())
  
  
  for(p in 1:n){
    
    pen <- selectPenguin(penguin)
    df <- df |> add_row(pen)
    
  }
  
}

artDat <- artificalPenguin(10,AdeliePenguin)
