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
    colnames(sizeData) <- c("Culmen Length (mm)","Culmen Depth (mm)","Flipper Length (mm)","Body Mass (g)")
    sizeData
   }
   
   penguinSize <- penAvg(10) #<- change number to get a different average of penguin data
                              # as N grows the penguin will be more average 
   penguinData <- data.frame(Island,penguinSize,gender)
   penguinData
}

#pulling penguin
test<- selectPenguin(AdeliePenguinFemale)

# This will add a penguin to a new data frame
penguinData <- data.frame(matrix(nrow =1,ncol=6))

artificalPenguin <-function(penguin){
    pen <- selectPenguin(penguin)
   #penguinData <- bind_rows(penguinData,data.frame(pen))
    pen
}
#repilcated penguins to be put combined with other data
repdata <- replicate(10,artificalPenguin(AdeliePenguin))
repdata <- matrix(unlist(repdata),ncol =6,byrow = TRUE)

repdata <- as_tibble(repdata) |> set_names(c("Island","Culmen Length (mm)","Culmen Depth (mm)","Flipper Length (mm)","Body Mass (g)","Sex"))

repdata <- as.data.frame(repdata)

artData<- bind_rows(AdeliePenguin,repdata)

#make columns numeric and stuff

