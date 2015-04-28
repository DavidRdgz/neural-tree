source("tree.R")

iris.sample <- function(cut.off, ...){
    ########
    #
    # Prereqs: cut.off < .5
    #
    #######

    df <- data.prep(iris, 5)
    t <- Tree()
    t$grow(list(list("None",0, df)), cut.off)
    
    pred <- t$predict.nt(df[,1:4])
    actu <- df[,5]
    table(pred,actu)
}

car.sample <- function(cut.off, ...){
    ########
    #
    # Prereqs: e < .35
    #
    #######
    df <- data.prep(mtcars, 10)
    t <- Tree()
    t$grow(list(list("None",0, df)), cut.off)
    
    pred <- t$predict.nt(df[,!names(df) %in% c("l")])
    actu <- df$l
    table(pred,actu)
}

letter.sample <- function(cut.off, ...){
    ########
    #
    # Prereqs: e < .5 
    #
    #######
    library(RCurl)
    letter.file <- "https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data"
    letter.url  <- getURL(letter.file)
    letter.data <- read.csv(textConnection(letter.url), header = FALSE)
    letter.data <- data.prep(letter.data, 1)
    letter.data   <-droplevels(subset(letter.data, l %in% c("A","B","C","D") ))
    df <- letter.data
    t <- Tree()
    t$grow(list(list("None",0, df)), cut.off)
    
    pred <- t$predict.nt(df[,!names(df) %in% c("l")])
    actu <- df$l
    table(pred,actu)
}

data.prep <- function(df, col.for.label,  ...) {
    tmp <- df
    colnames(tmp)[col.for.label] <- "l"
    tmp$l <- factor(tmp$l)
    tmp
}
