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

#gestures <- function(cut.off, ...) {
#    d <- read.csv("nt_train.csv")
#    dd <- read.csv("nt_test.csv")
#
#    d <- d[ , !apply(d==0,2,all)]
#    dd <- dd[ , !apply(dd==0,2,all)]
#
#    x.train <- d[, !names(d) %in% c("l")]
#    x.test <- dd[, !names(dd) %in% c("l")]
#    pr.d <- prcomp(x.train, scale = TRUE)
#    x.train <- predict(pr.d, x.train)
#    x.test <- predict(pr.d, x.test)
#    d <- cbind(as.data.frame(x.train),"l"= d$l)
#    dd <- cbind(as.data.frame(x.test),"l" =dd$l)
#
#    n <- nnet(l ~ ., data = d, size = 2)
#    pred <- predict(n, dd[, !names(dd) %in% c("l")], type = "class")
#    actu <- dd$l
#    table(pred, actu)
#
#
#    df <- d
#    t <- Tree()
#    t$grow(list(list("None",0, df)), .5)
#
#    pred <- t$predict.nt(df[,!names(df) %in% c("l")])
#    actu <- df$l
#    table(pred,actu)
#
#    df2 <- dd
#    pred2 <- t$predict.nt(df2[,!names(df2) %in% c("l")])
#    actu2 <- df2$l
#    ntree.score <- table(pred2,actu2)
#    sum(diag(ntree.score))/sum(ntree.score) 
#
#
#
#
#    rf.fit <- randomForest(l ~ ., data = df)
#    rf.pred <- predict(rf.fit, df2[, !names(df2) %in% c("l")], type="class")
#    rf.actu <- df2$l
#    rf.score <- table(rf.pred, rf.actu)
#    sum(diag(rf.score))/sum(rf.score)
#
#
#    rpart.fit <- rpart(l ~ ., data = df, method = "class")
#    rpart.pred <- predict(rpart.fit, df2[, !names(df2) %in% c("l")], type="class")
#    plot(rpart.fit, uniform = TRUE)
#    text(rpart.fit, use.n=TRUE,  cex=.8)
#
#
#}
#
#
#
#
#gestures.fold <- function(cut.off, ...) {
#
#    d <- read.csv("folds.csv")
#    d <- d[ , !apply(d==0,2,all)]
#
#    x.train <- d[, !names(d) %in% c("l")]
#    pr.d <- prcomp(x.train, scale = TRUE)
#    x.train <- predict(pr.d, x.train)
#    x.train <- round(x.train, 4)
#    d <- cbind(as.data.frame(x.train),"l"= d$l)
#
#    n <- nnet(l ~ ., data = d, size = 2)
#    pred <- predict(n, d[, !names(d) %in% c("l")], type = "class")
#    actu <- d$l
#    table(pred, actu)
#
#
#    flds <- createFolds(1:nrow(d), k = 10, list = TRUE, returnTrain = FALSE)
#    y <- 1:nrow(d)
#    test.train <- lapply(flds, function(x) list(test = x, train = setdiff(y,x)))
#
#    system.time(ntree.rez <- lapply(test.train, function(x) my.ntree(d[x$train,], d[x$test,])))
#
#    system.time(rf.rez <- lapply(test.train, function(x) my.rf(d[x$train,], d[x$test,])))
#    system.time(dt.rez <- lapply(test.train, function(x) my.dt(d[x$train,], d[x$test,])))
#
#
#}
#mean(c(8 , 9  , 8 , 9))
#my.ntree <- function(d, dd) {
#    df <- d
#    t <- Tree()
#    t$grow(list(list("None",0, df)), .7)
#    print(length(unlist(t$get.leaves())))
#    df2 <- dd
#    pred2 <- t$predict.nt(df2[,!names(df2) %in% c("l")])
#    actu2 <- df2$l
#    ntree.table <- table(pred2,actu2)
#    ntree.score <- sum(diag(ntree.table))/sum(ntree.table) 
#
#    list(ntree.score, ntree.table)
#}
#
#my.dt <- function(d, dd) {
#    df <- d
#    df2 <- dd
#    rf.fit <- rpart(l ~ ., data = df)
#    rf.pred <- predict(rf.fit, df2[, !names(df2) %in% c("l")], type="class")
#    rf.actu <- df2$l
#    rf.table <- table(rf.pred, rf.actu)
#    rf.score <- sum(diag(rf.table))/sum(rf.table)
#
#    list(rf.score, rf.table)
#}
#
#my.rf <- function(d, dd) {
#    df <- d
#    df2 <- dd
#    rf.fit <- randomForest(l ~ ., data = df, ntree = 300)
#    rf.pred <- predict(rf.fit, df2[, !names(df2) %in% c("l")], type="class")
#    rf.actu <- df2$l
#    rf.table <- table(rf.pred, rf.actu)
#    rf.score <- sum(diag(rf.table))/sum(rf.table)
#
#    list(rf.score, rf.table)
#}
#
#mean(unlist(lapply(ntree.rez, function(x) sum(diag(x[[2]]))/sum(x[[2]]))))
#
#
#example <- function(x) {
#
#    library("rgl")
#    N <- 3
#    M <- 29
#    A <- c(-4,2,-1) + matrix( rnorm(N*M,mean=0,sd=.1), M, N)/2
#    B <- c(-1,-1,-1) + matrix( rnorm(N*M,mean=0,sd=.5), M, N)/2 
#    C <- t(c(0,0,0) + t(matrix( rnorm(N*M,mean=0,sd=.75), M, N) /2))
#
#
#
#    x3 <- seq(-2,2, .05)
#    x1 <- x3^3
#    x2 <- x3^2-2
#    M1 <- length(x3)
#      
#    Z <- cbind(x1,x2,x3)
#    Shift <- matrix( rnorm(N*M1,mean=0,sd=.3), M1, N)/2
#    Z <- Z + setNames(Shift, names(Z))
#
#    #l <- c(rep("red", 10), rep("blue", 10), rep("green", 10))
#    l <- c(rep("red", M), rep("blue", length(x3)))
#
#    MM <-  rbind( C , setNames( Z , names( C ) ) )
#    #MM  <- rbind(A,B,C)
#    
#    MMl <- data.frame(MM, l)
#    MMl$l <- factor(MMl$l)
#
#    fit <- rpart(l ~ ., data = MMl, method= "class")
#    plot(fit, uniform=TRUE, main="Classification Tree for Example", margin = .2)
#    text(fit, use.n=FALSE, all=TRUE, cex=.9)
#
#    n.fit <- nnet(l ~ ., data = MMl, method = "class", size = 2, rang =.1)
#    n.fit <- randomForest(l ~ ., data = MMl)
#    x <- MM[,1]
#    y <- MM[,2]
#    z <- MM[,3]
#    plot3d(x,y,z, col=l, size=1, type = "s")
#
#    planes3d(0,1,0,)
#    a <- c(-5,-.569,2)
#    b <- c(5,-.569,2)
#    c <- c(5,-.569,-2)
#    d <- c(-5,-.569,-2)
#    m <- rbind(a,b,c,d)
#rgl.quads(m[,1],m[,2],m[,3], alpha = .5, color = "blue")
#
#    a <- c(1.024,2, 2)
#    b <- c(1.024,2, -2)
#    c <- c(1.024,-2, -2)
#    d <- c(1.024,-2,2)
#    m <- rbind(a,b,c,d)
#rgl.quads(m[,1],m[,2],m[,3], alpha = .5, color = "red")
#
#
#    a <- c(-1.294,2, 2)
#    b <- c(-1.294,2, -2)
#    c <- c(-1.294,-2, -2)
#    d <- c(-1.294,-2,2)
#    m <- rbind(a,b,c,d)
#rgl.quads(m[,1],m[,2],m[,3], alpha = .5, color = "green")
#    
#rgl.snapshot("rf-predictions-dots.png")
#
#tmp <- c()
#for (x in seq(-5,5,.25)) {
#   for (y in seq(-2,2,.25)) {
#        for (z in seq(-2,2,.25)) {
#            tmp <- rbind(tmp, c(x,y,z))
#        }
#   }
#}
#
#n.fit <- nnet(l ~ ., data = MMl, method = "class", size = 2, rang =.1)
#colnames(tmp) <- c("x1","x2","x3")
#pred <- predict(n.fit, tmp, type = "class")
#rgl.points( tmp[,1],tmp[,2],tmp[,3], col= pred, alpha =.4)
#
#plot3d(x1,x2,x3, col=l, size=1, type = "s")
#M  <- mesh(seq(-4,4),
#           seq(-3,3))
#u <-M$x;v <-M$y
#rgl.surface(u, v, sqrt((u/1)^2 + (v/1)^2) -1.5, alpha = .25, color = "red")
#
#
#    grid <- -4:4
#    surface3d(x=grid, y=grid,z = matrix(.5,ncol=length(grid),nrow=length(grid)),
#              col="grey", alpha=.2)
#    lines3d(x=grid,y=0,z=0, col="grey")
#    lines3d(x=0,y=grid,z=0, col="grey")
#    lines3d(x=0,y=0,z=grid, col="grey")
#
#            polygon3d(x, y, x + y)
#            shade3d( extrude3d(x, y), col = "red" )
#            planes3d(1,1,1, )
#}
#
#
#random <- function(x) {
#
#library(scatterplot3d) 
#attach(iris) 
#
#table(iris$Species)
#l <- c(rep("red", 50), rep("blue", 50), rep("green",50))
#
#s3d <-scatterplot3d(Sepal.Width, Petal.Width, Petal.Length, pch=16, color= l,
#                      type="h", main="3D Scatterplot")
#fit <- lm(mpg ~ wt+disp) 
#fit <- glm(mpg ~ wt+disp, ) 
#s3d$plane3d(fit)
#
#}
#
#
#
#x  <- sin(u)
#   y  <- sin(v)
#   z  <- sin(u + v)
#
