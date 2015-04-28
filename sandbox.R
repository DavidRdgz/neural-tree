s <- iris
colnames(s)[5] <- "l"


lvls      <- levels(s$l)
classj    <- lapply(lvls, function(x) s$l == x)

# fit neural net and get predictions per class
net <- nnet(l ~ . , data = s[sample(nrow(s)),], size =1, trace = FALSE)
r.pred <- lapply(classj, function(x) predict(net, s[x,], type = "class"))
l.pred <- lapply(classj, function(x) predict(net, s[!x,], type = "class"))

#evaluate which label gives best entropy score
e.r <- sapply(r.pred, function(x) entropy.empirical(table(x)))
e.l <- sapply(l.pred, function(x) entropy.empirical(table(x)))

#e.r <- sapply(Map("list",lvls, r.pred), function(x) entropy.empirical(table(x[[2]][ ,x[[1]]])))
#e.l <- sapply(Map("list",lvls, l.pred), function(x) entropy.empirical(table(x[[2]][ ,x[[1]]])))

r.p <- sapply(classj, function(x) sum(x)/length(x))
l.p <- sapply(classj, function(x) sum(!x)/length(x))

d.e <-  sort(l.p*e.l + r.p*e.r)

right.candidate <- names(d.e)[1]

pred <- predict(net,s, type = "class")
R <- droplevels(s[pred == right.candidate, ])
L <- droplevels(s[pred != right.candidate, ])

#R <- droplevels(s[s$l == right.candidate, ])
#L <- droplevels(s[s$l != right.candidate, ])


#binary classification stop condition

s <- droplevels(subset(s, l %in% c("setosa", "versicolor")))
lvls      <- levels(s$l)
classj    <- lapply(lvls, function(x) s$l == x)


# fit neural net and get predictions per class
net <- nnet(l ~ . , data = s[sample(nrow(s)),], size =1, trace = FALSE)
r.pred <- lapply(classj, function(x) predict(net, s[x,], type = "class"))
l.pred <- lapply(classj, function(x) predict(net, s[!x,], type = "class"))




#########
#
# Old stuff





# fit neural net and get predictions per class
net.tmp <- nnet(l ~ . , data = S[sample(nrow(S)),], size =1, trace = FALSE)
r.pred <- lapply(classj, function(x) round(predict(net.tmp, S[x,])))
l.pred <- lapply(classj, function(x) round(predict(net.tmp, S[!x,])))
print(head(r.pred[[1]]))

#evaluate which label gives best entropy score
e.r <- sapply(Map("list",lvls, r.pred), function(x) entropy.empirical(table(x[[2]][ ,x[[1]]])))
e.l <- sapply(Map("list",lvls, l.pred), function(x) entropy.empirical(table(x[[2]][ ,x[[1]]])))




#quick commands

unlist(lapply(t$tree, function(x) table(x$S$l)))
lapply(t$tree, function(x) entropy.empirical(table(x$S$l)))
lapply(t$tree, function(x) list(table(x$L$l),table(x$R$l)))


# net validation
library(car)
library(caret)
trainIndex <- createDataPartition(Prestige$income, p=.7, list=F)
prestige.train <- Prestige[trainIndex, ]
prestige.test <- Prestige[-trainIndex, ]

my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(1,2,3))
prestige.fit <- train(income ~ prestige + education, data = prestige.train,
                          method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)  

prestige.predict <- predict(prestige.fit, newdata = prestige.test)
prestige.rmse <- sqrt(mean((prestige.predict - prestige.test$income)^2)) 

