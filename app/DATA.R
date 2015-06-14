source("features.R")
library(ggplot2)

DATA <- function(my.df = 0)
{
        data <- list(
            raw.data = my.df,
            feature.data = 0,
            feature.matrix = 0,
            train.index = 0,
            test.index = 0,
            feature.pca.matrix = 0,
            pca = 0
       )

        ## Set the name for the class
        class(data) <- append(class(data),"DATA")
        return(data)
}

add.label <- function(df, label){
	data.frame(df, list(l = rep(strsplit(label, "_")[[1]][5], nrow(df))))
}

set_raw_dataa <- function(ob, ...) UseMethod("set_raw_dataa")
set_raw_dataa.DATA<- function(ob, dir,  ...){ 
    args <- list(...)
    object <- ob
    setwd(dir)

    tmp.list <- list()
    gesture.tmp <- c() 
    counter <- 1
    for (file in list.files()){
        tmp1 <- read.table(gsub("/", "_", file), header = TRUE, sep = ",")
        print(ncol(tmp1))
        tmp.list[[file]] <- as.matrix(tmp1[,3:10])
    }

    setwd("../..")
    object$raw.data <- tmp.list
    return(object)
}

get_gestures <- function(ob, ...) UseMethod("get_gestures")
get_gestures.DATA <- function(ob, ...){
    args <- list(...)
    object <- ob
    names(object$raw.data)
}

set_feature_data <- function(ob, window, slide, ...) UseMethod("set_feature_data")
set_feature_data.DATA <- function(ob, window, slide, ...){
    args <- list(...)
    object <- ob

    tmp.list <- list() 
    for (i in names(object$raw.data)){
        tmp.list[[i]] <- feature_matrix(object$raw.data[[i]], window, slide) 
    }
    object$feature.data <- tmp.list
    return(object)
}

set_feature_matrix <- function(ob,...) UseMethod("set_feature_matrix")
set_feature_matrix.DATA  <- function(ob, ...){
    args <- list(...)
    object <- ob

    object$feature.matrix <-dfs_label(object$feature.data) 
    return(object)
}

set.pca <- function(ob, ...) UseMethod("set.pca")
set.pca.DATA <- function(ob, ...) {
    args <- list(...)
    object <- ob

    d <- object$feature.matrix
    
    x.train <- d[, !names(d) %in% c("l")]
    pr.d <- prcomp(x.train, scale = TRUE)
    x.train <- predict(pr.d, x.train)
    x.train <- round(x.train, 4)
    object$feature.pca.matrix <- cbind(as.data.frame(x.train),"l"= d$l)
    return(object)
}

get.pca <- function(ob, ...) UseMethod("get.pca")
get.pca.DATA <- function(ob, ...) {
    args <- list(...)
    object <- ob

    d <- object$feature.matrix
    
    x.train <- d[, !names(d) %in% c("l")]
    pr.d <- prcomp(x.train, scale = TRUE)
    object$pca <- pr.d
    return(object)
}

# ---------
#
# Plot time series with facets being different columns.

plot_data <- function(...) UseMethod("plot_data")
plot_data.DATA <- function(...){
    args <- list(...)
    object  <- args[["object"]]
    type <- args[["type"]]
    number <- args[["number"]]
    cols <- args[["columns"]]
   
    if (type == "raw"){
        data <- object$raw.data[[number]]
    } else if (type == "feature"){
        data <- object$feature.data[[number]]
    }

    data <- data.frame(data[,cols], list( time = 1:nrow(data)))
    p <- ggplot(melt(data, id = "time"), aes(x=time, y = value, color = variable))
    p <- p + geom_line()
    p <- p + facet_grid(variable ~ . , scales = "free_y")
    p <- p + theme_bw()
    p <- p +  theme(axis.title.x = element_text(size = 12, vjust = .25), 
                    strip.text.x = element_blank(),
                    strip.background = element_blank(),
                    strip.text.y = element_text(colour = "black", angle = 0, size = 15),
                    legend.position="none")
    return(p)
}

# ----------
#
# Box plot comparing gestures tdfs

boxplot_data <- function(...) UseMethod("boxplot_data")
boxplot_data.DATA <- function(...){
    args <- list(...)
    object  <- args[["object"]]
    type <- args[["type"]]
    cols <- args[["columns"]]
    #file.name <- args[["file.name"]]
    if (type != "pca") {
        colnames(object$feature.matrix) <- c(paste0(rep("EMG Signal ",32), rep(1:8,each=4)),"l")
        x <-melt.df(data = object$feature.matrix, columns = cols)
    } else {
        print("right")
        x <-melt.df(data = object$feature.pca.matrix, columns = cols)
    }
    #png(file.name)
    print("yay")
    p <- ggplot(x, aes(factor(l), y = (value- mean(value))/sd(value) ))#,color = l))
    p <- p + geom_boxplot() + facet_grid(. ~ variable)
    #p <- p + geom_jitter(aes(alpha =.2))
    p <- p + theme_bw()
    p <- p +  theme(axis.title.x = element_text(size = 12, angle = 0, vjust = .25), 
                    axis.text.x = element_text(angle = -90, vjust = .5),
                    #strip.text.x = element_blank(),
                    strip.background = element_blank(),
                    strip.text.y = element_text(colour = "black", angle = 0, size = 15),
                    legend.position="none")
    p <- p + labs(#title = "4-TDFs By Signal Over Gestures",
             x = "Gesture",
             y = "Value")
    p
}

run.plots <- function(cols, t, ...) {
    d <- DATA()
    d <- set_raw_data(object = d, directory = "folds")
    d <- set_feature_data(object = d, window = 10, slide = 2)
    d <- set_feature_matrix(object = d)
    d <- set.pca(object = d)

    boxplot_data(object = d, columns =cols, type = t)


}

rf.fit <- function(...) UseMethod("rf.fit")
rf.fit.DATA <- function(...) {
    args <- list(...)
    object <- args[["object"]]

    tree <-rpart(l ~ ., data = object$feature.matrix)

    plot(tree, uniform = TRUE)
    text(fit, all = TRUE, cex = .9)


    }

