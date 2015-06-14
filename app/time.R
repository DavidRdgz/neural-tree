source("DATA.R")
library(shiny)
library(DT)
library(influxdb)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(randomForest)
library(rpart)


Subject <- function(feature.matrix = 0)
{
        data <- list(
                    feature.matrix = 0,
                    feature.pca.matrix = 0,
                    pca = 0,
                    model = 0
                    #sample.record.raw = 0,
                    #sample.record.feature.matrix = 0
    )

        ## Set the name for the class
        class(data) <- append(class(data),"Subject")
        return(data)
}

s <- Subject()

model.selection <- list("Decision Tree" = 1, "Random Forest" =2,
                         "Neural Tree" = 3)

model <- function(model.n, d, ...) {
    if (model.n == 1) {
        m = rpart(l ~ ., data =d)
    } else if (model.n == 2) {
        m = randomForest(l~ ., data =d)
    } else if (model == 3) {
        m = "nt.fit"
    }
    m
}

color.plot <- function(results, yy ) {
        d <- as.data.frame(results)
        m <- melt(d, id.vars=c("emg.time","emg.sequence_number"))

        #Create a custom color scale
        myColors <- brewer.pal(8,"Set3")
        names(myColors) <- sort(levels(m$variable))
        colScale <- scale_colour_manual(name = "variable",values = myColors)

        p <- ggplot(m, aes(x = emg.sequence_number, y = value, color = variable)) + geom_line() 
        p <- p +  ylim(-yy, yy) + colScale
        p + theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())
          #p + facet_wrap(  ~ variable, ncol = 1)
}

runApp(list(
  ui = pageWithSidebar(    

  headerPanel("Classifying Gestures"),

  sidebarPanel(
    #h3("Monitor"),
    sliderInput("ylim", 
                "Y-axis height", 
                min = 1,
                max = 200, 
                value = 100),
    tabsetPanel(id = "tabs1", selected = 1,
        tabPanel("Recording", value = 1,
            br(),
            sliderInput("window", 
                        "Recording Time", 
                        min = 1,
                        max = 100, 
                        value = 50),
            textInput("fileToRecord", "Subject's Name", "Test"),
            textInput("gestureToRecord", "My Gesture", "HandClosing"),
            actionButton("recordButton", "Record Gesture"),
            br(),
            br(),
            textOutput("nFileToRecord")
        ),
        tabPanel("Predicting", value = 2,
            br(),
            selectInput("subject", label = "Select Model Subject", 
                        choices = Map(function(x,y) y,list.files("data"), 1:length(list.files("data"))), 
                        selected = match("Test", list.files("data"))),
            selectInput("model", label = "Select Model", 
                        choices = model.selection, 
                        selected = 2),
            sliderInput("windowPredict", 
                        "Window Size", 
                        min = 1,
                        max = 100, 
                        value = 10),
            sliderInput("slidePredict", 
                        "Slide Size", 
                        min = 1,
                        max = 30, 
                        value = 2),
            actionButton("trainModelButton", "Train Model"),
            br(),
            br(),
            textInput("gestureToPredict", "My Gesture", "HandClosing"),
            actionButton("predictButton", "Predict Gesture")
        )
    )
 ),

  mainPanel(        plotOutput("plot"),
    h3("Real-Time EMG Reading"),
    tabsetPanel(id = "tabs2", selected = 1,
        tabPanel("Recording Feedback", value = 1,
            br(),
            textOutput('descriptionRecord'),
            br(),
            verbatimTextOutput("gestureToRecord"),
            h3("Window of Signals Recorded"),
            plotOutput("gestureRecordedPlot"),
            h3("Boxplot of Signals"),
            plotOutput("gesturePlot"),
            h3("Time Domain Features from Signals"),
            br(),
            DT::dataTableOutput("gestureFeatureTable")
            #textOutput('fileToRecord')
        ),
        tabPanel("Prediction Feedback", value = 2,
            br(),
            br(),
            textOutput('modelIsTrained'),
            br(),
            plotOutput("gesturePredictedPlot"),
            textOutput('descriptionPredict'),
            br(),
            verbatimTextOutput("gestureToPredict"),
            textOutput('descriptionPredicted'),
            br(),
            verbatimTextOutput("gesturePredicted")
        ),
        tabPanel("Usage", value = 3,
            br(),
            textOutput('text1')
        )
    )
  )
),
  server = function(input, output, session) {
    autoInvalidate <- reactiveTimer(20, session)
    output$plot <- renderPlot({
        autoInvalidate()
        query <- paste0("SELECT * FROM emg limit ", input$window) 
        results <- influxdb_query('localhost', 8086, 'root', 'root', 'test',
                                  query,
                                  time_precision = 's')
        color.plot(results, input$ylim)
    })
    output$text1 <- renderText({
        "This is a small application which can record gestures and having chosen the classifier return user generated gestures."
    })
    output$descriptionRecord<- renderText({
        "You recorded this gesture:"
    })

    output$gestureToRecord<- renderText({
        if (input$recordButton >= 1) {
            isolate({
                query   <- paste0("SELECT * FROM emg limit ", input$window) 
                results <- influxdb_query('localhost', 8086, 'root', 'root', 'test',
                                        query,
                                        time_precision = 's')
                output$gestureRecordedPlot <- renderPlot(color.plot(results, input$ylim))
                d <- as.data.frame(results)
                
                dir.create(file.path("data", input$fileToRecord ), showWarnings = FALSE)
                my.file <- paste0("data/",input$fileToRecord, "/",input$gestureToRecord)
                write.table(d, file = my.file, append = FALSE, sep = ",")

                tmp.names1 <- lapply(strsplit(colnames(d), ".", fixed = TRUE), function(x) x[[2]])
                colnames(d) <- unlist(tmp.names1)
                output$gestureFeatureTable <- DT::renderDataTable(matrix_to_feature(d[,3:10]))
                
                df <- matrix_to_feature(d[,3:10])
                output$gesturePlot <- renderPlot(boxplot(df))
        
                input$gestureToRecord
            })
        }
    })
    output$modelIsTrained <- renderText({
        if (input$trainModelButton >= 1) {
            isolate({
                tmp.path <- paste0("data/",input$fileToRecord)
                d <- DATA()
                d <- set_raw_dataa(d, tmp.path)
                d <- set_feature_data(d, input$windowPredict, input$slidePredict)
                d <- set_feature_matrix(d)
                d <- set.pca(d)
                d <- get.pca(d)

                s$feature.matrix     <<- d$feature.matrix
                s$feature.pca.matrix <<- d$feature.pca.matrix
                s$pca   <<- d$pca
                s$model <<- model(input$model, d$feature.pca.matrix, d$feature.matrix)
                
                "Model trained"
            })
        }
    })
    output$nFileToRecord <- renderText({
        if (input$recordButton >= 1) {
            isolate(paste0("Gestures recorded: ", length(list.files(paste0("data/",input$fileToRecord)))))
        }
    })
    output$gestureToRecorded<- renderText({
        if (input$recordButton >= 1){
            isolate(input$gestureToRecord)
        }
    })
    output$descriptionPredict <- renderText({
        "The gesture the model should predict:"
    })
    output$gestureToPredict <- renderText({
        if (input$predictButton >= 1) {
                isolate({
                isolate(input$gestureToPredict)
            })
        }
    })
    output$descriptionPredicted <- renderText({
        "The gesture the model predicted:"
    })
    output$gesturePredicted<- renderPrint({
        if (input$predictButton >= 1){
            isolate({
                query   <- paste0("SELECT * FROM emg limit ", input$window) 
                results <- influxdb_query('localhost', 8086, 'root', 'root', 'test',
                                        query,
                                        time_precision = 's')
                output$gesturePredictedPlot <- renderPlot(color.plot(results, input$ylim))
                d <- as.data.frame(results)

                # Uncomment if you want whole window
                #v <- window_to_feature(d[,3:10]) 

                # Comment this line for no windows / slide combo
                v <- feature_matrix(d[,3:10], 10,2)

                colnames(v) <- paste0(rep("X",ncol(v)), 1:ncol(v))
                vv <- predict(s$pca, as.data.frame(v))
                as.character(predict(s$model, vv, type = "class"))
            })
        }
    })
    observe({
        if (input$recordButton >= 1) {
            s_options <- Map(function(x,y) y,list.files("data"), 1:length(list.files("data")))
            updateSelectInput(session, "subject", choices = s_options)
        }
    })
    observe({
        input$tabs1
        updateTabsetPanel(session, "tabs2", selected= input$tabs1)
    })
})
)






