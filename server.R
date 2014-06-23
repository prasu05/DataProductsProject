
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
source('src/app/services.R')
source('src/utils/utils.R')
library(httr)
library(shiny)
library(datasets)
load('meta.rda')
shinyServer(
    function(input, output) {
        getUserIndex <- function(){
            if(input$fbId == ''){
                userIndex <- which(meta$user_id == '1786663222')
            }
            else{
                userIndex <- which(meta$user_id == input$fbId)    
            }
        }
        getMetaData <- reactive({
            input$go
            isolate({
                #fetch statuses data
                user_id <- input$fbId
                access_token <- input$acessToken
                
                #                     user_id <- '1786663222'
                #                     access_token <- 'CAACEdEose0cBADC6DtAwMZAz0OICAPyaaCGwxBW3J6oZBU3GQ9pomPZBVQBlFNZCm23ZBbOp86ZASwePEJaIJR9q9ApIVlShvRl1J5etgZCGp9gg53IxEQyUTAGWrEBQKNkvyGvLLX4YnXav2hGccVpCspH79cWyRSkvWdV9zvZCLA6IZA1q2evIZAb6rHJ4VbFzcZD'
                if(user_id != '' & access_token !='' ){
                    rawData <- getStatusesData(user_id, access_token)
                    meta <- preProcessStatusData(rawData, user_id)    
                }else{
                    load('meta.rda')
                }
                meta
            })    
        })
        output$fb_user_id <- reactive({
            meta <- getMetaData()
            if(input$fbId == ''){
                user_name <- 'Sushanta Pradhan'
            }
            else{
                user_name <- as.character(meta[meta$user_id == input$fbId,]$user_name)    
            }
            paste("Friends cluster for the user ::", user_name)
        })
        
        datasetInput <- reactive({
            switch(input$dataset,
                   "Cluster1" = 1,
                   "Cluster2" = 2,
                   "Cluster3" = 3,
                   "Cluster4" = 4,
                   "Cluster5" = 5
            )
        })
        output$clusterData <- renderTable({
            km <- getClusters()
            meta <- getMetaData()
            dist <- getRfDistance()
            selectedCluster <- datasetInput()
            userIndex <- getUserIndex()
            userCluster <- km$cluster[userIndex]
            if(selectedCluster != userCluster){
                userCluster <- selectedCluster
            }
            closest_friends <- km$cluster == userCluster
            d <- getDistance(dist[userIndex,], dist[closest_friends,])
            tableOutput <- meta[rownames(dist[closest_friends,][order(d),]),]
            tableOutput <- tableOutput[,-1]
            rownames(tableOutput) <- c(1:nrow(tableOutput))
            head(tableOutput,n= input$obs)
        })
        
        output$summary <- renderText({
            km <- getClusters()
            meta <- getMetaData()
            dist <- getRfDistance()
            userIndex <- getUserIndex()
            userCluster <- km$cluster[userIndex]
            selectedCluster <- datasetInput()
            userIndex <- which(meta$user_id == input$fbId)
            paste('You belong to Cluster', userCluster )
        })
        
        getRfDistance <- reactive({
            meta <- getMetaData()
            lico <- meta[,c(3,4)]
            rfModel <- randomForest(comment_count ~ like_count,
                                    proximity = TRUE,
                                    data = lico)
            rfd <- jitter(cmdscale(1-rfModel$proximity), input$jitter)*100
        })
        
        getClusters <- reactive({
            dist <- getRfDistance()
            kmeans(dist, centers = input$clusters, nstart = input$clusters^3)
        })
        output$plot1 <- renderPlot({
            meta <- getMetaData()
            dist <- getRfDistance()
            km <- getClusters()
            userIndex <- getUserIndex()
            
            par(mar = c(5.1, 4.1, 0, 1))
            plot(dist,
                 col = km$cluster,
                 pch = 20,
                 cex = 3,
                 xlab = 'likeIndex',
                 ylab = 'commentIndex')
            text(dist[userIndex,1],
                 dist[userIndex,2],
                 label='X', 
                 cex=3, 
                 col = 624)
        })
    }
)