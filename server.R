
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

shinyServer(
    function(input, output) {
        previousHit <- 0;
        getMetaData <- reactive({
            if( input$go > previousHit ){
                previousHit <- input$go 
                isolate({
                    #fetch statuses data
                    user_id <- input$fbId
                    access_token <- input$acessToken
                    
                    #                     user_id <- '1786663222'
                    #                     access_token <- 'CAACEdEose0cBADC6DtAwMZAz0OICAPyaaCGwxBW3J6oZBU3GQ9pomPZBVQBlFNZCm23ZBbOp86ZASwePEJaIJR9q9ApIVlShvRl1J5etgZCGp9gg53IxEQyUTAGWrEBQKNkvyGvLLX4YnXav2hGccVpCspH79cWyRSkvWdV9zvZCLA6IZA1q2evIZAb6rHJ4VbFzcZD'
                    rawData <- getStatusesData(user_id, access_token)
                    preProcessStatusData(rawData, user_id)
                    
                })    
            }
        })
        output$fb_user_id <- reactive({
            meta <- getMetaData()
            paste("Facebook user id::", input$fbId,
                  "Facebook user Name::", as.character(meta[meta$user_id == input$fbId,]$user_name))
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
            if( input$go > 0 ){
                km <- getClusters()
                meta <- getMetaData()
                dist <- getRfDistance()
                selectedCluster <- datasetInput()
                userIndex <- which(meta$user_id == input$fbId)
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
            }
        })
        
        output$summary <- renderText({
            if( input$go > 0 ){
                km <- getClusters()
                meta <- getMetaData()
                dist <- getRfDistance()
                userIndex <- which(meta$user_id == input$fbId)
                userCluster <- km$cluster[userIndex]
                selectedCluster <- datasetInput()
                userIndex <- which(meta$user_id == input$fbId)
                paste('You belong to Cluster', userCluster )
            }
        })
        
        getRfDistance <- reactive({
            if(input$go > 0){
                meta <- getMetaData()
                lico <- meta[,c(3,4)]
                rfModel <- randomForest(comment_count ~ like_count,
                                        proximity = TRUE,
                                        data = lico)
                rfd <- jitter(cmdscale(1-rfModel$proximity), 300)*100
            }
        })
        
        getClusters <- reactive({
            if(input$go > 0){
                dist <- getRfDistance()
                kmeans(dist, centers = input$clusters, nstart = input$clusters^3)
            }
        })
        output$plot1 <- renderPlot({
            if(input$go > 0){
                meta <- getMetaData()
                dist <- getRfDistance()
                km <- getClusters()
                userIndex <- which(meta$user_id == input$fbId)
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
            }
        })
    }
)