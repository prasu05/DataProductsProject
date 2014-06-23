
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(randomForest)
shinyUI(
    pageWithSidebar(
        # Application title
        headerPanel("Find Your Closest Buddies"),
        
        sidebarPanel(
            textInput("fbId", "facebook user id", value =""),
            textInput("acessToken", "acess token", value =""),
            HTML('<hr>'),
            actionButton('go', "Analyze"),
            HTML('<hr>'),
            HTML('<p>Use the slider below to increase the spread of observations in a cluster.</p>'),
            HTML('<hr>'),
            sliderInput('jitter', 
                        'Spread',
                        value = 500,
                        min = 100,
                        max = 1000,
                        step = 50),
            HTML('<hr>'),
            HTML('<hr>'),
            HTML('<p>Use the slider below to choose the number of clusters. The cluster with cross mark is the one where you are present.</p>'),
            HTML('<hr>'),
            sliderInput('clusters', 
                        'Number of Clusters',
                        value = 3,
                        min = 2,
                        max = 5,
                        step = 1),
            HTML('<hr>'),
            HTML('Select a cluster whose members you want to view. To view your closest friends select the cluster mentioned below in summary section. Friends are ordered in decreasing order of distance, i.e. friends listed on top of the table are closer to you.'),
            HTML('<hr>'),
            selectInput("dataset", "Choose a cluster:", 
                        choices = paste('Cluster',c(1:5), sep="")),
            HTML('<hr>'),
            numericInput("obs", "Number of friends to view:", 10)
        ),
        mainPanel(
            
            p("This application analyses the user's interaction with her friends through her facebook statuses and clusters the user and her closest friends together. The interaction is quantified as the number of likes and comments she is able to garner on her statuses from her friends."),
            p("The application assumes that the user's closest friends will like and comment on the user's statuses more often than others. And hence groups the user and her closest friends together."),
            p("To find your closest buddies, provide your facebook user-id and an access token for the application to pull your statuses from facebook."),
            HTML("<p>The default analysis is done on <b>Sushanta Pradhan's</b> facebook statuses.</p>"),
            p("Follow the steps below to fetch your facebook id and access token:"),
            p(HTML('<ul>
                        <li>Go to Facebook <a href="https://developers.facebook.com/tools/explorer/" target="_blank">Graph API Explorer</a>.</li>
                        <li>This will promt you for facebeook credentials if you already do not have an active session. Log in, if you do not have one.</li>
                        <li>
                            Click on the <b>Get Access Token</b> button, select the permission <b> user_status </b> and proceed as directed.
                        </li>
                        <li>
                            Hit <b> Submit </b> button to see your facebook account id. Copy and paste it in the <b> user id </b> text box in the left pane.
                        </li>
                        <li>
                            Copy the a big alpha-numeric string from the <b> Acesss Token </b> text box and paste in the <b> acess token </b> text box on the left pane. 
                        </li>
                        <li>
                            Finally hit the <b>Analyze</b> to see the results. Visualize your friend cluster in the below figure. View the details of your friends in the table below the figure. 
                        </li>
                    </ul> ')),
            textOutput("fb_user_id"),
            textOutput("fb_access_token"),
            p(HTML("<br>")),
            plotOutput('plot1'),
            tableOutput("clusterData"),
            h3('Summary'),
            textOutput("summary"),
            p(HTML("<br>")),
            HTML('<p>Keep a watch on this space, clustering of facebook friends based on other factors such as seasonality, interests and other activities on facebook will be added in near future. If you have any suggestions, please email them to <b>sushanta.pradhan@gmail.com</b></p>. ')
        )
    )
)

