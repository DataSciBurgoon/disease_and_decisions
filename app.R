#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gRain)
library(plotly)


yn <- c("yes", "no")
high_low <- c("high", "low")

#trh9010 is based on De Wolf et al Risk Assessment Models for Wheat Fusarium Head Blight Epidemics Based on Within-Season Weather Data
trh9010_level <- c("high", "low")  # high is >=90% relative humidity for greater than 56hrs; low does not meet high conditions

#climate_change is based on the fact that we are undergoing climate change -- the Bayesian Network does not like 100%, so it is coded as 99/1
climate_change <- cptable(~climate_change, values=c(99, 1), levels=yn)
trh9010 <- cptable(~trh9010 | climate_change, values=c(90, 10, 50, 50), levels=trh9010_level)

pesticide_usage <- cptable(~pesticide_usage, values=c(10, 90), levels=yn)

#the relationship between FHB index and DON is complicated by many factors. However, we do know that
#in the epidemics the DON levels will be unacceptably high.

#These values are from Paul et al Efficacy of Triazole-Based Fungicides for Fusarium Head Blight 
#and Deoxynivalenol Control in Wheat: A Multivariate Meta-Analysis 
#and
#De Wolf et al Risk Assessment Models for Wheat Fusarium Head Blight Epidemics
#Based on Within-Season Weather Data
don <- cptable(~don | trh9010:pesticide_usage, 
               values=c(25, 75, 1, 99, 99, 1, 1, 99), levels=high_low)

#this reflects the market discount when there is high levels of DON present
#see Naganje et al and Bianchini et al.
market_price <- cptable(~market_price | don, values=c(1, 99, 99, 1), levels=high_low)

#This is a measure of whether or not farm workers have been adequately trained on the use of pesticides
farm_worker_training <- cptable(~farm_worker_training, values=c(20, 80), levels=high_low)

#This is whether or not there is likely to be environmental harm
environmental_harm <- cptable(~environmental_harm | farm_worker_training:pesticide_usage,
                              values=c(5, 95, 95, 5, 1, 99, 1, 99), levels=high_low)

climate_change_ecosystem_damage_model_list <- compileCPT(climate_change, trh9010, 
                                                         pesticide_usage, don, market_price, 
                                                         farm_worker_training, environmental_harm)

climate_change_ecosystem_damage_model <- grain(climate_change_ecosystem_damage_model_list)
climate_change_ecosystem_damage_model <- compile(climate_change_ecosystem_damage_model)



# Define UI for application that draws a histogram
ui <- fluidPage(

    h3(tags$b("Raptor Pharm & Tox, Ltd")),
    
    # Application title
    titlePanel("Disease & Decisions: Pesticides, Ecotoxicity and Climate Change",
               windowTitle = "Disease & Decisions"),
    
    HTML("<p><b>DESCRIPTION:</b> Find out how climate change will impact the likelihood of farm worker use of 
         pesticides to prevent wheat diseases like fusarium head blight and the probability that pesticide 
         misuse may lead to ecological harm. This decision tool uses an expert-derived Bayesian Network to calculate
         probabilities.</p>
         <p>Note: trh9010 means the number of hours above 90% humidity over a 10 day period following 
         kernel emergence on wheat -- 56hrs of greater than 90% relative humidity been shown in field studies to
         promote fungal diseases on wheat kernels, like fusarium head blight</p>"),
    
    #h2("Disease & Decisions: Pesticides, Ecotoxicity, and Climate Change"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            HTML("<h4><p>Set the parameters below to try out different scenarios.</p></h4>"),
            selectInput("select_climate_change", label = h5("climate change"), 
                        choices = list("unknown" = 1, "it is happening" = 2, "not happening" = 3), 
                        selected = 1),
            selectInput("select_trh9010", label = h5("number of hours relative humidity > 90% (after anthesis)"), 
                        choices = list("unknown" = 1, "56+ hours" = 2, "< 56 hours" = 3), 
                        selected = 1),
            selectInput("select_pesticide_usage", label = h5("pesticide usage"), 
                        choices = list("unknown" = 1, "yes, pesticides are in use" = 2, "no, pesticides are not in use" = 3), 
                        selected = 1),
            selectInput("select_deoxynivalenol", label = h5("deoxynivalenol (DON) levels"), 
                        choices = list("unknown" = 1, "1+ppm" = 2, "< 1ppm" = 3), 
                        selected = 1),
            selectInput("select_market_price", label = h5("market price at mill"), 
                        choices = list("unknown" = 1, "maximum" = 2, "discount due to DON levels" = 3), 
                        selected = 1),
            selectInput("select_farm_worker_training", label = h5("farm worker pesticide use training"), 
                        choices = list("unknown" = 1, "high degree of training/low risk of misuse" = 2, "low degree of training/higher risk of misuse" = 3), 
                        selected = 1),
            selectInput("select_environmental_harm", label = h5("environmental harm from pesticide misuse"), 
                        choices = list("unknown" = 1, "high degree of harm" = 2, "low degree of harm" = 3), 
                        selected = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            img(src="./bayes_net.png", width=700),
            HTML("<br><br><br><br><center><p><b>Probability of event happening</b></p></center>"),
            plotlyOutput("pokeChart"),
            
           div(class="footer",
                HTML("<br><br><br><center><b>Copyright 2020 Raptor Pharm & Tox, Ltd. All Rights Reserved</b></center><br><br>")
            )
            
        )
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$pokeChart <- renderPlotly({
        evidence_list <- list()
        prediction_list <- list()
        
        #set climate change evidence
        if(input$select_climate_change == 1){
            evidence_list[["climate_change"]] <-NULL
            prediction_list[["climate_change"]] <- NULL
        }
        else if(input$select_climate_change == 2){
            evidence_list[["climate_change"]] <- "yes"
            prediction_list[["climate_change"]] <- c(yes = 1, no = 0)
        }
        else{
            evidence_list[["climate_change"]] <- "no"
            prediction_list[["climate_change"]] <- c(yes = 0, no = 1)
        }
        
        #set trh9010 evidence
        if(input$select_trh9010 == 1){
            evidence_list[["trh9010"]] <- NULL
            prediction_list[["trh9010"]] <- NULL
        }
        else if(input$select_trh9010 == 2){
            evidence_list[["trh9010"]] <- "high"
            prediction_list[["trh9010"]] <- c(high = 1, low = 0)
        }
        else{
            evidence_list[["trh9010"]] <- "low"
            prediction_list[["trh9010"]] <- c(high = 0, low = 1)
        }
        
        #set deoxynivalenol evidence
        if(input$select_deoxynivalenol == 1){
            evidence_list[["don"]] <- NULL
            prediction_list[["don"]] <- NULL
        }
        else if(input$select_deoxynivalenol == 2){
            evidence_list[["don"]] <- "high"
            prediction_list[["don"]] <- c(high = 1, low = 0)
        }
        else{
            evidence_list[["don"]] <- "low"
            prediction_list[["don"]] <- c(high = 0, low = 1)
        }
        
        #set pesticide usage
        if(input$select_pesticide_usage == 1){
            evidence_list[["pesticide_usage"]] <- NULL
            prediction_list[["pesticide_usage"]] <- NULL
        }
        else if(input$select_pesticide_usage == 2){
            evidence_list[["pesticide_usage"]] <- "yes"
            prediction_list[["pesticide_usage"]] <- c(yes = 1, no = 0)
        }
        else{
            evidence_list[["pesticide_usage"]] <- "no"
            prediction_list[["pesticide_usage"]] <- c(yes = 0, no = 1)
        }
        
        #set market price
        if(input$select_market_price == 1){
            evidence_list[["market_price"]] <- NULL
            prediction_list[["market_price"]] <- NULL
        }
        else if(input$select_market_price == 2){
            evidence_list[["market_price"]] <- "high"
            prediction_list[["market_price"]] <- c(high = 1, low = 0)
        }
        else{
            evidence_list[["market_price"]] <- "low"
            prediction_list[["market_price"]] <- c(high = 0, low = 1)
        }
        
        #set farm worker training
        if(input$select_farm_worker_training == 1){
            evidence_list[["farm_worker_training"]] <- NULL
            prediction_list[["farm_worker_training"]] <- NULL
        }
        else if(input$select_farm_worker_training == 2){
            evidence_list["farm_worker_training"] <- "high"
            prediction_list[["farm_worker_training"]] <- c(high = 1, low = 0)
        }
        else{
            evidence_list["farm_worker_training"] <- "low"
            prediction_list[["farm_worker_training"]] <- c(high = 0, low = 1)
        }
        
        #set environmental harm
        if(input$select_environmental_harm == 1){
            evidence_list[["environmental_harm"]] <- NULL
            prediction_list[["environmental_harm"]] <- NULL
        }
        else if(input$select_environmental_harm == 2){
            evidence_list[["environmental_harm"]] <- "high"
            prediction_list[["environmental_harm"]] <- c(high = 1, low = 0)
        }
        else{
            evidence_list[["environmental_harm"]] <- "low"
            prediction_list[["environmental_harm"]] <- c(high = 0, low = 1)
        }
        
        
        
        ccedm_evidence <- setEvidence(climate_change_ecosystem_damage_model, evidence=evidence_list)
        model_predictions <- querygrain(ccedm_evidence)
        
        df_model_predictions <- data.frame(model_predictions)
        df_prediction_list <- data.frame(prediction_list)
        
        df_model_predictions <- replace(df_model_predictions, colnames(df_prediction_list), df_prediction_list)
        print(as.vector(df_model_predictions[1,]))

        
        
        fig <- plot_ly(
            type = 'scatterpolar',
            r = as.numeric(df_model_predictions[1,]),
            theta = colnames(df_model_predictions),
            #mode = "lines",
            fill = 'toself'
        ) 
        
        fig <- fig %>%
            layout(
                polar = list(
                    radialaxis = list(
                        visible = T,
                        range = c(0,1)
                    )
                ),
                showlegend = F
            )
        fig
        
    })

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
