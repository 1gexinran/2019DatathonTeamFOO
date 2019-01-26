library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(shinythemes)


# Import Data  
harris_dat <- read.csv(file = "SHEA_Health_Survey_public_v00.csv")
head(harris_dat)

#### Feature Selection
# Boil down features to something that CAN be changed (my the mayor, the Harris county, or whatever) 
# 
# havinsur: Have health insurance? 
# sidewalks: Is there a sidewalk? 
# hoodactiv: See people with activity? 
# easyfood: Access to fresh fruit and vegetables in neighborhood 
# shopshere: Have restaurant within walking distance
# trailhere: Bike and trail within a mile 
# buyfood: Serious problem buying foods 
# 
# Demographics: 
# gender: gender
# age: Age
# party: political 
# ethgroup: Ethnic Group
# marriage: Marriage
# yrshere: How long lived in Houston 
# 
# Satisfaction living in Houston 
#   Assumption: the features compose/determines the "genearlly how they".
# houston: 


# filter out features that are relevant for the study. 
model_dat <- harris_dat %>% 
  # filter out Spanish speaking ppl 
  filter(lingo == 1) %>% 
  dplyr::select(havinsur, sidewalks, hoodactiv, easyfood, shopshere, trailhere, buyfood,
                gender, age, party, ethgroup, marriage, yrshere, 
                houston)


# Define UI for application that plots features of movies
ui <- fluidPage(theme = shinytheme("yeti"),
                
                titlePanel("Make Houston Great Again", windowTitle = "Houston"),
                
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel(
                    
                    h4("Select Population"),      # Third level header: Plotting
                    
                    # Select variable for gender: Radio Button
                    checkboxGroupInput(inputId = "gender_", 
                                       label = "Gender:",
                                       choices = c("Male" = 0, 
                                                   "Female" = 1), 
                                       selected = c(0, 1)),
                    
                    # Select variable for age: Select Range
                    sliderInput(inputId = "age_", 
                                label = "Age:",
                                min = min(model_dat$age), 
                                max = max(model_dat$age),
                                value = c(20, 40)),
                    
                    # Select variable for political party: select Input
                    checkboxGroupInput(inputId = "party_", 
                                       label = "Party:",
                                       choices = c("Don't Know/Refused to Answer" = -1, 
                                                   "Republican" = 1,
                                                   "Independent" = 2,
                                                   "Democrat" = 3,
                                                   "Other" = 4), 
                                       selected = c(-1, 1, 2, 3, 4)),
                    
                    # Select variable for ethnic group: checkboxGroupInput
                    checkboxGroupInput(inputId = "ethgroup_", 
                                       label = "Ethnic Group:",
                                       choices = c("Don't Know" = -2, 
                                                   "Refusedto to Answer" = -1,
                                                   "Anglo, non-Hispanic white" = 1,
                                                   "Black, African American" = 2,
                                                   "Hispanic, Latino" = 3,
                                                   "Asian" = 4,
                                                   "Middle Eastern" = 5,
                                                   "Native American" = 6,
                                                   "Mixed" = 7),
                                       selected = c(-2, -1, 1, 2, 3, 4, 5, 6, 7)),
                    
                    br(),
                    
                    # Built with Shiny by RStudio
                    h5(img(src = "d2k.jpg", height = "50px"),
                       img(src = "shell.png", height = "40px"),
                       img(src = "lilie.jpg", height = "40px"))
                    
                  ),
                  
                  # Output:
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel(title = "Histogram", 
                                         br(),
                                         h3("Distribution of Statisfaction Scores"),
                                         plotOutput(outputId = "histogram"),
                                         br(),
                                         h3("Summar Statistics"),
                                         br(),
                                         tableOutput(outputId = "summary_stat")
                                ),
                                
                                tabPanel(title = "Top Features", 
                                         br(),
                                         h3("Random Forest Feature Importance"),
                                         plotOutput(outputId = "random_forest"),
                                         h3("XGBoost Feature Importance"),
                                         plotOutput(outputId = "xgboost")
                                ),
                                
                                tabPanel(title = "Subsetted Population",
                                         br(),
                                         h3("Substted Population"),
                                         dataTableOutput(outputId = "subset_data"),
                                         br(),
                                         h3("Column Descriptions"),
                                         tableOutput(outputId = "data_dictionary"))
                    )
                    
                  )
                )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  ## Filter data based on User Selection
  model_subset <- reactive({
    subset <- model_dat %>% 
      filter(gender %in% input$gender_) %>% 
      filter(party %in% input$party_) %>% 
      filter(ethgroup %in% input$ethgroup_) %>% 
      filter(age >= input$age_[1] & age <= input$age_[2] )
    
    return(subset)
  })
  
  ## TAB 1:
  ### 1.1..Histogram
  output$histogram <- renderPlot({
    hist(model_dat$houston, col=rgb(1,0,0,0.5), freq = FALSE, breaks=-2.5:4.5,
         xlab = "Satisfaction Scores", ylim=c(0, 0.8), main = NULL)
    lines(density(model_dat$houston, adjust=2), lty="dotted", col="red", lwd=2) 
    grid (NULL,NULL, lty = 6, col = "cornsilk2") 
    
    legend("topleft", legend=c("Original", "Subset"),
           col=c("red", "blue"), pch = 15, cex=0.8)
    
    hist(model_subset()$houston, col=rgb(0,0,1,0.5), freq = FALSE, breaks=-2.5:4.5,
         add=T)
    lines(density(model_subset()$houston, adjust=2), lty="dotted", col="blue", lwd=2) 
    
    box()
  })
  
  ### 1.2..Summary Statistics: 
  output$summary_stat <- renderTable({
    subset <- model_subset()
    mean_pop <- round(mean(model_dat$houston), 2)
    mean_sub <- round(mean(subset$houston), 2)
    
    median_pop <- round(median(model_dat$houston), 2)
    median_sub <- round(median(subset$houston), 2)
    
    std_pop <- round(sd(model_dat$houston), 2)
    std_sub <- round(sd(subset$houston), 2)
    
    data.frame("Statistics" = c("Mean", "Median", "Std. Dev."),
               "Population" = c(mean_pop, median_pop, std_pop),
               "Subset" = c(mean_sub, median_sub, std_sub))
  })
  
  
  ## TAB 2: Top Features  
  ### 2.1..Random Forest
  output$random_forest <- renderPlot({
    model_dat<-model_subset()[,-(8:13)]
    
    rf <- randomForest::randomForest(houston ~ ., data=model_dat, ntree = 500, mtry = 100, 
                                     importance = TRUE, replace = FALSE, nodesize = 20)
    
    randomForest::varImpPlot(rf, n.var = 7, type = 1, main = "")
  })
  
  ### 2.2..XGBoost
  output$xgboost <- renderPlot({
    model_dat<-model_subset()[,-(8:13)]
    
    xgb <- xgboost::xgboost(as.matrix(model_dat[,-8]), 
                            label = model_dat$houston, eta = 0.5, nthread = 1, 
                            nrounds = 500,
                            verbose = 0, max_depth = 10)
    
    xgboost::xgb.plot.importance(xgboost::xgb.importance(model = xgb),
                                 rel_to_first = TRUE, xlab = "Relative Importance")
  }) 
  
  
  ## TAB 3: Subsetted Population
  ### 3.1..Subset 
  output$subset_data <- renderDataTable({
    print_data <- data.frame(model_subset())
    colnames(print_data) <- c("Health Insurance", "Sidewalk", "See People with Activity",
                              "Access to fresh fruit/vegi", "Have restaruant in walking dist")
    datatable(data = model_subset(), 
              options = list(pageLength = 10, 
                             lengthMenu = c(10, 25, 40)), 
              rownames = FALSE)
  })  
  
  ### 3.2..Column Description
  output$data_dictionary <- renderTable({
    data.frame("Column"= c("havinsur","sidewalks","hoodactiv", "easyfood", "shopshere", "trailhere", "buyfood"),
               "Description"= c("Have Health Insurance?", 
                                "Is there a sidewalk", 
                                "See people with activity?",
                                "Access to fresh fruit and vegetables in neighborhood?",
                                "Have restaurant within walking distance",
                                "Have bike and trail within a mile?",
                                "Have a serious (financial) problem buying foods?"))
  })
}

# Create Shiny app object
shinyApp(ui = ui, server = server)