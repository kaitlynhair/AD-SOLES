# This code is for the AD-SOLES shiny app 

# Load packages ------------------------------------------------------------------------
#devtools::install_github("ropensci/plotly")
library(viridis)
library(viridisLite)
library(shiny)
library(shinyWidgets)
library(sf)
library(ggplot2)
library(shiny)
library(plotly)
library(dplyr)
library(XML)
library(RCurl)
library(stringr)
library(tidyr)
library(shinythemes)
library(htmltools)
library(wordcloud2)
library(shinycustomloader)
library(gapminder)
library(leaflet)
library(RColorBrewer)
library(shinythemes)
library(DT)

# Source functions -----------------------------------------------------------------------------
source("functions/format_df_sun.R")
source("functions/reactive_rob_by_click.R")


# Load data ------------------------------------------------------------------------------------

# Read AD reference data 
AD_data <- read.csv("data/AD_shiny_data_final.csv")
AD_data$Year <- paste(as.character(AD_data$Year))

# Filter by year and remove likely conference abstracts (Number of DOI > 2)
AD_data_filtered <- AD_data %>%
 filter(Year > as.numeric(1994)) %>%
   group_by(DOI) %>%
   mutate(Type = ifelse(length(RecordID) > 2, "ConferenceAbstract", "Publication")) %>%
   filter(!Type == "ConferenceAbstract") %>%
   ungroup()

# Make Year numeric
AD_data_filtered$Year <- as.numeric(AD_data_filtered$Year)

AD_data_filtered_year <- AD_data_filtered %>%
   select(Year, RecordID) %>%
   unique()

# Read in country level data
AD_data_country <- read.csv("data/AD_data_country_full.csv")
AD_data_country <- AD_data_country  %>%
  filter(!Country == "") %>%
  group_by(Country) %>%
  unique() %>%
  select(Country, RecordID, Year) %>%
  mutate(nPubs = length(RecordID)) %>%
  rename(COUNTRY=Country) %>%
  ungroup() %>%
  select(COUNTRY, nPubs) %>%
  unique()

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')

AD_data_country_merged <- merge(df, AD_data_country, by="COUNTRY") 

# Read in annotated modelling data (refs labelled with model)
AD_model <- read.csv("data/AD_Models_200620.csv", encoding = "UTF-8") %>%
   filter(RecordID %in% AD_data_filtered$RecordID) %>%
   select(-X, -PdfRelativePathStatus, -Regex) %>%
   unique()

AD_model <- AD_model %>%
   filter(RecordID %in% AD_data_filtered$RecordID) 

AD_model<- merge(AD_model, AD_data_filtered_year, by="RecordID")

# Read in refs labelled with interventions
AD_intervention <- read.csv("data/AD_Interventions_180620.csv")

AD_intervention <- AD_intervention %>%
   filter(RecordID %in% AD_data_filtered$RecordID) 

AD_intervention<- merge(AD_intervention, AD_data_filtered_year, by="RecordID")

AD_intervention <- AD_intervention %>%
   filter(RegexResult > 0) %>%
   select(RecordID, RegexResult, Drug, TargetType, Year)

# Read in refs labelled with outcomes
library(readr)
AD_outcomes <- read_csv("data/AD_Outcomes_050220.csv")

AD_outcomes <- AD_outcomes %>%
   filter(RecordID %in% AD_data_filtered$RecordID) %>%
   select(RecordID, RegexResult, Outcome, Category)

AD_outcomes<- merge(AD_outcomes, AD_data_filtered_year, by="RecordID")

# Read in refs labelled with Risk of Bias reporting 
AD_rob <- read.csv("data/AD_rob_010720.csv")

AD_rob <- AD_rob %>%
   filter(RecordID %in% AD_rob$RecordID)

AD_rob <- merge(AD_rob, AD_data_filtered_year, by="RecordID")

# Categorised data (write in once)

# AD_model_refs <- AD_model %>%
#    select(RecordID, Model, Gene.Mutations) %>%
#    unique()
# 
# AD_intervention_refs <- AD_intervention %>%
#    select(RecordID, Drug, TargetType) %>%
#    unique()
# 
# AD_outcomes_refs <- AD_outcomes %>%
#    select(RecordID, Outcome) %>%
#    unique()
# 
# df <- merge(AD_data_filtered, AD_model_refs, by=c("RecordID"), all.x=T)
# df <- merge(df, AD_intervention_refs, by=c("RecordID"), all.x=T)
# df <- merge(df, AD_outcomes_refs, by=c("RecordID"), all.x=T)
# 
# df$Year <- as.numeric(df$Year)
# 
# categorised_data <- df %>%
#  select(RecordID, Gene.Mutations, Model,
#         TargetType, Drug, Outcome)
# 
# categorised_data <- categorised_data %>%
#    mutate(Drug = ifelse(is.na(Drug), "No treatment", as.character(Drug))) %>%
#    mutate(TargetType = ifelse(is.na(TargetType), "No treatment",  as.character(TargetType))) %>%
#    mutate(Outcome = ifelse(is.na(Outcome), "Other",  as.character(Outcome))) %>%
#    mutate(Model = ifelse(is.na(Model), "Unknown",  as.character(Model))) %>%
#    mutate(Gene.Mutations = ifelse(is.na(Gene.Mutations), "Unknown", as.character(Gene.Mutations)))
# 
# 
# categorised_data_small <- categorised_data %>%
#     select(-RecordID) %>%
#    unique()
# write.csv(categorised_data, "data/categorised_data_update.csv")

categorised_data <- read.csv("data/categorised_data_update.csv")

# UI ----------------------------------------------------------------------------------------

ui <- navbarPage("Living Evidence: Transgenic Animal Models of Alzheimer's Disease",

# Set theme
theme = shinytheme("flatly"),
              
tabPanel("Overview",
         
  fluidPage(
            
      fluidRow(
            
         column(12, 
         h4("Number of publications over time"),
         plotlyOutput("yearhist", height = "500px"))),
   
      
      fluidRow(
         
         column(8,
                h4("Number of publications worldwide"),
                plotlyOutput("map", height = "550px")),
      
         column(4, 
                h4("Reporting quality of publications"),
                plotlyOutput("rob", height = "550px")))
         )),


tabPanel("Modelling",
         
         fluidPage(
           
           fluidRow(
             
             column(12,
                    p("Below you can see the number of publications for each transgenic AD model (left) and the reporting quality by model type (right).
              Select a date range to filter by time. Risk of bias reporting e.g. randomisation to group, blinded outcome assessment, conflict of interest statements (COI) and sample size calculations (SSC) is shown for all model categories.
              Click on the large chunk of any category on the Sunburst chart below to see all the relevant models and click an individual model to see the reporting quality of publications for that specific model.
              To return to the original risk of bias plot, select all data by clicking the centre of the sunburst plot, labelled Tg.", style = "font-family: 'helvetica'; font-si16pt"), 
                    hr(),
                    chooseSliderSkin("Flat"),
                    sliderInput("selectmodelYear", "Select a year range",
                                min = min(as.numeric(AD_model$Year)),
                                max = max(as.numeric(AD_model$Year)),
                                value = c(1995, 2018), sep = "", width = "1000"))),
           
           fluidRow(
             column(7,
                    h4("Number of publications using each Tg Model"),
                    plotlyOutput("modelPubs", height = "750px", width = "900px")),
             
             column(5,
                    h4("Reporting quality"),
                    plotlyOutput("modelRQ", height = "750px", width = "750px")))
           
         )),

tabPanel("Interventions",
        
  fluidPage(
     
     fluidRow(
        
      column(12,
             p("Below you can see the number of publications for each intervention tested in transgenic AD models (left) and the reporting quality by outcome type (right).
              Select a date range to filter by time. Risk of bias reporting e.g. randomisation to group, blinded outcome assessment, conflict of interest statements (COI) and sample size calculations (SSC) is shown for all model categories.
              Click on the large chunk of any intervention category on the Sunburst chart below to see all the relevant interventions  and click an individual drug to see the reporting quality of publications for that specific intervention
              To return to the original risk of bias plot, select all data by clicking the centre of the sunburst plot, labelled Interventions.", style = "font-family: 'helvetica'; font-si16pt"), 
             hr(),
      chooseSliderSkin("Flat"),
      sliderInput("selectdrugYear", "Select a year:",
                  min = min(as.numeric(AD_intervention$Year)),
                  max = max(as.numeric(AD_intervention$Year)),
                  value = c(1995,2018), sep="", width = "1000"))),
             
     fluidRow(
     column(7,       
     h4("Number of publications using each intervention"),
     plotlyOutput("interventionPubs", height = "750px", width = "900px")),
             
     column(3,    
     h4("Reporting quality"),
     plotlyOutput("interventionRQ", height = "750px", width = "750")))
     
     )),


tabPanel("Outcomes",
         
         fluidPage(
            
            fluidRow(
               
               column(12,
                      p("Below you can see the number of publications for each outcome tested in transgenic AD models (left) and the reporting quality by outcome type (right).
              Select a date range to filter by time. Risk of bias reporting e.g. randomisation to group, blinded outcome assessment, conflict of interest statements (COI) and sample size calculations (SSC) is shown for all outcome categories.
              Click on the large chunk of any outcome category on the Sunburst chart below to see all the relevant outcomes and click an individual outcome to see the reporting quality of publications for that specific outcome
              To return to the original risk of bias plot, select all data by clicking the centre of the sunburst plot, labelled Outcomes.", style = "font-family: 'helvetica'; font-si16pt"), 
                      hr(),
                      chooseSliderSkin("Flat"),
                      sliderInput("selectoutcomeYear", "Select a year:",
                                  min = min(as.numeric(AD_outcomes$Year)),
                                  max = max(as.numeric(AD_outcomes$Year)),
                                  value = c(1995,2018), sep="", width = "1000"))),
            
            fluidRow(
               column(7,       
               h4("Number of publications using each outcome"),
                      plotlyOutput("outcomePubs", height = "750px", width = "900px")),
               
               column(3,  
               h4("Reporting quality"),
               plotlyOutput("outcomeRQ", height = "750px", width = "750px")))
            
         )),

tabPanel("Download Citations",
         
   sidebarPanel(
      
    pickerInput("Model", "Choose model of interest",
                   choices = as.character(sort(unique(AD_model$Model))),
                   multiple = T,  options = list(`actions-box` = TRUE)),

    pickerInput("Intervention", "Choose intervention(s) of interest",
                  choices = as.character(sort(unique(AD_intervention$Drug))),
                  multiple = T,  options = list(`actions-box` = TRUE)),

    pickerInput("Outcome", "Choose outcome(s) of interest",
                  choices = as.character(sort(unique(AD_outcomes$Outcome))),
                  multiple = T,  options = list(`actions-box` = TRUE)),

      sliderInput("Year", "Select a year:",
                  min = min(as.numeric(AD_data_filtered$Year)),
                  max = max(as.numeric(AD_data_filtered$Year)),
                  value = c(1995, 2018)),
      
      downloadButton("downloadData", "Download filtered data"),
      downloadButton("downloadAllData", "Download all data")),
   
   mainPanel(
      
      textOutput("text"),
      
      br(),
      
      DT::dataTableOutput("table")
   )
),

tabPanel("About",
         
         p("Alzheimer's disease data from publications have been generated according to a protocol available",
         tags$a(href="https://osf.io/myeja/", "here.")), 
         
         p("Risk of bias regular expression dictionaries have been adapted from original dictionaries built by Dr Zsanett Bahor.",
         tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5869854/", "Earlier versions of these have been used previously to assess reporting quality."),  
         "Regex dictionaries for transgenic model, drug, and outcome measures are custom built for this project."),
         
         p("Text-mining was performed in R uisng the", 
         tags$a(href="https://github.com/shihikoo/AutoAnnotation", "AutoAnnotation package"), "created by Dr Jing Liao"),
         
         p("If you have any questions about this dataset please contact kaitlyn.hair@ed.ac.uk"),
         p("If you would like to use it for your review, please cite:
         Hair, K. (2020, February 11). Living Evidence: Transgenic Animal Models of Alzheimer's Disease. Retrieved from https://camarades.shinyapps.io/LivingEvidence_AD/"),
         
         br(),
         
         tags$img(src = "CAMARADESlogo.jpg"))
)

# Server --------------------------------------------------------------------------------------------
server <- function(input, output){
   
   output$yearhist <- renderPlotly({
      
      AD_data_filtered %>%
         group_by(Year) %>%
         mutate(N=length(RecordID)) %>%
         ungroup() %>%
         select(Year, N) %>%
         unique() %>%
         arrange(Year) %>%
         mutate(Total = cumsum(N)) %>%
      select(Year, N, Total) %>%
      plot_ly(x=~Year, y=~N,
              type = "bar",
              name = "N Publications",
              hoverinfo = "text",
              text = ~paste(Year, ":", N, "Papers"),
              marker = list(
              color = viridis::viridis_pal(option = "D", direction = 1)(50))) %>%
       add_trace(x = ~Year, y = ~Total, type = 'scatter', mode = 'lines', name = 'Total N Publications',
               line = list(color = '#45171D'),
               hoverinfo = "text",
               text = ~paste(Year, ":", Total, "Total Papers")) %>%
         layout(
            xaxis = list(
               type = 'category'),
            yaxis = list(
               title = "Number of publications by year"))
      
                   })

   output$map <- renderPlotly({
      
      # light grey boundaries
      l <- list(color = toRGB("grey"), width = 0.5)
      
      # specify map projection/options
      g <- list(
         showframe = FALSE,
         showcoastlines = FALSE,
         projection = list(type = 'Mercator')
         )
      
      
      plot_geo(AD_data_country_merged) %>%
         add_trace(
            z = ~nPubs,
            color = ~nPubs, 
            colors = 'Blues',
            text = ~COUNTRY,
            locations = ~CODE, 
            marker = list(line = l)
         ) %>%
         colorbar(title = 'Number of Publications') %>%
         layout(
            geo = g)
      
   })

   AD_model_sun <- reactive({
     
         
         i <- subset(AD_model, subset = AD_model$Year %in% c(input$selectmodelYear[1]:input$selectmodelYear[2]))
         i <- droplevels(i)
         
         #Over 1
         i <- i %>%
            select(-RegexResult, -Year) %>%
            unique()
         
         #edit
         testdata <- i %>%
            group_by(Gene.Mutations, Model, RecordID) %>%
            count() %>%
            rename(value = n) %>%
            ungroup()
         
         DF0 <- testdata %>% 
            group_by(Gene.Mutations) %>% 
            unique() %>%
            summarise(value=sum(value))
         
         DF1 <- testdata %>% 
            group_by(Gene.Mutations, Model) %>% 
            summarise(value=sum(value))
         
         df0 <- data.frame(
            ids = paste(DF0$Gene.Mutations),
            labels = DF0$Gene.Mutations,
            parents = "",
            values = DF0$value,
            stringsAsFactors = F
         )
         
         df1 <- data.frame(
            ids = paste(DF1$Gene.Mutations, "-", DF1$Model),
            labels = DF1$Model,
            parents = paste(DF1$Gene.Mutations),
            values = DF1$value,
            stringsAsFactors = F
         )
         
         
         model_sun <- rbind(df0, df1)
         
         return(model_sun)
         
         
   #       m <- subset(AD_model, subset = AD_model$Year %in% c(input$selectmodelYear[1]:input$selectmodelYear[2]))
   #    m <- droplevels(m)
   #    
   #    m <- m %>%
   #       group_by(Model) %>%
   #       ungroup() 
   # 
   # #Over 1 
   #    
   #    m <- m %>%
   #       select(-RegexResult, -Year) %>%
   #       unique()
   #    
   #    model_sun <- format_df_sun(
   #       id = RecordID,
   #       data = m,
   #       p_col = Gene.Mutations,
   #       c_col =  Model, 
   #       parent_val = "Tg")
   #    
   #    
   #    model_sun<-model_sun %>%
   #    group_by(labels) %>%
   #    ungroup()
   #    
   #    return(model_sun)
   })
   

 output$modelPubs <- renderPlotly({
  
    p <- plot_ly(AD_model_sun(),
                     labels = ~labels,
                     parents = ~parents,
                     type = 'sunburst',
                     values =  ~values,
                     source = "modelPlot",
                     hoverinfo = "hovertext",
                     branchvalues = 'total')
    
       event_register(p, 'plotly_click')
   
   p
   
    })
        
     
     AD_int_sun <- reactive({
      
      i <- subset(AD_intervention, subset = AD_intervention$Year %in% c(input$selectdrugYear[1]:input$selectdrugYear[2]))
      i <- droplevels(i)
      
   #Over 1
      i <- i %>%
         select(-RegexResult, -Year) %>%
         unique()
      
      #edit
      testdata <- i %>%
         group_by(TargetType, Drug, RecordID) %>%
         count() %>%
         rename(value = n) %>%
         ungroup()
      
      DF0 <- testdata %>% 
         group_by(TargetType) %>% 
         unique() %>%
         summarise(value=sum(value))
      
      DF1 <- testdata %>% 
         group_by(TargetType, Drug) %>% 
         summarise(value=sum(value))
      
      df0 <- data.frame(
         ids = paste(DF0$TargetType),
         labels = DF0$TargetType,
         parents = "",
         values = DF0$value,
         stringsAsFactors = F
      )
      
      df1 <- data.frame(
         ids = paste(DF1$TargetType, "-", DF1$Drug),
         labels = DF1$Drug,
         parents = paste(DF1$TargetType),
         values = DF1$value,
         stringsAsFactors = F
      )
         

      int_sun <- rbind(df0, df1)
      return(int_sun)
   })
   
        output$interventionPubs <- renderPlotly({
          
       p <- plot_ly(AD_int_sun(),
                    ids = ~ids,
                    labels = ~labels,
                    parents = ~parents,
                    type = 'sunburst',
                    values =  ~values,
                    hoverinfo = "hovertext",
                    branchvalues = 'total', 
                    source = "treatPlot")
       
       event_register(p, 'plotly_click')
       
       p
           
   })
   
        AD_outcome_sun <- reactive({
           
           
           i <- subset(AD_outcomes, subset = AD_outcomes$Year %in% c(input$selectoutcomeYear[1]:input$selectoutcomeYear[2]))
           i <- droplevels(i)
           
           #Over 1
           i <- i %>%
              select(-RegexResult, -Year) %>%
              unique()
           
           #edit
           testdata <- i %>%
              group_by(Category, Outcome, RecordID) %>%
              count() %>%
              rename(value = n) %>%
              ungroup()
           
           DF0 <- testdata %>% 
              group_by(Category) %>% 
              unique() %>%
              summarise(value=sum(value))
           
           DF1 <- testdata %>% 
              group_by(Category, Outcome) %>% 
              summarise(value=sum(value))
           
           df0 <- data.frame(
              ids = paste(DF0$Category),
              labels = DF0$Category,
              parents = "",
              values = DF0$value,
              stringsAsFactors = F
           )
           
           df1 <- data.frame(
              ids = paste(DF1$Category, "-", DF1$Outcome),
              labels = DF1$Outcome,
              parents = paste(DF1$Category),
              values = DF1$value,
              stringsAsFactors = F
           )
           
           
          out_sun <- rbind(df0, df1)
           
          return(out_sun)

        })
        
        output$outcomePubs <- renderPlotly({
           
           p <-  plot_ly(AD_outcome_sun(),
                         ids = ~ids,
                         labels = ~labels,
                         parents = ~parents,
                         type = 'sunburst',
                         values =  ~values,
                         hoverinfo = "hovertext",
                         branchvalues = 'total', 
                         source = "outcomePlot")
           
           
           event_register(p, 'plotly_click')
           
           p
            
        })
        
      
        output$interventionRQ <- renderPlotly({
           
           reactive_rob_by_click(RoB_data = AD_rob,
                                 SourceSunPlot = "treatPlot",
                                 SunPlot = AD_int_sun(),
                                 RoBFilter = "Drug",
                                 AD_data = AD_intervention,
                                 subsetinput = input$selectdrugYear,
                                 ParentFilter = TargetType)
           
        })
        
   

        output$outcomeRQ <- renderPlotly({
           
           reactive_rob_by_click(RoB_data = AD_rob,
                                 SourceSunPlot = "outcomePlot",
                                 SunPlot = AD_outcome_sun(),
                                 RoBFilter = "Outcome",
                                 AD_data = AD_outcomes,
                                 subsetinput = input$selectoutcomeYear,
                                 ParentFilter = Category)
           
        })
        
        
        output$modelRQ <- renderPlotly({
           
           reactive_rob_by_click(RoB_data = AD_rob,
                                 SourceSunPlot = "modelPlot",
                                 SunPlot = AD_model_sun(),
                                 RoBFilter = "Model",
                                 AD_data = AD_model,
                                 subsetinput = input$selectmodelYear,
                                 ParentFilter = Gene.Mutations)
           
        })
 

  output$rob <- renderPlotly({
      
      AD_rob %>%
         select(Regex, RegexResult, RecordID) %>%
         group_by(Regex) %>%
         mutate(Ntotal = 22432) %>%
         mutate(RegexResult = ifelse(RegexResult > 0, 1, 0)) %>%
         mutate(count = sum(RegexResult)) %>%
         mutate(percent = round((count/Ntotal)*100,2)) %>% 
         select(Regex, count, percent, Ntotal) %>%
         unique() %>%
         plot_ly(
            x = ~Regex, 
            y= ~percent, 
            type = "bar",
            hoverinfo = 'text',
            text = ~paste('</br><b>% publications reporting item: </b>', percent,
                          '</br><b>N publications reporting item: </b>', count,'/',Ntotal),
            marker = list(
               color = ~percent,
               colorscale='Viridis',
               reverse = T )) %>%
         
         layout(
            xaxis = 
               list(
                  title = ""
               ),
            yaxis = list(
               title = "% Reporting",
               range = c(0,100)))
      
   })
     
   
   
   # filtered_datasetInput <- callModule(
   #       module = pickerGroupServer,
   #       id = "my-filters",
   #       data = categorised_data_small,
   #       vars = c("Gene.Mutations", "Model", "TargetType", "Drug", "Outcome"))
      
   datasetInput <- reactive({

      # df_download<-merge(AD_data, AD_model_tiab, by=c("RecordID","Year"), all=T)
      # df_download <- merge(df_download, AD_intervention, by=c("RecordID","Year"), all=T)
      # 
      # df_download <- df_download %>%
      #    select(Author, Title, Journal, Pages, Volume, Year, DOI, Abstract) %>%
      #    unique()
      # 
      # df_download$Year <- as.numeric(df_download$Year)

      df_download<-categorised_data %>%
         filter(Model %in% input$Model,
                Drug %in% input$Intervention,
                Outcome %in% input$Outcome)
      
      df_download<-df_download %>%
         select(RecordID) %>%
         unique()
      
     df_filtered_download <- AD_data_filtered %>%
         filter(RecordID %in% df_download$RecordID) %>%
         select(Author, Title, Journal, Pages, Volume, Year, DOI) %>%
         filter(Year %in% c(input$Year[1]:input$Year[2])) %>%
         unique()
      
      return(df_filtered_download)
      
    })
   
   # datasetInput <- reactive({
   #    
   #    categorised_data_id <- categorised_data %>%
   #       filter(Model %in% filtered_datasetInput()$Model) %>%
   #       filter(Gene.Mutations %in% filtered_datasetInput()$Gene.Mutations) %>%
   #       filter(TargetType %in% filtered_datasetInput()$TargetType) %>%
   #       filter(Drug %in% filtered_datasetInput()$Drug) %>%
   #       filter(Outcome %in% filtered_datasetInput()$Outcome) %>%
   #       select(RecordID)
   #    
   #    dfnew <- AD_data_filtered %>%
   #       filter(RecordID %in% categorised_data_id)
   #    
   #    print(dfnew)
   #    return(dfnew)
   # })
   #    
   output$text <- renderText({
      
      records <- as.numeric(nrow(datasetInput()))
      paste0("There are ", records, " references in this dataset available to download.")
      
      })
   
   output$table <- DT::renderDataTable({
      
      DT::datatable(datasetInput())
      
   })
      
# Downloadable csv of selected dataset ----
   output$downloadData <- downloadHandler(
      filename = function() {
         paste("AD_download.csv", sep = "")
      },
      content = function(file) {
         write.csv(datasetInput(), file, row.names = FALSE)
      })
   
   
  all_datasetInput <- reactive({
      
      df <- AD_data_filtered %>%
         select(Author, Title, Journal, Pages, Volume, Year, DOI) %>%
         unique()
      
      return(df)
   })
   
  output$downloadAllData <- downloadHandler(
     filename = function() {
        paste("AD_full_download.csv", sep = "")
     },
     content = function(file) {
        write.csv(all_datasetInput(), file, row.names = FALSE)
     })
}
# Run the application 

shinyApp(ui = ui, server = server)
