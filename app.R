


library(shiny)
library(bs4Dash)
library(dplyr)
library(readr)
library(plotly)
library(leaflet)
library(tidyverse)
library(DT)
library(ggpattern)
library(fresh)
library(gridExtra)
library(gridExtra)
library(ggforce)
library(ggplot2)




##load data----------------------
df <- read.csv("Landing_data_mock.csv") %>%
  filter(sc_name != "Unknown")
## overall count----------------------------
Overall_count = df %>% 
  filter(sc_name != "Unknown") %>%
  group_by (c_name) %>%
  summarise (count = n(), .groups = 'rowwise') %>%
  arrange (desc(count))

## family count--------------------------------------------
family_count = df %>%
  filter(sc_name != "Unknown") %>%
  group_by (Family) %>%
  summarise (count = n(), .groups = 'rowwise') %>%
  arrange (desc(count))
##info box values----------------------------------------
num_individuals <- sum(Overall_count$count)
species_count <- length(unique(df$c_name))
Family_count <- length(unique (df$Family))
Total_sampling_days <- length(unique(df$dmy))


##UI-----------------------------------------------
library(bs4Dash)
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Data Insight", tabName = "dashboard", icon = icon("chart-bar"))
    )
  )
  ,
  controlbar = dashboardControlbar(),
  dashboardBody(tabItems(
    tabItem(tabName = "home",
            fluidRow(
              box(
                title = "Welcome",
                width = 12,
                status = "info",
                solidHeader = TRUE,
                collapsible = FALSE,
                p("This dashboard presents mock data on elasmobranch landings at Kochi, India."),
                p("The data was collected by me (Sharang) during the period 2021–2023 while working as a Project Assistant at WCS-India."),
                p("To ensure privacy, the dataset has been anonymized, randomized, and noise-added."),
                p("It is designed for demonstration purposes, showcasing how species-level catch data can be visualized and summarized using Shiny and Plotly."),
                p("Use the sidebar to explore visualizations of shark landings and related summaries.")
                
              )
            )
    ),
    
     tabItem(tabName = "dashboard",
    fluidRow(
      # Total Individual----------------------------------------------------
      bs4InfoBox(title = "Total Individuals",
                 value = num_individuals,
                 icon = icon("fish"),
                 color = "primary",
                 width = 3), 
      # Species Count-------------------------
       bs4InfoBox( title = "Species Count",
                  value = species_count,
                  icon = icon("dna"),
                  color = "info",
                  width = 3
      ),  # Family Count---------------------------
      bs4InfoBox( title = "Family Count",
                  value = Family_count,
                  icon = icon("sitemap"),
                  color = "success",
                  width = 3
      ),  # Sampling Days-------------------------------------
      bs4InfoBox(title = "Sampling Days",
                 value = Total_sampling_days,
                 icon = icon("calendar-alt"),
                 color = "warning",
                 width = 3
      )   
    ),
    
    ##Add  redlist plot-------------------------------
    fluidRow(

      box(
        title = "No of species in each redlist category",
        width = 12, 
        closable = FALSE, 
        status = "danger",
        
        plotlyOutput("redlist_plot")),
        
        #plot for species count in each family-----------------------------------
      fluidRow(
      box(
          title = "No of species in each Family",
          width = 12, 
          closable = FALSE, 
          status = "danger",
          
          plotlyOutput("Family_plot")),
     
      #plot for sex ratio for top 5 species-------------------------------------
       fluidRow(
          box(
            title = "Sex ratios of top 5 species",
            
            width = 12, 
            closable = FALSE, 
            status = "danger",
            
            plotlyOutput("sizedata_plot", height = "600px")),
          
          #sizeclass plot
          fluidRow(
            box(
              title = "Size class distribution of top 5 species",
              width = 12, 
              closable = FALSE, 
              status = "danger",
              
              plotlyOutput("sizeclass_plot",, height = "600px")),
          
          
          #summarised table include family, species, common name, Avg TL and SD ----------------------------------
           fluidRow( 
            tabBox(
              title = "Species List",
              width = 12,
              type = "tabs",
              status = "danger",
              solidHeader = TRUE,
              
              tabPanel(
                "Species Table",
                DTOutput("table_final")
              ))
      
      )))))))))
  


server <- function(input, output, session) {
  # Redlist species pie chart------------------------------------------------------------------------------
  output$redlist_plot <- renderPlotly({
    
    species_redlist <- df %>%
      filter( red_list != "NA") %>%
      distinct(sc_name, red_list) %>%
      count(red_list) %>%
      mutate(percentage = round(n / sum(n) * 100,digits=2)) %>% arrange(desc(percentage)) 
    species_redlist$red_list<- factor(species_redlist$red_list, levels = c("DD","LC", "NT"
                                                                           ,"VU","EN","CR"
                                                                            ))
   status_colors <- c("CR" = "#D81E05", "EN" = "#FC7F3F", "VU"="#F9E814",
                       "NT"= "#CCE226", "LC"= "#60C659","DD"= "#D1D1C6" )
   species_redlist$color <- status_colors[as.character(species_redlist$red_list)]
   
    
   plot_ly(
     data = species_redlist,
     labels = ~red_list,
     values = ~n,
     type = "pie",
     marker = list(colors = species_redlist$color),
     textinfo = "label+percent",
     insidetextorientation = "radial"
   ) %>%
     layout(
       title = "Species Distribution by Red List Status",
       showlegend = TRUE
     ) %>%
     config(displayModeBar = FALSE)
   
  })
##Species in each family plot--------------------------------------------------------------------------
output$Family_plot <- renderPlotly({
  
  family_distcount <- df %>%   
    filter(sc_name != "Unknown") %>%
    group_by(type,Family)%>%
    summarise (n = n_distinct(sc_name), .groups = 'rowwise') %>%
    arrange(desc(n))
  
  family_distcount$color <- ifelse(family_distcount$type == "R", "red", "blue")
  
  # Reorder factor levels 
  family_distcount$Family <- factor(family_distcount$Family, levels = rev(unique(family_distcount$Family)))
  
  
  
  plot_colour <- "grey"
  plot_ly(data = family_distcount,
          x = ~n,
          y = ~Family,
          type = "bar",
          marker = list(
            color = ~color),
          text = ~n, textposition = 'outside',
          orientation = "h"
  ) %>% 
    layout(
      xaxis = list(title = "Count"),
      yaxis = list(title = "") ,
      legend = list(title = list(text = "Type"))
    ) %>%
    config(displayModeBar = FALSE)
  
  
})


##size distribution plot for top 5 species----------------------------------------------------------
output$sizedata_plot <- renderPlotly({
  
  top5shark_sexdata_kochi <- df %>%
    filter( sc_name %in% c("Carcharhinus falciformis", "Carcharhinus longimanus",
                           "Galeocerdo cuvier", "Sphyrna lewini", "Mobula mobular" )) %>% mutate(sc_name = factor(sc_name, levels = c("Carcharhinus falciformis", 
                                                  "Mobula mobular", "Carcharhinus longimanus",
                                                  
                                                  "Sphyrna lewini","Galeocerdo cuvier")))
  
  ggplotly( 
  ggplot(top5shark_sexdata_kochi, aes(fill = sex, x = factor(sex) )) +
    geom_bar(position = "dodge",color = "black", linetype = "solid") + facet_wrap(~ sc_name, scales = "free", as.table=TRUE,drop = TRUE) +labs(x = "", y = "Observed number of individuals landed")+theme(axis.text.x = element_text(hjust=1,angle = 0),plot.title = element_text(size = 12, color = "black", face = "bold"))+
    scale_fill_manual(values = c("M" = "#0072B2", "F" = "#FFC0CB") )  
  
  )
})

output$sizeclass_plot <- renderPlotly({
  top5shark_sizedata_kochi <- df %>%
    filter( sc_name %in% c("Carcharhinus falciformis", "Carcharhinus longimanus",
                           "Galeocerdo cuvier", "Sphyrna lewini", "Mobula mobular" )) %>% mutate(sc_name = factor(sc_name, levels = c("Carcharhinus falciformis", 
                                                                                                                                      "Mobula mobular", "Carcharhinus longimanus",
                                                                                                                                      
                                                                                                                                      "Sphyrna lewini","Galeocerdo cuvier")))
  
  ggplotly(
    ggplot(top5shark_sizedata_kochi, aes(fill = sex,x = cut(TL, breaks = seq(29, max(TL) + 10, by = 50), include.lowest = TRUE)))+
      geom_bar(position = "dodge",color = "black", linetype = "solid")+
      facet_wrap(~ sc_name, scales = "free", as.table=TRUE)+labs(x = "Total length (cm)", y = "Observed number of individuals landed")+
      scale_x_discrete(labels = c("30-79", "80-129", "130-179","180-229","230-279","280-329","330-379","380-429","430-479"))+ 
      theme(axis.text.x = element_text(vjust =.5,hjust=1,angle = 20,size = 7.1))+
      scale_fill_manual(values = c("M" = "#0072B2", "F" = "#FFC0CB") )
    
  )

})
  ## Data table--------------------------------------------------------------------------------------------------
output$table_final <- renderDT({
  
  Avg_size_Shark <- df %>% filter(sc_name != "Unknown") %>%
    group_by(Family,sc_name) %>%summarise(count = n(),M_F_Ratio = paste("1:", round(sum(sex == "F", na.rm = TRUE) / sum(sex == "M", na.rm = TRUE), 2), sep=""),Avg_TL = round(mean(TL), digits = 2),sd = round(sd(TL), digits = 2)
              ,AVG_SD = paste(Avg_TL, sd, sep="±"), .groups = 'rowwise'  ) %>%
    arrange(Family, desc(count)) %>%
    rename(
      `Scientific Name` = sc_name,
      `Sex Ratio` = M_F_Ratio,
      `Avg Total Length ± SD` = AVG_SD,
      `Average Total Length` = Avg_TL,
      `SD` = sd,
      `Count` = count
    )
  
  
  
})


}



shinyApp(ui, server)
