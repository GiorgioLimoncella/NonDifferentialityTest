library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(ggthemes)
library(plotly)

DT <- fread("DT_combinations.csv")

# Define UI 
ui <- fluidPage( theme = shinytheme("flatly"),

    # Application title
    navbarPage("Simulations",
    
               tabPanel( "Test on non-differential sensitivity",
                         
                         sidebarPanel(width=3,
                                      HTML("Select the following parameters:"),
                                      HTML("<br><br>"),
                                      selectInput("prop_exp", "Proportion of exposed",
                                                   c(0.05, 0.2)),
                                       
                                      selectInput("sample_size", "Sample size",
                                                 c("100_100_50",
                                                   "200_200_100",
                                                   "300_300_150")),
                                       
                                      selectInput("prev_ne", "Prevalence in non-exposed group",
                                                  c(0.01, 0.05, 0.1)),
                                      
                                      selectInput("risk", "Risk ratio",
                                                  c(1.2, 2)),
                                      
                                      selectInput("SE_B_int_A_e", "SE B int A",
                                                  c(0, 0.2, 0.4))
                         ),
                
                mainPanel(
                  HTML("<h1>Test on non-differential sensitivity</h1>"),
                  plotlyOutput("distPlot")
                )
               ),
      
    tabPanel("PPV: MC distribution",
             sidebarPanel(width=3,
               selectInput("SE_ratio", "Select the sensitivity ratio:",
                           c("0.6",
                             "0.8",
                             "1",
                             "1.2", 
                             "1.4"))),

              mainPanel(
                fluidRow( splitLayout(cellWidths = c("100%", "100%"), plotlyOutput("plt_PPV"),
                dataTableOutput('table')))
              ))))
        

# Define server
server <- function(input, output) {

    output$distPlot <- renderPlotly({
      
      DT_selected <- DT[prev_ne == input$prev_ne & 
                        prop_exp == input$prop_exp &
                        risk == input$risk & 
                        SE_A_int_B_e == input$SE_B_int_A_e &
                        sample_size == input$sample_size]

      #plot
      plot <- ggplot(DT_selected, aes(x=SE_ratio, y=power))+ 
        geom_point(col = "royalblue4")+
        geom_line(size=1, col = "royalblue4", alpha = 0.5)+
        scale_y_continuous(limits = c(0,1))+
        labs(x = "sensitivity ratio", 
             y = "power of the test",
             title = "Power of the test")+
        theme_hc()+
        ylab("")
      
      plotly <- ggplotly(plot)
      plotly

    })
    
    output$plt_PPV <- renderPlotly({
      if(input$SE_ratio == 0.6){
        tmp = 0.3
      }
      if(input$SE_ratio == 0.8){
        tmp = 0.4
      }
      
      if(input$SE_ratio == 1){
        tmp = 0.5
      }
      if(input$SE_ratio == 1.2){
        tmp = 0.6
      }
      if(input$SE_ratio == 1.4){
        tmp = 0.7
      }
    
      DT_selected_PPV <- fread(paste0("DT_PPV_RR_",
                                      input$prop_exp,"_",
                                      input$prev_ne,"_",
                                      input$risk, "_", 
                                      tmp,  "_",
                                      "0.5_",
                                      input$sample_size,
                                      "_",
                                      input$SE_B_int_A_e,
                                      ".csv"))
      
      if(input$SE_B_int_A_e != 0){
        DT_selected_PPV <- data.table::melt(DT_selected_PPV, id.vars = c(),
                                            measure.vars = c("PPV_A_e",  
                                                             "PPV_A_ne", 
                                                             "PPV_B_e",  
                                                             "PPV_B_ne", 
                                                             "PPV_C_e",  
                                                             "PPV_C_ne"))
      }else{
        DT_selected_PPV <- data.table::melt(DT_selected_PPV, id.vars = c(),
                                            measure.vars = c("PPV_A_e",  
                                                             "PPV_A_ne", 
                                                             "PPV_B_e",  
                                                             "PPV_B_ne"))
      }
     

      setnames(DT_selected_PPV, "variable", "algorithm")
      setnames(DT_selected_PPV, "value","PPV")


      plt_PPV <- ggplot(DT_selected_PPV, aes(x = PPV, fill = algorithm, col = algorithm))+
              geom_density(alpha = 0.5, adjust = 2)+
              labs(x = "PPV",
                   y = "Density",
                   title = "")+
              theme_hc()

      plotly_PPV <- ggplotly(plt_PPV)
      plotly_PPV

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
