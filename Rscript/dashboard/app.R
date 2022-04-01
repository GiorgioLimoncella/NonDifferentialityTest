#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(ggthemes)
library(plotly)

### Setting the working directory
if (!require("rstudioapi")) install.packages("rstudioapi")
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
thisdir<-setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(thisdir)


DT_combination <- fread(paste0(thisdir, "/../05_Results/DT_combination.csv"))
DT_PPV <- fread(paste0(thisdir, "/../05_Results/DT_sim_PPV.csv"))
                
# Define UI for application that draws a histogram
ui <- fluidPage( theme = shinytheme("superhero"),

    # Application title
   
    navbarPage("Multiple Component Strategy - Simulations",

      
    tabPanel("Algorithm Simulation", 
             sidebarPanel(
               HTML('<p> The power of the non-differentiality test is calculated with the following fixed parameters: 
                  <ul>
                    <li>sensitivity in the unexposed group: 0.50 </li>
                    <li>Numerosity of the validation sample: 250</li>
                  </ul>
                </p> 
                <br><br>'),
               # selectInput("prop_exp", "Proportion of exposed:",
               #               unique(DT_combination[, prop_exp])),
               selectInput("prev_ne_PPV", "Prevalence in non-exposed group:",
                           unique(DT_PPV[, pi_ne])),
               
               selectInput("risk_PPV", "Risk ratio:",
                           unique(DT_PPV[, risk]))
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotlyOutput("plt_PPV")
             )),
    
    tabPanel( "Non-Differentility test",
              sidebarPanel(
                HTML('<p> The power of the non-differentiality test is calculated with the following fixed parameters: 
                  <ul>
                    <li>sensitivity in the unexposed group: 0.50 </li>
                    <li>Numerosity of the validation sample: 250</li>
                  </ul>
                </p> 
                <br><br>'),
                # selectInput("prop_exp", "Proportion of exposed:",
                #               unique(DT_combination[, prop_exp])),
                selectInput("prev_ne", "Prevalence in non-exposed group:",
                            unique(DT_combination[, prev_ne])),
                
                selectInput("risk", "Risk ratio:",
                            unique(DT_combination[, risk]))
              ),
              
              # Show a plot of the generated distribution
              mainPanel(
                plotlyOutput("distPlot")
              )
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlotly({
      
      selected <- paste0("0.1_", input$prev_ne, "_", input$risk)
      DT_selected <- DT_combination[combination==selected]
      
      #plot
      plot <- ggplot(DT_selected, aes(x=SE_exp, y=power))+ 
        geom_point()+
        geom_line(size=1)+
        scale_x_continuous( limits = c(0,1))+
        scale_y_continuous(limits = c(0,1))+
        theme_hc()+
        ylab("")
      
      plotly <- ggplotly(plot)
      plotly

    })
    
    output$plt_PPV <- renderPlotly({
      
      selected <- paste0("0.1_", input$prev_ne_PPV, "_", input$risk_PPV)
      DT_selected_PPV <- DT_PPV[combination==selected]
      
      plt_PPV <- ggplot(DT_selected_PPV, aes(x = PPV, fill = algorithm))+
              geom_density(alpha = 0.5)+
              ggtitle("PPV distributions")+
              theme_hc()
    
      plotly_PPV <- ggplotly(plt_PPV)
      plotly_PPV
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
