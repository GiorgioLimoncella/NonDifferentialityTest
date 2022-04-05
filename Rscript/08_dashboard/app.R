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


DT_combination <- fread(paste0(thisdir, "/../05_Results/DT_combination_v2.csv"))
DT_PPV <- fread(paste0(thisdir, "/../05_Results/DT_sim_PPV_v2.csv"))
                
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
               selectInput("prop_exp_PPV", "Proportion of exposed:",
                            unique(DT_combination[, prop_exp])),
               
               selectInput("sample_size_PPV", "Sample size:",
                           unique(DT_combination[, sample_size])),
               
               selectInput("prev_ne_PPV", "Prevalence in non-exposed group:",
                           unique(DT_PPV[, pi_ne])),
               
               selectInput("risk_PPV", "Risk ratio:",
                           unique(DT_PPV[, risk])),
               
               selectInput("SE_exposed_PPV", "SE exposed_PPV:",
                           unique(DT_PPV[, SE_exposed]))
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotlyOutput("plt_PPV"),
               dataTableOutput('table')
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
                selectInput("prop_exp", "Proportion of exposed:",
                               unique(DT_combination[, prop_exp])),
                
                selectInput("sample_size", "Sample size:",
                            unique(DT_combination[, sample_size])),
                
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
      
      #selected <- paste0("0.1_", input$prev_ne, "_", input$risk)
      #DT_selected <- DT_combination[combination==selected]
      
      DT_selected <- DT_combination[prop_exp == input$prop_exp & 
                                      sample_size == input$sample_size & 
                                      prev_ne == input$prev_ne & 
                                      risk == input$risk  ]
      
      #plot
      plot <- ggplot(DT_selected, aes(x=SE_exp, y=power))+ 
        geom_point(col = "seashell4")+
        geom_line(size=1, col = "seashell4")+
        geom_vline(xintercept = c(0.4, 0.6), col ="seashell3", linetype="dotted", alpha = 0.5)+
        scale_x_continuous( limits = c(0,1), breaks = c(0.15, 0.30, 0.40, 0.50, 0.60, 0.70, 0.85))+
        scale_y_continuous(limits = c(0,1))+
        labs(x = "sensitivity in the exposed group (SE_ne = 0.5)", 
             y = "power of the test")+
        theme_hc()+
        ylab("")
      
      plotly <- ggplotly(plot)
      plotly

    })
    
    output$plt_PPV <- renderPlotly({
      
      # selected <- paste0("0.1_", input$prev_ne_PPV, "_", input$risk_PPV)
      # DT_selected_PPV <- DT_PPV[combination==selected]
      
      DT_selected_PPV <- DT_PPV[prop_exp == input$prop_exp_PPV & 
                                      sample_size == input$sample_size_PPV & 
                                      pi_ne == input$prev_ne_PPV & 
                                      risk == input$risk_PPV  ]
      
      plt_PPV <- ggplot(DT_selected_PPV, aes(x = PPV, fill = algorithm))+
              geom_density(alpha = 0.5)+
              ggtitle("PPV distributions")+
              theme_hc()
    
      plotly_PPV <- ggplotly(plt_PPV)
      plotly_PPV
      
    })
    
    output$table <- renderDataTable({
      DT_selected_PPV <- DT_PPV[prop_exp == input$prop_exp_PPV & 
                                  sample_size == input$sample_size_PPV & 
                                  pi_ne == input$prev_ne_PPV & 
                                  risk == input$risk_PPV &
                                  SE_exposed == input$SE_exposed_PPV]
      
      DT_sel_PPV <- DT_selected_PPV[PPV == 0 | 
                                      PPV == Inf | 
                                      is.na(PPV), 
                                    .N, by = c("algorithm")]
      
      DT_sel_PPV <- DT_sel_PPV[, prop_corrected := N/1000]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
