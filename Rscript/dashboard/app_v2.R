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
ui <- fluidPage( theme = shinytheme("flatly"),

    # Application title
   
    navbarPage("Multiple Component Strategy - Simulations",
    
               tabPanel( "Non-Differentility test",
                         
                sidebarPanel(width=3,
                  HTML("Please select the following parameters:"),
                  HTML("<br><br>"),
                selectInput("prop_exp", "Proportion of exposed",
                            c(0.10, 0.01)),
                
                selectInput("sample_size", "Sample size",
                            c(500, 250)),
                
                selectInput("prev_ne", "Prevalence in non-exposed group",
                            c(0.1, 0.05, 0.01)),
                
                selectInput("risk", "Risk ratio",
                            c(2, 1.1, 0.9, 0.5))
                         ),
                
                # Show a plot of the generated distribution
                mainPanel(
                  HTML("<h1>Non-Differentility test</h1>"),
                  
                  HTML('<p> The power of the non-differentiality test is calculated with the following fixed parameters: 
                  <ul>
                    <li>sensitivity in the unexposed group: 0.50 </li>
                    <li>Numerosity of the validation sample: 250</li>
                  </ul>
                </p> 
                <br><br>'),
                  plotlyOutput("distPlot")
                )
               ),
      
    tabPanel("Algorithm Simulation",
             sidebarPanel(width=3,
               selectInput("SE_exposed_PPV", "Please select the sensitivity in the exposed group:",
                           unique(DT_PPV[, SE_exposed]))
             
             # Show a plot of the generated distribution
             # mainPanel(
             #   fluidRow( splitLayout(cellWidths = c("50%", "100%"), plotlyOutput("plt_PPV"),
             #   dataTableOutput('table')))
             # )
             ),
             mainPanel(
               
               HTML("<h1> Algorithm Simulation </h1>"),
               HTML("<br><br>"),
               HTML("<h3> PPV distribution </h3>"),
               HTML("<br><br>"),
                  plotlyOutput("plt_PPV"),
                 tableOutput('table_params'),
               HTML("<br><br>"),
               HTML("<h3> RR estimation </h3>"),
               HTML("<br><br>"),
               
               HTML("<br><br>"),
               HTML("<h3> PPV correction </h3>"),
               HTML("<br><br>"),
                 dataTableOutput('table'))
             
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
        geom_point(col = "royalblue4")+
        geom_line(size=1, col = "royalblue4", alpha = 0.5)+
        geom_vline(xintercept = c(0.4, 0.6), col ="seashell3", linetype="dotted", alpha = 0.5)+
        scale_x_continuous( limits = c(0,1), breaks = c(0.15, 0.30, 0.40, 0.50, 0.60, 0.70, 0.85))+
        scale_y_continuous(limits = c(0,1))+
        labs(x = "sensitivity in the exposed group (SE_ne = 0.5)", 
             y = "power of the test",
             title = "Power of the test")+
        theme_hc()+
        ylab("")
      
      plotly <- ggplotly(plot)
      plotly

    })
    
    output$plt_PPV <- renderPlotly({
      
      # selected <- paste0("0.1_", input$prev_ne_PPV, "_", input$risk_PPV)
      # DT_selected_PPV <- DT_PPV[combination==selected]
      
      DT_selected_PPV <- DT_PPV[prop_exp == input$prop_exp & 
                                      sample_size == input$sample_size & 
                                      pi_ne == input$prev_ne & 
                                      risk == input$risk&
                                  SE_exposed == input$SE_exposed_PPV  ]
      
      plt_PPV <- ggplot(DT_selected_PPV, aes(x = PPV, fill = algorithm, col = algorithm))+
              geom_density(alpha = 0.5)+
              labs(x = "PPV", 
                   y = "Density",
                   title = "PPV distribution")+
              theme_hc()
    
      plotly_PPV <- ggplotly(plt_PPV)
      plotly_PPV
      
    })
    
    output$table <- renderDataTable({
      DT_selected_PPV <- DT_PPV[prop_exp == input$prop_exp & 
                                  sample_size == input$sample_size & 
                                  pi_ne == input$prev_ne & 
                                  risk == input$risk &
                                  SE_exposed == input$SE_exposed_PPV]
      
      DT_sel_PPV <- DT_selected_PPV[PPV == 0 | 
                                      PPV == Inf | 
                                      is.na(PPV), 
                                    .N, by = c("algorithm")]
      
      DT_sel_PPV <- DT_sel_PPV[, prop_corrected := N/1000]
    })
    
    output$table_params <- renderTable({
      DT_selected_params <- DT_combination[prop_exp == input$prop_exp & 
                                      sample_size == input$sample_size & 
                                      prev_ne == input$prev_ne & 
                                      risk == input$risk &
                                      SE_exp == input$SE_exposed_PPV]
      
      DT_selected_params <- DT_selected_params[, .(SE_A_e,
                                                   SE_B_e,
                                                   SE_C_e = SE_A_int_B_e,
                                                   
                                                   SE_A_ne,
                                                   SE_B_ne,
                                                   SE_C_ne = SE_A_int_B_ne,
                                                   
                                                   SP_A_e,
                                                   SP_B_e,
                                                   SP_A_ne,
                                                   SP_B_ne
                                                   
                                                  # SE_B_given_A_e,
                                                  # SE_B_given_A_ne,
                                                  # 
                                                  # SE_A_given_B_e,
                                                  # SE_A_given_B_ne,
                                                  # 
                                                  #    
                                                  # SE_B_given_not_A_e,
                                                  # SE_B_given_not_A_ne
                                                  )]
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
