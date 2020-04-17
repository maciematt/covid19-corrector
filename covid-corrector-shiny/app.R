

library(shiny)
library(plotly)



source("./corrector.R")


covid_cor <- Corrector$new()



ui <- fluidPage(
    
    ## Application title
    titlePanel("COVID-19 Case & Death Report Number Corrector"),
    
    br(),
    
    
    sidebarLayout(
        
        ## Sidebar -------------------------------------------------------------- ##
        sidebarPanel(
            selectInput("act_country", "Country to correct", covid_cor$countries, "USA"),
            selectInput("ref_country", "Reference country", covid_cor$countries, "South Korea"),
            br(),
            h3("Statistical Correction", align = "center"),
            br(),
            h4("of Death:", align = "center"),
            selectInput("death_fix_type", "Distribution type", c("normal", "gamma")),
            numericInput("death_fix_loc", "Location parameter\n(normal only)", 3, min = 0),
            numericInput("death_fix_sc", "Scale parameter\n(SD in normal)", 0.5, min = 0),
            numericInput("death_fix_sh", "Shape parameter\n(gamma only)", 6, min = 0),
            h4("of Cases:", align = "center"),
            selectInput("cases_fix_type", "Distribution type", c("normal", "gamma")),
            numericInput("cases_fix_loc", "Location parameter\n(normal only)", 3, min = 0),
            numericInput("cases_fix_sc", "Scale parameter\n(SD in normal)", 0.5, min = 0),
            numericInput("cases_fix_sh", "Shape parameter\n(gamma only)", 6, min = 0)
        ),
        
        ## ---------------------------------------------------------------------- ##
        
        # Show a plot of the generated distribution
        mainPanel(
            br(),
            fluidRow(
                column(6,
                       plotlyOutput("act_country_plot")
                ),
                column(6, 
                       plotlyOutput("ref_country_plot")
                )
            ),
            br(), br(),
            fluidRow(
                column(6,
                       plotlyOutput("corrected_cases_plot")
                ),
                column(6,
                       plotlyOutput("dr_plot")
                )
            ),
            br(), br(),
            fluidRow(
                column(6,
                       plotlyOutput("stats_fixed_cases_plot")
                ),
                column(6,
                       plotlyOutput("stats_fixed_death_plot")
                )
            ),
            br(), br(),
            fluidRow(
                column(6,
                       plotlyOutput("demo_plot")
                ),
                column(6,
                       plotlyOutput("demo_death_plot")
                )
            ),
            br(), br()
        )
    )
)

# Define server logic required to draw the plots
server <- function(input, output) {
    
    output$act_country_plot <- renderPlotly({
        covid_cor$get_country_plot_data(input$act_country) %>% 
            plot_ly(x = ~date, y = ~cases, type = "scatter", mode = "lines+markers", color = I("dark green"), name = "Cases") %>% 
            add_trace(x = ~date, y = ~recovered, type = "scatter", mode = "lines+markers", color = I("blue"), name = "Recovered") %>% 
            add_trace(x = ~date, y = ~deaths, type = "scatter", mode = "lines+markers", color = I("red"), name = "Deaths", yaxis = "y2") %>% 
            layout(title = list(text = input$act_country), yaxis2 = list(overlaying = "y", side = "right", automargin = T), yaxis = list(title = "Cumulative Report Number"), xaxis = list(title = "Date"), legend = list(x = 0.05, y = 0.95))
    })
    output$ref_country_plot <- renderPlotly({
        covid_cor$get_country_plot_data(input$ref_country) %>% 
            plot_ly(x = ~date, y = ~cases, type = "scatter", mode = "lines+markers", color = I("dark green"), name = "Cases") %>% 
            add_trace(x = ~date, y = ~recovered, type = "scatter", mode = "lines+markers", color = I("blue"), name = "Recovered") %>% 
            add_trace(x = ~date, y = ~deaths, type = "scatter", mode = "lines+markers", color = I("red"), name = "Deaths", yaxis = "y2") %>% 
            layout(title = list(text = paste0(input$ref_country, " (as reference)")), yaxis2 = list(overlaying = "y", side = "right", automargin = T), yaxis = list(title = "Cumulative Report Number"), xaxis = list(title = "Date"), legend = list(x = 0.05, y = 0.95))
    })
    
    
    output$corrected_cases_plot <- renderPlotly({
        covid_cor$new_correction(active_country = input$act_country, reference_country = input$ref_country)
        covid_cor$get_corrected_cases_plot_data() %>% 
            plot_ly(x = ~date, y = ~cases, type = "scatter", mode = "lines+markers", color = I("dark green"), name = "Cases") %>% 
            add_trace(x = ~date, y = ~potential_cases, type = "scatter", mode = "lines+markers", color = I("light green"), name = "Adj. Cases") %>% 
            layout(title = list(text = paste0(input$act_country, " Cases (Adj. via ", input$ref_country, ")")), yaxis = list(title = "Cumulative Report Number"), xaxis = list(title = "Date"), legend = list(x = 0.05, y = 0.95))
    })
    
    
    output$dr_plot <- renderPlotly({
        covid_cor$compare_deathrates(active_country = input$act_country, reference_country = input$ref_country) %>% 
            plot_ly(x = ~date, y = ~dr_ref, type = "scatter", mode = "lines+markers", color = I("grey"), name = input$ref_country) %>% 
            add_trace(x = ~date, y = ~dr_act, type = "scatter", mode = "lines+markers", color = I("red"), name = input$act_country) %>% 
            layout(title = list(text = paste0("Death Rate Comparison: ", input$act_country, " & ", input$ref_country)), yaxis = list(title = "Death Rate"), xaxis = list(title = "Date"), showlegend = F)
    })
    
    
    output$stats_fixed_cases_plot <- renderPlotly({
        covid_cor$stat_fix(
            death_fix_loc = input$death_fix_loc, death_fix_sh = input$death_fix_sh, death_fix_sc = input$death_fix_sc, death_fix_type = input$death_fix_type, death_sampling = 1000,
            cases_fix_loc = input$cases_fix_loc, cases_fix_sh = input$cases_fix_sh, cases_fix_sc = input$cases_fix_sc, cases_fix_type = input$cases_fix_type, cases_sampling = 1000
        )
        
        covid_cor$get_cases_stat_fix_plot_data() %>% 
            plot_ly(type = "scatter") %>% 
            add_trace(x = ~day_date, y = ~value_975, mode = "lines", line = list(color = "rgba(44, 160, 44, 0.2)"), name = "Adj. Cases (Stats)", showlegend = F) %>% 
            add_trace(x = ~day_date, y = ~value_median, mode = "lines+markers", fill = "tonexty", fillcolor = "rgba(44, 160, 44, 0.2)", line = list(color = "rgba(44, 160, 44, 1)"), name = "Adj. Cases (Stats)") %>% 
            add_trace(x = ~day_date, y = ~value_025, mode = "lines", fill = "tonexty", fillcolor = "rgba(44, 160, 44, 0.2)", line = list(color = "rgba(44, 160, 44, 0.2)"), name = "Adj. Cases (Stats)", showlegend = F) %>% 
            add_trace(x = ~day_date, y = ~potential_cases, type = "scatter", mode = "lines+markers", color = I("light green"), name = "Adj. Cases (Ref)") %>% 
            layout(title = list(text = paste0(input$act_country, " Cases (Adj. via Stats)")), yaxis = list(title = "Cumulative Report Number"), xaxis = list(title = "Date"), legend = list(x = 0.05, y = 0.95))
    })
    
    
    output$stats_fixed_death_plot <- renderPlotly({
        covid_cor$stat_fix(
            death_fix_loc = input$death_fix_loc, death_fix_sh = input$death_fix_sh, death_fix_sc = input$death_fix_sc, death_fix_type = input$death_fix_type, death_sampling = 1000,
            cases_fix_loc = input$cases_fix_loc, cases_fix_sh = input$cases_fix_sh, cases_fix_sc = input$cases_fix_sc, cases_fix_type = input$cases_fix_type, cases_sampling = 1000
        )
        
        covid_cor$get_death_stat_fix_plot_data() %>% 
            plot_ly(type = "scatter") %>% 
            add_trace(x = ~day_date, y = ~value_975, mode = "lines", line = list(color = "rgba(255, 0, 255, 0.2)"), name = "Adj. Deaths", showlegend = F) %>% 
            add_trace(x = ~day_date, y = ~value_median, mode = "lines+markers", fill = "tonexty", fillcolor = "rgba(255, 0, 255, 0.2)", color = I("magenta"), name = "Adj. Deaths") %>% 
            add_trace(x = ~day_date, y = ~value_025, mode = "lines", fill = "tonexty", fillcolor = "rgba(255, 0, 255, 0.2)", line = list(color = "rgba(255, 0, 255, 0.2)"), name = "Adj. Deaths", showlegend = F) %>% 
            add_trace(x = ~day_date, y = ~value, type = "scatter", mode = "lines+markers", color = I("red"), name = "Deaths") %>% 
            layout(title = list(text = paste0(input$act_country, " Deaths (Adj. via Stats)")), yaxis = list(title = "Cumulative Report Number"), xaxis = list(title = "Date"), legend = list(x = 0.05, y = 0.95))
    })
    
    
    output$demo_plot <- renderPlotly({
        d <- covid_cor$compare_demo(active_country = input$act_country, reference_country = input$ref_country) 
        d %>% plot_ly %>% 
            add_bars(x = ~ -act_value, y = ~ Age, orientation = "h", name = input$act_country, marker = list(color = "powderblue"), text = d$act_value, hovertemplate = "Age: %{y}<br>Number: %{text:.3s}") %>% 
            add_bars(x = ~ ref_value, y = ~ Age, orientation = "h", name = input$ref_country, marker = list(color = "seagreen"), hovertemplate = "Age: %{y}<br>Number: %{x:.3s}") %>% 
            layout(title = list(text = "Demographics"), xaxis = list(title = "Number", showticklabels = F), legend = list(x = 0.1, y = 0.9), barmode = "overlay", bargap = 0.1, showlegend = F)
    })
    
    
    output$demo_death_plot <- renderPlotly({
        d <- covid_cor$compare_vuln_demo(active_country = input$act_country, reference_country = input$ref_country) 
        d %>% plot_ly %>% 
            add_bars(x = ~ -act_vul, y = ~ Age, orientation = "h", name = input$act_country, marker = list(color = "powderblue"), text = d$act_vul, hovertemplate = "Age: %{y}<br>Vulnerability: %{text:.3s}") %>% #, hoverinfo = "name+text+y"
            add_bars(x = ~ ref_vul, y = ~ Age, orientation = "h", name = input$ref_country, marker = list(color = "seagreen"), hovertemplate = "Age: %{y}<br>Vulnerability: %{x:.3s}") %>% 
            layout(title = list(text = "Demographics Vulnerabilities"), xaxis = list(title = "Vulnerability", showticklabels = F), legend = list(x = 0.1, y = 0.9), barmode = "overlay", bargap = 0.1, showlegend = F)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
