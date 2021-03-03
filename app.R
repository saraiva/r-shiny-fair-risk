library(shiny)
library(shinyWidgets)
library(bslib)

light <- bs_theme(bg = "WhiteSmoke", fg = "black", primary = "#00563f")
dark <- bs_theme(bg = "black", fg = "white", primary = "#00563f")

# Define UI ----
ui <- fluidPage(
  
  theme = dark,
  
  div(
    class = "custom-control custom-switch", 
    tags$input(
      id = "dark_mode", type = "checkbox", class = "custom-control-input",
      onclick = HTML("Shiny.setInputValue('dark_mode', document.getElementById('dark_mode').value);")
    ),
    tags$label(
      "Light/Dark Mode", `for` = "dark_mode", class = "custom-control-label"
    )
  ),
  
  titlePanel("Factor Analysis for Information Risk (FAIR) Analysis"),
  
  fluidRow(
    column(4,
           textInput("text", h3("Risk Analysis Title"),  width = '100%',
                     value = "Title..."))
  ),
  
  fluidRow(
    column(4,
           textInput("text", h3("Risk Analyst Name"),  width = '100%',
                     value = "Name..."))
  ),
  
  fluidRow(
    column(4,
           textAreaInput("text", h3("Risk Analysis Participants"), width = '100%',
                         value = "Names/Titles..."))
  ),
  
  fluidRow(
    column(4,
           dateInput("date", 
                     h3("Date of Risk Analysis"),
                     value = Sys.Date()))
  ),
  
  fluidRow(
    column(4,
           textAreaInput("text", h3("Risk Analysis Description"), width = '100%',
                     value = "Description..."))
  ),

  fluidRow(
    
    h5("Note: Estimates below should be made with 90% confidence. This means roughly the bottom and top 5% of possible cases can be ignored."),
    
  ),
  
  fluidRow(
    h2("Likelihood/Frequency")
  ),
  
  fluidRow(
    radioButtons("lkh_radio", h4("- Likelihood Methods"),
                 choices = list("Threat Event Frequency/Vulnerability" = 1,
                                "Likelihood" = 2),selected = 1)
  ),
  conditionalPanel(
    condition = "input.lkh_radio == 1",
      
      fluidRow(
        h3("Threat Event Frequency")
      ),
      
      fluidRow(  
        h5("- The annual frequency that Threats are encountered, whether successful or not.")
      ),
      
      fluidRow(
        column(4,
               textAreaInput("text", h4("Threat Description"), width = '100%',
                             value = "Threat..."))
      ),
        
      fluidRow(
        
        column(4,
          formatNumericInput(
            inputId = "tef_min_num",
            label = "Threat Event Frequency - Minimum",
            value = 1,
            format = "dotDecimalCharCommaSeparator",
            width = "100%",
            align = "right"
          ),
        ),
        column(4,
          formatNumericInput(
            inputId = "tef_max_num",
            label = "Threat Event Frequency - Maximum",
            value = 10,
            format = "dotDecimalCharCommaSeparator",
            width = "100%",
            align = "right"
          ),
        ),
        
      ), 
      
      fluidRow(
        h3("Vulnerability")
      ),
      
      fluidRow(
        column(4,
               textAreaInput("text", h4("Vulnerability Description"), width = '100%',
                             value = "Vulnerability..."))
      ),
      
      fluidRow(
        radioButtons("vuln_radio", h4("- Vulnerability Methods"),
                   choices = list("Threat Capability/Control Strength" = 1,
                                  "Vulnerability Percentage" = 2),selected = 1)
      ),
      conditionalPanel(
        condition = "input.vuln_radio == 1",
    
        fluidRow(  
          h5("- Threat Capability - the percentile range of ability and resources the typical Threat is likely to possess.")
        ),
      
        fluidRow(  
          h5("- Current Control Strength - the current percentile range of resistence strength the organization's controls have to Threats.")
        ),
        fluidRow(  
          h5("- Future Control Strength - the percentile range of resistence strength the organization's controls will have to Threats after additional controls are implemented.")
        ),
      
        fluidRow(
          chooseSliderSkin("Flat", color = "#00563f"),
            column(4, 
               sliderInput("tcap_slider", h4("- Threat Capability (%)"), width = '100%',
                           min = 1, max = 99, value = c(10, 50)),
               sliderInput("cs_slider", h4("- Current Control Strength (%)"), width = '100%',
                           min = 1, max = 99, value = c(25, 75)),
               sliderInput("f_cs_slider", h4("- Future Control Strength (%)"), width = '100%',
                           min = 1, max = 99, value = c(25, 75))
          ),
        ),
        
        fluidRow(
          h4("- Current Vulnerability Percentage")
        ),
        
        fluidRow(
          verbatimTextOutput("r_vuln")
        ),
        
        fluidRow(
          h4("- Future Vulnerability Percentage")
        ),
        
        fluidRow(
          verbatimTextOutput("f_r_vuln")
        ),
        
      ),
      
      conditionalPanel(
        condition = "input.vuln_radio == 2",
        
        fluidRow(  
          h5("- Current Vulnerability - the current percentage of attempts when a Threat will be successful.")
        ),
        fluidRow(  
          h5("- Future Vulnerability - the percentage of attempts when a Threat will be successful after additional controls are implemented.")
        ),
        
        column(4,
          sliderInput("vuln_slider", h4("- Current Vulnerability (%)"), width = '100%',
                           min = 1, max = 99, value = c(10)),
          sliderInput("f_vuln_slider", h4("- Future Vulnerability (%)"), width = '100%',
                      min = 1, max = 99, value = c(10))
        )
      ),
  ),
  
  conditionalPanel(
    condition = "input.lkh_radio == 2",
    
    fluidRow(  
      h5("- Likelihood - the percentage of event when the Impact will be realized.")
    ),
    
    fluidRow(
      column(4,
             textAreaInput("text", h4("Likelihood Description"), width = '100%',
                           value = "Likelihood..."))
    ),
    
    fluidRow(
      
      column(4,
             formatNumericInput(
               inputId = "lkh_min_num",
               label = "Likelihood - Minimum",
               value = 1,
               format = "dotDecimalCharCommaSeparator",
               width = "100%",
               align = "right"
             ),
      ),
      column(4,
             formatNumericInput(
               inputId = "lkh_max_num",
               label = "Likelihood - Maximum",
               value = 10,
               format = "dotDecimalCharCommaSeparator",
               width = "100%",
               align = "right"
             ),
      ),
    ),
  ),
  
  fluidRow(
    h2("Impact/Magnitude")
  ),
  
  fluidRow(
    h3("Primary Impact")
  ),
  
  fluidRow(  
    h5("- Those Impacts incurred with every partially or wholly successful event.")
  ),
  
  fluidRow(  
    h5("- Impacts may include: (1) Response/Replacement, (2) Lost Productivity, (3) Competative Advantage, (4) Reputational Damage, and (5) Legal/Regulatory.")
  ),
  
  fluidRow(
    column(4,
           textAreaInput("text", h4("Primary Impact Description"), width = '100%',
                         value = "Impact..."))
  ),
    
  fluidRow(
    
    column(4,
    currencyInput("pi_min_num", "Primary Impact - Minimum ($)", value = 1000, format = "dollar", width = "100%", align = "right"),
    ),
    column(4,
    currencyInput("pi_max_num", "Primary Impact - Maximum ($)", value = 10000, format = "dollar", width = "100%", align = "right")
    )
  ), 

  fluidRow(
    h3("Secondary Impact")
  ),
  
  fluidRow(  
    h5("- Those Impacts (Secondary Impact) incurred only a certain percentage (Secondary Impact Likelihood) of the time.")
  ),
  
  fluidRow(  
    h5("- Impacts may include: (1) Response/Replacement, (2) Lost Productivity, (3) Competative Advantage, (4) Reputational Damage, and (5) Legal/Regulatory.")
  ),
  
  fluidRow(
    column(4,
           textAreaInput("text", h4("Secondary Impact Description"), width = '100%',
                         value = "Impact..."))
  ),
  
  fluidRow(
    
    column(4, 
           sliderInput("sl_slider", h4("- Secondary Impact Likelihood (%)"), width = '100%',
                       min = 1, max = 100, value = c(1, 5))
    )
  ),
  
  fluidRow(
    
    column(4,
    currencyInput("si_min_num", "Secondary Impact - Minimum ($)", value = 50000, format = "dollar", width = "100%", align = "right"),
    ),
    column(4,
    currencyInput("si_max_num", "Secondary Impact - Maximum ($)", value = 100000, format = "dollar", width = "100%", align = "right")
    ),
    
  ), 

  conditionalPanel(
    condition = "input.lkh_radio == 1",
  
          fluidRow(
            h1("Inherent Risk")
          ),
          
          fluidRow(
            h2("Inherent Likelihood Summary")
          ),
          
          fluidRow(
            
            h2(" "),
            verbatimTextOutput("in_lkh_sum")
        
          ),
          
          fluidRow(
            
            sliderInput("in_lkh_bins", h3("# of bins"), width = '50%',
                        min = 1, max = 50, value = c(25)),
            plotOutput('in_lkh_hist')
            
          ),
          
          fluidRow(
        
            h2("Inherent Impact Summary")
            
          ),
          
          fluidRow(
            
            h2(" "),
            verbatimTextOutput("in_ipt_sum")
        
          ),
          
          fluidRow(
            
            sliderInput("in_ipt_bins", h3("# of bins"), width = '50%',
                        min = 1, max = 50, value = c(25)),
            plotOutput('in_ipt_hist')
            
          ),
          
          fluidRow(
        
            h2("Inherent Risk Summary")
            
          ),  
          
          fluidRow(
            
            h4("Inherent Annual Loss Expectancy (1 year)")
            
          ),
          
          fluidRow(
            
            verbatimTextOutput("in_ale_sum"),
            verbatimTextOutput("in_ale_10"),
            verbatimTextOutput("in_ale_mode"),
            verbatimTextOutput("in_ale_90"),
            verbatimTextOutput("in_ale_99")
            
          ),  
          
          fluidRow(
            
            h4("Inherent Annual Loss Expectancy (10 years)")
            
          ),
          
          fluidRow(
            
            verbatimTextOutput("in_ale_ten_sum"),
            verbatimTextOutput("in_ale_ten_10"),
            verbatimTextOutput("in_ale_ten_90"),
            verbatimTextOutput("in_ale_ten_99")
            
          ),  
          
          fluidRow(
            
            sliderInput("in_ale_bins", h4("# of bins"), width = '50%',
                        min = 1, max = 50, value = c(25)),
            plotOutput('in_ale_hist')
            
          ),
  ),

  fluidRow(
    h1("Current Residual Risk")
  ),
    
  fluidRow(

    h2("Current Residual Likelihood Summary")
    
  ),
  
  fluidRow(

    h2(" "),
    verbatimTextOutput("lkh_sum")

  ),
  
  fluidRow(
    
    sliderInput("lkh_bins", h4("# of bins"), width = '50%',
                min = 1, max = 50, value = c(25)),
    plotOutput('lkh_hist')
    
  ),
  
  fluidRow(

    h2("Current Residual Impact Summary")
    
  ),
  
  fluidRow(
    
    h2(" "),
    verbatimTextOutput("ipt_sum")

  ),
  
  fluidRow(
    
    sliderInput("ipt_bins", h4("# of bins"), width = '50%',
                min = 1, max = 50, value = c(25)),
    plotOutput('ipt_hist')
    
  ),
  
  fluidRow(

    h2("Current Residual Risk Summary")
    
  ),
  
  fluidRow(
    
    h4("Current Residual Annual Loss Expectancy (1 year)")
    
  ),
  
  fluidRow(
    
    verbatimTextOutput("rale_sum"),
    verbatimTextOutput("rale_10"),
    verbatimTextOutput("rale_mode"),
    verbatimTextOutput("rale_90"),
    verbatimTextOutput("rale_99")
    
  ),
  
  fluidRow(
  
    h4("Current Residual Annual Loss Expectancy (10 years)")
    
  ),
  
  fluidRow(
    
    h2(" "),
    verbatimTextOutput("rale_ten_sum"),
    verbatimTextOutput("rale_ten_10"),
    verbatimTextOutput("rale_ten_90"),
    verbatimTextOutput("rale_ten_99")
    
  ),
  
  fluidRow(
    
    sliderInput("rale_bins", h4("# of bins"), width = '50%',
                min = 1, max = 50, value = c(25)),
    plotOutput('rale_hist_1'),
    plotOutput('rale_hist_2')
    
  ),
  
  conditionalPanel(
    condition = "input.lkh_radio == 1",
    
    fluidRow(
      h1("Future Residual Risk")
    ),
    
    fluidRow(
      
      h2("Future Residual Likelihood Summary")
      
    ),
    
    fluidRow(
      
      h2(" "),
      verbatimTextOutput("f_lkh_sum")
      
    ),
    
    fluidRow(
      
      sliderInput("f_lkh_bins", h4("# of bins"), width = '50%',
                  min = 1, max = 50, value = c(25)),
      plotOutput('f_lkh_hist')
      
    ),
    
    fluidRow(
      
      h2("Future Residual Impact Summary")
      
    ),
    
    fluidRow(
      
      h2(" "),
      verbatimTextOutput("f_ipt_sum")
      
    ),
    
    fluidRow(
      
      sliderInput("f_ipt_bins", h4("# of bins"), width = '50%',
                  min = 1, max = 50, value = c(25)),
      plotOutput('f_ipt_hist')
      
    ),
    
    fluidRow(
      
      h2("Future Residual Risk Summary")
      
    ),
    
    fluidRow(
      
      h4("Future Residual Annual Loss Expectancy (1 year)")
      
    ),
    
    fluidRow(
      
      verbatimTextOutput("f_rale_sum"),
      verbatimTextOutput("f_rale_10"),
      verbatimTextOutput("f_rale_mode"),
      verbatimTextOutput("f_rale_90"),
      verbatimTextOutput("f_rale_99")
      
    ),
    
    fluidRow(
      
      h4("Future Residual Annual Loss Expectancy (10 years)")
      
    ),
    
    fluidRow(
      
      h2(" "),
      verbatimTextOutput("f_rale_ten_sum"),
      verbatimTextOutput("f_rale_ten_10"),
      verbatimTextOutput("f_rale_ten_90"),
      verbatimTextOutput("f_rale_ten_99")
      
    ),
    
    fluidRow(
      
      sliderInput("f_rale_bins", h4("# of bins"), width = '50%',
                  min = 1, max = 50, value = c(25)),
      plotOutput('f_rale_hist_1'),
      plotOutput('f_rale_hist_2')
      
    ),
  ),
  
)
# Define server logic ----
server <- function(input, output, session) {

  library(ggplot2)
  library(scales)
  ##### Input Variables #############################################################################
  # Threat Event Frequency (Min, Max) 90% Confidence
  tef_min <- reactive({
    cbind(input$tef_min_num)
  })
  tef_max <- reactive({
    cbind(input$tef_max_num)
  })
  # Threat Capability (Min, Max) 90% Confidence
  # Percentage in decimal format
  tcap_min_stage <- reactive({
    cbind(input$tcap_slider[1])
  })
  tcap_min <- reactive({
    tcap_min_stage() / 100
  })
  tcap_max_stage <- reactive({
    cbind(input$tcap_slider[2])
  })
  tcap_max <- reactive({
    tcap_max_stage() / 100
  })
  # Control Strength (Min, Max) 90% Confidence
  # Percentage in decimal format
  cs_min_stage <- reactive({
    cbind(input$cs_slider[1])
  })
  cs_min <- reactive({
    cs_min_stage() / 100
  })
  cs_max_stage <- reactive({
    cbind(input$cs_slider[2])
  })
  cs_max <- reactive({
    cs_max_stage() / 100
  })
  # Future Control Strength (Min, Max) 90% Confidence
  # Percentage in decimal format
  f_cs_min_stage <- reactive({
    cbind(input$f_cs_slider[1])
  })
  f_cs_min <- reactive({
    f_cs_min_stage() / 100
  })
  f_cs_max_stage <- reactive({
    cbind(input$f_cs_slider[2])
  })
  f_cs_max <- reactive({
    f_cs_max_stage() / 100
  })
  # Direct Vulnerability percentage
  vuln_option <- reactive({
    cbind(input$vuln_radio)
  })
  vuln_percent_stage <- reactive({
    cbind(input$vuln_slider)
  })
  vuln_percent <- reactive({
    vuln_percent_stage() / 100
  })
  # Direct Vulnerability percentage
  f_vuln_percent_stage <- reactive({
    cbind(input$f_vuln_slider)
  })
  f_vuln_percent <- reactive({
    f_vuln_percent_stage() / 100
  })
  # Likelihood (Min, Max) 90% Confidence
  # Percentage in decimal format
  lkh_option <- reactive({
    cbind(input$lkh_radio)
  })
  lkh_min <- reactive({
    cbind(input$lkh_min_num)
  })
  lkh_max <- reactive({
    cbind(input$lkh_max_num)
  })
  # Primary Impact (Min, Max) 90% Confidence
  pi_min <- reactive({
    cbind(input$pi_min_num)
  })
  pi_max <- reactive({
    cbind(input$pi_max_num)
  })
  # Secondary Impact Likelihood (Min, Max) 90% Confidence
  # Percentage in decimal format
  sl_min_stage <- reactive({
    cbind(input$sl_slider[1])
  })
  sl_min <- reactive({
    sl_min_stage() / 100
  })
  sl_max_stage <- reactive({
    cbind(input$sl_slider[2])
  })
  sl_max <- reactive({
    sl_max_stage() / 100
  })
  # Secondary Impact (Min, Max) 90% Confidence
  si_min <- reactive({
    cbind(input$si_min_num)
  })
  si_max <- reactive({
    cbind(input$si_max_num)
  })
  # How Many Simulations?
  n <- 100000
  ###################################################################################################
  ##### Input Mean and Standard Deviation  Calculations #############################################
  # Threat Event Frequency
  tef_mean <- reactive({
    (log(tef_max())+log(tef_min()))/2
  })
  tef_sd <- reactive({
    (log(tef_max())-log(tef_min()))/3.29
  })
  # Threat Capability
  tcap_mean <- reactive({
    (log(tcap_max())+log(tcap_min()))/2
  })
  tcap_sd <- reactive({
    (log(tcap_max())-log(tcap_min()))/3.29
  })
  # Resistance Strength
  cs_mean <- reactive({
    (log(cs_max())+log(cs_min()))/2
  })
  cs_sd <- reactive({
    (log(cs_max())-log(cs_min()))/3.29
  })
  # Future Resistance Strength
  f_cs_mean <- reactive({
    (log(f_cs_max())+log(f_cs_min()))/2
  })
  f_cs_sd <- reactive({
    (log(f_cs_max())-log(f_cs_min()))/3.29
  })
  # Likelihood
  lkh_mean <- reactive({
    (log(lkh_max())+log(lkh_min()))/2
  })
  lkh_sd <- reactive({
    (log(lkh_max())-log(lkh_min()))/3.29
  })
  # Primary Single Event Impact
  pi_mean <- reactive({
    (log(pi_max())+log(pi_min()))/2
  })
  pi_sd <- reactive({
    (log(pi_max())-log(pi_min()))/3.29
  })
  # Secondary Impact Likelihood
  sl_mean <- reactive({
    (log(sl_max())+log(sl_min()))/2
  })
  sl_sd <- reactive({
    (log(sl_max())-log(sl_min()))/3.29
  })
  # Secondary Single Event Impact
  si_mean <- reactive({
    (log(si_max())+log(si_min()))/2
  })
  si_sd <- reactive({
    (log(si_max())-log(si_min()))/3.29
  })
  ###################################################################################################
  ##### FAIR Analysis Calculations ##################################################################
  # Generate a random seed
  rand <- floor(runif(1, min=1000, max=999999))
  #print(rand)
  set.seed(rand)
  # Calculate the Current Residual Threat Event Frequency (TEF)
  tef <- reactive({
    rlnorm(n,tef_mean(),tef_sd())
  })
  # Calculate the Current Residual Threat Capability (TCap)
  tcap <- reactive({
    rlnorm(n,tcap_mean(),tcap_sd())
  })
  # Calculate the Current Residual Control Strength (CS)
  cs <- reactive({
    rlnorm(n,cs_mean(),cs_sd())
  })
  # Calculate the Future Residual Control Strength (CS)
  f_cs <- reactive({
    rlnorm(n,f_cs_mean(),f_cs_sd())
  })
  # Calculate the Current Likelihood (CS)
  lkh_temp2 <- reactive({
    rlnorm(n,lkh_mean(),lkh_sd())
  })
  # Calculate the Current Residual Vulnerability (Vuln)
  vuln_temp4 <- reactive({
    tcap() - cs()
  })
  vuln_temp3 <- reactive({
    pmax(vuln_temp4(),0)
  })
  vuln_temp2 <- reactive({
    pmin(vuln_temp3(),1)
  })
  vuln_temp1 <- reactive({
    (sum(vuln_temp2() > 0))/n
  })
  vuln <- reactive({
    if (vuln_option()==1) {
      vuln_temp1()
    } else {
      vuln_percent()
    }
  })
  # Calculate the Future Residual Vulnerability (Vuln)
  f_vuln_temp4 <- reactive({
    tcap() - f_cs()
  })
  f_vuln_temp3 <- reactive({
    pmax(f_vuln_temp4(),0)
  })
  f_vuln_temp2 <- reactive({
    pmin(f_vuln_temp3(),1)
  })
  f_vuln_temp1 <- reactive({
    (sum(f_vuln_temp2() > 0))/n
  })
  f_vuln <- reactive({
    if (vuln_option()==1) {
      f_vuln_temp1()
    } else {
      f_vuln_percent()
    }
  })
  ## Assumes the Inherent Vulnerability level is 90%
  in_vuln <- 0.95
  # Calculate the Current Residual Primary Likelihood (LKH)
  lkh_temp1 <- reactive({
    tef() * as.vector(vuln())
  })
  lkh <- reactive({
    if (lkh_option()==1) {
      #lkh_temp1()
      round(lkh_temp1(), digits = 2)
    } else {
      #lkh_temp2()
      round(lkh_temp2(), digits = 2)
    }
  })
  # Calculate the Future Residual Primary Likelihood (LKH)
  f_lkh <- reactive({
    #tef() * as.vector(f_vuln())
    round(tef() * as.vector(f_vuln()), digits = 2)
  })
  ## Assumes that the Threat Event Frequency increases 20% in the absense of controls
  in_tef <- reactive({
    tef() * 1.2
  })
  in_lkh <- reactive({
    #in_tef() * in_vuln
    round(in_tef() * in_vuln, digits = 2)
  })
  ### Calculate the Inherent and Residual Primary and Secondary Impacts
  ### The Residual Primary Impact is the loss that will result with every occurrence
  pi <- reactive({
    rlnorm(n,pi_mean(),pi_sd())
  })
  ### Assumes that the Primary Impact increases 20% in the absence of controls
  in_pi <- reactive({
    pi() * 1.2
  })
  ### The Residual Secondary Impact Likelihood is the probability that the Secondary Impact will occur
  sl <- reactive({
    rlnorm(n,sl_mean(),sl_sd())
  })
  ### The Residual Secondary Impact is the loss that will only result with some percentage of occurrences
  si <- reactive({
    rlnorm(n,si_mean(),si_sd())
  })
  ### Inherent Secondary Impact
  in_si <- reactive({
    si() * 1.14
  })
  # Inherent Impact
  in_ipt <- reactive({
    #in_pi() + in_si()
    round(in_pi() + in_si())
  })
  # Residual Secondary Impact per Event
  sie <- reactive({
    sl() * si()
  })
  # Residual Impact
  ipt <- reactive({
    #pi() + sie()
    round(pi() + sie())
  })
  # Current Residual Annual Loss Expectancy = lkh * ipt
  rale <- reactive({
    round(lkh() * ipt())
  })
  # Future Residual Annual Loss Expectancy = lkh * ipt
  f_rale <- reactive({
    round(f_lkh() * ipt())
  })
  # Current Residual Annual Loss Expectancy over 10 years
  rale_ten <- reactive({
    rale() * 10
  })
  # Future Residual Annual Loss Expectancy over 10 years
  f_rale_ten <- reactive({
    f_rale() * 10
  })
  ## Inherent Annual Loss Expectancy - Assumes the single loss expectancy increases 20% in the absense of controls
  in_ale <- reactive({
    round(in_lkh() * in_ipt())
  })
  ## Inherent Annual Loss Expectancy over 10 years
  in_ale_ten <- reactive({
    in_ale() * 10
  })
  # Mode identification function
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  # Residual Annual Loss Expectancy Percentile Calculations
  output$rale_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(c(quantile(rale(), maxDecimals = 4, c(0.1))))
  })
  # Residual Annual Loss Expectancy Most Likely (Mode)
  output$rale_mode <- renderPrint({
    print("Most Likely")
    dollar(c(getmode(rale())))
  })
  output$rale_90 <- renderPrint({
    print("Ninetieth Percentile")  
    dollar(c(quantile(rale(), maxDecimals = 4, c(0.9))))
  })
  output$rale_99 <- renderPrint({
    print("Ninety-Ninth Percentile")  
    dollar(c(quantile(rale(), maxDecimals = 4, c(0.99))))
  })
  # Residual Annual Loss Expectancy over 10 year Percentile Calculations
  output$rale_ten_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(c(quantile(rale_ten(), maxDecimals = 4, c(0.1))))
  })
  output$rale_ten_90 <- renderPrint({
    print("Ninetieth Percentile")  
    dollar(c(quantile(rale_ten(), maxDecimals = 4, c(0.9))))
  })
  output$rale_ten_99 <- renderPrint({
    print("Ninety-Ninth Percentile")  
    dollar(c(quantile(rale_ten(), maxDecimals = 4, c(0.99))))
  })
  # Future Residual Annual Loss Expectancy Percentile Calculations
  output$f_rale_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(c(quantile(f_rale(), maxDecimals = 4, c(0.1))))
  })
  # Future Residual Annual Loss Expectancy Most Likely (Mode)
  output$f_rale_mode <- renderPrint({
    print("Most Likely")
    dollar(c(getmode(f_rale())))
  })
  output$f_rale_90 <- renderPrint({
    print("Ninetieth Percentile")  
    dollar(c(quantile(f_rale(), maxDecimals = 4, c(0.9))))
  })
  output$f_rale_99 <- renderPrint({
    print("Ninety-Ninth Percentile")  
    dollar(c(quantile(f_rale(), maxDecimals = 4, c(0.99))))
  })
  # Future Residual Annual Loss Expectancy over 10 year Percentile Calculations
  output$f_rale_ten_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(c(quantile(f_rale_ten(), maxDecimals = 4, c(0.1))))
  })
  output$f_rale_ten_90 <- renderPrint({
    print("Ninetieth Percentile")  
    dollar(c(quantile(f_rale_ten(), maxDecimals = 4, c(0.9))))
  })
  output$f_rale_ten_99 <- renderPrint({
    print("Ninety-Ninth Percentile")  
    dollar(c(quantile(f_rale_ten(), maxDecimals = 4, c(0.99))))
  })
  # Inherent Annual Loss Expectancy Percentile Calculations
  output$in_ale_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(c(quantile(in_ale(), maxDecimals = 4, c(0.1))))
  })
  # Inherent Annual Loss Expectancy Most Likely (Mode)
  output$in_ale_mode <- renderPrint({
    print("Most Likely")
    dollar(c(getmode(in_ale())))
  })
  output$in_ale_90 <- renderPrint({
    print("Ninetieth Percentile")
    dollar(c(quantile(in_ale(), maxDecimals = 4, c(0.9))))
  })
  output$in_ale_99 <- renderPrint({
    print("Ninety-Ninth Percentile")
    dollar(c(quantile(in_ale(), maxDecimals = 4, c(0.99))))
  })
  # Inherent Annual Loss Expectancy over 10 years Percentile Calculations
  output$in_ale_ten_10 <- renderPrint({
    print("Tenth Percentile")
    dollar(c(quantile(in_ale_ten(), maxDecimals = 4, c(0.1))))
  })
  output$in_ale_ten_90 <- renderPrint({
    print("Ninetieth Percentile")
    dollar(c(quantile(in_ale_ten(), maxDecimals = 4, c(0.9))))
  })
  output$in_ale_ten_99 <- renderPrint({
    print("Ninety-Ninth Percentile")
    dollar(c(quantile(in_ale_ten(), maxDecimals = 4, c(0.99))))
  })
  # Current Vulnerability
  output$r_vuln <- renderPrint({
    print(percent(vuln()))
  })
  # Future Vulnerability
  output$f_r_vuln <- renderPrint({
    print(percent(f_vuln()))
  })

  # Inherent Risk Renders
  
  output$in_lkh_sum <- renderPrint({
    
    summary(in_lkh())
  })
  
  ilb <- reactive({
    cbind(input$in_lkh_bins)
  })
  
  output$in_lkh_hist <- renderPlot({
    req(in_lkh())
    par(xpd=TRUE)
    hist(in_lkh(), col = "#00563f", breaks = ilb(), labels = TRUE)
  })
  
  output$in_ipt_sum <- renderPrint({
    
    dollar(c(summary(in_ipt())))
  })
  
  iib <- reactive({
    cbind(input$in_ipt_bins)
  })
  
  output$in_ipt_hist <- renderPlot({
    req(in_ipt())
    par(xpd=TRUE)
    hist(in_ipt(), col = "#00563f", breaks = iib(), labels = TRUE)
  })
  # Inherent Annual Loss Expectancy Summary
  output$in_ale_sum <- renderPrint({
    
    dollar(c(summary(in_ale())))
  })
  
  # Inherent Annual Loss Expectancy over 10 years Summary
  output$in_ale_ten_sum <- renderPrint({
    
    dollar(c(summary(in_ale_ten())))
  })
  
  iab <- reactive({
    cbind(input$in_ale_bins)
  })
  
  output$in_ale_hist <- renderPlot({
    req(in_ale())
    par(xpd=TRUE)
    hist(in_ale(), col = "#00563f",  breaks = iab(), labels = TRUE)
  })
  
  # Current Residual Risk Renders
  
  output$lkh_sum <- renderPrint({
    
    summary(lkh())
  })
  
  lb <- reactive({
    cbind(input$lkh_bins)
  })
  
  output$lkh_hist <- renderPlot({
    req(lkh())
    par(xpd=TRUE)
    hist(lkh(), col = "#00563f",  breaks = lb(), labels = TRUE)
  })
  
  output$ipt_sum <- renderPrint({
    
    dollar(c(summary(ipt())))
  })
  
  ib <- reactive({
    cbind(input$ipt_bins)
  })
  
  output$ipt_hist <- renderPlot({
    req(ipt())
    par(xpd=TRUE)
    hist(ipt(), col = "#00563f",  breaks = ib(), labels = TRUE)
  })
  # Current Residual Annual Loss Expectancy Summary
  output$rale_sum <- renderPrint({
    
    dollar(c(summary(rale())))
  })
  
  # Current Residual Annual Loss Expectancy over 10 years Summary
  output$rale_ten_sum <- renderPrint({
    
    dollar(c(summary(rale_ten())))
  })
  
  rab <- reactive({
    cbind(input$rale_bins)
  })
  
  output$rale_hist_1 <- renderPlot({
    req(rale())
    par(xpd=TRUE)
    hist(rale(), col = "#00563f",  breaks = rab(), labels = TRUE)
  })
  
  output$rale_hist_2 <- renderPlot({
    req(rale())
    par(xpd=TRUE)
    rh <- hist(rale(), plot=FALSE)
    rh$density = rh$counts/sum(rh$counts)*100
    plot(rh, freq=FALSE, col = "#00563f", labels = TRUE)
  })
  
  # Future Residual Risk Renders
  
  output$f_lkh_sum <- renderPrint({
    
    summary(f_lkh())
  })
  
  f_lb <- reactive({
    cbind(input$f_lkh_bins)
  })
  
  output$f_lkh_hist <- renderPlot({
    req(f_lkh())
    par(xpd=TRUE)
    hist(f_lkh(), col = "#00563f",  breaks = lb(), labels = TRUE)
  })
  
  output$f_ipt_sum <- renderPrint({
    
    dollar(c(summary(ipt())))
  })
  
  f_ib <- reactive({
    cbind(input$f_ipt_bins)
  })
  
  output$f_ipt_hist <- renderPlot({
    req(ipt())
    par(xpd=TRUE)
    hist(ipt(), col = "#00563f",  breaks = ib(), labels = TRUE)
  })
  # Future Residual Annual Loss Expectancy Summary
  output$f_rale_sum <- renderPrint({
    
    dollar(c(summary(f_rale())))
  })
  
  # Future Residual Annual Loss Expectancy over 10 years Summary
  output$f_rale_ten_sum <- renderPrint({
    
    dollar(c(summary(f_rale_ten())))
  })
  
  f_rab <- reactive({
    cbind(input$f_rale_bins)
  })
  
  output$f_rale_hist_1 <- renderPlot({
    req(f_rale())
    par(xpd=TRUE)
    hist(f_rale(), col = "#00563f",  breaks = rab(), labels = TRUE)
  })
  
  output$f_rale_hist_2 <- renderPlot({
    req(f_rale())
    par(xpd=TRUE)
    f_rh <- hist(f_rale(), plot=FALSE)
    f_rh$density = f_rh$counts/sum(f_rh$counts)*100
    plot(f_rh, freq=FALSE, col = "#00563f", labels = TRUE)
  })
  
  # Change Theme
 
  observe({
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) light else dark
    )
  }) 
}

# Run the app ----
shinyApp(ui = ui, server = server)
