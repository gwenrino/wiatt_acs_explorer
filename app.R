library(shiny)
library(shinydashboard)
library(tidycensus)
library(DT)
library(rsconnect)

# Source helper functions
source("helpers.R")

sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("file", "Choose CSV File", accept = c(".csv")),
    menuItem("Info about App", tabName = "info"),
    menuItem("Age Data", tabName = "age"),
    menuItem("Sex Data", tabName = "sex"),
    menuItem("Race/Ethnicity Data", tabName = "race_ethnicity"),
    menuItem("Home Language Data", tabName = "language"),
    menuItem("Household Income Data", tabName = "income"),
    menuItem("Households with Children Data", tabName = "hh_w_children"),
    menuItem("Education Level Data", tabName = "education"),
    menuItem("Disability Status Data", tabName = "disability_status"),
    menuItem("Disability Type Data", tabName = "disability_type"),
    menuItem("Veteran Status Data", tabName = "veteran_status"),
    menuItem("Internet Access Data", tabName = "internet_access"),
    menuItem("Summary Demographic Data", tabName = "summary")
  )
)

body <- dashboardBody(
  tabItems(
    
    tabItem(tabName = "info",
            h2("Information"),
            h3("What This App Does"),
            p("This app takes as input a .csv file of census tracts and returns demographic information from the 2020 ACS survey for each tract."),
            p("It also returns a summary table of the demographics of all the tracts on your list (see Summary Demographic Data tab)."),
            p("Be patient! Results may take several minutes to load, especially in populous states. The Disability Type tab is especially slow because it requires six separate queries to the ACS database.", style = "color:red"),
            h3("Details about Input"),
            p("Use the button on the sidebar to select a .csv file of the census tracts of interest from your file directory."),
            p("One column ", span("must ", style = "color:red"), "contain the official geoid of the tract and be titled 'geoid' (no caps)."),
            p("One column ", span("must ", style = "color:red"), "contain the official two-digit state code and be titled 'state_code'."),
            p("Here is an example of the columns the .csv file must include. The file may include other columns."),
            img(src = "sample_tract_data.png", height = 240, width = 240)),
    
    tabItem(tabName = "age",
            
            fluidRow(
              box(title = "Summary of Age Data for Region", 
                  plotOutput("agePlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Sex by Age"),
                p("Table B01001. 
                Variables 003, 004, 005, 006, 007, 008, 009, 
                010, 011, 012, 013, 014, 015, 016, 017, 018, 019, 
                020, 021, 022, 023, 024, 025, 027, 028, 029,
                030, 031, 032, 033, 034, 035, 036, 037, 038, 039,
                040, 041, 042, 043, 044, 045, 046, 047, 048, 049"),
                p("Variables collapsed and rebucketed to 'Age Under 18', 'Age 18 to 24', 'Age 25 to 44', 'Age 45 to 64', 'Age 65 and Over.'"),
                strong("Total = All Individuals"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Age Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_age_data", "Download"), 
                  width = 3),
              
              box(title = "Age Data by Census Tract", 
                  DTOutput("ageTable"),
                  width = 9)
              )
            ),
    
    tabItem(tabName = "sex",

            fluidRow(
              box(title = "Summary of Sex Data for Region",
                  plotOutput("sexPlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Sex by Age"),
                p("Table B01001. Variables 002, 026"),
                strong("Total = All Individuals"),
                width = 12
              )
            ),

            fluidRow(
              box("Click this button to download raw Sex Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_sex_data", "Download"),
                  width = 3),

              box(title = "Sex Data by Census Tract",
                  DTOutput("sexTable"),
                  width = 9)
            )
          ),
    
    tabItem(tabName = "race_ethnicity",
            
            fluidRow(
              box(title = "Summary of Race/Ethnicity Data for Region",
                  plotOutput("race_ethnicityPlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Hispanic or Latino Origin by Race"),
                p("Table B03002. Variables 003 (White not Hispanic), 004 (Black not Hispanic), 005 (AIAN not Hispanic), 
                  006 (Asian not Hispanic), 007 (NHOPI not Hispanic), 008 (Other race not Hispanic), 
                  009 (Two or more races not Hispanic), 012 (Hispanic/Latino any race)"),
                strong("Total = All Individuals"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Race/Ethnicity Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_race_ethnicity_data", "Download"),
                  width = 3),
              
              box(title = "Race/Ethnicity Data by Census Tract",
                  DTOutput("race_ethnicityTable"),
                  width = 9)
            )
          ),
    
    tabItem(tabName = "language",
            
            fluidRow(
              box(title = "Summary of Language Data for Region",
                  plotOutput("languagePlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Language Spoken at Home for the Population 5 Years and Over"),
                p("Table C16001. Variables 002 (English only), 003 (Spanish), 006 (French, Haitian, Cajun),
                  009 (German or other West Germanic Language), 012 (Russian, Polish, other Slavic Language),
                  015 (Other Indo-European Language), 018 (Korean), 021 (Chinese, including Mandarin, Cantonese),
                  024 (Vietnamese), 027 (Tagalog), 030 (Other Asian/Pacific Island Language),
                  033 (Arabic), 036 (Other or Unspecified Language)"),
                p("Variables 009, 012, and 015 bucketed together as 'Other European'."),
                strong("Total = Individuals Age 5 Years and Over"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Language Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_language_data", "Download"),
                  width = 3),
              
              box(title = "Language Data by Census Tract",
                  DTOutput("languageTable"),
                  width = 9)
            )
          ),
    
    tabItem(tabName = "income",
            
            fluidRow(
              box(title = "Summary of Income Data for Region",
                  plotOutput("incomePlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Household Income in the Past 12 Months (in 2020 Inflation-Adjusted Dollars)"),
                p("Table B19001. Variables 002, 003, 004, 005, 006, 007, 008, 009,
                  010, 011, 012, 013, 014, 015, 016, 017"),
                p("Variables collapsed and rebucketed to 'Less than 25K', '25K to 50K', '50K to 100K', 'Over 100K'."),
                strong("Total = All Households"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Income Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_income_data", "Download"),
                  width = 3),
              
              box(title = "Income Data by Census Tract",
                  DTOutput("incomeTable"),
                  width = 9)
            )
          ),
    
    tabItem(tabName = "hh_w_children",
            
            fluidRow(
              box(title = "Summary of Households with Children Data for Region",
                  plotOutput("hh_w_childrenPlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Households by Presence of People Under 18 Years of Age by Household Type"),
                p("Table B11005. Variables 002, 011"),
                strong("Total = All Households"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Households with Children Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_hh_w_children_data", "Download"),
                  width = 3),
              
              box(title = "Households with Children Data by Census Tract",
                  DTOutput("hh_w_childrenTable"),
                  width = 9)
            )
          ),
    
    tabItem(tabName = "education",

            fluidRow(
              box(title = "Summary of Education Data for Region",
                  plotOutput("educationPlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Educational Attainment for the Population 25 Years and Over"),
                p("Table B15003. Variables 002, 003, 004, 005, 006, 007, 008, 009, 010,
                  011, 012, 013, 014, 015, 016, 017, 018, 019, 020,
                  021, 022, 023, 024, 025"),
                p("Variables collapsed and rebucketed to 'Did Not Complete High School', 'Graduated High School', 'College with or without Degree', 'Advanced Degree'."),
                strong("Total = Individuals Age 25 and Over"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Education Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_education_data", "Download"),
                  width = 3),
              
              box(title = "Education Data by Census Tract",
                  DTOutput("educationTable"),
                  width = 9)
            )
          ),
    
    tabItem(tabName = "disability_status",
            
            fluidRow(
              box(title = "Summary of Disability Status Data for Region",
                  plotOutput("disability_statusPlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Sex by Age by Disability Status"),
                p("Table B18101. Variables 010, 011, 013, 014, 016, 017, 019, 020,
                  029, 030, 032, 033, 035, 036, 038, 039"),
                p("Variables collapsed and rebucketed to 'Has Disability', 'No Disability'."),
                strong("Total = Individuals Age 18 and Over"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Disability Status Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_disability_status_data", "Download"),
                  width = 3),
              
              box(title = "Disability Status Data by Census Tract",
                  DTOutput("disability_statusTable"),
                  width = 9)
            )
          ),
    
    tabItem(tabName = "disability_type",
            
            fluidRow(
              box(title = "Summary of Disability Type Data for Region",
                  p("This tab takes extra time to load. Be patient!"),
                  plotOutput("disability_typePlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Sex by Age by Hearing Difficulty"),
                p("Table B18102. Variables 010, 011, 013, 014, 016, 017, 019, 020,
                  029, 030, 032, 033, 035, 036, 038, 039"),
                p("Variables collapsed and rebucketed to 'Has Hearing Difficulty', 'No Hearing Difficulty'."),
                strong("Total = Individuals Age 18 and Over"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Hearing Difficulty Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_disability_hearing_data", "Download"),
                  width = 3),
              
              box(title = "Hearing Difficulty Data by Census Tract",
                  DTOutput("disability_hearingTable"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Sex by Age by Vision Difficulty"),
                p("Table B18103. 010, 011, 013, 014, 016, 017, 019, 020,
                  029, 030, 032, 033, 035, 036, 038, 039"),
                p("Variables collapsed and rebucketed to 'Has Vision Difficulty', 'No Vision Difficulty'."),
                strong("Total = Individuals Age 18 and Over"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Vision Difficulty Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_disability_vision_data", "Download"),
                  width = 3),
              
              box(title = "Vision Difficulty Data by Census Tract",
                  DTOutput("disability_visionTable"),
                  width = 9)
              ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Sex by Age by Cognitive Difficulty"),
                p("Table B18104. Variables 007, 008, 010, 011, 013, 014, 016, 017,
                  023, 024, 026, 027, 029, 030, 032, 033"),
                p("Variables collapsed and rebucketed to 'Has Cognitive Difficulty', 'No Cognitive Difficulty'."),
                strong("Total = Individuals Age 18 and Over"),
                width = 12
              )
            ),
              
            fluidRow(
              box("Click this button to download raw Cognitive Difficulty Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_disability_cognitive_data", "Download"),
                  width = 3),
              
              box(title = "Cognitive Difficulty Data by Census Tract",
                  DTOutput("disability_cognitiveTable"),
                  width = 9)
              ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Sex by Age by Ambulatory Difficulty"),
                p("Table B18105. Variables 007, 008, 010, 011, 013, 014, 016, 017,
                  023, 024, 026, 027, 029, 030, 032, 033"),
                p("Variables collapsed and rebucketed to 'Has Ambulatory Difficulty', 'No Ambulatory Difficulty'."),
                strong("Total = Individuals Age 18 and Over"),
                width = 12
              )
            ),
              
            fluidRow(
              box("Click this button to download raw Ambulatory Difficulty Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_disability_ambulatory_data", "Download"),
                  width = 3),
                
              box(title = "Ambulatory Difficulty Data by Census Tract",
                  DTOutput("disability_ambulatoryTable"),
                  width = 9)
              ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Sex by Age by Self Care Difficulty"),
                p("Table B18106. Variables 007, 008, 010, 011, 013, 014, 016, 017,
                  023, 024, 026, 027, 029, 030, 032, 033"),
                p("Variables collapsed and rebucketed to 'Has Self Care Difficulty', 'No Self Care Difficulty'."),
                strong("Total = Individuals Age 18 and Over"),
                width = 12
              )
            ),
                
            fluidRow(
              box("Click this button to download raw Self Care Difficulty Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_disability_self_care_data", "Download"),
                  width = 3),
                  
              box(title = "Self Care Difficulty Data by Census Tract",
                  DTOutput("disability_self_careTable"),
                  width = 9)
              ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Sex by Age by Independent Living Difficulty"),
                p("Table B18107. Variables 004, 005, 007, 008, 010, 011, 013, 014,
                  017, 018, 020, 021, 023, 024, 026, 027"),
                p("Variables collapsed and rebucketed to 'Has Independent Living Difficulty', 'No Independent Living Difficulty'."),
                strong("Total = Individuals Age 18 and Over"),
                width = 12
              )
            ),
                  
              fluidRow(
                box("Click this button to download raw Independent Living Difficulty Data by Census Tract shown on the right",
                    br(),
                    br(),
                    downloadButton("download_disability_independent_living_data", "Download"),
                    width = 3),
                    
                box(title = "Independent Living Difficulty Data by Census Tract",
                    DTOutput("disability_independent_livingTable"),
                    width = 9)
              )
                ),
    
    tabItem(tabName = "veteran_status",
            
            fluidRow(
              box(title = "Summary of Veteran Status Data for Region",
                  plotOutput("veteranPlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Sex by Age by Veteran Status for the Civilian Population 18 Years and Over"),
                p("Table B21001. Variables 002, 003"),
                strong("Total = Civilians Age 18 and Over"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Veteran Status Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_veteran_data", "Download"),
                  width = 3),
              
              box(title = "Veteran Status Data by Census Tract",
                  DTOutput("veteranTable"),
                  width = 9)
            )
          ),
    
    tabItem(tabName = "internet_access",
            
            fluidRow(
              box(title = "Summary of Internet Access Data for Region",
                  plotOutput("internetPlot"),
                  width = 9)
            ),
            
            fluidRow(
              box(
                h4("2020 ACS Variables: Presence and Types of Internet Subscriptions in Household"),
                p("Table B28002. Variables 002 (has internet subscription), 012 (has internet without subscription), 013 (no internet access)"),
                p("Variables collapsed and rebucketed to 'Has Internet Access', 'No Internet Access'."),
                strong("Total = All Households"),
                width = 12
              )
            ),
            
            fluidRow(
              box("Click this button to download raw Internet Access Data by Census Tract shown on the right",
                  br(),
                  br(),
                  downloadButton("download_internet_data", "Download"),
                  width = 3),
              
              box(title = "Internet Access Data by Census Tract",
                  DTOutput("internetTable"),
                  width = 9)
            )
          ), 
    
    tabItem(tabName = "summary",
            
            fluidRow(
              box("Click this button to download Summary Demographic Data shown on the right",
                  br(),
                  br(),
                  downloadButton("download_summary_data", "Download"),
                  width = 3),
              
              box(title = "Summary Demographic Data",
                  p("Note that 'Area Total' is not the same for all variables because the variables do not all refer to the same groups.
                    For example, 'Veteran Status' is among the civilian population over age 18, while 'Internet Access' is among households.
                    See '2020 ACS Variables' information on each tab for details about each variable."),
                  DTOutput("summaryTable"),
                  width = 9)
            )
          )
        )
)

ui <- dashboardPage(
  dashboardHeader(title = "WIATT ACS Data Explorer", titleWidth = 350),
  sidebar,
  body
)

server <- function(input, output) {
  
  # Read the input file
  tract_list <- reactive({
    req(input$file)
    read_csv(input$file$datapath)
  })
  
  # Pull state code(s) to use in ACS query
  state_codes <- reactive({
    req(tract_list())
    
    list_of_state_codes <- tract_list() %>%
      mutate(state_code = as.integer(state_code)) %>% # confirm state_code is an integer
      select(state_code) %>%
      unique() %>% pull()
    
  })
  
  ### AGE ###
  
  process_age_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get age data for all state codes
    age_list <- lapply(codes, get_age_data)
    
    # Combine the list of age dataframes into one dataframe
    age_all <- do.call(rbind, age_list)

    # Process data
    age_all <- process_acs_pull(age_all)
    age_all$geoid <- as.character(age_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)

    # Filter to relevant tracts
    age_data <- age_all %>% 
      filter(geoid %in% tracts$geoid)

    # Transform
    age_data <- age_data %>%
      mutate(age_under_18 = male_under_5 + male_5_to_9 + male_10_to_14 + male_15_to_17 +
               female_under_5 + female_5_to_9 + female_10_to_14 + female_15_to_17,
             age_18_to_24 = male_18_and_19 + male_20 + male_21 + male_22_to_24 +
               female_18_and_19 + female_20 + female_21 + female_22_to_24,
             age_25_to_44 = male_25_to_29 + male_30_to_34 + male_35_to_39 + male_40_to_44 +
               female_25_to_29 + female_30_to_34 + female_35_to_39 + female_40_to_44,
             age_45_to_64 = male_45_to_49 + male_50_to_54 + male_55_to_59 + male_60_and_61 + male_62_to_64 +
               female_45_to_49 + female_50_to_54 + female_55_to_59 + female_60_and_61 + female_62_to_64,
             age_65_and_over = male_65_and_66 + male_67_to_69 + male_70_to_74 + male_75_to_79 + male_80_to_84 + male_over_85 +
               female_65_and_66 + female_67_to_69 + female_70_to_74 + female_75_to_79 + female_80_to_84 + female_over_85) %>%
      select(geoid, name, age_under_18, age_18_to_24, age_25_to_44, age_45_to_64, age_65_and_over)

    return(age_data)

  })
  
  output$ageTable <- renderDT({
    datatable(process_age_data(), 
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_age_data <- downloadHandler(
    filename = function() {
      paste("age_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_age_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be pulled on final tab)
  create_age_summary <- reactive({
    req(process_age_data())
    
    age_data_summary <- summarize_tract_data(process_age_data())
    
    age_data_summary <- age_data_summary %>% 
      mutate(Category = "Age")
    
  })

  # Output age plot
  output$agePlot <- renderPlot({
    req(create_age_summary())

    age_data_to_graph <- create_age_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(age = Variable)

    age_data_to_graph$age <- str_sub(age_data_to_graph$age, 5, -1)

    age_data_to_graph$age <- factor(age_data_to_graph$age,
                                    levels = c("under_18",
                                               "18_to_24",
                                               "25_to_44",
                                               "45_to_64",
                                               "65_and_over"))

    ggplot(data = age_data_to_graph,
                        aes(x = age, y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Individuals in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  ### SEX ###

  process_sex_data <- reactive({
    req(tract_list(), state_codes())

    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())

    # Get sex data for all state codes
    sex_list <- lapply(codes, get_sex_data)

    # Combine the list of sex dataframes into one dataframe
    sex_all <- do.call(rbind, sex_list)

    # Process data
    sex_all <- process_acs_pull(sex_all)
    sex_all$geoid <- as.character(sex_all$geoid)

    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)

    # Filter to relevant tracts
    sex_data <- sex_all %>% 
      filter(geoid %in% tracts$geoid)

    return(sex_data)

  })

  output$sexTable <- renderDT({
    datatable(process_sex_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })

  # Downloadable final data
  output$download_sex_data <- downloadHandler(
    filename = function() {
      paste("sex_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_sex_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be pulled on final tab)
  create_sex_summary <- reactive({
    req(process_sex_data())
    
    sex_data_summary <- summarize_tract_data(process_sex_data())
    
    sex_data_summary <- sex_data_summary %>%
      mutate(Category = "Sex")
  })

  # Output sex plot
  output$sexPlot <- renderPlot({
    req(create_sex_summary())

    sex_data_to_graph <- create_sex_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(sex = Variable)

    ggplot(data = sex_data_to_graph,
           aes(x = sex, y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Individuals in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  ### RACE/ETHNICITY ###
  
  process_race_ethnicity_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get race_ethnicity data for all state codes
    race_ethnicity_list <- lapply(codes, get_race_ethnicity_data)
    
    # Combine the list of race_ethnicity dataframes into one dataframe
    race_ethnicity_all <- do.call(rbind, race_ethnicity_list)
    
    # Process data
    race_ethnicity_all <- process_acs_pull(race_ethnicity_all)
    race_ethnicity_all$geoid <- as.character(race_ethnicity_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    race_ethnicity_data <- race_ethnicity_all %>% 
      filter(geoid %in% tracts$geoid)
    
    return(race_ethnicity_data)
    
  })
  
  output$race_ethnicityTable <- renderDT({
    datatable(process_race_ethnicity_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_race_ethnicity_data <- downloadHandler(
    filename = function() {
      paste("race_ethnicity_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_race_ethnicity_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be pulled on final tab)
  create_race_ethnicity_summary <- reactive({
    req(process_race_ethnicity_data())
    
    race_ethnicity_data_summary <- summarize_tract_data(process_race_ethnicity_data())
    
    race_ethnicity_data_summary <- race_ethnicity_data_summary %>%
      mutate(Category = "Race/Ethnicity")
  })
  
  # Output race_ethnicity plot
  output$race_ethnicityPlot <- renderPlot({
    req(create_race_ethnicity_summary())
    
    race_ethnicity_data_to_graph <- create_race_ethnicity_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(race_ethnicity = Variable)
    
    ggplot(data = race_ethnicity_data_to_graph,
           aes(x = reorder(race_ethnicity, Portion), y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Individuals in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  ### LANGUAGE ###
  
  process_language_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get language data for all state codes
    language_list <- lapply(codes, get_language_data)
    
    # Combine the list of language dataframes into one dataframe
    language_all <- do.call(rbind, language_list)
    
    # Process data
    language_all <- process_acs_pull(language_all)
    language_all$geoid <- as.character(language_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    language_data <- language_all %>% 
      filter(geoid %in% tracts$geoid)
    
    # Transform
    language_data <- language_data %>%
      mutate(Other_European = german_westgermanic + russian_polish_slavic + other_indo_european) %>%
      select(geoid, name, English, Spanish, French_Haitian_Cajun, Other_European, 
             Korean, Chinese, Vietnamese, Tagalog, Other_Asian_Pac_Island, Arabic, Other_or_unspecified)
    
    return(language_data)
    
  })
  
  output$languageTable <- renderDT({
    datatable(process_language_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_language_data <- downloadHandler(
    filename = function() {
      paste("language_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_language_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be pulled on final tab)
  create_language_summary <- reactive({
    req(process_language_data())
    
    language_data_summary <- summarize_tract_data(process_language_data())
    
    language_data_summary <- language_data_summary %>%
      mutate(Category = "Home Language")
  })
  
  # Output language plot
  output$languagePlot <- renderPlot({
    req(create_language_summary())
    
    language_data_to_graph <- create_language_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(language = Variable)
    
    ggplot(data = language_data_to_graph,
           aes(x = reorder(language, Portion), y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Individuals over Age 5 in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  ### INCOME ###
  
  process_income_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get income data for all state codes
    income_list <- lapply(codes, get_income_data)
    
    # Combine the list of income dataframes into one dataframe
    income_all <- do.call(rbind, income_list)
    
    # Process data
    income_all <- process_acs_pull(income_all)
    income_all$geoid <- as.character(income_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    income_data <- income_all %>% 
      filter(geoid %in% tracts$geoid)
    
    # Transform
    income_data <- income_data %>%
      mutate(income_less_than_25k = income_less_than_10k + income_10k_to_15k + income_15k_to_20k + income_20k_to_25k,
             income_25k_to_50k = income_25k_to_30k + income_30k_to_35k + income_35k_to_40k + income_40k_to_45k + income_45k_to_50k,
             income_50k_to_100k = income_50k_to_60k + income_60k_to_75k + income_75k_to_100k,
             income_over_100k = income_100k_to_125k + income_125k_to_150k + income_150k_to_200k + income_over_200k) %>%
      select(geoid, name, income_less_than_25k, income_25k_to_50k, income_50k_to_100k, income_over_100k)
    
    return(income_data)
    
  })
  
  output$incomeTable <- renderDT({
    datatable(process_income_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_income_data <- downloadHandler(
    filename = function() {
      paste("income_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_income_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be pulled on final tab)
  create_income_summary <- reactive({
    req(process_income_data())
    
    income_data_summary <- summarize_tract_data(process_income_data())
    
    income_data_summary <- income_data_summary %>%
      mutate(Category = "Household Income")
  })
  
  # Output income plot
  output$incomePlot <- renderPlot({
    req(create_income_summary())
    
    income_data_to_graph <- create_income_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(income = Variable)
    
    income_data_to_graph$income <- str_sub(income_data_to_graph$income, 8, -1)

    income_data_to_graph$income <- factor(income_data_to_graph$income,
                                          levels = c("less_than_25k",
                                                     "25k_to_50k",
                                                     "50k_to_100k",
                                                     "over_100k"))

    ggplot(data = income_data_to_graph,
           aes(x = income, y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Households in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  ### CHILDREN ###
  
  process_hh_w_children_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get hh_w_children data for all state codes
    hh_w_children_list <- lapply(codes, get_hh_w_children_data)
    
    # Combine the list of hh_w_children dataframes into one dataframe
    hh_w_children_all <- do.call(rbind, hh_w_children_list)
    
    # Process data
    hh_w_children_all <- process_acs_pull(hh_w_children_all)
    hh_w_children_all$geoid <- as.character(hh_w_children_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    hh_w_children_data <- hh_w_children_all %>% 
      filter(geoid %in% tracts$geoid)
    
    return(hh_w_children_data)
    
  })
  
  output$hh_w_childrenTable <- renderDT({
    datatable(process_hh_w_children_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_hh_w_children_data <- downloadHandler(
    filename = function() {
      paste("hh_w_children_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_hh_w_children_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be pulled on final tab)
  create_hh_w_children_summary <- reactive({
    req(process_hh_w_children_data())
    
    hh_w_children_data_summary <- summarize_tract_data(process_hh_w_children_data())
    
    hh_w_children_data_summary <- hh_w_children_data_summary %>%
      mutate(Category = "Households with Children")
  })
  
  # Output hh_w_children plot
  output$hh_w_childrenPlot <- renderPlot({
    req(create_hh_w_children_summary())
    
    hh_w_children_data_to_graph <- create_hh_w_children_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(hh_w_children = Variable)
    
    ggplot(data = hh_w_children_data_to_graph,
           aes(x = hh_w_children, y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Households in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  ### EDUCATION ###
  
  process_education_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get education data for all state codes
    education_list <- lapply(codes, get_education_data)
    
    # Combine the list of education dataframes into one dataframe
    education_all <- do.call(rbind, education_list)
    
    # Process data
    education_all <- process_acs_pull(education_all)
    education_all$geoid <- as.character(education_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    education_data <- education_all %>% 
      filter(geoid %in% tracts$geoid)
    
    # Transform
    education_data <- education_data %>%
      mutate(did_not_complete_high_school = no_schooling + nursery_school + kindergarten + grade1 + 
               grade2 + grade3 + grade4 + grade5 + grade6 + grade7 + grade8 + grade9 + grade10 + grade11 + grade12_no_diploma,
             graduated_high_school = high_school_diploma + ged,
             college_w_or_wo_degree = less_than_1_year_college + some_college_no_degree + associates_degree + bachelors_degree,
             graduate_or_prof_degree = masters_degree + professional_degree + doctorate_degree) %>%
      select(geoid, name, did_not_complete_high_school, graduated_high_school, college_w_or_wo_degree, graduate_or_prof_degree)
    
    return(education_data)
    
  })
  
  output$educationTable <- renderDT({
    datatable(process_education_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_education_data <- downloadHandler(
    filename = function() {
      paste("education_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_education_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be pulled on final tab)
  create_education_summary <- reactive({
    req(process_education_data())
    
    education_data_summary <- summarize_tract_data(process_education_data())
    
    education_data_summary <- education_data_summary %>%
      mutate(Category = "Education Level")
  })
  
  # Output education plot
  output$educationPlot <- renderPlot({
    req(create_education_summary())
    
    education_data_to_graph <- create_education_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(education = Variable)
    
    education_data_to_graph$education <- factor(education_data_to_graph$education, 
                                                levels = c("did_not_complete_high_school", 
                                                           "graduated_high_school", 
                                                           "college_w_or_wo_degree",
                                                            "graduate_or_prof_degree"))
    
    ggplot(data = education_data_to_graph,
           aes(x = education, y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Individuals over Age 25 in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })

### DISABILITY STATUS ###

  process_disability_status_data <- reactive({
    req(tract_list(), state_codes())
  
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
  
    # Get disability_status data for all state codes
    disability_status_list <- lapply(codes, get_disability_status_data)
  
    # Combine the list of disability_status dataframes into one dataframe
    disability_status_all <- do.call(rbind, disability_status_list)
  
    # Process data
    disability_status_all <- process_acs_pull(disability_status_all)
    disability_status_all$geoid <- as.character(disability_status_all$geoid)
  
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
  
    # Filter to relevant tracts
    disability_status_data <- disability_status_all %>% 
      filter(geoid %in% tracts$geoid)
  
    # Transform
    disability_status_data <- disability_status_data %>%
      mutate(has_disability = select(., starts_with("disability")) %>% rowSums,
             no_disability = select(., starts_with("no_")) %>% rowSums) %>%
      select(geoid, name, has_disability, no_disability)
  
    return(disability_status_data)
  
  })

  output$disability_statusTable <- renderDT({
    datatable(process_disability_status_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })

  # Downloadable final data
  output$download_disability_status_data <- downloadHandler(
    filename = function() {
      paste("disability_status_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_disability_status_data(), file, row.names = F)
    }
  )

# Summarize data (to be pulled on final tab)
  create_disability_status_summary <- reactive({
    req(process_disability_status_data())
    
    disability_status_data_summary <- summarize_tract_data(process_disability_status_data())
    
    disability_status_data_summary <- disability_status_data_summary %>%
      mutate(Category = "Disability Status")
  })

  # Output disability_status plot
  output$disability_statusPlot <- renderPlot({
    req(create_disability_status_summary())
  
    disability_status_data_to_graph <- create_disability_status_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(disability_status = Variable)
  
    ggplot(data = disability_status_data_to_graph,
           aes(x = disability_status, y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Individuals over Age 18 in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  ### DISABILITY TYPE ###
  
  # Hearing
  process_disability_hearing_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get disability_hearing data for all state codes
    disability_hearing_list <- lapply(codes, get_hearing_data)
    
    # Combine the list of disability_hearing dataframes into one dataframe
    disability_hearing_all <- do.call(rbind, disability_hearing_list)
    
    # Process data
    disability_hearing_all <- process_acs_pull(disability_hearing_all)
    disability_hearing_all$geoid <- as.character(disability_hearing_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    disability_hearing_data <- disability_hearing_all %>% 
      filter(geoid %in% tracts$geoid)
    
    # Transform
    disability_hearing_data <- disability_hearing_data %>%
      mutate(has_hearing_difficulty = select(., starts_with("hearing")) %>% rowSums,
             no_hearing_difficulty = select(., starts_with("no_")) %>% rowSums) %>%
      select(geoid, name, has_hearing_difficulty, no_hearing_difficulty)
    
    return(disability_hearing_data)
    
  })
  
  output$disability_hearingTable <- renderDT({
    datatable(process_disability_hearing_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_disability_hearing_data <- downloadHandler(
    filename = function() {
      paste("disability_hearing_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_disability_hearing_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be rbinded for graph and pulled on final tab)
  create_disability_hearing_summary <- reactive({
    req(process_disability_hearing_data())
    disability_hearing_data_summary <- summarize_tract_data(process_disability_hearing_data())
  })
  
  # Vision
  process_disability_vision_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get disability_vision data for all state codes
    disability_vision_list <- lapply(codes, get_vision_data)
    
    # Combine the list of disability_vision dataframes into one dataframe
    disability_vision_all <- do.call(rbind, disability_vision_list)
    
    # Process data
    disability_vision_all <- process_acs_pull(disability_vision_all)
    disability_vision_all$geoid <- as.character(disability_vision_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    disability_vision_data <- disability_vision_all %>% 
      filter(geoid %in% tracts$geoid)
    
    # Transform
    disability_vision_data <- disability_vision_data %>%
      mutate(has_vision_difficulty = select(., starts_with("vision")) %>% rowSums,
             no_vision_difficulty = select(., starts_with("no_")) %>% rowSums) %>%
      select(geoid, name, has_vision_difficulty, no_vision_difficulty)
    
    return(disability_vision_data)
    
  })
  
  output$disability_visionTable <- renderDT({
    datatable(process_disability_vision_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_disability_vision_data <- downloadHandler(
    filename = function() {
      paste("disability_vision_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_disability_vision_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be rbinded for graph and pulled on final tab)
  create_disability_vision_summary <- reactive({
    req(process_disability_vision_data())
    disability_vision_data_summary <- summarize_tract_data(process_disability_vision_data())
  })
  
  # Cognitive
  process_disability_cognitive_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get disability_cognitive data for all state codes
    disability_cognitive_list <- lapply(codes, get_cognitive_data)
    
    # Combine the list of disability_cognitive dataframes into one dataframe
    disability_cognitive_all <- do.call(rbind, disability_cognitive_list)
    
    # Process data
    disability_cognitive_all <- process_acs_pull(disability_cognitive_all)
    disability_cognitive_all$geoid <- as.character(disability_cognitive_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    disability_cognitive_data <- disability_cognitive_all %>% 
      filter(geoid %in% tracts$geoid)
    
    # Transform
    disability_cognitive_data <- disability_cognitive_data %>%
      mutate(has_cognitive_difficulty = select(., starts_with("cognitive")) %>% rowSums,
             no_cognitive_difficulty = select(., starts_with("no_")) %>% rowSums) %>%
      select(geoid, name, has_cognitive_difficulty, no_cognitive_difficulty)
    
    return(disability_cognitive_data)
    
  })
  
  output$disability_cognitiveTable <- renderDT({
    datatable(process_disability_cognitive_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_disability_cognitive_data <- downloadHandler(
    filename = function() {
      paste("disability_cognitive_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_disability_cognitive_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be rbinded for graph and pulled on final tab)
  create_disability_cognitive_summary <- reactive({
    req(process_disability_cognitive_data())
    disability_cognitive_data_summary <- summarize_tract_data(process_disability_cognitive_data())
  })
  
  # Ambulatory
  process_disability_ambulatory_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get disability_ambulatory data for all state codes
    disability_ambulatory_list <- lapply(codes, get_ambulatory_data)
    
    # Combine the list of disability_ambulatory dataframes into one dataframe
    disability_ambulatory_all <- do.call(rbind, disability_ambulatory_list)
    
    # Process data
    disability_ambulatory_all <- process_acs_pull(disability_ambulatory_all)
    disability_ambulatory_all$geoid <- as.character(disability_ambulatory_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    disability_ambulatory_data <- disability_ambulatory_all %>% 
      filter(geoid %in% tracts$geoid)
    
    # Transform
    disability_ambulatory_data <- disability_ambulatory_data %>%
      mutate(has_ambulatory_difficulty = select(., starts_with("ambulatory")) %>% rowSums,
             no_ambulatory_difficulty = select(., starts_with("no_")) %>% rowSums) %>%
      select(geoid, name, has_ambulatory_difficulty, no_ambulatory_difficulty)
    
    return(disability_ambulatory_data)
    
  })
  
  output$disability_ambulatoryTable <- renderDT({
    datatable(process_disability_ambulatory_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_disability_ambulatory_data <- downloadHandler(
    filename = function() {
      paste("disability_ambulatory_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_disability_ambulatory_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be rbinded for graph and pulled on final tab)
  create_disability_ambulatory_summary <- reactive({
    req(process_disability_ambulatory_data())
    disability_ambulatory_data_summary <- summarize_tract_data(process_disability_ambulatory_data())
  })
  
  # Self Care
  process_disability_self_care_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get disability_self_care data for all state codes
    disability_self_care_list <- lapply(codes, get_self_care_data)
    
    # Combine the list of disability_self_care dataframes into one dataframe
    disability_self_care_all <- do.call(rbind, disability_self_care_list)
    
    # Process data
    disability_self_care_all <- process_acs_pull(disability_self_care_all)
    disability_self_care_all$geoid <- as.character(disability_self_care_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    disability_self_care_data <- disability_self_care_all %>% 
      filter(geoid %in% tracts$geoid)
    
    # Transform
    disability_self_care_data <- disability_self_care_data %>%
      mutate(has_self_care_difficulty = select(., starts_with("self_care")) %>% rowSums,
             no_self_care_difficulty = select(., starts_with("no_")) %>% rowSums) %>%
      select(geoid, name, has_self_care_difficulty, no_self_care_difficulty)
    
    return(disability_self_care_data)
    
  })
  
  output$disability_self_careTable <- renderDT({
    datatable(process_disability_self_care_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_disability_self_care_data <- downloadHandler(
    filename = function() {
      paste("disability_self_care_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_disability_self_care_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be rbinded for graph and pulled on final tab)
  create_disability_self_care_summary <- reactive({
    req(process_disability_self_care_data())
    disability_self_care_data_summary <- summarize_tract_data(process_disability_self_care_data())
  })
  
  # Independent Living
  process_disability_independent_living_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get disability_independent_living data for all state codes
    disability_independent_living_list <- lapply(codes, get_independent_living_data)
    
    # Combine the list of disability_independent_living dataframes into one dataframe
    disability_independent_living_all <- do.call(rbind, disability_independent_living_list)
    
    # Process data
    disability_independent_living_all <- process_acs_pull(disability_independent_living_all)
    disability_independent_living_all$geoid <- as.character(disability_independent_living_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    disability_independent_living_data <- disability_independent_living_all %>% 
      filter(geoid %in% tracts$geoid)
    
    # Transform
    disability_independent_living_data <- disability_independent_living_data %>%
      mutate(has_independent_living_difficulty = select(., starts_with("independent_living")) %>% rowSums,
             no_independent_living_difficulty = select(., starts_with("no_")) %>% rowSums) %>%
      select(geoid, name, has_independent_living_difficulty, no_independent_living_difficulty)
    
    return(disability_independent_living_data)
    
  })
  
  output$disability_independent_livingTable <- renderDT({
    datatable(process_disability_independent_living_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_disability_independent_living_data <- downloadHandler(
    filename = function() {
      paste("disability_independent_living_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_disability_independent_living_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be rbinded for graph)
  create_disability_independent_living_summary <- reactive({
    req(process_disability_independent_living_data())
    disability_independent_living_data_summary <- summarize_tract_data(process_disability_independent_living_data())
  })
  
  # Rbind data (to be pulled on final tab)
  create_disability_type_summary <- reactive({
    req(create_disability_hearing_summary(),
        create_disability_vision_summary(),
        create_disability_cognitive_summary(),
        create_disability_ambulatory_summary(),
        create_disability_self_care_summary(),
        create_disability_independent_living_summary())
    
    disability_type_summary <- bind_rows(create_disability_hearing_summary(),
                                         create_disability_vision_summary(),
                                         create_disability_cognitive_summary(),
                                         create_disability_ambulatory_summary(),
                                         create_disability_self_care_summary(),
                                         create_disability_independent_living_summary())
    
    # Filter out the counts of those without disabilities
    disability_type_summary <- disability_type_summary[grep('^has_', disability_type_summary$Variable),]
    
    disability_type_summary <- disability_type_summary %>%
      mutate(Category = "Disability Type")
    
  })
  
  # Output disability_type plot
  output$disability_typePlot <- renderPlot({
    req(create_disability_type_summary())
    
    disability_type_data_to_graph <- create_disability_type_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(disability_type = Variable)
    
    ggplot(data = disability_type_data_to_graph,
           aes(x = reorder(disability_type, Portion), y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Individuals over Age 18 in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  ### VETERAN STATUS ###
  
  process_veteran_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get veteran data for all state codes
    veteran_list <- lapply(codes, get_veteran_data)
    
    # Combine the list of veteran dataframes into one dataframe
    veteran_all <- do.call(rbind, veteran_list)
    
    # Process data
    veteran_all <- process_acs_pull(veteran_all)
    veteran_all$geoid <- as.character(veteran_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    veteran_data <- veteran_all %>% 
      filter(geoid %in% tracts$geoid)
    
    return(veteran_data)
    
  })
  
  output$veteranTable <- renderDT({
    datatable(process_veteran_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_veteran_data <- downloadHandler(
    filename = function() {
      paste("veteran_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_veteran_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be pulled on final tab)
  create_veteran_summary <- reactive({
    req(process_veteran_data())
    
    veteran_data_summary <- summarize_tract_data(process_veteran_data())
    
    veteran_data_summary <- veteran_data_summary %>%
      mutate(Category = "Veteran Status")
  })
  
  # Output veteran plot
  output$veteranPlot <- renderPlot({
    req(create_veteran_summary())
    
    veteran_data_to_graph <- create_veteran_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(veteran = Variable)
    
    ggplot(data = veteran_data_to_graph,
           aes(x = veteran, y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Civilians over Age 18 in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  ### INTERNET ACCESS ###
  
  process_internet_data <- reactive({
    req(tract_list(), state_codes())
    
    # state_codes() must be two digits
    codes <- sprintf("%02d", state_codes())
    
    # Get internet data for all state codes
    internet_list <- lapply(codes, get_internet_data)
    
    # Combine the list of internet dataframes into one dataframe
    internet_all <- do.call(rbind, internet_list)
    
    # Process data
    internet_all <- process_acs_pull(internet_all)
    internet_all$geoid <- as.character(internet_all$geoid)
    
    tracts <- tract_list()
    tracts$geoid <- as.character(tracts$geoid)
    
    # Filter to relevant tracts
    internet_data <- internet_all %>% 
      filter(geoid %in% tracts$geoid)
    
    # Transform
    internet_data <- internet_data %>%
      mutate(has_internet_access = internet_subscription + internet_no_subscription) %>%
      select(geoid, name, has_internet_access, no_internet_access)
    
    return(internet_data)
    
  })
  
  output$internetTable <- renderDT({
    datatable(process_internet_data(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
  # Downloadable final data
  output$download_internet_data <- downloadHandler(
    filename = function() {
      paste("internet_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(process_internet_data(), file, row.names = F)
    }
  )
  
  # Summarize data (to be pulled on final tab)
  create_internet_summary <- reactive({
    req(process_internet_data())
    
    internet_data_summary <- summarize_tract_data(process_internet_data())
    
    internet_data_summary <- internet_data_summary %>%
      mutate(Category = "Household Internet Access")
  })
  
  # Output internet plot
  output$internetPlot <- renderPlot({
    req(create_internet_summary())
    
    internet_data_to_graph <- create_internet_summary() %>%
      select(Variable, `Tract Total`, Portion) %>%
      rename(internet = Variable)
    
    ggplot(data = internet_data_to_graph,
           aes(x = internet, y = Portion)) +
      geom_bar(stat = "identity", fill = "mediumblue") +
      geom_text(aes(label = scales::percent(Portion), y = Portion), vjust = 2.0, color = "white") + 
      scale_y_continuous(labels = scales::percent) +
      labs(x = "",
           y = "Percent of Households in Region") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  })
  
  # Summary table
  create_summary_table <- reactive({
    req(create_age_summary(),
        create_sex_summary(),
        create_race_ethnicity_summary(),
        create_language_summary(),
        create_income_summary(),
        create_hh_w_children_summary(),
        create_education_summary(),
        create_disability_status_summary(),
        create_disability_type_summary(),
        create_veteran_summary(),
        create_internet_summary())
    
    demographic_summary <- bind_rows(create_age_summary(),
                                     create_sex_summary(),
                                     create_race_ethnicity_summary(),
                                     create_language_summary(),
                                     create_income_summary(),
                                     create_hh_w_children_summary(),
                                     create_education_summary(),
                                     create_disability_status_summary(),
                                     create_disability_type_summary(),
                                     create_veteran_summary(),
                                     create_internet_summary()) %>%
      filter(!Variable %in% c("households_wo_children","no_disability","non_veteran","no_internet_access")) %>%
      rename(Count = `Tract Total`)
    
    # Reorder columns
    num_columns <- ncol(demographic_summary)
    
    demographic_summary <- demographic_summary %>% 
      select(all_of(num_columns), 1, all_of(num_columns - 3), all_of(num_columns - 2), all_of(num_columns - 1), 2:all_of(num_columns - 4))

  })
  
  # Downloadable summary data
  output$download_summary_data <- downloadHandler(
    filename = function() {
      paste("summary_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(create_summary_table(), file, row.names = F)
    }
  )
  
  # Summary table
  output$summaryTable <- renderDT({
    datatable(create_summary_table(),
              options = list(scrollX = TRUE, scrollY = '400px'),
              class = "display"
    )
  })
  
}

shinyApp(ui, server)
