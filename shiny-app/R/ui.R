# source needed variables using global.R
source("R/global.R")

# Define UI
ui <- fluidPage(
  
  # Sidebar for potential controls (add inputs here as needed)
  div(class = "scroll-container",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          p(a("Introduction", href = "#intro")),
          p(a("Table 1: Hypothetical Example", href = "#table1")),
          p(a("Table 2: 2005-2022 Changes by Race and Age", href = "#table2")),
          p(a("Table 3: Contributions", href = "#table3"))
        ),
        
        # Main panel for displaying content
        mainPanel(
          titlePanel("Changes in Young Adult Cohabitation Patterns, 2012 - 2022"),
          
          tags$h3("Introduction", id = "intro"),
          p("Young adults who live with their parents comprise a substantial",
            "fraction of the U.S. population. In 2012,", 
            pct_cohabit_2012,
            "percent of young adults aged 18 to 29 lived with their parents. By 2022,",
            "this figure shrank to",
            pct_cohabit_2022, 
            "percent.",
            
          ),
          
          p(paste(
            "Tables 1a and 1b show the percentage of young people aged 18-29 who lived",
            "with their parents in 2012 and 2022. Households where the child lives with",
            "a parent are further broken down by who is the (presumable) financial provider:",
            "the child, the parent, or some other individual. [TODO: Link to the methodology",
            "document for this categorization.]",
            "The tables show that while the",
            "total percentage of young adults living with their parents has shrunk in the",
            "past 10 years, the small group of young adults supporting their parents has grown."
          )),
          
          tags$h4("Table 1a: Cohabitation in 2012 among Americans ages 18-29", id = "table1a"),
          DTOutput("table1a"),
          tags$h4("Table 1b: Cohabitation in 2022 among Americans ages 18-29", id = "table1b"),
          DTOutput("table1b"),
          
          p(paste(
            "The factors driving these aggregate trends remain unclear. A decline in parental",
            "cohabitation might indicate improvements in the financial independence of young adults,",
            "enabling them to support themselves—and potentially others—at an earlier age.",
            "Alternatively, it could reflect increasing economic burdens in the face of rapidly",
            "rising housing costs. Reduced cohabitation could",
            "also reflect the growing fraction of Americans in their teens and twenties",
            "who are attending college [FACT CHECK].",
            "Cultural norms around living with parents might also be shifting, making young adults",
            "more averse to the practice [note from Lorae: As a young adult, I anecdotally don't think this",
            "is true. I think living with your parents is becoming less stigmatized over time].",
            "Demographics may also play a role, but preliminary findings suggest this is not",
            "a good explanation. Hispanic Americans, who tend to cohabit with parents",
            "at higher rates, are a growing share of the population, while White (non-Hispanic)",
            "Americans, who have the lowest parental cohabitation rates, are a shrinking share.",
            "This demographic shift runs counter to the observed decline in cohabitation overall."
          )),
          
          p(paste(
            "The effects of parental cohabitation on America's youth are also unclear.",
            "Cohabitation could help young adults accumulate savings. If the practice",
            "is becoming less common, this may predict worse economic outcomes later in life.",
            "On the other hand, if parental cohabitation inhibits geographic mobility,",
            "the observed decline might mean that young adults are now more willing to",
            "move in seek of job opportunities."
          )),
          
          p(paste(
            "Surveys have shown a growing prevalence of parental cohabitation among young",
            "adults [Cite Pew or other surveys here], but little is known about the",
            "socioeconomic profiles of these individuals, the factors driving the",
            "reduction in cohabitation, and the potential longer-run outcomes.",
            "In this document, we investigate some basic preliminary associations:"
          )),
          
          p(paste(
            "To what extent does cohabitation differ by gender, race, geography,",
            "the homeownership / renter status of the parent, and the education and",
            "income levels of the child?")),
          
          p(paste(
            "TODO: add 95% confidence interval for percents to hover element on graphs"
          )),
          
          tags$h3("Table 1: Homeownership in the United States", id = "table1"),
          
          p(paste("Table 1a shows the fraction of individuals in the United States",
                  "in 2022, by their household status. Table 1b shows these values",
                  "in 2012.")),
          
          p(strong("Table 1")),
          DTOutput("table1"),
          
          tags$h3("Cohabitation by Ownership", id = "graph"),
          p(paste("The graph below shows the fraction of the population living with their",
                  "parents by age group. Individuals are filtered by the type of household",
                  "they live in: owner-occupied, renter-occupied, or not otherwise specified.",
                  "Unsurprisingly, a large fraction of young Americans who live in an owner-occupied",
                  "household live with their parents. "
          )),
          
          # Add a row with radio buttons and graph
          fluidRow(
            column(
              width = 6,
              radioButtons(
                "ownership_status_a", 
                label = "Homeownership status:",
                choices = c(
                  "Owned free and clear" = "Owned free and clear",
                  "Owned with mortgage or loan" = "Owned with mortgage or loan",
                  "With cash rent" = "With cash rent",
                  "No cash rent" = "No cash rent",
                  "N/A" = "N/A"
                ),
                selected = "Owned free and clear"
              )),
            column(
              width = 6,
              radioButtons(
                "year_selection_a", 
                label = "Year:",
                choices = c("2012", "2022"),
                selected = "2022"
              ))),
          
          plotlyOutput("ownership_graph", height = "400px"),
          
          p(paste("It may be more informative to look at the question a different way:",
                  "given a young person's cohabitation status,",
                  "what type of home -- owner-occupied or renter-occupied -- do they",
                  "typically inhabit? The results below show that a majority of young",
                  "adults in their twenties who do not live with their parents occupy rented",
                  "housholds. And a large majority of these same Americans in their",
                  "twenties who live with and financially depend on a parent live in owner-",
                  "occupied households. There is some ambiguity in defining cohabiting",
                  "households where the child relies on the parent versus where the parent",
                  "relies on the child. [TODO: add hyperlink to the document defining",
                  "how we classify these individuals]. Nevertheless, we can feel somewhat optimistic",
                  "that our measure of parental reliance is accurate: the profiles",
                  "of young adults who support their parents and of young adults who",
                  "don't live with their parents both show similar age patterns skewed",
                  "toward rentership. This relationship is consistent with a hypothesis that both these groups are",
                  "more financially burdened than their peers who rely on their parents for",
                  "shelter, and are therefore unlikely to have already purchased a home."
          )),
          
          # Add a row with radio buttons and graph
          fluidRow(
            column(
              width = 3,
              radioButtons(
                "cohabit_status_b", 
                label = "Cohabit status:",
                choices = c(
                  "Not living with parents" = "Not living with parents",
                  "Child provides for parent" = "Child provides for parent",
                  "Both child and parent are dependent" = "Both child and parent are dependent",
                  "Child depends on parent" = "Child depends on parent",
                  "Living in institution" = "Living in institution"
                ),
                selected = "Not living with parents"
              ),
              radioButtons(
                "year_selection_b", 
                label = "Year:",
                choices = c("2012", "2022"),
                selected = "2022"
              )
            ),
            column(
              width = 9,
              plotlyOutput("ownership_graph_cohabit", height = "400px")
            )
          ),
          
          tags$h3("Cohabitation by Race and Sex", id = "graph"),
          
          p(paste(
            "Cohabitation varies greatly by both race and sex. Men live with their",
            "their parents much more frequently than women do. Hispanics and those",
            "who self-identify their race as \"other\" tend to cohabit at a higher rate,",
            "while Whites tend to live with their parents at the lowest rate." 
          )),
          
          fluidRow(
            column(
              width = 4,
              radioButtons(
                "race_selection_c",
                label = "Race / Ethnicity",
                choices = c(
                  "Asian American / Pacific Islander" = "AAPI",
                  "American Indian / Alaska Native" = "AIAN",
                  "Black" = "Black",
                  "Hispanic" = "Hispanic",
                  "Multiracial" = "Multiracial",
                  "White" = "White",
                  "Other" = "Other",
                  "All" = "All"
                ),
                selected = "All"
              )),
            column(
              width = 4,
              radioButtons(
                "sex_selection_c", 
                label = "Gender:",
                choices = c(
                  "Male" = "Male",
                  "Female" = "Female",
                  "All" = "All"
                ),
                selected = "All"
              )),
            column(
              width = 4,
              radioButtons(
                "year_selection_c", 
                label = "Year:",
                choices = c("2012", "2022"),
                selected = "2022"
              ))
          ),
          
          plotlyOutput("race_sex_cohabit", height = "400px"),
          
          tags$h3("Cohabitation over time", id = "graph"),
          
          p(paste(
            "TODO: all line graph here"
          ))
          
        )
      )
  )
)
