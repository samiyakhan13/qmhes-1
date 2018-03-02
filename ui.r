library("bigrquery")
library("shiny")

fluidPage(
  # Application title
  titlePanel("Quality Management Tool for Higher Education Systems"),
  sidebarLayout(

    sidebarPanel(

    verbatimTextOutput("textop", placeholder = TRUE),
    textInput("pid", "Please Enter BigQuery Project ID*"),
    textInput("dbname", "Please Enter Database Name*"),
    textInput("sdtable", "Please Enter Name of Student Data Table*"),
    textInput("urtable", "Please Enter Name of University Ranking Table*"),
    textInput("crtable", "Please Enter Name of Company Ranking Table*"),
    conditionalPanel(
                        condition="input.conditionedPanels == 1",
                        textInput("EvalYear", "Please Select Year*"),
                        actionButton("button1", "Submit")

    ),

    conditionalPanel(
                        condition="input.conditionedPanels == 2",
                        actionButton("button2", "Submit")

    ),

    verbatimTextOutput("textopx", placeholder = FALSE),
    tags$style("#textopx{color: red;font-size: 10px;font-style: italic;}")

    ),
    # Show a plot
    mainPanel(
        tabsetPanel(
            tabPanel("Line Chart", plotOutput("linePlot"), value = 2),
            tabPanel("Pie Chart", plotOutput("piePlot"), value = 1),
            id = "conditionedPanels"
    )
    )
  )
)