# original tab content

home_tab = function() {
  shiny::fluidRow(
    shinydashboard::box(
      title = NULL,
      width = 12,
      shiny::h2(
        "DiAGRAM - The ",
        shiny::tags$b("Di", .noWS="outside"), "gital ",
        shiny::tags$b("A", .noWS="outside"), "rchiving ",
        shiny::tags$b("G", .noWS="outside"), "raphical",
        shiny::tags$b("R", .noWS="outside"), "isk ",
        shiny::tags$b("A", .noWS="outside"), "ssessment ",
        shiny::tags$b("M", .noWS="outside"), "odel",
        align="center"
      ),
      shiny::h3("Version 0.9.7 (Prototype)", align="center"), #update in June
      shiny::br(),
      shiny::p(
        "This is the Digital Archiving Graphical Risk Assessment Model (DiAGRAM) built by the ",
        shiny::a(href="https://warwick.ac.uk", "University of Warwick"),
        " and ",
        shiny::a(href="https://www.nationalarchives.gov.uk"," The National Archives"),
        "with support from the ",
        shiny::a(href="https://www.heritagefund.org.uk/", "National Lottery Heritage Fund"),
        " and the ",
        shiny::a(href="https://epsrc.ukri.org/", "Engineering and Physical Sciences Research Council."),
        "For more information about the project please see our ",
        shiny::a(href="https://www.nationalarchives.gov.uk/information-management/manage-information/preserving-digital-records/research-collaboration/safeguarding-the-nations-digital-memory/",
                 "project page.")),
      shiny::p(
        "Before using the tool for the first time, we would advise you to read the ",
        shiny::a(
          href="https://www.dpconline.org/events/past-events/quantifying-digital-preservation-risk-online-workshop-2",
          "presentations"
        ),
        " from our online workshop with the Digital Preservation Coalition, where there is also an ",
        shiny::a(href="https://www.dpconline.org/docs/miscellaneous/events/2020-events/2307-dpc-workshop-2-exercise/file", "exercise sheet"),
        " you can work though."
      ),
      shiny::br(),
      shiny::div(
        id = "create-model-button-container",
        shiny::actionButton("createModel", "Create your model")
      ),
      shiny::br(),
      shiny::h3("Introduction"),
      shiny::p(
        "This decision support tool enables users to score their archive's
                digital preservation risk and then explore how this would change under
                different policies and risk scenarios. The risk score is based on the proportion of
                files in the archive that are renderable and where the archivist has full
                intellectual control."
      ),
      shiny::p(
        "The underlying methodology used to create this model is based on a Bayesian network
                - a probabilistic graphical model that captures the conditional dependencies of risk
                events. When historical data were unavailable, data from an expert elicitation
                session conducted in April 2020 were used to inform the probabilities needed for
                this model."
      ),
      shiny::p("This interface enables users to:"),
      shiny::tags$ul(
        shiny::tags$li(
          "Understand the risk definitions used in the model and
                  how the risk events are linked together"
        ),
        shiny::tags$li(
          "Create a model that reflects the policies and practices for their
                  Digital Archive"
        ),
        shiny::tags$li("Test alternative policies to see how this impacts the risk score"),
        shiny::tags$li("Download the model and a summary of the results"),
        shiny::tags$li("Upload a pre-built model and continue exploring scenarios from there"),
        shiny::tags$li(
          "Update the probability tables for the model based on the user's own data or
                  experience"
        ),
        shiny::tags$li(
          "Create bespoke scenarios by directly manipulating the probabilities
                  used in the model"
        )
      ),
      shiny::br(),
      # Adding Logos
      shiny::img(
        src="https://www.nationalarchives.gov.uk/wp-content/uploads/2019/06/TNA-SQUARE-LOGO-POSITIVE-01-720x720.jpg",
        height=100,
        width=100
      ),
      shiny::img(
        src='https://www.underconsideration.com/brandnew/archives/university_of_warwick_logo_detail.png',
        height=80,
        width=120
      ),
      shiny::img(
        src="https://www.heritagefund.org.uk/sites/default/files/media/attachments/English%20logo%20-%20Colour%20%28JPEG%29.jpg",
        height=80,
        width=216
      ),
      shiny::img(src="UKRI_EPSR_Council-Logo_Horiz-RGB.png", height=75)),
    shinydashboard::box(
      width = 12,
      shiny::h3("Guidance"),
      shiny::br(),
      shiny::p(
        shiny::tags$b("Definitions"),
        ": This page has a visualisation of the underlying network of digital preservation risks and
                allows you to see the full definitions, states and data sources used for each 'node'."
      ),
      shiny::p(
        shiny::tags$b("1. Create your model"),
        ": This goes through 9 questions to create a risk model and a score which is
                based on the user's archive and policies."
      ),
      shiny::p(
        shiny::tags$b("Recommendations"),
        ": This page looks at the impact changing each of the answers to the input questions would
                have to the risk score."
      ),
      shiny::p(
        shiny::tags$b("2. Compare policies"),
        ": Create and save different policies and see how the risk score changes."
      ),
      shiny::p(
        shiny::tags$b("3. Advanced customisation"),
        ": This tab allows users to edit the marginal and conditional probabilities
                in the model directly. This allows for users to input their own data for any nodes within the model
                or create scenarios by altering conditional probabilities."
      ),
      shiny::p(
        shiny::tags$b("4. Report"),
        ": This contains a summary and comparison of the policies for each model, and allows
                the model and plots to be downloaded."
      )
      #br(),
      #p("If you have further questions, please contact the workshop facilitator.")
    )
  )
}

definitions_tab = function() {
 shiny::tagList(
   shiny::h1("Definitions"),
   shiny::br(),
   shiny::fluidRow(
     shiny::column(
       width=4,
       shinydashboard::box(
         title="Node selection",
         width=NULL,
         collapsible=TRUE,
         shiny::h4("Please select a node from the menu to view its definition."),
         shiny::br(),
         shiny::br(),
         shiny::selectInput(
           inputId="NodeSelection",
           label=NULL,
           choices="No Nodes Available"
         )
       )
     ),
     shiny::column(
       width=8,
       shinydashboard::box(
         title="Node description",
         width=NULL,
         collapsible=TRUE,
         shiny::column(
           width=6,
           shiny::uiOutput("NodeDefinition"),
           shiny::br(),
           shiny::uiOutput("DataLink"),
           shiny::br(),
           shiny::uiOutput("DataYear")
         ),
         shiny::column(
           width=6,
           shiny::tableOutput("StateDefinition")
         )
       )
     )
   ),
   shiny::fluidRow(
     shiny::column(
       width=12,
       shinydashboard::box(
         title="DiAGRAM structure",
         collapsible=TRUE,
         width=NULL,
         shiny::plotOutput("NetworkStructure")
       )
     )
   )
 )
}

model_tab = function() {
  shiny::tagList(
    shiny::h1("Create your model"),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(
        width=12,
        shinydashboard::box(
          title=NULL,
          width=NULL,
          shinyWidgets::progressBar("Question_Progress", value=1, total=nquestions),
          #h4("Please answer the following question: "),
          shiny::uiOutput("Question"),
          shinyjs::useShinyjs(),
          shiny::br(),
          shiny::uiOutput("CustomisationInput")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width=8,
        shinydashboard::box(
          title="Model risk score",
          width=NULL,
          collapsible=TRUE,
          shiny::plotOutput("BasicUtilityComparison")
        )
      ),
      shiny::column(
        width=4,
        shinydashboard::box(
          title="Upload a previous model",
          collapsible=TRUE,
          width=NULL,
          shiny::strong("Please ensure any models uploaded have been generated from DiAGRAM"),
          shiny::br(),
          shiny::br(),
          shiny::fileInput(
            "customModel",
            "Choose custom model",
            accept=c(".bif")
          ),
          shiny::textInput("uploadName", label="Custom Model Name"),
          shiny::actionButton("uploadCustomModel", "Add model")
        )
      )
    )
  )
}
