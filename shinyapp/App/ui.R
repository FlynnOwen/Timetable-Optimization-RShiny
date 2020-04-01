#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source('../R_Scripts/libraries.R')

DataDir <- '../Data'
timetable <- read.csv(file.path(DataDir,"Timetable_data.csv"))



shinyUI(fluidPage( 
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css?family=Open+Sans&display=swap');
      h1{
        font-family: 'Open+Sans', bold;
        font-weight: 800;
        line-height: 2;
        color: #FF0000;
        
      }
    "))
  ),
  column(12, h1("Exams Made Easy (EME)",align = 'center')),
  tabsetPanel(type = "tabs",
              tabPanel(tippy("Main",tooltip = "The main tab for the application. Here the overall network, as well as subnetworks are plotted. All tabs for altering the data are in this tab."),  fluidRow(
                splitLayout(cellwidths = c("50%", "50%"),h4(textOutput("Title1"),align = "center"),h4(textOutput("Title2"), align = "center")),
                splitLayout(style = "border: 1px solid black",cellWidths = c("50%", "50%"), plotOutput(outputId = "net",height = "500px",width = "500px"),forceNetworkOutput(outputId = "net2",height = "500px", width = "400px"))),
                hr(),
                column(width = 12, offset = 5,
                       selectInput("Dataset", tippy("Choose a dataset:",tooltip = "The dataset that will be considered for all outputs"),
                                   choices = c("Original Dataset","Alternative Dataset","Small school Dataset"))),
                br(),
                fluidRow(
                  column(style = 'border-right: 1px solid black',
                         3,
                         textOutput("Density"),
                         br(),
                         checkboxInput("colourme",tippy("Chromatic Colouring",tooltip = "Colour the complete network in such a way that no two adjacent nodes (classes) are the same colour, in the least amount of colours possible")),
                         br(),
                         checkboxInput("Circle",tippy("Plot as Circle", tooltip = "Plot the graph such that all vertices form a cirlce, with edges on in the inside of the circle (Works best with low density networks)")),
                         br(),
                         uiOutput("SingleWeights")
                         
                  ),
                  column(
                    3,
                    sliderInput("opacity",
                                tippy("Node opacity",tooltip = "The opacity of the nodes in the star and clique graphs"),
                                min = 0.1,
                                max = 1,
                                value = 0.85),
                    br(),
                    sliderInput("fontsize",
                                tippy("Font Size", tooltip = "The size of the text when hovering over the nodes in the star and clique graphs"),
                                min = 5,
                                max = 30,
                                value = 20)
                  ),
                  column(style = 'border-left: 1px solid black',
                         3,
                         uiOutput("SubjectSelector"),
                         br(),
                         numericInput("Seed", tippy("Seed",tooltip = "A random seed for plotting clique graphs - there could be hundreds of cliques of size X, but setting the seed insures that you can get the same group of classes."), 123, min = NA, max = NA, step = NA,
                                      width = NULL),
                         selectInput("Clique", tippy("Star vs Clique", tooltip = "Choose to plot either a star or a clique - the application can only plot one at a time (note that a star of size 1 is the same as a clique of size 2)"),
                                     choices = c("Star","Clique"))
                  ),
                  column(style = "border-left: 1px solid black",
                         3,
                         selectInput("NumClasses", tippy("Star Size",tooltip = "The number of nodes (classes) that should be plotted against the chosen class in the star plot"),
                                     choices = 1:12),
                         br(),
                         uiOutput("AdjSize"),
                         br(),
                         splitLayout(cellwidths = c("50%", "50%"), numericInput("Adj1",tippy("Class 1: ", tooltip = "Test whether this node and node 2 are adjacent. See legend in complete network plot for which number corresponds to which subject"), 1,min = 1, max = 30),numericInput("Adj2",tippy("Class 2: ", tooltip = "Test whether this node and node 1 are adjacent. See legend in complete network plot for which number corresponds to which subject"), 2,min = 1, max = 30)),
                         br(),
                         textOutput("Adjacency")
                  )
                  
                )),
              tabPanel(tippy("Bar graph", "A bar graph showing classes taken with a particular class. Note this is reactive in that it will plot barplots for whichever subject is in the center of the star plot"),plotOutput(outputId = "subjecthist")),
              tabPanel(tippy("Time-table","Tab showing the optimal timetable found from the graph-colouring. This can also be downloaded."),  h3(tippy("The Optimal Timetable for this Dataset", tooltip = "This is the optimal timetable for this dataset, there will not be any clashes. Note that the 'days' and 'times' are not important - ie the subjects scheduled for morning of day 1 could be swapped with the subjects of morning day 2. As long as the groupings of subjects stay the same."),align = "center"),
                       hr(),
                       tableOutput(outputId = "time-table"),
                       column(width = 6, offset = 5,
                              downloadButton("report", tippy("Download Timetable", tooltip = "Press to download a CSV file of this timetable"),align = "center"))),
              tabPanel(tippy("About",tooltip = "Infomation about the application"),
                       h3("What is EME?",align = 'center'),
                       "Exams Made Easy is an application designed for schools to help provide information about a relational dataset.
                       Relational data is often known as difficult to gain information from, as well as hard to navigate. EME tries to make this
                       as seemless as possible by having an intuitive UI, and beautiful visualisations to aid this. As well as providing insights,
                       it also creates an optimal timetable given the classes - as this can be resource intensive, confusing, and can ultimately lead to
                       very inefficient outcomes if not done properly.",
                       hr(),
                       br(),
                       h3("How does it work?", align = 'center'),
                       "EME uses software R and R Shiny to take the dataset input by the user, and creates stunning visualisations using clever algorithms, and packages within R.
                       The application uses clever methods of network colouring to find the timetable - nodes are coloured in the least amount of colours such that no two adjacent 
                       (directly connected) nodes are the same colour. These colours then represent the time-slots.",
                       hr(),
                       br(),
                       h3("How do I remember what every tool does?", align = 'center'),
                       "Tooltips are included, so by scrolling over (almost) any interactive tool, information is provided on how to use this tool,
                       and what insights can be gained from it",
                       hr(),
                       br(),
                       h3("What sort of data can I use?", align = 'center'),
                       "The data must be in a CSV format, with the first column representing a unique student ID, which will be different for every row. The next columns will represent each course
                       that this student is taking. The order of the classes does not matter. At the moment, all columns must be of the same length - i.e all students must
                       be taking the same number of classes. The timetable plotted and insights visible are unique given every dataset input. If you would like to add an additional dataset, this must be done through server.R. 
                       Data can also be simulated through Data_Simulation.R.",
                       br()
                       )
              
  )
)
)


