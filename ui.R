library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
# loading ->  %>% withSpinner(color="blue")
library(shinycssloaders)

library(dplyr) 
library(plotly)
library(DT)
library(magick)
library(networkD3)
library(rintrojs)
#options(shiny.error = browser)
#library(bs4Dash)

guess_id <- function(x) {
  stopifnot(any(class(x) %in% c("shiny.tag", "shiny.tag.list")))
  ll <- unlist(x)
  match <- grepl("\\.id$", names(ll))
  if (any(match)) {
    return(as.character(ll[match])[1])
  } else {
    match <- grepl("\\.data-value$", names(ll))
    if (any(match)) return(as.character(ll[match])[1])
  }
  return(NA)
}

add_class <- function(x, class_id = guess_id(x)) {
  stopifnot(any(class(x) %in% c("shiny.tag", "shiny.tag.list")))
  x$attribs <- append(x$attribs, list(class = class_id))
  if (is.na(class_id)) stop("class_id is NA, maybe guess_id could not determine the ID of x")
  x
}
header <- dashboardHeader(title = "ShinyTemplate", dropdownMenuOutput("messageMenu"))


# sidebar ----
sidebar <- dashboardSidebar(
  introjsUI(),
  sidebarMenu(
    menuItem("Welcome Page", tabName = "intro", icon = icon("italic")),
    menuItem("User", tabName = "dashboard", icon = icon("user")),
    menuItem("Table", tabName = "table", icon = icon("table")),
    menuItem("Figure", tabName = "visual", icon = icon("chart-area")),
    menuItem("Other", tabName = "gsea", icon = icon("dna")),
    #menuItem("Network", tabName = "network", icon = icon("project-diagram")),
    menuItem("Feedback", icon = icon("comment-alt"),href="https://github.com/ss-lab-cancerunit/ISGverseFeedback/issues"),
    HTML(paste0(
      # position: absolute; bottom: 0; text-align: center; position: absolute; bottom: 0;
      "<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>",
      "",
      "<table style='margin-left:auto; margin-right:auto;' >",
      "<tr>",
      "<td style='padding: 5px;'><a href='https://twitter.com/shamith' target='_blank'><i class='fab fa-twitter fa-lg'></i></a></td>",
      "<td style='padding: 5px;'><a href='https://github.com/ss-lab-cancerunit/' target='_blank'><i class='fab fa-github fa-lg'></i></a></td>",
      "</tr>",
      "</table>",
      "<br>"),
      HTML(paste0(
        "<script>",
        "var today = new Date();",
        "var yyyy = today.getFullYear();",
        "</script>",
        "<p style = 'text-align: center;'><small>&copy; - <a href='https://www.samarajiwa-lab.org/' target='_blank'>samarajiwa-lab.org</a> - <script>document.write(yyyy);</script></small></p>")
      ))
  )
  

)



# controlbar ----
control_bar <- column(12,
                      conditionalPanel(
                        condition = "input.upload_on == true",
                        h4("CSV options"),
                        column(12, checkboxInput('header', 'Header', TRUE)),
                        column(12, radioButtons('sep', 'Separator',
                                                c(Comma=',',
                                                  Semicolon=';',
                                                  Tab='\t'),
                                                inline = FALSE, 
                                                ',')),
                        column(12, radioButtons('quote', 'Quote',
                                                c(None='',
                                                  'Double Quote'='"',
                                                  'Single Quote'="'"),
                                                inline = FALSE, 
                                                '"'))
                      ),
)

# p0. welcome box ----
welcome_box <- box(title = "Welcome to ISG Shiny Web APP",
                   includeHTML("insert.html"),
                   htmlOutput("video"))

# p2. table box ----
table_box <- box(title = "Selected Gene",
                 status = "info",
                 solidHeader = TRUE, width = 12,
                 DTOutput('clickedPoints')
)


# p2. intersection table ----
intersec_box <- box(title = introBox(actionLink("downloadintersection", "Download", icon = icon("download"))), 
                    downloadButton("save_intersection_hidden", style = "visibility: hidden;"),
                    status = "info",
                    solidHeader = TRUE, width = 12,
                    introBox(DTOutput('IntersecTable'))
)

# p3. network ----
network_box <- box(title = actionLink("downloadnetwork", "Network", icon = icon("download")), 
                   downloadButton("save_network_hidden", style = "visibility: hidden;"),
                   status = "warning",
                   solidHeader = TRUE, width = 12,
                   simpleNetworkOutput('networkPlot',height = 800)  %>% withSpinner(color="#3c8dbc")
)
# body ----
body <- dashboardBody(
  # Boxes need to be put in a row (or column)
    shinyjs::useShinyjs(),
    #shinyWidgets::chooseSliderSkin("HTML5"),
    tabItems(
      
      tabItem(tabName = "intro",
              div(id = 'welcome',
                  includeHTML("insert.html"),
                  #h3('Welcome to ISGverse, the Interferon Stimulated Gene detection resource.',
                  #   style = "color:darkblue" , align = "center" ) ,
                  tags$hr(),
                  htmlOutput("video"),
                  tags$script(HTML("
                        var p = document.getElementById('video')
                        $(p).attr('align', 'center');"))
              ),
              #counts
              valueBoxOutput("counter")  %>% withSpinner(color="blue")
              
      ),
      tabItem(tabName = "dashboard",
 ),
      # tab table ----
      tabItem(tabName = "table",
              
      ),
      # tab visual ----
      tabItem(tabName = "visual",

      ),
      
      # tab gsea ----
      tabItem(tabName = "gsea",
             
              fluidRow(),
      )
      
    ),
    # footer  ----
    #tags$footer(title="Your footer here", align = "right", style = "
    #    position: fixed;
    #    bottom:0;
    #    left: 0;
    #    width:100%;
    #    height:50px; /* Height of the footer */
    #    color: white;
    #    /*padding: 10px;*/
    #    background-color: #222d32;
    #    z-index: 1000;"
    #)
    # example1 footer: https://stackoverflow.com/questions/67763901/footer-position-in-shiny
    # example2 footer: https://stackoverflow.com/questions/38939827/footer-alignment-in-shiny-app-dashboard
    
)
controlbar <-  dashboardControlbar(
  skin = "dark",
  controlbarMenu(
    id = "menu"
  ),
  control_bar
  
)


footer <- dashboardFooter(
  left = "By Junfan Huang",
  right = "Cambridge, 2021"
)

# ui ----
ui <- dashboardPage(header, sidebar, controlbar=controlbar, body, skin = "blue",scrollToTop=TRUE)#,rightsidebar=rightsidebar)
