#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(openxlsx)
library(shinyjs)
source("helper.R")


# Define UI for application
ui <- fluidPage(
  
   useShinyjs(),
   inlineCSS("#species { min-height: 200px; }"),
   
   # Application title
   titlePanel("Проверка по ботаническим базам данных"),
   
   # Note on beta testing
   fluidRow(
     column(12,
            div(class="alert alert-warning",
            p("Это бета-тест новой версии. В ней, скорее всего, есть ошибки. Об ошибках пишите на", a(href = "mailto:pas.ntg@gmail.com", "pas.ntg@gmail.com"), "или", a(href = "https://github.com/PavelSyomin/checkplantlist", "Гитхаб"), "или сообщите любым другим способом."),
            p("Старая версия приложения находится на", a(href="https://plantlist.shinyapps.io/check/", "https://plantlist.shinyapps.io/check/"), "и будет там, пока не станет ясно, что новая версия не хуже.")
            )
     )
   ),
   
   # Sidebar with a textarea input for a list of species
   fluidRow(
      column(4,
             wellPanel(
               textAreaInput("species",
                             "Список видов",
                             value = "Betula Pendula Roth\nAbies Alba (Münchh.) Michx.\nPinus Sylvestris Thunb.\nAcanthopale azaleoides\nCaput Draconis",
                             width = "100%",
                             height = "200px",
                             resize = "vertical",
                             rows = 10
               ),
               helpText("Одна строка — одно название"),
               selectInput("plantdb",
                           "База для проверки",
                            choices = list(
                              "ThePlantList" = "tpl",
                              "WorldFloraOnline" = "wfo",
                              "World Checklist of Vascular Plants" = "wcvp",
                              "GBIF Backbone" = "gbif",
                              "Leipzig Catalogue of Vascular Plants" = "lcvp"
                            ),
                           width = "100%",
                           selected = "tpl",
                           multiple = TRUE),
               helpText("Можно выбрать одну или несколько"),
               actionButton("update", "Проверить")
             ),
         p("Это веб-приложение сделали Павел Сёмин и Николай Груданов в 2019–2021 году. Исходный код есть ",
         a(href = "https://github.com/PavelSyomin/checkplantlist", target = "_blank", "на Гитхабе."), "По любым вопросам пишите на ",
         a(href = "mailto:pas.ntg@gmail.com", "pas.ntg@gmail.com"))
      ),
      
      # Show a table with a result and buttons to download result as a file
      column(8,
        conditionalPanel("output.table",
                         downloadButton("csv", "Скачать как CSV-файл"),
                         downloadButton("xlsx", "Скачать как таблицу Excel")),
        tableOutput("table")
         
      )
   )
)

# Define server logic
server <- function(input, output, session) {
   list <- reactiveValues(
    data = NULL,
    demo = readRDS("sample.rds"),
    plantdb = "tpl",
    result = NULL
  )
  
   observeEvent(input$update, {
    list$data <- parse_input(input$species)
    list$plantdb <- input$plantdb
    if (is.null(list$plantdb))
      list$plantdb <- "tpl"
    if (length(list$plantdb) == 1)
      list$result <- process_data(list$data, list$plantdb)
    else
      list$result <- process_multiple(list$data, list$plantdb)
  })

  output$table <- renderTable({
    if (!is.null(list$data)) {
      if (length(list$plantdb) == 1)
        then(list$result, prepare_for_web)
      else
        then(list$result, prepare_for_web_multiple)
      }
    else {
      prepare_for_web(list$demo)
    }
  }, sanitize.text.function = function(x) x)
  
  output$csv <- downloadHandler(
    filename = function() {
      paste0("species-", Sys.Date(), ".csv")
      },
    content = function(file) {
      if (!is.null(list$data)) {
        then(list$result, ~{
          data <- prepare_for_download(.)
          write.csv(data, file, row.names = FALSE)})
      }
      else {
        data = prepare_for_download(list$demo)
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
  
  output$xlsx <- downloadHandler(
    filename = function() {
      paste0("species-", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      if (!is.null(list$result)) {
        then(list$result, ~{
          data <- prepare_for_download(.)
          write.xlsx(data, file, row.names = FALSE)
        })
      }
      else {
        data <- prepare_for_download(list$demo)
        write.xlsx(data, file, row.names = FALSE)
      }
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
