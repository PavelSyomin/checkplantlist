#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(plantlist)
library(openxlsx)
library(promises)
library(future)
library(ipc)
library(shinyjs)
plan(multicore, workers = 2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   useShinyjs(),
   inlineCSS("#species { min-height: 200px; }"),
   
   # Application title
   titlePanel("Проверка по ThePlantList"),
   
   # Sidebar with a slider input for number of bins 
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
               helpText("Одна строка — одно название.",
                        "Любые опечатки пока трактуются не в вашу пользу."),
               actionButton("update", "Проверить")
             ),
         p("Это веб-приложение сделали Павел Сёмин и Николай Груданов в 2019–2020 году. Исходный код есть ",
         a(href = "https://github.com/PavelSyomin/checkplantlist", target = "_blank", "на Гитхабе"), "По любым вопросам пишите на ",
         a(href = "mailto:pas.ntg@gmail.com", "pas.ntg@gmail.com"))
      ),
      
      # Show a plot of the generated distribution
      column(8,
        conditionalPanel("output.checked_species",
                         downloadButton("csv", "Скачать как CSV-файл"),
                         downloadButton("xlsx", "Скачать как таблицу Excel")),
        tableOutput("checked_species")
         
      )
   )
)

# Define server logic
server <- function(input, output, session) {
  
  retrieve_accepted_name <- function(name, progress, progress_increment = 0.2) {
    matches <- status(name, detail = TRUE)
    colnames(matches) <- tolower(colnames(matches))
    name_parts <- parse_taxa(name)
    colnames(name_parts) <- tolower(colnames(name_parts))
    search_author <- ""
    search_author <- name_parts$author_of_species_parsed
    search_author <- paste0(toupper(substring(search_author, 1, 1)), substring(search_author, 2))
    matches$search_author <- search_author
    if(nrow(matches) == 1) {
      result <- matches
    }
    if(nrow(matches) > 1) {
      matches_with_author <- matches[matches$author == matches$search_author, ]
      if(nrow(matches_with_author) >= 1) {
        result <- matches_with_author
      }
      else {
        result <- matches
      }
    }
    result[is.na.data.frame(result)] <- ""
    result$search <- paste(result$search, result$search_author)
    result$accepted_name <- paste(result$accepted_species, result$accepted_author)
    result[result$status == "", "status"] <- "Not found"
    result[, c("id", "family", "scientific_name",
               "author", "accepted_species",
               "accepted_author", "search_author")] <- NULL
    progress$inc(progress_increment, message = "Проверка", detail = name)
    return(result)
  }
  
  status_colorizer <- function(value) {
    switch(value,
           Accepted = "<span class=\"text-success\">Accepted</span>",
           Synonym = "<span class=\"text-warning\">Synonym</span>",
           Unresolved = "<span class=\"text-info\">Unresolved</span>",
           Misapplied = "<span class=\"text-danger\">Misapplied</span>",
           "Not found" = "Not found",
           "Error")
  }
  
  parse_input <- function(text) {
    x <- trimws(text)
    x <- strsplit(x, "\n")
    x <- lapply(x, gsub, pattern = "\UA0", replacement = " ")
    x <- as.list(x[[1]])
  }
  
  get_url <- function(text) {
    if (text == "") {return("")}
    paste0("http://theplantlist.org/tpl1.1/record/", text)
  }
  
  get_hyperlink <- function(text) {
    if (text == "") {return("")}
    href = get_url(text)
    paste0("<a href=\"", href, "\" target=\"_blank\">", text, "</a>")
  }
  
  process_data <- function(species) {
    species <- species[trimws(species) != ""]
    progress <- AsyncProgress$new(message = "Ищем ресурсы", detail = "Это занимает от секунды до 10 минут", min = 0, max = 1, value = 0, millis = 100)
    disable("update")
    disable("csv")
    disable("xlsx")
    matches <- future(
      {
        do.call(rbind, lapply(species, retrieve_accepted_name, progress, progress_increment = 1/length(species)))
        }, lazy = TRUE)
    then(matches, function(x) {
      progress$close()
      enable("update")
      enable("csv")
      enable("xlsx")
      x <- x[c("search", "status", "accepted_name", "accepted_id")]
      x
      })
  }
  
  prepare_for_web <- function(df) {
    df$accepted_id <- sapply(df$accepted_id, get_hyperlink)
    df$status <- sapply(df$status, status_colorizer)
    colnames(df) <- c("Введённое название", "Статус",
                      "Принятое название",
                      "Номер и&nbsp;ссылка на&nbsp;ThePlantList")
    df
  }
  
  prepare_for_download <- function(df) {
    df$accepted_id <- sapply(df$accepted_id, get_url)
    colnames(df) <- c("Введённое название", "Статус",
                      "Принятое название",
                      "Номер и ссылка на ThePlantList")
    df
  }
  
  list <- reactiveValues(
    data = NULL,
    demo = readRDS("sample.rds"),
    result = NULL
  )
  
  observeEvent(input$update, {
    list$data <- parse_input(input$species)
    list$result <- process_data(list$data)
  })
  

  output$checked_species <- renderTable({
    if (!is.null(list$data)) {
      then(list$result, prepare_for_web)
      }
    else {
      prepare_for_web(list$demo)
    }
  }, sanitize.text.function = function(x) {x})
  
  output$csv <- downloadHandler(
    filename = function() {
      paste0("species-", Sys.Date(), ".csv")
      },
    content = function(file) {
      if (!is.null(list$data)) {
        then(list$result, ~{
          data <- prepare_for_download(.)
          write.csv(data, file)})
      }
      else {
        data = prepare_for_download(list$demo)
        write.csv(data, file)
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
          write.xlsx(data, file)
        })
      }
      else {
        data <- prepare_for_download(list$demo)
        write.xlsx(data, file)
      }
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# saveRDS(process_data(parse_input("Betula Pendula Roth\nAbies Alba (Münchh.) Michx.\nPinus Sylvestris Thunb.\nAcanthopale azaleoides\nCaput Draconis")), file = "sample.rds")
# df <- readRDS("sample.rds")
