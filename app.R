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
library(promises)
library(future)
library(ipc)
library(shinyjs)
library(RSQLite)
plan(multisession, workers = 2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   useShinyjs(),
   inlineCSS("#species { min-height: 200px; }"),
   
   # Application title
   titlePanel("Проверка по ThePlantList"),
   
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
               selectInput("plantdb",
                           "База для проверки",
                            choices = list(
                              "ThePlantList" = "plantlist",
                              "WorldFloraOnline" = "wfo",
                              "World Checklist of Vascular Plants" = "wcvp",
                              "GBIF Backbone" = "gbif",
                              "Leipzig Catalogue of Vascular Plants" = "lcvp"
                            ),
                           width = "100%",
                           selected = "plantlist",
                           multiple = TRUE),
               helpText("Одна строка — одно название.",
                        "Любые опечатки пока трактуются не в вашу пользу."),
               actionButton("update", "Проверить")
             ),
         p("Это веб-приложение сделали Павел Сёмин и Николай Груданов в 2019–2020 году. Исходный код есть ",
         a(href = "https://github.com/PavelSyomin/checkplantlist", target = "_blank", "на Гитхабе."), "По любым вопросам пишите на ",
         a(href = "mailto:pas.ntg@gmail.com", "pas.ntg@gmail.com"))
      ),
      
      # Show a table with a result and buttons to download result as a file
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
  
  # Copied from plantlist package
  parse_taxa <- function (taxa) 
  {
    parse_taxon <- function(taxon) {
      replace_space <- function(x) {
        gsub("[[:space:]]+", " ", gsub("^[[:space:]]+|[[:space:]]+$", 
                                       "", x))
      }
      if (length(taxon) > 1) {
        stop("Only one taxon allowed")
      }
      GENUS <- ""
      SPECIES <- ""
      AUTHOR_OF_SPECIES <- ""
      INFRASPECIFIC_RANK <- ""
      INFRASPECIFIC_EPITHET <- ""
      AUTHOR_OF_INFRASPECIFIC_RANK <- ""
      taxon <- gsub(" +", " ", replace_space(taxon))
      gap1 <- regexpr(pattern = " ", text = taxon)
      GENUS <- replace_space(substr(taxon, start = 1, stop = (gap1 - 
                                                                1)))
      part1 <- replace_space(substr(taxon, start = gap1 + 1, 
                                    stop = nchar(taxon)))
      gap2 <- regexpr(pattern = " ", text = part1)
      if (gap2 < 0) {
        SPECIES <- replace_space(substr(part1, start = gap2 + 
                                          1, stop = nchar(part1)))
        author_temp <- ""
      }
      else {
        SPECIES <- replace_space(substr(part1, start = 1, 
                                        stop = (gap2 - 1)))
        author_temp <- replace_space(substr(part1, start = gap2 + 
                                              1, stop = nchar(part1)))
      }
      AUTHOR_OF_SPECIES <- author_temp
      gap3 <- regexpr(pattern = " ", text = author_temp)
      if (grepl("var\\. ", taxon) | grepl("subsp\\. ", taxon) | 
          grepl(" f\\. ", taxon)) {
        if (grepl("var\\. ", taxon)) {
          INFRASPECIFIC_RANK <- "var."
          gap_var <- regexpr(pattern = "var\\. ", text = author_temp) + 
            nchar("var.")
          AUTHOR_OF_SPECIES <- replace_space(substr(author_temp, 
                                                    start = 1, stop = gap_var - nchar("var.") - 
                                                      1))
          part_INFRASP_EP_AUTHOR_OF_INFRASP <- replace_space(substr(author_temp, 
                                                                    start = gap_var + 1, stop = nchar(author_temp)))
        }
        else {
          if (grepl("subsp\\. ", taxon)) {
            INFRASPECIFIC_RANK <- "subsp."
            gap_subsp <- regexpr(pattern = "subsp\\. ", 
                                 text = author_temp) + nchar("subsp.")
            AUTHOR_OF_SPECIES <- replace_space(substr(author_temp, 
                                                      start = 1, stop = gap_subsp - nchar("subsp.") - 
                                                        1))
            part_INFRASP_EP_AUTHOR_OF_INFRASP <- replace_space(substr(author_temp, 
                                                                      start = gap_subsp + 1, stop = nchar(author_temp)))
          }
          else {
            if (substr(author_temp, start = 1, stop = nchar("f.")) == 
                "f.") {
              INFRASPECIFIC_RANK <- "f."
              gap_f <- regexpr(pattern = "f\\. ", text = author_temp) + 
                nchar("f.")
              position_f <- regexpr(pattern = "f\\.", text = taxon)[[1]][1]
              position_white_space <- regexpr(pattern = "", 
                                              text = taxon)[[1]]
              location_species_end <- position_white_space[2]
              AUTHOR_OF_SPECIES_temp <- replace_space(substr(taxon, 
                                                             start = location_species_end, stop = position_f - 
                                                               1))
              AUTHOR_OF_SPECIES <- ifelse(is.na(AUTHOR_OF_SPECIES_temp), 
                                          "", AUTHOR_OF_SPECIES_temp)
              part_INFRASP_EP_AUTHOR_OF_INFRASP <- replace_space(substr(author_temp, 
                                                                        start = gap_f + 1, stop = nchar(author_temp)))
            }
            else {
              INFRASPECIFIC_RANK <- ""
              part_INFRASP_EP_AUTHOR_OF_INFRASP <- ""
              AUTHOR_OF_SPECIES <- replace_space(substr(author_temp, 
                                                        start = 1, stop = nchar(author_temp)))
            }
          }
        }
      }
      else {
        part_INFRASP_EP_AUTHOR_OF_INFRASP <- ""
      }
      gap4 <- regexpr(pattern = " ", text = part_INFRASP_EP_AUTHOR_OF_INFRASP)
      if (gap4 > 0) {
        INFRASPECIFIC_EPITHET <- replace_space(substr(part_INFRASP_EP_AUTHOR_OF_INFRASP, 
                                                      start = 1, stop = gap4 - 1))
        AUTHOR_OF_INFRASPECIFIC_RANK <- replace_space(substr(part_INFRASP_EP_AUTHOR_OF_INFRASP, 
                                                             start = gap4 + 1, stop = nchar(part_INFRASP_EP_AUTHOR_OF_INFRASP)))
      }
      else {
        INFRASPECIFIC_EPITHET <- replace_space(substr(part_INFRASP_EP_AUTHOR_OF_INFRASP, 
                                                      start = 1, stop = nchar(part_INFRASP_EP_AUTHOR_OF_INFRASP)))
        if (INFRASPECIFIC_EPITHET %in% strsplit(AUTHOR_OF_SPECIES, 
                                                " ")[[1]]) {
          INFRASPECIFIC_EPITHET <- ""
        }
      }
      if (!grepl(" ", taxon)) {
        GENUS = taxon
        SPECIES <- ""
        AUTHOR_OF_SPECIES <- ""
        INFRASPECIFIC_RANK <- ""
        INFRASPECIFIC_EPITHET <- ""
        AUTHOR_OF_INFRASPECIFIC_RANK <- ""
      }
      res <- c(taxon, GENUS, SPECIES, AUTHOR_OF_SPECIES, INFRASPECIFIC_RANK, 
               INFRASPECIFIC_EPITHET, AUTHOR_OF_INFRASPECIFIC_RANK)
      names(res) <- c("TAXON_PARSED", "GENUS_PARSED", "SPECIES_PARSED", 
                      "AUTHOR_OF_SPECIES_PARSED", "INFRASPECIFIC_RANK_PARSED", 
                      "INFRASPECIFIC_EPITHET_PARSED", "AUTHOR_OF_INFRASPECIFIC_RANK_PARSED")
      return(res)
    }
    res <- data.frame(t(sapply(taxa, parse_taxon, USE.NAMES = FALSE)), 
                      stringsAsFactors = FALSE)
    return(res)
  }
  
  not_found <- data.frame(
    name = "Not found",
    author = "",
    status = "Not fonud",
    id = "",
    accepted_name = "",
    accepted_author = "",
    accepted_id = ""
  )
  
  get_data <- function(name, table_name = "plantlist")
  {
    conn <- dbConnect(SQLite(), "species.db")
    query <- switch(table_name,
                    plantlist = "SELECT * FROM plantlist WHERE search_string = ?",
                    wfo = "SELECT * FROM wfo WHERE search_string = ?",
                    wcvp = "SELECT * FROM wcvp WHERE search_string = ?",
                    gbif = "SELECT * FROM gbif WHERE search_string = ?",
                    lcvp = "SELECT * FROM lcvp WHERE search_string = ?"
    )
    parts <- strsplit(name, " ")[[1]]
    if (length(parts) == 1)
      parts[2] <- ""
    search_string <- paste(parts[1], tolower(parts[2]), sep = "")
    result <- dbGetQuery(conn, query, params = search_string)
    if (nrow(result) == 0)
      result <- not_found
    result$search_string <- NULL
    return(result)
  }
  
  find_most_appropriate <- function(data, name)
  {
    data$name <- tolower(data$name)
    data$author <- tolower(data$author)
    name <- tolower(name)
    
    result <- data
    if (nrow(result) == 1)
      return(result)
    
    result <- data[data$name == name, ]
    if (nrow(result) == 1)
      return(result)
    
    data$full_name <- paste(data$name, data$author, sep = " ")
    result <- data[data$full_name == name, ]
    result$full_name <- NULL
    if (nrow(result) == 1)
      return(result)
    data$full_name <- NULL
    
    name_parts <- parse_taxa(name)
    colnames(name_parts) <- tolower(colnames(name_parts))
    name_parts$name <- paste(name_parts$genus_parsed, name_parts$species_parsed, sep = " ")
    
    result <- data[data$name == name_parts$name & data$author == name_parts$author_of_species_parsed, ]
    if (nrow(result) == 1)
      return(result)
    
    name_pattern <- paste("^", name, sep = "")
    result <- data[grepl(name_pattern, data$name), ]
    if (nrow(result) == 1)
      return(result)
    
    result <- data[grepl(name_pattern, data$name) & data$author == name_parts$author_of_species_parsed, ]
    if(nrow(result) == 1)
      return(result)
    
    isHybrid <- grepl(" [x×] ", name)
    if (isHybrid)
    {
      return(not_found)
    }
    
    data_parsed <- do.call(rbind, apply(data, 1, function(x) parse_taxa(x["name"])))
    
    colnames(data_parsed) <- tolower(colnames(data_parsed))
    
    scores <- apply(data_parsed, 1, function (x, parts) {
      total <-  0
      if (x["genus_parsed"] == parts$genus_parsed)
        total <- total + 1
      if (x["species_parsed"] == parts$species_parsed)
        total <- total + 1
      if (x["author_of_species_parsed"] == parts$author_of_species_parsed)
        total <- total + 1
      if (x["infraspecific_rank_parsed"] == parts$infraspecific_rank_parsed)
        total <- total + 1
      if (x["infraspecific_epithet_parsed"] == parts$infraspecific_epithet_parsed)
        total <- total + 1
      if (x["author_of_infraspecific_rank_parsed"] == parts$author_of_infraspecific_rank_parsed)
        total <- total + 1
      return(total)
    }, name_parts)
    scores <- as.numeric(scores)
    max_score <- which.max(scores)
    result <- data[max_score, ]
    if (nrow(result) == 1)
      return(result)
    return(data)
  }
  
  retrieve_accepted_name <- function(name, plantdb, progress, progress_increment = 0.2) {
    matches <- get_data(name, plantdb)
    result <- find_most_appropriate(matches, name)
    result[is.na.data.frame(result)] <- ""
    result$search <- name
    result$accepted_name <- paste(result$accepted_name, result$accepted_author)
    result[result$status == "Not Found", "status"] <- "Not found"
    result[, c("id", "name",
               "author", "accepted_author")] <- NULL
    progress$inc(progress_increment, message = "Проверка", detail = name)
    return(result)
  }
  
  status_colorizer <- function(value) {
    switch(value,
           Accepted = "<span class=\"text-success\">Accepted</span>",
           accepted = "<span class=\"text-success\">Accepted</span>",
           heterotypicSynonym = "<span class=\"text-warning\">Heterotypic Synonym</span>",
           "heterotypic synonym" = "<span class=\"text-warning\">Heterotypic Synonym</span>",
           homotypicSynonym = "<span class=\"text-warning\">Homotypic Synonym</span>",
           "homotypic synonym" = "<span class=\"text-warning\">Homotypic Synonym</span>",
           Homotypic_Synonym = "<span class=\"text-warning\">Homotypic Synonym</span>",
           Synonym = "<span class=\"text-warning\">Synonym</span>",
           synonym = "<span class=\"text-warning\">Synonym</span>",
           "proparte synonym" = "<span class=\"text-warning\"> Proparte Synonym</span>",
           Unresolved = "<span class=\"text-info\">Unresolved</span>",
           Unchecked = "<span class=\"text-info\">Unchecked</span>",
           Unplaced = "<span class=\"text-info\">Unplaced</span>",
           doubtful = "<span class=\"text-info\">Doubtful</span>",
           Misapplied = "<span class=\"text-danger\">Misapplied</span>",
           misapplied = "<span class=\"text-danger\">Misapplied</span>",
           external = "External",
           "Artificial Hybrid" = "Artificial hybrid",
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
    if (text == "") 
      return("")
    switch(list$plantdb,
           "plantlist" = paste0("http://theplantlist.org/tpl1.1/record/", text),
           "wfo" = paste0("http://www.worldfloraonline.org/taxon/", text),
           "wcvp" = paste0("https://wcvp.science.kew.org/taxon/", text),
           "gbif" = paste0("https://www.gbif.org/species/", text),
           "lcvp" = ""
    )
  }
  
  get_hyperlink <- function(text) {
    if (text == "")
      return("")
    href = get_url(text)
    paste0("<a href=\"", href, "\" target=\"_blank\">", text, "</a>")
  }
  
  process_data <- function(species, plantdb) {
    species <- species[trimws(species) != ""]
    progress <- AsyncProgress$new(message = "Ищем ресурсы", detail = "Это занимает от секунды до 10 минут", min = 0, max = 1, value = 0, millis = 100)
    disable("update")
    disable("csv")
    disable("xlsx")
    matches <- future(
      {
        do.call(rbind, lapply(species, retrieve_accepted_name, plantdb, progress, progress_increment = 1/length(species)))
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
    if (ncol(df) > 4)
      return(df)
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
    plantdb = "plantlist",
    result = NULL
  )
  
  process_multiple <- function(species, db)
  {
    results_list <- promise_map(db, function(x, species) process_data(species, x), species)
    then(results_list, function(x)
     {
      d <- Reduce(function(a, b) merge(a, b, by = "search"), x)
      d
     })
  }
  
  observeEvent(input$update, {
    list$data <- parse_input(input$species)
    list$plantdb <- input$plantdb
    if (is.null(list$plantdb))
      list$plantdb <- "plantlist"
    if (length(list$plantdb) == 1)
      list$result <- process_data(list$data, list$plantdb)
    else
      list$result <- process_multiple(list$data, list$plantdb)
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

# The commented code was used to generate sapmle.rds and should not be uncommented unless you want to re-generate sample.rds
# saveRDS(process_data(parse_input("Betula Pendula Roth\nAbies Alba (Münchh.) Michx.\nPinus Sylvestris Thunb.\nAcanthopale azaleoides\nCaput Draconis")), file = "sample.rds")
# df <- readRDS("sample.rds")
