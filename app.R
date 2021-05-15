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

# Define UI for application
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
    status = "Not found",
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
    dbDisconnect(conn)
    if (nrow(result) == 0)
      result <- not_found
    result$search_string <- NULL
    return(result)
  }
  
  preprocess_name <- function(x)
  {
    x <- gsub("[.() x×&]", "", x)
    tolower(x)
  }
  
  find_most_appropriate <- function(data, name)
  {
    # Turn everything to lovercase and remove some punctuation characters
    data$name <- sapply(data$name, preprocess_name)
    data$author <- sapply(data$author, preprocess_name)
    data$full_name <- paste(data$name, data$author, sep = "")
    name <- preprocess_name(name)
    
    # Attempt # 1: try exact name+author match
    result <- data[data$full_name == name, ]
    
    # If we have any results, return them
    # Limit by 1 to ensure that we have one result for one input name
    if (nrow(result) > 0)
      return(result[1, ])
    
    # Attempt # 2: try exact match, but by name only
    result <- data[data$name == name, ]
    
    if (nrow(result) > 0)
      return(result[1, ])
    
    # Attempl # 3: if we have only one row in data, assume that it is the correct name
    if (nrow(data) == 1)
      return(data)
  
    # If all attmepts have failed, return not found
    return(not_found)
  }
  
  retrieve_accepted_name <- function(name, plantdb, progress, progress_increment = 0.2) {
    matches <- get_data(name, plantdb)
    result <- find_most_appropriate(matches, name)
    result[is.na.data.frame(result)] <- ""
    result$search <- name
    result$accepted_name <- paste(result$accepted_name, result$accepted_author)
    result$accepted_url = get_url(result$accepted_id, plantdb)
    result <-  result[c("search", "status", "accepted_name", "accepted_url")]
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
  
  get_url <- function(id, plantdb) {
    if (id == "") 
      return("")
    switch(plantdb,
           "plantlist" = paste0("http://theplantlist.org/tpl1.1/record/", id),
           "wfo" = paste0("http://www.worldfloraonline.org/taxon/", id),
           "wcvp" = paste0("https://wcvp.science.kew.org/taxon/", id),
           "gbif" = paste0("https://www.gbif.org/species/", id),
           "lcvp" = ""
    )
  }
  
  get_hyperlink <- function(text, href) {
    if (text == "")
      return("")
    if (href == "")
      return(text)
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
      x
      })
  }
  
  prepare_for_web <- function(df) {
    df$accepted_name <- mapply(get_hyperlink, df$accepted_name, df$accepted_url)
    df$status <- sapply(df$status, status_colorizer)
    df <- df[c("search", "status", "accepted_name")]
    colnames(df) <- c("Введённое название", "Статус",
                      "Принятое название")
    df
  }
  
  prepare_for_web_multiple <- function(df)
  {
    res <- do.call(rbind, by(df, 1:nrow(df), function(x)
                   {
                     input <- x[1, 1]
                     x_split <- split.default(x[-1], gl(floor(length(x[-1]) / 3), 3, labels = c("tpl", "wfo", "wcvp", "gbif", "lcvp")), drop = TRUE)
                     x_split <- lapply(x_split, function(x) {
                       db <- strsplit(names(x[1]), "-")[[1]][2]
                       x$db <- db
                       colnames(x) <- c("status", "accepted_name", "accepted_url", "db_name")
                       x
                     })
                     x <- do.call(rbind, x_split)
                     x$search <- ""
                     x[1, "search"] <- input
                     x[c("search", "db_name", "status", "accepted_name", "accepted_url")]
                   }))
    db_names <- res[2]
    db_names$db_name <- factor(db_names$db_name,
                       levels = c("plantlist", "wcvp", "wfo", "gbif", "lcvp"),
                       labels = c("ThePlantlist", "World Checklist of Vascular Plants",
                                  "WorldFloraOnline", "GBIF Backbone",
                                  "Leipzig Catalogue of Vascular Plants"))
    results <- res[-2]
    results <- prepare_for_web(results)
    colnames(db_names) <- "База данных"
    cbind(results[1], db_names, results[-1])
  }
  
  prepare_for_download <- function(df) {
    if(ncol(df) == 4)
    {
      colnames(df) <- c("Введённое название", "Статус",
                        "Принятое название",
                        "Ссылка на сайт с информацией")
    }
    else
    {
      old_colnames <- colnames(df)[-1]
      n_groups <- length(old_colnames) / 3
      print(rep(1:n_groups, each = 3))
      old_colnames_groups <- split(old_colnames, rep(1:n_groups, each = 3))
      print(old_colnames_groups)
      new_colnames_groups <- lapply(old_colnames_groups, function(group)
        {
        suffix <- strsplit(group[1], "-")[[1]][2]
        new_colnames <- paste(c("Статус", "Принятое название", "Ссылка на сайт базы данных"), suffix)
        new_colnames
      })
      new_colnames <- c("Введённое название", do.call(c, new_colnames_groups))
      colnames(df) <- new_colnames
    }
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
      x <- mapply(function(df, suffix) {
        old_colnames <- colnames(df)
        new_colnames <- c(old_colnames[1], paste(old_colnames[-1], suffix, sep = "-"))
        colnames(df) <- new_colnames
        df
      }, x, db, SIMPLIFY = FALSE)
      d <- Reduce(function(a, b) merge(a, b, by = "search", sort = FALSE), x)
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
      if (length(list$plantdb) == 1)
        then(list$result, prepare_for_web)
      else
        then(list$result, prepare_for_web_multiple)
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
# df$accepted_url <- sapply(df$accepted_id, get_url, "plantlist")
# df$accepted_id <- NULL
# saveRDS(df, "sample.rds")
