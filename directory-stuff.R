# ------------------- CHOOSE WORKING DIRECTORY -----------------------------
## COPIED FROM https://community.rstudio.com/t/shiny-directory-input/29160/2

shinyDirChoose(
  input,
  'dir',
  roots = c(home = '~'),
  filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
)

global <- reactiveValues(datapath = paste0(getwd()))

dir <- reactive(input$dir)

output$dir <- renderText({
  global$datapath
})

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$dir
             },
             handlerExpr = {
               if (!"path" %in% names(dir())) return()
               home <- normalizePath("~")
               global$datapath <-
                 file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
             })



output$wd <- renderText({
  paste('<B>Workout Directory: </B>')
})

