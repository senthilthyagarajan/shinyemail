library(shiny)
library(shinyFiles)
library(fs)
library(shinyAce)
library(mailR)



ui <- fluidPage(
  title = 'Download a PDF report',
  sidebarLayout(
    sidebarPanel(
      helpText(),
      selectInput('x', 'Build a regression model of mpg against:',
                  choices = names(mtcars)[-1]),
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'), inline = TRUE),
      downloadButton('downloadReport'),
      # textInput("from", "From:", value="from@gmail.com"),
      textInput("to", "To:", value="to@gmail.com"),
      textInput("subject", "Subject:", value=""),
      aceEditor("message", value="write message here"),
      shinyFilesButton("file", "File select", "Please select a file", multiple = TRUE),
      actionButton("send", "Send mail")
    ),
    mainPanel(
      plotOutput('regPlot')
      # ,
      # verbatimTextOutput("filepaths")
    )
  )
)

server <- function(input, output, session) {

  regFormula <- reactive({
    as.formula(paste('mpg ~', input$x))
  })

  output$regPlot <- renderPlot({
    par(mar = c(4, 4, .1, .1))
    plot(regFormula(), data = mtcars, pch = 19)
  })

  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  shinyFileChoose(input, "file", roots = volumes, session = session)
  observe({
    cat("\ninput$file value:\n\n")
    path <- parseFilePaths(volumes, input$file)
    print( path$name)
  })

  output$filepaths <- renderPrint({
    parseFilePaths(volumes, input$file)
  })

  observe({
    if(is.null(input$send) || input$send==0) return(NULL)

    subject <- isolate(input$subject)
    msg <- isolate(input$message)
    sender <- "from@gmail.com"
    recipients <- isolate(input$to)
    path <- parseFilePaths(volumes, input$file)
    #fileName <-   shinyDirChoose(input, 'dir', roots = c(home = '~'), filetypes = c('pdf', 'html'))
    send.mail(from = sender,
              to = recipients,
              subject = subject,
              body = msg,
              #body,
              #attach.files = path$datapath,#file.path(folder, fileName),
              attach.files = path$datapath,
              # attach.files  = c("./my-report.pdf"),
              #file.names = c("Download my-report.pdf"),
              # file.names = path$name,
              smtp = list(host.name = "smtp.gmail.com", port = 465,
                          user.name=sender, passwd="*******", ssl=TRUE),
              authenticate = TRUE,
              send = TRUE)

  })

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      src <- normalizePath('report.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)

      library(rmarkdown)
      out <- render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )

}

shinyApp(ui, server)