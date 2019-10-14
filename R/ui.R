#shinyUI( 
ui <- function()fluidPage(
  
  # Application title
  titlePanel("Upload Data With ICD-10-CM"),
  
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file10', 'Find the CSV file with ICD-10-CM',
                accept=c('text/csv', '.csv')),
      
      radioButtons("od_intent", "OD Intent:",
                   c("All" = "all",
                     "Unintentional/Undetermined" = "uu",
                     "Intentional Self-Harm" = "ish")),

      
      h4('Select the columns with ICD 10 CM to include in the definition'),
      
      selectizeInput('ediag', 'Diagnosis and Ecode fields', choices = NULL, multiple = TRUE),
      
      h4('Click the submit button before downloading'),
      
      actionButton("submit", "Submit")
    ),
    
    # download button 
    mainPanel(
      h4('After submitting, click the download button then select where to save the downloaded file'),
      p(downloadButton("download10", "Download file", class = "btn-primary"))
    )
  )
)
