server <- function(input, output, session) {

  require(dplyr)
  require(purrr)
  
  hosp_data10 <- shiny::reactive({
    
    infile10 <- input$file10
    
    shiny::req(infile10)
    
    readr::read_csv(infile10$datapath)
  })
  
  
  shiny::observeEvent(input$file10,{
    shiny::updateSelectizeInput(session, 'ediag', choices = names(hosp_data10()))
  })
  
  outputdata10 <- 
    shiny::reactive(if(!is.null(input$ediag)){
      sel10 = unique(unlist(list(strsplit(input$ediag,","))))
      hosp_data10() %>% 
        select(!!sel10, everything())
    })
  
  dxm10 <- shiny::reactive(1:ncol(outputdata10()))
  
  
  outputdata10_2 <- shiny::eventReactive(input$submit, {outputdata10() %>%
      s_drug_opioid(., diag_ecode_col = dxm10()) })
  # %>% 
  #     s_drug_opioid_uu(., diag_ecode_col = dxm10()) %>% 
  #     s_drug_opioid_ish(., diag_ecode_col = dxm10())})
  # 
  # outputdata10_2()
  
  output$download10 <- shiny::downloadHandler(
    filename = function() {
      paste0("output_", input$file10)
    },
    content = function(file) {
      readr::write_csv(outputdata10_2() %>% 
                  select(!!names(hosp_data10()), everything()), file)
    }
  )
  
}
