library(data.table)
library(shiny)
library(DT)
library(plotly)

## Options
options(warn=-1)

########
# Load Data
##
load('data/drugrepodb.RData')

## Status Summary Plotting
#status_dt <- data.table(status = names(table(drug_dt$status)), count = as.integer(table(drug_dt$status)))
status_dt <- drug_dt[, .(count = .N), by=status]
N_DRUGS <- drug_dt[, uniqueN(drug_id)]
N_INDS <- drug_dt[, uniqueN(ind_id)]

#################
# UI Definition #
#################
ui <- fluidPage(
  ## Header
  headerPanel(tags$head(tags$img(src="logo.png", height="80px", width='275px', style = "padding-left: 25px; padding-top: 15px")),
        windowTitle="repoDB"
  ),
  
  tags$br(),
  
  ## Define Navigation
  navlistPanel(
    ## Overview Panel
    tabPanel(
      "Introduction",
      
      p("repoDB contains a standard set of drug repositioning successes and failures that can be
       used to fairly and reproducibly benchmark computational repositioning methods. repoDB data
       was extracted from ", 
       a('DrugCentral', href='http://drugcentral.org/'),
       "and ",
       a('ClinicalTrials.gov.', href='http://clinicaltrials.gov')
      ),
      
      p("The repoDB website has several functionalities, which can be accessed from the navigation bar:",
       tags$ul(
         tags$li("Drug-centric searching"),
         tags$li("Disease-centric searching"),
         tags$li("Full repoDB download")
       )
      ),
      
      p("You can explore the types and characteristics of data in repoDB in the plot below."),
      plotlyOutput("summary_plot")
      
    ),
    
    ## Drug Search Panel
    tabPanel(
      "Drug Search",
      
      p(sprintf('repoDB contains information about %d currently approved drugs (as curated by DrugCentral).
        To search repoDB for a specific drug, select a drug and the current statuses you\'d like to display.
        Drugs are listed with their DrugCentral and DrugBank IDs, for easier integration into your existing pipelines.
        Search results can be downloaded as a tab-separated values file using the download button below the table
        of drug indications.', N_DRUGS)
        
      ),
      uiOutput('drugdrop'),
      checkboxGroupInput('drugcheck',
                'Select the status categories you\'d like to display',
                choices = c('Approved','Terminated','Withdrawn','Suspended'),
                selected = c('Approved','Terminated','Withdrawn','Suspended'),
                inline=T
      ),
      checkboxGroupInput('phasecheckdrug',
                'Select the phases you\'d like to display',
                choices = c('Phase 0', 'Phase 1', 'Phase 2', 'Phase 3'),
                selected = c('Phase 0', 'Phase 1', 'Phase 2', 'Phase 3'),
                inline = T
                
      ),
      tags$hr(),
      dataTableOutput('drugtable'),
      downloadButton(
        outputId = 'drugdownload',
        label = 'Download the current search results'
      )
    ),
    tabPanel(
      "Disease Search",
      
      p(sprintf('repoDB contains information about %d diseases (indications), all mapped to UMLS terms for easier 
        integration into your existing pipelines. To search for a specific disease,
        select a disease and the current statuses you\'d like to display.
        Search results can be downloaded as a tab-separated values file using the download button below the table
        of drug indications.', N_INDS)
      ),
      uiOutput('inddrop'),
      checkboxGroupInput('indcheck',
                'Select the status categories you\'d like to display',
                choices = c('Approved','Terminated','Withdrawn','Suspended'),
                selected = c('Approved','Terminated','Withdrawn','Suspended'),
                inline=T
      ),
      checkboxGroupInput('phasecheckind',
                'Select the phases you\'d like to display',
                choices = c('Phase 0', 'Phase 1', 'Phase 2', 'Phase 3'),
                selected = c('Phase 0', 'Phase 1', 'Phase 2', 'Phase 3'),
                inline = T
                
      ),
      tags$hr(),
      dataTableOutput('indtable'),
      downloadButton(
        outputId = 'inddownload',
        label = 'Download the current search results'
      )
    ),
    tabPanel(
      "Download",
      
      p("The full repoDB database is available for download using the button below.
       Please note that the data is presented as-is, and not all entries have been
       validated before publication." 
      ),
      downloadButton(
        outputId = 'downloadFull',
        label = 'Download the full repoDB Dataset'
      )
    ),
    tabPanel(
      "Citing repoDB",
      
      p("To acknowledge use of the repoDB resource, please cite the following paper:" ),
      tags$code( "Brown AS and Patel CJ. repoDB: A New Standard for Drug Repositioning Validation.", em("Scientific Data."), "170029 (2017)."),
      tags$br(),
      tags$br(),
      p("repoDB was built using the May 16, 2020 release of ",
        a("DrugCentral,", href='http://drugcentral.org/download'),
        "the live version, accessed in June 2020, of the ",
        a("AACT database,", href='https://www.ctti-clinicaltrials.org/aact-database'),
        "and the 2020AA Release of the ",
        a("Unified Medical Language System.", href='https://www.nlm.nih.gov/research/umls/'),
        "Metformin and recycling symbol used under CC0 license from wikimedia commons. Database symbol by Designmodo,
        used under a CC3.0 licnesne."
      ),
      p (strong("By using the repoDB database, users agree to cite our work, as well as AACT,
            DrugCentral, and UMLS for their role in data curation. This data is available under a ",
            a('Creative Commons Attribution 4.0 International License.',href='https://creativecommons.org/licenses/by/4.0/')
            )
      )
    ),
    tabPanel(
      "Version History",
      p("As repoDB is improved and augmented with new data, we will track any substantial changes made to repoDB here:"),
      tags$ul(
        tags$li(strong('v1.0 (March 14, 2017)'), ' - Initial release'),
        tags$li(strong('v1.1 (June 26, 2017)'), ' - Fixed a bug in the ClinicalTrials.gov parser that created multiple DrugBank Identifiers for a
            single drug (many thanks to Cristina Leal for spotting the error).'),
        tags$li(strong('v1.2 (July 28, 2017)'), ' -', code('Version History'), ' tab was added to address discrepancies introduced in totals for Terminated
            Withdrawn, and Suspended drug-disease pairs versus published values due to bugfix in v1.1 (many thanks to Beste Turanli for
            spotting the discrepancy).'),
        tags$li(strong('v2.0-SNAPSHOT (June 12, 2020)'), ' - Updated with new versions of DrugCentral, AACT and UMLS, in cooperation with developers of DrugCentral from the University of New Mexico.')
      )
    )
    
  ),

  ## Footer
  tags$hr(),
  p(strong('repoDB is intended for educational and scientific research purposes only.'),
    'This work is licensed under a ',
    a('Creative Commons Attribution 4.0 International License.', href="http://creativecommons.org/licenses/by/4.0/"),
    'repoDB was developed by AS Brown and CJ Patel. See the "Citing repoDB" tab for citation information. In 2020, repoDB was updated with new versions of DrugCentral, AACT, and UMLS, in cooperation with Tudor Oprea and co-workers, developers of DrugCentral from the University of New Mexico Translational Informatics Division.',
    'For more projects, visit the ', a('Patel Group Homepage.', href='http://www.chiragjpgroup.org/')
  )
)


#####################
# Server Definition #
#####################

server <- function(input, output, session) {

  # Infographic definition
  output$summary_plot <- renderPlotly({
    plot_ly(type="bar", orientation="v", data=dcast(drug_dt[, .(count = .N), by=c("status", "sem_type")], status ~ sem_type,  value.var = "count"),
            x=~status, y=~`Acquired Abnormality`, name="Acquired Abnormality") %>%
      add_trace(y=~`Disease or Syndrome`, name="Disease or Syndrome") %>%
      add_trace(y=~`Sign or Symptom`, name="Sign or Symptom") %>%
      add_trace(y=~`Neoplastic Process`, name="Neoplastic Process") %>%
      add_trace(y=~`Cell or Molecular Dysfunction`, name="Cell or Molecular Dysfunction") %>%
      add_trace(y=~`Congenital Abnormality`, name="Congenital Abnormality") %>%
      add_trace(y=~`Finding`, name="Finding") %>%
      add_trace(y=~`Injury or Poisoning`, name="Injury or Poisoning") %>%
      add_trace(y=~`Mental or Behavioral Dysfunction`, name="Mental or Behavioral Dysfunction") %>%
      add_trace(y=~`Pathologic Function`, name="Pathologic Function") %>%
      layout(barmode="stack", xaxis=list(title="Status"), yaxis=list(title="Count"), 
             title = list(text=paste0("Drug indication counts (N = ", nrow(drug_dt), ")"), font=list(size=14)), font=list(family="Arial", size=12),
             legend=list(x=.8, y=1.1), margin=list(t=80, l=20, b=20, r=20))
  })
  
  # Dropmenu Definition
  output$drugdrop <- renderUI({
    selectizeInput(
      inputId = 'drugdrop',
      label = 'Select a drug from the dropdown menu, or enter a search term:',
      choices = sort(unique(drug_dt$drug_name)),
      selected = 'Sitagliptin',
      width = '100%',
      multiple = F,
      options = list(maxOptions = length(unique(drug_dt$drug_name)))
    )
  })
  
  output$inddrop <- renderUI({
    selectizeInput(
      inputId = 'inddrop',
      label = 'Select an indication from the dropdown menu, or enter a search term:',
      choices = sort(unique(drug_dt$ind_name)),
      selected = 'Diabetes Mellitus, Non-Insulin-Dependent',
      width = '100%', 
      multiple = F,
      options = list(maxOptions = length(unique(drug_dt$ind_name)))
    )
  })
  
  # Reactive Datatable Subsetting
  drugreact <- reactive({
    drugtable <- subset(drug_dt, drug_name == input$drugdrop & status %in% input$drugcheck & (is.na(phase) | phase %in% input$phasecheckdrug),
              select = c('Drug', 'Indication', 'TrialStatus', 'DetailedStatus'))
    return(drugtable)
  })
  
  indreact <- reactive({
    indtable <- subset(drug_dt, ind_name == input$inddrop & status %in% input$indcheck & (is.na(phase) | phase %in% input$phasecheckind),
              select = c('Drug', 'Indication', 'TrialStatus','DetailedStatus'))
    return(indtable)
  })
  
  # Reactive UI Datatable Definition
  output$drugtable <- renderDataTable({
    DT::datatable(drugreact(), options = list(pageLength = 5), rownames = F, escape = F)
  })
  output$indtable <- renderDataTable({
    DT::datatable(indreact(), options = list(pageLength = 5), rownames = F, escape = F)
  })
  
  # Subset download handlers
  output$drugdownload <- downloadHandler(
    filename = 'drugsearch.tsv',
    content = function(file) {
      drugtable <- subset(drug_dt, drug_name == input$drugdrop & status %in% input$drugcheck & (is.na(phase) | phase %in% input$phasecheckdrug),
                select = c('drug_name','drug_id','ind_name','ind_id','NCT','status','phase','DetailedStatus'))
      write.table(drugtable, file, sep='\t', row.names = F)
    }
  )
  
  output$inddownload <- downloadHandler(
    filename = 'diseasesearch.tsv',
    content = function(file) {
      indtable <- subset(drug_dt, ind_name == input$inddrop & status %in% input$drugcheck & (is.na(phase) | phase %in% input$phasecheckdrug),
                select = c('drug_name','drug_id','ind_name','ind_id','NCT','status','phase','DetailedStatus'))
      write.table(indtable, file, sep='\t', row.names = F)
    }
  )
  
  # Full download handler
  output$downloadFull <- downloadHandler(
    filename = 'full.csv',
    content = function(file) {
      table <- subset(drug_dt, select = c('drug_name','drug_id','ind_name','ind_id','NCT','status','phase','DetailedStatus'))
      write.table(table, file, sep = ',', row.names = FALSE)
    }
  )
  
}

shinyApp(ui, server)

