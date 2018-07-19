#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(RODBC)
library(dplyr)
library(reshape2)
library(ggplot2)
# query CDW
pipeSQL <- function(){
  channel <- RODBC::odbcDriverConnect(connection = "
    driver={SQL Server};
    server=vhacdwdwhsql33.vha.med.va.gov;
    database=LSV;
    trusted_connection=true
    ")
  function(q){
    sqlQuery(channel, q)
  } 
}
# return a melted dataframe
pipeMelt <- function(df){
  function(vars){
    return(melt(df, id.vars = vars))
  }
}
query_sepsis <- function(nDays, presentFlag){
  paste("
    select distinct 
    presentAdmit.PatientSID,
    patient.PatientSSN,
    patient.PatientName,
    patient.DeceasedFlag,
    patient.DeathDateTime,
    cast(inpat.AdmitDateTime as date) as AdmitDate,
    datediff(d, inpat.AdmitDateTime, patient.DeathDateTime) as t2Death,
    presentAdmit.PresentOnAdmissionCode
    from
    LSV.Inpat.PresentOnAdmission as presentAdmit 
    inner join LSV.Inpat.Inpatient as inpat 
    on inpat.InpatientSID = presentAdmit.InpatientSID
    inner join LSV.SPatient.SPatient as patient 
    on presentAdmit.PatientSID = patient.PatientSID 
    and inpat.Sta3n = patient.Sta3n
    inner join LSV.Dim.ICD10 as icd10 
    on presentAdmit.ICD10SID = icd10.ICD10SID
    inner join LSV.Dim.ICD10DescriptionVersion as icd10Label 
    on icd10Label.ICD10SID = icd10.ICD10SID 
    where
    patient.Sta3n = '612'
    and patient.CDWPossibleTestPatientFlag <> 'y'
    and presentAdmit.PresentOnAdmissionCode  =",presentFlag,"
    and inpat.AdmitDateTime >= Dateadd(d, ",-nDays,", getdate()) 
    and (icd10.ICD10Code like ('[A][0-9][0-9]%') 
    OR icd10.ICD10Code like ('R78.81') 
    OR icd10.ICD10Code like ('B37.%')
    OR icd10.ICD10Code like ('R65.2[0-1]%')
    )
    and (datediff(d, inpat.AdmitDateTime, patient.DeathDateTime) <= 30 
    OR datediff(d, inpat.AdmitDateTime, patient.DeathDateTime) is NULL
    )
    order by
    patient.PatientSSN
  ")
}
nDays = 270
tracker_no <- pipeSQL()(query_sepsis(nDays, "'N'")) %>% as.data.frame()
tracker_yes <- pipeSQL()(query_sepsis(nDays, "'Y'")) %>% as.data.frame()
odbcCloseAll()

## END OF DATA FUN
      ###
## BEGIN SHINY APP

# UI
ui <- fluidPage(
  titlePanel("ED Sepsis Admits"),
  # sidebar
  column(4, verbatimTextOutput("value"),
    wellPanel(
      dateRangeInput('dates',
        label = 'Date input: yyyy-mm-dd'
      )
    )
  ),  
  # mainpanel
  mainPanel(
    plotOutput("plot1"),
    plotOutput("plot2")
  )
)
# SERVER
server <- function(input, output, session){
  process_tracker <- function(tracker){
    tracker$AdmitDate <- as.Date(strftime(tracker$AdmitDate, '%Y-%m-%d', origin = "1970-01-01"))
    mortality <- tracker %>% filter(DeceasedFlag == 'Y') %>% group_by(AdmitDate) %>% summarise(deceased = n())
    survive <- tracker %>% filter(DeceasedFlag == 'N') %>% group_by(AdmitDate) %>% summarise(survive = n())
    Admits <- tibble(AdmitDate = seq(min(input$dates), max(input$dates), by = 1)) 
    Admits <- left_join(Admits, mortality)
    Admits <- left_join(Admits, survive)
    Admits$deceased <- Admits$deceased %>% sapply(function(x){if (is.na(x)) 0 else x}) 
    Admits$survive <- Admits$survive %>% sapply(function(x){if (is.na(x)) 0 else x})
    magic <- pipeMelt(Admits)('AdmitDate')
  }
  output$plot1 <- renderPlot({process_tracker(tracker_no) %>% 
      ggplot(aes(x = AdmitDate, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      ggtitle("Counts sepsis NOT presenting on admission") +
      theme(axis.text.x = element_text(size = 12), axis.title.x = element_blank()) +
      theme(axis.text.y = element_text(size = 12), axis.title.y = element_blank()) +
      theme(legend.text = element_text(size = 12), legend.title = element_blank()) +
      theme(legend.background = element_rect(fill = 'yellow', size = 6)) +
      scale_y_continuous(breaks = seq(0, 10, 1))
  })
  output$plot2 <- renderPlot({process_tracker(tracker_yes) %>% 
      ggplot(aes(x = AdmitDate, y = value, fill = variable)) +
      geom_bar(stat = "identity", position = "dodge") +
      ggtitle("Counts sepsis presenting on admission") +
      theme(axis.text.x = element_text(size = 12), axis.title.x = element_blank()) +
      theme(axis.text.y = element_text(size = 12), axis.title.y = element_blank()) +
      theme(legend.text = element_text(size = 12), legend.title = element_blank()) +
      theme(legend.background = element_rect(fill = 'yellow', size = 6)) +
      scale_y_continuous(breaks = seq(0, 10, 1))
  })
}
# APP
shinyApp(ui = ui, server = server)