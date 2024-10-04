#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(DT)
library(plotly)
library(readr)
library(RODBC)
library(tidyverse)
library(writexl)
library(profvis)
library(bslib)
library(treemapify)
library(viridis)
con <- odbcConnect("IeDEA_MR_1")
databases <- sqlQuery(con,"SELECT name
                      FROM master.sys.databases
                      where name not in ('master','tempdb','model','msdb','CIDER_TIER','CIDER_CONCEPTS')
                      and name not like 'CIDER_IeDEA_%'
                      and name like 'CIDER_%' ")
custom_theme <- bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0199F8",
  secondary = "#FF374B",
  base_font = "Maven Pro"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
                                   theme = shinytheme("sandstone"),
                                       # tags$head(
                                       #  tags$style(
                                       #   HTML( ".title-panel a {
                                       #          font-size: 12pt;
                                       #          font-family: garamond;
                                       #          font-style: italic;
                                       #          margin: 0;
                                       #         }"
                                       #        )
                                       #            )
                                       #         ),

                                    titlePanel(title=div(
                                                         img(src="logo2.png"
                                                         ,span(" Compare ", span("IeDEA", style = "color: darkred"), span("cohort extractions") , style ="font-weight: bold; font-size: 50px;float:right")
                                                         , height =100, width = 400
                                                         , style = " float:center"))

                                               ),

                # Sidebar with a slider input for number of bins
                                   sidebarLayout(
                    sidebarPanel(
                      p("This application is used for internal data validation within the data management team to understand data integrity between data extractions for the same cohort within ",a("IeDEA South Africa. "),
                          href = "https://www.iedea-sa.org/"),
                        selectInput("extract_1", "Previous Data Extraction", choices = databases, selected = "KHAYELITSHA_7"),
                        selectInput("extract_2", "Current Data Extraction", choices  = databases, selected = "KHAYELITSHA_8"),
                      shinycssloaders::withSpinner(plotlyOutput("age_profile"), type = 5),
                      shinycssloaders::withSpinner(plotlyOutput("enr_plot"), type = 5)
                    ),

                    # Show a plot of the generated distribution
                    mainPanel(
                        tabsetPanel(
                            # tabPanel("Tables", selectInput("indicator", "Indicator"
                            #                                , choices = c("Total population by Year","Population by Age Group","Total Households by Year", "Total Fertility Rate by Year", "Fertility by Age Group", "Adult mortality rates","Child mortality rates","Infant Mortality Rates","Life expectancy at Birth","Unemployment by Year","Unemployment by Agegroup")
                            #                                , multiple = FALSE, selected = 1), dataTableOutput("table1"),downloadButton('download',"Download the data")),
                            tabPanel("Enrollments", fluidRow(column(12,shinycssloaders::withSpinner(plotlyOutput("enrol"), type = 5))),
                                                  fluidRow(column(12,shinycssloaders::withSpinner(plotlyOutput("bas_plot"), type = 5)))),
                            #tabPanel("Age profile",shinycssloaders::withSpinner(plotlyOutput("age_profile"))),
                            tabPanel("Clinical measurements & ART",fluidRow(column(12,shinycssloaders::withSpinner(plotlyOutput("vl_count_plot"), type = 5))),
                                     fluidRow(column(12,shinycssloaders::withSpinner(plotlyOutput("cd4_count_plot"), type = 5))),
                                     fluidRow(column(12,shinycssloaders::withSpinner(plotlyOutput("art_count_plot"), type = 5)))),
                            tabPanel("Lost To Follow up",fluidRow(column(6,shinycssloaders::withSpinner(plotlyOutput('death_plot'), type = 5)),
                                                            column(6,shinycssloaders::withSpinner(plotlyOutput('npr_fac_plot'), type = 5)))
                                     ,fluidRow(column(6,shinycssloaders::withSpinner(plotlyOutput("drop_plot"), type = 5)),
                                               column(6,shinycssloaders::withSpinner(plotlyOutput("ltfu_plot"), type = 5)))),
                            tabPanel("CD4 & Viral Load",fluidRow(column(12,shinycssloaders::withSpinner(plotlyOutput("vl_plot"), type = 5))),
                                                        fluidRow(column(12,shinycssloaders::withSpinner(plotlyOutput("cd4_plot"), type = 5)))),
                            tabPanel("Other Diseases",shinycssloaders::withSpinner(plotlyOutput("dis_plot"), type = 5)),
                            tabPanel("Lab measures",shinycssloaders::withSpinner(plotlyOutput("lab"), type = 5)),
                            tabPanel("ART Regimen",fluidRow(column(6,shinycssloaders::withSpinner(plotlyOutput('art_gauge_2'), type = 5))
                                                            ,column(6,shinycssloaders::withSpinner(plotlyOutput('art_gauge_1'), type = 5)))
                                                  ,fluidRow(column(12,shinycssloaders::withSpinner(plotOutput("art_plot"), type = 5)))),
                            tabPanel("Visits",fluidRow(column(12,shinycssloaders::withSpinner(plotlyOutput("vis_plot"), type = 5)))
                                             ,fluidRow(column(6,shinycssloaders::withSpinner(plotlyOutput('weigh_plot'), type = 5)),
                                               column(6,shinycssloaders::withSpinner(plotlyOutput('heigh_plot'), type = 5)))),
                            tabPanel("Summary table", shinycssloaders::withSpinner(dataTableOutput("table1"), type = 5), downloadButton("download"))

                        )
                    )
                )
)


# Define server logic required to draw a histogram

server <- function(input, output) {
  
  
  
  #sql data extracts
  
    lab <- reactive({
        sqlQuery(con,paste0("select year(lab_d) year, lab_id, count(*) cnt, 'Previous' source  from ",input$extract_1,"..tblLAB where year(lab_d) < 2025
                group by year(lab_d), lab_id union select year(lab_d) year, lab_id, count(*) cnt,'Current' source  from ",input$extract_2,"..tblLAB where year(lab_d) < 2023
                group by year(lab_d), lab_id" ))

    })
    
    data_age_pro <- reactive({
      rbind(sqlQuery(con,paste0("select sex, case when datediff(year,birth_d, enrol_d) between 0 and 4 then 0
            when datediff(year,birth_d, enrol_d) between 5 and 9 then 1
			when datediff(year,birth_d, enrol_d) between 10 and 14 then 2
			when datediff(year,birth_d, enrol_d) between 15 and 19 then 3
			when datediff(year,birth_d, enrol_d) between 20 and 24 then 4
			when datediff(year,birth_d, enrol_d) between 25 and 29 then 5
			when datediff(year,birth_d, enrol_d) between 30 and 34 then 6
			when datediff(year,birth_d, enrol_d) between 35 and 39 then 7
			when datediff(year,birth_d, enrol_d) between 40 and 44 then 8
			when datediff(year,birth_d, enrol_d) between 45 and 49 then 9
			when datediff(year,birth_d, enrol_d) between 50 and 54 then 10
			when datediff(year,birth_d, enrol_d) between 55 and 59 then 11
			when datediff(year,birth_d, enrol_d) between 60 and 64 then 12
			when datediff(year,birth_d, enrol_d) between 65 and 69 then 13
			else 14 end AgeGroup, count(*) Total, 'Previous' source
from ",input$extract_1,"..tblBAS
group by sex, case when datediff(year,birth_d, enrol_d) between 0 and 4 then 0
            when datediff(year,birth_d, enrol_d) between 5 and 9 then 1
			when datediff(year,birth_d, enrol_d) between 10 and 14 then 2
			when datediff(year,birth_d, enrol_d) between 15 and 19 then 3
			when datediff(year,birth_d, enrol_d) between 20 and 24 then 4
			when datediff(year,birth_d, enrol_d) between 25 and 29 then 5
			when datediff(year,birth_d, enrol_d) between 30 and 34 then 6
			when datediff(year,birth_d, enrol_d) between 35 and 39 then 7
			when datediff(year,birth_d, enrol_d) between 40 and 44 then 8
			when datediff(year,birth_d, enrol_d) between 45 and 49 then 9
			when datediff(year,birth_d, enrol_d) between 50 and 54 then 10
			when datediff(year,birth_d, enrol_d) between 55 and 59 then 11
			when datediff(year,birth_d, enrol_d) between 60 and 64 then 12
			when datediff(year,birth_d, enrol_d) between 65 and 69 then 13
			else 14 end")),
            sqlQuery(con,paste0("select sex, case when datediff(year,birth_d, enrol_d) between 0 and 4 then 0
            when datediff(year,birth_d, enrol_d) between 5 and 9 then 1
			when datediff(year,birth_d, enrol_d) between 10 and 14 then 2
			when datediff(year,birth_d, enrol_d) between 15 and 19 then 3
			when datediff(year,birth_d, enrol_d) between 20 and 24 then 4
			when datediff(year,birth_d, enrol_d) between 25 and 29 then 5
			when datediff(year,birth_d, enrol_d) between 30 and 34 then 6
			when datediff(year,birth_d, enrol_d) between 35 and 39 then 7
			when datediff(year,birth_d, enrol_d) between 40 and 44 then 8
			when datediff(year,birth_d, enrol_d) between 45 and 49 then 9
			when datediff(year,birth_d, enrol_d) between 50 and 54 then 10
			when datediff(year,birth_d, enrol_d) between 55 and 59 then 11
			when datediff(year,birth_d, enrol_d) between 60 and 64 then 12
			when datediff(year,birth_d, enrol_d) between 65 and 69 then 13
			else 14 end AgeGroup, count(*) Total, 'Current' source
from ",input$extract_2,"..tblBAS
group by sex, case when datediff(year,birth_d, enrol_d) between 0 and 4 then 0
            when datediff(year,birth_d, enrol_d) between 5 and 9 then 1
			when datediff(year,birth_d, enrol_d) between 10 and 14 then 2
			when datediff(year,birth_d, enrol_d) between 15 and 19 then 3
			when datediff(year,birth_d, enrol_d) between 20 and 24 then 4
			when datediff(year,birth_d, enrol_d) between 25 and 29 then 5
			when datediff(year,birth_d, enrol_d) between 30 and 34 then 6
			when datediff(year,birth_d, enrol_d) between 35 and 39 then 7
			when datediff(year,birth_d, enrol_d) between 40 and 44 then 8
			when datediff(year,birth_d, enrol_d) between 45 and 49 then 9
			when datediff(year,birth_d, enrol_d) between 50 and 54 then 10
			when datediff(year,birth_d, enrol_d) between 55 and 59 then 11
			when datediff(year,birth_d, enrol_d) between 60 and 64 then 12
			when datediff(year,birth_d, enrol_d) between 65 and 69 then 13
			else 14 end")))
    })
    
    data_art <- reactive({
      sqlQuery(con,paste0("with a as
(
select a.patient, a.art_id collate SQL_Latin1_General_CP1_CI_AS art_id, art_sd, case when art_ed is null then l_alive_d else art_ed end art_ed, 'Previous' source
from ",input$extract_1,"..tblART a
join ",input$extract_1,"..tblLTFU l on a.patient=l.patient
union
select a.patient, a.art_id, art_sd, case when art_ed is null then l_alive_d else art_ed end art_ed, 'Current' source
from ",input$extract_2,"..tblART a
join ",input$extract_2,"..tblLTFU l on a.patient=l.patient
)
select source, artdefinition, sum(datediff(month, art_sd, art_ed))*1.0/12 years
from a
join CIDER_IeDEA_MR_1..ART_ID_Mapping c on c.art_id=a.art_id
group by source, artdefinition
"))
    })
    
    data_enrol <- reactive({
      rbind(
      sqlQuery(con,paste0("select patient, birth_d, enrol_d, sex, mode, naive_y, proph_y, recart_y, recart_d, aids_y, aids_d, hiv_pos_d, 'Previous' source from ",
                          input$extract_1,
                          "..tblBAS"))
      ,
      sqlQuery(con,paste0("select patient, birth_d, enrol_d, sex, mode, naive_y, proph_y, recart_y, recart_d, aids_y, aids_d, hiv_pos_d, 'Current' source from ",
                          input$extract_2,
                          "..tblBAS")))
    })
    
    
    data_vl <- reactive({
      rbind(
        sqlQuery(con,paste0("select year(rna_d) year, count(*) total, 'Previous' source
from ",input$extract_1,"..tblLAB_RNA
group by year(rna_d)
order by year(rna_d)")),
        sqlQuery(con,paste0("select year(rna_d) year, count(*) total, 'Current' source
from ",input$extract_2,"..tblLAB_RNA
group by year(rna_d)
order by year(rna_d)"))
      )
    })
    
    
    data_vl2 <- reactive({
      rbind(
        sqlQuery(con,paste0("select year(rna_d) year, case when cast(rna_v as float) <125 then 0 else 1 end suppressed, count(*) total, 'Previous' source
from ",input$extract_1,"..tblLAB_RNA
group by year(rna_d), case when cast(rna_v as float) <125 then 0 else 1 end
order by year(rna_d), case when cast(rna_v as float) <125 then 0 else 1 end")),
        sqlQuery(con,paste0("select year(rna_d) year, case when cast(rna_v as float) <125 then 0 else 1 end suppressed, count(*) total, 'Current' source
from ",input$extract_2,"..tblLAB_RNA
group by year(rna_d), case when cast(rna_v as float) <125 then 0 else 1 end
order by year(rna_d), case when cast(rna_v as float) <125 then 0 else 1 end"))
      )
    })
    
    data_cd4 <- reactive({
      rbind(
        sqlQuery(con,paste0("select year(cd4_d) year, count(*) total, 'Previous' source
from ",input$extract_1,"..tblLAB_CD4
where cd4_u =1
group by year(cd4_d)")),
        sqlQuery(con,paste0("select year(cd4_d) year, count(*) total, 'Current' source
from ",input$extract_2,"..tblLAB_CD4
where cd4_u =1
group by year(cd4_d)"))
      )
    })
    
    data_art2 <- reactive({
      rbind(
        sqlQuery(con,paste0("with a as (
select distinct art_sd
from ",input$extract_1,"..tblART
)
select year(art_sd) year, count(*) total, 'Previous' source
from a
group by year(art_sd) ")),
      sqlQuery(con,paste0("with a as (
select distinct art_sd
from ",input$extract_2,"..tblART
)
select year(art_sd) year, count(*) total, 'Current' source
from a
group by year(art_sd) "))
      )
    })


    data_cd42<- reactive({
      rbind(
        sqlQuery(con,paste0("select year(cd4_d) year, case when cast(cd4_v as float) < 200.0 then 0 when cast(cd4_v as float) between 200.0 and 500.0 then 1  when cast(cd4_v as float) between 500.0 and 1600.0 then 2 else 3 end range, count(*) total, 'Previous' source
from ",input$extract_1,"..tblLAB_CD4
where cd4_u =1
group by year(cd4_d),case when cast(cd4_v as float) < 200.0 then 0 when cast(cd4_v as float) between 200.0 and 500.0 then 1  when cast(cd4_v as float) between 500.0 and 1600.0 then 2 else 3 end")),
        sqlQuery(con,paste0("select year(cd4_d) year, case when cast(cd4_v as float) < 200.0 then 0 when cast(cd4_v as float) between 200.0 and 500.0 then 1  when cast(cd4_v as float) between 500.0 and 1600.0 then 2 else 3 end range, count(*) total, 'Current' source
from ",input$extract_2,"..tblLAB_CD4
where cd4_u =1
group by year(cd4_d),case when cast(cd4_v as float) < 200.0 then 0 when cast(cd4_v as float) between 200.0 and 500.0 then 1  when cast(cd4_v as float) between 500.0 and 1600.0 then 2 else 3 end"))
      )
    })
    
    data_death<- reactive({
      sqlQuery(con,paste0("select year(death_d) year, count(*) deaths, 'Current' source
from  ",input$extract_2,"..tblLTFU
where death_y=1
group by year(death_d)
union
select year(death_d) year, count(*) deaths, 'Previous' source
from  ",input$extract_1,"..tblLTFU
where death_y=1
group by year(death_d) "))
    })
   
    data_rev <- reactive({
      sqlQuery(con,paste0("select year(npr_death_d) year, count(*) deaths, 'Current' source
from  ",input$extract_2,"..tblREV_VITAL_STATUS
where vital_status=1
group by year(npr_death_d)
union
select year(npr_death_d) year, count(*) deaths, 'Previous' source
from  ",input$extract_1,"..tblREV_VITAL_STATUS
where vital_status=1
group by year(npr_death_d) "))
    })
    
    data_drop <- reactive({
      sqlQuery(con,paste0("select year(drop_d) year, count(*) deaths, 'Current' source
from  ",input$extract_2,"..tblLTFU
where drop_y=1
group by year(drop_d)
union
select year(drop_d) year, count(*) deaths, 'Previous' source
from  ",input$extract_1,"..tblLTFU
where drop_y=1
group by year(drop_d) "))
    })
    
    data_npr <- reactive({
      sqlQuery(con,paste0("select year(death_d) year,'facility' source, count(*) deaths, 'Previous' extract
from ",input$extract_1,"..tblLTFU
where death_d is not null
group by year(death_d)
union
select year(NPR_DEATH_D) year, 'npr' source, count(*) deaths, 'Previous' extract
from ",input$extract_1,"..tblREV_VITAL_STATUS
where NPR_DEATH_D is not null
group by year(NPR_DEATH_D)
                union
select year(death_d) year,'facility' source, count(*) deaths, 'Current' extract
from ",input$extract_2,"..tblLTFU
where death_d is not null
group by year(death_d)
union
select year(NPR_DEATH_D) year, 'npr' source, count(*) deaths, 'Current' extract
from ",input$extract_2,"..tblREV_VITAL_STATUS
where NPR_DEATH_D is not null
group by year(NPR_DEATH_D)"))
    })
    
    
    data_enr3 <- reactive({
      sqlQuery(con,paste0("with a as (
select tblLTFU*1.0/tblBAS*100 tblLTFU, tblART* 1.0/tblBAS*100 tblART, tblDIS*1.0/tblBAS*100 tblDIS, tblMED*1.0/tblBAS*100 tblMED, tblLAB*1.0/tblBAS*100 tblLAB, tblLAB_RNA*1.0/tblBAS*100 tblLAB_RNA, tblLAB_CD4*1.0/tblBAS*100 tblLAB_CD4, tblVIS*1.0/tblBAS*100 tblVIS, 'Current' source
from (select count(distinct patient) tblBAS
from ",input$extract_2,"..tblBAS) b1
cross join (select count(distinct patient) tblLTFU
       from ",input$extract_2,"..tblLTFU) b2
cross join (select count(distinct patient) tblART
       from ",input$extract_2,"..tblART ) b3
cross join (select count(distinct patient) tblDIS
       from ",input$extract_2,"..tblDIS ) b4
cross join (select count(distinct patient) tblMED
       from ",input$extract_2,"..tblMED ) b5
cross join (select count(distinct patient) tblLAB
       from ",input$extract_2,"..tblLAB ) b6
cross join (select count(distinct patient) tblLAB_RNA
       from ",input$extract_2,"..tblLAB_RNA ) b7
cross join (select count(distinct patient) tblLAB_CD4
       from ",input$extract_2,"..tblLAB_CD4 ) b8
cross join (select count(distinct patient) tblVIS
       from ",input$extract_2,"..tblVIS ) b9
union
select tblLTFU*1.0/tblBAS*100 tblLTFU, tblART* 1.0/tblBAS*100 tblART, tblDIS*1.0/tblBAS*100 tblDIS, tblMED*1.0/tblBAS*100 tblMED, tblLAB*1.0/tblBAS*100 tblLAB, tblLAB_RNA*1.0/tblBAS*100 tblLAB_RNA, tblLAB_CD4*1.0/tblBAS*100 tblLAB_CD4, tblVIS*1.0/tblBAS*100 tblVIS, 'Previous' source
from (select count(distinct patient) tblBAS
from ",input$extract_1,"..tblBAS) b1
cross join (select count(distinct patient) tblLTFU
       from ",input$extract_1,"..tblLTFU) b2
cross join (select count(distinct patient) tblART
       from ",input$extract_1,"..tblART ) b3
cross join (select count(distinct patient) tblDIS
       from ",input$extract_1,"..tblDIS ) b4
cross join (select count(distinct patient) tblMED
       from ",input$extract_1,"..tblMED ) b5
cross join (select count(distinct patient) tblLAB
       from ",input$extract_1,"..tblLAB ) b6
cross join (select count(distinct patient) tblLAB_RNA
       from ",input$extract_1,"..tblLAB_RNA ) b7
cross join (select count(distinct patient) tblLAB_CD4
       from ",input$extract_1,"..tblLAB_CD4 ) b8
cross join (select count(distinct patient) tblVIS
       from ",input$extract_1,"..tblVIS ) b9
	   )
, b as (
select *
from a
unpivot ( perc for [table] in
                 ([tblLTFU], [tblART], [tblDIS], [tblMED], [tblLAB], [tblLAB_RNA], [tblLAB_CD4], [tblVIS])
) as p
    )
                 select * , case when [table]= 'tblLTFU' then 1 when [table]='tblVIS' then 2 when [table]='tblART' then 3 when [table]='tblLAB_CD4' then 4 when [table]='tblLAB_RNA' then 5 when [table]='tblLAB' then 6 when [table]='tblMED' then 7 when [table]='tblDIS' then 8 end [order]
                 from b"))
    })
    
    data_bas <- reactive({
      sqlQuery(con,paste0("select *
from (
select sum(case when naive_y is not null and  naive_y <>9 then 1 else 0 end)*100.0/ count(*) naive_y,
                   sum(case when proph_y is not null and  proph_y <>9 then 1 else 0 end)*100.0/ count(*) proph_y,
                   sum(case when mode is not null and mode <>99 then 1 else 0 end)*100.0/ count(*) mode,
				   sum(case when aids_y is not null and aids_y <> 9 then 1 else 0 end)*100.0/count(*) aids_y,
				   sum(case when sex is not null and sex<> 9 then 1 else 0 end)*100.0/count(*) sex,
				   sum(case when enrol_d is not null then 1 else 0 end)*100.0/count(*) enrol_d,
				   sum(case when birth_d is not null then 1 else 0 end)*100.0/count(*) birth_d,
				   sum(case when recart_d is not null then 1 else 0 end)*100.0/count(*) recart_d,
				   sum(case when hiv_pos_d is not null then 1 else 0 end)*100.0/count(*) hiv_pos_d,
				   'Current' source
       from ",input$extract_2,"..tblBAS
	   union
select	   sum(case when naive_y is not null and  naive_y <>9 then 1 else 0 end)*100.0/ count(*) naive_y,
                   sum(case when proph_y is not null and  proph_y <>9 then 1 else 0 end)*100.0/ count(*) proph_y,
                   sum(case when mode is not null and mode <>99 then 1 else 0 end)*100.0/ count(*) mode,
				   sum(case when aids_y is not null and aids_y <> 9 then 1 else 0 end)*100.0/count(*) aids_y,
				   sum(case when sex is not null and sex<> 9 then 1 else 0 end)*100.0/count(*) sex,
				   sum(case when enrol_d is not null then 1 else 0 end)*100.0/count(*) enrol_d,
				   sum(case when birth_d is not null then 1 else 0 end)*100.0/count(*) birth_d,
				   sum(case when recart_d is not null then 1 else 0 end)*100.0/count(*) recart_d,
				   sum(case when hiv_pos_d is not null then 1 else 0 end)*100.0/count(*) hiv_pos_d,
				   'Previous' source
       from ",input$extract_1,"..tblBAS) a
unpivot ( perc for [table] in
                 ([birth_d], [sex], [naive_y], [proph_y], [mode], [aids_y], [recart_d], [enrol_d],[hiv_pos_d])
) as p"))
    })
    
    
    data_ltfu <- reactive({
      sqlQuery(con,paste0("select *
from (
select sum(case when drop_y is not null and  drop_y <>9 then 1 else 0 end)*100.0/ count(*) drop_y,
                   sum(case when death_y is not null and  death_y <>9 then 1 else 0 end)*100.0/ count(*) death_y,
                   sum(case when drop_rs is not null and drop_rs <>99 then 1 else 0 end)*100.0/ count(*) drop_rs,
				   sum(case when l_alive_d is not null then 1 else 0 end)*100.0/count(*) l_alive_d,
				   'Current' source
       from ",input$extract_2,"..tblLTFU
	   union
select	   sum(case when drop_y is not null and  drop_y <>9 then 1 else 0 end)*100.0/ count(*) drop_y,
                   sum(case when death_y is not null and  death_y <>9 then 1 else 0 end)*100.0/ count(*) death_y,
                   sum(case when drop_rs is not null and drop_rs <>99 then 1 else 0 end)*100.0/ count(*) drop_rs,
				   sum(case when l_alive_d is not null then 1 else 0 end)*100.0/count(*) l_alive_d,
				   'previous' source
       from ",input$extract_1,"..tblLTFU) a
unpivot ( perc for [table] in
                 ([drop_y], [death_y], [drop_rs], [l_alive_d])
) as p"))
    }) 
    
    
    data_heigh <- reactive({
    sqlQuery(con,paste0("select patient, vis_d, cast(heigh as float) heigh, 'Previous' source
                     from ",input$extract_1, "..tblVIS
                     where heigh is not null
                  union
                  select patient, vis_d, cast(heigh as float) heigh, 'Current' source
                     from ",input$extract_2,"..tblVIS
                                    where heigh is not null"))
    })
    
    
    data_weigh <- reactive({
    sqlQuery(con,paste0("select patient, vis_d, cast(weigh as float) weigh, 'Previous' source
                     from ",input$extract_1, "..tblVIS
                     where weigh is not null
                  union
                  select  patient, vis_d, cast(weigh as float) weigh, 'Current' source
                     from ",input$extract_2,"..tblVIS
                                    where weigh is not null"))
    })
    
    data_dis <- reactive({
      sqlQuery(con,paste0("select patient, year(dis_d) dis_year,  dis_id collate SQL_Latin1_General_CP1_CI_AS dis_id, 'Previous' source
from ", input$extract_1,"..tblDIS
                                    union
                                   select patient, year(dis_d) dis_year, dis_id, 'Current' source
from ",input$extract_2,"..tblDIS"))
    })
    
    
    data_vis <- reactive({
      sqlQuery(con,paste0("select *,
				   'Current' source
       from ",input$extract_2,"..vw_vis_summary
	   union
select *,
				   'Previous' source
       from ",input$extract_1,"..vw_vis_summary")
      )
    })
    
    
    data_t1 <- reactive({
      sqlQuery(con,paste0("select v1.[Table], v1.Measure, v1.[Current], v2.[Current] Previous from ",
                          input$extract_2,
                          "..vw_Summary v1 left join ",
                          input$extract_1,
                          "..vw_Summary v2 on v1.[Table]=v2.[Table] and v1.Measure=v2.Measure
order by case when v1.[Table] ='tblBAS' then 1
              when v1.[Table] ='tblLTFU' then 2
			  when v1.[Table] ='tblART' then 3
			  when v1.[Table] ='tblLAB_CD4' then 4
			  when v1.[Table] ='tblLAB_RNA' then 5
			  when v1.[Table] ='tblVIS' then 6
			  when v1.[Table] ='tblLAB' then 7 end"))
    })
   
     output$age_profile <- renderPlotly({
        ggplotly(
         ggplot( data_age_pro() %>%
                   mutate(Tot = case_when( sex==1 ~  -1*Total,
                                               sex==2 ~ 1*Total),
                          sex = factor(as.factor(sex), levels = c(1,2), labels= c("Male","Female")),
                          AgeGroup = factor(as.factor(AgeGroup), levels =c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14)
                                            , labels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70+") )
                   )

               , aes(x =AgeGroup, y = Tot, fill = sex)
               ) +
            geom_bar(stat = "identity") +
            facet_grid(~source)+
            scale_y_continuous(labels =abs, n.breaks = 10) +
            labs(x = "Age group",y = "Number of patients at enrolment", fill = "Sex") +
          #  scale_x_discrete(breaks= c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14), labels=c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70+"))+
            coord_flip()+
            scale_fill_viridis(discrete = TRUE)+
            #scale_fill_manual(values=c("dodgerblue4", "darkolivegreen4","darkorchid3", "goldenrod1"))+
            theme_classic()+
            theme(axis.text.x = element_text(angle = 90),axis.title = element_text(size = 15), legend.text = element_text(size = 15), legend.title =element_text(size = 15),strip.text.x = element_text(size = 15))
        )
         })

    output$lab <- renderPlotly({

        ggplotly(
          ggplot(lab(), aes(x=year, y=cnt, fill= lab_id))+
            geom_bar(stat = "identity") +
            facet_grid(~source) +
            scale_x_continuous(limits= c(2000,2021)) +
            labs(x= '', y= "count", fill = "lab measure")+
            scale_fill_viridis(discrete = TRUE)+
            theme_classic() +
            scale_size(range=c(5,20))
        )
    })
    output$enrol <- renderPlotly({
            ggplotly(
              ggplot(data_enrol() %>%
                       mutate(year_enrol = lubridate::year(enrol_d)) %>%
                       select (source, year_enrol) %>%
                       group_by(source, year_enrol) %>%
                       mutate(count = n()), aes(x = year_enrol, y= count, color = source)) +
                geom_line(size = 1.5) +
                scale_x_continuous(limits=c(2000,2023), n.breaks = 23) +
                scale_color_viridis(discrete = TRUE, option = "D")+
                labs(color ="Extract", y = "Number of Patients", x = "Year of enrolment")+
                theme_classic() +
                theme(axis.text.x = element_text(angle = 90))
            )
        })
    

    output$art_plot <- renderPlot({
      
      
      

      ggplot(data_art(), aes(area = years, fill =artdefinition, label = paste(artdefinition, round(years), sep = "\n"))) +
        geom_treemap()+
        geom_treemap_text(colour = "white",
                          place = "centre",
                          size = 15) +
        labs(title = "Exposure to ART in Person Years") +
        facet_grid(~source) +
        scale_fill_viridis(discrete = TRUE) +
        theme(legend.position = "none", strip.text.x = element_text(size = 30),plot.title = element_text(hjust = 0.5,size=20))
    })

    output$art_gauge_1 <- renderPlotly({
        plot_ly(
            type = "indicator",
            mode = "gauge+number+delta",
            value = sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_1,"..tblART"))[1,1],
            title = list(text = "Previous number of patients on ART", font = list(size = 15)),
            delta = list(reference = sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_1,"..tblBAS"))[1,1]*0.8, increasing = list(color = "RebeccaPurple")),
            gauge = list(
                axis = list(range = list(NULL, sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_1,"..tblBAS"))[1,1]), tickwidth = 1, tickcolor = "darkblue"),
                bar = list(color = "darkblue"),
                bgcolor = "white",
                borderwidth = 2,
                bordercolor = "gray",
                steps = list(
                    list(range = c(0, sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_1,"..tblBAS"))[1,1]*0.5), color = "red"),
                    list(range = c(sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_1,"..tblBAS"))[1,1]*0.5, sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_1,"..tblBAS"))[1,1]*0.8), color = "orange"),
                    list(range = c(sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_1,"..tblBAS"))[1,1]*0.8, sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_1,"..tblBAS"))[1,1]), color = "green")),
                threshold = list(
                    line = list(color = "cyan", width = 4),
                    thickness = 0.75,
                    value = sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_1,"..tblBAS"))[1,1])))  %>%
            layout(
                margin = list(l=20,r=30),
                paper_bgcolor = "white",
                font = list(color = "darkblue", family = "Arial"))

    })

    output$art_gauge_2 <- renderPlotly({
        plot_ly(
            type = "indicator",
            mode = "gauge+number+delta",
            value = sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_2,"..tblART"))[1,1],
            title = list(text = "Current number of patients on ART", font = list(size = 15)),
            delta = list(reference = sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_2,"..tblBAS"))[1,1]*0.8, increasing = list(color = "RebeccaPurple")),
            gauge = list(
                axis = list(range = list(NULL, sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_2,"..tblBAS"))[1,1]), tickwidth = 1, tickcolor = "darkblue"),
                bar = list(color = "darkblue"),
                bgcolor = "white",
                borderwidth = 2,
                bordercolor = "gray",
                steps = list(
                    list(range = c(0, sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_2,"..tblBAS"))[1,1]*0.5), color = "red"),
                    list(range = c(sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_2,"..tblBAS"))[1,1]*0.5, sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_2,"..tblBAS"))[1,1]*0.8), color = "orange"),
                    list(range = c(sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_2,"..tblBAS"))[1,1]*0.8, sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_2,"..tblBAS"))[1,1]), color = "green")),
                threshold = list(
                    line = list(color = "cyan", width = 4),
                    thickness = 0.75,
                    value = sqlQuery(con,paste0("select count(distinct patient)
from ",input$extract_2,"..tblBAS"))[1,1])))  %>%
            layout(
                margin = list(l=20,r=30),
                paper_bgcolor = "white",
                font = list(color = "darkblue", family = "Arial"))

    })


    output$vl_count_plot <- renderPlotly({
      ggplotly(
        ggplot(data_vl(), aes(x=year, y = total, fill = source)
        ) +
          geom_bar(stat= "identity", position="dodge")+
          #facet_grid(~source) +
          scale_x_continuous(limits=c(2000,2025), n.breaks = 23)+
          labs(x="",y = "Total Viral loads")+
          #scale_fill_discrete(labels= c("Undetectable","Detectable"))+
          scale_fill_viridis(discrete = TRUE) +
          theme_classic()  +
          theme(axis.text.x = element_text(angle = 90))
      )
    })

    output$vl_plot <- renderPlotly({
        ggplotly(
          ggplot(data_vl2() %>%
              mutate(suppressed = factor(as.factor(suppressed), levels = c(0,1), labels= c("Undetectable","Detectable"))), aes(x=year, y = total, fill= factor(suppressed))
        ) +
            geom_bar(stat= "identity")+
            facet_grid(~source) +
            scale_x_continuous(limits=c(2000,2025), n.breaks = 23)+
            labs(fill = "Viral Load", y = "Total Viral loads")+
            #scale_fill_discrete(labels= c("Undetectable","Detectable"))+
            scale_fill_viridis(discrete = TRUE) +
            theme_classic()  +
            theme(axis.text.x = element_text(angle = 90))
        )
    })

    output$cd4_count_plot <- renderPlotly({
      ggplotly(
        ggplot(data_cd4(), aes(x=year, y = total, fill= source))+
          geom_bar(stat= "identity", position = "dodge")+
          #facet_grid(~source) +
          scale_x_continuous(limits=c(2000,2025), n.breaks = 23)+
          labs( x="", y = "Number of CD4 measurements")+
          scale_fill_viridis(discrete = TRUE) +
          theme_classic()  +
          theme(axis.text.x = element_text(angle = 90))
      )
    })

    output$art_count_plot <- renderPlotly({
      ggplotly(
        ggplot(data_art2(), aes(x=year, y = total, fill=source))+
          geom_bar(stat= "identity", position ="dodge")+
         # facet_grid(~source) +
          scale_x_continuous(limits=c(2000,2025), n.breaks = 23)+
          labs( x="", y = "Number of unique ART episodes")+
          scale_fill_viridis(discrete = TRUE) +
          theme_classic()  +
          theme(axis.text.x = element_text(angle = 90))
      )
    })

    output$cd4_plot <- renderPlotly({
        ggplotly(
          ggplot(data_cd42() %>%
          mutate(range= factor(as.factor(range), levels = c(0,1,2,3), labels = c("Under 200 cells/mm3","200-500 cells/mm3","500-1,600 cells/mm3","Over 1,600 cells/mm3"))), aes(x=year, y = total, fill= range))+
            geom_area(stat= "identity")+
            facet_grid(~source) +
            scale_x_continuous(limits=c(2000,2025), n.breaks = 23)+
            labs(fill = "CD4 Count",x="", y = "Number of patients")+
           scale_fill_viridis(discrete = TRUE) +
           # scale_fill_discrete(labels= c("Under 200 cells/mm3","200-500 cells/mm3","500-1,600 cells/mm3","Over 1,600 cells/mm3"))+
            theme_classic()  +
            theme(axis.text.x = element_text(angle = 90))
        )
    })

    output$death_plot <- renderPlotly({
      ggplotly(
      ggplot(data_death(), aes(x=year, y=deaths, color = source))+
        geom_line() +
        labs(y = "Number of deaths", x = "", color = "Extract") +
        scale_x_continuous(limits=c(2000,2025), n.breaks = 23) +
        scale_color_viridis(discrete = TRUE, option = "D")+
        theme_classic() +
        theme(axis.text.x = element_text(angle = 90))
      )
    })

    output$rev_plot <- renderPlotly({
      ggplotly(
        ggplot(data_rev(), aes(x=year, y=deaths, color = source))+
          geom_line() +
          labs(y = "Number of NPR linked deaths", x = "", color = "Extract") +
          scale_x_continuous(limits=c(2000,2025), n.breaks = 23) +
          scale_color_viridis(discrete = TRUE, option = "D")+
          theme_classic() +
          theme(axis.text.x = element_text(angle = 90))
      )
    })

    output$drop_plot <- renderPlotly({
      ggplotly(
        ggplot(data_drop(), aes(x=year, y=deaths, color = source))+
          geom_line() +
          labs(y = "Number of patients who dropped out", x = "", color = "Extract") +
          scale_x_continuous(limits=c(2000,2025), n.breaks = 23) +
          scale_color_viridis(discrete = TRUE, option = "D")+
          theme_classic() +
          theme(axis.text.x = element_text(angle = 90))
      )
    })


    output$enr_plot <- renderPlotly({
      ggplotly(
        ggplot(
      data_enr3() %>%
      mutate(order = factor(as.factor(order), labels= c("tblLTFU", "tblVIS", "tblART","tblLAB_CD4","tblLAB_RNA","tblLAB","tblMED","tblDIS"), levels = c(1,2,3,4,5,6,7,8))), aes(x=perc, y=order, fill = order))+
        geom_bar(stat ="identity") +
        labs(y = "IeDEA table", x = "Percentage of patients contained from tblBAS") +
        facet_grid(~source)+
        scale_fill_viridis(discrete = TRUE) +
        theme_classic() +
        theme(legend.position="none")
      )
    })

     output$bas_plot <- renderPlotly({
       ggplotly(
         ggplot(data_bas()
                , aes(x=perc, y=table, fill = table))+
           geom_bar(stat ="identity") +
           labs(y = "tblBAS variables", x = "Percentage of complete records") +
           facet_grid(~source)+
           scale_fill_viridis(discrete = TRUE)+
           theme_classic() +
           theme(legend.position="none")
       )
     })

     output$ltfu_plot <- renderPlotly({
       ggplotly(
         ggplot(data_ltfu()
                , aes(x=perc, y=table, fill = table))+
           geom_bar(stat ="identity") +
           labs(y = "tblLTFU variables", x = "Percentage of complete records") +
           scale_fill_viridis(discrete = TRUE)+
           facet_grid(~source)+
           theme_classic() +
           theme(legend.position="none")
       )

     })

     output$npr_fac_plot <- renderPlotly({
      # validate(
       #  need(is.data.frame(sqlQuery(con,paste0("select count(*) from",input$extract_1,"..tblREV_VITAL_STATUS union select count(*) from",input$extract_2,"..tblREV_VITAL_STATUS")))=TRUE, "No plot available for chosen filter criteria!")
       #)
       
       ggplotly(
         ggplot(data_npr(), aes(x=year, y =deaths, color = source)) +
           geom_line() +
           labs(x="Year of death", y = "cases", title = "Deaths from different sources") +
           facet_grid(~extract) +
           scale_color_viridis(discrete = TRUE, option = "D")+
           theme_classic()
       )
     })

     output$weigh_plot <- renderPlotly({
       ggplotly(
         ggplot(data_weigh(), aes(x=weigh))+
           geom_histogram()+
           scale_x_continuous(limits= c(0,200)) +
           labs(x = "Weight (kgs)")+
           facet_grid(~source) +
           theme_classic()
         )

     })


     output$heigh_plot <- renderPlotly({
       ggplotly(
         ggplot(data_heigh(), aes(x=heigh))+
           geom_histogram()+
           scale_x_continuous(limits= c(0.1,3.0)) +
           labs(x="Height (metres)")+
           facet_grid(~source) +
           theme_classic()
       )

     })

     output$dis_plot <- renderPlotly({
       ggplotly(
         ggplot(data_dis(), aes(x=dis_year, fill = dis_id))+
           geom_bar() +
           labs(x="Disease diagnosis year", y = "cases") +
           scale_fill_viridis(discrete = TRUE) +
           scale_x_continuous(limits=c(2000,2023), n.breaks = 23) +
           theme_classic()+
           theme(axis.text.x = element_text(angle = 90)) +
           facet_grid(~source)
       )
     })



     output$vis_plot <- renderPlotly({
       ggplotly(
         ggplot(data_vis(), aes(x=perc, y=table, fill = table))+
           geom_bar(stat ="identity") +
           labs(y = "tblVIS variables", x = "Percentage of complete records") +
           scale_fill_viridis(discrete = TRUE) +
           facet_grid(~source)+
           theme_classic() +
           theme(legend.position="none")
       )

     })

    t1 <-  reactive ({

      data_t1() %>%
        mutate(Percentage_change = round((Current-Previous)/Previous*100))

    })


    output$table1 <- renderDataTable({
        datatable(t1(), options = list("pagelength"=40))
    })

    output$download <- downloadHandler(
        filename = function(){"table_1.xlsx"},
        content = function(fname){
            write_xlsx(t1(), fname)})

    output$plot <- renderPlot({
        input$go
        Sys.sleep(1.5)
        plot(runif(10))
    })

}
# Run the application
shinyApp(ui = ui, server = server)
