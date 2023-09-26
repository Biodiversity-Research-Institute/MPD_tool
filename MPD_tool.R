#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# A. Gilbert
# Biodiversity Research Institute
# 276 Canco Rd
# Portland, ME 04103
# The Tool houses a collection of MMPs to serve as a resource to the E-TWG and F-TWG. The Tool is searchable by various categories, including, but not limited to:
#   
#   1) Marine resources - birds/bats, marine mammals/sea turtles, fish, benthos, and fisheries; 
#   2) Stressors; 
#   3) Potential effects; and
#   4) Development phases of offshore wind.
# This tool was modified from one originally developed by ECOLOGY AND ENVIRONMENT, INC. and BRI for NYSERDA
# l
# created: 16 Feb 2022
# Modified 06 April 2022


library(shiny)
library(shinyBS)
library(shinyTree)
library(shinyWidgets)
library(shinybusy)
library(shinyjs)
library(shinydashboard)
library(readxl)
library(dplyr)
library(DT)
library(stringr)

unique_no_na <- function(x) unique(x[!is.na(x)])
MPD_version_num = "0.10"
MPD_version_name = "Primary Artemis"

options(shiny.trace = F)


##################################################################################################################################################
##                                                                                                                                              ##
##                                                              DASHBOARD HEADER                                                                ##
##                                                                                                                                              ##
##################################################################################################################################################

# Define UI
ui <- navbarPage(title = "MPD Tool", 
                 id =  "MPDTool",
                 header = list(),
                 
  tabPanel("Start Here", 
           fluidRow(
             column(width = 12, 
                    # img(src = "https://www.nyftwg.com/wp-content/uploads/2020/04/offshore-windfarm-1.jpg", height = "100%"),
                    h3(HTML(paste("Mitigation Practices Discovery Tool (MPD Tool): v", MPD_version_num, "-",MPD_version_name)), style = "padding-left: 10px; margin-bottom: 10px; color: CornflowerBlue;"),
                    a("Quick Start Guide", 
                      href = 'MMP Tool Quick Start Guide 3-24-20.pdf',
                      style = "padding-left: 30px; padding-right: 50px; font-weight: 600; font-size: 18px;", 
                      target="_blank"),
                    a("User Manual", 
                      href = 'MMP Tool User Manual 3-24-20.pdf',
                      style = "padding-left: 0px; padding-right: 50px; font-weight: 600; font-size: 18px;", 
                      target="_blank"),
                    a("Tool Glossary", 
                      href = 'MMP Tool Glossary 3-24-20.pdf',
                      style = "padding-left: 0px; padding-right: 50px; font-weight: 600; font-size: 18px;", 
                      target="_blank"),
                    a("Full Database Download", 
                      href = "Full-MMP-and-Reference-List-8-24-2023.zip",
                      style = "padding-left: 0px; padding-right: 20px; font-weight: 600; font-size: 18px;"),
                    h5(HTML(paste("New York State Energy Research and Development Authority (NYSERDA) has developed a", 
                                  "Mitigation Practices Discovery (MPD) Tool that is publicly available  for use",
                                  "by environmental and fisheries stakeholders. It houses a searchable database of mitigation practices",
                                  "extracted from agency reports, environmental assessments, scientific literature,",
                                  "technical guidance documents, and other sources.",
                                  "<br>",
                                  "<br>",
                                  "<ul>",
                                  "<li>The tools is searchable by various categories. including but not limited to:</li>",
                                  "<li>Resource Groups - birds/bats, marine mammals/sea turtles, fish, benthos, fisheries</li>",
                                  "<li>Potential effect; and</li>",
                                  "<li>Development phases of offshore wind</li>",
                                  "</ul>",
                                  "<br>",
                                  "As part of the effort to support development and evaluation of mitigation practices, the Tool provides details about these practices ",
                                  "that could support further evaluation of how best to incorporate mitigation practices into the state’s plans for offshore wind ",
                                  "energy development. This Tool does not prioritize or judge the value of individual or combined mitigation practices, and it does not consider site-",
                                  "and project-specific conditions that might affect how and whether certain mitigation practices may be practicably implemented. ",
                                  "It does, however, provide several sorting criteria that may be useful to the E-TWG and F-TWG and other users when ",
                                  "assessing potential mitigation practices."
                    )), 
                    style = "padding-top:20px; padding-left: 20px; padding-right: 60px; margin-top: 10px; margin-bottom: 10px; color: black; font-weight: bold; white-space: normal;")),
             ),
           fluidRow(
             column(12, 
                    hr()
             )),
           fluidRow(
             column(width = 2),
             column(width = 4,
                    shinyTree("general_mmps_tree", checkbox = TRUE),
                    bsTooltip("general_mmps_tree",
                              "Categories or types of specific mitigation practices gathered in the spreadsheets. Generalized mitigation practices needed to be general enough that multiple specific mitigation practices would aggregate into a generalized mitigation practice category. Generalized mitigation practices have also been designed to be mutually exclusive.",
                              "left",
                              options = list(container = "body")),
                    
                    shinyTree("resources_tree", checkbox = TRUE),
                    bsTooltip("resources_tree",
                              "Mitigation practices are focused on minimizing and avoiding potential impacts of offshore wind energy development on the following resources",
                              "left",
                              options = list(container = "body")),
                    
                    shinyTree("stressors_tree", checkbox = TRUE),
                    bsTooltip("stressors_tree",
                              "External stimuli that can cause changes to the behavioral, physical, chemical, and/or biological characteristics of an organism, species, or the ecosystem inhabited by the organism/species.",
                              "left",
                              options = list(container = "body")),
                    
                    shinyTree("potential_effects_tree", checkbox = TRUE),
                    bsTooltip("potential_effects_tree",
                              "The changes to the behavioral, physical, chemical, and/or bio- logical characteristics of an organism, species, or the ecosystem inhabited by the organism/species due to stressors related to offshore wind energy development.",
                              "left",
                              options = list(container = "body")),
                    
                    shinyTree("dev_phase_tree", checkbox = TRUE),
                    bsTooltip("dev_phase_tree",
                              "The stages of offshore wind facility development/operation, each of which encompass a number of activities and, as a result, may have different types of stressors.",
                              "right",
                              options = list(container = "body")),
                    shinyTree("industries_tree", checkbox = TRUE),
                    bsTooltip("industries_tree",
                              "The type of industry for which mitigation practices have been suggested or implemented in the U.S. or other countries.",
                              "right",
                              options = list(container = "body")),
                    
                    shinyTree("implem_status_tree", checkbox = TRUE),
                    bsTooltip("implem_status_tree",
                              "The implementation status defines the degree to which the use or efficacy of a mitigation practice has been tested.",
                              "right",
                              options = list(container = "body")),
                    
                    shinyTree("mitigation_hierarchy_tree", checkbox = TRUE),
                    bsTooltip("mitigation_hierarchy_tree",
                              "The most applicable level(s) of the mitigation hierarchy was(were) chosen for each mitigation practice.",
                              "right",
                              options = list(container = "body"))   
                    
             ),
             column(width = 6,

                    shinyTree("subgroup_tree", checkbox = TRUE),
                    bsTooltip("subgroup_tree",
                              "Resource subgroups.",
                              "right",
                              options = list(container = "body")),

          
                    )
           ),
           fluidRow(
             br(), br(), br(),br(),
             
             actionButton(
               inputId = "filter_btn",
               "MPD Query",
               width = "200px",
               style = "width: 100px; background-color: cornflowerblue; color: white; font-weight: bold;", 
               class = "btn-filter"
             ), 
             style = "display: flex; align-items: center; justify-content: center;"
           ),

           fluidRow(
             p("This tool was developed by Biodiversity Research Institute and NYSERDA.", 
               style = "padding: 20px; color: steelblue; font-size: 14px")
           )
  ), #tabpanel

  tabPanel("Mitigation data",
           # mititigation data table for display
           downloadButton("download_btn", 
                          HTML("Download</br>Filtered Data"), 
                          style = "width: 130px; padding-left: 40 px; background-color: Goldenrod; color: white; font-weight: bold;"),
           br(), br(),
           DT::dataTableOutput("data_table")),

  tabPanel("References",
           # References for display
           downloadButton("download_refs_btn", 
                          HTML("Download</br>Filtered Refs"), 
                          style = "width: 130px; padding-left: 40 px; background-color: maroon; color: white; font-weight: bold;"),
           br(), br(),
           DT::dataTableOutput("refs")),

  tabPanel("Glossary",
           # Glossary for display
           selectInput(inputId = "glossary_grp", "Group:", choices = c("Stressors", "Potential Effects", "Development Phases",
                                                                      "Industry", "Implementation Status",
                                                                      "Mitigation Hierarchy", "Generalized mitigation practices")),
           htmlOutput("grp_def"),
           htmlOutput("glossary")
  ),

  #add the links and logos
  tabPanel(title="", icon=
  tags$li(
    class = "dropdown",
    a(
      icon('github', "fa-2x"),
      href = 'https://github.com/Biodiversity-Research-Institute/MMP-tool',
      style = "margin-top: -33px; padding-bottom: 0px; margin-left: 40px; padding-right: 0px",
      target = '_blank',
      id = "lbl_codeLink"),
    style = "float: left" 
  )),
  
  tabPanel(title="", icon=  
    tags$li(
    class = "dropdown",
    a(
      icon('bug', "fa-2x"),
      href = 'https://github.com/Biodiversity-Research-Institute/MMP-tool/issues',
      #exclamation-circle
      style = "margin-top: -33px; padding-bottom: 0px; margin-left: -15px; padding-right: 0px",
      target = '_blank',
      id = "lbl_issuesLink"),
    style = "float: left"
  )),
  
  tabPanel(title="", icon=  
    tags$li(
    class = "dropdown",
    a(
      img(src = "BRI_color_logo_no_words.png", height = "40px"),
      href = 'https://briwildlife.org',
      style = "margin-top: -40px; padding-bottom: 0px; margin-left: -25px;, padding-right: 0px;",
      target = '_blank',
      id = "lbl_BRILogoLink"),
    style = "float: left"
  )),
  
  tabPanel(title="", icon=  
    tags$li(
    class = "dropdown",
    a(
      img(src = "NYSERDA Logo.jpeg", height = "40px"),
      href = 'https://www.nyserda.ny.gov/',
      style = "margin-top: -40px; padding-bottom: 0px; margin-left: -40px;, padding-right: 0px;",
      target = '_blank',
      id = "lbl_URILogoLink"),
    style = "float: left"
  ))
  ) #navbarPage

verbose <- F

source("mpd_glossary.R")
source("mpd_tree_choices.R")

##################################################################################################################################################
##                                                                                                                                              ##
##                                                              Define server logic                                                             ##
##                                                                                                                                              ##
##################################################################################################################################################

server <- function(input, output, session) {
  
  
  # MMP Tool resources tab ----------------------------------------------------------
  
  #tree choices
  output$general_mmps_tree <- renderTree(general_mmps_choices)
  output$resources_tree <- renderTree(resources_choices)
  output$stressors_tree <- renderTree(stressors_choices)
  output$potential_effects_tree <- renderTree(potential_effects_choices)
  output$dev_phase_tree <- renderTree(dev_phase_choices)
  output$industries_tree <- renderTree(industries_choices)
  output$subgroup_tree <- renderTree(subgroup_choices)
  output$implem_status_tree <- renderTree(implem_status_choices)
  output$mitigation_hierarchy_tree <- renderTree(mitigation_hierarchy_choices)
  
  
  # MMP Tool glossary tab ----------------------------------------------------------
  
  output$grp_def <- eventReactive(input$glossary_grp, {
    grp <- input$glossary_grp
    preamb_out <- case_when(
      grp == "Stressors" ~ stressor_preamb,
      grp == "Potential Effects" ~ pot_effects_preamb,
      grp == "Development Phases" ~ dev_phase_preamb,
      grp == "Industry" ~ industry_preamb,
      grp == "Implementation Status" ~ implement_status_preamb,
      grp == "Mitigation Hierarchy" ~ mit_heir_preamb,
      grp == "Generalized mitigation practices" ~ gen_mitpractices_preamb
    )
    # renderText(preamb_out)
    preamb_out
  })
  
  output$glossary <- eventReactive(input$glossary_grp,{
    grp <- input$glossary_grp
    def_out <- case_when(
      grp == "Stressors" ~ stressor_defs,
      grp == "Potential Effects" ~ pot_effects_defs,
      grp == "Development Phases" ~ dev_phase_defs,
      grp == "Industry" ~ industry_defs,
      grp == "Implementation Status" ~ implement_status_defs,
      grp == "Mitigation Hierarchy" ~ mit_heir_defs,
      grp == "Generalized mitigation practices" ~ gen_mitpractices_defs
    )
    def_out
    # renderText(def_out)
  })
  
  # output$downloads <- renderText("<</h4>")
  
  # Read in Excel file
  mmp_data <- read_excel("Full-MMP-and-Reference-List-8-24-2023.xlsx", sheet = "MMP Data")
  #remove NULLS, convert "NA" to NA
  mmp_data[mmp_data=="NULL"] <-  NA
  mmp_data[mmp_data=="NA"] <-  NA
  
  # Render data table
  output$data_table <- DT::renderDT(datatable(filtered_data()[,c(2:16)],
                                              selection = 'none', rownames = F, class = "display nowrap",
                                              #add a download button to the table. https://rstudio.github.io/DT/extensions.html
                                              # extensions = 'Buttons', 
                                              options = list(
                                                pageLength = 15, 
                                                autoWidth = T,
                                                paging = TRUE,
                                                searching = TRUE,
                                                scrollX = TRUE
                                                # dom = 'Bfrtip',
                                                # buttons = list(list(extend = 'csv', filename = paste0('MPD_tool_v', str_replace_all(MPD_version_num, "\\.", "_"), "_filtered_", strftime(Sys.time(), "%Y%m%d_%H%M%S"))), 
                                                #                list(extend = 'excel', filename = paste0('MPD_tool_v', str_replace_all(MPD_version_num, "\\.", "_"), "_filtered_", strftime(Sys.time(), "%Y%m%d_%H%M%S"))))
                                                # columnDefs = list(
                                                #   list(
                                                #     targets = c(0:8),
                                                #     render = JS(
                                                #       "function(data, type, row, meta) {",
                                                #       "return type === 'display' && data.length > 10 ?",
                                                #       "'<span title=\"' + data + '\">' + data.substr(0, 10) + '...</span>' : data;",
                                                #       "}")
                                                #     ))
                                                )
  )
  )
  
  #Load the reference data
  mmp_ref_data <- read_excel("Full-MMP-and-Reference-List-8-24-2023.xlsx", sheet = "cross-check")
  
  #render filtered refs
  output$refs <- DT::renderDT(datatable(filtered_refs()[,c(2:4)],
                                        selection = 'none', rownames = F, 
                                        # extensions = 'Buttons', 
                                        options = list(
                                          pageLength = 8, 
                                          paging = TRUE,
                                          searching = TRUE,
                                          autoWidth = T,
                                          scrollX = TRUE
                                          # dom = 'Bfrtip',
                                          # buttons = list(list(extend = 'csv', filename = paste0('MPD_tool_v', str_replace_all(MPD_version_num, "\\.", "_"), "_refs_", strftime(Sys.time(), "%Y%m%d_%H%M%S"))), 
                                          #                list(extend = 'excel', filename = paste0('MPD_tool_v', str_replace_all(MPD_version_num, "\\.", "_"), "_refs_", strftime(Sys.time(), "%Y%m%d_%H%M%S"))))
                                          )))
  
  # #select all data if unselected
  # observeEvent(input$all_data, {
  #   select_all_data()
  #   
  # })
  # 
  # #select all data if unselected
  # observeEvent(input$clear_data, {
  #   clear_all_data()
  #   
  # })
  
  selection_tree_1level <- function(tree_selection) {
    sapply(tree_selection, function(level) {
      names(level[[1]])
    })
  }
  
  selection_tree_multilevel <- function(tree_selection) {
    sapply(tree_selection, function(level) {
      value = ""
      n_levels <- purrr::vec_depth(level[[1]])
      if (n_levels == 2){
        value = paste0("(", names(level[[1]]), ")")}
      if (n_levels == 3){
        value = paste0("(", names(level[[1]][[1]]), ")")}
      if (n_levels == 4){
        value = paste0("(", names(level[[1]][[1]][[1]]), ")")}
      return(value)
    })
  }
  
  # Filter data based on selected variable and filter
  filtered_data <- eventReactive(input$filter_btn, {
    
    general_mmps_selected <- selection_tree_multilevel(get_selected(input$general_mmps_tree, format = "slices"))
    resources_selected <- selection_tree_multilevel(get_selected(input$resources_tree, format = "slices"))
    stressors_selected <- selection_tree_multilevel(get_selected(input$stressors_tree, format = "slices"))
    potential_effects_selected <- selection_tree_multilevel(get_selected(input$potential_effects_tree, format = "slices"))
    dev_phase_selected <- selection_tree_multilevel(get_selected(input$dev_phase_tree, format = "slices"))
    industries_selected <- selection_tree_multilevel(get_selected(input$industries_tree, format = "slices"))
    # browser()
    subgroup_selected <- selection_tree_multilevel(get_selected(input$subgroup_tree, format = "slices"))
    implem_status_selected <- selection_tree_multilevel(get_selected(input$implem_status_tree, format = "slices"))
    mitigation_hierarchy_selected <- selection_tree_multilevel(get_selected(input$mitigation_hierarchy_tree, format = "slices"))
    
    data_out <- mmp_data %>%
      #multiple options to filter out in input and also in columns
      #need to add word boundaries
      filter(grepl(paste0(general_mmps_selected, collapse="|"), GeneralizedMMP) &
               grepl(paste0(resources_selected, collapse="|"), Resources) &
               grepl(paste0(stressors_selected, collapse="|"), Stressors) &
               grepl(paste0(potential_effects_selected, collapse="|"), PotentialEffects) &
               grepl(paste0(dev_phase_selected, collapse="|"), DevelopmentPhases) &
               grepl(paste0(industries_selected, collapse="|"), Industry) &
               grepl(paste0(subgroup_selected, collapse="|"), SubGroup) &
               grepl(paste0(implem_status_selected, collapse="|"), ImplementationStatus) &
               grepl(paste0(mitigation_hierarchy_selected, collapse="|"), MitigationHierarchy)
      )
    # browser()

    return(data_out)
  })
  
  #filter references too based on mmp data
  filtered_refs <- eventReactive(filtered_data(), {
    mmp_ref_data %>%
      filter(MMPID %in% filtered_data()$MMPID)
  })

  #navigate to "MMP data table" tab once filter selections made
  observeEvent(filtered_data(), 
               updateTabItems(session, inputId = "MPDTool", selected = "Mitigation data")
  )
  
  # Download filtered data as CSV file
  output$download_btn <- downloadHandler(
    filename = function() {
      paste0('MPD_tool_v', str_replace_all(MPD_version_num, "\\.", "_"), "_filtered_data_", strftime(Sys.time(), "%Y%m%d_%H%M%S"),'.csv')
    },
    content = function(file) {
      #combine data with full refs
      # data_export <- dplyr::left_join(filtered_data(), filtered_refs())
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Download filtered data as CSV file
  output$download_refs_btn <- downloadHandler(
    filename = function() {
      paste0('MPD_tool_v', str_replace_all(MPD_version_num, "\\.", "_"), "_filtered_refs_", strftime(Sys.time(), "%Y%m%d_%H%M%S"),'.csv')
    },
    content = function(file) {
      #combine data with full refs
      write.csv(filtered_refs(), file, row.names = FALSE)
    }
  )
  
} #server

# Run app
shinyApp(ui, server)


