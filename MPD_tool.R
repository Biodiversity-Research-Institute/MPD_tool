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
# 13 Jun 24 - fixed where all references not being downloaded.


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
MPD_version_num = "0.14.1"
MPD_version_name = "Primary Artemis"
MPD_db_date = "30 November 2023"

options(shiny.trace = F)


##################################################################################################################################################
##                                                                                                                                              ##
##                                                              DASHBOARD HEADER                                                                ##
##                                                                                                                                              ##
##################################################################################################################################################

# Define UI
ui <- navbarPage(title = paste("MPD version: ", MPD_version_num, "-",MPD_version_name), 
                 id =  "MPDTool",
                 header = list(),

  tabPanel("Instructions", 
           fluidRow(
             column(width = 12, 
                    # img(src = "https://www.nyftwg.com/wp-content/uploads/2020/04/offshore-windfarm-1.jpg", height = "100%"),
                    h2("Mitigation Practices Database (MPD) Tool for Offshore Wind", style = "padding-left: 10px; margin-bottom: 10px; color: CornflowerBlue;"),
                    # p(paste("Version: ", MPD_version_num, "-",MPD_version_name), style = "padding-left: 10px; margin-bottom: 10px;"),

                    h3("Background", style = "padding-left: 10px; margin-bottom: 10px; color: CornflowerBlue;"),
                    p(HTML(paste("New York State Energy Research and Development Authority (NYSERDA) has developed a ",
                                  "Mitigation Practices Database (MPD) Tool for Offshore Wind (previously the Mitigation and ",
                                  "Monitoring or MMP Tool) that is publicly available for use by environmental and fisheries ", 
                                  "stakeholders. It houses a searchable database of potential mitigation practices that may be relevant ",
                                  "to avoiding, minimizing, offsetting and restoring potential effects of offshore wind energy ",
                                  "development on wildlife, the environment, and fisheries.",
                                  "<br>",
                                  "<br>",
                                  "The Tool is searchable by various categories, including species group, type of effect, offshore wind ",
                                  "development phase, and implementation status (e.g., whether the mitigation measure has been ",
                                  "tested or shown to be effective). This Tool does not prioritize or assess the value of individual or ",
                                  "combined mitigation practices, and it does not consider site- and project-specific conditions that ",
                                  "might affect how and whether certain mitigation practices may be practicably implemented.")),
                    style = "padding-top:20px; padding-left: 20px; padding-right: 60px; margin-top: 10px; margin-bottom: 10px; color: black; white-space: normal; font-size: 16px;"),
                   h3("Resources", style = "padding-left: 10px; margin-bottom: 10px; color: CornflowerBlue;"),
                   a("User Manual", 
                     href = 'MPD Updated user manual 2023_final.pdf',
                     style = "padding-left: 30px; padding-right: 50px; font-weight: 600; font-size: 18px;", 
                     target="_blank"),
                   a("Full Database Download", 
                     href = "Full-MMP-and-Reference-List-10-13-2023.zip",
                     style = "padding-left: 0px; padding-right: 20px; font-weight: 600; font-size: 18px;"),
                                        
                    h3("Instructions", style = "padding-left: 10px; margin-bottom: 10px; color: CornflowerBlue;"),
                    p(HTML(paste(
                      "<ul>",
                      "<li>Click on the <strong>\"Filter Database\"</strong> tab in the header.</li>",
                      "<li>Select the filter categories you want to display (by default, all categories are off).",
                      "Categories can be expanded to view subgroups using the arrow buttons to the left of each category name.</li>",
                      "<li>Hover over filters for term definitions or go to the <strong>\"Glossary\"</strong> tab in the header. ",
                      "Term definitions are also included in the user manual (link above).</li>",
                      "<li>Once your selections are made, click the <strong>\"Click to Filter Data\"</strong> query button to filter the database.</li>",
                      "<li>The resulting database entries will show up in a table to the right in the same tab. ",
                      "For fields that are too long to view on screen, hover over them with your mouse to see the full entry. ",
                      "It is recommended that you download the filtered data to explore it in detail.</li>",
                      "<li>To see the full database, you can either click the query button with all filters applied or ",
                      "download the full database at the link above (<strong>\"Full Database Download\"</strong>).</li>",
                      "<li>Download the filtered data by clicking the <strong>\"CSV\"</strong> or <strong>\"Excel\"</strong> buttons above the table.</li>",
                      "<li>References can be displayed for the filtered database selection in the <strong>\"Filtered References\"</strong> tab. ",
                      "Like the main table, filtered references can be downloaded by clicking the <strong>\"CSV\"</strong> or <strong>\"Excel\"</strong> buttons above the table.</li>",
                      "</ul>"
                      )),
                       style = "padding-top:20px; padding-left: 20px; padding-right: 60px; margin-top: 10px; margin-bottom: 10px; color: black; white-space: normal;font-size: 16px;")
                      ),
             fluidRow(
               column(12, 
                      hr()
               )),
             
             fluidRow(
               p(HTML("This tool was developed by Biodiversity Research Institute with support from NYSERDA. <br> An earlier version of this tool was developed by Environment and Ecology Inc. and Biodiversity Research Institute."), 
                 style = "padding-left: 30px; margin-top: -20px; color: steelblue; font-size: 14px")
             )
             ),
           
  ), #tabpanel

  tabPanel("Filter Database",
           
           fluidRow(
             column(
               width = 3,
               style='margin-bottom:20px; border-right:1px solid; padding: 5px;',

               fluidRow(
                 column(8, 
                 h5("Click the plus sign to select a category. If you want to select specific subcategories, press the down arrow to the left of the category checkbox to see the list of subcategories.",
                    style = "margin-left: 10px; margin-top: 0px; padding-right: 0px;")),
                 column(4, 
                        actionButton(
                          inputId = "filter_btn",
                          HTML("Click to</br>Filter Data"),
                          width = "200px",
                          style = "width: 100px; background-color: cornflowerblue; color: white; font-weight: bold; margin-left: -10px; padding-right:10px;",
                          class = "btn-filter"),
                        style = "padding-right: 10px;"
                        ),
                 
                 style = "margin-bottom: 0px;"
                 # style = "display: flex; align-items: center; justify-content: center;"
               ),

               h3("Filter categories", style = "margin-left: 20px; margin-top: 0px;"),
               
               shinyTree("subgroup_tree", checkbox = TRUE),
               bsTooltip(
                 "subgroup_tree",
                 "Resource subgroups.",
                 "right",
                 options = list(container = "body")
               ),
               
               hr(style = "margin-top: -1px; margin-bottom: -1px; height: 1px; background-color: #2980B9;"),
               
               shinyTree("general_mmps_tree", checkbox = TRUE),
               bsTooltip(
                 "general_mmps_tree",
                 "Categories or types of mitigation practices identified during literature review.",
                 "right",
                 options = list(container = "body")
               ),
               
               hr(style = "margin-top: -1px; margin-bottom: -1px; height: 1px; background-color: #2980B9;"),

               shinyTree("stressors_tree", checkbox = TRUE),
               bsTooltip(
                 "stressors_tree",
                 "External stimuli that can cause changes to the behavioral, physical, chemical, and/or biological characteristics of an organism, species, or the ecosystem inhabited by the organism/species.",
                 "right",
                 options = list(container = "body")
               ),
               
               hr(style = "margin-top: -1px; margin-bottom: -1px; height: 1px; background-color: #2980B9;"),
               
               shinyTree("potential_effects_tree", checkbox = TRUE),
               bsTooltip(
                 "potential_effects_tree",
                 "The changes to the behavioral, physical, chemical, and/or bio- logical characteristics of an organism, species, or the ecosystem inhabited by the organism/species due to stressors related to offshore wind energy development.",
                 "right",
                 options = list(container = "body")
               ),
               
               hr(style = "margin-top: -1px; margin-bottom: -1px; height: 1px; background-color: #2980B9;"),
               
               shinyTree("dev_phase_tree", checkbox = TRUE),
               bsTooltip(
                 "dev_phase_tree",
                 "The stages of offshore wind facility development/operation, each of which encompass a number of activities and, as a result, may have different types of stressors.",
                 "right",
                 options = list(container = "body")
               ),
               
               hr(style = "margin-top: -1px; margin-bottom: -1px; height: 1px; background-color: #2980B9;"),
               
               shinyTree("industries_tree", checkbox = TRUE),
               bsTooltip(
                 "industries_tree",
                 "The type of industry for which mitigation practices have been suggested or implemented in the U.S. or other countries.",
                 "right",
                 options = list(container = "body")
               ),
               
               hr(style = "margin-top: -1px; margin-bottom: -1px; height: 1px; background-color: #2980B9;"),
               
               shinyTree("implem_status_tree", checkbox = TRUE),
               bsTooltip(
                 "implem_status_tree",
                 "The implementation status defines the degree to which the use or efficacy of a mitigation practice has been tested.",
                 "right",
                 options = list(container = "body")
               ),
               
               hr(style = "margin-top: -1px; margin-bottom: -1px; height: 1px; background-color: #2980B9;"),
               
               shinyTree("mitigation_hierarchy_tree", checkbox = TRUE),
               bsTooltip(
                 "mitigation_hierarchy_tree",
                 "The most applicable level(s) of the mitigation hierarchy was(were) chosen for each mitigation practice.",
                 "right",
                 options = list(container = "body")
               ),
             ), 
               
             column(width = 9,
                    # mitigation data table for display
                    DT::dataTableOutput("data_table"))
           ),
           fluidRow(
             hr(),
             p(paste("Date database last updated:", MPD_db_date), style = "margin-left: 20px;")
             )
           ),

  tabPanel("Filtered References",
           fluidRow(
             column(width = 12, align = "center",
              DT::dataTableOutput("refs"))),
           fluidRow(
             hr(),
             p(paste("Date database last updated:", MPD_db_date), style = "margin-left: 20px;")
           )
           ),

  tabPanel("Glossary",
           # Glossary for display
           selectInput(inputId = "glossary_grp", "Group:", choices = c("Stressors", "Potential Effects", "Development Phases",
                                                                      "Industry Origin", "Implementation Status",
                                                                      "Mitigation Hierarchy", "Mitigation Type")),
           htmlOutput("grp_def"),
           htmlOutput("glossary")
  ),

  #add the links and logos
  tabPanel(title="", icon=
  tags$li(
    class = "dropdown",
    a(
      icon('github', "fa-2x"),
      href = 'https://github.com/Biodiversity-Research-Institute/MPD_tool',
      style = "margin-top: -33px; padding-bottom: 0px; margin-left: 40px; padding-right: 4px",
      target = '_blank',
      id = "lbl_codeLink"),
    style = "float: left" 
  )),
  
  tabPanel(title="", icon=  
    tags$li(
    class = "dropdown",
    a(
      icon('bug', "fa-2x"),
      href = 'https://github.com/Biodiversity-Research-Institute/MPD_tool/issues',
      #exclamation-circle
      style = "margin-top: -33px; padding-bottom: 0px; margin-left: -15px; padding-right: 4px",
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
      style = "margin-top: -40px; padding-bottom: 0px; margin-left: -25px;, padding-right: 4px;",
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
  # output$resources_tree <- renderTree(resources_choices)
  output$subgroup_tree <- renderTree(subgroup_choices)
  output$stressors_tree <- renderTree(stressors_choices)
  output$potential_effects_tree <- renderTree(potential_effects_choices)
  output$dev_phase_tree <- renderTree(dev_phase_choices)
  output$industries_tree <- renderTree(industries_choices)
  output$implem_status_tree <- renderTree(implem_status_choices)
  output$mitigation_hierarchy_tree <- renderTree(mitigation_hierarchy_choices)
  
  
  # MMP Tool glossary tab ----------------------------------------------------------
  
  output$grp_def <- eventReactive(input$glossary_grp, {
    grp <- input$glossary_grp
    preamb_out <- case_when(
      grp == "Stressors" ~ stressor_preamb,
      grp == "Potential Effects" ~ pot_effects_preamb,
      grp == "Development Phases" ~ dev_phase_preamb,
      grp == "Industry Origin" ~ industry_preamb,
      grp == "Implementation Status" ~ implement_status_preamb,
      grp == "Mitigation Hierarchy" ~ mit_heir_preamb,
      grp == "Mitigation Type" ~ gen_mitpractices_preamb
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
      grp == "Industry Origin" ~ industry_defs,
      grp == "Implementation Status" ~ implement_status_defs,
      grp == "Mitigation Hierarchy" ~ mit_heir_defs,
      grp == "Mitigation Type" ~ gen_mitpractices_defs
    )
    def_out
    # renderText(def_out)
  })
  
  # c(cat(paste0('"',names(mmp_data), '"'), sep = ", ")
  mmp_field_order <- c("MMPID", "Mitigation Approach", "Description", "GeneralizedMMP", "Resources", "SubGroup", 
                       "Stressors", "PotentialEffects", "DevelopmentPhases", "Industry", "MitigationHierarchy", 
                       "ImplementationStatus", "ImplementationDetails", "SpeciesNotes", "Citations", "Notes")

  # Read in Excel file
  MMP_DB_file = "Full-MMP-and-Reference-List-10-13-2023.xlsx"
  
  # write out "Electromagnetic Fields (EMF)" instead of just "EMF" under stressors
  mmp_data <- read_excel(MMP_DB_file, sheet = "MMP Data") %>% 
    dplyr::select(-c("MitigationMonitoring")) %>%
    # dplyr::mutate(across('Stressors', ~ str_replace(.x, 'EMF', 'Electromagnetic Fields (EMF)'))) %>% 
    dplyr::select(all_of(mmp_field_order))
  
  #remove NULLS, convert "NA" to NA
  mmp_data[mmp_data=="NULL"] <- NA
  mmp_data[mmp_data=="NA"] <- NA
  
  empty_data_out <- mmp_data %>% 
    filter(GeneralizedMMP == "pistachio gelato")
  
  # Render data table
  output$data_table <- DT::renderDT(datatable(filtered_data()[,2:16],
                                              selection = 'none', 
                                              rownames = F, 
                                              class = "display nowrap",
                                              plugins = 'ellipsis',
                                              extensions = c('Buttons','FixedColumns'),
                                              #add a download button to the table. https://rstudio.github.io/DT/extensions.html
                                              colnames = c("Mitigation Approach", "Description", "Mitigation Type", "Resource", "Resource Sub-group", "Stressors", "Potential Effects",
                                                "Development Phases", "Industry", "Mitigation Hierarchy", "Implementation Status", "Implementation Details",
                                                "Species Notes", "Citations", "Notes"),
                                              options = list(
                                                # fixedHeader = TRUE,
                                                fixedColumns = list(leftColumns = 1),
                                                pageLength = 15,
                                                # autoWidth = T,  #required to be T for column width setting to work
                                                paging = TRUE,
                                                searching = TRUE,
                                                scrollX = TRUE,
                                                escape = FALSE,
                                                dom = 'Bfrtip',
                                                buttons = list(list(extend = 'csv',
                                                                    filename = paste0('MPD_tool_v', str_replace_all(MPD_version_num, "\\.", "_"), "_filtered_", strftime(Sys.time(), "%Y%m%d_%H%M%S")),
                                                                    exportOptions = list(
                                                                      modifier = list(page = "all"),
                                                                      orthogonal = "export")
                                                                    ),
                                                               list(extend = 'excel',
                                                                    filename = paste0('MPD_tool_v', str_replace_all(MPD_version_num, "\\.", "_"), "_filtered_", strftime(Sys.time(), "%Y%m%d_%H%M%S")),
                                                                    exportOptions = list(
                                                                      modifier = list(page = "all"),
                                                                      orthogonal = "export"))
                                                               ),
                                                rowCallback = JS(
                                                  "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {",
                                                  "var full_text = 'Description: ' + aData[1]",
                                                  # "if (nRow > 0) {",
                                                  "$('td:eq(0)', nRow).attr('title', full_text);",
                                                  "}"),
                                                columnDefs = list(
                                                  list(
                                                    width = "120em",
                                                    targets = 0
                                                    # render = JS("$.fn.dataTable.render.ellipsis(40)")
                                                  ),
                                                  # list(
                                                  #   visible = FALSE,
                                                  #   targets = 1
                                                  #   ),
                                                  list(
                                                    width = "22em",
                                                    targets = 1:14,
                                                    render = JS("$.fn.dataTable.render.ellipsis(20)")
                                                    ))
                                                # Whether the buttons export all data or only visible data is determined by the server argument
                                                # in the DT::renderDT function call. If server=FALSE then the buttons will export all data in the
                                                # table, while if server=TRUE they will only export visible data.

                                                )
                                    ), server = F 
  )
  
  #Load the reference data
  mmp_ref_data <- read_excel(MMP_DB_file, sheet = "cross-check")
  
  #render filtered refs
  output$refs <- DT::renderDT(datatable(filtered_refs()[,c(2:4)],
                                        selection = 'none', 
                                        rownames = F, 
                                        class = "display nowrap",
                                        plugins = 'ellipsis',
                                        extensions = c('Buttons'),
                                        options = list(
                                          pageLength = 15,
                                          paging = TRUE,
                                          searching = TRUE,
                                          autoWidth = T,
                                          scrollX = TRUE,
                                          escape = FALSE,
                                          dom = 'Bfrtip',
                                          buttons = list(list(extend = 'csv', 
                                                              filename = paste0('MPD_tool_v', str_replace_all(MPD_version_num, "\\.", "_"), "_refs_", strftime(Sys.time(), "%Y%m%d_%H%M%S")),
                                                              exportOptions = list(
                                                                modifier = list(page = "all"),
                                                                orthogonal = "export")),
                                                         list(extend = 'excel', 
                                                              filename = paste0('MPD_tool_v', str_replace_all(MPD_version_num, "\\.", "_"), "_refs_", strftime(Sys.time(), "%Y%m%d_%H%M%S")),
                                                              exportOptions = list(
                                                                modifier = list(page = "all"),
                                                                orthogonal = "export"))),
                                          
                                          columnDefs = list(
                                            list(
                                              width = "14em",
                                              targets = 0:1,
                                              render = JS("$.fn.dataTable.render.ellipsis(20)")
                                            ),
                                            list(
                                              width = "14em",
                                              targets = 2,
                                              render = JS("$.fn.dataTable.render.ellipsis(60)")
                                            )
                                            
                                          ))
  ), server = F 
  )


  selection_tree_multilevel <- function(tree_selection) {
    sapply(tree_selection, function(level) {
      value = ""
      n_levels <- purrr::vec_depth(level[[1]])
      if (n_levels == 2){
        # value = paste0("(\\", names(level[[1]]), "\\b)")}
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
    # resources_selected <- selection_tree_multilevel(get_selected(input$resources_tree, format = "slices"))
    subgroup_selected <- selection_tree_multilevel(get_selected(input$subgroup_tree, format = "slices"))
    stressors_selected <- selection_tree_multilevel(get_selected(input$stressors_tree, format = "slices"))
    potential_effects_selected <- selection_tree_multilevel(get_selected(input$potential_effects_tree, format = "slices"))
    dev_phase_selected <- selection_tree_multilevel(get_selected(input$dev_phase_tree, format = "slices"))
    industries_selected <- selection_tree_multilevel(get_selected(input$industries_tree, format = "slices"))
    implem_status_selected <- selection_tree_multilevel(get_selected(input$implem_status_tree, format = "slices"))
    mitigation_hierarchy_selected <- selection_tree_multilevel(get_selected(input$mitigation_hierarchy_tree, format = "slices"))
    
    #check to see if anything selected, if not return empty header
    if(length(general_mmps_selected)  + length(stressors_selected) + length(potential_effects_selected) + length(dev_phase_selected) + 
       length(industries_selected) + length(subgroup_selected) + length(implem_status_selected) + length(mitigation_hierarchy_selected) > 0){
      # browser()
      data_out <- mmp_data %>%
        #multiple options to filter out in input and also in columns
        #need to add word boundaries
        filter(grepl(paste0(general_mmps_selected, collapse="|"), GeneralizedMMP) &
                 # grepl(paste0(resources_selected, collapse="|"), Resources) &
                 grepl(paste0(subgroup_selected, collapse="|"), SubGroup) &
                 grepl(paste0(stressors_selected, collapse="|"), Stressors) &
                 grepl(paste0(potential_effects_selected, collapse="|"), PotentialEffects) &
                 grepl(paste0(dev_phase_selected, collapse="|"), DevelopmentPhases) &
                 grepl(paste0(industries_selected, collapse="|"), Industry) &
                 grepl(paste0(implem_status_selected, collapse="|"), ImplementationStatus) &
                 grepl(paste0(mitigation_hierarchy_selected, collapse="|"), MitigationHierarchy)
        )
      return(data_out)
    } else {
      #return empty frame
      return(empty_data_out)
    }

  })
  
  #filter references too based on mmp data
  filtered_refs <- eventReactive(filtered_data(), {
    mmp_ref_data %>%
      filter(MMPID %in% filtered_data()$MMPID)
  })

  
} #server

# Run app
shinyApp(ui, server)

