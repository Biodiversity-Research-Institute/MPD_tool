

#Store unique choices
# general_mmps_choices <- generate_choices(mmp_data$GeneralizedMMP)
# [1] "Barriers"                        "Compensation"                    "Deterrence/Attraction Reduction" "Engagement/Communication"        "Fisheries Safety"               
# [6] "Lighting Alternatives"           "Limit An Activity"               "Monitoring"                      "Shutdown/Low Power"              "Siting/Seasonality"             
# [11] "Structure Configuration"         "Turbine Operation Parameters"    "Vessel Operation Parameters"     "Water Quality Management" 

general_mmps_choices <- list(
  'Generalized MMPs' = structure(list(
    'Barriers' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Compensation' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Deterrence/Attraction Reduction' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Engagement/Communication' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Fisheries Safety' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Lighting Alternatives' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Limit An Activity' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Monitoring' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Shutdown/Low Power' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Siting/Seasonality' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Structure Configuration' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Turbine Operation Parameters' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Vessel Operation Parameters' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Water Quality Management' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
  sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

# resources_choices <- generate_choices(mmp_data$Resources)
# [1] "Benthos"        "Birds and Bats" "Fish"           "Fisheries"      "Marine Mammals" "Sea Turtles" 

resources_choices <- list(
  'Resources' = structure(list(
    'Benthos' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Birds and Bats' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Fish' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Fisheries' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Marine Mammals' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Sea Turtles' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

# stressors_choices <- generate_choices(mmp_data$Stressors)
# [1] "Bottom Disturbance"                "Changes in Vessel Traffic"         "Effects to Fishery Target Species" "EMF"                              
# [5] "Heat"                              "Impaired Safe Fishery Access"      "Inadequate Infrastructure"         "Insufficient Communication"       
# [9] "Light"                             "Long-Term Structures"              "Loss of Fishing Grounds"           "Scouring"                         
# [13] "Sound"                             "Vibration" 

stressors_choices <- list(
  'Stressors' = structure(list(
    'Bottom Disturbance' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Changes in Vessel Traffic' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Effects to Fishery Target Species' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'EMF' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Heat' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Impaired Safe Fishery Access' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Inadequate Infrastructure' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Insufficient Communication' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Light' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Long-Term Structures' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Loss of Fishing Grounds' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Scouring' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Sound' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Vibration' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

# potential_effects_choices <- generate_choices(mmp_data$PotentialEffects)
# [1] "Attraction"                            "Behavioral Disturbance"                "Change in Fishing Effort"              "Community Alteration/Invasive Species"
# [5] "Displacement"                          "Habitat Fragmentation/Modification"    "Injury/Mortality"                      "Loss of Revenue" 

potential_effects_choices <- list(
  'Potential effects' = structure(list(
    'Attraction' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Behavioral Disturbance' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Change in Fishing Effort' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Community Alteration/Invasive Species' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Displacement' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Habitat Fragmentation/Modification' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Injury/Mortality"' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Loss of Revenue' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

# dev_phase_choices <- generate_choices(mmp_data$DevelopmentPhases)
# [1] "Construction"              "Decommissioning"           "Operation & Maintenance"   "Operation and Maintenance" "Pre-construction"     

dev_phase_choices <- list(
  'Development phases' = structure(list(
    'Construction' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Decommissioning' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Operation and Maintenance' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Pre-construction' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

# industries_choices <- generate_choices(mmp_data$Industry)
# [1] "Generic/General" "Maritime"        "Offshore Wind"   "Oil and Gas"     "Onshore Wind"   

industries_choices <- list(
'Industry' = structure(list(
  'Generic/General' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
  'Maritime' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
  'Offshore Wind' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
  'Oil and Gas' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
  'Onshore Wind' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
  sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

# subgroup_choices <- generate_choices(mmp_data$SubGroup)
# [1] "All Bats"                   "All Benthos"                "All Birds"                  "All Fish"                   "All Marine Mammals"         "Benthic Invertebrates"     
# [7] "Commercial Fisheries"       "Demersal/Groundfish"        "High-Frequency Cetaceans"   "Low-Frequency Cetaceans"    "Marine Birds"               "Mid-Frequency Cetaceans"   
# [13] "Nocturnal Aerial Migrants"  "North Atlantic Right Whale" "Pinnipeds"                  "Recreational Fisheries"     "Sea Turtles"                "Seagrass/Kelp/Algae" 
# Create subgroup tree data ----

subgroup_choices <- list(
  'Subgroups' = structure(list(
    'Bats' = structure(list(), sttype="default",stopened=F,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Benthos' = structure(list(
      'Demersal/Groundfish'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Benthic Invertebrates' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Seagrass/Kelp/Algae' = structure("",sttype="default",sticon="glyphicon glyphicon-record")),
      sttype="default",stopened=F,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Birds' = structure(list(
      'Marine Birds'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Nocturnal Aerial Migrants' = structure("",sttype="default",sticon="glyphicon glyphicon-record")),
      sttype="default",stopened=F,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Fish' = structure(list(
      'Pelagic Fish'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Demersal/Groundfish' = structure("",sttype="default",sticon="glyphicon glyphicon-record")),
      sttype="default",stopened=F,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Fisheries' = structure(list(
      'Commercial Fisheries'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Recreational Fisheries' = structure("",sttype="default",sticon="glyphicon glyphicon-record")),
      sttype="default",stopened=F,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Marine Mammal and Sea Turtle' = structure(list(
      'Low-Frequency Cetaceans'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Mid-Frequency Cetaceans' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'High-Frequency Cetaceans' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Pinnipeds'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'North Atlantic Right Whale' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Sea Turtles' = structure("",sttype="default",sticon="glyphicon glyphicon-record")),
      sttype="default",stopened=F,sticon="glyphicon glyphicon-plus", stdisabled=F)
  ),
  sttype="default",stopened=T,sticon="glyphicon glyphicon-plus")
)

# implem_status_choices <- generate_choices(mmp_data$ImplementationStatus)
# [1] "Field Tested"                              "Implemented"                               "Implemented and Evidence of Effectiveness"
# [4] "Not Implemented"                           "Unknown"   

implem_status_choices <- list(
  'Implementation status' = structure(list(
    'Field Tested' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Implemented' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Implemented and Evidence of Effectiveness' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Not Implemented' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Unknown' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))


# mitigation_hierarchy_choices <- generate_choices(mmp_data$MitigationHierarchy)
# [1] "Avoidance"    "Minimization" "Offset"       "Restoration" 

mitigation_hierarchy_choices <- list(
  'Mitigation hierarchy' = structure(list(
    'Avoidance' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Minimization' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Offset' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Restoration' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))


# load("subgroup_selected.rdata")
# 
# selection_tree_2level <- function(tree_selection) {
#   sapply(tree_selection, function(level) {
#     
#     n_levels <- purrr::vec_depth(level[[1]])
#     browser()
#     names(level[[1]])
#   })
# }
# 
# selection_tree_2level(subgroup_selected)
# 
# sapply(subgroup_selected, function(level) {
#   
#   n_levels <- purrr::vec_depth(level[[1]])
#   if (n_levels == 1){
#     names(level[[1]])}
#   if (n_levels == 2){
#     names(level[[1]][[1]])}
#   if (n_levels == 3){
#     names(level[[1]][[1]][[1]])}
# })



