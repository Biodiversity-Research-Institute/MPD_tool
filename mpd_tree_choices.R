
general_mmps_choices <- list(
  'Mitigation Type' = structure(list(
    'Barriers' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Compensation' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Deterrence/Attraction Reduction' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Engagement/Communication' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Fisheries Safety' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Lighting Alternatives' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Limit An Activity' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    # 'Monitoring' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Shutdown/Low Power' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Siting/Seasonality' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Structure Configuration' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Turbine Operation Parameters' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Vessel Operation Parameters' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Visibility and Detectability Enhancement' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Water Quality Management' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
  sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

# resources_choices <- list(
#   'Resources' = structure(list(
#     'Benthos' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
#     'Birds and Bats' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
#     'Fish' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
#     'Fisheries' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
#     'Marine Mammals' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
#     'Sea Turtles' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
#     sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

subgroup_choices <- list(
  'Resource sub-group' = structure(list(
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
      'Essential Fish Habitat'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Recreational Fisheries' = structure("",sttype="default",sticon="glyphicon glyphicon-record")),
      sttype="default",stopened=F,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Marine Mammals' = structure(list(
      'Low-Frequency Cetaceans'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Mid-Frequency Cetaceans' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'High-Frequency Cetaceans' = structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'Pinnipeds'= structure("",sttype="default",sticon="glyphicon glyphicon-record"),
      'North Atlantic Right Whale' = structure("",sttype="default",sticon="glyphicon glyphicon-record")),
      sttype="default",stopened=F,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Sea Turtles' = structure(list(), sttype="default",stopened=F,sticon="glyphicon glyphicon-plus", stdisabled=F)),
  sttype="default",stopened=T,sticon="glyphicon glyphicon-plus"))

stressors_choices <- list(
  'Stressors' = structure(list(
    'Bottom Disturbance' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Changes in Vessel Traffic' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Effects to Fishery Target Species' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'EMF' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Habitat Alteration' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Heat' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Impaired Safe Fishery Access' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Inadequate Infrastructure' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Insufficient Communication' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Light' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Long-Term Structures' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Loss of Fishing Grounds' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Scouring' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Sound' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Vibration' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Water Quality Changes' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

potential_effects_choices <- list(
  'Potential effects' = structure(list(
    'Attraction' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Behavioral Disturbance' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Change in Fishing Effort' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Community Alteration/Invasive Species' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Displacement' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Habitat Fragmentation/Modification' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Injury/Mortality' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Loss of Revenue' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

dev_phase_choices <- list(
  'Development phases' = structure(list(
    'Pre-construction' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F), 
    'Construction' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Operation and Maintenance' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Decommissioning' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)),
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

industries_choices <- list(
'Industry Origin' = structure(list(
  'Generic/General' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
  'Maritime' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
  'Offshore Wind' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
  'Oil and Gas' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
  'Onshore Wind' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
  sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

implem_status_choices <- list(
  'Implementation status' = structure(list(
    'Field Tested' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Implemented' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Implemented and Evidence of Effectiveness' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Not Implemented' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Unknown' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)), 
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))

mitigation_hierarchy_choices <- list(
  'Mitigation hierarchy' = structure(list(
    'Avoidance' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Minimization' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F),
    'Restoration' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F), 
    'Offset' = structure(list(), sttype="default",stopened=FALSE,sticon="glyphicon glyphicon-plus", stdisabled=F)),
    sttype="default",stopened=F,sticon="glyphicon glyphicon-plus"))



