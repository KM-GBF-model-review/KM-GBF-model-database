LoadDatabaseInR <- function(dataset_path) {
  # load in sheet
  questionnaires <- data.frame(
    readxl::read_excel(
      dataset_path,
      sheet = "KM-GBF database",
      skip = 3,
      .name_repair = "universal_quiet"
    )
  )
  
  # questionnaires <- questionnaires |>
  #   dplyr::filter(Model_full_names != "Ecosystem Demography (v2)")
  
  # add an attribute to the columns which are filled in with numbers, pointing to
  # the corresponding answer.
  
  attr(questionnaires$Description.Domain_of_focus, "value") <- c(
    "Biodiversity/ species",
    "Ecosystem",
    "Nature's contributions to people",
    "Land, freshwater, ocean use/ protection/ restoration",
    "Resource management/ exploitation",
    "Invasive alien species",
    "Pollution",
    "Climate Change",
    "Economy",
    "Governance",
    "Human wellbeing/ Good quality of life",
    "Others"
  )
  
  
  attr(questionnaires$Description.Regional_scale, "value") <- c("Global",
                                                                "Continental/regional",
                                                                "National",
                                                                "Subnational/local")
  
  
  attr(questionnaires$Description.Realm, "value") <- c("Terrestrial",
                                                       "Marine",
                                                       "Freshwater",
                                                       "Anthropomorphized Areas")
  
  
  attr(questionnaires$Description.Species_group, "value") <- c(
    "Amphibians",
    "Bacteria",
    "Birds",
    "Fish",
    "Fungi",
    "Invertebrates",
    "Mammals",
    "Plants/Trees",
    "Reptiles",
    "Protists",
    "All species",
    "Not applicable"
  )
  
  
  attr(questionnaires$Description.Ecosystems, "value") <- c(
    "Coastal",
    "Cropland",
    "Grassland",
    "Heathland and shrub",
    "Marine inlet and transitional waters",
    "Montane",
    "Open ocean",
    "Peatland",
    "Rivers and lakes",
    "Shelf",
    "Sparsely vegetated land",
    "Tundra and taiga",
    "Urban/peri-urban",
    "Wetlands",
    "Woodland and forest",
    "All ecosystems",
    "Not applicable"
  )
  
  
  attr(questionnaires$Description.NCP, "value") <- c(
    "Habitat",
    "Crop pollination",
    "Air quality regulation",
    "Climate regulation",
    "Ocean acidification regulation",
    "Water qauntity and flow regulation",
    "Water quality regulation",
    "Soil protection",
    "Hazard regulation",
    "Pest regulation",
    "Bioenergy",
    "Food",
    "Wood material",
    "Medicine",
    "Learning and inspiration",
    "Experience",
    "Identity",
    "Diversity of options"
  )
  
  
  
  attr(questionnaires$Drivers.Drivers_represented_in_the_model,
       "value") <- c(
         "Land use",
         "Sea use",
         "Climate Change",
         "Resource exploitation",
         "Invasive alien species",
         "Pollution",
         "Demography",
         "Economy",
         "Energy",
         "Infrastructure/ transport",
         "Goverance",
         "Sociocultural/ Values",
         "Technology",
         "Health",
         "Lifestyle/behaviour",
         "Others"
       )
  
  
  attr(questionnaires$Drivers.Considers_Driver_Interaction,
       "value") <- c("Yes", "No", "Not yet, but possible")
  
  
  attr(questionnaires$Input.Model_Uses_Obersvation_Data, "value") <-
    c("Yes",
      "No",
      "Not yet, but possible",
      "Possible, depending on model selection")
  
  
  attr(questionnaires$Input.Model_Scalable, "value") <-
    c("Yes", "No", "Not yet, but possible")
  
  
  
  attr(questionnaires$Methods.Methodological_Approach, "value") <-
    c(
      "Expert-based (knowledge/qualitative)",
      "Correlative (statistical/ quantitative",
      "Process-based (mechanistic)",
      "Hybrid (correlative + process-based)",
      "Others"
    )
  
  
  attr(questionnaires$Methods.Detect_Changes_In_Nature, "value") <-
    c("Yes", "No", "Partially")
  
  
  attr(questionnaires$Methods.Detect_Changes_In_Drivers, "value") <-
    c("Yes", "No", "Partially")
  
  attr(
    questionnaires$Methods.Detect_and_Attribute_Changes_In_Nature_To_Drivers,
    "value"
  ) <- c("Yes", "No", "Not yet, but possible")
  
  
  
  
  attr(questionnaires$Methods.Model_Evaluated_On_Calibration_Data,
       "value") <-
    c("Yes", "No", "Partially")
  
  
  attr(
    questionnaires$QA_Methods.Model_Evaluation_Calibration_Validation_during_model_development,
    "value"
  ) <-
    c("Yes", "No", "Partially")
  
  attr(questionnaires$QA_Methods.Update_Calibration, "value") <-
    c("Yes", "No", "Partially", "Not yet, but possible")
  
  attr(
    questionnaires$Methods.Model_Validated_On_Data_Independent_From_Calibration,
    "value"
  ) <- c("Yes", "No", "Partially")
  
  
  attr(questionnaires$Output.Spatially_Explicit_Output, "value") <-
    c("Yes", "No", "Not yet, but possible")
  
  
  attr(questionnaires$Output.Temporally_Dynamic_Outputs, "value") <-
    c("Dynamic", "Static")
  
  
  attr(questionnaires$Integrated_Modelling.Model_work_Jointly_With_Others,
       "value") <- c("Yes", "No")
  
  
  attr(
    questionnaires$Integrated_Modelling.Joint_Modelling_Reflect_Interactions_And_Feedbacks_Between_Nature_And_Drivers,
    "value"
  ) <- c("Yes", "No")
  
  
  attr(questionnaires$Scenario_Analyses.Scenario_Analysis_Capabilities,
       "value") <- c(
         "Exploratory",
         "Target setting",
         "Policy screening",
         "Retrospective (monitoring/ observation)",
         "Spatial planning"
       )
  
  
  attr(questionnaires$Scenario_Analyses.Temporal_Scale_Model_Projections,
       "value") <- c(
         "back-casting",
         "Short-term (until 2030)",
         "Mid-term (until 2050)",
         "Long-term (until 2070 to 2100)"
       )
  
  
  attr(
    questionnaires$Scenario_Analyses.Reflect_Policy_Options_In_Simulation,
    "value"
  ) <- c("Yes", "No")
  
  attr(
    questionnaires$Scenario_Analyses.Reflect_Management_Options_In_Simulation,
    "value"
  ) <- c("Yes", "No")
  
  
  attr(questionnaires$Scenario_Analyses.Explore_Interventions_Positive,
       "value") <- c("Yes", "No")
  
  
  attr(questionnaires$Scenario_Analyses.Explore_Interventions_Negative,
       "value") <- c("Yes", "No")
  
  
  attr(questionnaires$KMGBF_Relevance.KMGBF_Goals_Current,
       "value") <- c(
         "Goal A - ecosystems",
         "Goal A - species",
         "Goal A - genetic diversity",
         "Goal B - nature's contributions to people"
       )
  
  attr(questionnaires$QA_KMGBF_Relevance.KMGBF_Goals_Aspirational,
       "value") <- c(
         "Goal A - ecosystems",
         "Goal A - species",
         "Goal A - genetic diversity",
         "Goal B - nature's contributions to people"
       )
  
  
  attr(questionnaires$KMGBF_Relevance.KMGBF_Targets_Current,
       "value") <-
    paste("Target", 1:23)
  
  attr(questionnaires$QA_KMGBF_Relevance.KMGBF_Targets_Aspirational,
       "value") <-
    paste("Target", 1:23)
  
  attr(questionnaires$NFF_Relevance.Reflect_Intrinsic_Value_Input,
       "value") <- c("Yes",
                     "No",
                     "Not yet, but possible",
                     "Possible depending on application context")
  
  
  attr(questionnaires$NFF_Relevance.Reflect_Intrinsic_Value_Output,
       "value") <- c("Yes",
                     "No",
                     "Not yet, but possible",
                     "Possible depending on application context")
  
  attr(questionnaires$NFF_Relevance.Reflect_Instrumental_Value_Input,
       "value") <- c("Yes",
                     "No",
                     "Not yet, but possible",
                     "Possible depending on application context")
  
  attr(questionnaires$NFF_Relevance.Reflect_Instrumental_Value_Output,
       "value") <- c("Yes",
                     "No",
                     "Not yet, but possible",
                     "Possible depending on application context")
  
  attr(questionnaires$NFF_Relevance.Reflect_Cultural_Value_Input,
       "value") <- c("Yes",
                     "No",
                     "Not yet, but possible",
                     "Possible depending on application context")
  
  
  attr(questionnaires$NFF_Relevance.Reflect_Cultural_Value_Output,
       "value") <- c("Yes",
                     "No",
                     "Not yet, but possible",
                     "Possible depending on application context")
  
  
  attr(questionnaires$QA.Reflect_diverse_NFF_values_Current,
       "value") <-
    c(
      "Intrinsic value as input",
      "Intrinsic value as output",
      "Instrumental value as input",
      "Instrumental value as output",
      "Cultural value as output",
      "Cultural value as output"
    )
  
  attr(questionnaires$QA.Reflect_diverse_NFF_values_Aspirational,
       "value") <-
    c(
      "Intrinsic value as input",
      "Intrinsic value as output",
      "Instrumental value as input",
      "Instrumental value as output",
      "Cultural value as output",
      "Cultural value as output"
    )
  
  
  attr(questionnaires$Application_Use.Informed_Policy, "value") <-
    c("Yes", "No")
  
  
  
  return(questionnaires)
  
  
}
