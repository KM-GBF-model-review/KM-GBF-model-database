LoadDatabaseInR <- function(dataset_path) {
  # load in sheet
  questionnaires <- data.frame(
    readxl::read_excel(
      dataset_path,
      sheet = "Dataset",
      skip = 4,
      .name_repair = "universal_quiet"
    )
  )
  # dplyr::filter(!(Name_of_the_model_with_acronym %in% c("EDv3", "ED2")))

  questions <- as.character(readxl::read_excel(
    dataset_path,
    sheet = "Dataset",
    .name_repair = "universal_quiet"
  )[1, ])

  # add attributes

  attr(questionnaires$Description.Resource_type, "value") <- c(
    "Model",
    "Indicator",
    "Data source"
  )

  attr(
    questionnaires$Description.Resource_type,
    "name"
  ) <- "Modelling resource type"
  attr(questionnaires$Description.Resource_type, "question") <-
    questions[which(colnames(questionnaires) == "Description.Resource_type")]

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
  attr(questionnaires$Description.Domain_of_focus, "name") <- "Domain of focus"
  attr(questionnaires$Description.Domain_of_focus, "question") <-
    questions[which(colnames(questionnaires) == "Description.Domain_of_focus")]

  attr(questionnaires$Description.Regional_scale, "value") <- c(
    "Global",
    "Continental/regional",
    "National",
    "Subnational/local"
  )
  attr(questionnaires$Description.Regional_scale, "name") <- "Regional scale"
  attr(questionnaires$Description.Regional_scale, "question") <-
    questions[which(colnames(questionnaires) == "Description.Regional_scale")]

  attr(questionnaires$Description.Realm, "value") <- c(
    "Terrestrial",
    "Marine",
    "Freshwater",
    "Anthropomorphized Areas"
  )
  attr(questionnaires$Description.Realm, "name") <- "Realm"
  attr(questionnaires$Description.Realm, "question") <-
    questions[which(colnames(questionnaires) == "Description.Realm")]

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
  attr(questionnaires$Description.Species_group, "name") <- "Species group"
  attr(questionnaires$Description.Species_group, "question") <-
    questions[which(colnames(questionnaires) == "Description.Species_group")]

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
  attr(questionnaires$Description.Ecosystems, "name") <- "Ecosystems"
  attr(questionnaires$Description.Ecosystems, "question") <-
    questions[which(colnames(questionnaires) == "Description.Ecosystems")]

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
  attr(
    questionnaires$Description.NCP,
    "name"
  ) <- "Nature's Contributions to People (NCP)"
  attr(questionnaires$Description.NCP, "question") <-
    questions[which(colnames(questionnaires) == "Description.NCP")]

  attr(questionnaires$Drivers.Drivers_represented_in_the_model, "value") <- c(
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
  attr(
    questionnaires$Drivers.Drivers_represented_in_the_model,
    "name"
  ) <- "Drivers represented in the model"
  attr(questionnaires$Drivers.Drivers_represented_in_the_model, "question") <-
    questions[which(
      colnames(questionnaires) == "Drivers.Drivers_represented_in_the_model"
    )]

  attr(questionnaires$Drivers.Considers_Driver_Interaction, "value") <- c(
    "Yes",
    "No",
    "Not yet, but possible"
  )
  attr(
    questionnaires$Drivers.Considers_Driver_Interaction,
    "name"
  ) <- "Considers driver interaction"
  attr(questionnaires$Drivers.Considers_Driver_Interaction, "question") <-
    questions[which(
      colnames(questionnaires) == "Drivers.Considers_Driver_Interaction"
    )]

  attr(questionnaires$Input.Model_Uses_Obersvation_Data, "value") <- c(
    "Yes",
    "No",
    "Not yet, but possible",
    "Possible, depending on model selection"
  )
  attr(
    questionnaires$Input.Model_Uses_Obersvation_Data,
    "name"
  ) <- "Model uses observation data"
  attr(questionnaires$Input.Model_Uses_Obersvation_Data, "question") <-
    questions[which(
      colnames(questionnaires) == "Input.Model_Uses_Obersvation_Data"
    )]

  attr(questionnaires$Input.Model_Scalable, "value") <- c(
    "Yes",
    "No",
    "Not yet, but possible"
  )
  attr(questionnaires$Input.Model_Scalable, "name") <- "Model scalable"
  attr(questionnaires$Input.Model_Scalable, "question") <-
    questions[which(colnames(questionnaires) == "Input.Model_Scalable")]

  attr(questionnaires$Methods.Methodological_Approach, "value") <- c(
    "Expert-based (knowledge/qualitative)",
    "Correlative (statistical/ quantitative",
    "Process-based (mechanistic)",
    "Hybrid (correlative + process-based)",
    "Others"
  )
  attr(
    questionnaires$Methods.Methodological_Approach,
    "name"
  ) <- "Methodological approach"
  attr(questionnaires$Methods.Methodological_Approach, "question") <-
    questions[which(
      colnames(questionnaires) == "Methods.Methodological_Approach"
    )]

  attr(questionnaires$Methods.Detect_Changes_In_Nature, "value") <- c(
    "Yes",
    "No",
    "Partially"
  )
  attr(
    questionnaires$Methods.Detect_Changes_In_Nature,
    "name"
  ) <- "Detect changes in nature"
  attr(questionnaires$Methods.Detect_Changes_In_Nature, "question") <-
    questions[which(
      colnames(questionnaires) == "Methods.Detect_Changes_In_Nature"
    )]

  attr(questionnaires$Methods.Detect_Changes_In_Drivers, "value") <- c(
    "Yes",
    "No",
    "Partially"
  )
  attr(
    questionnaires$Methods.Detect_Changes_In_Drivers,
    "name"
  ) <- "Detect changes in drivers"
  attr(questionnaires$Methods.Detect_Changes_In_Drivers, "question") <-
    questions[which(
      colnames(questionnaires) == "Methods.Detect_Changes_In_Drivers"
    )]

  attr(
    questionnaires$Methods.Detect_and_Attribute_Changes_In_Nature_To_Drivers,
    "value"
  ) <- c("Yes", "No", "Not yet, but possible")
  attr(
    questionnaires$Methods.Detect_and_Attribute_Changes_In_Nature_To_Drivers,
    "name"
  ) <- "Detect and attribute changes in nature to drivers"
  attr(
    questionnaires$Methods.Detect_and_Attribute_Changes_In_Nature_To_Drivers,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "Methods.Detect_and_Attribute_Changes_In_Nature_To_Drivers"
    )]

  attr(
    questionnaires$Methods.Model_Evaluated_On_Calibration_Data,
    "value"
  ) <- c("Yes", "No", "Partially")
  attr(
    questionnaires$Methods.Model_Evaluated_On_Calibration_Data,
    "name"
  ) <- "Model evaluated on calibration data"
  attr(
    questionnaires$Methods.Model_Evaluated_On_Calibration_Data,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) == "Methods.Model_Evaluated_On_Calibration_Data"
    )]

  attr(
    questionnaires$QA_Methods.Model_Evaluation_Calibration_Validation_during_model_development,
    "value"
  ) <- c("Yes", "No", "Partially")
  attr(
    questionnaires$QA_Methods.Model_Evaluation_Calibration_Validation_during_model_development,
    "name"
  ) <- "Model evaluation, calibration and validation during model development"
  attr(
    questionnaires$QA_Methods.Model_Evaluation_Calibration_Validation_during_model_development,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "QA_Methods.Model_Evaluation_Calibration_Validation_during_model_development"
    )]

  attr(questionnaires$QA_Methods.Update_Calibration, "value") <- c(
    "Yes",
    "No",
    "Partially",
    "Not yet, but possible"
  )
  attr(
    questionnaires$QA_Methods.Update_Calibration,
    "name"
  ) <- "Update calibration"
  attr(questionnaires$QA_Methods.Update_Calibration, "question") <-
    questions[which(
      colnames(questionnaires) == "QA_Methods.Update_Calibration"
    )]

  attr(
    questionnaires$Methods.Model_Validated_On_Data_Independent_From_Calibration,
    "value"
  ) <- c("Yes", "No", "Partially")
  attr(
    questionnaires$Methods.Model_Validated_On_Data_Independent_From_Calibration,
    "name"
  ) <- "Model validated on independent data"
  attr(
    questionnaires$Methods.Model_Validated_On_Data_Independent_From_Calibration,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "Methods.Model_Validated_On_Data_Independent_From_Calibration"
    )]

  attr(questionnaires$Output.Spatially_Explicit_Output, "value") <- c(
    "Yes",
    "No",
    "Not yet, but possible"
  )
  attr(
    questionnaires$Output.Spatially_Explicit_Output,
    "name"
  ) <- "Spatially explicit output"
  attr(questionnaires$Output.Spatially_Explicit_Output, "question") <-
    questions[which(
      colnames(questionnaires) == "Output.Spatially_Explicit_Output"
    )]

  attr(questionnaires$Output.Temporally_Dynamic_Outputs, "value") <- c(
    "Dynamic",
    "Static"
  )
  attr(
    questionnaires$Output.Temporally_Dynamic_Outputs,
    "name"
  ) <- "Temporally dynamic outputs"
  attr(questionnaires$Output.Temporally_Dynamic_Outputs, "question") <-
    questions[which(
      colnames(questionnaires) == "Output.Temporally_Dynamic_Outputs"
    )]

  attr(
    questionnaires$Integrated_Modelling.Model_work_Jointly_With_Others,
    "value"
  ) <- c("Yes", "No")
  attr(
    questionnaires$Integrated_Modelling.Model_work_Jointly_With_Others,
    "name"
  ) <- "Model works jointly with others"
  attr(
    questionnaires$Integrated_Modelling.Model_work_Jointly_With_Others,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "Integrated_Modelling.Model_work_Jointly_With_Others"
    )]

  attr(
    questionnaires$Integrated_Modelling.Joint_Modelling_Reflect_Interactions_And_Feedbacks_Between_Nature_And_Drivers,
    "value"
  ) <- c("Yes", "No")
  attr(
    questionnaires$Integrated_Modelling.Joint_Modelling_Reflect_Interactions_And_Feedbacks_Between_Nature_And_Drivers,
    "name"
  ) <- "Joint modelling reflects interactions and feedbacks"
  attr(
    questionnaires$Integrated_Modelling.Joint_Modelling_Reflect_Interactions_And_Feedbacks_Between_Nature_And_Drivers,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "Integrated_Modelling.Joint_Modelling_Reflect_Interactions_And_Feedbacks_Between_Nature_And_Drivers"
    )]

  attr(
    questionnaires$Scenario_Analyses.Scenario_Analysis_Capabilities,
    "value"
  ) <- c(
    "Exploratory",
    "Target setting",
    "Policy screening",
    "Retrospective (monitoring/ observation)",
    "Spatial planning"
  )
  attr(
    questionnaires$Scenario_Analyses.Scenario_Analysis_Capabilities,
    "name"
  ) <- "Scenario analysis capabilities"
  attr(
    questionnaires$Scenario_Analyses.Scenario_Analysis_Capabilities,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "Scenario_Analyses.Scenario_Analysis_Capabilities"
    )]

  attr(
    questionnaires$Scenario_Analyses.Temporal_Scale_Model_Projections,
    "value"
  ) <- c(
    "back-casting",
    "Short-term (until 2030)",
    "Mid-term (until 2050)",
    "Long-term (until 2070 to 2100)"
  )
  attr(
    questionnaires$Scenario_Analyses.Temporal_Scale_Model_Projections,
    "name"
  ) <- "Temporal scale of model projections"
  attr(
    questionnaires$Scenario_Analyses.Temporal_Scale_Model_Projections,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "Scenario_Analyses.Temporal_Scale_Model_Projections"
    )]

  attr(
    questionnaires$Scenario_Analyses.Reflect_Policy_Options_In_Simulation,
    "value"
  ) <- c("Yes", "No")
  attr(
    questionnaires$Scenario_Analyses.Reflect_Policy_Options_In_Simulation,
    "name"
  ) <- "Reflect policy options in simulation"
  attr(
    questionnaires$Scenario_Analyses.Reflect_Policy_Options_In_Simulation,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "Scenario_Analyses.Reflect_Policy_Options_In_Simulation"
    )]

  attr(
    questionnaires$Scenario_Analyses.Reflect_Management_Options_In_Simulation,
    "value"
  ) <- c("Yes", "No")
  attr(
    questionnaires$Scenario_Analyses.Reflect_Management_Options_In_Simulation,
    "name"
  ) <- "Reflect management options in simulation"
  attr(
    questionnaires$Scenario_Analyses.Reflect_Management_Options_In_Simulation,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "Scenario_Analyses.Reflect_Management_Options_In_Simulation"
    )]

  attr(
    questionnaires$Scenario_Analyses.Explore_Interventions_Positive,
    "value"
  ) <- c("Yes", "No")
  attr(
    questionnaires$Scenario_Analyses.Explore_Interventions_Positive,
    "name"
  ) <- "Explore positive interventions"
  attr(
    questionnaires$Scenario_Analyses.Explore_Interventions_Positive,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "Scenario_Analyses.Explore_Interventions_Positive"
    )]

  attr(
    questionnaires$Scenario_Analyses.Explore_Interventions_Negative,
    "value"
  ) <- c("Yes", "No")
  attr(
    questionnaires$Scenario_Analyses.Explore_Interventions_Negative,
    "name"
  ) <- "Explore negative interventions"
  attr(
    questionnaires$Scenario_Analyses.Explore_Interventions_Negative,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "Scenario_Analyses.Explore_Interventions_Negative"
    )]

  attr(questionnaires$KMGBF_Relevance.KMGBF_Goals_Current, "value") <- c(
    "Goal A - ecosystems",
    "Goal A - species",
    "Goal A - genetic diversity",
    "Goal B - nature's contributions to people"
  )
  attr(
    questionnaires$KMGBF_Relevance.KMGBF_Goals_Current,
    "name"
  ) <- "KMGBF goals (current)"
  attr(questionnaires$KMGBF_Relevance.KMGBF_Goals_Current, "question") <-
    questions[which(
      colnames(questionnaires) == "KMGBF_Relevance.KMGBF_Goals_Current"
    )]

  attr(
    questionnaires$QA_KMGBF_Relevance.KMGBF_Goals_Aspirational,
    "value"
  ) <- c(
    "Goal A - ecosystems",
    "Goal A - species",
    "Goal A - genetic diversity",
    "Goal B - nature's contributions to people"
  )
  attr(
    questionnaires$QA_KMGBF_Relevance.KMGBF_Goals_Aspirational,
    "name"
  ) <- "KMGBF goals (aspirational)"
  attr(
    questionnaires$QA_KMGBF_Relevance.KMGBF_Goals_Aspirational,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) == "QA_KMGBF_Relevance.KMGBF_Goals_Aspirational"
    )]

  attr(questionnaires$KMGBF_Relevance.KMGBF_Targets_Current, "value") <- paste(
    "Target",
    1:23
  )
  attr(
    questionnaires$KMGBF_Relevance.KMGBF_Targets_Current,
    "name"
  ) <- "KMGBF targets (current)"
  attr(questionnaires$KMGBF_Relevance.KMGBF_Targets_Current, "question") <-
    questions[which(
      colnames(questionnaires) == "KMGBF_Relevance.KMGBF_Targets_Current"
    )]

  attr(
    questionnaires$QA_KMGBF_Relevance.KMGBF_Targets_Aspirational,
    "value"
  ) <- paste("Target", 1:23)
  attr(
    questionnaires$QA_KMGBF_Relevance.KMGBF_Targets_Aspirational,
    "name"
  ) <- "KMGBF targets (aspirational)"
  attr(
    questionnaires$QA_KMGBF_Relevance.KMGBF_Targets_Aspirational,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "QA_KMGBF_Relevance.KMGBF_Targets_Aspirational"
    )]

  attr(
    questionnaires$NFF_Relevance.Reflect_Intrinsic_Value_Input,
    "value"
  ) <- c(
    "Yes",
    "No",
    "Not yet, but possible",
    "Possible depending on application context"
  )
  attr(
    questionnaires$NFF_Relevance.Reflect_Intrinsic_Value_Input,
    "name"
  ) <- "Reflect intrinsic value (input)"
  attr(
    questionnaires$NFF_Relevance.Reflect_Intrinsic_Value_Input,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) == "NFF_Relevance.Reflect_Intrinsic_Value_Input"
    )]

  attr(
    questionnaires$NFF_Relevance.Reflect_Intrinsic_Value_Output,
    "value"
  ) <- c(
    "Yes",
    "No",
    "Not yet, but possible",
    "Possible depending on application context"
  )
  attr(
    questionnaires$NFF_Relevance.Reflect_Intrinsic_Value_Output,
    "name"
  ) <- "Reflect intrinsic value (output)"
  attr(
    questionnaires$NFF_Relevance.Reflect_Intrinsic_Value_Output,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) == "NFF_Relevance.Reflect_Intrinsic_Value_Output"
    )]

  attr(
    questionnaires$NFF_Relevance.Reflect_Instrumental_Value_Input,
    "value"
  ) <- c(
    "Yes",
    "No",
    "Not yet, but possible",
    "Possible depending on application context"
  )
  attr(
    questionnaires$NFF_Relevance.Reflect_Instrumental_Value_Input,
    "name"
  ) <- "Reflect instrumental value (input)"
  attr(
    questionnaires$NFF_Relevance.Reflect_Instrumental_Value_Input,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "NFF_Relevance.Reflect_Instrumental_Value_Input"
    )]

  attr(
    questionnaires$NFF_Relevance.Reflect_Instrumental_Value_Output,
    "value"
  ) <- c(
    "Yes",
    "No",
    "Not yet, but possible",
    "Possible depending on application context"
  )
  attr(
    questionnaires$NFF_Relevance.Reflect_Instrumental_Value_Output,
    "name"
  ) <- "Reflect instrumental value (output)"
  attr(
    questionnaires$NFF_Relevance.Reflect_Instrumental_Value_Output,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "NFF_Relevance.Reflect_Instrumental_Value_Output"
    )]

  attr(questionnaires$NFF_Relevance.Reflect_Cultural_Value_Input, "value") <- c(
    "Yes",
    "No",
    "Not yet, but possible",
    "Possible depending on application context"
  )
  attr(
    questionnaires$NFF_Relevance.Reflect_Cultural_Value_Input,
    "name"
  ) <- "Reflect cultural value (input)"
  attr(questionnaires$NFF_Relevance.Reflect_Cultural_Value_Input, "question") <-
    questions[which(
      colnames(questionnaires) == "NFF_Relevance.Reflect_Cultural_Value_Input"
    )]

  attr(
    questionnaires$NFF_Relevance.Reflect_Cultural_Value_Output,
    "value"
  ) <- c(
    "Yes",
    "No",
    "Not yet, but possible",
    "Possible depending on application context"
  )
  attr(
    questionnaires$NFF_Relevance.Reflect_Cultural_Value_Output,
    "name"
  ) <- "Reflect cultural value (output)"
  attr(
    questionnaires$NFF_Relevance.Reflect_Cultural_Value_Output,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) == "NFF_Relevance.Reflect_Cultural_Value_Output"
    )]

  attr(questionnaires$QA.Reflect_diverse_NFF_values_Current, "value") <- c(
    "Intrinsic value as input",
    "Intrinsic value as output",
    "Instrumental value as input",
    "Instrumental value as output",
    "Cultural value as input",
    "Cultural value as output"
  )
  attr(
    questionnaires$QA.Reflect_diverse_NFF_values_Current,
    "name"
  ) <- "Reflect diverse NFF values (current)"
  attr(questionnaires$QA.Reflect_diverse_NFF_values_Current, "question") <-
    questions[which(
      colnames(questionnaires) == "QA.Reflect_diverse_NFF_values_Current"
    )]

  attr(questionnaires$QA.Reflect_diverse_NFF_values_Aspirational, "value") <- c(
    "Intrinsic value as input",
    "Intrinsic value as output",
    "Instrumental value as input",
    "Instrumental value as output",
    "Cultural value as input",
    "Cultural value as output"
  )
  attr(
    questionnaires$QA.Reflect_diverse_NFF_values_Aspirational,
    "name"
  ) <- "Reflect diverse NFF values (aspirational)"
  attr(questionnaires$QA.Reflect_diverse_NFF_values_Aspirational, "question") <-
    questions[which(
      colnames(questionnaires) == "QA.Reflect_diverse_NFF_values_Aspirational"
    )]

  attr(questionnaires$Application_Use.Informed_Policy, "value") <- c(
    "Yes",
    "No"
  )
  attr(
    questionnaires$Application_Use.Informed_Policy,
    "name"
  ) <- "Informed policy"
  attr(questionnaires$Application_Use.Informed_Policy, "question") <-
    questions[which(
      colnames(questionnaires) == "Application_Use.Informed_Policy"
    )]

  attr(questionnaires$QA_Questionnaire_validation.Validated, "value") <- c(
    "Yes",
    "No"
  )
  attr(
    questionnaires$QA_Questionnaire_validation.Validated,
    "name"
  ) <- "Quality assurance - Validated"
  attr(questionnaires$QA_Questionnaire_validation.Validated, "question") <-
    questions[which(
      colnames(questionnaires) == "QA_Questionnaire_validation.Validated"
    )]

  attr(
    questionnaires$QA_Questionnaire_validation.Additional_questions,
    "value"
  ) <- c("Yes", "No")
  attr(
    questionnaires$QA_Questionnaire_validation.Additional_questions,
    "name"
  ) <- "Quality assurance - Second round"
  attr(
    questionnaires$QA_Questionnaire_validation.Additional_questions,
    "question"
  ) <-
    questions[which(
      colnames(questionnaires) ==
        "QA_Questionnaire_validation.Additional_questions"
    )]

  return(questionnaires)
}
