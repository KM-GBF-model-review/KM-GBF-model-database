#==============================================================================#
# KM-GBF Model Review                                                          #
# Bernd Lenzner (bernd.lenzner@univie.ac.at)                                   #
# 2025                                                                         #
#                                                                              #
# Cluster analysis                                                             #
#                                                                              #
#==============================================================================#


#==============================================================================#
# Packages ----

## Package names of the ones we need
packages <- c("tidyverse", "cluster", "factoextra", "paletteer", "fcp"
              ,"cowplot", "grateful", "ggpubr", "caret", "factoextra", "paletteer", "fcp", "ggrepel"
              ) 

## Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

## Load packages
invisible(lapply(packages, library, character.only = TRUE))


#==============================================================================#
# Project paths ----
datum <- Sys.Date()

dir <- "D:"
proj.path <- paste0(paste0(dir, "/Publication_Projects/2024_Kim_KM-GBF-Sceanrios-Review/Review Publication"))     # path for directory with code
code.path <- paste0(proj.path, '/Code')                          # path for directory datasets
data.path <- paste0(proj.path, '/Data')                          # path for directory datasets
res.path <- paste0(proj.path, '/Results')                        # path for directory with results





#==============================================================================#
# Get references for R and all packages used ----

cite_packages(pkgs = "Session", 
              out.format = "docx", #citation.style = "nature",
              out.dir = res.path,
              out.file = "R-packages-citation")


#==============================================================================#
# Functions  ----
## Load custom functions
source(paste0(code.path, "/Functions.R"))

### separate_columns() 
#### separating out all rows with aggregated information (e.g., multiple drivers mentioned in the driver column) to have individual rows per entry (multiplying rows per study)

#==============================================================================#
# Data  ----
## Load and prepare dataset
dat <- read_delim(paste0(data.path, "/2025-08-27-dataset.csv"),
           delim = ",",
           skip = 3)
  

x <- dat %>%
  select(!Model_Type:Model_ID & 
           !Model_full_names : Key_references &
           !Description.Region_nation_subnation &
           !Drivers.Drivers_description &
           !Drivers.Considers_Driver_Interactions_comments &
           !Input.Input_Data_Requirements &
           !Input.Model_Uses_Obersvation_Data_comments &
           !Input.Key_Parameterization_Requirements &
           !Methods.Methodological_Approach_comments &
           !Methods.Detect_Changes_In_Nature_comments &
           !Methods.Detect_Changes_In_Drivers_comments &
           !Methods.Detect_and_Attribute_Changes_In_Nature_To_Drivers_comments &
           !Output.Spatial_Scale_With_Confidence &
           !Output.Temporal_Scale_With_Confidence &
           !Integrated_Modelling.Model_work_Jointly_With_Others_comments &
           !Integrated_Modelling.Joint_Modelling_Reflect_Interactions_And_Feedbacks_Between_Nature_And_Drivers_comments &
           !Scenario_Analyses.Explore_Nature_Positive_Impacts_comments &
           !Scenario_Analyses.Explore_Nature_Negative_Impacts_comments &
           !KMGBF_Relevance.KMGBF_Goal_Elements &
           !KMGBF_Relevance.KMGBF_Goal_Indicators &
           !KMGBF_Relevance.KMGBF_Target_Elements &
           !KMGBF_Relevance.KMGBF_Target_Indicators &
           !NFF_Relevance.Reflect_Intrinsic_Value_Input_Requirements_Parameters &
           !NFF_Relevance.Reflect_Instrumental_Value_Input_Requirements_Parameters &
           !NFF_Relevance.Reflect_Cultural_Value_Input_Requirements_Parameters &
           !NFF_Relevance.Reflect_Intrinsic_Value_Output_Indicators &
           !NFF_Relevance.Reflect_Instrumental_Value_Output_Indicators &
           !NFF_Relevance.Reflect_Cultural_Value_Output_Indicators &
           !Application_Use.Informed_Policy : ...101
           ) %>%
  rename(
    Model = Name_of_the_model_with_acronym,
    Domain = Description.Domain_of_focus,
    Spatial_Scale = Description.Regional_scale,
    Realm = Description.Realm,
    Tax_Group = Description.Species_group,
    Ecosystem = Description.Ecosystems,
    NCP = Description.NCP,
    Driver = Drivers.Drivers_represented_in_the_model,
    Diver_Int = Drivers.Considers_Driver_Interaction,
    Observation_Data = Input.Model_Uses_Obersvation_Data,
    Model_Scalable = Input.Model_Scalable,
    Model_Type = Methods.Methodological_Approach,
    Det_Nature = Methods.Detect_Changes_In_Nature,
    Det_Driver = Methods.Detect_Changes_In_Drivers,
    Det_Attr = Methods.Detect_and_Attribute_Changes_In_Nature_To_Drivers,
    Model_Eval = Methods.Model_Evaluated_On_Calibration_Data,
    Model_Val = Methods.Model_Validated_On_Data_Independent_From_Calibration,
    Spat_Expl = Output.Spatially_Explicit_Output,
    Temp_Dyn = Output.Temporally_Dynamic_Outputs,
    Model_Integr = Integrated_Modelling.Model_work_Jointly_With_Others,
    Model_Feedb = Integrated_Modelling.Joint_Modelling_Reflect_Interactions_And_Feedbacks_Between_Nature_And_Drivers,
    Scenario_Type = Scenario_Analyses.Scenario_Analysis_Capabilities,
    Scenario_Temp = Scenario_Analyses.Temporal_Scale_Model_Projections,
    Scenario_Policy = Scenario_Analyses.Reflect_Policy_Options_In_Simulation,
    Scenario_Mngmt = Scenario_Analyses.Reflect_Management_Options_In_Simulation,
    Scenario_Imp_Pos = Scenario_Analyses.Explore_Nature_Positive_Impacts,
    Scenario_Imp_Neg = Scenario_Analyses.Explore_Nature_Negative_Impacts,
    KMGBF_Goal = KMGBF_Relevance.KMGBF_Goals,
    KMGBF_Target = KMGBF_Relevance.KMGBF_Targets,
    NFF_Inp_Intr = NFF_Relevance.Reflect_Intrinsic_Value_Input,
    NFF_Out_Intr = NFF_Relevance.Reflect_Intrinsic_Value_Output,
    NFF_Inp_Inst = NFF_Relevance.Reflect_Instrumental_Value_Input,
    NFF_Out_Inst = NFF_Relevance.Reflect_Instrumental_Value_Output,
    NFF_Inp_Cult = NFF_Relevance.Reflect_Cultural_Value_Input,
    NFF_Out_Cult = NFF_Relevance.Reflect_Cultural_Value_Output)  %>%
  mutate(across(where(is.character), str_trim))





#==============================================================================#
# Coverage of models per driver
x1 <- x %>%
  select(Model, KMGBF_Goal, Driver) %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ",") %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ".") %>%
  rename(Variable = Driver) %>%
  mutate(
    Variable = case_when(Variable == "16" ~ "Driver_Others",
                       Variable == "15"   ~ "Lifestyle",
                       Variable == "14"   ~ "Health",
                       Variable == "13"   ~ "Technology",
                       Variable == "12"   ~ "Values",
                       Variable == "11"   ~ "Governance",
                       Variable == "10"   ~ "Infrastructure",
                       Variable == "9"    ~ "Energy",
                       Variable == "8"    ~ "Economy",
                       Variable == "7"    ~ "Demography",
                       Variable == "6"    ~ "Pollution",
                       Variable == "5"    ~ "IAS",
                       Variable == "4"    ~ "Exploitation",
                       Variable == "3"    ~ "Climate Change",
                       Variable == "2"    ~ "Sea Use",
                       Variable == "1"    ~ "Land Use",
                       
                       TRUE ~ Variable),
    
    Variable = case_when(Variable ==  "Driver_Others"  ~ "Other Drivers",
                         Variable ==  "Lifestyle"      ~ "Indirect Drivers",
                         Variable ==  "Health"         ~ "Indirect Drivers",
                         Variable ==  "Technology"     ~ "Indirect Drivers",
                         Variable ==  "Values"         ~ "Indirect Drivers",
                         Variable ==  "Governance"     ~ "Indirect Drivers",
                         Variable ==  "Infrastructure" ~ "Indirect Drivers",
                         Variable == "Energy"          ~ "Indirect Drivers",
                         Variable == "Economy"         ~ "Indirect Drivers",
                         Variable == "Demography"      ~ "Indirect Drivers",
                         Variable == "Pollution"       ~ "Direct Drivers",
                         Variable == "IAS"             ~ "Direct Drivers",
                         Variable == "Exploitation"    ~ "Direct Drivers",
                         Variable == "Climate Change"  ~ "Direct Drivers",
                         Variable == "Sea Use"         ~ "Direct Drivers",
                         Variable == "Land Use"        ~ "Direct Drivers",
                         
                         TRUE ~ Variable),
    
    KMGBF_Goal = case_when(KMGBF_Goal ==  "4" ~ "Goal B - NCP",
                           KMGBF_Goal ==  "3" ~ "Goal A - Genetic",
                           KMGBF_Goal ==  "2" ~ "Goal A - Species",
                           KMGBF_Goal ==  "1" ~ "Goal A - Ecosystems",
                           
                           
                           TRUE ~ KMGBF_Goal)
    ) %>%
  filter(!is.na(Variable) & !Variable == "") %>%
  distinct() %>%
  group_by(KMGBF_Goal, Variable) %>%
  count() %>%
  mutate(Group = "Driver") %>%
  rename(Category = KMGBF_Goal)




x1.1 <- x %>%
  select(Model, Driver, 
         NFF_Inp_Intr, NFF_Out_Intr,
         NFF_Inp_Inst, NFF_Out_Inst,
         NFF_Inp_Cult, NFF_Out_Cult) %>%
  mutate(
    NFF_Inp_Intr = replace_na(NFF_Inp_Intr, 2),
    NFF_Out_Intr = replace_na(NFF_Out_Intr, 2),
    
    NFF_Inp_Inst = replace_na(NFF_Inp_Inst, 2),
    NFF_Out_Inst = replace_na(NFF_Out_Inst, 2),
    
    NFF_Inp_Cult = replace_na(NFF_Inp_Cult, 2),
    NFF_Out_Cult = replace_na(NFF_Out_Cult, 2),
    
    NFF_intr = paste0(NFF_Inp_Intr,",", NFF_Out_Intr),
    NFF_inst = paste0(NFF_Inp_Inst,",", NFF_Out_Inst),
    NFF_cult = paste0(NFF_Inp_Cult, ",",NFF_Out_Cult)) %>%
  
  select(!NFF_Inp_Intr & !NFF_Out_Intr&
           !NFF_Inp_Inst & !NFF_Out_Inst&
           !NFF_Inp_Cult & !NFF_Out_Cult) %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ",") %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ".") %>%
  rename(Variable = Driver) %>%
  mutate(
    Variable = case_when(Variable == "16" ~ "Driver_Others",
                         Variable == "15"   ~ "Lifestyle",
                         Variable == "14"   ~ "Health",
                         Variable == "13"   ~ "Technology",
                         Variable == "12"   ~ "Values",
                         Variable == "11"   ~ "Governance",
                         Variable == "10"   ~ "Infrastructure",
                         Variable == "9"    ~ "Energy",
                         Variable == "8"    ~ "Economy",
                         Variable == "7"    ~ "Demography",
                         Variable == "6"    ~ "Pollution",
                         Variable == "5"    ~ "IAS",
                         Variable == "4"    ~ "Exploitation",
                         Variable == "3"    ~ "Climate Change",
                         Variable == "2"    ~ "Sea Use",
                         Variable == "1"    ~ "Land Use",
                         
                         TRUE ~ Variable),
    
    Variable = case_when(Variable ==  "Driver_Others"  ~ "Other Drivers",
                         Variable ==  "Lifestyle"      ~ "Indirect Drivers",
                         Variable ==  "Health"         ~ "Indirect Drivers",
                         Variable ==  "Technology"     ~ "Indirect Drivers",
                         Variable ==  "Values"         ~ "Indirect Drivers",
                         Variable ==  "Governance"     ~ "Indirect Drivers",
                         Variable ==  "Infrastructure" ~ "Indirect Drivers",
                         Variable == "Energy"          ~ "Indirect Drivers",
                         Variable == "Economy"         ~ "Indirect Drivers",
                         Variable == "Demography"      ~ "Indirect Drivers",
                         Variable == "Pollution"       ~ "Direct Drivers",
                         Variable == "IAS"             ~ "Direct Drivers",
                         Variable == "Exploitation"    ~ "Direct Drivers",
                         Variable == "Climate Change"  ~ "Direct Drivers",
                         Variable == "Sea Use"         ~ "Direct Drivers",
                         Variable == "Land Use"        ~ "Direct Drivers",
                         
                         TRUE ~ Variable),
    NFF_intr = case_when( NFF_intr == "4" ~ "Possible",
                          NFF_intr == "3" ~ "Not yet",
                          NFF_intr == "2" ~ "No",
                          NFF_intr == "1" ~ "Yes",
                          
                          TRUE ~ NFF_intr),
    NFF_inst = case_when( NFF_inst == "4" ~ "Possible",
                          NFF_inst == "3" ~ "Not yet",
                          NFF_inst == "2" ~ "No",
                          NFF_inst == "1" ~ "Yes",
                          
                          TRUE ~ NFF_inst),
    NFF_cult = case_when( NFF_cult == "4" ~ "Possible",
                          NFF_cult == "3" ~ "Not yet",
                          NFF_cult == "2" ~ "No",
                          NFF_cult == "1" ~ "Yes",
                          
                          TRUE ~ NFF_cult)) %>%
  filter(!is.na(Variable) & !Variable == "") %>%
  pivot_longer(!c(Model, Variable), names_to = "NFF_value", values_to = "NFF") %>%
  filter(NFF == "Yes") %>%
  select(!NFF) %>%
  distinct() %>%  
  group_by(NFF_value, Variable) %>%
  
  count() %>%
 
  mutate(Group = "Driver") %>%
  rename(Category = NFF_value) %>%
  mutate(
    Category = case_when(Category == "NFF_cult" ~ "NFF (cultural)",
                         Category == "NFF_inst"   ~ "NFF (instrumental)",
                         Category == "NFF_intr"   ~ "NFF (intrinsic)",
                         
                         TRUE ~ Category))



  
x2 <- x %>%
  select(Model, KMGBF_Goal, NCP) %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ",") %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ".") %>%
  rename(Variable = NCP) %>%
  mutate(KMGBF_Goal = case_when(KMGBF_Goal ==  "4" ~ "Goal B - NCP",
                           KMGBF_Goal ==  "3"      ~ "Goal A - Genetic",
                           KMGBF_Goal ==  "2"      ~ "Goal A - Species",
                           KMGBF_Goal ==  "1"      ~ "Goal A - Ecosystems",
                           
                           
                           TRUE ~ KMGBF_Goal),
    
    Variable = case_when( Variable == "18" ~  "Maintenance of Options",
                     Variable == "17"      ~  "Non-Material",
                     Variable == "16"      ~  "Non-Material",
                     Variable == "15"      ~  "Non-Material",
                     Variable == "14"      ~  "Material",
                     Variable == "13"      ~  "Material",
                     Variable == "12"      ~  "Material",
                     Variable == "11"      ~  "Material",
                     Variable == "10"      ~  "Regulating",
                     Variable == "9"       ~  "Regulating",
                     Variable == "8"       ~  "Regulating",
                     Variable == "7"       ~  "Regulating",
                     Variable == "6"       ~  "Regulating",
                     Variable == "5"       ~  "Regulating",
                     Variable == "4"       ~  "Regulating",
                     Variable == "3"       ~  "Regulating",
                     Variable == "2"       ~  "Regulating",
                     Variable == "1"       ~  "Regulating",
                     
                     TRUE ~ Variable)
    
  ) %>%
  filter(!is.na(Variable) & !Variable == "") %>%
  group_by(KMGBF_Goal, Variable) %>%
  distinct() %>%
  count() %>%
  mutate(Group = "NCP") %>%
  rename(Category = KMGBF_Goal)


x2.1 <- x %>%
  select(Model, NCP, 
         NFF_Inp_Intr, NFF_Out_Intr,
         NFF_Inp_Inst, NFF_Out_Inst,
         NFF_Inp_Cult, NFF_Out_Cult) %>%
  mutate(
    NFF_Inp_Intr = replace_na(NFF_Inp_Intr, 2),
    NFF_Out_Intr = replace_na(NFF_Out_Intr, 2),
    
    NFF_Inp_Inst = replace_na(NFF_Inp_Inst, 2),
    NFF_Out_Inst = replace_na(NFF_Out_Inst, 2),
    
    NFF_Inp_Cult = replace_na(NFF_Inp_Cult, 2),
    NFF_Out_Cult = replace_na(NFF_Out_Cult, 2),
    
    NFF_intr = paste0(NFF_Inp_Intr,",", NFF_Out_Intr),
    NFF_inst = paste0(NFF_Inp_Inst,",", NFF_Out_Inst),
    NFF_cult = paste0(NFF_Inp_Cult, ",",NFF_Out_Cult)) %>%
  
  select(!NFF_Inp_Intr & !NFF_Out_Intr&
           !NFF_Inp_Inst & !NFF_Out_Inst&
           !NFF_Inp_Cult & !NFF_Out_Cult) %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ",") %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ".") %>%
  rename(Variable = NCP) %>%
  mutate(Variable = case_when( Variable == "18" ~  "Maintenance of Options",
                          Variable == "17"      ~  "Non-Material",
                          Variable == "16"      ~  "Non-Material",
                          Variable == "15"      ~  "Non-Material",
                          Variable == "14"      ~  "Material",
                          Variable == "13"      ~  "Material",
                          Variable == "12"      ~  "Material",
                          Variable == "11"      ~  "Material",
                          Variable == "10"      ~  "Regulating",
                          Variable == "9"       ~  "Regulating",
                          Variable == "8"       ~  "Regulating",
                          Variable == "7"       ~  "Regulating",
                          Variable == "6"       ~  "Regulating",
                          Variable == "5"       ~  "Regulating",
                          Variable == "4"       ~  "Regulating",
                          Variable == "3"       ~  "Regulating",
                          Variable == "2"       ~  "Regulating",
                          Variable == "1"       ~  "Regulating",
                          
                          TRUE ~ Variable),
    NFF_intr = case_when( NFF_intr == "4" ~ "Possible",
                          NFF_intr == "3" ~ "Not yet",
                          NFF_intr == "2" ~ "No",
                          NFF_intr == "1" ~ "Yes",
                          
                          TRUE ~ NFF_intr),
    NFF_inst = case_when( NFF_inst == "4" ~ "Possible",
                          NFF_inst == "3" ~ "Not yet",
                          NFF_inst == "2" ~ "No",
                          NFF_inst == "1" ~ "Yes",
                          
                          TRUE ~ NFF_inst),
    NFF_cult = case_when( NFF_cult == "4" ~ "Possible",
                          NFF_cult == "3" ~ "Not yet",
                          NFF_cult == "2" ~ "No",
                          NFF_cult == "1" ~ "Yes",
                          
                          TRUE ~ NFF_cult)
    
  ) %>%
  filter(!is.na(Variable) & !Variable == "") %>%
  pivot_longer(!c(Model, Variable), names_to = "NFF_value", values_to = "NFF") %>%
  filter(NFF == "Yes") %>%
  select(!NFF) %>%
  distinct() %>%  
  group_by(NFF_value, Variable) %>%
  
  count() %>%
  mutate(Group = "NCP") %>%
  rename(Category = NFF_value)%>%
  mutate(
    Category = case_when(Category == "NFF_cult" ~ "NFF (cultural)",
                         Category == "NFF_inst"   ~ "NFF (instrumental)",
                         Category == "NFF_intr"   ~ "NFF (intrinsic)",
                         
                         TRUE ~ Category))







x3 <- x %>%
  select(Model, KMGBF_Goal, Model_Type) %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ",") %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ".") %>%
  rename(Variable = Model_Type) %>%
  mutate(KMGBF_Goal = case_when(KMGBF_Goal ==  "4" ~ "Goal B - NCP",
                                KMGBF_Goal ==  "3" ~ "Goal A - Genetic",
                                KMGBF_Goal ==  "2" ~ "Goal A - Species",
                                KMGBF_Goal ==  "1" ~ "Goal A - Ecosystems",
                                
                                
                                TRUE ~ KMGBF_Goal),
         
         Variable = case_when( Variable == "5" ~ "Others",
                               Variable == "4" ~ "Hybrid",
                               Variable == "3" ~ "Process-Based",
                               Variable == "2" ~ "Correlative",
                               Variable == "1" ~ "Expert-Based",
           
                               TRUE ~ Variable)
         
  ) %>%
  filter(!is.na(Variable) & !Variable == "") %>%
  group_by(KMGBF_Goal, Variable) %>%
  distinct() %>%
  count() %>%
  mutate(Group = "Model Type") %>%
  rename(Category = KMGBF_Goal)




x3.1 <- x %>%
    select(Model, Model_Type, 
           NFF_Inp_Intr, NFF_Out_Intr,
           NFF_Inp_Inst, NFF_Out_Inst,
           NFF_Inp_Cult, NFF_Out_Cult) %>%
  mutate(
    NFF_Inp_Intr = replace_na(NFF_Inp_Intr, 2),
    NFF_Out_Intr = replace_na(NFF_Out_Intr, 2),
    
    NFF_Inp_Inst = replace_na(NFF_Inp_Inst, 2),
    NFF_Out_Inst = replace_na(NFF_Out_Inst, 2),
    
    NFF_Inp_Cult = replace_na(NFF_Inp_Cult, 2),
    NFF_Out_Cult = replace_na(NFF_Out_Cult, 2),
    
    NFF_intr = paste0(NFF_Inp_Intr,",", NFF_Out_Intr),
    NFF_inst = paste0(NFF_Inp_Inst,",", NFF_Out_Inst),
    NFF_cult = paste0(NFF_Inp_Cult, ",",NFF_Out_Cult)) %>%
  
  select(!NFF_Inp_Intr & !NFF_Out_Intr&
           !NFF_Inp_Inst & !NFF_Out_Inst&
           !NFF_Inp_Cult & !NFF_Out_Cult) %>%

    separate_columns(columns = setdiff(names(.), "Model"), delim = ",") %>%
    separate_columns(columns = setdiff(names(.), "Model"), delim = ".") %>%
    rename(Variable = Model_Type) %>%
    mutate(
            Variable = case_when( Variable == "5" ~ "Others",
                            Variable == "4" ~ "Hybrid",
                            Variable == "3" ~ "Process-Based",
                            Variable == "2" ~ "Correlative",
                            Variable == "1" ~ "Expert-Based",
                            
                            TRUE ~ Variable),
            NFF_intr = case_when( NFF_intr == "4" ~ "Possible",
                                  NFF_intr == "3" ~ "Not yet",
                                  NFF_intr == "2" ~ "No",
                                  NFF_intr == "1" ~ "Yes",
                                  
                                  TRUE ~ NFF_intr),
            NFF_inst = case_when( NFF_inst == "4" ~ "Possible",
                                  NFF_inst == "3" ~ "Not yet",
                                  NFF_inst == "2" ~ "No",
                                  NFF_inst == "1" ~ "Yes",
                                  
                                  TRUE ~ NFF_inst),
            NFF_cult = case_when( NFF_cult == "4" ~ "Possible",
                                  NFF_cult == "3" ~ "Not yet",
                                  NFF_cult == "2" ~ "No",
                                  NFF_cult == "1" ~ "Yes",
                                  
                                  TRUE ~ NFF_cult)
           
    ) %>%
  pivot_longer(!c(Model, Variable), names_to = "NFF_value", values_to = "NFF") %>%
  filter(NFF == "Yes") %>%
  select(!NFF) %>%
  distinct() %>%  
  group_by(NFF_value, Variable) %>%
  
  count() %>%
  mutate(Group = "Model Type") %>%
  rename(Category = NFF_value)%>%
  mutate(
    Category = case_when(Category == "NFF_cult" ~ "NFF (cultural)",
                         Category == "NFF_inst"   ~ "NFF (instrumental)",
                         Category == "NFF_intr"   ~ "NFF (intrinsic)",
                         
                         TRUE ~ Category))




x4 <- x %>%
  select(Model, KMGBF_Goal, Scenario_Type) %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ",") %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ".") %>%
  rename(Variable = Scenario_Type) %>%
  mutate(KMGBF_Goal = case_when(KMGBF_Goal ==  "4" ~ "Goal B - NCP",
                                KMGBF_Goal ==  "3" ~ "Goal A - Genetic",
                                KMGBF_Goal ==  "2" ~ "Goal A - Species",
                                KMGBF_Goal ==  "1" ~ "Goal A - Ecosystems",
                                
                                
                                TRUE ~ KMGBF_Goal),
         
         Variable = case_when(Variable == "5" ~ "Spatial Planning",
                              Variable == "4" ~ "Retrospective",
                              Variable == "3" ~ "Policy Screening",
                              Variable == "2" ~ "Target Seeking",
                              Variable == "1" ~ "Exploratory",
                                
                               TRUE ~ Variable)
         
  ) %>%
  filter(!is.na(Variable) & !Variable == "") %>%
  group_by(KMGBF_Goal, Variable) %>%
  distinct() %>%
  count() %>%
  mutate(Group = "Scenario Type") %>%
  rename(Category = KMGBF_Goal)


x4.1 <- x %>%
  select(Model, Scenario_Type, 
         NFF_Inp_Intr, NFF_Out_Intr,
         NFF_Inp_Inst, NFF_Out_Inst,
         NFF_Inp_Cult, NFF_Out_Cult) %>%
  mutate(
    NFF_Inp_Intr = replace_na(NFF_Inp_Intr, 2),
    NFF_Out_Intr = replace_na(NFF_Out_Intr, 2),
    
    NFF_Inp_Inst = replace_na(NFF_Inp_Inst, 2),
    NFF_Out_Inst = replace_na(NFF_Out_Inst, 2),
    
    NFF_Inp_Cult = replace_na(NFF_Inp_Cult, 2),
    NFF_Out_Cult = replace_na(NFF_Out_Cult, 2),
    
    NFF_intr = paste0(NFF_Inp_Intr,",", NFF_Out_Intr),
    NFF_inst = paste0(NFF_Inp_Inst,",", NFF_Out_Inst),
    NFF_cult = paste0(NFF_Inp_Cult, ",",NFF_Out_Cult)) %>%
  
  select(!NFF_Inp_Intr & !NFF_Out_Intr&
           !NFF_Inp_Inst & !NFF_Out_Inst&
           !NFF_Inp_Cult & !NFF_Out_Cult) %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ",") %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ".") %>%
  rename(Variable = Scenario_Type) %>%
  mutate(
    Variable = case_when(Variable == "5" ~ "Spatial Planning",
                         Variable == "4" ~ "Retrospective",
                         Variable == "3" ~ "Policy Screening",
                         Variable == "2" ~ "Target Seeking",
                         Variable == "1" ~ "Exploratory",
                         
                         TRUE ~ Variable),
    NFF_intr = case_when( NFF_intr == "4" ~ "Possible",
                          NFF_intr == "3" ~ "Not yet",
                          NFF_intr == "2" ~ "No",
                          NFF_intr == "1" ~ "Yes",
                          
                          TRUE ~ NFF_intr),
    NFF_inst = case_when( NFF_inst == "4" ~ "Possible",
                          NFF_inst == "3" ~ "Not yet",
                          NFF_inst == "2" ~ "No",
                          NFF_inst == "1" ~ "Yes",
                          
                          TRUE ~ NFF_inst),
    NFF_cult = case_when( NFF_cult == "4" ~ "Possible",
                          NFF_cult == "3" ~ "Not yet",
                          NFF_cult == "2" ~ "No",
                          NFF_cult == "1" ~ "Yes",
                          
                          TRUE ~ NFF_cult)) %>%
  filter(!is.na(Variable) & !Variable == "") %>%
  pivot_longer(!c(Model, Variable), names_to = "NFF_value", values_to = "NFF") %>%
  filter(NFF == "Yes") %>%
  select(!NFF) %>%
  distinct() %>%  
  group_by(NFF_value, Variable) %>%
  
  count() %>%
  mutate(Group = "Scenario Type") %>%
  rename(Category = NFF_value)%>%
  mutate(
    Category = case_when(Category == "NFF_cult" ~ "NFF (cultural)",
                         Category == "NFF_inst"   ~ "NFF (instrumental)",
                         Category == "NFF_intr"   ~ "NFF (intrinsic)",
                         
                         TRUE ~ Category))



x.all <- x1 %>%
  bind_rows(x1.1, x2, x2.1, x3, x3.1, x4, x4.1) %>%
  mutate(Group = factor(Group, levels = c("Model Type", "Scenario Type", "Driver", "NCP")),
         Variable = factor(Variable, levels = c("Process-Based", "Hybrid", "Correlative", "Expert-Based", "Others",
                                                
                                                "Exploratory", "Policy Screening", "Target Seeking",  "Spatial Planning","Retrospective", 
                                             "Direct Drivers", "Indirect Drivers", "Other Drivers",
                                             
                                             "Regulating",
                                             "Material",
                                             "Non-Material",
                                             "Maintenance of Options")),
         Category = factor(Category, levels = c("Goal A - Genetic",
                           "Goal A - Species",
                           "Goal A - Ecosystems",
                           "Goal B - NCP",
                           NA,
                           "NFF (intrinsic)",
                           "NFF (instrumental)",
                           "NFF (cultural)")))
  


p.fig1 <- ggplot(x.all,
       aes(x = Variable, 
           y = Category, 
           fill = n, 
           color = Category)) +
  geom_point(aes(size = n)) +
  ylab("") + xlab("") +
  scale_color_brewer(palette = "Paired") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits=rev) +
  facet_grid(.~Group, scales = "free_x") +
  guides(
    fill = guide_none(), # Remove the "fill" legend
    color = guide_none(), # Remove the "color" legend
    size = guide_none() #guide_legend(title = "# Studies") # Keep the "size" legend
  ) +
  geom_text(aes(label = n), vjust = -1, size = 5) +  # Add numbers above points
  #theme(axis.text.x = element_text(angle = 45, hjust = 0))
  
  theme_minimal() +
  theme(legend.position = "none",
        #panel.spacing = unit(0, "lines"), 
        strip.background = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0, size = 14),
        panel.spacing = unit(1, "lines"),
        axis.text.y = element_text(size = 14),
        strip.text = element_text(size = 16))
  


ggsave(paste0(res.path,"/","Figure1B.png"), plot = p.fig1, height = 155, width = 310, units = "mm")
ggsave(paste0(res.path,"/","Figure1B.pdf"), plot = p.fig1, height = 155, width = 310, units = "mm")

  
#inst 40
#intr 39
#cult 15




############################################################
# Gephy data


labels.goals <- x %>%
  select(Model, KMGBF_Goal) %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ",") %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ".")
mutate(KMGBF_Goal = case_when(KMGBF_Goal ==  "4" ~ "Goal B - NCP",
                              KMGBF_Goal ==  "3" ~ "Goal A - Genetic",
                              KMGBF_Goal ==  "2" ~ "Goal A - Species",
                              KMGBF_Goal ==  "1" ~ "Goal A - Ecosystems",
                              
                              TRUE ~ KMGBF_Goal)) %>%
  select(KMGBF_Goal, Model) %>%
  rename(Source = KMGBF_Goal,
         Target = Model)


labels.targets <- x %>%
  select(Model, KMGBF_Target) %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ",") %>%
  separate_columns(columns = setdiff(names(.), "Model"), delim = ".") %>%
  filter(KMGBF_Target != "9" &
           KMGBF_Target != "10" &
           KMGBF_Target != "11" &
           KMGBF_Target != "12" &
           KMGBF_Target != "13" &
           KMGBF_Target != "14" &
           KMGBF_Target != "15" &
           KMGBF_Target != "16" &
           KMGBF_Target != "17" &
           KMGBF_Target != "18" &
           KMGBF_Target != "19" &
           KMGBF_Target != "20" &
           KMGBF_Target != "21" &
           KMGBF_Target != "22" &
           KMGBF_Target != "23" &
           KMGBF_Target != "1-3" &
           KMGBF_Target != "" ) %>%
  mutate(KMGBF_Target = case_when(KMGBF_Target ==  "8" ~ "Target 8",
                                  KMGBF_Target ==  "7" ~ "Target 7",
                                  KMGBF_Target ==  "6" ~ "Target 6",
                                  KMGBF_Target ==  "5" ~ "Target 5",
                                  KMGBF_Target ==  "4" ~ "Target 4",
                                  KMGBF_Target ==  "3" ~ "Target 3",
                                  KMGBF_Target ==  "2" ~ "Target 2",
                                  KMGBF_Target ==  "1" ~ "Target 1",
                                
                                TRUE ~ KMGBF_Target)) %>%
  select(KMGBF_Target, Model) %>%
  rename(Source = KMGBF_Target,
         Target = Model)



write.csv(labels.goals, paste0(data.path, "/", "Gephy-labels-goals.csv"), row.names = F)
write.csv(labels.targets, paste0(data.path, "/", "Gephy-labels-targets.csv"), row.names = F)
  
