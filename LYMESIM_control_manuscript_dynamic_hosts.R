# Recoding of Mount, Haile and Daniels: Simulation of Blacklegged Tick (Acari: Ixodidae)
# Population Dynamics and Transmission of Borrelia burgdorferi
# J. Med. Entomol. 34(4): 461-484 (1997)
rm(list=ls())

core_dir <- "/Users/hollygaff/Dropbox/1_tick_research/LYMESIM/Working_Folder/"

#Locations = c("CARY", "NFLK","ITAS")
#Locations = c("CARY")
Locations = c("NFLK")
#Locations = c("ITAS")

#Treatments = c("1","2","3","4","5","13","14","15","23","24","25","134","234","1345", "2345")
#Treatments = c("2", "3", "5", "23", "25", "35","235")
Treatments = c("0")
#Treatments = c("5")
#Treatments = c("6", "7", "8", "9")

# Treatment 6: Bait box all 6 years, Acaricide in year 1 only
# Treatment 7: Acaricide for 4 years, 4-poster all years
# Treatment 8: Bait box for 4 years, 4-poster all years
# Treatment 9: Acaricide in year 1 only, bait boxes for 4 years, 4-poster all years

#YearsControl = c(6)
#YearsControl = c(0, 1, 2)
YearsControl = c(0)

# Note that effectiveness is really 1-value
#Effectiveness = c(1,.8,.6,.4,.2,.0)
#Effectiveness = c(1,.9,.8,.7,.6,.5,.4,.3,.2,.1,.0)
#Effectiveness = c(1.0, 0.75, 0.5,0)
#Effectiveness = c(1.0, 0.25, 0.5,0)
Effectiveness = c(0)

# set this to 1 to use long term average for all years
weather_only_long_term = 0

#set this to 0 to vary according to vector given for WFM density
WFM_constant = 1

# IMPORTANT NOTE: 
# if varying WFM or using yearly data, can only run for 10 years after burn in period 
# or you must redefine year array
WFM_varying = c(37.2, 9.5, 9.1, 13.9, 41.5, 22.3, 3.8, 9.9, 19.9, 40.0)

# Maximum number of weeks in host-seeking activity for all life stages
max_life_span = 80

# Cost (in weeks) for a week of unsuccessful questing
quest_cost = 3

# Change the name to the output folder here - directories are created if they don't exist
if(!dir.exists(paste0(core_dir, "Output_Folder_control_2/")))
{
    dir.create(paste0(core_dir, "Output_Folder_control_2/"))
    output_dir = paste0(core_dir, "Output_Folder_control_2/")
}  else 
{
    output_dir = paste0(core_dir, "Output_Folder_control_2/")
}
# This creates subfolders for location(s) 
for (location_i in 1:length(Locations))
{
    if (Locations[location_i] == "CARY" && weather_only_long_term == 0){WFM_constant = 0} # varying WFM density for Cary years
    if(!dir.exists(paste0(output_dir, Locations[location_i], weather_only_long_term)))
    {
        dir.create(paste0(output_dir, Locations[location_i], weather_only_long_term,"/"))
        loc_dir = paste0(output_dir, Locations[location_i], weather_only_long_term,"/")
    } else
    {
        loc_dir = paste0(output_dir, Locations[location_i], weather_only_long_term,"/")
    }
    
        
for(Years in 1:length(YearsControl))
{
  if(!dir.exists(paste0(loc_dir, YearsControl[Years], "_Years_of_Control/")))
  {
    dir.create(paste0(loc_dir, YearsControl[Years],"_Years_of_Control/"))
    year_dir = paste0(loc_dir, YearsControl[Years],"_Years_of_Control/")
  } else 
  {
    year_dir = paste0(loc_dir, YearsControl[Years],"_Years_of_Control/")
  }
  for(Treatment in Treatments)
  {
    if(!dir.exists(paste0(year_dir, Treatment, "_Treatment_Type/")))
    {
      dir.create(paste0(year_dir, Treatment,"_Treatment_Type/"))
    }
  
    weather_dir = paste0(core_dir,"LYMESIM_INPUTS/")
  
    # years to stabilize from initial conditions
    burn_in_years = 10 
    
    # years after intervention
    years_after_control = 10
  
    # number of geographic locations
    num_loc = length(Locations)
  
    # names of locations for file name
    location = c(paste0(Locations))
    
    #number of effectiveness levels for control
    num_effectiveness = length(Effectiveness)

    #weather information
    part2 = "_nldas_weekly_" # middle part of filename
  
    # final part of file name
    year = c("2007-2016_ltmean.txt", "2007.txt", "2008.txt", "2009.txt", "2010.txt", 
              "2011.txt", "2012.txt", "2013.txt", "2014.txt", "2015.txt", "2016.txt")
  
    #start simulation the first week of January        
    start_week = 1
    years_of_control = YearsControl[Years]
    num_years = burn_in_years + years_of_control + years_after_control
    
    # variable for future runs with varying parameters
    num_scenarios = 1
    scenario_i = 1
    
   
    for(effectiveness_i in 1:num_effectiveness)
                {
          
                    # switches for various control options
                    WTD_culling = 0 # deer removal
                    WTD_treating = 0 # 4-poster
                    WFM_treating = 0 # bait boxes or tick tubes
                    WFM_vaccine = 0 # treat mice with vaccine Control gives % vaccinated!
                    spraying = 0 # acaricide application 1 = on, 0 = off
                    freq_spraying = 0 # week number(s) for treatment application 
          
                    switch(Treatment,
                        "1"={
                            WTD_culling = years_of_control # deer removal
                            print("WTD_culling")
                            },
                        "2"={
                            WTD_treating = years_of_control # 4-poster
                            print("WTD_treating")
                            },
                        "3"={
                            WFM_treating = years_of_control # bait boxes or tick tubes
                            print("WFM_treating")
                            },
                        "4"={
                            WFM_vaccine = years_of_control # treat mice with vaccine Control gives % vaccinated!
                            print("WFM_vaccine")
                            },
                        "5"={
                            spraying = years_of_control # treat mice with vaccine Control gives % vaccinated!
                            print("Spraying")
                            },
                        "13"={
                            WTD_culling = years_of_control # deer removal
                            WFM_treating = years_of_control # bait boxes or tick tubes
                            print("WTD_culling and WFM_treating")
                            },
                        "14"={
                            WTD_culling = years_of_control # deer removal
                            WFM_vaccine = years_of_control # treat mice with vaccine Control gives % vaccinated!
                            print("WTD_culling and WFM_vaccine")
                            },
                        "15"={
                            WTD_culling = years_of_control # deer removal
                            spraying = years_of_control # apply acaricide
                            print("WTD_culling and Spraying")
                            },
                        "23"={
                            WTD_treating = years_of_control # 4-poster
                            WFM_treating = years_of_control # bait boxes or tick tubes
                            print("WTD_treating and WFM_treating")
                            },
                        "24"={
                            WTD_treating = years_of_control # 4-poster
                            WFM_vaccine = years_of_control # treat mice with vaccine Control gives % vaccinated!
                            print("WTD_treating and WFM_vaccine")
                            },
                        "25"={
                            WTD_treating = years_of_control # 4-poster
                            spraying = years_of_control # apply acaricide
                            print("WTD_treating and Spraying")
                            },
                        "35"={
                            WFM_treating = years_of_control # bait boxes
                            spraying = years_of_control # apply acaricide
                            print("WTD_treating and Spraying")
                            },
                        "235"={
                            WTD_treating = years_of_control # 4-poster
                            WFM_treating = years_of_control # bait boxes 
                            spraying = years_of_control # apply acaricide
                            print("WTD_treating and Spraying")
                            },
                        "134"={
                            WTD_culling = years_of_control # deer removal
                            WFM_treating = years_of_control # bait boxes or tick tubes
                            WFM_vaccine = years_of_control # treat mice with vaccine Control gives % vaccinated!
                            print("WTD_culling, WFM_treating, WFM_vaccine")
                            },
                        "234"={
                            WTD_treating = years_of_control # 4-poster
                            WFM_treating = years_of_control # bait boxes or tick tubes
                            WFM_vaccine = years_of_control # treat mice with vaccine Control gives % vaccinated!
                            print("WTD_treating, WFM_treating, WFM_vaccine")
                        },
                        "6"={
                            WFM_treating = 6 # bait boxes 
                            spraying = 1 # apply acaricide
                            print("BB all years, spraying year 1")
                        },
                        "7"={
                            WTD_treating = 6 # bait boxes 
                            spraying = 4 # apply acaricide
                            print("4-poster all years, spraying years 1-4")
                        },
                        "8"={
                            WFM_treating = 4 # bait boxes 
                            WTD_treating = 6 # bait boxes 
                            print("BB 4 years, 4-poster all years")
                        },
                        "9"={
                            WFM_treating = 4 # bait boxes 
                            WTD_treating = 6 # bait boxes 
                            spraying = 1 # apply acaricide
                            print("BB 4 years, 4-poster all, spraying year 1")
                        }
                    )
          
# Treatment 6: Bait box all 6 years, Acaricide in year 1 only
# Treatment 7: Acaricide for 4 years, 4-poster all years
# Treatment 8: Bait box for 4 years, 4-poster all years
# Treatment 9: Acaricide in year 1 only, bait boxes for 4 years, 4-poster all years

                    #Effectiveness of Treatment
                    control_value = Effectiveness[effectiveness_i] #array(Effectiveness, dim = c(1))

                    #set up control information
                    WTD_culling_control = array(1.0, dim = c(num_years)) # 1.0 is no intervention
                    if (WTD_culling)
                    {
                        WTD_culling_control[(burn_in_years + 1): (burn_in_years + WTD_culling)] = control_value
                    }
    
                    WTD_treating_control = array(1.0, dim = c(num_years)) # 1.0 is no intervention
                    if (WTD_treating)
                    {
                        WTD_treating_control[(burn_in_years + 1): (burn_in_years + WTD_treating)] = control_value
                    }
    
                    WFM_vaccine_control = array(1.0, dim = c(num_years)) # 1.0 is no intervention
                    if (WFM_vaccine)
                    {
                        WFM_vaccine_control[(burn_in_years + 1): (burn_in_years + WFM_vaccine)] = control_value
                    }
    
                    WFM_treating_control = array(1.0, dim = c(num_years)) # 1.0 is no intervention
                    if (WFM_treating)
                    {
                        WFM_treating_control[(burn_in_years + 1): (burn_in_years + WFM_treating)] = control_value
                    }

                    spraying_control = array(1.0, dim = c(num_years)) # 1.0 is no intervention
                    if (spraying)
                    {
                        spraying_control[(burn_in_years + 1): (burn_in_years + spraying)] = control_value
                    }

    total_weeks = num_years * 52
    DIN = array(0, dim = c(num_loc, num_scenarios,total_weeks))
    DIN_OH = array(0, dim = c(num_loc, num_scenarios,total_weeks))
    DIN_Surplus = array(0, dim = c(num_loc, num_scenarios, total_weeks))

    DON = array(0, dim = c(num_loc, num_scenarios,total_weeks))
    DON_OH = array(0, dim = c(num_loc, num_scenarios,total_weeks))
    DON_Surplus = array(0, dim = c(num_loc, num_scenarios, total_weeks))
    
    num_tick_steps = 75 # for monte carlo simulation
    num_sims = 1000 # for monte carlo simulation
    host_infected = mat.or.vec(num_tick_steps,num_sims)
    perc_trans = mat.or.vec(1,num_tick_steps)
    
    for (i in 1:num_tick_steps)
    {
      for (sim in 1:num_sims)
      {
        num_infected_ticks = i*10
        y = sample(c(1:100), num_infected_ticks, replace = TRUE)
        host_infected[i,sim] = length(unique(y))
      }
      perc_trans[i] = mean(host_infected[i,])
    }
    
    # this value can be used with the ITH calculation each time step
    
    S_E = 0.0 # egg survival during incubation
    
    CDW_E = 110.0 #110.0 # cumulative degree weeks above 6 for eggs
    CDW_L = 58.0 # cumulative degree weeks above 6 for larvae
    CDW_N = 81.0 # cumulative degree weeks above 6 for nymphs
    CDW_A = 28.0 # cumulative degree weeks above 6 for engorged females
    
    DT_E = 6.0 # critical threshold for eggs to larva
    DT_L = 6.0 # critical threshold for engorged larve to HSN
    DT_N = 6.0 # critical threshold for engorged nymphs to HSA
    DT_A = 6.0 # critical threshold for engorged females to lay eggs
   
    S1_L = 0.0 # initial survival of free-living larvae
    S1_N = 0.0 # initial survival of free-living nymphs
    S1_A = 0.0 # initial survival of free-living adults
   
    S2_L = 0.0 # final survival of free-living larvae
    S2_N = 0.0 # final survival of free-living nymphs
    S2_A = 0.0 # final survival of free-living adults
    
    H_L = 0.0 # host finding rates for larvae
    H_N = 0.0 # host finding rates for nymphs
    H_A = 0.0 # host finding rates for adults
    
    SD_L = 0.0 # density dependant survival rates on host for larvae
    SD_N = 0.0 # density dependant survival rates on host for nymphs
    SD_A = 0.0 # density dependant survival rates on host for adults
    
    SE_L = 0.0 # survival rates for engorged larvae
    SE_N = 0.0 # survival rates for engorged nymphs
    SE_A = 0.0 # survival rates for engorged adults
    
    Egg_Rate = 0 # number of eggs per female
    
    Base_S_E = c(0.95, 0.94, 0.8)
                 
    Base_S1_L = c(0.965, 0.957, 0.856)
    Base_S2_L = c(0.917, 0.91, 0.814)
    Base_SE_L = c(0.978, 0.974, 0.92)
                 
    Base_S1_N = c(0.999, 0.991, 0.941)
    Base_S2_N = c(0.941, 0.932, 0.885)
    Base_SE_N = c(0.984, 0.978, 0.932)
                 
    Base_S1_A = c(0.999, 0.991, 0.901)
    Base_S2_A = c(0.941, 0.933, 0.886)
    Base_SE_A = c(0.985, 0.982, 0.942)
                
    SRE_S_E_a_sd = -0.000222
    SRE_S_E_b_sd = 0.00133
    SRE_S_E_c_sd = 0.998
    SRE_S_E_a_pi = -0.000408
    SRE_S_E_b_pi = 0.00571
    SRE_S_E_c_pi = 0.98
                
    SRE_S1_L_a_sd = -0.000167
    SRE_S1_L_b_sd = 0.001
    SRE_S1_L_c_sd = 0.998
    SRE_S1_L_a_pi = -0.000306
    SRE_S1_L_b_pi = 0.00429
    SRE_S1_L_c_pi = 0.985
                
    SRE_S2_L_a_sd = -0.000222
    SRE_S2_L_b_sd = 0.00133
    SRE_S2_L_c_sd = 0.998
    SRE_S2_L_a_pi = -0.000408
    SRE_S2_L_b_pi = 0.00571
    SRE_S2_L_c_pi = 0.98
                
    SRE_SE_L_a_sd = -0.000167
    SRE_SE_L_b_sd = 0.001
    SRE_SE_L_c_sd = 0.998
    SRE_SE_L_a_pi = -0.000306
    SRE_SE_L_b_pi = 0.00429
    SRE_SE_L_c_pi = 0.985
                
    SRE_S1_N_a_sd = -0.0000556
    SRE_S1_N_b_sd = 0.000333
    SRE_S1_N_c_sd = 0.999
    SRE_S1_N_a_pi = -0.000102
    SRE_S1_N_b_pi = 0.00143
    SRE_S1_N_c_pi = 0.995
                
    SRE_S2_N_a_sd = -0.000111
    SRE_S2_N_b_sd = 0.000667
    SRE_S2_N_c_sd = 0.999
    SRE_S2_N_a_pi = -0.000204
    SRE_S2_N_b_pi = 0.00286
    SRE_S2_N_c_pi = 0.99
                
    SRE_SE_N_a_sd = -0.0000556
    SRE_SE_N_b_sd = 0.000333
    SRE_SE_N_c_sd = 0.999
    SRE_SE_N_a_pi = -0.000102
    SRE_SE_N_b_pi = 0.00143
    SRE_SE_N_c_pi = 0.995
                
    SRE_S1_A_a_sd = -0.0000556
    SRE_S1_A_b_sd = 0.000333
    SRE_S1_A_c_sd = 0.999
    SRE_S1_A_a_pi = -0.000102
    SRE_S1_A_b_pi = 0.00143
    SRE_S1_A_c_pi = 0.995
                
    SRE_S2_A_a_sd = -0.000111
    SRE_S2_A_b_sd = 0.000667

    SRE_S2_A_c_sd = 0.999
    SRE_S2_A_a_pi = -0.000204
    SRE_S2_A_b_pi = 0.00286
    SRE_S2_A_c_pi = 0.99
                
    SRE_SE_A_a_sd = -0.0000556
    SRE_SE_A_b_sd = 0.000333
    SRE_SE_A_c_sd = 0.999
    SRE_SE_A_a_pi = -0.000102
    SRE_SE_A_b_pi = 0.00143
    SRE_SE_A_c_pi = 0.995
                
    SRE_a_t = 0.999
    SRE_b_t = 0.02094
    SRE_c_t = 0.02088
    SRE_d_t = -0.00136
    SRE_e_t = -0.00137
   
    # new values from Lars 12/17
    WFM_BHFR_Immature_a = 0.01 #0.008935476
    SMB_BHFR_Immature_a = 0.001 #0.004467738
    REP_BHFR_Immature_a = 0.005 #0.008935476
    MSM_BHFR_Immature_a = 0.025 #0.017640004
    WTD_BHFR_Immature_a = 0.050 #0.061224475
    ###########
    SHREW_BHFR_Immature_a = 0.01 #0.008935476
    ###########
    MSM_BHFR_Adult_a = 0.025 #0.003430001
    WTD_BHFR_Adult_a = 0.1 #0.132653029
    ###########
    #SHREW_BHFR_Adult_a
    ###########
    
    WFM_EI = 0.0
    SMB_EI = 0.0
    REP_EI = 0.0
    MSM_EI = 0.0
    WTD_EI = 0.0
    SHREW_EI = 0.0
    
    I_WFM_EI = 0.0
    I_SMB_EI = 0.0
    I_REP_EI = 0.0
    I_MSM_EI = 0.0
    I_WTD_EI = 0.0
    I_SHREW_EI = 0.0
    
    #birth and death rates
    WFM_mu = 0.038
    SMB_mu = 0.038
    REP_mu = 0.038
    MSM_mu = 0.0192
    WTD_mu = 0.0096
    ########
    SHREW_mu = 0.0096
    #########
    
    # host to vector transmission rates
    # update from Lars 12/17
    I_WFM_to_TICK =  0.5 #0.7
    I_SHREW_to_TICK = 0.3 #0.5  #0.0
    I_SMB_to_TICK = 0.05 # 0.1
    I_REP_to_TICK = 0.0
    I_MSM_to_TICK = 0.025 # 0.05 #0.1
    I_WTD_to_TICK = 0.0
    ###########
    ###########
    
    # vector to host transmission rates
    TICK_TO_HOST = 0.9 ##
                
    # transstadial transmission
    Egg_to_Larva = 1.0
    Larva_to_Nymph = 1.0
    Nymph_to_Adult = 1.0
    
    # transovarial
    Adult_to_Egg = 0.0
    
    # size conversions
    Larva_equal_adult = 0.0021 # 0.0017
    Nymph_equal_adult = 0.014 # 0.0034
    
    
      save_dir = paste0(output_dir, location[location_i], weather_only_long_term, "/",
                        YearsControl[Years],"_Years_of_Control/",Treatment,"_Treatment_Type/") 

      #Life Table vectors
      #Susceptible ticks
      Eggs = mat.or.vec(total_weeks, total_weeks) # Eggs to hatch at CDW threshold
      Egg_CDW = mat.or.vec(total_weeks, total_weeks)
    
      #Larvae
      HSL = mat.or.vec(max_life_span, total_weeks) # Host Seeking Larvae
      HSL_life_span = mat.or.vec(max_life_span, total_weeks) # max_life_span minus 2 weeks for each week cohort is questing
      LOH = mat.or.vec(1, total_weeks) # Larvae on host
      L_surplus = array(0, dim = c(1, total_weeks)) # Larvae surplus
      EL = mat.or.vec(total_weeks, total_weeks) # engorged larvae, emerge at CDW threshold
      EL_CDW = mat.or.vec(total_weeks, total_weeks)
    
      #Nymph
      HSN = mat.or.vec(max_life_span, total_weeks) # host seeking nymphs
      HSN_life_span = mat.or.vec(max_life_span, total_weeks) # max_life_span minus 2 weeks for each week cohort is questing
      NOH = mat.or.vec(1, total_weeks) # nymphs on host
      N_surplus = array(0, dim = c(1, total_weeks)) # surplus nymphs
      EN = mat.or.vec(total_weeks, total_weeks) # engorged nymphs, emerge at CDW threshold
      EN_CDW = mat.or.vec(total_weeks, total_weeks)
    
      #Adults
      HSA = mat.or.vec(max_life_span, total_weeks) # host seeking adults
      HSA_life_span = mat.or.vec(max_life_span, total_weeks) # max_life_span minus 2 weeks for each week cohort is questing
      AOH = mat.or.vec(2, total_weeks) # adults on host
      A_surplus = array(0, dim = c(1, total_weeks)) # surplus adults
      EA = mat.or.vec(total_weeks, total_weeks) # egg laying females, assume 1:1 ratio
      EA_CDW = mat.or.vec(total_weeks, total_weeks)
    
      #Percent Infected eggs
      I_Eggs = mat.or.vec(total_weeks, total_weeks) # Eggs to hatch at CDW threshold
    
      #Percent Infected Larvae
      I_HSL = mat.or.vec(max_life_span, total_weeks) # Host Seeking Larvae
      I_LOH = array(0, dim = c(1, total_weeks)) # Larvae on host
      I_L_surplus = mat.or.vec(1, total_weeks) # Larvae surplus
      I_EL = mat.or.vec(total_weeks, total_weeks) # engorged larvae, emerge at CDW threshold
    
      #Percent Infected Nymph
      I_HSN = mat.or.vec(max_life_span, total_weeks) # host seeking nymphs
      I_NOH = mat.or.vec(1, total_weeks) # nymphs on host
      I_N_surplus = array(0, dim = c(1, total_weeks)) # surplus nymphs
      I_EN = mat.or.vec(total_weeks, total_weeks) # engorged nymphs, emerge at CDW threshold
    
      #Percent Infected Adults
      I_HSA = mat.or.vec(max_life_span, total_weeks) # host seeking adults
      I_AOH = mat.or.vec(2, total_weeks) # adults on host
      I_A_surplus = array(0, dim = c(1, total_weeks)) # surplus adults
      I_EA = mat.or.vec(total_weeks, total_weeks) # egg laying females, assume 1:1 ratio
    
      # Host burdens by life stage for all weeks of scenario
      WFM_HD_Plot = mat.or.vec(1,total_weeks)
      WFM_Burden = mat.or.vec(3,total_weeks)
      SMB_Burden = mat.or.vec(3,total_weeks)
      REP_Burden = mat.or.vec(3,total_weeks)
      MSM_Burden = mat.or.vec(3,total_weeks)
      WTD_Burden = mat.or.vec(3,total_weeks)
      SHREW_Burden = mat.or.vec(3,total_weeks)
    
      # Host burdens by life stage for all weeks of scenario
      I_WFM_Burden = mat.or.vec(3,total_weeks)
      I_SMB_Burden = mat.or.vec(3,total_weeks)
      I_REP_Burden = mat.or.vec(3,total_weeks)
      I_MSM_Burden = mat.or.vec(3,total_weeks)
      I_WTD_Burden = mat.or.vec(3,total_weeks)
      I_SHREW_Burden = mat.or.vec(3,total_weeks)
    
      temperature_record = mat.or.vec(1,total_weeks) #record of temperatures by simulation week
    
      #percent of hosts that are infected
      I_WFM_HD = mat.or.vec(1,total_weeks)
      I_SMB_HD = mat.or.vec(1,total_weeks)
      I_REP_HD = mat.or.vec(1,total_weeks)
      I_MSM_HD = mat.or.vec(1,total_weeks)
      I_WTD_HD = mat.or.vec(1,total_weeks)
      I_SHREW_HD = mat.or.vec(1,total_weeks)
    
      New_infected_WFM = mat.or.vec(1,total_weeks)
      New_infected_SMB = mat.or.vec(1,total_weeks)
      New_infected_REP = mat.or.vec(1,total_weeks)
      New_infected_MSM = mat.or.vec(1,total_weeks)
      New_infected_WTD = mat.or.vec(1,total_weeks)
      New_infected_SHREW = mat.or.vec(1,total_weeks)
    
      #initialization 
      Eggs[1,1]=300000
      HSL[1,1] = 5000
      HSN[1,1] = 5000
      HSA[1,1] = 5000
      HSL_life_span[1,1] = max_life_span
      HSN_life_span[1,1] = max_life_span
      HSA_life_span[1,1] = max_life_span
      num_cohorts_E = 1
    
      I_WFM_HD[1]=0.25
    
      habitat = c(0.95, 0.05, 0.0) # 1 = Forest, 2 = Ecotone, 3 = Meadow
    
      Host_blood_meal = mat.or.vec(3,6)
      #Preference for each habitat type for each host type
      Host_Habitat = mat.or.vec(3,6)
    
      Host_Habitat[1,1] = 0.6
      Host_Habitat[2,1] = 0.3
      Host_Habitat[3,1] = 0.1

      Host_Habitat[1,2] = 0.6
      Host_Habitat[2,2] = 0.3
      Host_Habitat[3,2] = 0.1

      Host_Habitat[1,3] = 0.6
      Host_Habitat[2,3] = 0.3
      Host_Habitat[3,3] = 0.1

      Host_Habitat[1,4] = 0.6
      Host_Habitat[2,4] = 0.3
      Host_Habitat[3,4] = 0.1

      Host_Habitat[1,5] = 0.6
      Host_Habitat[2,5] = 0.3
      Host_Habitat[3,5] = 0.1

      Host_Habitat[1,6] = 0.6
      Host_Habitat[2,6] = 0.3
      Host_Habitat[3,6] = 0.1
    
      #Adjusted fraction of each life stage by habitat
      Adjusted_habitat = mat.or.vec(3,3)
      WFM_HD = 40.0 # Lars 12/17
      SMB_HD = 40.0 # Lars 12/17
      REP_HD = 10.0 # Lars 12/17
      MSM_HD = 4.0 # Lars 12/17
      WTD_HD = 0.4 # Lars 12/17
      SHREW_HD = 40.0 # Lars 12/17
    
      #maximum number of each life stage for each host type all from Lars 12/17
      WFM_K = c(WFM_HD*100, WFM_HD*20, WFM_HD*0)
      SHREW_K = c(SHREW_HD*75, SHREW_HD*15, SHREW_HD*0)
      SMB_K = c(SMB_HD*15, SMB_HD*3, SMB_HD*0)
      REP_K = c(REP_HD*20, REP_HD*4, REP_HD*0)
      MSM_K = c(MSM_HD*200, MSM_HD*100, MSM_HD*20)
      WTD_K = c(WTD_HD*1000, WTD_HD*500, WTD_HD*100)
    
      #Percent of blood meals from each host type
      Total_meals_L = WFM_K[1] + SMB_K[1] + REP_K[1] + MSM_K[1] + WTD_K[1] + SHREW_K[1]
      Total_meals_N = WFM_K[2] + SMB_K[2] + REP_K[2] + MSM_K[2] + WTD_K[2] + SHREW_K[2]
      Total_meals_A = WFM_K[3] + SMB_K[3] + REP_K[3] + MSM_K[3] + WTD_K[3] + SHREW_K[3]
   
      Host_blood_meal[1,1] =  WFM_K[1]/Total_meals_L
      Host_blood_meal[1,2] =  SMB_K[1]/Total_meals_L
      Host_blood_meal[1,3] =  REP_K[1]/Total_meals_L
      Host_blood_meal[1,4] =  MSM_K[1]/Total_meals_L
      Host_blood_meal[1,5] =  WTD_K[1]/Total_meals_L
      Host_blood_meal[1,6] =  SHREW_K[1]/Total_meals_L
    
      Host_blood_meal[2,1] =  WFM_K[2]/Total_meals_N
      Host_blood_meal[2,2] =  SMB_K[2]/Total_meals_N
      Host_blood_meal[2,3] =  REP_K[2]/Total_meals_N
      Host_blood_meal[2,4] =  MSM_K[2]/Total_meals_N
      Host_blood_meal[2,5] =  WTD_K[2]/Total_meals_N
      Host_blood_meal[2,6] =  SHREW_K[2]/Total_meals_N
    
      Host_blood_meal[3,1] =  WFM_K[3]/Total_meals_A
      Host_blood_meal[3,2] =  SMB_K[3]/Total_meals_A
      Host_blood_meal[3,3] =  REP_K[3]/Total_meals_A
      Host_blood_meal[3,4] =  MSM_K[3]/Total_meals_A
      Host_blood_meal[3,5] =  WTD_K[3]/Total_meals_A
      Host_blood_meal[3,6] =  SHREW_K[3]/Total_meals_A
                        
      for (adjusted_i in 1:3)
      {
        Adjusted_habitat[adjusted_i,1] =
          Host_blood_meal[adjusted_i,1]* (habitat[1]*Host_Habitat[1,1]/
          (habitat[1]*Host_Habitat[1,1]+habitat[2]*Host_Habitat[2,1]+habitat[3]*Host_Habitat[3,1])) +
          Host_blood_meal[adjusted_i,2]*(habitat[1]*Host_Habitat[1,2]/
          (habitat[1]*Host_Habitat[1,2]+habitat[2]*Host_Habitat[2,2]+habitat[3]*Host_Habitat[3,2])) +
          Host_blood_meal[adjusted_i,3]*(habitat[1]*Host_Habitat[1,3]/
          (habitat[1]*Host_Habitat[1,3]+habitat[2]*Host_Habitat[2,3]+habitat[3]*Host_Habitat[3,3])) +
          Host_blood_meal[adjusted_i,4]*(habitat[1]*Host_Habitat[1,4]/
          (habitat[1]*Host_Habitat[1,4]+habitat[2]*Host_Habitat[2,4]+habitat[3]*Host_Habitat[3,4])) +
          Host_blood_meal[adjusted_i,5]*(habitat[1]*Host_Habitat[1,5]/
          (habitat[1]*Host_Habitat[1,5]+habitat[2]*Host_Habitat[2,5]+habitat[3]*Host_Habitat[3,5])) +
          Host_blood_meal[adjusted_i,6]*(habitat[1]*Host_Habitat[1,6]/
          (habitat[1]*Host_Habitat[1,6]+habitat[2]*Host_Habitat[2,6]+habitat[3]*Host_Habitat[3,6]))
      
        Adjusted_habitat[adjusted_i,2] =
          Host_blood_meal[adjusted_i,1]* (habitat[2]*Host_Habitat[2,1]/
          (habitat[1]*Host_Habitat[1,1]+habitat[2]*Host_Habitat[2,1]+habitat[3]*Host_Habitat[3,1])) +
          Host_blood_meal[adjusted_i,2]*(habitat[2]*Host_Habitat[2,2]/
          (habitat[1]*Host_Habitat[1,2]+habitat[2]*Host_Habitat[2,2]+habitat[3]*Host_Habitat[3,2])) +
          Host_blood_meal[adjusted_i,3]*(habitat[2]*Host_Habitat[2,3]/
          (habitat[1]*Host_Habitat[1,3]+habitat[2]*Host_Habitat[2,3]+habitat[3]*Host_Habitat[3,3])) +
          Host_blood_meal[adjusted_i,4]*(habitat[2]*Host_Habitat[2,4]/
          (habitat[1]*Host_Habitat[1,4]+habitat[2]*Host_Habitat[2,4]+habitat[3]*Host_Habitat[3,4])) +
          Host_blood_meal[adjusted_i,5]*(habitat[2]*Host_Habitat[2,5]/
          (habitat[1]*Host_Habitat[1,5]+habitat[2]*Host_Habitat[2,5]+habitat[3]*Host_Habitat[3,5])) +
          Host_blood_meal[adjusted_i,6]*(habitat[2]*Host_Habitat[2,6]/
          (habitat[1]*Host_Habitat[1,6]+habitat[2]*Host_Habitat[2,6]+habitat[3]*Host_Habitat[3,6]))
      
        Adjusted_habitat[adjusted_i,3] =
          Host_blood_meal[adjusted_i,1]* (habitat[3]*Host_Habitat[3,1]/
          (habitat[1]*Host_Habitat[1,1]+habitat[2]*Host_Habitat[2,1]+habitat[3]*Host_Habitat[3,1])) +
          Host_blood_meal[adjusted_i,2]*(habitat[3]*Host_Habitat[3,2]/
          (habitat[1]*Host_Habitat[1,2]+habitat[2]*Host_Habitat[2,2]+habitat[3]*Host_Habitat[3,2])) +
          Host_blood_meal[adjusted_i,3]*(habitat[3]*Host_Habitat[3,3]/
          (habitat[1]*Host_Habitat[1,3]+habitat[2]*Host_Habitat[2,3]+habitat[3]*Host_Habitat[3,3])) +
          Host_blood_meal[adjusted_i,4]*(habitat[3]*Host_Habitat[3,4]/
          (habitat[1]*Host_Habitat[1,4]+habitat[2]*Host_Habitat[2,4]+habitat[3]*Host_Habitat[3,4])) +
          Host_blood_meal[adjusted_i,5]*(habitat[3]*Host_Habitat[3,5]/
          (habitat[1]*Host_Habitat[1,5]+habitat[2]*Host_Habitat[2,5]+habitat[3]*Host_Habitat[3,5])) +
          Host_blood_meal[adjusted_i,6]*(habitat[3]*Host_Habitat[3,6]/
          (habitat[1]*Host_Habitat[1,6]+habitat[2]*Host_Habitat[2,6]+habitat[3]*Host_Habitat[3,6]))
       }
    
       year_count = 1
       weather_year = 1
       WFM_year = 1
       # start with long-term average data
       Weekly_Weather = read.csv(paste0(weather_dir, location[location_i], part2, year[1]))
       # hold variables for control reset
       WTD_K_orig = WTD_K # hold original carrying capacity
       WFM_K_orig = WFM_K # hold original carrying capacity
       WTD_treating_effect = 1.0
       WFM_treating_effect = 1.0
       WFM_vaccine_effect = 1.0
       spraying_death = 1.0
                        
       for (week_i in 1:(total_weeks-2))
       {
          cal_week = (week_i + start_week)%%52 + 1
          WFM_HD_Plot[1,week_i]=WFM_HD
          spraying_death = 1.0 # reset here each week
      
          if (cal_week == 1 ) #new year, read in new weather and update control options
          {
            year_count = year_count + 1 # advance year count
            
             # check to see if WFM_HD changes
             if (WFM_constant == 0)
             {
              if (year_count > burn_in_years) # simulation runs for burn_in_years before starting with yearly data
              {
                WFM_HD = WFM_varying[WFM_year]
                WFM_year = WFM_year + 1
                print(WFM_HD)
              }
             }
                          
            if (WTD_treating)
            {
              WTD_treating_effect = WTD_treating_control[year_count]
            }
                                
            if (WFM_treating)
            {
              WFM_treating_effect = WFM_treating_control[year_count]
            }
                                
            if (WFM_vaccine)
            {
              WFM_vaccine_effect = WFM_vaccine_control[year_count]
            }
        
            # check to see if WFM_HD changes
            if (WFM_constant == 0)
            {
              if (year_count > burn_in_years) # simulation runs for burn_in_years before starting with yearly data
              {
                WFM_HD = WFM_varying[WFM_year]
                WFM_year = WFM_year + 1
                print(WFM_HD)
              }
            }
            
             # check to see if weather changes
             if (weather_only_long_term == 0)
             {
                if (year_count > burn_in_years) # simulation runs for burn_in_years before starting with yearly data
                {
                  weather_year = weather_year + 1
                  Weekly_Weather = read.csv(paste0(weather_dir, location[location_i], part2, year[weather_year]))
                }
              }
          }
          
          if (cal_week > 15 && cal_week < 26 && spraying) # weeks for spraying and spraying active
          {
            spraying_death = spraying_control[year_count]
            print("Spraying this week")
          }
      
            temperature_i = Weekly_Weather$MeanTemp[cal_week]
            if (temperature_i < 0.0)
            {
              temperature_i = 0.0
            }
                            
            temperature_record[week_i] = temperature_i
            sat_def_i = Weekly_Weather$SatDef[cal_week]
            pi_i = Weekly_Weather$PrecipIndex[cal_week]
            day_length_i = Weekly_Weather$DayLength[cal_week]
            avg_day_length = mean(Weekly_Weather$DayLength)
            sd_day_length = sqrt(var(Weekly_Weather$DayLength))
            max_day_length = max(Weekly_Weather$DayLength)
      
            crit_day_length = max_day_length-sd_day_length
    
            #maximum number of each life stage for each host type all from Lars 12/17
            WFM_K = c(WFM_HD*100, WFM_HD*20, WFM_HD*0)
            SHREW_K = c(SHREW_HD*75, SHREW_HD*15, SHREW_HD*0)
            SMB_K = c(SMB_HD*15, SMB_HD*3, SMB_HD*0)
            REP_K = c(REP_HD*20, REP_HD*4, REP_HD*0)
            MSM_K = c(MSM_HD*200, MSM_HD*100, MSM_HD*20)
            WTD_K = c(WTD_HD*1000, WTD_HD*500, WTD_HD*100)
            
            if (WTD_culling)
            {
              WTD_K = WTD_K_orig * WTD_culling_control[year_count]
            }
                  
    
            #Percent of blood meals from each host type
            Total_meals_L = WFM_K[1] + SMB_K[1] + REP_K[1] + MSM_K[1] + WTD_K[1] + SHREW_K[1]
            Total_meals_N = WFM_K[2] + SMB_K[2] + REP_K[2] + MSM_K[2] + WTD_K[2] + SHREW_K[2]
            Total_meals_A = WFM_K[3] + SMB_K[3] + REP_K[3] + MSM_K[3] + WTD_K[3] + SHREW_K[3]
    
            Host_blood_meal[1,1] =  WFM_K[1]/Total_meals_L
            Host_blood_meal[1,2] =  SMB_K[1]/Total_meals_L
            Host_blood_meal[1,3] =  REP_K[1]/Total_meals_L
            Host_blood_meal[1,4] =  MSM_K[1]/Total_meals_L
            Host_blood_meal[1,5] =  WTD_K[1]/Total_meals_L
            Host_blood_meal[1,6] =  SHREW_K[1]/Total_meals_L
    
            Host_blood_meal[2,1] =  WFM_K[2]/Total_meals_N
            Host_blood_meal[2,2] =  SMB_K[2]/Total_meals_N
            Host_blood_meal[2,3] =  REP_K[2]/Total_meals_N
            Host_blood_meal[2,4] =  MSM_K[2]/Total_meals_N
            Host_blood_meal[2,5] =  WTD_K[2]/Total_meals_N
            Host_blood_meal[2,6] =  SHREW_K[2]/Total_meals_N
    
            Host_blood_meal[3,1] =  WFM_K[3]/Total_meals_A
            Host_blood_meal[3,2] =  SMB_K[3]/Total_meals_A
            Host_blood_meal[3,3] =  REP_K[3]/Total_meals_A
            Host_blood_meal[3,4] =  MSM_K[3]/Total_meals_A
            Host_blood_meal[3,5] =  WTD_K[3]/Total_meals_A
            Host_blood_meal[3,6] =  SHREW_K[3]/Total_meals_A
                        
            for (adjusted_i in 1:3)
            {
              Adjusted_habitat[adjusted_i,1] =
                Host_blood_meal[adjusted_i,1]* (habitat[1]*Host_Habitat[1,1]/
                (habitat[1]*Host_Habitat[1,1]+habitat[2]*Host_Habitat[2,1]+habitat[3]*Host_Habitat[3,1])) +
                Host_blood_meal[adjusted_i,2]*(habitat[1]*Host_Habitat[1,2]/
                (habitat[1]*Host_Habitat[1,2]+habitat[2]*Host_Habitat[2,2]+habitat[3]*Host_Habitat[3,2])) +
                Host_blood_meal[adjusted_i,3]*(habitat[1]*Host_Habitat[1,3]/
                (habitat[1]*Host_Habitat[1,3]+habitat[2]*Host_Habitat[2,3]+habitat[3]*Host_Habitat[3,3])) +
                Host_blood_meal[adjusted_i,4]*(habitat[1]*Host_Habitat[1,4]/
                (habitat[1]*Host_Habitat[1,4]+habitat[2]*Host_Habitat[2,4]+habitat[3]*Host_Habitat[3,4])) +
                Host_blood_meal[adjusted_i,5]*(habitat[1]*Host_Habitat[1,5]/
                (habitat[1]*Host_Habitat[1,5]+habitat[2]*Host_Habitat[2,5]+habitat[3]*Host_Habitat[3,5])) +
                Host_blood_meal[adjusted_i,6]*(habitat[1]*Host_Habitat[1,6]/
                (habitat[1]*Host_Habitat[1,6]+habitat[2]*Host_Habitat[2,6]+habitat[3]*Host_Habitat[3,6]))
      
              Adjusted_habitat[adjusted_i,2] =
                Host_blood_meal[adjusted_i,1]* (habitat[2]*Host_Habitat[2,1]/
                (habitat[1]*Host_Habitat[1,1]+habitat[2]*Host_Habitat[2,1]+habitat[3]*Host_Habitat[3,1])) +
                Host_blood_meal[adjusted_i,2]*(habitat[2]*Host_Habitat[2,2]/
                (habitat[1]*Host_Habitat[1,2]+habitat[2]*Host_Habitat[2,2]+habitat[3]*Host_Habitat[3,2])) +
                Host_blood_meal[adjusted_i,3]*(habitat[2]*Host_Habitat[2,3]/
                (habitat[1]*Host_Habitat[1,3]+habitat[2]*Host_Habitat[2,3]+habitat[3]*Host_Habitat[3,3])) +
                Host_blood_meal[adjusted_i,4]*(habitat[2]*Host_Habitat[2,4]/
                (habitat[1]*Host_Habitat[1,4]+habitat[2]*Host_Habitat[2,4]+habitat[3]*Host_Habitat[3,4])) +
                Host_blood_meal[adjusted_i,5]*(habitat[2]*Host_Habitat[2,5]/
                (habitat[1]*Host_Habitat[1,5]+habitat[2]*Host_Habitat[2,5]+habitat[3]*Host_Habitat[3,5])) +
                Host_blood_meal[adjusted_i,6]*(habitat[2]*Host_Habitat[2,6]/
                (habitat[1]*Host_Habitat[1,6]+habitat[2]*Host_Habitat[2,6]+habitat[3]*Host_Habitat[3,6]))
      
              Adjusted_habitat[adjusted_i,3] =
                Host_blood_meal[adjusted_i,1]* (habitat[3]*Host_Habitat[3,1]/
                (habitat[1]*Host_Habitat[1,1]+habitat[2]*Host_Habitat[2,1]+habitat[3]*Host_Habitat[3,1])) +
                Host_blood_meal[adjusted_i,2]*(habitat[3]*Host_Habitat[3,2]/
                (habitat[1]*Host_Habitat[1,2]+habitat[2]*Host_Habitat[2,2]+habitat[3]*Host_Habitat[3,2])) +
                Host_blood_meal[adjusted_i,3]*(habitat[3]*Host_Habitat[3,3]/
                (habitat[1]*Host_Habitat[1,3]+habitat[2]*Host_Habitat[2,3]+habitat[3]*Host_Habitat[3,3])) +
                Host_blood_meal[adjusted_i,4]*(habitat[3]*Host_Habitat[3,4]/
                (habitat[1]*Host_Habitat[1,4]+habitat[2]*Host_Habitat[2,4]+habitat[3]*Host_Habitat[3,4])) +
                Host_blood_meal[adjusted_i,5]*(habitat[3]*Host_Habitat[3,5]/
                (habitat[1]*Host_Habitat[1,5]+habitat[2]*Host_Habitat[2,5]+habitat[3]*Host_Habitat[3,5])) +
                Host_blood_meal[adjusted_i,6]*(habitat[3]*Host_Habitat[3,6]/
                (habitat[1]*Host_Habitat[1,6]+habitat[2]*Host_Habitat[2,6]+habitat[3]*Host_Habitat[3,6]))
              }
    
                        
              # assuming all hosts are born susceptible
              # assuming host populations are constant
              # simply reduce the percent of hosts infected by the turnover rate
              #percent of hosts that are infected
              I_WFM_HD[week_i+1] = I_WFM_HD[week_i]*(1-WFM_mu)
              I_SMB_HD[week_i+1] = I_SMB_HD[week_i]*(1-SMB_mu)
              I_REP_HD[week_i+1] = I_REP_HD[week_i]*(1-REP_mu)
              I_MSM_HD[week_i+1] = I_MSM_HD[week_i]*(1-MSM_mu)
              I_WTD_HD[week_i+1] = I_WTD_HD[week_i]*(1-WTD_mu)
              I_SHREW_HD[week_i+1] = I_SHREW_HD[week_i]*(1-SHREW_mu)
      
              temperature_SRE = temperature_i
      
              SRE_S_E = (((SRE_a_t + SRE_c_t * temperature_SRE + SRE_e_t * temperature_SRE^2) /
                (1+SRE_b_t*temperature_SRE + SRE_d_t * temperature_SRE^2))*
                (SRE_S_E_a_sd*sat_def_i^2 + SRE_S_E_b_sd*sat_def_i + SRE_S_E_c_sd)*
                (SRE_S_E_a_pi*pi_i^2 + SRE_S_E_b_pi*pi_i + SRE_S_E_c_pi))
      
              SRE_S1_L = (((SRE_a_t + SRE_c_t * temperature_SRE + SRE_e_t * temperature_SRE^2) /
                (1+SRE_b_t*temperature_SRE + SRE_d_t * temperature_SRE^2))*
                (SRE_S1_L_a_sd*sat_def_i^2 + SRE_S1_L_b_sd*sat_def_i + SRE_S1_L_c_sd)*
                (SRE_S1_L_a_pi*pi_i^2 + SRE_S1_L_b_pi*pi_i + SRE_S1_L_c_pi))
      
              SRE_S2_L = (((SRE_a_t + SRE_c_t * temperature_SRE + SRE_e_t * temperature_SRE^2) /
                (1+SRE_b_t*temperature_SRE + SRE_d_t * temperature_SRE^2))*
                (SRE_S2_L_a_sd*sat_def_i^2 + SRE_S2_L_b_sd*sat_def_i + SRE_S2_L_c_sd)*
                (SRE_S2_L_a_pi*pi_i^2 + SRE_S2_L_b_pi*pi_i + SRE_S2_L_c_pi))
      
              SRE_SE_L = (((SRE_a_t + SRE_c_t * temperature_SRE + SRE_e_t * temperature_SRE^2) /
                (1+SRE_b_t*temperature_SRE + SRE_d_t * temperature_SRE^2))*
                (SRE_SE_L_a_sd*sat_def_i^2 + SRE_SE_L_b_sd*sat_def_i + SRE_SE_L_c_sd)*
                (SRE_SE_L_a_pi*pi_i^2 + SRE_SE_L_b_pi*pi_i + SRE_SE_L_c_pi))
      
              SRE_S1_N = (((SRE_a_t + SRE_c_t * temperature_SRE + SRE_e_t * temperature_SRE^2) / 
                (1+SRE_b_t*temperature_SRE + SRE_d_t * temperature_SRE^2))*
                (SRE_S1_N_a_sd*sat_def_i^2 + SRE_S1_N_b_sd*sat_def_i + SRE_S1_N_c_sd)*
                (SRE_S1_N_a_pi*pi_i^2 + SRE_S1_N_b_pi*pi_i + SRE_S1_N_c_pi))
      
              SRE_S2_N = (((SRE_a_t + SRE_c_t * temperature_SRE + SRE_e_t * temperature_SRE^2) /
                (1+SRE_b_t*temperature_SRE + SRE_d_t * temperature_SRE^2))*
                (SRE_S2_N_a_sd*sat_def_i^2 + SRE_S2_N_b_sd*sat_def_i + SRE_S2_N_c_sd)*
                (SRE_S2_N_a_pi*pi_i^2 + SRE_S2_N_b_pi*pi_i + SRE_S2_N_c_pi))
      
              SRE_SE_N = (((SRE_a_t + SRE_c_t * temperature_SRE + SRE_e_t * temperature_SRE^2) /
                (1+SRE_b_t*temperature_SRE + SRE_d_t * temperature_SRE^2))*
                (SRE_SE_N_a_sd*sat_def_i^2 + SRE_SE_N_b_sd*sat_def_i + SRE_SE_N_c_sd)*
                (SRE_SE_N_a_pi*pi_i^2 + SRE_SE_N_b_pi*pi_i + SRE_SE_N_c_pi))
      
              SRE_S1_A = (((SRE_a_t + SRE_c_t * temperature_SRE + SRE_e_t * temperature_SRE^2) /
                (1+SRE_b_t*temperature_SRE + SRE_d_t * temperature_SRE^2))*
                (SRE_S1_A_a_sd*sat_def_i^2 + SRE_S1_A_b_sd*sat_def_i + SRE_S1_A_c_sd)*
                (SRE_S1_A_a_pi*pi_i^2 + SRE_S1_A_b_pi*pi_i + SRE_S1_A_c_pi))
      
              SRE_S2_A = (((SRE_a_t + SRE_c_t * temperature_SRE + SRE_e_t * temperature_SRE^2) /
                (1+SRE_b_t*temperature_SRE + SRE_d_t * temperature_SRE^2))*
                (SRE_S2_A_a_sd*sat_def_i^2 + SRE_S2_A_b_sd*sat_def_i + SRE_S2_A_c_sd)*
                (SRE_S2_A_a_pi*pi_i^2 + SRE_S2_A_b_pi*pi_i + SRE_S2_A_c_pi))
      
              SRE_SE_A = (((SRE_a_t + SRE_c_t * temperature_SRE + SRE_e_t * temperature_SRE^2) /
                (1+SRE_b_t*temperature_SRE + SRE_d_t * temperature_SRE^2))*
                (SRE_SE_A_a_sd*sat_def_i^2 + SRE_SE_A_b_sd*sat_def_i + SRE_SE_A_c_sd)*
                (SRE_SE_A_a_pi*pi_i^2 + SRE_SE_A_b_pi*pi_i + SRE_SE_A_c_pi))
      
              S_E = (Adjusted_habitat[3,1]*Base_S_E[1] + Adjusted_habitat[3,2]*Base_S_E[2] + Adjusted_habitat[3,3]*Base_S_E[3]) * SRE_S_E
              S1_L = spraying_death*(Adjusted_habitat[3,1]*Base_S1_L[1] + Adjusted_habitat[3,2]*Base_S1_L[2] + Adjusted_habitat[3,3]*Base_S1_L[3]) * SRE_S1_L
              S2_L = spraying_death*(Adjusted_habitat[3,1]*Base_S2_L[1] + Adjusted_habitat[3,2]*Base_S2_L[2] + Adjusted_habitat[3,3]*Base_S2_L[3]) * SRE_S2_L
              #S1_L = (Adjusted_habitat[3,1]*Base_S1_L[1] + Adjusted_habitat[3,2]*Base_S1_L[2] + Adjusted_habitat[3,3]*Base_S1_L[3]) * SRE_S1_L
              #S2_L = (Adjusted_habitat[3,1]*Base_S2_L[1] + Adjusted_habitat[3,2]*Base_S2_L[2] + Adjusted_habitat[3,3]*Base_S2_L[3]) * SRE_S2_L
              SE_L = (Adjusted_habitat[1,1]*Base_SE_L[1] + Adjusted_habitat[1,2]*Base_SE_L[2] + Adjusted_habitat[1,3]*Base_SE_L[3]) * SRE_SE_L

              S1_N = spraying_death*(Adjusted_habitat[1,1]*Base_S1_N[1] + Adjusted_habitat[1,2]*Base_S1_N[2] + Adjusted_habitat[1,3]*Base_S1_N[3]) * SRE_S1_N
              S2_N = spraying_death*(Adjusted_habitat[1,1]*Base_S2_N[1] + Adjusted_habitat[1,2]*Base_S2_N[2] + Adjusted_habitat[1,3]*Base_S2_N[3]) * SRE_S2_N
              #S1_N = (Adjusted_habitat[1,1]*Base_S1_N[1] + Adjusted_habitat[1,2]*Base_S1_N[2] + Adjusted_habitat[1,3]*Base_S1_N[3]) * SRE_S1_N
              #S2_N = (Adjusted_habitat[1,1]*Base_S2_N[1] + Adjusted_habitat[1,2]*Base_S2_N[2] + Adjusted_habitat[1,3]*Base_S2_N[3]) * SRE_S2_N
              SE_N = (Adjusted_habitat[2,1]*Base_SE_N[1] + Adjusted_habitat[2,2]*Base_SE_N[2] + Adjusted_habitat[2,3]*Base_SE_N[3]) * SRE_SE_N
      
              S1_A = spraying_death*(Adjusted_habitat[2,1]*Base_S1_A[1] + Adjusted_habitat[2,2]*Base_S1_A[2] + Adjusted_habitat[2,3]*Base_S1_A[3]) * SRE_S1_A
              S2_A = spraying_death*(Adjusted_habitat[2,1]*Base_S2_A[1] + Adjusted_habitat[2,2]*Base_S2_A[2] + Adjusted_habitat[2,3]*Base_S2_A[3]) * SRE_S2_A
              #S1_A = (Adjusted_habitat[2,1]*Base_S1_A[1] + Adjusted_habitat[2,2]*Base_S1_A[2] + Adjusted_habitat[2,3]*Base_S1_A[3]) * SRE_S1_A
              #S2_A = (Adjusted_habitat[2,1]*Base_S2_A[1] + Adjusted_habitat[2,2]*Base_S2_A[2] + Adjusted_habitat[2,3]*Base_S2_A[3]) * SRE_S2_A
              SE_A = (Adjusted_habitat[3,1]*Base_SE_A[1] + Adjusted_habitat[3,2]*Base_SE_A[2] + Adjusted_habitat[3,3]*Base_SE_A[3]) * SRE_SE_A
      
              # Artificially keeping survival to less than maximum life span.
              S2_L = 0.0 
              S2_N = 0.0 
       
              if (temperature_i > 5.9)
              {
                Fecundity = -24.59*temperature_i^2 + 835.9*temperature_i - 4105.58
              } else {
                Fecundity = 0
              }
      
              if (temperature_i>10.8 && temperature_i<30.2 && sat_def_i < 8.0 && day_length_i > crit_day_length)
              {
                HFRE_immature = #(-0.01*sat_def_i^2+0.001*sat_def_i+0.998)*
                  (-0.0105*temperature_i^2 + 0.4316*temperature_i-3.424)*
                  ((0.03116-0.007615*day_length_i + 0.0004469*day_length_i^2)/
                  (1-0.1374*day_length_i+0.004788*day_length_i^2))^2
              } else {
                HFRE_immature = 0.0
              }
      
              if (temperature_i>0.0 && temperature_i < 20.2)
              {
                HFRE_adult = -0.0095*temperature_i^2+0.19*temperature_i+0.05
              } else {
                HFRE_adult = 0.0
              }
      
              WFM_HFR_L = HFRE_immature * WFM_BHFR_Immature_a*(WFM_HD)^0.515
              SMB_HFR_L = HFRE_immature * SMB_BHFR_Immature_a*(SMB_HD)^0.515
              REP_HFR_L = HFRE_immature * REP_BHFR_Immature_a*(REP_HD)^0.515
              MSM_HFR_L = HFRE_immature * MSM_BHFR_Immature_a*(MSM_HD)^0.515
              WTD_HFR_L = HFRE_immature * WTD_BHFR_Immature_a*(WTD_HD)^0.515
              SHREW_HFR_L = HFRE_immature * SHREW_BHFR_Immature_a*(SHREW_HD)^0.515
      
              WFM_HFR_N = HFRE_immature * WFM_BHFR_Immature_a*(WFM_HD)^0.515
              SMB_HFR_N = HFRE_immature * SMB_BHFR_Immature_a*(SMB_HD)^0.515
              REP_HFR_N = HFRE_immature * REP_BHFR_Immature_a*(REP_HD)^0.515
              MSM_HFR_N = HFRE_immature * MSM_BHFR_Immature_a*(MSM_HD)^0.515
              WTD_HFR_N = HFRE_immature * WTD_BHFR_Immature_a*(WTD_HD)^0.515
              SHREW_HFR_N = HFRE_immature * SHREW_BHFR_Immature_a*(SHREW_HD)^0.515
      
              MSM_HFR_A = HFRE_adult * MSM_BHFR_Adult_a*(MSM_HD)^0.515
              WTD_HFR_A = HFRE_adult * WTD_BHFR_Adult_a*(WTD_HD)^0.515
      
              WFM_EI = 0.0
              SMB_EI = 0.0
              REP_EI = 0.0
              MSM_EI = 0.0
              WTD_EI = 0.0
              ##############
              SHREW_EI = 0.0
              ##############
      
              for (exp_i in 1:9)
              {
                week_before = week_i-exp_i
                if (week_before > 0)
                {
                  if (WFM_HD > 0) 
                  {
                    WFM_EI = WFM_EI + ((WFM_Burden[1,week_before]*Larva_equal_adult+
                      WFM_Burden[2,week_before]*Nymph_equal_adult)/WFM_HD)*0.44^(exp_i-1)
                  } else { 
                    WFM_EI = 0 
                  }
                    if (SMB_HD > 0) 
                    {
                      SMB_EI = SMB_EI + ((SMB_Burden[1,week_before]*Larva_equal_adult+
                        SMB_Burden[2,week_before]*Nymph_equal_adult)/SMB_HD)*0.44^(exp_i-1)
                    } else {
                      SMB_EI = 0
                    }
                    if (REP_HD > 0)
                    {
                      REP_EI = REP_EI + ((REP_Burden[1,week_before]*Larva_equal_adult+
                        REP_Burden[2,week_before]*Nymph_equal_adult)/REP_HD)*0.44^(exp_i-1)
                    } else {
                      REP_EI = 0
                    }
                    if (MSM_HD > 0)
                    {
                      MSM_EI = MSM_EI + ((MSM_Burden[1,week_before]*Larva_equal_adult+
                        MSM_Burden[2,week_before]*Nymph_equal_adult + 
                        MSM_Burden[3,week_before])/MSM_HD)*0.44^(exp_i-1)
                    } else { 
                      MSM_EI = 0
                    }
                    if (WTD_HD > 0) 
                    { 
                      WTD_EI = WTD_EI + ((WTD_Burden[1,week_before]*Larva_equal_adult+
                        WTD_Burden[2,week_before]*Nymph_equal_adult +
                        WTD_Burden[3,week_before])/WTD_HD)*0.44^(exp_i-1)
                    } else {
                      WTD_EI = 0
                    }
                    if (SHREW_HD > 0) 
                      { 
                        SHREW_EI = SHREW_EI + ((SHREW_Burden[1,week_before]*Larva_equal_adult+
                          SHREW_Burden[2,week_before]*Nymph_equal_adult)/SHREW_HD)*0.44^(exp_i-1)
                      } else {
                        SHREW_EI = 0
                      }
                    }
                  }
      
                  if (WFM_EI < 0.15) 
                  { 
                    WFM_SD_L = 0.6 
                    WFM_SD_N = 0.6 
                  } else if (WFM_EI > 0.75) {
                    WFM_SD_L = 0.2
                    WFM_SD_N = 0.2
                  } else {
                    ly1 = 0.6 
                    ly2 = 0.2 
                    x1 = 0.15 
                    x2 = 0.75 
                    WFM_SD_L = ((ly2-ly1)/(x2-x1))*(WFM_EI-x1)+ly1 
                    WFM_SD_N = WFM_SD_L 
                  }
      
                  if (SMB_EI < 0.06) 
                  { 
                    SMB_SD_L = 0.6 
                    SMB_SD_N = 0.6 
                  } else if (SMB_EI > 0.3) { 
                    SMB_SD_L = 0.2 
                    SMB_SD_N = 0.2 
                  } else { 
                    ly1 = 0.6 
                    ly2 = 0.2 
                    x1 = 0.06 
                    x2 = 0.3 
                    SMB_SD_L = ((ly2-ly1)/(x2-x1))*(SMB_EI-x1)+ly1 
                    SMB_SD_N = SMB_SD_L 
                  }
      
                  if (REP_EI < 0.15) 
                  { 
                    REP_SD_L = 0.6 
                    REP_SD_N = 0.6 
                  } else if (REP_EI > 0.75) { 
                    REP_SD_L = 0.2 
                    REP_SD_N = 0.2 
                  } else { 
                    ly1 = 0.6 
                    ly2 = 0.2 
                    x1 = 0.15 
                    x2 = 0.75 
                    REP_SD_L = ((ly2-ly1)/(x2-x1))*(REP_EI-x1)+ly1 
                    REP_SD_N = REP_SD_L 
                  }
      
                  if (MSM_EI < 0.8) 
                  { 
                    MSM_SD_L = 0.6 
                    MSM_SD_N = 0.6 
                    MSM_SD_A = 0.7746 
                  } else if (MSM_EI > 4.0) { 
                    MSM_SD_L = 0.2 
                    MSM_SD_N = 0.2 
                    MSM_SD_A = 0.4474 
                  } else { 
                    ly1 = 0.6 
                    ay1 = 0.7746 
                    ly2 = 0.2 
                    ay2 = 0.4474 
                    x1 = 0.8 
                    x2 = 4.0 
                    MSM_SD_L = ((ly2-ly1)/(x2-x1))*(MSM_EI-x1)+ly1 
                    MSM_SD_N = MSM_SD_L 
                    MSM_SD_A = ((ay2-ay1)/(x2-x1))*(MSM_EI-x1)+ay1 
                  }
      
                  if (WTD_EI < 60) 
                  { 
                    WTD_SD_L = 0.6 
                    WTD_SD_N = 0.6 
                    WTD_SD_A = 0.7746 
                  } else if (WTD_EI > 300) { 
                    WTD_SD_L = 0.2 
                    WTD_SD_N = 0.2 
                    WTD_SD_A = 0.4474 
                  } else { 
                    ly1 = 0.6 
                    ay1 = 0.7746 
                    ly2 = 0.2 
                    ay2 = 0.4474 
                    x1 = 60 
                    x2 = 300 
                    WTD_SD_L = ((ly2-ly1)/(x2-x1))*(WTD_EI-x1)+ly1 
                    WTD_SD_N = WTD_SD_L 
                    WTD_SD_A = ((ay2-ay1)/(x2-x1))*(WTD_EI-x1)+ay1 
                  }
                            
                  if (SHREW_EI < .15) 
                  { 
                    SHREW_SD_L = 0.6 
                    SHREW_SD_N = 0.6 
                  } else if (SHREW_EI > .75) { 
                    SHREW_SD_L = 0.2 
                    SHREW_SD_N = 0.2 
                  } else {  
                    ly1 = 0.6 
                    ly2 = 0.2 
                    x1 = .15 
                    x2 = .75 
                    SHREW_SD_L = ((ly2-ly1)/(x2-x1))*(SHREW_EI-x1)+ly1 
                    SHREW_SD_N = SHREW_SD_L 
                  }
                  dev_delay = 1.0
      
                  #Update all stage classes
                  # eggs have weekly age classes based on week eggs were laid
                  # check cumumlative degree for hatching
                  hold_temperature = 0 # temporary variable
                  for(cohort_i in week_i:1) #cycle through all cohorts to see if ready to hatch
                  { 
                    CDW = 0 # clear out cumulative degree weeks 
                    week_check = week_i - cohort_i + 1 # set backwards through time 
                    if (Eggs[week_check,week_i]>0) 
                    { 
                      hold_temperature <- temperature_record[week_i] # get this weeks temperature
                      if (hold_temperature > DT_E)
                      { # add this week DW to CDW
                        CDW = Egg_CDW[week_check,week_i] + (hold_temperature -DT_E)*dev_delay
                      } else {
                        CDW = Egg_CDW[week_check, week_i]
                      }
                      if (CDW > CDW_E)
                      { # threshold met, eggs hatch
                        I_HSL[1, week_i+1] =  (I_HSL[1,week_i+1]*HSL[1,week_i+1] + # already there
                          Egg_to_Larva * I_Eggs[week_check,week_i])/ #newly joined
                          (HSL[1,week_i+1]+Eggs[week_check,week_i])
                        HSL[1,week_i+1] = HSL[1,week_i+1]+Eggs[week_check,week_i]
                        HSL_life_span[1, week_i+1] = max_life_span # starts with max life span
                        Eggs[week_check, week_i+1] = 0 # all eggs hatch at same time from this cohort
                      } else { #if don't hatch, must survive
                        Eggs[week_check, week_i+1] = Eggs[week_check,week_i]*S_E
                        Egg_CDW[week_check,week_i+1] = CDW
                        I_Eggs[week_check, week_i+1]=I_Eggs[week_check,week_i]
                      }
                    }
                  }
      
                  # check on Host-seeking larvae
                  # note, start from 2 since week 1 just emerged
                  # note, stage max_life_span gets different survival
                  HSL[2, week_i+1]=HSL[1, week_i]*S1_L
                  I_HSL[2, week_i+1]=I_HSL[2,week_i] # pass along percent infected
                  HSL_life_span[2, week_i+1] = spraying_death*HSL_life_span[1,week_i]
      
                  for (HSL_check in 2:max_life_span)
                  { # find host and advance to larvae on host
                    TOTAL_HFR_L = WFM_HFR_L + SMB_HFR_L + REP_HFR_L + MSM_HFR_L + WTD_HFR_L + SHREW_HFR_L
                    if (TOTAL_HFR_L > 1)
                    {
                      stop("Total host finding rate for larvae > 1")
                    }
        
                    Want_host = HSL[HSL_check,week_i]*TOTAL_HFR_L
                    # lose two weeks of life for a week of questing
        
                    Found_WFM = WFM_HFR_L*HSL[HSL_check,week_i]
                    if (WFM_Burden[1, week_i+1] + Found_WFM < WFM_K[1])
                    {
                      WFM_Burden[1,week_i+1]= WFM_Burden[1,week_i+1] + Found_WFM
                    } else {
                      Found_WFM = WFM_K[1] -WFM_Burden[1,week_i+1]
                      WFM_Burden[1, week_i+1] = WFM_K[1]
                    }
                                
                    Found_SMB = SMB_HFR_L*HSL[HSL_check,week_i]
                    if (SMB_Burden[1, week_i+1] + Found_SMB < SMB_K[1])
                    {
                      SMB_Burden[1,week_i+1]= SMB_Burden[1,week_i+1] + Found_SMB
                    } else {
                      Found_SMB = SMB_K[1]-SMB_Burden[1,week_i+1]
                      SMB_Burden[1, week_i+1] = SMB_K[1]
                    }
            
                    Found_REP = REP_HFR_L*HSL[HSL_check,week_i]
                    if (REP_Burden[1, week_i+1] + Found_REP < REP_K[1])
                    {
                      REP_Burden[1,week_i+1]= REP_Burden[1,week_i+1] + Found_REP
                    } else {
                      Found_REP = REP_K[1]-REP_Burden[1,week_i+1]
                      REP_Burden[1, week_i+1] = REP_K[1] 
                    }
            
                    Found_MSM = MSM_HFR_L*HSL[HSL_check,week_i]
                    if (MSM_Burden[1, week_i+1] + Found_MSM < MSM_K[1])
                    {
                      MSM_Burden[1,week_i+1]= MSM_Burden[1,week_i+1] + Found_MSM
                    } else {
                      Found_MSM = MSM_K[1]-MSM_Burden[1,week_i+1]
                      MSM_Burden[1, week_i+1] = MSM_K[1]
                    }
            
                    Found_WTD = WTD_HFR_L*HSL[HSL_check,week_i]
                    if (WTD_Burden[1, week_i+1] + Found_WTD < WTD_K[1])
                    {
                      WTD_Burden[1,week_i+1]= WTD_Burden[1,week_i+1] + Found_WTD
                    } else {
                      Found_WTD = WTD_K[1]-WTD_Burden[1,week_i+1]
                      WTD_Burden[1, week_i+1] = WTD_K[1]
                    }
                                
                    Found_SHREW = SHREW_HFR_L*HSL[HSL_check,week_i]
                    if (SHREW_Burden[1, week_i+1] + Found_SHREW < SHREW_K[1])
                    {
                      SHREW_Burden[1,week_i+1]= SHREW_Burden[1,week_i+1] + Found_SHREW
                    } else {
                      Found_SHREW = SHREW_K[1]-SHREW_Burden[1,week_i+1]
                      SHREW_Burden[1, week_i+1] = SHREW_K[1]
                    }
            
                    Found_host = Found_WFM + Found_SMB + Found_REP + Found_MSM + Found_WTD + Found_SHREW
                                
                    if(Found_host>0)
                    { 
                      I_LOH[1, week_i+1] = (I_LOH[1,week_i+1]*LOH[1,week_i+1] + # number infected from before
                        I_HSL[HSL_check, week_i]*Found_host)/ # new infecteds
                        (LOH[1,week_i+1]+Found_host) # total
                      LOH[1,week_i+1] = LOH[1,week_i+1] + Found_host # total
                    } else {
                      I_LOH[1, week_i+1] = I_LOH[1, week_i+1]
                      LOH[1,week_i+1] = LOH[1,week_i+1]
                    }
                                
                    surplus = Want_host - Found_host
                    if (surplus < 0)
                    { #correct for round-off error
                      surplus = 0
                    }
                                
                    if(surplus>0)
                    {
                      I_L_surplus[1, week_i+1] = (I_L_surplus[1,week_i+1]*L_surplus[1,week_i+1] + # number infected from before
                        I_HSL[HSL_check, week_i]*surplus)/ # new infecteds
                        (L_surplus[1,week_i+1]+surplus) # total
                      L_surplus[1,week_i+1] = L_surplus[1, week_i+1] + surplus
                    } else {
                      I_L_surplus[1, week_i+1] = I_L_surplus[1, week_i+1]
                      L_surplus[1,week_i+1] = L_surplus[1, week_i+1]
                    }
                
                    # else must survive and advance to next stage
                    if (HSL_check == max_life_span)
                    {
                      HSL[HSL_check, week_i+1]=
                      HSL[HSL_check, week_i+1] + 
                        (HSL[HSL_check, week_i]-Found_host - surplus)*S2_L + # ticks that didn't quest 
                        surplus*0 # ticks that quested but failed to find a host 
                      I_HSL[HSL_check, week_i+1]=I_HSL[HSL_check,week_i]
                                    
                      if (Want_host> 0)
                      {  # lose two weeks of life for a week of questing
                        HSL_life_span[HSL_check,week_i+1] = HSL_life_span[HSL_check, week_i] - quest_cost 
                      } else {  
                        HSL_life_span[HSL_check,week_i+1] = HSL_life_span[HSL_check, week_i]  
                      } 
                    } else {
                      if (HSL_check<HSL_life_span[HSL_check, week_i]) 
                      { 
                        HSL[HSL_check+1, week_i+1]= HSL[HSL_check+1, week_i+1] + # ticks already in that bucket 
                          (HSL[HSL_check, week_i]-Found_host - surplus)*S1_L + # ticks that didn't quest 
                          (surplus)*0 # ticks that quested but failed - not currently used, but here in case these have different survival
                        I_HSL[HSL_check+1, week_i+1]=I_HSL[HSL_check,week_i]
                                    
                        if (Want_host> 0)
                        {  # lose two weeks of life for a week of questing
                          HSL_life_span[HSL_check+1,week_i+1] = HSL_life_span[HSL_check, week_i] - quest_cost 
                        } else { 
                          HSL_life_span[HSL_check+1,week_i+1] = HSL_life_span[HSL_check, week_i]  
                        }  
                      }  
                    } 
                  }
      
                  fed_larvae_WFM = WFM_Burden[1,week_i]*WFM_SD_L*WFM_treating_effect
                  fed_larvae_SMB = SMB_Burden[1,week_i]*SMB_SD_L 
                  fed_larvae_REP = REP_Burden[1,week_i]*REP_SD_L 
                  fed_larvae_MSM = MSM_Burden[1,week_i]*MSM_SD_L 
                  fed_larvae_WTD = WTD_Burden[1,week_i]*WTD_SD_L*WTD_treating_effect 
                  fed_larvae_SHREW = SHREW_Burden[1,week_i]*SHREW_SD_L
      
                  Total_EL = fed_larvae_WFM + fed_larvae_SMB + fed_larvae_REP + fed_larvae_MSM + fed_larvae_WTD + fed_larvae_SHREW
      
                  if (Total_EL>0) # just finished feeding
                  {
                    EL[week_i+1,week_i+1]=Total_EL

                    # transstadial infection
                    transstadial_infection_L = I_LOH[1, week_i]*Total_EL
                                
                    # Vector-borne transmission from infected hosts to susceptible ticks
                    WFM_to_tick_infection_L = fed_larvae_WFM*(1-I_LOH[1,week_i]) * # susceptible ticks from WFM
                      I_WFM_HD[week_i]* #percent infected hosts
                      I_WFM_to_TICK #transmission rate
                                
                    SMB_to_tick_infection_L = fed_larvae_SMB*(1-I_LOH[1,week_i]) * # susceptible ticks from SMB
                      I_SMB_HD[week_i]* #percent infected hosts
                      I_SMB_to_TICK #transmission rate
                                
                    REP_to_tick_infection_L = fed_larvae_REP*(1-I_LOH[1,week_i]) * # susceptible ticks from REP
                      I_REP_HD[week_i]* #percent infected hosts
                      I_REP_to_TICK #transmission rate
                                
                    MSM_to_tick_infection_L = fed_larvae_MSM*(1-I_LOH[1,week_i]) * # susceptible ticks from MSM
                      I_MSM_HD[week_i]* #percent infected hosts
                      I_MSM_to_TICK #transmission rate
                                
                    WTD_to_tick_infection_L = fed_larvae_WTD*(1-I_LOH[1,week_i]) * # susceptible ticks from WTD
                      I_WTD_HD[week_i]* #percent infected hosts
                      I_WTD_to_TICK #transmission rate
                    ##################################3
                    SHREW_to_tick_infection_L = fed_larvae_SHREW*(1-I_LOH[1,week_i]) * # susceptible ticks from SHREW
                      I_SHREW_HD[week_i]* #percent infected hosts
                      I_SHREW_to_TICK #transmission rate
                    ########################################
                                
                    I_EL[week_i+1, week_i+1] = (transstadial_infection_L + WFM_to_tick_infection_L+
                      SMB_to_tick_infection_L+REP_to_tick_infection_L+
                      MSM_to_tick_infection_L+WTD_to_tick_infection_L+SHREW_to_tick_infection_L)/Total_EL
                                
                  }
      
                  for(cohort_i in week_i:1) # check on other cohorts to see if ready to molt
                  {
                    CDW = 0 # clear out cumulative degree weeks
                    week_check = week_i - cohort_i + 1
        
                    if (EL[week_check,week_i]>0)
                    { 
                      hold_temperature <- temperature_record[week_i] # get this weeks temperature 
                      if (hold_temperature > DT_L) 
                      { # add this week DW to CDW 
                        CDW = EL_CDW[week_check,week_i] + (hold_temperature -DT_L)*dev_delay 
                      } else { 
                        CDW = EL_CDW[week_check,week_i] 
                      }
                      if (CDW > CDW_L)
                      { 
                        I_HSN[1,week_i+1] = (I_HSN[1,week_i+1]*HSN[1,week_i+1] + #previously molted 
                          Larva_to_Nymph*I_EL[week_check,week_i]*EL[week_check,week_i])/ #new 
                          (HSN[1,week_i+1]+EL[week_check,week_i]) #total
            
                        HSN[1,week_i+1] = HSN[1,week_i+1]+EL[week_check,week_i]
                        HSN_life_span[1, week_i+1] = max_life_span # starts with max life span
                        EL[week_check, week_i+1] = 0 # all eggs hatch at same time from this cohort  
                      } else { #if don't hatch, must survive
                        EL[week_check, week_i+1] = EL[week_check,week_i]*SE_L 
                        EL_CDW[week_check,week_i+1] = CDW 
                        I_EL[week_check, week_i+1]=I_EL[week_check,week_i] 
                      } 
                    } 
                  }
      
                  # check on Host-seeking nymphs
                  # note, start from 2 since week 1 just emerged
                  # note, stage max_life_span gets different survival
                  # check on Host-seeking larvae
                  # note, start from 2 since week 1 just emerged
                  # note, stage max_life_span gets different survival
                  HSN[2, week_i+1]=HSN[1, week_i]*S1_N
                  I_HSN[2, week_i+1]=I_HSN[1,week_i] # pass along percent infected
                  HSN_life_span[2, week_i+1] = spraying_death*HSN_life_span[1,week_i]
      
                  for (HSN_check in 2:max_life_span) 
                  { # find host and advance to nymphs on host 
                    TOTAL_HFR_N = WFM_HFR_N + SMB_HFR_N + REP_HFR_N + MSM_HFR_N + WTD_HFR_N + SHREW_HFR_N 
                    if (TOTAL_HFR_N > 1)
                    { 
                      stop("Total host finding rate for nymphs > 1") 
                    } 
                                
                    Want_host_n = HSN[HSN_check,week_i]*TOTAL_HFR_N
        
                    Found_WFM = WFM_HFR_N*HSN[HSN_check,week_i] 
                    if (WFM_Burden[2, week_i+1] + Found_WFM < WFM_K[2]) 
                    { 
                      WFM_Burden[2,week_i+1]= WFM_Burden[2,week_i+1] + Found_WFM 
                    } else { 
                      Found_WFM = WFM_K[2]-WFM_Burden[2,week_i+1] 
                      WFM_Burden[2, week_i+1] = WFM_K[2] 
                    }
        
                    Found_SMB = SMB_HFR_N*HSN[HSN_check,week_i] 
                    if (SMB_Burden[2, week_i+1] + Found_SMB < SMB_K[2]) 
                    { 
                      SMB_Burden[2,week_i+1]= SMB_Burden[2,week_i+1] + Found_SMB 
                    } else { 
                      Found_SMB = SMB_K[2]-SMB_Burden[2,week_i+1] 
                      SMB_Burden[2, week_i+1] = SMB_K[2] 
                    }
        
                    Found_REP = REP_HFR_N*HSN[HSN_check,week_i] 
                    if (REP_Burden[2, week_i+1] + Found_REP < REP_K[2]) 
                    { 
                      REP_Burden[2,week_i+1]= REP_Burden[2,week_i+1] + Found_REP 
                    } else { 
                      Found_REP = REP_K[2]-REP_Burden[2,week_i+1] 
                      REP_Burden[2, week_i+1] = REP_K[2] 
                    }
        
                    Found_MSM = MSM_HFR_N*HSN[HSN_check,week_i] 
                    if (MSM_Burden[2, week_i+1] + Found_MSM < MSM_K[2]) 
                    { 
                      MSM_Burden[2,week_i+1]= MSM_Burden[2,week_i+1] + Found_MSM 
                    } else { 
                      Found_MSM = MSM_K[2]-MSM_Burden[2,week_i+1] 
                      MSM_Burden[2, week_i+1] = MSM_K[2] 
                    }
        
                    Found_WTD = WTD_HFR_N*HSN[HSN_check,week_i] 
                    if (WTD_Burden[2, week_i+1] + Found_WTD < WTD_K[2]) 
                    { 
                      WTD_Burden[2,week_i+1]= WTD_Burden[2,week_i+1] + Found_WTD 
                    } else { 
                      Found_WTD = WTD_K[2]-WTD_Burden[2,week_i+1] 
                      WTD_Burden[2, week_i+1] = WTD_K[2] 
                    }
        
                    Found_SHREW = SHREW_HFR_N*HSN[HSN_check,week_i] 
                    if (SHREW_Burden[2, week_i+1] + Found_SHREW < SHREW_K[2]) 
                    { 
                      SHREW_Burden[2,week_i+1]= SHREW_Burden[2,week_i+1] + Found_SHREW 
                    } else { 
                      Found_SHREW = SHREW_K[2]-SHREW_Burden[2,week_i+1] 
                      SHREW_Burden[2, week_i+1] = SHREW_K[2] 
                    }  
                                
                    Found_host = Found_WFM + Found_SMB + Found_REP + Found_MSM + Found_WTD + Found_SHREW
        
                    if(Found_host>0)
                    {
                      I_NOH[1, week_i+1] = (I_NOH[1,week_i+1]*NOH[1,week_i+1] + # number infected from before 
                        I_HSN[HSN_check, week_i]*Found_host)/ # new infecteds 
                        (NOH[1,week_i+1]+Found_host) # total 
                      NOH[1,week_i+1] = NOH[1,week_i+1] + Found_host 
                    } else { 
                      I_NOH[1, week_i+1] = I_NOH[1, week_i+1] 
                      NOH[1,week_i+1] = NOH[1,week_i+1] 
                    } 
                                
                    surplus = Want_host_n - Found_host 
                    if(surplus>0) 
                    { 
                      I_N_surplus[1, week_i+1] = (I_N_surplus[1,week_i+1]*N_surplus[1,week_i+1] + # number infected from before
                        I_HSN[HSN_check, week_i]*surplus)/ # new infecteds
                        (N_surplus[1,week_i+1]+surplus) # total
                      N_surplus[1,week_i+1] = N_surplus[1, week_i+1] + surplus 
                    } else { 
                      I_N_surplus[1, week_i+1] = I_N_surplus[1, week_i+1] 
                      N_surplus[1,week_i+1] = N_surplus[1, week_i+1] 
                    }
        
                    # else must survive and advance to next stage
                    if (HSN_check == max_life_span)
                    {
                      HSN[HSN_check, week_i+1]=
                      HSN[HSN_check, week_i+1] + 
                      (HSN[HSN_check, week_i]-Found_host - surplus)*S2_N + # ticks that didn't quest 
                      surplus*0 # ticks that quested but failed to find a host 
                      #HSN[HSN_check, week_i+1]=HSN[HSN_check, week_i+1]+(HSN[HSN_check, week_i]-Found_host)*S2_N 
                      I_HSN[HSN_check, week_i+1]=I_HSN[HSN_check,week_i]
                                    
                      if (Want_host_n> 0)
                      {  # lose two weeks of life for a week of questing
                        HSN_life_span[HSN_check,week_i+1] = HSN_life_span[HSN_check, week_i] - quest_cost  
                      } else {  
                        HSN_life_span[HSN_check,week_i+1] = HSN_life_span[HSN_check, week_i]  
                      } 
                    } else {
                      if (HSN_check<HSN_life_span[HSN_check, week_i]) 
                      { 
                        HSN[HSN_check+1, week_i+1]= HSN[HSN_check+1, week_i+1] + # ticks already in that bucket 
                          (HSN[HSN_check, week_i]-Found_host - surplus)*S1_N + # ticks that didn't quest 
                          (surplus)*0 # ticks that quested but failed - not currently used, but here in case these have different survival
                         I_HSN[HSN_check+1, week_i+1]=I_HSN[HSN_check,week_i]
                                    
                         if (Want_host_n> 0)
                         {  # lose two weeks of life for a week of questing
                           HSN_life_span[HSN_check+1,week_i+1] = HSN_life_span[HSN_check, week_i] - quest_cost 
                         } else { 
                           HSN_life_span[HSN_check+1,week_i+1] = HSN_life_span[HSN_check, week_i]  
                         }  
                       }  
                     }
                   }
      
                   fed_nymph_WFM = WFM_Burden[2,week_i]*WFM_SD_N*WFM_treating_effect 
                   fed_nypmh_SMB = SMB_Burden[2,week_i]*SMB_SD_N 
                   fed_nypmh_REP = REP_Burden[2,week_i]*REP_SD_N 
                   fed_nypmh_MSM = MSM_Burden[2,week_i]*MSM_SD_N 
                   fed_nypmh_WTD = WTD_Burden[2,week_i]*WTD_SD_N*WTD_treating_effect 
                   fed_nypmh_SHREW = SHREW_Burden[2,week_i]*SHREW_SD_N
      
                   Total_EN = fed_nymph_WFM + fed_nypmh_SMB + fed_nypmh_REP + fed_nypmh_MSM + fed_nypmh_WTD + fed_nypmh_SHREW

                   if (Total_EN>0) # just finished feeding 
                   { 
                     EN[week_i+1,week_i+1]=Total_EN
        
                     # transstadial infection
                     transstadial_infection_N = I_NOH[1, week_i]*Total_EN
                                
                     # Vector-borne transmission from infected hosts to susceptible ticks
                     WFM_to_tick_infection = fed_nymph_WFM*(1-I_NOH[1,week_i]) * # susceptible ticks from WFM
                       I_WFM_HD[week_i]* #percent infected hosts
                       I_WFM_to_TICK #transmission rate
                                
                     SMB_to_tick_infection = fed_nypmh_SMB*(1-I_NOH[1,week_i]) * # susceptible ticks from SMB
                       I_SMB_HD[week_i]* #percent infected hosts
                       I_SMB_to_TICK #transmission rate
                                
                     REP_to_tick_infection = fed_nypmh_REP*(1-I_NOH[1,week_i]) * # susceptible ticks from REP
                       I_REP_HD[week_i]* #percent infected hosts
                       I_REP_to_TICK #transmission rate
                                
                     MSM_to_tick_infection = fed_nypmh_MSM*(1-I_NOH[1,week_i]) * # susceptible ticks from MSM
                       I_MSM_HD[week_i]* #percent infected hosts
                       I_MSM_to_TICK #transmission rate
                                
                     WTD_to_tick_infection = fed_nypmh_WTD*(1-I_NOH[1,week_i]) * # susceptible ticks from WTD
                       I_WTD_HD[week_i]* #percent infected hosts
                       I_WTD_to_TICK #transmission rate
                                
                     SHREW_to_tick_infection = fed_nypmh_SHREW*(1-I_NOH[1,week_i]) * # susceptible ticks from SHREW
                       I_SHREW_HD[week_i]* #percent infected hosts
                       I_SHREW_to_TICK #transmission rate
                                
                     I_EN[week_i+1, week_i+1] = (transstadial_infection_N+ WFM_to_tick_infection+
                       SMB_to_tick_infection+REP_to_tick_infection+
                       MSM_to_tick_infection+WTD_to_tick_infection+
                       SHREW_to_tick_infection)/Total_EN 
                   }
      
      
                   for(cohort_i in week_i:1) # check on other cohorts to see if ready to molt 
                   { 
                     CDW = 0 # clear out cumulative degree weeks 
                     week_check = week_i - cohort_i + 1 
                     if (EN[week_check,week_i]>0)
                     { 
                       hold_temperature <- temperature_record[week_i] # get this weeks temperature 
                       if (hold_temperature > DT_N) 
                       { # add this week DW to CDW 
                         CDW = EN_CDW[week_check,week_i] + (hold_temperature -DT_N)*dev_delay 
                       } else { 
                         CDW = EN_CDW[week_check, week_i] 
                       } 
                       if (CDW > CDW_N) 
                       { 
                         I_HSA[1,week_i+1] = (I_EN[week_check,week_i]*EN[week_check,week_i] + 
                           Nymph_to_Adult * I_HSA[1,week_i+1]*HSA[1,week_i+1])/ 
                           (HSA[1,week_i+1] = HSA[1,week_i+1]+EN[week_check,week_i]) 
                         HSA[1,week_i+1] = HSA[1,week_i+1]+EN[week_check,week_i] 
                         HSA_life_span[1, week_i+1] = max_life_span # starts with max life span
                         EN[week_check, week_i+1] = 0 # all molt at same time from this cohort 
                         I_EN[week_check,week_i+1]=0 # infected ticks molted too! 
                       } else { #if don't molt, must survive 
                         EN[week_check, week_i+1] = EN[week_check,week_i]*SE_N 
                         EN_CDW[week_check,week_i+1] = CDW 
                         I_EN[week_check, week_i+1] = I_EN[week_check,week_i] 
                       } 
                     } 
                  }
      
                  # check on Host-seeking adults 
                  # note, start from 2 since week 1 just emerged 
                  # note, stage max_life_span gets different survival 
                  HSA[2, week_i+1]=HSA[1, week_i]*S1_A 
                  I_HSA[2, week_i+1]=I_HSA[1,week_i] # pass along percent infected
                  HSA_life_span[2, week_i+1] = spraying_death*HSA_life_span[1,week_i]
      
                  for (HSA_check in 2:max_life_span)
                  { 
                    # find host and advance to larvae on host 
                    TOTAL_HFR_A = MSM_HFR_A + WTD_HFR_A 
                    if (TOTAL_HFR_A > 1)
                    { 
                      stop("Total host finding rate for adults > 1") 
                    } 
                    Want_host_a = HSA[HSA_check,week_i]*TOTAL_HFR_A
        
                    Found_MSM = MSM_HFR_A*HSA[HSA_check,week_i] 
                    if (MSM_Burden[3, week_i+1] + Found_MSM < MSM_K[3]) 
                    { 
                      MSM_Burden[3,week_i+1]= MSM_Burden[3,week_i+1] + Found_MSM 
                     } else { 
                       Found_MSM = MSM_K[3]-MSM_Burden[3,week_i+1] 
                       MSM_Burden[3, week_i+1] = MSM_K[3] 
                     }
        
                     Found_WTD = WTD_HFR_A*HSA[HSA_check,week_i] 
                     if (WTD_Burden[3, week_i+1] + Found_WTD < WTD_K[3]) 
                     { 
                       WTD_Burden[3,week_i+1]= WTD_Burden[3,week_i+1] + Found_WTD 
                     } else { 
                       Found_WTD = WTD_K[3]-WTD_Burden[3,week_i+1] 
                       WTD_Burden[3, week_i+1] = WTD_K[3] 
                     }
        
                     Found_host = Found_MSM + Found_WTD
        
                     if(Found_host>0)
                     { 
                       I_AOH[1, week_i+1] = (I_AOH[1,week_i+1]*AOH[1,week_i+1] + 
                         I_HSA[HSA_check, week_i]*Found_host)/ 
                         (AOH[1,week_i+1] + Found_host) 
                       AOH[1,week_i+1] = AOH[1,week_i+1] + Found_host 
                     } else { 
                       I_AOH[1, week_i+1] = I_AOH[1, week_i+1] 
                       AOH[1,week_i+1] = AOH[1,week_i+1] 
                     } 
                     surplus = Want_host_a - Found_host 
                     if(surplus>0) 
                     { 
                       I_A_surplus[1, week_i+1] = (I_A_surplus[1,week_i+1]*A_surplus[1,week_i+1] + # number infected from before 
                         I_HSA[HSA_check, week_i]*surplus)/ # new infecteds 
                         (A_surplus[1,week_i+1]+surplus) # total 
                       A_surplus[1,week_i+1] = A_surplus[1, week_i+1] + surplus 
                     } else { 
                       I_A_surplus[1, week_i+1] = I_A_surplus[1, week_i+1] 
                       A_surplus[1,week_i+1] = A_surplus[1, week_i+1] 
                     }
        
        
                     # else must survive and advance to next stage
                     if (HSA_check == max_life_span)
                     {
                       HSA[HSA_check, week_i+1]=
                       HSA[HSA_check, week_i+1] + 
                       (HSA[HSA_check, week_i]-Found_host - surplus)*S2_A + # ticks that didn't quest 
                       surplus*0 # ticks that quested but failed to find a host 
                       I_HSA[HSA_check, week_i+1]=I_HSA[HSA_check,week_i]
                                    
                       if (Want_host_a> 0)
                       {  # lose two weeks of life for a week of questing
                         HSA_life_span[HSA_check,week_i+1] = HSA_life_span[HSA_check, week_i] - quest_cost 
                       } else {  
                         HSA_life_span[HSA_check,week_i+1] = HSA_life_span[HSA_check, week_i]  
                       } 
                     } else {
                       if (HSA_check<HSA_life_span[HSA_check, week_i]) 
                       { 
                         HSA[HSA_check+1, week_i+1]= HSA[HSA_check+1, week_i+1] + # ticks already in that bucket 
                           (HSA[HSA_check, week_i]-Found_host - surplus)*S1_A + # ticks that didn't quest 
                           (surplus)*0 # ticks that quested but failed - not currently used, but here in case these have different survival
                         I_HSA[HSA_check+1, week_i+1]=I_HSA[HSA_check,week_i]
                                    
                         if (Want_host_a> 0)
                         {  # lose two weeks of life for a week of questing
                           HSA_life_span[HSA_check+1,week_i+1] = HSA_life_span[HSA_check, week_i] - quest_cost 
                         } else { 
                           HSA_life_span[HSA_check+1,week_i+1] = HSA_life_span[HSA_check, week_i]  
                         }  
                       }  
                     }
                   }
      
                   fed_adult_MSM = MSM_Burden[3,week_i]*MSM_SD_A
                   fed_adult_WTD = WTD_Burden[3,week_i]*WTD_SD_A*WTD_treating_effect
                   Total_EA = fed_adult_MSM + fed_adult_WTD
      
                   if (Total_EA>0) # just finished feeding 
                   { 
                     EA[week_i+1,week_i+1]=Total_EA
        
                     # transstadial infection
                     transstadial_infection_A = I_AOH[1, week_i]*Total_EA

                     MSM_to_tick_infection = fed_adult_MSM*(1-I_AOH[1,week_i]) * # susceptible ticks from MSM 
                       I_MSM_HD[week_i]* #percent infected hosts 
                       I_MSM_to_TICK #transmission rate
        
                     WTD_to_tick_infection = fed_adult_WTD*(1-I_AOH[1,week_i]) * # susceptible ticks from WTD 
                       I_WTD_HD[week_i]* #percent infected hosts 
                       I_WTD_to_TICK #transmission rate
        
                     I_EA[week_i+1, week_i+1] = (transstadial_infection_A + MSM_to_tick_infection+
                       WTD_to_tick_infection)/Total_EA   
                   }
      
                   for(cohort_i in week_i:1) # check on other cohorts to see if ready to molt 
                   { 
                     CDW = 0 # clear out cumulative degree weeks 
                     week_check = week_i - cohort_i + 1 
                     if (EA[week_check,week_i]>0)
                     { 
                       hold_temperature <- temperature_record[week_i] # get this weeks temperature 
                       if (hold_temperature > DT_A) 
                       { # add this week DW to CDW 
                         CDW = EA_CDW[week_check,week_i] + (hold_temperature - DT_A)*dev_delay 
                       } else { 
                         CDW = EA_CDW[week_check,week_i] 
                       } 
                       if (CDW > CDW_A) 
                       { 
                         I_Eggs[week_check, week_i+1] = (I_Eggs[week_check,week_i+1]*Eggs[week_check,week_i+1] + 
                           Adult_to_Egg*I_EA[week_check,week_i])/(Eggs[week_check, week_i+1] + 
                           Fecundity*EA[week_check,week_i]/2)
                         Eggs[week_check, week_i+1] = Eggs[week_check, week_i+1] + Fecundity*EA[week_check,week_i]/2
                         EA[week_check, week_i+1] = 0 # all molt at same time from this cohort
                         EA[week_check,week_i+1] = 0 # infecteds molt too
                       } else { #if don't molt, must survive
                         EA[week_check, week_i+1] = EA[week_check,week_i]*SE_A 
                         EA_CDW[week_check,week_i+1] = CDW 
                         I_EA[week_check, week_i+1] = I_EA[week_check, week_i] 
                       } 
                     } 
                   }
      
                   # new infections from infected ticks to susceptible hosts
      
                   ITD = I_LOH[1,week_i]*WFM_Burden[1,week_i] + I_NOH[1,week_i]*WFM_Burden[2,week_i]
                   ITH = 0.0
                   if (WFM_HD > 0)
                   { 
                     ITH = (ITD/WFM_HD)*TICK_TO_HOST 
                   } 
                   look_up_val = round(ITH*10)+1 
                   if (look_up_val<1) 
                   { 
                     effective_trans = 0 
                   } else if (look_up_val > length(perc_trans)) 
                   { 
                     effective_trans = 100 
                   } else { 
                     effective_trans = perc_trans[look_up_val] 
                   }
      
                   New_infected_WFM[1,week_i+1] = 
                     (1-I_WFM_HD[1,week_i])*(WFM_vaccine_effect) * effective_trans/100.0 # susceptible hosts * transmission 
                   I_WFM_HD[1,week_i+1] = I_WFM_HD[1,week_i+1] + New_infected_WFM[1,week_i+1]
      
                   ITD = I_LOH[1,week_i]*SMB_Burden[1,week_i] + I_NOH[1,week_i]*SMB_Burden[2,week_i]
                   ITH = 0.0
                   if (SMB_HD > 0.0) 
                   { 
                     ITH = (ITD/SMB_HD)*TICK_TO_HOST 
                   } 
                   look_up_val = round(ITH*10)+1 
                   if (look_up_val<1) 
                   { 
                     effective_trans = 0 
                   } else if (look_up_val > length(perc_trans)) { 
                     effective_trans = 100 
                   } else { 
                     effective_trans = perc_trans[look_up_val] 
                   }
      
                   New_infected_SMB[1,week_i+1] = (1-I_SMB_HD[1,week_i]) * effective_trans/100.0 # susceptible hosts 
                   I_SMB_HD[1,week_i+1] = I_SMB_HD[1,week_i+1] + New_infected_SMB[1,week_i+1]
      
                   ITD = I_LOH[1,week_i]*REP_Burden[1,week_i] + I_NOH[1,week_i]*REP_Burden[2,week_i]
                   ITH = 0.0
                   if (REP_HD> 0) 
                   { 
                     ITH = (ITD/REP_HD)*TICK_TO_HOST 
                   }
      
                   look_up_val = round(ITH*10)+1
                   if (look_up_val<1) 
                   { 
                     effective_trans = 0 
                   } else if (look_up_val > length(perc_trans)) { 
                     effective_trans = 100 
                   } else { 
                     effective_trans = perc_trans[look_up_val] 
                   }
                            
                   New_infected_REP[1,week_i+1] = (1-I_REP_HD[1,week_i]) * effective_trans/100.0 # susceptible hosts
                   I_REP_HD[1,week_i+1] = I_REP_HD[1,week_i+1] + New_infected_REP[1,week_i+1]
      
                   ITD = I_LOH[1,week_i]*MSM_Burden[1,week_i] + I_NOH[1,week_i]*MSM_Burden[2,week_i] +
                   I_AOH[1,week_i]*MSM_Burden[3,week_i] 
                   ITH = 0.0 
                   if (MSM_HD > 0) 
                   { 
                     ITH = (ITD/MSM_HD)*TICK_TO_HOST 
                   }
      
                   look_up_val = round(ITH*10)+1
                   if (look_up_val<1) 
                   { 
                     effective_trans = 0 
                   } else if (look_up_val > length(perc_trans)) { 
                     effective_trans = 100 
                   } else { 
                     effective_trans = perc_trans[look_up_val] 
                   }
                            
                   New_infected_MSM[1,week_i+1] = (1-I_MSM_HD[1,week_i]) * effective_trans/100.0 # susceptible hosts
                   I_MSM_HD[1,week_i+1] = I_MSM_HD[1,week_i+1] + New_infected_MSM[1,week_i+1]
      
                   ITD = I_LOH[1,week_i]*WTD_Burden[1,week_i] + I_NOH[1,week_i]*WTD_Burden[2,week_i] +
                   I_AOH[1,week_i]*WTD_Burden[3,week_i]
                   ITH = 0.0
                   if (WTD_HD > 0) 
                   { 
                     ITH = (ITD/WTD_HD)*TICK_TO_HOST 
                   }
      
                   look_up_val = round(ITH*10)+1
                   if (look_up_val<1) 
                   { 
                     effective_trans = 0 
                   } else if (look_up_val > length(perc_trans)) { 
                     effective_trans = 100 
                   } else { 
                     effective_trans = perc_trans[look_up_val] 
                   }
      
                   New_infected_WTD[1,week_i+1] = (1-I_WTD_HD[1,week_i]) * effective_trans/100.0 # susceptible hosts
                   I_WTD_HD[1,week_i+1] = I_WTD_HD[1,week_i+1] + New_infected_WTD[1,week_i+1]
      
                   ITD = I_LOH[1,week_i]*SHREW_Burden[1,week_i] + I_NOH[1,week_i]*SHREW_Burden[2,week_i] +
                   I_AOH[1,week_i]*SHREW_Burden[3, week_i]
                   ITH = 0.0
                   if (SHREW_HD > 0)
                   { 
                     ITH = (ITD/SHREW_HD)*TICK_TO_HOST 
                   }
      
                   look_up_val = round(ITH*10)+1
                   if (look_up_val<1)
                   { 
                     effective_trans = 0 
                   } else if (look_up_val > length(perc_trans)) { 
                     effective_trans = 100 
                   } else { 
                     effective_trans = perc_trans[look_up_val] 
                   } 
                   New_infected_SHREW[1,week_i+1] = (1-I_SHREW_HD[1,week_i]) * effective_trans/100.0 # susceptible hosts 
                   I_SHREW_HD[1,week_i+1] = I_SHREW_HD[1,week_i+1] + New_infected_SHREW[1,week_i+1]  
       }
       
       #end of particular run
       #output to keep from that set
       print("Write output")
                        #write(my_param_i, file="data", append = TRUE)
                        #write(max(colSums(HSN)[400:600]), file = "data", append = TRUE)
                        #min(colSums(HSN)[400:600])
                        larvae = colSums(HSL)
                        nymphs = colSums(HSN)
                        adults = colSums(HSA)
                        i_larvae = (colSums(HSL*I_HSL)) #/colSums(HSL+1))
                        i_nymphs = (colSums(HSN*I_HSN)) #/colSums(HSN+1))
                        i_adults = (colSums(HSA*I_HSA)) #/colSums(HSA+1))
                        
                        DON[location_i, scenario_i, 1:total_weeks] = nymphs[1:total_weeks]
                        DIN[location_i, scenario_i, 1:total_weeks] = i_nymphs[1:total_weeks]
                        
                        #larvae_OH = LOH[1,start_t:end_t]
                        #nymphs_OH = NOH[1,start_t:end_t]
                        #adults_OH = colSums(AOH[1:2,start_t:end_t])
                        i_larvae_OH = (LOH*I_LOH)
                        i_nymphs_OH = (NOH*I_NOH)
                        i_adults_OH = colSums(AOH*I_AOH)
                        i_nymphs_surplus = (N_surplus*I_N_surplus)
                        
                        DON_OH[location_i, scenario_i, 1:total_weeks] = NOH[1:total_weeks]
                        DIN_OH[location_i, scenario_i, 1:total_weeks] = i_nymphs_OH[1:total_weeks]
                        
                        DON_Surplus[location_i,scenario_i,1:total_weeks] = N_surplus[1:total_weeks]
                        DIN_Surplus[location_i,scenario_i,1:total_weeks] = i_nymphs_surplus[1:total_weeks]
                        
                        Effect_here = Effectiveness[effectiveness_i]
                        print("Write output")
                        write.table(LOH, paste0(save_dir, "LOH_", abs(Effect_here-1),"_Effect_here", abs(scenario_i),".txt"), sep="\t")
                        write.table(NOH, paste0(save_dir, "NOH_", abs(Effect_here-1),"_Effect_here", abs(scenario_i),".txt"), sep="\t")
                        write.table(AOH, paste0(save_dir, "AOH_", abs(Effect_here-1),"_Effect_here", abs(scenario_i),".txt"), sep="\t")
    
    
                      start_lt = 52*(burn_in_years-2)  #use this if running controls
                      #start_lt = 52*(burn_in_years) # use this if running annual variation
                      end_lt = start_lt+52*(years_of_control+years_after_control)
                      control_applied = array("c", c(years_of_control))
                      at_plot = c(1:years_of_control)
                      at_plot = (at_plot-1)*52 + 26 + 52*2
                      
                      HOLD_DON_OH = NOH[start_lt:end_lt]/100.0
                      HOLD_DIN_OH = i_nymphs_OH[start_lt:end_lt]/100.0
                      
                      #HOLD_DON_OH = DON_OH[location_i,scenario_i,start_lt:end_lt]
                      #HOLD_DIN_OH = DIN_OH[location_i,scenario_i,start_lt:end_lt]
                      
                      xrange = c(1:52*(years_of_control+years_after_control))
                      yrange <- range(HOLD_DON_OH)
                      #yrange[2] = 375 # yrange[2] + 50 # give space for legend
                      colors <- rainbow(num_loc) 
                      #linetype <- c("l",1,1) #c(1:num_loc) 
                      write.table(HOLD_DIN_OH, paste0(save_dir, "DIN_", abs(Effect_here-1),"_Effect_here", abs(scenario_i),".txt"), sep="\t")
                      write.table(HOLD_DON_OH, paste0(save_dir,"DON_", abs(Effect_here-1),"_Effect_here", abs(scenario_i),".txt"), sep="\t")
                      png(paste0(save_dir,"OH_",abs(Effect_here-1),abs(scenario_i),".png"))
                      
          
                      plot(HOLD_DON_OH, ylim = yrange, xlab="Week", type = "l", 
                           lty =1, ylab="Ticks on host per 100 m^2", col = 1, lwd = 3, xaxt = "n")
                      lines(HOLD_DIN_OH,  type = "l", 
                            lty =1, col = 2, lwd = 3)
                      if (years_of_control > 0)
                        {
                        axis(1, at = at_plot, labels = control_applied, cex.axis = 0.9) 
                        }
                      legend("topright", c("DON", "DIN"), col=1:2, lty = 1, lwd = 3)
                      dev.off()
                      print(paste0("Effect_here: ", Effect_here))
                      
                      #HOLD_DON_OH = DON_Surplus[location_i,scenario_i,start_lt:end_lt]
                      #HOLD_DIN_OH = DIN_Surplus[location_i,scenario_i,start_lt:end_lt]
                      
                      HOLD_DON_OH = N_surplus[start_lt:end_lt]/100.0
                      HOLD_DIN_OH = i_nymphs_surplus[start_lt:end_lt]/100.0
                      
                      xrange = c(1:52*(years_of_control+years_after_control))
                      yrange <- range(HOLD_DON_OH)
                      #yrange[2] = 375 # yrange[2] + 50 # give space for legend
                      colors <- rainbow(num_loc) 
                      #linetype <- c("l",1,1) #c(1:num_loc) 
                      write.table(HOLD_DIN_OH, paste0(save_dir, "DIN_", abs(Effect_here-1),"_Effect_here_Surplus",abs(scenario_i),".txt"), sep="\t")
                      write.table(HOLD_DON_OH, paste0(save_dir,"DON_", abs(Effect_here-1),"_Effect_here_Surplus",abs(scenario_i),".txt"), sep="\t")
                      png(paste0(save_dir,"Surplus_",abs(Effect_here-1),abs(scenario_i),".png"))
                      
                      
                      plot(HOLD_DON_OH, ylim = yrange, xlab="Week", type = "l", 
                           lty =1, ylab="Ticks per 100 m^2 ", col = 1, lwd = 3, xaxt = "n")
                      lines(HOLD_DIN_OH,  type = "l", 
                            lty =1, col = 2, lwd = 3)
                      if (years_of_control > 0)
                        {
                        axis(1, at = at_plot, labels = control_applied, cex.axis = 0.9) 
                        }
                      legend("topright", c("DON", "DIN"), col=1:2, lty = 1, lwd = 3)
                      dev.off()
                      
                      for (ls_i in 1:3)
                      {
                      png(paste0(save_dir,"WFM_",abs(ls_i),abs(Effect_here-1),abs(scenario_i),".png"))
                      plot(WFM_Burden[ls_i,start_lt:end_lt]/WFM_HD_Plot[start_lt:end_lt], xlab="Week", type = "l", 
                           lty =1, ylab="Ticks per WFM per 100 m^2 ", col = 1, lwd = 3) #, xaxt = "n")
                      if (years_of_control > 0)
                        {
                        axis(1, at = at_plot, labels = control_applied, cex.axis = 0.9) 
                        }
                      dev.off()
                      
                      png(paste0(save_dir,"SHREW_",abs(ls_i),abs(Effect_here-1),abs(scenario_i),".png"))
                      plot(SHREW_Burden[ls_i,start_lt:end_lt]/SHREW_HD, xlab="Week", type = "l", 
                           lty =1, ylab="Ticks per SHREW per 100 m^2 ", col = 1, lwd = 3) #, xaxt = "n")
                      if (years_of_control > 0)
                        {
                        axis(1, at = at_plot, labels = control_applied, cex.axis = 0.9) 
                        }
                      dev.off()
                      
                      png(paste0(save_dir,"MSM_",abs(ls_i),abs(Effect_here-1),abs(scenario_i),".png"))
                      plot(MSM_Burden[ls_i,start_lt:end_lt]/MSM_HD, xlab="Week", type = "l", 
                           lty =1, ylab="Ticks per MSM per 100 m^2 ", col = 1, lwd = 3) #, xaxt = "n")
                      if (years_of_control > 0)
                        {
                        axis(1, at = at_plot, labels = control_applied, cex.axis = 0.9) 
                        }
                      dev.off()
                      
                      png(paste0(save_dir,"WTD_",abs(ls_i),abs(Effect_here-1),abs(scenario_i),".png"))
                      plot(WTD_Burden[ls_i,start_lt:end_lt]/WTD_HD, xlab="Week", type = "l", 
                           lty =1, ylab="Ticks per WTD per 100 m^2 ", col = 1, lwd = 3) #, xaxt = "n")
                      if (years_of_control > 0)
                        {
                        axis(1, at = at_plot, labels = control_applied, cex.axis = 0.9) 
                        }
                      dev.off()
                      
                      png(paste0(save_dir,"REP_",abs(ls_i),abs(Effect_here-1),abs(scenario_i),".png"))
                      plot(REP_Burden[ls_i,start_lt:end_lt]/REP_HD, xlab="Week", type = "l", 
                           lty =1, ylab="Ticks per REP per 100 m^2 ", col = 1, lwd = 3) #, xaxt = "n")
                      if (years_of_control > 0)
                        {
                        axis(1, at = at_plot, labels = control_applied, cex.axis = 0.9) 
                        }
                      dev.off()
                      
                      png(paste0(save_dir,"SMB_",abs(ls_i),abs(Effect_here-1),abs(scenario_i),".png"))
                      plot(SMB_Burden[ls_i,start_lt:end_lt]/SMB_HD, xlab="Week", type = "l", 
                           lty =1, ylab="Ticks per SMB per 100 m^2 ", col = 1, lwd = 3) #, xaxt = "n")
                      if (years_of_control > 0)
                        {
                        axis(1, at = at_plot, labels = control_applied, cex.axis = 0.9) 
                        }
                      dev.off()
                      }
    } #end of that control option for all effectivenesses
  } # end of that time frame
  } #end of that location
} #end of everything
