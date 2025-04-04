#########################################################################
## Read in enrollment data for January of each year
#########################################################################

for (y in 2007:2015) {
  ## Basic contract/plan information
  
  ma_path <- paste0(
    "C:/Users/mirac/Documents/GitHub/econ470_ma/hw4/data/input/",
    "monthly-ma-and-pdp-enrollment-by-cpsc/CPSC_Contract_Info_", 
    y, "_01.csv"
  )
  
  contract_info <- read_csv(
    ma_path,
    skip = 1,
    col_names = c(
      "contractid", "planid", "org_type", "plan_type", "partd", "snp", 
      "eghp", "org_name", "org_marketing_name", "plan_name", "parent_org", 
      "contract_date"
    ),
    col_types = cols(
      contractid = col_character(),
      planid = col_double(),
      org_type = col_character(),
      plan_type = col_character(),
      partd = col_character(),
      snp = col_character(),
      eghp = col_character(),
      org_name = col_character(),
      org_marketing_name = col_character(),
      plan_name = col_character(),
      parent_org = col_character(),
      contract_date = col_character()
    )
  )

  contract_info <- contract_info %>%
    group_by(contractid, planid) %>%
    mutate(id_count = row_number())
    
  contract_info <- contract_info %>%
    filter(id_count == 1) %>%
    select(-id_count)
    
  ## Enrollments per plan
  ma_path <- paste0(
    "C:/Users/mirac/Documents/GitHub/econ470_ma/hw4/data/input/",
    "monthly-ma-and-pdp-enrollment-by-cpsc/CPSC_Enrollment_Info_", 
    y, "_01.csv"
  )
  
  enroll_info <- read_csv(
    ma_path,
    skip = 1,
    col_names = c(
      "contractid", "planid", "ssa", "fips", "state", "county", "enrollment"
    ),
    col_types = cols(
      contractid = col_character(),
      planid = col_double(),
      ssa = col_double(),
      fips = col_double(),
      state = col_character(),
      county = col_character(),
      enrollment = col_double()
    ),
    na = "*"
  )
    
  ## Merge contract info with enrollment info
  plan_data <- contract_info %>%
    left_join(enroll_info, by = c("contractid", "planid")) %>%
    mutate(year = y)
    
  ## Fill in missing fips codes (by state and county)
  plan_data <- plan_data %>%
    group_by(state, county) %>%
    fill(fips)

  ## Fill in missing plan characteristics by contract and plan id
  plan_data <- plan_data %>%
    group_by(contractid, planid) %>%
    fill(plan_type, partd, snp, eghp, plan_name)
  
  ## Fill in missing contract characteristics by contractid
  plan_data <- plan_data %>%
    group_by(contractid) %>%
    fill(org_type, org_name, org_marketing_name, parent_org)
    
  ## Collapse from monthly data to yearly
  plan_year <- plan_data %>%
    group_by(contractid, planid, fips) %>%
    arrange(contractid, planid, fips) %>%
    rename(avg_enrollment = enrollment)
  
  write_rds(
    plan_year, 
    paste0(
      "C:/Users/mirac/Documents/GitHub/econ470_ma/hw4/data/output/ma_data_", 
      y, ".rds"
    )
  )
}

full_ma_data <- read_rds(
  "C:/Users/mirac/Documents/GitHub/econ470_ma/hw4/data/output/ma_data_2007.rds"
)
for (y in 2008:2015) {
  full_ma_data <- rbind(
    full_ma_data,
    read_rds(
      paste0(
        "C:/Users/mirac/Documents/GitHub/econ470_ma/hw4/data/output/ma_data_", 
        y, ".rds"
      )
    )
  )
}

write_rds(
  full_ma_data, 
  "C:/Users/mirac/Documents/GitHub/econ470_ma/hw4/data/output/full_ma_data.rds"
)
sapply(
  paste0("ma_data_", 2007:2015, ".rds"), 
  unlink
)

print(ma_path)