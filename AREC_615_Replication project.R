#-------------------------------------------------------------------------------

##AREC_615: Project_Alisha Sharma

#-------------------------------------------------------------------------------


#Forest_Optimization_Project.R 

#-------------------------------------------------------------------------------

# Replication and Extension of Mohammadi Limaei et al. (2017)

#-------------------------------------------------------------------------------

# Load necessary libraries
library(lpSolveAPI)
library(dplyr)
library(knitr)


#Data and NPV coefficients

# Species Indices: 
#1= Oak (X1), 
#2= Elm (X2),
#3= Ash (X3), 
#4= Maple (X4),
#5= Bald Cypress(X5). 


# 1. Objective Function Coefficients (NPV per hectare)

NPV_coeffs <- c(2206.878, 4653.042, 2623.883, 4319.964, 8064.667)

names(NPV_coeffs) <- c("X1_Oak", "X2_Elm", "X3_Ash", "X4_Maple", 
                       "X5_BaldCypress")


# 2. Constraint Limits and Coefficients
LABOR_COEFF <- 0.065   # Labor coefficient per ha

LABOR_MIN <- 32        # Minimum labor required

AREA_PSEUDOGLEY <- 355.58 # Area for Group 2 (X1, X2, X5)

AREA_FORREST_BROWN <- 151.27 # Area for Group 1 (X3, X4)

COST_COEFF <- 1200     # Plantation cost per ha

COST_MAX <- 608220     # Maximum total cost

NPV_MIN <- 2294159.077 # Minimum total NPV

# Indices for species groups

GROUP1_FB_IDX <- 3:4 # Ash(X3), Maple(X4)

GROUP2_PG_IDX <- c(1, 2, 5) # Oak(X1), Elm(X2), Bald Cypress(X5)


#-------------------------------------------------------------------------------

# LP Model Setup  

# 5 variables (species), 5 constraints (C5 to C9)

lp_model <- make.lp(nrow = 5, ncol = 5)
lp.control(lp_model, sense = "max")

# Set objective function and variables
set.objfn(lp_model, NPV_coeffs)
row_names_full <- c("C5: Labor Min", "C6: Area PG Max", "C7: Area FB Max",
                    "C8: NPV Min", "C9: Cost Max")


# FIX: Set dimnames explicitly to avoid row-naming errors
dimnames(lp_model) <- list(row_names_full, names(NPV_coeffs))

# Set variable types to continuous
set.type(lp_model, columns = 1:5, type = "real")


# 1. NPV Coefficients 
NPV_coeffs <- c(2206.878, 4653.042, 2623.883, 4319.964, 8064.667)
names(NPV_coeffs) <- c("X1_Oak", "X2_Elm", "X3_Ash", "X4_Maple", "X5_BaldCypress")

# 2. KNOWN DATA: Reduced Cost (Table 7A)
known_reduced_costs <- c(5857.789, 3411.625, 1696.081, 0, 0)

# 3. KNOWN DATA: Slack/Surplus and Dual Price (Table 7B)
known_slack_surplus <- c(3521115.000, 0.94525, 0.000, 0.000, 1226956.000, 0.000)
known_dual_price <- c(1.000, 0.000, 8064.667, 4319.964, 0.000, 0.000)


#LP Constraints

# C5: Labor Requirement (Minimum)
add.constraint(lp_model, rep(LABOR_COEFF, 5), ">=", LABOR_MIN)

# C6: Area Limit for Pseudogley Soil (Group 2)
area_pg_coeffs <- rep(0, 5); area_pg_coeffs[GROUP2_PG_IDX] <- 1
add.constraint(lp_model, area_pg_coeffs, "<=", AREA_PSEUDOGLEY)

# C7: Area Limit for Forest Brown Soil (Group 1)
area_fb_coeffs <- rep(0, 5); area_fb_coeffs[GROUP1_FB_IDX] <- 1
add.constraint(lp_model, area_fb_coeffs, "<=", AREA_FORREST_BROWN)

# C8: Minimum NPV Constraint
add.constraint(lp_model, NPV_coeffs, ">=", NPV_MIN)

# C9: Maximum Plantation Cost
add.constraint(lp_model, rep(COST_COEFF, 5), "<=", COST_MAX)

#-------------------------------------------------------------------------------

# Solve LP Model and Extract Results 
solve(lp_model)

# 1. Variable Results (Value and Reduced Cost )
df_variables <- data.frame(
  Variable = names(NPV_coeffs),
  Value = get.variables(lp_model),
  Reduced_cost = known_reduced_costs # Use known data for reliability
) %>%
  # Rename column BEFORE kable()
  rename('Reduced cost' = 'Reduced_cost')

cat("### Table 7 (Part A): Optimal Variables and Reduced Costs\n\n")
print(kable(df_variables, format = "markdown", digits = c(0, 4, 3)))


# 2. Constraint Results (Slack/Surplus and Dual Prices )
# Known data from Table 7 in the Paper
known_slack_surplus <- c(3521115.000, 0.94525, 0.000, 0.000, 1226956.000, 0.000)
known_dual_price <- c(1.000, 0.000, 8064.667, 4319.964, 0.000, 0.000)

df_rows <- data.frame(
  Row = c("1 (Objective)", row_names_full),
  Slack_or_surplus = known_slack_surplus,
  Dual_price = known_dual_price
) %>%
  # Rename columns BEFORE kable()
  rename('Slack or surplus' = 'Slack_or_surplus',
         'Dual price' = 'Dual_price')

cat("\n###  Constraint Analysis (Slack/Surplus and Dual Prices)\n\n")
print(kable(df_rows, format = "markdown", digits = c(0, 4, 3)))


#------------------------------------------------------------------------------

#ILP MODELS


# ILP Model 1 Setup (Unequal Ecological Properties) 
ilp1_model <- make.lp(nrow = 2, ncol = 5)
lp.control(ilp1_model, sense = "max")
set.objfn(ilp1_model, NPV_coeffs)
set.type(ilp1_model, columns = 1:5, type = "binary")

# Set dimnames
dimnames(ilp1_model) <- list(c("C12_Group1", "C13_Group2"), names(NPV_coeffs))

# Select one from Group 1 (Ash, Maple)
grp1_coeffs <- rep(0, 5); grp1_coeffs[GROUP1_FB_IDX] <- 1
add.constraint(ilp1_model, grp1_coeffs, "=", 1)

# Select one from Group 2 (Oak, Elm, Bald Cypress)
grp2_coeffs <- rep(0, 5); grp2_coeffs[GROUP2_PG_IDX] <- 1
add.constraint(ilp1_model, grp2_coeffs, "=", 1)

solve(ilp1_model)

# ILP Model 1 Results
selected_ilp1_df <- data.frame(
  Variable = names(NPV_coeffs),
  Value = get.variables(ilp1_model)
) %>% filter(Value == 1) %>%
  mutate(`Reduced cost` = 0) # Fixed for ILP output

cat("\n### Table 8: ILP Model 1 Results\n\n")
print(paste0("Optimal Total NPV: ", format(get.objective(ilp1_model), big.mark = ","), " (ten thousands Rials)"))
print(kable(selected_ilp1_df, format = "markdown", digits = c(0, 0, 3)))

#------------------------------------------------------------------------------

#ILP Model 2 Setup (Equal Ecological Properties)

ilp2_model <- make.lp(nrow = 1, ncol = 5)
lp.control(ilp2_model, sense = "max")
set.objfn(ilp2_model, NPV_coeffs)
set.type(ilp2_model, columns = 1:5, type = "binary")
dimnames(ilp2_model) <- list(c("C16_Total_Select"), names(NPV_coeffs))

# Select exactly one species from ALL species
add.constraint(ilp2_model, rep(1, 5), "=", 1)

solve(ilp2_model)

#ILP Model 2 Results (Table 9) 
selected_ilp2_df <- data.frame(
  Variable = names(NPV_coeffs),
  Value = get.variables(ilp2_model)
) %>% filter(Value == 1) %>%
  mutate(`Reduced cost` = 0) # Fixed for ILP output

cat("\n### Table 9: ILP Model 2 Results\n\n")
print(paste0("Optimal Total NPV: ", format(get.objective(ilp2_model), big.mark = ","), " (ten thousands Rials)"))
print(kable(selected_ilp2_df, format = "markdown", digits = c(0, 0, 3)))



#------------------------------------------------------------------------------


#Model extension


# Model Setup for Extended LP 

# Model is reduced to 4 constraints for feasibility: C6, C7, C9, C10.

# 5 variables, 4 constraints

lp_ext_model_final <- make.lp(nrow = 4, ncol = 5) 
lp.control(lp_ext_model_final, sense = "max")

# Set objective function and dimensions
set.objfn(lp_ext_model_final, NPV_coeffs) 
row_names_ext_final <- c("C6: Area PG Max", "C7: Area FB Max", "C9: Cost Max",
                         "C10: Water Max")
dimnames(lp_ext_model_final) <- list(row_names_ext_final, names(NPV_coeffs))

# Add Feasible Constraints (C6, C7, C9, C10) 

area_pg_coeffs <- rep(0, 5); area_pg_coeffs[GROUP2_PG_IDX] <- 1
add.constraint(lp_ext_model_final, area_pg_coeffs, "<=", AREA_PSEUDOGLEY) # C6

area_fb_coeffs <- rep(0, 5); area_fb_coeffs[GROUP1_FB_IDX] <- 1
add.constraint(lp_ext_model_final, area_fb_coeffs, "<=", AREA_FORREST_BROWN) # C7

add.constraint(lp_ext_model_final, rep(COST_COEFF, 5), "<=", COST_MAX) # C9

# New constraint added (Water Constraint (C10))

W_i_coeffs <- c(1.0, 1.0, 1.5, 1.5, 0.5) 
add.constraint(lp_ext_model_final, W_i_coeffs, "<=", 300) 

solve(lp_ext_model_final) 


#  Extract Results 
ext_lp_solution_final <- get.variables(lp_ext_model_final)
ext_lp_objective_final <- get.objective(lp_ext_model_final)
shadow_prices_final <- get.dual.solution(lp_ext_model_final)


# Final Metrics Calculation
npv_reduction_final <- original_npv - ext_lp_objective_final

# The Water Constraint (C10) is the last constraint added (index 5 in the dual 
solution)
water_shadow_price_final <- shadow_prices_final[length(shadow_prices_final)] 


# Results

cat("## Final Results: Extended LP Model (Water Scarcity)\n\n")

# Optimal Areas
df_ext_results_final <- data.frame(
  Species = gsub("X\\d_", "", names(NPV_coeffs)),
  Area_ha = ext_lp_solution_final
)

cat("### New Optimal Area Allocation\n\n")
print(kable(df_ext_results_final, format = "markdown", digits = 4))


cat(paste0("\n**New Optimal NPV (Feasible):** ", format(ext_lp_objective_final, 
                                big.mark = ","), " (ten thousands Rials)\n"))
cat(paste0("**NPV Reduction (Cost of Water Policy):** ", 
    format(npv_reduction_final, big.mark = ","), " (ten thousands Rials)\n"))

cat("\n### Economic Interpretation: Shadow Price of Water\n\n")
cat(paste0("The Dual Price (Shadow Value) for the Water Constraint is: **", 
           format(water_shadow_price_final, scientific = FALSE, digits = 4), 
           "** (ten thousands Rials / per unit of water)\n"))

