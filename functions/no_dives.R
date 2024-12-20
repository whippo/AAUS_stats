no_dives <- function(df) {
 
  # Sum the groups by row
  df <- df %>%
    select(-ContactKey) %>%
    rowwise() %>%
    mutate(
      sum_diveGas = sum(c_across(all_of(diveGas)), na.rm = TRUE),
      sum_diveMode = sum(c_across(all_of(diveMode)), na.rm = TRUE),
      sum_divePlan = sum(c_across(all_of(divePlan)), na.rm = TRUE),
      sum_diveDepth = sum(c_across(all_of(diveDepth)), na.rm = TRUE),
      sum_divePurpose = sum(c_across(all_of(divePurpose)), na.rm = TRUE),
      sum_diveEnviron = sum(c_across(all_of(diveEnviron)), na.rm = TRUE),
      sum_diveDecomp = sum(c_across(all_of(diveDecomp)), na.rm = TRUE)
    )
  
  # Compare the sums and print the results
  df <- df %>%
    mutate(Gas_Mode = case_when(
      sum_diveGas > sum_diveMode ~ "greater than",
      sum_diveGas < sum_diveMode ~ "less than",
      TRUE ~ "equal to"
    ),
    Gas_Plan = case_when(
      sum_diveGas > sum_divePlan ~ "greater than",
      sum_diveGas < sum_divePlan ~ "less than",
      TRUE ~ "equal to"
    ),
    Gas_Depth = case_when(
      sum_diveGas > sum_diveDepth ~ "greater than",
      sum_diveGas < sum_diveDepth ~ "less than",
      TRUE ~ "equal to"
    ),
    Gas_Purpose = case_when(
      sum_diveGas > sum_divePurpose ~ "greater than",
      sum_diveGas < sum_divePurpose ~ "less than",
      TRUE ~ "equal to"
    ),
    Gas_Environ = case_when(
      sum_diveGas > sum_diveEnviron ~ "greater than",
      sum_diveGas < sum_diveEnviron ~ "less than",
      TRUE ~ "equal to"
    ),
    Gas_Decomp = case_when(
      sum_diveGas > sum_diveDecomp ~ "greater than",
      sum_diveGas < sum_diveDecomp ~ "less than",
      TRUE ~ "equal to"
    ),
    Mode_Plan = case_when(
      sum_diveMode > sum_divePlan ~ "greater than",
      sum_diveMode < sum_divePlan ~ "less than",
      TRUE ~ "equal to"
    ),
    Mode_Depth = case_when(
      sum_diveMode > sum_diveDepth ~ "greater than",
      sum_diveMode < sum_diveDepth ~ "less than",
      TRUE ~ "equal to"
    ),
    Mode_Purpose = case_when(
      sum_diveMode > sum_divePurpose ~ "greater than",
      sum_diveMode < sum_divePurpose ~ "less than",
      TRUE ~ "equal to"
    ),
    Mode_Environ = case_when(
      sum_diveMode > sum_diveEnviron ~ "greater than",
      sum_diveMode < sum_diveEnviron ~ "less than",
      TRUE ~ "equal to"
    ),
    Mode_Decomp = case_when(
      sum_diveMode > sum_diveDecomp ~ "greater than",
      sum_diveMode < sum_diveDecomp ~ "less than",
      TRUE ~ "equal to"
    ),
    Plan_Depth = case_when(
      sum_divePlan > sum_diveDepth ~ "greater than",
      sum_divePlan < sum_diveDepth ~ "less than",
      TRUE ~ "equal to"
    ),
    Plan_Purpose = case_when(
      sum_divePlan > sum_divePurpose ~ "greater than",
      sum_divePlan < sum_divePurpose ~ "less than",
      TRUE ~ "equal to"
    ),
    Plan_Environ = case_when(
      sum_divePlan > sum_diveEnviron ~ "greater than",
      sum_divePlan < sum_diveEnviron ~ "less than",
      TRUE ~ "equal to"
    ),
    Plan_Decomp = case_when(
      sum_divePlan > sum_diveDecomp ~ "greater than",
      sum_divePlan < sum_diveDecomp ~ "less than",
      TRUE ~ "equal to"
    ),
    Depth_Purpose = case_when(
      sum_diveDepth > sum_divePurpose ~ "greater than",
      sum_diveDepth < sum_divePurpose ~ "less than",
      TRUE ~ "equal to"
    ),
    Depth_Environ = case_when(
      sum_diveDepth > sum_diveEnviron ~ "greater than",
      sum_diveDepth < sum_diveEnviron ~ "less than",
      TRUE ~ "equal to"
    ),
    Depth_Decomp = case_when(
      sum_diveDepth > sum_diveDecomp ~ "greater than",
      sum_diveDepth < sum_diveDecomp ~ "less than",
      TRUE ~ "equal to"
    ),
    Purpose_Environ = case_when(
      sum_divePurpose > sum_diveEnviron ~ "greater than",
      sum_divePurpose < sum_diveEnviron ~ "less than",
      TRUE ~ "equal to"
    ),
    Purpose_Decomp = case_when(
      sum_divePurpose > sum_diveDecomp ~ "greater than",
      sum_divePurpose < sum_diveDecomp ~ "less than",
      TRUE ~ "equal to"
    ),
    Environ_Decomp = case_when(
      sum_diveEnviron > sum_diveDecomp ~ "greater than",
      sum_diveEnviron < sum_diveDecomp ~ "less than",
      TRUE ~ "equal to"
    ),
    row_number = row_number()
    ) 
  # Select relevant columns for output
  output <- df %>%
    select(label = all_of(label_col), sum_diveGas, sum_diveMode, sum_divePlan, sum_diveDepth, sum_divePurpose, sum_diveEnviron, sum_diveDecomp,
           Gas_Mode, Gas_Plan, Gas_Depth, Gas_Purpose, Gas_Environ, Gas_Decomp,
           Mode_Plan, Mode_Depth, Mode_Purpose, Mode_Environ, Mode_Decomp,
           Plan_Depth, Plan_Purpose, Plan_Environ, Plan_Decomp,
           Depth_Purpose, Depth_Environ, Depth_Decomp,
           Purpose_Environ, Purpose_Decomp,
           Environ_Decomp)
  
  
  # Print the output as a table
  print(output)
}
