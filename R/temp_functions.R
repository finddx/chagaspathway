
calculate_pathways <- function(scenario1, scenario2=NULL, scenario3=NULL, advance, pathways, displayed_scenarios){

  result_list <- list(scenario1 = scenario1())

  result_list$advance <- advance
  result_list$pathways <- pathways

  tmp_params_scenario1 <- format_app_params_react(scenario_vars=result_list$scenario1, global_vars=result_list$pathways, advance_vars=result_list$advance, scn_lab="Scenario 1")

  params_scenario1 <- make_params(
    tmp_params_scenario1$pathway,
    tmp_params_scenario1$prev,
    tmp_params_scenario1$test1,
    tmp_params_scenario1$test2,
    tmp_params_scenario1$test3,
    tmp_params_scenario1$test4,
    tmp_params_scenario1$test5,
    tmp_params_scenario1$daly_avert_per_tx,
    tmp_params_scenario1$tx_eff,
    tmp_params_scenario1$n,
    tmp_params_scenario1$scenario
  )
  out_scenario1 <- run_pathway(params_scenario1)

  if (exists("scenario2_vars") & length(displayed_scenarios())>=2) {
    result_list$scenario2 <- scenario2()

    tmp_params_scenario2 <- format_app_params_react(scenario_vars=result_list$scenario2, global_vars=result_list$pathways, advance_vars=result_list$advance, scn_lab="Scenario 2")

    params_scenario2 <- make_params(
      tmp_params_scenario2$pathway,
      tmp_params_scenario2$prev,
      tmp_params_scenario2$test1,
      tmp_params_scenario2$test2,
      tmp_params_scenario2$test3,
      tmp_params_scenario2$test4,
      tmp_params_scenario2$test5,
      tmp_params_scenario2$daly_avert_per_tx,
      tmp_params_scenario2$tx_eff,
      tmp_params_scenario2$n,
      tmp_params_scenario2$scenario
    )
    out_scenario2 <- run_pathway(params_scenario2)

  }
  if (exists("scenario3_vars") & length(displayed_scenarios())>=3) {
    result_list$scenario3 <- scenario3()

    tmp_params_scenario3 <- format_app_params_react(scenario_vars=result_list$scenario3, global_vars=result_list$pathways, advance_vars=result_list$advance, scn_lab="Scenario 3")

    params_scenario3 <- make_params(
      tmp_params_scenario3$pathway,
      tmp_params_scenario3$prev,
      tmp_params_scenario3$test1,
      tmp_params_scenario3$test2,
      tmp_params_scenario3$test3,
      tmp_params_scenario3$test4,
      tmp_params_scenario3$test5,
      tmp_params_scenario3$daly_avert_per_tx,
      tmp_params_scenario3$tx_eff,
      tmp_params_scenario3$n,
      tmp_params_scenario3$scenario
    )
    out_scenario3 <- run_pathway(params_scenario3)

  }

  # result_list

  out_list <- list(
    params_scenario1 = if(exists("scenario1_vars")) params_scenario1 else NULL,
    params_scenario2 = if(exists("scenario2_vars") & length(displayed_scenarios())>=2) params_scenario2 else NULL,
    params_scenario3 = if(exists("scenario3_vars") & length(displayed_scenarios())>=3) params_scenario3 else NULL,
    out_scenario1 = if(exists("scenario1_vars")) out_scenario1 else NULL,
    out_scenario2 = if(exists("scenario2_vars") & length(displayed_scenarios())>=2) out_scenario2 else NULL,
    out_scenario3 = if(exists("scenario3_vars") & length(displayed_scenarios())>=3) out_scenario3 else NULL
  )
  return(out_list)
}
