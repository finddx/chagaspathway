# scenario_vars$pathway_type <- scenario_vars$pathway_type


format_app_params_react <- function(scenario_vars, global_vars, advance_vars){

  # assign each test a visit value, visit=1 if new visit or sample referral required
  # assign LTFU values (where LTFU=0 if visit=0, or ltfu if visit=1)
  # assign linkage to tx values (where linkage can only occur after test2, test3, test5, depending on facility level)
  # assign costs, where total_cost=cost_test if visit=0, or total_cost=cost_test+cost_visit if visit=1
  # update test names (use label if provided; if not, use test_type + number)

  # full:
  # test 1 - visit=1; test 2, 4 - visit=1 if test 1 is serological or if facility type changes;
  # test 3, 5 - visit=1 if 2/4 are serological or if facility type changes

  global_vars$population <- global_vars$population()
  global_vars$lftu <- global_vars$lftu()
  global_vars$link_treatment_high <- global_vars$link_treatment_high()
  global_vars$link_treatment_low <- global_vars$link_treatment_low()
  #
  advance_vars$prev_chagas <- advance_vars$prev_chaga()
  advance_vars$treat_effect <- advance_vars$treat_effect()
  advance_vars$untreated_pats <- advance_vars$untreated_pats()
  advance_vars$avg_dalys <- advance_vars$avg_dalys()
  advance_vars$cost_visit <- advance_vars$cost_visit()



  if(scenario_vars$pathway_type=="full"){

    scenario_vars$test1$test_type <-  scenario_vars$test1$test_type()
    scenario_vars$test1$label <-  scenario_vars$test1$label()
    scenario_vars$test1$facility_type <-  scenario_vars$test1$facility_type()
    scenario_vars$test1$sens <-  scenario_vars$test1$sens()
    scenario_vars$test1$spec <-  scenario_vars$test1$spec()
    scenario_vars$test1$cost_test <-  scenario_vars$test1$cost_test()

    scenario_vars$test2$test_type <-  scenario_vars$test2$test_type()
    scenario_vars$test2$label <-  scenario_vars$test2$label()
    scenario_vars$test2$facility_type <-  scenario_vars$test2$facility_type()
    scenario_vars$test2$sens <-  scenario_vars$test2$sens()
    scenario_vars$test2$spec <-  scenario_vars$test2$spec()
    scenario_vars$test2$cost_test <-  scenario_vars$test2$cost_test()

    scenario_vars$test3$test_type <-  scenario_vars$test3$test_type()
    scenario_vars$test3$label <-  scenario_vars$test3$label()
    scenario_vars$test3$facility_type <-  scenario_vars$test3$facility_type()
    scenario_vars$test3$sens <-  scenario_vars$test3$sens()
    scenario_vars$test3$spec <-  scenario_vars$test3$spec()
    scenario_vars$test3$cost_test <-  scenario_vars$test3$cost_test()

    scenario_vars$test4$test_type <-  scenario_vars$test4$test_type()
    scenario_vars$test4$label <-  scenario_vars$test4$label()
    scenario_vars$test4$facility_type <-  scenario_vars$test4$facility_type()
    scenario_vars$test4$sens <-  scenario_vars$test4$sens()
    scenario_vars$test4$spec <-  scenario_vars$test4$spec()
    scenario_vars$test4$cost_test <-  scenario_vars$test4$cost_test()

    scenario_vars$test5$test_type <-  scenario_vars$test5$test_type()
    scenario_vars$test5$label <-  scenario_vars$test5$label()
    scenario_vars$test5$facility_type <-  scenario_vars$test5$facility_type()
    scenario_vars$test5$sens <-  scenario_vars$test5$sens()
    scenario_vars$test5$spec <-  scenario_vars$test5$spec()
    scenario_vars$test5$cost_test <-  scenario_vars$test5$cost_test()

    # visits
    scenario_vars$test1$visit = 1
    scenario_vars$test2$visit = ifelse(scenario_vars$test1$test_type=="Serological test" | scenario_vars$test1$facility_type!=scenario_vars$test2$facility_type, 1, 0)
    scenario_vars$test3$visit = ifelse(scenario_vars$test2$test_type=="Serological test" | scenario_vars$test2$facility_type!=scenario_vars$test3$facility_type, 1, 0)
    scenario_vars$test4$visit = ifelse(scenario_vars$test1$test_type=="Serological test" | scenario_vars$test1$facility_type!=scenario_vars$test4$facility_type, 1, 0)
    scenario_vars$test5$visit = ifelse(scenario_vars$test4$test_type=="Serological test" | scenario_vars$test4$facility_type!=scenario_vars$test5$facility_type, 1, 0)

    # ltfu (occuring prior to testX)
    scenario_vars$test1$ltfu = 0
    scenario_vars$test2$ltfu = ifelse(scenario_vars$test2$visit==1, global_vars$ltfu, 0)
    scenario_vars$test3$ltfu = ifelse(scenario_vars$test3$visit==1, global_vars$ltfu, 0)
    scenario_vars$test4$ltfu = ifelse(scenario_vars$test4$visit==1, global_vars$ltfu, 0)
    scenario_vars$test5$ltfu = ifelse(scenario_vars$test5$visit==1, global_vars$ltfu, 0)

    # linkage to tx (only after tests 2, 3, 5)
    scenario_vars$test1$tx_link = 0
    scenario_vars$test2$tx_link = ifelse(scenario_vars$test2$facility_type=="Low complexity", global_vars$link_treatment_low, global_vars$link_treatment_high)
    scenario_vars$test3$tx_link = ifelse(scenario_vars$test3$facility_type=="Low complexity", global_vars$link_treatment_low, global_vars$link_treatment_high)
    scenario_vars$test4$tx_link = 0
    scenario_vars$test5$tx_link = ifelse(scenario_vars$test5$facility_type=="Low complexity", global_vars$link_treatment_low, global_vars$link_treatment_high)

    # costs
    scenario_vars$test1$total_cost = scenario_vars$test1$cost_test + scenario_vars$test1$visit*advance_vars$cost_visit
    scenario_vars$test2$total_cost = scenario_vars$test2$cost_test + scenario_vars$test2$visit*advance_vars$cost_visit
    scenario_vars$test3$total_cost = scenario_vars$test3$cost_test + scenario_vars$test3$visit*advance_vars$cost_visit
    scenario_vars$test4$total_cost = scenario_vars$test4$cost_test + scenario_vars$test4$visit*advance_vars$cost_visit
    scenario_vars$test5$total_cost = scenario_vars$test5$cost_test + scenario_vars$test5$visit*advance_vars$cost_visit

    # names
    if(is.null(scenario_vars$test1$label)){scenario_vars$test1$label = paste0(scenario_vars$test1$test_type, " 1")}
    if(is.null(scenario_vars$test2$label)){scenario_vars$test2$label = paste0(scenario_vars$test1$test_type, " 2")}
    if(is.null(scenario_vars$test3$label)){scenario_vars$test3$label = paste0(scenario_vars$test1$test_type, " 3")}
    if(is.null(scenario_vars$test4$label)){scenario_vars$test4$label = paste0(scenario_vars$test1$test_type, " 4")}
    if(is.null(scenario_vars$test5$label)){scenario_vars$test5$label = paste0(scenario_vars$test1$test_type, " 5")}

  }

  # parallel:
  # test 1 - visit=1; test 2 - visit=0; test 3 - visit=1 if test 1 or 2 are serological or if facility type changes
  if(scenario_vars$pathway_type=="parallel"){

    scenario_vars$test1$test_type <-  scenario_vars$test1$test_type()
    scenario_vars$test1$label <-  scenario_vars$test1$label()
    scenario_vars$test1$facility_type <-  scenario_vars$test1$facility_type()
    scenario_vars$test1$sens <-  scenario_vars$test1$sens()
    scenario_vars$test1$spec <-  scenario_vars$test1$spec()
    scenario_vars$test1$cost_test <-  scenario_vars$test1$cost_test()

    scenario_vars$test2$test_type <-  scenario_vars$test2$test_type()
    scenario_vars$test2$label <-  scenario_vars$test2$label()
    scenario_vars$test2$facility_type <-  scenario_vars$test2$facility_type()
    scenario_vars$test2$sens <-  scenario_vars$test2$sens()
    scenario_vars$test2$spec <-  scenario_vars$test2$spec()
    scenario_vars$test2$cost_test <-  scenario_vars$test2$cost_test()

    scenario_vars$test3$test_type <-  scenario_vars$test3$test_type()
    scenario_vars$test3$label <-  scenario_vars$test3$label()
    scenario_vars$test3$facility_type <-  scenario_vars$test3$facility_type()
    scenario_vars$test3$sens <-  scenario_vars$test3$sens()
    scenario_vars$test3$spec <-  scenario_vars$test3$spec()
    scenario_vars$test3$cost_test <-  scenario_vars$test3$cost_test()


    # create dummy test4, test5 objects for passing to make_params
    scenario_vars$test4 = NULL
    scenario_vars$test5 = NULL

    # visits
    scenario_vars$test1$visit = 1
    scenario_vars$test2$visit = 0
    scenario_vars$test3$visit = ifelse(
      scenario_vars$test3$facility_type!=scenario_vars$test2$facility_type |
        scenario_vars$test3$facility_type!=scenario_vars$test1$facility_type |
        scenario_vars$test1$test_type=="Serological test" |
        scenario_vars$test2$test_type=="Serological test",
      1,
      0)

    # ltfu (occuring prior to testX)
    scenario_vars$test1$ltfu = 0
    scenario_vars$test2$ltfu = ifelse(scenario_vars$test2$visit==1, global_vars$ltfu, 0)
    scenario_vars$test3$ltfu = ifelse(scenario_vars$test3$visit==1, global_vars$ltfu, 0)

    # linkage to tx (only after tests 2, 3)
    scenario_vars$test1$tx_link = 0
    scenario_vars$test2$tx_link = ifelse(scenario_vars$test2$facility_type=="Low complexity", global_vars$link_treatment_low, global_vars$link_treatment_high)
    scenario_vars$test3$tx_link = ifelse(scenario_vars$test3$facility_type=="Low complexity", global_vars$link_treatment_low, global_vars$link_treatment_high)

    # costs
    scenario_vars$test1$total_cost = scenario_vars$test1$cost_test + scenario_vars$test1$visit*advance_vars$cost_visit
    scenario_vars$test2$total_cost = scenario_vars$test2$cost_test + scenario_vars$test2$visit*advance_vars$cost_visit
    scenario_vars$test3$total_cost = scenario_vars$test3$cost_test + scenario_vars$test3$visit*advance_vars$cost_visit

    # names
    if(is.null(scenario_vars$test1$label)){scenario_vars$test1$label = paste0(scenario_vars$test1$test_type, " 1")}
    if(is.null(scenario_vars$test2$label)){scenario_vars$test2$label = paste0(scenario_vars$test1$test_type, " 2")}
    if(is.null(scenario_vars$test3$label)){scenario_vars$test3$label = paste0(scenario_vars$test1$test_type, " 3")}
  }

  # rule-out:
  # test 1 - visit=1; test 2 - visit=1 if facility type changes or if test 1 serological
  if(scenario_vars$pathway_type=="rule-out"){

    scenario_vars$test1$test_type <-  scenario_vars$test1$test_type()
    scenario_vars$test1$label <-  scenario_vars$test1$label()
    scenario_vars$test1$facility_type <-  scenario_vars$test1$facility_type()
    scenario_vars$test1$sens <-  scenario_vars$test1$sens()
    scenario_vars$test1$spec <-  scenario_vars$test1$spec()
    scenario_vars$test1$cost_test <-  scenario_vars$test1$cost_test()

    scenario_vars$test2$test_type <-  scenario_vars$test2$test_type()
    scenario_vars$test2$label <-  scenario_vars$test2$label()
    scenario_vars$test2$facility_type <-  scenario_vars$test2$facility_type()
    scenario_vars$test2$sens <-  scenario_vars$test2$sens()
    scenario_vars$test2$spec <-  scenario_vars$test2$spec()
    scenario_vars$test2$cost_test <-  scenario_vars$test2$cost_test()

    # create dummy test3, test4, test5 objects for passing to make_params
    scenario_vars$test3 = NULL
    scenario_vars$test4 = NULL
    scenario_vars$test5 = NULL

    # visits
    scenario_vars$test1$visit = 1
    scenario_vars$test2$visit = ifelse(scenario_vars$test1$test_type=="Serological test" | scenario_vars$test2$facility_type!=scenario_vars$test1$facility_type, 1, 0)

    # ltfu (occuring prior to testX)
    scenario_vars$test1$ltfu = 0
    scenario_vars$test2$ltfu = ifelse(scenario_vars$test2$visit==1, global_vars$ltfu, 0)

    # linkage to tx (only after test 2)
    scenario_vars$test1$tx_link = 0
    scenario_vars$test2$tx_link = ifelse(scenario_vars$test2$facility_type=="Low complexity", global_vars$link_treatment_low, global_vars$link_treatment_high)

    # costs
    scenario_vars$test1$total_cost = scenario_vars$test1$cost_test + scenario_vars$test1$visit*advance_vars$cost_visit
    scenario_vars$test2$total_cost = scenario_vars$test2$cost_test + scenario_vars$test2$visit*advance_vars$cost_visit

    # names
    if(is.null(scenario_vars$test1$label)){scenario_vars$test1$label = paste0(scenario_vars$test1$test_type, " 1")}
    if(is.null(scenario_vars$test2$label)){scenario_vars$test2$label = paste0(scenario_vars$test1$test_type, " 2")}
  }



  out <- list(
    pathway = scenario_vars$pathway_type,
    prev = advance_vars$prev_chagas,
    test1 = scenario_vars$test1,
    test2 = scenario_vars$test2,
    test3 = scenario_vars$test3,
    test4 = scenario_vars$test4,
    test5 = scenario_vars$test5,
    daly_avert_per_tx = advance_vars$avg_dalys,
    tx_eff = advance_vars$treat_effect,
    n = 1000
  )

  return(out)

}






# format_app_params_react <- function(scenario_vars, global_vars, advance_vars){
#
#   # assign each test a visit value, visit=1 if new visit or sample referral required
#   # assign LTFU values (where LTFU=0 if visit=0, or ltfu if visit=1)
#   # assign linkage to tx values (where linkage can only occur after test2, test3, test5, depending on facility level)
#   # assign costs, where total_cost=cost_test if visit=0, or total_cost=cost_test+cost_visit if visit=1
#   # update test names (use label if provided; if not, use test_type + number)
#
#   # full:
#   # test 1 - visit=1; test 2, 4 - visit=1 if test 1 is serological or if facility type changes;
#   # test 3, 5 - visit=1 if 2/4 are serological or if facility type changes
#
#
#   if(scenario_vars$pathway_type=="full"){
#
#     # visits
#     scenario_vars$test1$visit = 1
#     scenario_vars$test2$visit = ifelse(scenario_vars$test1$test_type()=="Serological test" | scenario_vars$test1$facility_type()!=scenario_vars$test2$facility_type(), 1, 0)
#     scenario_vars$test3$visit = ifelse(scenario_vars$test2$test_type()=="Serological test" | scenario_vars$test2$facility_type()!=scenario_vars$test3$facility_type(), 1, 0)
#     scenario_vars$test4$visit = ifelse(scenario_vars$test1$test_type()=="Serological test" | scenario_vars$test1$facility_type()!=scenario_vars$test4$facility_type(), 1, 0)
#     scenario_vars$test5$visit = ifelse(scenario_vars$test4$test_type()=="Serological test" | scenario_vars$test4$facility_type()!=scenario_vars$test5$facility_type(), 1, 0)
#
#     # ltfu (occuring prior to testX)
#     scenario_vars$test1$ltfu = 0
#     scenario_vars$test2$ltfu = ifelse(scenario_vars$test2$visit==1, global_vars$ltfu(), 0)
#     scenario_vars$test3$ltfu = ifelse(scenario_vars$test3$visit==1, global_vars$ltfu(), 0)
#     scenario_vars$test4$ltfu = ifelse(scenario_vars$test4$visit==1, global_vars$ltfu(), 0)
#     scenario_vars$test5$ltfu = ifelse(scenario_vars$test5$visit==1, global_vars$ltfu(), 0)
#
#     # linkage to tx (only after tests 2, 3, 5)
#     scenario_vars$test1$tx_link = 0
#     scenario_vars$test2$tx_link = ifelse(scenario_vars$test2$facility_type()=="Low complexity", global_vars$link_treatment_low(), global_vars$link_treatment_high())
#     scenario_vars$test3$tx_link = ifelse(scenario_vars$test3$facility_type()=="Low complexity", global_vars$link_treatment_low(), global_vars$link_treatment_high())
#     scenario_vars$test4$tx_link = 0
#     scenario_vars$test5$tx_link = ifelse(scenario_vars$test5$facility_type()=="Low complexity", global_vars$link_treatment_low(), global_vars$link_treatment_high())
#
#     # costs
#     scenario_vars$test1$total_cost = scenario_vars$test1$cost_test() + scenario_vars$test1$visit*advance_vars$cost_visit()
#     scenario_vars$test2$total_cost = scenario_vars$test2$cost_test() + scenario_vars$test2$visit*advance_vars$cost_visit()
#     scenario_vars$test3$total_cost = scenario_vars$test3$cost_test() + scenario_vars$test3$visit*advance_vars$cost_visit()
#     scenario_vars$test4$total_cost = scenario_vars$test4$cost_test() + scenario_vars$test4$visit*advance_vars$cost_visit()
#     scenario_vars$test5$total_cost = scenario_vars$test5$cost_test() + scenario_vars$test5$visit*advance_vars$cost_visit()
#
#     # names
#     if(is.null(scenario_vars$test1$label)){scenario_vars$test1$label() = paste0(scenario_vars$test1$test_type(), " 1")}
#     if(is.null(scenario_vars$test2$label)){scenario_vars$test2$label() = paste0(scenario_vars$test1$test_type(), " 2")}
#     if(is.null(scenario_vars$test3$label)){scenario_vars$test3$label() = paste0(scenario_vars$test1$test_type(), " 3")}
#     if(is.null(scenario_vars$test4$label)){scenario_vars$test4$label() = paste0(scenario_vars$test1$test_type(), " 4")}
#     if(is.null(scenario_vars$test5$label)){scenario_vars$test5$label() = paste0(scenario_vars$test1$test_type(), " 5")}
#
#   }
#
#   # parallel:
#   # test 1 - visit=1; test 2 - visit=0; test 3 - visit=1 if test 1 or 2 are serological or if facility type changes
#   if(scenario_vars$pathway_type=="parallel"){
#
#     # create dummy test4, test5 objects for passing to make_params
#     scenario_vars$test4 = NULL
#     scenario_vars$test5 = NULL
#
#     # visits
#     scenario_vars$test1$visit = 1
#     scenario_vars$test2$visit = 0
#     scenario_vars$test3$visit = ifelse(
#       scenario_vars$test3$facility_type()!=scenario_vars$test2$facility_type() |
#         scenario_vars$test3$facility_type()!=scenario_vars$test1$facility_type() |
#         scenario_vars$test1$test_type()=="Serological test" |
#         scenario_vars$test2$test_type()=="Serological test",
#       1,
#       0)
#
#     # ltfu (occuring prior to testX)
#     scenario_vars$test1$ltfu = 0
#     scenario_vars$test2$ltfu = ifelse(scenario_vars$test2$visit==1, global_vars$ltfu(), 0)
#     scenario_vars$test3$ltfu = ifelse(scenario_vars$test3$visit==1, global_vars$ltfu(), 0)
#
#     # linkage to tx (only after tests 2, 3)
#     scenario_vars$test1$tx_link = 0
#     scenario_vars$test2$tx_link = ifelse(scenario_vars$test2$facility_type()=="Low complexity", global_vars$link_treatment_low(), global_vars$link_treatment_high())
#     scenario_vars$test3$tx_link = ifelse(scenario_vars$test3$facility_type()=="Low complexity", global_vars$link_treatment_low(), global_vars$link_treatment_high())
#
#     # costs
#     scenario_vars$test1$total_cost = scenario_vars$test1$cost_test() + scenario_vars$test1$visit*advance_vars$cost_visit()
#     scenario_vars$test2$total_cost = scenario_vars$test2$cost_test() + scenario_vars$test2$visit*advance_vars$cost_visit()
#     scenario_vars$test3$total_cost = scenario_vars$test3$cost_test() + scenario_vars$test3$visit*advance_vars$cost_visit()
#
#     # names
#     if(is.null(scenario_vars$test1$label())){scenario_vars$test1$label() = paste0(scenario_vars$test1$test_type(), " 1")}
#     if(is.null(scenario_vars$test2$label())){scenario_vars$test2$label() = paste0(scenario_vars$test1$test_type(), " 2")}
#     if(is.null(scenario_vars$test3$label())){scenario_vars$test3$label() = paste0(scenario_vars$test1$test_type(), " 3")}
#   }
#
#   # rule-out:
#   # test 1 - visit=1; test 2 - visit=1 if facility type changes or if test 1 serological
#   if(scenario_vars$pathway_type=="rule-out"){
#     # create dummy test3, test4, test5 objects for passing to make_params
#     scenario_vars$test3 = NULL
#     scenario_vars$test4 = NULL
#     scenario_vars$test5 = NULL
#
#     # visits
#     scenario_vars$test1$visit = 1
#     scenario_vars$test2$visit = ifelse(scenario_vars$test1$test_type()=="Serological test" | scenario_vars$test2$facility_type()!=scenario_vars$test1$facility_type(), 1, 0)
#
#     # ltfu (occuring prior to testX)
#     scenario_vars$test1$ltfu = 0
#     scenario_vars$test2$ltfu = ifelse(scenario_vars$test2$visit==1, global_vars$ltfu(), 0)
#
#     # linkage to tx (only after test 2)
#     scenario_vars$test1$tx_link = 0
#     scenario_vars$test2$tx_link = ifelse(scenario_vars$test2$facility_type()=="Low complexity", global_vars$link_treatment_low(), global_vars$link_treatment_high())
#
#     # costs
#     scenario_vars$test1$total_cost = scenario_vars$test1$cost_test() + scenario_vars$test1$visit*advance_vars$cost_visit()
#     scenario_vars$test2$total_cost = scenario_vars$test2$cost_test() + scenario_vars$test2$visit*advance_vars$cost_visit()
#
#     # names
#     if(is.null(scenario_vars$test1$label)){scenario_vars$test1$label() = paste0(scenario_vars$test1$test_type(), " 1")}
#     if(is.null(scenario_vars$test2$label)){scenario_vars$test2$label() = paste0(scenario_vars$test1$test_type(), " 2")}
#   }
#
#
#   out <- list(
#     pathway = scenario_vars$pathway_type,
#     prev = advance_vars$prev_chagas(),
#     test1 = scenario_vars$test1,
#     test2 = scenario_vars$test2,
#     test3 = scenario_vars$test3,
#     test4 = scenario_vars$test4,
#     test5 = scenario_vars$test5,
#     daly_avert_per_tx = advance_vars$avg_dalys(),
#     tx_eff = advance_vars$treat_effect(),
#     n = 1000
#   )
#
#
#   return(out)
#
# }
