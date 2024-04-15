make_params <- function(
    pathway,
    prev,
    test1,
    test2,
    test3=NULL,
    test4=NULL,
    test5=NULL,
    daly_avert_per_tx,
    tx_eff,
    n = 1000
){

  stopifnot(pathway=="full" | pathway=="parallel" | pathway=="rule-out")

  # TO DO: make DALY/tx linkage/costing options

  # check tests 1 and 2
  check_test_params(test1)
  check_test_params(test2)

  # add names if they don't already exist
  if(is.null(test1$label)){ test1 = c(test1, label="Test 1")}
  if(is.null(test2$label)){ test2 = c(test2, label="Test 2")}

  if(pathway=="full"){

    # check tests 3-5
    stopifnot(!is.null(test3) & !is.null(test4) & !is.null(test5))
    check_test_params(test3)
    check_test_params(test4)
    check_test_params(test5)

    # add names if they don't already exist
    if(is.null(test3$label)){ test3 = c(test3, label="Test 3")}
    if(is.null(test4$label)){ test4 = c(test4, label="Test 4")}
    if(is.null(test5$label)){ test5 = c(test5, label="Test 5")}

    params = list(
      pathway="full",
      prev = prev,
      test1 = test1,
      test2 = test2,
      test3 = test3,
      test4 = test4,
      test5 = test5,
      daly_avert_per_tx = daly_avert_per_tx,
      tx_eff = tx_eff,
      n = n
    )
  }

  if(pathway=="parallel"){

    # check test 3
    stopifnot(!is.null(test3))
    check_test_params(test3)

    # add names if they don't already exist
    if(is.null(test3$label)){ test3 = c(test3, label="Test 3")}

    # no T1-T2 (and by proxy T1-T4) LTFU with parallel testing
    test2$ltfu = 0

    params = list(
      pathway = "parallel",
      prev = prev,
      test1 = test1,
      test2 = test2,
      test3 = test3,
      test4 = test2,
      test5 = test3,
      daly_avert_per_tx = daly_avert_per_tx,
      tx_eff = tx_eff,
      n = n
    )
  }

  if(pathway=="rule-out"){
    params = list(
      pathway = "rule-out",
      prev = prev,
      test1 = test1,
      test2 = test2,
      test3 = list(label="N/A", sens=0, spec=1, ltfu=0, cost_test=0),
      test4 = list(label="N/A", sens=0, spec=1, ltfu=0, cost_test=0),
      test5 = list(label="N/A", sens=0, spec=1, ltfu=0, cost_test=0),
      daly_avert_per_tx = daly_avert_per_tx,
      tx_eff = tx_eff,
      n = n
    )
  }

  return(params)

}


check_test_params <- function(test){

  # sens, spec, ltfu must be provided
  stopifnot(any(names(test)=="sens"))
  stopifnot(any(names(test)=="spec"))
  stopifnot(any(names(test)=="ltfu"))

  # sens, spec, ltfu must be within 0-1
  stopifnot(test$sens>=0 & test$sens<=1)
  stopifnot(test$spec>=0 & test$spec<=1)
  stopifnot(test$ltfu>=0 & test$ltfu<=1)

}


make_pathway_diagram <- function(params){
  # TO DO: figure out where package dependencies go?

  # manually unlist params
  test1 = params$test1
  test2 = params$test2
  test3 = params$test3
  test4 = params$test4
  test5 = params$test5

  # full, 5-test pathway
  if(params$pathway=="full"){

    ndf <- create_node_df(
      n         = 11,
      label     = c(test1$label, test2$label, test3$label, test4$label, test5$label, rep("Positive", 3), rep("Negative", 3)),
      shape     = rep("rectangle", 11),
      style     = "empty",
      fontsize  = 14,
      fixedsize = TRUE,
      height    = 0.75,
      width     = 1.25,
      color     = c(rep("gray80",5), rep("#4F9E89", 3), rep("#D59A6B", 3)),
      x         = c(0, 2, 4, 2, 4, 4, 6, 6, 6, 6, 4),
      y         = c(3, 5, 4, 1, 2, 6, 5, 2.5, 3.5, 1, 0.5)
    )

    # Create an edge data frame (edf)
    edf <- create_edge_df(
      from     = c(1, 1, 2, 4, 2, 3, 5, 3, 4,  5),
      to       = c(2, 4, 3, 5, 6, 7, 8, 9, 11, 10),
      label    = c("+", "-", "-", rep("+", 4), rep("-", 3)),
      color    = c("#4F9E89", "#D59A6B", "#D59A6B", "#4F9E89", rep("#4F9E89", 3), rep("#D59A6B", 3)),
      fontsize = 14,
      minlen   = 1
    )
  }

  # parallel 3-test pathway
  if(params$pathway=="parallel"){
    ndf <- create_node_df(
      n         = 6,
      label     = c(paste0(test1$label, " +\n", test2$label), test3$label, rep("Positive", 2), rep("Negative", 2)),
      shape     = rep("rectangle", 6),
      style     = "empty",
      fontsize  = 14,
      fixedsize = TRUE,
      height    = c(1.25, rep(0.75, 5)),
      width     = 1.25,
      color     = c(rep("gray80", 2), rep("#4F9E89", 2), rep("#D59A6B", 2)),
      x         = c(0, 2.5, 2.5, 5, 2.5, 5),
      y         = c(3, 3,   5,   4, 1,   2)
    )

    # Create an edge data frame (edf)
    edf <- create_edge_df(
      from     = c(1, 1, 1, 2, 2),
      to       = c(2, 3, 5, 4, 6),
      label    = c("+/-", "+/+", "-/-", "+", "-"),
      color    = c("gray80", "#4F9E89", "#D59A6B", "#4F9E89", "#D59A6B"),
      fontsize = 14,
      minlen   = 1,
    )
  }

  # rule-out 3 test pathway
  if(params$pathway=="rule-out"){
    # remove 5, 8, 10-11
    ndf <- create_node_df(
      n         = 7,
      label     = c(test1$label, test2$label, test3$label, "Negative", rep("Positive", 2), "Negative"),
      shape     = rep("rectangle", 7),
      style     = "empty",
      fontsize  = 14,
      fixedsize = TRUE,
      height    = 0.75,
      width     = 1.25,
      color     = c(rep("gray80",3), "#D59A6B", rep("#4F9E89", 2), "#D59A6B"),
      x         = c(0, 2, 4, 2, 4, 6, 6),
      y         = c(3, 5, 4, 1, 6, 5, 3.5)
    )

    # Create an edge data frame (edf)
    edf <- create_edge_df(
      from     = c(1, 1, 2, 2, 3, 3),
      to       = c(2, 4, 3, 5, 6, 7),
      label    = c("+", "-", "-", "+", "+", "-"),
      color    = c("#4F9E89", "#D59A6B", "#D59A6B", "#4F9E89", "#4F9E89","#D59A6B"),
      fontsize = 14,
      minlen   = 1
    )
  }

  # Create a graph with the ndf and edf
  graph <- create_graph(
    nodes_df = ndf,
    edges_df = edf
  )

  return(graph)
}
