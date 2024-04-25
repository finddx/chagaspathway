#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
    # golem_opts = list(translator=shiny.i18n::Translator$new(translation_json_path = "www/translations/translation.json"))
    # golem_opts = list(translator=shiny.i18n::Translator$new(translation_csvs_path = "www/translations/trans_es.csv"))
    # golem_opts = list(translator = shiny.i18n::Translator$new(translation_csvs_path =system.file("app/www/translations/trans_es.csv", package="chagaspathway")))
  )#translation_json_path
}
