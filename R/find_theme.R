library(fresh)
#colors
find_white <- "#FFFFFF"
find_black <- "#000000"
find_purple <- "#491E5D"
find_blue <- "#489FA9"
find_red <- "#D44F4E"
find_light_grey <- "#EDEFF1"
find_grey <- "#81969F"
find_dark_grey <- "#414B50"
find_yellow <- "#F8B136"
find_green <- "#379437"

# x <- data.frame(search_vars_bs(theme = c("default")))

find_theme <- create_theme(
  theme = "default",
  #Text color
  bs_vars_global(
    text_color =find_black,
  ),
  # bs_vars(
  #   
  # ),
  #Navigation page
  # bs_vars_navbar(
  #   default_bg = find_white,
  #   default_color = find_purple,
  #   default_link_color = find_purple,
  #   default_link_active_color = find_purple,
  #   default_link_hover_color = find_purple,
  #   default_link_hover_bg = find_light_grey,
  #   default_link_active_bg = find_light_grey,
  #   inverse_link_hover_color = find_purple
  # ),
  bs_vars_navbar(
    default_bg = find_purple,
    default_color = find_white,
    default_link_color = find_white,
    default_link_active_color = find_white,
    default_link_hover_color = find_white,
    default_link_hover_bg = "#351644",
    default_link_active_bg = "#351644",
    inverse_link_hover_color = find_white,
    
    default_border = find_white,
    inverse_border = find_purple,

  ),
  #tabs
  bs_vars_tabs(
    border_color = find_purple ,
    active_link_hover_bg = find_white,
    active_link_hover_color = find_purple ,
    active_link_hover_border_color = find_purple ,
    link_hover_border_color = find_purple 
  ),
  #Side bar
  bs_vars_wells(
    bg = find_light_grey,
    border = find_light_grey
  ),
  #Buttons
  bs_vars_color(
    gray_base = find_grey,
    brand_primary = find_purple ,
    brand_success = find_green,
    brand_info = find_white,
    brand_warning = find_yellow,
    brand_danger = find_red
  ),
  bs_vars_state(
    success_text = find_white,
    success_bg = find_green,
    success_border = find_green,
    info_text = find_black,
    info_bg = find_white,
    info_border = find_white,
    warning_text = find_white,
    warning_bg = find_yellow,
    warning_border = find_yellow,
    danger_text = find_white,
    danger_bg = find_red,
    danger_border = find_red
  ),
  bs_vars_button(
    default_color = find_white,
    default_bg = find_blue,
    default_border = find_blue,
    primary_color = find_white,
    primary_bg = find_blue,
    primary_border = find_blue
  ),
  #Widgets
  bs_vars_input(
    color = find_grey,
    border_focus = find_blue
  ),
   #Popups
  bs_vars_modal(
    header_border_color = find_purple ,
    footer_border_color = find_purple 
  ),
  output_file = "inst/app/www/find_theme.css"
)
