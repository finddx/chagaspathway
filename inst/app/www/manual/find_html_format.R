library(finddxtemplate)

dir <-"C:/Users/juan.vallarta/OneDrive - Foundation for Innovative New Diagnostics FIND/Documents/Repositories/chagaspathway/inst/app/www/manual/user_manual.Rmd"
#Knit
rmarkdown::render(input = dir,
                  output_format = html_document_find(code_folding="none"))
