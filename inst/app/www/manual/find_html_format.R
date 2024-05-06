library(finddxtemplate)

dir <-"user_manual.Rmd"
#Knit
rmarkdown::render(input = dir,
                  output_format = html_document_find(code_folding="none"))
