
# library
if (!require("rmarkdown")) install.packages("rmarkdown")
library(rmarkdown )


#converting rmd the file to md

knitr::knit(input = "210415.Rmd",           
            output = "210415.md") 
