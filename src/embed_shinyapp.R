
install.packages('rsconnect')

library(rsconnect)

setAccountInfo(name='avouacr', 
                          token='A73EE2E80E48D4B64576F96F00DFE4A5', 
                          secret='jZdzUiLziWY3HNsjdh9bPuiKulvFT+/llTIi96yQ')

deployApp('eduscope_shinyapp.Rmd')
