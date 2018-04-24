# configuracion segun OS --------------------------------------------------


if(version$os == "mingw32"){
  # Hacer si es Windows
}else{
  #Cosas que pasan cuando corre en linux
}


options(stringsAsFactors = FALSE)
Sys.setenv(TZ='GMT')
options(java.parameters = "-Xmx256m")
# options(repos=structure(c(CRAN="https://cran.rstudio.com/")))
options(xtable.include.rownames=F)

