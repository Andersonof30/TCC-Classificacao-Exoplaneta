ver_packages = function(package){
if(!require(package, character.only = T)){
  install.packages(package, dependencies = T)
}
}


ver_packages(package = 'rstan')
ver_packages(package = 'dplyr')
ver_packages(package = 'readr')
ver_packages(package = 'caret')
ver_packages(package = 'pROC')