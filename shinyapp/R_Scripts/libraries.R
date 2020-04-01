# Check whether the library is installed, and if not, install it.
# Call all libraries needed for the application.
install_load <- function (packages)  {   
  for(package in packages){
    if(package %in% rownames(installed.packages()))
      do.call('library', list(package))
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  } 
}

libraries <- c('shiny','networkD3','devtools','MapColoring','igraph','viridis',
               'tidyverse','network','htmltools','tippy','sna')

install_load(libraries)
