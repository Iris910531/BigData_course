# requirements.R

# List of required packages
required_packages <- c(
  "shinydashboard", 
  "shiny", 
  "ggplot2", 
  "reticulate", 
  "plotly", 
  "gridExtra", 
  "colourpicker", 
  "showtext"
)

# Function to install missing packages
install_if_missing <- function(pkg) {
  if (!pkg %in% installed.packages()[, "Package"]) {
    install.packages(pkg)
  }
}

# Install missing packages
invisible(lapply(required_packages, install_if_missing))

# Load required packages
invisible(lapply(required_packages, library, character.only = TRUE))