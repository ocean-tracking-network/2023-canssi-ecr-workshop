## Requirements------

### R version: 3.6.x or newer (recommend 4.0.x) and RStudio.
### Windows users: Please also download the Rtools version compatible with your R version https://cran.r-project.org/bin/windows/Rtools/history.html (not for MacOS)

# Advanced Telemetry Workshop ONLY: You must install also GDAL software, which can take a long time. See the extra setup document provided.

#Due to recent issues with certain spatial packages being delisted from CRAN, you will need to install the archived version of rgeos before the package will work. 
#You can get the most recent archive from https://cran.r-project.org/src/contrib/Archive/rgeos/. 
#At time of writing, 0.6-4 is the most recent. 
#You will then need to install the rgeos package. Make sure you change the path in the first line of the script.

# Once R/RStudio is installed: open RStudio and run this install script. Please run it line-by-line instead of all at once in case there are errors.

#Note: When running through the installs, you may encounter a prompt asking you to upgrade dependent packages.
      #Choosing Option `3: None`, works in most situations and will prevent upgrades of packages you weren't explicitly looking to upgrade.



## Beginner R Workshop Requirements ----
# Install the archived rgeos package: 
install.packages("YOUR/PATH/TO/rgeos_0.6-4.tar.gz", repos = NULL, type = "source")

# Tidyverse (data cleaning and arrangement)
install.packages('tidyverse')

# Lubridate - part of Tidyverse, improves the process of creating date objects
install.packages('lubridate')

# GGmap - complimentary to ggplot2, which is in the Tidyverse
install.packages('ggmap')

# Plotly - Interactive web-based data visualization
install.packages('plotly')

# ReadXL - reads Excel format
install.packages("readxl")

# Viridis - color scales in this package are easier to read by those with colorblindness, and print well in grey scale.
install.packages("viridis")

# glatos - acoustic telemetry package that does filtering, vis, array simulation, etc.
install.packages('remotes')
library(remotes) 
remotes::install_github('ocean-tracking-network/glatos', build_vignettes = TRUE)

#Additional packages for mapping.
install.packages('mapview')
install.packages('spdplyr')
install.packages('geodata')

#SP and Raster packages for mapping.
install.packages('sp')
install.packages('raster')

# Install packages for building/displaying R Markdown
install.packages('rmarkdown')
install.packages('knitr', dependencies = TRUE)

# Install additonal packages for `remora` lesson
install.packages('readr')
install.packages('sf')
install.packages('stars')

# Install packages for animating detection data
remotes::install_github("jmlondon/pathroutr")

install.packages('gganimate')
install.packages('ggspatial')

#Install packages for YAPS lessons
remotes::install_github("robertlennox/miscYAPS")

#Install packages for GAMS lessons
install.packages('mgcv')
install.packages('Thermimage')
install.packages('lunar')
install.packages('gratia')

#The basic animations lesson requires a Stadia Maps API key. You can set up your own if you want, or use the
#one provided below:
library(ggmap)
ggmap::register_stadiamaps("b01d1235-69e8-49ea-b3bd-c35b42424b00")
                                                            

### Dataset and Code -----
# Once the packages are installed, you can download the datasets and code for this workshop from https://github.com/ocean-tracking-network/2023-canssi-ecr-workshop/tree/master.
# 1) Select the GREEN "code" button at the top and choose "Download ZIP"
# 2) Unzip the folder and move to secure location on your computer (Documents, Desktop etc.)
# 3) Copy the folder's path and use it to set your working directly in R using `setwd('<path-to-folder>')`.

# If you are familiar with Git and Github, feel free to clone this repository as you normally would,
# by running `git clone https://github.com/ocean-tracking-network/2023-canssi-ecr-workshop.git` in a terminal program
# and following from step 3 above.
