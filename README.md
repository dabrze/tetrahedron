Tetrahedron Measure Visualization
=================================

Visualize and analyze measures with respect to complete ranges of their values in
a barycentric coordinate system using a 3D tetrahedron. Explore the properties of
popular classifier performance <a href="https://www.sciencedirect.com/science/article/pii/S0020025518304602" target="_blank">(Brzezinski et al., 2018)</a> and rule interestingness measures <a href="https://www.amcs.uz.zgora.pl/?action=paper&paper=827" target="_blank">(Susmaga & Szczech, 2015)</a>,
or visualize custom functions.

Live app for this code: https://dabrze.shinyapps.io/Tetrahedron/

## Running the app

1. First install all the required packages by running `install_libraries.R` (you can also try to do this manually if you do not want to install a couple of unnecessary dependencies)
2. Open `Tetrahedron.Rproj` in [RStudio](https://www.rstudio.com/products/rstudio/download/)
3. Open the `ui.R` file
4. Press the *Run App* button 

The application works best in a standard browser (rather than RStudio chromium-based viewer). To open the app in a regular browser press the 
*Open in Browser* button in the chromium-based viewer or select *Run External* from the *Run App* dropdown in Rstudio.

If you do not want to use RStudio, you can use the R console, go into the Tetrahedron directory and run the command `shiny::runApp()`.