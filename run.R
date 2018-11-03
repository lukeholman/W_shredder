library(drake, verbose = FALSE, warn.conflicts = FALSE)
library(workflowr)
# library(future.batchtools, verbose = FALSE, warn.conflicts = FALSE)
library(dplyr, verbose = FALSE, warn.conflicts = FALSE)
library(ggplot2, verbose = FALSE, warn.conflicts = FALSE)

# function to source .rmd files (found on Stack Overflow)
source_rmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = "allow")
  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output = tempR, quiet = TRUE)
  source(tempR, local = globalenv(), ...)
}
source_rmd("analysis/custom_functions.Rmd")
           
# Helper function to render documents from .rmd files
neat_render <- function(path) rmarkdown::render(input = path, output_file = NULL, quiet = TRUE)


# Define the drake workflow plan
my_plan <- drake_plan(
  
  # Load in the raw data
  raw_data = readRDS(file_in("data/iris.rds")),
  
  # Clean up the raw data
  cleaned_data = raw_data %>% clean_raw_data(),
  
  # Run statistical analysis
  model_results = cleaned_data %>% run_model(),
  
  # Make figure 1
  fig1 = cleaned_data %>% make_fig1("figures/fig1.pdf"),
  
  # Make figure S1
  figS1 = cleaned_data %>% make_fig_S1("figures/figS1.pdf"),
  
  # Render an HTML report showing all the custom functions
  pretty_functions = neat_render(knitr_in("analysis/custom_functions.Rmd")),
  
  # Render the manuscript as a PDF
  pretty_manuscript = neat_render(knitr_in("manuscript/my_manuscript.Rmd")),
  
  # Render the supplementary material as a PDF
  pretty_ESM = neat_render(knitr_in("manuscript/supplementary_material.Rmd")),
  strings_in_dots = "literals"
)

clean(destroy = TRUE) # running this line will delete the drake cache, allowing a complete re-run
make(my_plan, seed = 1) # See also the 'jobs' option to re-run everything in parallel

if(length(outdated(drake_config(my_plan))) == 0){
  output_files <- c("analysis/custom_functions.html",
                    "manuscript/my_manuscript.pdf",
                    "manuscript/supplementary_material.pdf")
  
  file.copy(output_files, "output")
  unlink(output_files)
  print("Everything worked fine!")
}
