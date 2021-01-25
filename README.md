
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DiAGRAM [![Travis build status](https://travis-ci.com/jumpingrivers/DiAGRAM.svg?branch=master)](https://travis-ci.com/jumpingrivers/DiAGRAM)

Repository for the Digital Archiving Graphical Risk Assessment Model -
DiAGRAM

For more details [see the project
webpage](https://nationalarchives.gov.uk/information-management/manage-information/preserving-digital-records/research-collaboration/safeguarding-the-nations-digital-memory/).

## Getting Started

### Installing Dependencies

``` r
devtools::install_deps()
devtools::install_version("gRain", "1.3-0")
devtools::install()
```

### Running the App

See `inst/example/app.R`

``` r
questions = diagramNAT::read_config(system.file("text_content", "final_questions.yml", package = "diagramNAT"))
default_response = diagramNAT::load_single_response(system.file("default_model", "new.json", package = "diagramNAT"))
reference = base::readRDS(system.file("default_model", "reference_model.rds", package = "diagramNAT"))
model = bnlearn::read.bif(system.file("default_model", "Model.bif", package = "diagramNAT"))
diagramNAT::run_app(questions, default_response, reference, model)
```

