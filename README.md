# Scoring infectious disease forecasts: To log or not

## Repository

* [Manuscript](https://www.overleaf.com/project/61436ea81fed8f4b5e8a8be8)

* Output:
  * `output/data`: for data
  * `output/figures`: for figures.

* Workflow: The workflow is built using the `targets` R package. See `_targets.Rmd` for workflow details and
`_targets.md` for a rendered version. This file also contains details of how  to run and inspect the workflow.

* All R functions used in the analysis can be found in `R`.

## Reproducibility

R package dependencies are handled using the `renv` R package. This means that when an R session is opened in the working directory of this repository for the first time `renv` will attempt to install all required R packages pinned to the correct version. To enhance reproducibility a Docker file has also been provided (`.devcontainer/Dockerfile`) and  optionally this can be accessed via VS code or GitHub codespaces.