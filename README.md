ABCD DDM repository
================

This repository contains data, code, and output for a manuscript
entitled “Cognitive deficits and enhancements in youth from adverse
conditions: An integrative assessment using Drift Diffusion Modeling in
the ABCD study” submitted as a Registered Report at *Developmental
Science*.

\#`` {r} #knitr::kable( #  gert::git_log() |>  #  select(commit, author, time, message) |> #  (function(.) filter(., str_detect(string = .$message, pattern = "^\\[\\[")))() |>  #  separate(message, into = c("Message", "Data Hash", 'Code'), sep = "\n") |>  #  separate(Message, into = c("Milestone", "Description"), sep = "\\]\\]\\s") %>%  #  mutate( #    Milestone = map_chr(Milestone, function(Milestone) { #      Milestone |>  #        str_remove("\\[\\[") |> #        str_replace("_", " ") |>  #        str_to_title() #    }) #  ) |>  #  select(Time=time, Milestone, Description, Code, `Data Hash`) |>  #  mutate( #    Code = str_replace_all(Code, "\\|\\>", "\\|\\>\\\n") #  ) #) # ``

## Open Science Workflow

Details about the participants, methods, procedures, and analysis plans
can be found on the Open Science Framework (OSF). Below are the relevant
links for different aspects of the project:

- [OSF project](https://osf.io/6r95z/)
- [Original preregistration](https://osf.io/d2gz6/)
- [Overview of measures](https://osf.io/5kawy/), which fully describes
  what was collected (including measures not used/analyzed in this
  project). Variable codebooks for the current project are contained in
  this repository (see Codebooks below).
- [Final preregistration](https://osf.io/4vsnz/) which also contains a
  preliminary write-up of the Methods for an eventual publication
- [Secondary analysis plan - set 1](https://osf.io/7fu35/)
- [Secondary analysis plan - set 2](https://osf.io/wcauf/)

## Directory Structure

The names of each folder are intended to be self-explanatory. There are
six top-level folders to organize the inputs and outputs of this
project:

1.  `codebooks/`: lists of variable names, labels, and value labels
    (where applicable).
2.  `synthetic_data/`: synthetic (i.e., simulated) data, stored as an
    `.Rdata` file and `.csv` files.
3.  `open_data/`: Empty folder in which real ABCD data can be placed to
    make the analyses fully reproducible. Note that we cannot openly
    share the raw data on the open repository.
4.  `preregistrations/`: The preregistration(s) for the analyses done
    prior to Stage 1 submission.
5.  `registered_report/`: The Registered Report written in R markdown
    for submission to a journal.
6.  `scripts/`: R-scripts that read, analyze, and produce all analysis
    objects.
7.  `supplement/`: a supplemental text (to be submitted with the
    manuscript) documenting all secondary analyses in detail.

Below is a simple visualization of the full directory structure.

    ## .
    ## ├── Dockerfile
    ## ├── preregistrations
    ## │   ├── 2022-09-20_preregistration_DDM.md
    ## │   └── 2022-09-20_preregistration_DDM.Rmd
    ## ├── registered_report
    ## │   ├── apa.csl
    ## │   ├── images
    ## │   │   ├── fig1.png
    ## │   │   └── fig2.png
    ## │   ├── reference-doc.docx
    ## │   ├── references.bib
    ## │   ├── registered_report.docx
    ## │   └── registered_report.qmd
    ## ├── scripts
    ## │   ├── 0_simulations
    ## │   │   ├── choiceRT_ddm.stan
    ## │   │   ├── data.csv
    ## │   │   ├── ddm_trial_sim.R
    ## │   │   ├── model.txt
    ## │   │   └── power_analysis.R
    ## │   ├── 1_data_prep
    ## │   │   ├── 1_preprocessing.R
    ## │   │   └── 2_clean_data.R
    ## │   ├── 2_analyses
    ## │   │   ├── 1_ddm_fit.R
    ## │   │   ├── 2_sem_training.R
    ## │   │   └── 3_sem_test.R
    ## │   ├── custom_functions
    ## │   │   ├── general-functions.R
    ## │   │   └── read-functions.R
    ## │   └── dependencies.R
    ## ├── supplement
    ## │   ├── apa.csl
    ## │   ├── reference-doc.docx
    ## │   ├── references.bib
    ## │   ├── supplemental_materials.docx
    ## │   └── supplemental_materials.qmd
    ## └── synthetic_data

Each of the top-level folders are explained in more detail below:

## Codebooks

These are lists of variable names, labels, and value labels (where
applicable) (see [synthetic_data](#synthetic_data) below).

## Synthetic Data

ABCD data cannot be shared on open repositories. Therefore, interested
readers can only fully reproduce our analyses with personal access to
the ABCD data repository. To facilitate computational reproducibility,
we provide synthetic data files. These files contain simulated data with
the same variables as the original data and with the same basic
characteristics. Thus, these files can be used as input to the analyses
scripts. Note that, because the values are not identical to the real
data, the output will deviate from the statistics described in the
manuscript.

The synthetic data files resemble the data after preprocessing (as done
in `/scripts/0_data_prep`).

## Preregistrations

## Manuscript

This directory contains the manuscript for this project. It was written
in R Markdown and is 100% reproducible. The `.Rmd` fie contains the
written text with all figures, tables, and in-text statistics. All
outputs reported in the manuscript were staged prior to compiling into a
`.docx` document and stored in a `.Rdata` file called
`staged-objects.Rdata`. The R Markdown document reads these staged files
to make the overall document easier to read. The scripts that produce
the staged objects are located in the `manuscript/scripts/` directory
and are named to be self-explanatory. The `rmd-staging.R` script reads
necessary data/objects from the `multiverse-objects/` directory and
sources the other scripts to produce the objects needed for the
manuscript.

## Multiverse-objects

There are four steps to doing the multiverse:

1.  Specifying a grid of all possible arbitrary data processing
    decisions
2.  Creating a “multiverse” of alternative data sets one could analyze
3.  Analyzing each “universe” within the multiverse
4.  Extracting the relevant information from the analyses (e.g., effect
    sizes, p-values, etc.)
5.  Visualizing results or summarizing them in tables

This folder contains all objects necessary for representing each step.
Specifically:

- `1-multiverse-datasets/`: multiverse data sets with each data universe
  stored in a list. There is one list used for the composites and one
  list used for all components of each IV.
- `2-primary-analyses/`: contain the multiverse of full results and
  extracted effects for the primary confirmatory and exploratory results
- `3-secondary-analyses/`: contain the same objects as above for each
  set of secondary analyses.

## Data Processing Scripts

There are four types of R-scripts in this repository, each with a
separate folder.

- `0-functions/`: Custom R-functions written for this project
- `1-data-prep/`: Data processing scripts
- `2-primary-analyses/`: Primary analysis scripts
- `3-secondary-analyses/`: Secondary analysis scripts

Each script takes an input(s) and produces output(s). The tables below
provides an overview of the inputs and outputs of each script.

### Data Prep

| script | input | output |
|--------|-------|--------|

### Primary Analyses

| script | input | output |
|--------|-------|--------|

### Secondary Analyses

| script | input | output |
|--------|-------|--------|

## Supplement

This directory contains the supplement to the manuscript described
above. The supplement was produced using the same approach as the
manuscript: it was written in R Markdown, is 100% reproducible, and all
outputs were staged prior to compiling into a `.docx`. The staged
objects are stored in `supplement/staged-objects.Rdata`. The scripts
that produce the staged objects are located in the `supplement/scripts/`
directory and are named to be self-explanatory.

## How to reproduce this repository

This repository can be reproduced in two ways.

The first way is by downloading all files locally. You can do this by
[cloning the
repository](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/adding-and-cloning-repositories/cloning-and-forking-repositories-from-github-desktop)
or by downloading the repository as a `.zip` file. After
cloning/downloading, double click on `abcd_ddm.Rproj` to open up
RStudio.

The second way is by using [Docker](https://docs.docker.com/). We supply
a Docker image that instantiates an Rstudio cloud environment with all
the required dependencies. We provide more information about how to use
the Docker image below (see [6. Using Docker](#using_docker). In short,
Docker provides a ‘container’—similar to a virtual machine—with all the
project dependencies (R packages, software, operating systems)
pre-loaded in their (historic) versions as they were used when the
project was created. Thus, Docker ensures that scripts will still
function as intended even into the future. **Note: It is not necessary
to use Docker to reproduce our results**, but some of our scripts might
stop to function properly over time as some dependencies get updated
and/or deprecated.

Below is the exact order these scripts should be ran (irrespective of
whether you are working with or without Docker):

### 1. Loading Dependencies

1.  `/scripts/dependencies.R`: Loads (and installs if necessary) all the
    necessary R packages as they were on 2022-10-15 using the
    [`Groundhog`](https://groundhogr.com/) package.

### 2. Data Preparation

The following steps are only required when you are using real ABCD study
data. If you are using the synthetic data, you can skill to step 3.

1.  `0_data_prep/1_preprocessing.R`: Reads and binds individual data
    files, and fixes participant IDs where necessary.
2.  `0_data_prep/2_clean_data.R`: Takes care of trial-level and
    participant-level exclusions.

### 3. Primary Analyses

TBD

### 4. Manuscript

1.  `manuscript/manuscript.Rmd`: loads the required data objects and
    knits together the written manuscript and all statistics, tables,
    and figures computed in previous scripts.

### 5. Supplement

1.  `supplement/supplement.Rmd`: loads the required data objects and
    knits together the written supplement and all tables and figures
    computed by the staging script above.

### 6. Using Docker

If you want to create a Docker container in which to run the scripts,
you will have to follow the following steps:

1.  Download and install docker at
    <https://docs.docker.com/get-docker/>.
2.  If on Windows, open the PowerShell. If on Mac, open the terminal
    through ‘Applications \> Utilities \> Terminal’.
3.  On the command line, type:
    `docker run \-\-rm \-d \-e PASSWORD=my_password \-p 8787:8787 stefanvermeent/abcd_ddm`
4.  Open a browser window and enter: `localhost:8787` as the URL.
5.  You will be redirected to an Rstudio cloud login page. As the
    username, type *rstudio*. As the password, type *my_password*,
    unless you changed it under step 3.
6.  You should now see an RStudio environment with the required
    dependencies pre-installed and loaded.
