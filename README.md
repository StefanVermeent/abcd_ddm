ABCD DDM repository
================

This repository contains data, code, and output for a manuscript
entitled “Cognitive deficits and enhancements in youth from adverse
conditions: An integrative assessment using Drift Diffusion Modeling in
the ABCD study” submitted as a Registered Report at *Developmental
Science*.

## Open Science Workflow

Prior to Stage 1 submission of the Registered Report, we accessed the
cognitive task data for a couple of
[preregistered](https://github.com/stefanvermeent/abcd_ddm/preregistrations/2022-09-20_preregistration_DDM.md)
data checks. By only accessing the cognitive task data, these steps did
not bias or substantive analyses involving measures of adversity. To
transparently show when we accessed which data, we created an open
science workflow that would automate this process. The main aim of this
workflow was to create a transparent log of every major milestone of the
project, such as accessing new data, submitting preregistrations, and
finalizing analyses.

The main ingredient of this workflow is a set of custom functions that
we created for reading in data files. These are wrappers for the read
functions in the `readr` package, sourced from
`scripts/custom_functions/read-functions.R`. Whenever one of these
functions (e.g., `read_csv`) was called, it went through a couple of
internal processes. First, the specified data file would be read into R.
This could be a single file, or a list of individual data files that
would first be combined into a single dataframe. Second, any specified
manipulations would be applied to the data. This could be selecting
specific variables, filtering specific rows, or randomly shuffling
values (e.g., participant IDs). Third, An MD5 hash of the final R object
would be generated using the `digest` package. An MD5 hash is a unique,
32-digit string that maps directly onto the content of the R object. The
same R object will always generate the same MD5 hash, but as soon as
anything changes (e.g., a variable is added, a value is rounded), the
MD5 hash changes. Fourth, this MD5 hash would be compared to previously
generated hashes stored in `.gitlog/MD5`.

If the newly generated MD5 hash was not recognized, this triggered an
automatic commit to GitHub. At this point, the user gets the choice to
abort the process or to continue. If opting to continue, the user could
supply an informative message (e.g., “accessed Flanker data”), which
would be added to the Git commit. The Git commit message stored other
relevant meta-data as well, such as the object hash and the code used to
read and manipulate the data. Committing and pushing to Git was handled
using the `gert` package.

Thus, any accessing of raw data was automatically tracked via GitHub.
Using this same approach, we also logged other major milestones, such as
submitting preregistrations and finalizing analyses. For an overview of
all milestones, see the [Data Access History](data_access_history.md).

## Directory Structure

The names of each folder are intended to be self-explanatory. There are
six top-level folders to organize the inputs and outputs of this
project:

1.  `preregistrations/`: The preregistration(s) for the analyses done
    prior to Stage 1 submission.
2.  `registered_report/`: The Registered Report written in R markdown
    for submission to a journal.
3.  `scripts/`: R-scripts that read, analyze, and produce all analysis
    objects.
4.  `supplement/`: a supplemental text (to be submitted with the
    manuscript) documenting all secondary analyses in detail.
5.  `data/`: Folder in which real ABCD data can be placed to make the
    analyses fully reproducible. Note that we cannot openly share the
    raw data on the open repository. Contains synthetic data to
    facilitate computational reproducibility in the absence of ABCD
    access.
6.  `codebooks/`: lists of variable names, labels, and value labels
    (where applicable).

Below is a simple visualization of the full directory structure.

    ## .
    ## ├── codebooks
    ## ├── data_access_history.md
    ## ├── data_access_history.Rmd
    ## ├── Dockerfile
    ## ├── preregistrations
    ## │   ├── 2022-09-20_preregistration_DDM.md
    ## │   └── 2022-09-20_preregistration_DDM.Rmd
    ## ├── registered_report
    ## │   ├── apa.csl
    ## │   ├── images
    ## │   │   ├── fig1.png
    ## │   │   ├── fig2.png
    ## │   │   └── fig3.png
    ## │   ├── reference-doc.docx
    ## │   ├── references.bib
    ## │   ├── registered_report.docx
    ## │   ├── registered_report.qmd
    ## │   ├── scripts
    ## │   │   └── staging.R
    ## │   └── staged_results.RData
    ## ├── scripts
    ## │   ├── 0_simulations
    ## │   │   ├── ddm_trial_simulations.R
    ## │   │   └── power_analysis.R
    ## │   ├── 1_data_prep
    ## │   │   ├── 1_preprocessing.R
    ## │   │   ├── 2_clean_data.R
    ## │   │   └── 3_data_subsets.R
    ## │   ├── 2_analyses
    ## │   │   ├── 1_ddm_fit.R
    ## │   │   ├── 2_sem_training.R
    ## │   │   └── 3_sem_test.R
    ## │   ├── custom_functions
    ## │   │   ├── DBDA2E-utilities.R
    ## │   │   ├── general-functions.R
    ## │   │   └── read-functions.R
    ## │   └── dependencies.R
    ## ├── supplement
    ## │   ├── apa.csl
    ## │   ├── reference-doc.docx
    ## │   ├── references.bib
    ## │   ├── scripts
    ## │   │   └── staging_suppl.R
    ## │   ├── supplemental_materials.docx
    ## │   └── supplemental_materials.qmd
    ## └── synthetic_data
    ##     ├── ddm_sim1.RData
    ##     ├── ddm_sim1_mod.RData
    ##     ├── ddm_sim1_results.RData
    ##     ├── ddm_sim2_mod.RData
    ##     ├── ddm_sim2_results.RData
    ##     ├── power.Rdata
    ##     ├── sim_DDM_6trials.RData
    ##     └── staged_sim_results_supp.RData

Each of the top-level folders are explained in more detail below:

## Preregistrations

This directory contains the preregistrations for this project.

## Registered Report

This directory contains the manuscript for this project. It was written
in R Markdown and is 100% reproducible. The `.qmd` file contains the
written text with all figures, tables, and in-text statistics. All
outputs reported in the manuscript were staged prior to compiling into a
`.docx` document and stored in a `.Rdata` file called
`staged_results.Rdata`. The Quarto document reads these staged files to
make the overall document easier to read. The scripts that produce the
staged objects are located in the `registered_report/scripts/` directory
and are named to be self-explanatory. The `staging.R` script reads
necessary data/objects from the `analysis-objects/` directory and
sources the other scripts to produce the objects needed for the
manuscript.

## Data Processing Scripts

There are four types of R-scripts in this repository, each with a
separate folder.

- `custom_functions/`: Custom R-functions written for this project
- `0_simulations/`: Simulation scripts for power and DDM analyses
- `1_data_prep/`: Data processing scripts
- `2_analyses/`: Primary analysis scripts

Each script takes an input(s) and produces output(s). The tables below
provides an overview of the inputs and outputs of each script.

### Simulations

| script                  | input | output                 |
|-------------------------|-------|------------------------|
| ddm_trial_simulations.R |       | ddm_sim2_results.RData |
| power_analysis.R        |       | power.RData’           |

### Data Prep

| script            | input              | output                            |
|-------------------|--------------------|-----------------------------------|
| 1_preprocessing.R |                    | tasks_raw.RData’                  |
| 2_clean_data.R    | tasks_raw.RData    | tasks_clean.RData                 |
| 3_data_subsets.R  | tasks_clean.RData’ | training_set.csv<br> test_set.csv |

### Analyses

| script           | input                                             | output                     |
|------------------|---------------------------------------------------|----------------------------|
| 1_ddm_fit.R      | tasks_clean.RData’                                | mcmc_dccs_mod1.RData’      |
| 2_sem_training.R | ddm_data.csv<br> iv_data.csv<br> training_set.csv | sem_training_results.RData |
| 3_sem_test.R     |                                                   | NA                         |

## Codebooks

These are lists of variable names, labels, and value labels (where
applicable) (see [data](data) below).

## Data

This folder contains all the data that serves as input for the analysis
scripts. ABCD data cannot be shared on open repositories. Therefore,
interested readers can only fully reproduce our analyses with personal
access to the ABCD data repository. To facilitate computational
reproducibility, we provide synthetic data files. These files contain
simulated data with the same variables as the original data and with the
same basic characteristics. Thus, these files can be used as input to
the analyses scripts. Note that, because the values are not identical to
the real data, the output will deviate from the statistics described in
the manuscript.

The synthetic data files resemble the data after preprocessing (as done
in `/scripts/1_data_prep`). Therefore, when using the synthetic data you
should skip the preprocessing steps and continue to the analysis scripts
(`/scripts/2_analyses`). If you do have access to the ABCD data, make
sure that you place tje correct data files/folders in the `data` folder
(see [how to reproduce this repository](#reproduce))

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
    [`Groundhog`](https://groundhogr.com/) package. This ensures that
    scripts will continue functioning as intended even after packages
    have been updated.

### 2. Data Preparation

The data preparation steps are only required when you are using real
ABCD study data. If you are using the synthetic data, you can skill to
step 3. You should have the following data files/folders in your `data`
folder (Remember to set the `data_folder` object in
`scripts/dependencies.R` to `"data"`):

- `abcd_tbss01.txt`: File containing Cognitive summary scores (used for
  Participant ID verification).
- `abcd_tbi01.txt`: File containing TBI data.
- `abcd_lmtlb01`: Folder containing the Little Man Task Data.
- `abcd_tb_tlb01`: Folder containing the NIH Toolbox Cognitive Data.
- `acspsw03.txt`: File containing Family IDs (used for making sure that
  siblings end up in the same subset).

After downloading the above files/folders, run the preprocessing scripts
in the following order:

1.  `0_data_prep/1_preprocessing.R`: Reads and binds individual data
    files, and fixes participant IDs where necessary. Note that this
    script may take several hours to complete.
2.  `0_data_prep/2_clean_data.R`: Takes care of trial-level and
    participant-level exclusions.
3.  `0_data_prep/3_data_subsets.R`: Divides the full dataset into a
    training and test set.

### 3. Primary Analyses

1.  `1_ddm_fit.R`: Fits the hierarchical Bayesian DDM models to the data
    of the four cognitive tasks and saves the results.
2.  `2_sem_training.R`: Iteratively fits the SEM model to the training
    set and saves the results.
3.  `3_sem_test.R`: Fits the final SEM model to the test set.

### 4. Manuscript

1.  `registered_report/scripts/staging.R`: Reads and combines all the
    results generated by the primary analyses and combines them in a
    list for the final manuscript.
2.  `registered_report/registered_report.qmd`: loads the staged results
    list and knits together the written manuscript and all statistics,
    tables, and figures computed in previous scripts.

### 5. Supplement

1.  `supplement/staging.R`: Reads and combines all the results generated
    by the primary analyses and combines them in a list for the
    supplemental materials.
2.  `supplement/supplemental_materials.qmd`: loads the staged results
    list and knits together the written supplement and all tables and
    figures computed by the staging script above.

### 6. Using Docker

If you want to create a Docker container in which to run the scripts,
you will have to follow the following steps:

1.  Download and install docker at
    <https://docs.docker.com/get-docker/>.
2.  If on Windows, open the PowerShell. If on Mac, open the terminal
    through ‘Applications \> Utilities \> Terminal’.
3.  On the command line, type:
    `docker run --rm -d -e PASSWORD=my_password -p 8787:8787 stefanvermeent/abcd_ddm`
4.  Open a browser window and enter: `localhost:8787` as the URL.
5.  You will be redirected to an Rstudio cloud login page. As the
    username, type *rstudio*. As the password, type *my_password*,
    unless you changed it under step 3.
6.  You should now see an RStudio environment with the required
    dependencies pre-installed and loaded.
