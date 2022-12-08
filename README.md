ABCD DDM repository
================

This repository contains data, code, and output for a manuscript
entitled “Cognitive deficits and enhancements in youth from adverse
conditions: An integrative assessment using Drift Diffusion Modeling in
the ABCD study” submitted as a Registered Report at *Developmental
Science*.

## Open Science Workflow

\[TBD\]

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
    ## │   │   ├── fig2.png
    ## │   │   └── fig3.png
    ## │   ├── reference-doc.docx
    ## │   ├── references.bib
    ## │   ├── registered_report.docx
    ## │   ├── registered_report.qmd
    ## │   └── scripts
    ## │       └── staging.R
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
2.  `manuscript/manuscript.Rmd`: loads the staged results list and knits
    together the written manuscript and all statistics, tables, and
    figures computed in previous scripts.

### 5. Supplement

1.  `supplement/staging.R`: Reads and combines all the results generated
    by the primary analyses and combines them in a list for the
    supplemental materials.
2.  `supplement/supplemental_materials.Rmd`: loads the staged results
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
    `docker run \-\-rm \-d \-e PASSWORD=my_password \-p 8787:8787 stefanvermeent/abcd_ddm`
4.  Open a browser window and enter: `localhost:8787` as the URL.
5.  You will be redirected to an Rstudio cloud login page. As the
    username, type *rstudio*. As the password, type *my_password*,
    unless you changed it under step 3.
6.  You should now see an RStudio environment with the required
    dependencies pre-installed and loaded.

### 7. Data Access History

- **[2022-09-20
  09:31:08](https://github.com/StefanVermeent/abcd_ddm/tree/29b2d2bd75710c1e99d5e614a0e42cf9cd47ba99):
  data quality checks for DDM models**
  - **Milestone:** Preregistration
  - **Data MD5 hash**:
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/29b2d2bd75710c1e99d5e614a0e42cf9cd47ba99.Rmd)
- **[2022-09-22
  10:01:58](https://github.com/StefanVermeent/abcd_ddm/tree/c4d6e2410ae09db8c0191811583de6498e91af40):
  read single LMT file to develop parsing strategy.**
  - **Milestone:** Data Access
  - **Data MD5 hash**: c3077f193b17f0218489d0a2bc839ff9
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/c4d6e2410ae09db8c0191811583de6498e91af40.Rmd)
- **[2022-09-26
  10:45:23](https://github.com/StefanVermeent/abcd_ddm/tree/f2c4b1e6808bd216eb096dd0be45bb2882570072):
  read all Little Man Task data.**
  - **Milestone:** Data Access
  - **Data MD5 hash**: 43594b8749c0ec3ecbe92f80bb033482
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/f2c4b1e6808bd216eb096dd0be45bb2882570072.Rmd)
- **[2022-09-28
  08:48:40](https://github.com/StefanVermeent/abcd_ddm/tree/0eb03a16b3790a88b522757ee0b691994ec1693c):
  Read TBI data for data exclusions.**
  - **Milestone:** Data Access
  - **Data MD5 hash**: cdf1f71286a9609dc48fac74dbd4eb04
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/0eb03a16b3790a88b522757ee0b691994ec1693c.Rmd)
- **[2022-09-28
  08:49:57](https://github.com/StefanVermeent/abcd_ddm/tree/2c0cee2412ffbab57aca72be952001bdde5de7af):
  Read TBI data for data exclusions.**
  - **Milestone:** Data Access
  - **Data MD5 hash**: cdf1f71286a9609dc48fac74dbd4eb04
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/2c0cee2412ffbab57aca72be952001bdde5de7af.Rmd)
- **[2022-09-28
  08:50:56](https://github.com/StefanVermeent/abcd_ddm/tree/6ffc6d87621a3a0f041918aa926d50e24c0d92f1):
  Read TBI data for data exclusions.**
  - **Milestone:** Data Access
  - **Data MD5 hash**: cdf1f71286a9609dc48fac74dbd4eb04
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/6ffc6d87621a3a0f041918aa926d50e24c0d92f1.Rmd)
- **[2022-09-29
  12:33:42](https://github.com/StefanVermeent/abcd_ddm/tree/d5245a2ffa01a2429a4ebd87624c345e20070a42):
  Read first part of NIH Toolbox data.**
  - **Milestone:** Data Access
  - **Data MD5 hash**: 1dbeb4e70b986f18b9467a1afe3f80dd
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/d5245a2ffa01a2429a4ebd87624c345e20070a42.Rmd)
- **[2022-09-29
  17:28:04](https://github.com/StefanVermeent/abcd_ddm/tree/c42a59876450c81cc6dbfc6ade5920a64feb4326):
  Read second part of the NIH toolbox data.**
  - **Milestone:** Data Access
  - **Data MD5 hash**: 88b5e9aa6bbb24b4fef905af7a560696
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/c42a59876450c81cc6dbfc6ade5920a64feb4326.Rmd)
- **[2022-10-07
  12:52:55](https://github.com/StefanVermeent/abcd_ddm/tree/ec19aea7b66d095174841088d52669177f90c793):
  Read NIH Toolbox performance summary scores**
  - **Milestone:** Data Access
  - **Data MD5 hash**: 5b65303a7b83bb599c6ac1916ab7cd07
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/ec19aea7b66d095174841088d52669177f90c793.Rmd)
- **[2022-10-11
  12:03:50](https://github.com/StefanVermeent/abcd_ddm/tree/4d475280d7afe545fb3f9c2591ad98a7a0e98c25):
  Read NIH Toolbox trial-level data part 1 – incl. files that previously
  got dropped by accident.**
  - **Milestone:** Data Access
  - **Data MD5 hash**: abfd4d7bd6986d3810acf21998ca5780
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/4d475280d7afe545fb3f9c2591ad98a7a0e98c25.Rmd)
- **[2022-10-11
  20:26:24](https://github.com/StefanVermeent/abcd_ddm/tree/5fb550e788bb2ec0a49df908854ebccc979578fe):
  Read NIH Toolbox Part 2. Incl. files that got previously dropped by
  accident.**
  - **Milestone:** Data Access
  - **Data MD5 hash**: 2364dde9131dfce5b249b3091f6b82aa
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/5fb550e788bb2ec0a49df908854ebccc979578fe.Rmd)
- **[2022-12-08
  09:34:04](https://github.com/StefanVermeent/abcd_ddm/tree/153e7d4ea4e633f34f4b04c531999c335e602757):
  Access family ids**
  - **Milestone:** Data Access
  - **Data MD5 hash**: 335c000784108004113778a186cf34c7
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/153e7d4ea4e633f34f4b04c531999c335e602757.Rmd)
- **[2022-12-08
  10:04:00](https://github.com/StefanVermeent/abcd_ddm/tree/46e752f3255c245140d0bdf13c14fc956159c93b):
  read family ids including info on waves.**
  - **Milestone:** Data Access
  - **Data MD5 hash**: 7a422f847f70b13c8388c2d437a0cefc
  - [Link to code
    snippet](https://github.com/stefanvermeent/abcd_ddm/blob/main/.gitlog/46e752f3255c245140d0bdf13c14fc956159c93b.Rmd)
