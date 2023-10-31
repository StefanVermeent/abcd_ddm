---
title: ""
output: 
  html_document:
    template: assets/template.html
    css: assets/style.css
    keep_md: true
editor_options: 
  markdown: 
    wrap: sentence
---




<br>

This repository contains data, code, and output for a manuscript entitled "Cognitive deficits and enhancements in youth from adverse conditions: An integrative assessment using Drift Diffusion Modeling in the ABCD study". The manuscript has received Stage 1 In-Principle acceptance at *Developmental Science*.

Do you want to download or clone the materials for this project? Go to [https://github.com/stefanvermeent/abcd_ddm](https://github.com/stefanvermeent/abcd_ddm)

## Directory Structure {#structure}

The names of each folder are intended to be self-explanatory.
There are eight top-level folders to organize the inputs and outputs of this project:

1.  [`Open Science Workflow`](https://stefanvermeent.github.io/abcd_ddm/opensci_workflow/README.html): Statement on and explanation of our Open Science Workflow.
2.  [`Preregistrations`](https://stefanvermeent.github.io/abcd_ddm/preregistrations/README.html): The preregistration for the analyses done prior to Stage 1 submission.
3.  [`Registered Report`](https://stefanvermeent.github.io/abcd_ddm/registered_report/README.html): The Registered Report written in R markdown.
4.  [`Supplement`](https://stefanvermeent.github.io/abcd_ddm/supplement/README.html): a supplemental text (to be submitted with the manuscript) documenting all secondary analyses in detail.
5.  [`Scripts`](https://stefanvermeent.github.io/abcd_ddm/scripts/README.html): R-scripts that read, analyze, and produce all analysis objects.
6.  [`Data`](https://stefanvermeent.github.io/abcd_ddm/data/README.html): Folder in which real ABCD data can be placed to make the analyses fully reproducible. Note that we cannot openly share the raw data on the open repository. Contains synthetic data to facilitate computational reproducibility in the absence of ABCD access.
7.  [`Analysis Objects`](https://stefanvermeent.github.io/abcd_ddm/analysis_objects/README.html): Folder containing all analysis objects.
8.  [`Codebooks`](https://stefanvermeent.github.io/abcd_ddm/codebooks/README.html): lists of variable names, labels, and value labels (where applicable).

Click on each of the folders to get more details.

## Overview of project milestones

Below is an overview of all the project milestones, such as first-time data access, submissions, and revisions.
Data access events were automatically captured using custom code, which over the course of this project was collected in the R package `projectlog` [https://stefanvermeent.github.io/projectlog/](https://stefanvermeent.github.io/projectlog/).
For more information about how tracking worked, Go to the [Open science workflow tab](https://stefanvermeent.github.io/abcd_ddm/opensci_workflow/README.html).

- **[2022-09-22 10:01:58](<anonymized repository>): read single LMT file to develop parsing strategy.**
    - **Milestone:** Data Access
    - **Data MD5 hash**: c3077f193b17f0218489d0a2bc839ff9
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/c4d6e2410ae09db8c0191811583de6498e91af40.R)
    

- **[2022-09-26 10:45:23](<anonymized repository>): read all Little Man Task data.**
    - **Milestone:** Data Access
    - **Data MD5 hash**: 43594b8749c0ec3ecbe92f80bb033482
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/f2c4b1e6808bd216eb096dd0be45bb2882570072.R)
    

- **[2022-09-28 08:50:56](<anonymized repository>): Read TBI data for data exclusions.**
    - **Milestone:** Data Access
    - **Data MD5 hash**: cdf1f71286a9609dc48fac74dbd4eb04
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/6ffc6d87621a3a0f041918aa926d50e24c0d92f1.R)
    

- **[2022-09-29 12:33:42](<anonymized repository>): Read first part of NIH Toolbox data.**
    - **Milestone:** Data Access
    - **Data MD5 hash**: 1dbeb4e70b986f18b9467a1afe3f80dd
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/d5245a2ffa01a2429a4ebd87624c345e20070a42.R)
    

- **[2022-09-29 17:28:04](<anonymized repository>): Read second part of the NIH toolbox data.**
    - **Milestone:** Data Access
    - **Data MD5 hash**: 88b5e9aa6bbb24b4fef905af7a560696
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/c42a59876450c81cc6dbfc6ade5920a64feb4326.R)
    

- **[2022-10-07 12:52:55](<anonymized repository>): Read NIH Toolbox performance summary scores**
    - **Milestone:** Data Access
    - **Data MD5 hash**: 5b65303a7b83bb599c6ac1916ab7cd07
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/ec19aea7b66d095174841088d52669177f90c793.R)
    

- **[2022-10-11 12:03:50](<anonymized repository>): Read NIH Toolbox trial-level data part 1 -- incl. files that previously got dropped by accident.**
    - **Milestone:** Data Access
    - **Data MD5 hash**: abfd4d7bd6986d3810acf21998ca5780
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/4d475280d7afe545fb3f9c2591ad98a7a0e98c25.R)
    

- **[2022-10-11 20:26:24](<anonymized repository>): Read NIH Toolbox Part 2. Incl. files that got previously dropped by accident.**
    - **Milestone:** Data Access
    - **Data MD5 hash**: 2364dde9131dfce5b249b3091f6b82aa
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/5fb550e788bb2ec0a49df908854ebccc979578fe.R)
    

- **[2022-12-08 09:34:04](<anonymized repository>): Access family ids**
    - **Milestone:** Data Access
    - **Data MD5 hash**: 335c000784108004113778a186cf34c7
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/153e7d4ea4e633f34f4b04c531999c335e602757.R)
    

- **[2022-12-08 10:04:00](<anonymized repository>): read family ids including info on waves.**
    - **Milestone:** Data Access
    - **Data MD5 hash**: 7a422f847f70b13c8388c2d437a0cefc
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/46e752f3255c245140d0bdf13c14fc956159c93b.R)
    

- **[2023-07-17 09:31:09](<anonymized repository>): Revision 2 of Stage 1 Registered Report for Developmental Science**
    - **Milestone:** Revision
    - **Data MD5 hash**: 
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/1606a17fb4349843fd88566bb3ea3c868a9fc316.R)
    

- **[2023-08-01 12:08:29](<anonymized repository>): first-time access to ivs with mnlfa correction**
    - **Milestone:** Data Access
    - **Data MD5 hash**: 01189ac84ccf49b95c2c82f2f3f89cef
    - [Link to code snippet](https://anonymous.4open.science/r/anon-255D/.gitlog/81bed689986fbaadc625206c899b90fadf0b2600.R)
    

## How to reproduce this repository {#reproduce}

This repository can be reproduced in two ways.

The first way is by downloading all files locally.
You can do this by [cloning the repository](https://docs.github.com/en/desktop/contributing-and-collaborating-using-github-desktop/adding-and-cloning-repositories/cloning-and-forking-repositories-from-github-desktop) or by downloading the repository as a `.zip` file.
After cloning/downloading, double click on `abcd.Rproj` to open up RStudio.

The second way is by using [Docker](https://docs.docker.com/).
We supply a Docker image that instantiates an Rstudio cloud environment with all the required dependencies.
We provide more information about how to use the Docker image below (see [6. Using Docker](#using_docker). 
In short, Docker provides a 'container'---similar to a virtual machine---with all the project dependencies (R packages, software, operating systems) pre-loaded in their (historic) versions as they were used when the project was created.
Thus, Docker ensures that scripts will still function as intended even into the future.
**Note: It is not necessary to use Docker to reproduce our results**.
If you're cloning the repository within a couple of years of publication and installing the proper dependencies (see step 1 below) you should be able to run all scripts without issues.
However, some of our scripts might stop to function properly over time as other dependencies get updated and/or deprecated (e.g., future versions of RStudio)
If that case, Docker is the solution.

Below is the exact order these scripts should be ran (irrespective of whether you are working with or without Docker):

### 1. Loading Dependencies

1.  [`/scripts/dependencies.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/dependencies): Loads (and installs if necessary) all the necessary R packages as they were on 2022-10-15 using the [`Groundhog`](https://groundhogr.com/) package. 
This ensures that scripts will continue functioning as intended even after packages have been updated. 
`Groundhog` is a more lightweight, easy-to-use alternative to package dependency managers such as `renv`.

### 2. Data Preparation

The data preparation steps are only required when you are using real ABCD study data.
If you are using the synthetic data, you can skill to step 3.
You should have the following data files/folders in your `data` folder (Remember to set the `data_folder` object in [`/scripts/dependencies.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/dependencies.R) to `"data"`):

  - `abcd_tbss01.txt`: File containing Cognitive summary scores (used for Participant ID verification).
  - `abcd_tbi01.txt`:  File containing TBI data.
  - `abcd_lmtlb01`:    Folder containing the Little Man Task Data.
  - `abcd_tb_tlb01`:   Folder containing the NIH Toolbox Cognitive Data.
  - `acspsw03.txt`:    File containing Family IDs (used for making sure that siblings end up in the same subset).
  
After downloading the above files/folders, run the preprocessing scripts in the following order:

1.  [`1_data_prep/1_preprocessing.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/1_data_prep/1_preprocessing.R): Reads and binds individual data files, and fixes participant IDs where necessary. Note that this script may take several hours to complete.
2.  [`1_data_prep/2_clean_data.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/1_data_prep/2_clean_data.R): Takes care of trial-level and participant-level exclusions.
3.  [`1_data_prep/3_data_subsets.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/1_data_prep/3_data_subsets.R): Divides the full dataset into a training and test set.

### 3. Primary Analyses

1.  [`1_ddm_fit.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/2_analyses/1_ddm_fit.R): Fits the hierarchical Bayesian DDM models to the data of the four cognitive tasks and saves the results.
2.  [`2_ivs.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/2_analyses/2_ivs.R): Prepare the independent variables necessary for the main analyses.
3.  [`3_ddm_extract_results.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/2_analyses/3_ddm_extract_results.R): Extract and parse the DDM model objects.
4.  [`4_sem_training.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/2_analyses/4_sem_training.R): Iteratively fits the SEM model to the training set and saves the results.
5.  [`5_sem_test.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/2_analyses/5_sem_test.R): Fits the final SEM model to the test set.
6.  [`6_exploratory.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/scripts/2_analyses/6_exploratory.R): Exploratory (i.e., not pre-registered) analyses.

### 4. Manuscript

1.  [`registered_report/scripts/staging.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/registered_report/scripts/staging.R): Reads and combines all the relevant analysis objects generated by the analysis scripts and combines them in a list for the supplemental materials.
2.  [`registered_report/registered_report.qmd`](https://github.com/stefanvermeent/abcd_ddm/blob/main/registered_report/registered_report.qmd): loads the staged results list and knits together the manuscript and all statistics, tables, and figures computed in previous scripts.

### 5. Supplement

1.  [`supplement/scripts/staging_suppl.R`](https://github.com/stefanvermeent/abcd_ddm/blob/main/supplement/scripts/staging_suppl.R): Reads and combines all the relevant analysis objects generated by the analysis scripts and combines them in a list for the supplemental materials.
2.  [`supplement/supplemental_materials.qmd`](https://github.com/stefanvermeent/abcd_ddm/blob/main/supplement/supplemental_materials.qmd): loads the staged results list and knits together the written supplement and all tables and figures computed by the staging script above.

### 6. Using Docker {#using_docker}

If you want to create a Docker container in which to run the scripts, you will have to follow the following steps:

1.  Download and install docker at <https://docs.docker.com/get-docker/>.
2.  If on Windows, open the PowerShell. If on Mac, open the terminal through 'Applications \> Utilities \> Terminal'.
3.  On the command line, type: `docker run --rm -d -e PASSWORD=my_password -p 8787:8787 stefanvermeent/abcd_ddm`
4.  Open a browser window and enter: `localhost:8787` as the URL.
5.  You will be redirected to an Rstudio cloud login page. As the username, type *rstudio*. As the password, type *my_password*, unless you changed it under step 3.
6.  You should now see an RStudio environment with the required dependencies pre-installed and loaded.

### Contact

For questions or comments, feel free to contact me at p.c.s.vermeent@gmail.com.
