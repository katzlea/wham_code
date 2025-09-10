Project WHAM!
======

## About this project
This repository is for all analysis R scripts of the project *"Western Antarctic Peninsula Habitat Mapping"* aka *WHAM!*, my thesis project, where I take images of the bottom of the sea with an ROV and study benthic biodiversity and heterogeneity in the Western Antarctic Peninsula. 
This repository is my image analysis workflow, from right after the fieldwork, until end results. You can use these scripts for your own images, wether you record by hand or by an ROV. Below is a detailed explanation of the workflow.

## Gear
I am using a BLUEROV2 from Blue Robotics ("heavy" configuration) equipped with a WaterLinked positionning system, a ping 1D sonar and a GoPro camera that films the bottom. 

## How to use this repository 
Start by cloning this repository. 
Use the following file structure :
```Markdown
wham 
 >wham.Rproj
 >wham-code (this repo)
 >wham-data (not in this repo)
   >campaign
     >dive
     >etc.
```

## Worfklow

- Film bottom with camera
- Videos => Still images (using **ffmpeg**)
- Rename all files : "campaign_dive.ext" **except QGC video**

### 1_ROV_metadata

- Re-format sonar files 
wham_code>1_ROV_metadata>ping>bin_to_csv.ipynb

- Follow instructions in the script 1_sync.R to synchronize ROV metadata with images

### 2_BIIGLE

- Image annotation in BIIGLE
[www.biigle.de](https://www.biigle.com)

- Download annotation reports from BIIGLE

- Process BIIGLE annotations using script 2.1-process-biigle-data.R
- If needed, group BIIGLE datasets with script 2.2-group-biigle-datasets.R

### 3_BIODIVERSITY

- Use one of the 3 scripts for biodiversity analysis.
  
### 4_statistics

- Perform NMDS or other clustering methods on community data

### 5_BANJO

- Prep data for Bayesian Network Analysis with BANJO using 5.1-prep-for-bni-TANGO1.R or alternatives
- Perform BNI in BANJO
- Analyse outputs from BANJO with 5.2-banjo-outputs.R
- Caluculate conditional probabilities for specific nodes with 5.3-predictive models.R
- Extract parameters from BANJO networks to make bnlearn compatible networks with 5.4-extract-parameters.R
- Calculate conditional probabilities for ALL nodes (in bnlearn) with 5.5-CPT-ALL.R
  
### 6_maps.R
- Create maps in R with 6_maps.R


<img width="451" height="688" alt="image" src="https://github.com/user-attachments/assets/99fe2aac-60d3-4c76-a757-f4d8b00a7040" />
