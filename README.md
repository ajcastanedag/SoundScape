![](https://img.shields.io/badge/EAGLE-Design-blue) ![](https://img.shields.io/badge/Approval-pending-red)
# SoundScape


![](https://github.com/ajcastanedag/SoundScape/blob/main/Images/Spectrogram.PNG)           

The FUSECISIONR algorithm based on the 16EAGLES/GetSpatialData package provides automated decision making for finding the most suitable Sentinel-1 and Sentinel-2 images for your research area. The decision is based on the Geometry of both scenes and provides Statistics about the coverage of the scenes above your research area. The algorithm which serves as a decision support for the selection of optical and passive images for data fusion purposes developed by EAGLE/UniversityofWuerzburg first semester Msc students is described in more details in this blog.

## Objective

The algorithm supports the search for suitable active and passive images for data fusion. Usually a long time is spent to find suitable images for the fusion, as not only the cloud cover or the orbit direction is decisive but also the ideal spatial and temporal axis must be considered. The extension to the package 16Eagles/GetSpatialData developed for the EAGLE program course MB02 at the University of Würzburg, offers the possibility to get an overview of the geometries and relations of active and passive images in relation to the initially determined polygons of the research area.
The data is loaded via the API of ESA Scihub.Copernicus ( https://scihub.copernicus.eu/ ) using the 16Eagles/GetSpatialData package and then filtered by our algorithm.

## **Workflow**

The first step is to determine the area of investigation. Then the log-in data for the Copernicus Skihub (https://scihub.copernicus.eu/) are entered which is using the functions within the 16/EaglesGetSpatialData packet. The next step determines the date at which the existing data is to be searched ("Enter start / end of timespan (YYYY-MM-DD). The data is loaded via the API of Copernicus Skihub ( https://scihub.copernicus.eu/ ) using the 16Eagles/GetSpatialData package and is then guided by us through a statistical analysis.

## **Outlook**


Furthermore, in the near future a cloud mask of the optical image is added to determine the percentage cloud content in the polygon of the study area. This enables the user to use the images which have been filtered out of the search by an increased cloud content. 

