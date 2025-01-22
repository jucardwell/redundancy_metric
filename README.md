# redundancy_metric
This repository contains the scripts utilized to create the redundancy metric described in  "Developing an origin-based, demand-aware metric for road network redundancy".

For questions or issues related to this code, please contact [Julia Cardwell](https://jucardwell.github.io/).

## **Software environment:** 
Software environment and package versions can be viewed in “environment.txt”

## **Data availability:**

**SafeGraph Data:** Monthly patterns SafeGraph data from 2018-2022 was obtained through an academic research agreement. This raw data cannot be publicly shared. For replication of this project, a user would need to make their own request for the SafeGraph data.

**North Carolina Block Group and County**: North Carolina block group and county shapefile can be downloaded from the United States Census Bureau (https://www.census.gov/geographies/mapping-files/2020/geo/tiger-line-file.html)

**2010 to 2020 Crosswalk Table:** Can be downloaded from IPUMS NHGIS (https://www.nhgis.org/geographic-crosswalks)

*Researchers who do not have access to SafeGraph data may need to seek alternative datasets or contact the data provider for access. Please note that replication may be limited by the availability of these specific data sources*

## **Folder setup:**
Project Directory/
- data/
  - safegraph/
  - travel_matrices/
- output/
  - redundancy_output/
- script/

## **Workflow:**
Note that some of the processes in this analysis were done outside of a scripting workflow. Those instances are denoted with a **

**Network Dataset Production Workflow:**
1.	**extract_osmnetwork.R** extracts osm data for “driving” mode for North Carolina via API. Saves “data/nc_road_network.shp”
2.	**save_weighted_network.R** removes “service roads” and weights the road network using the “motorcar” weighting scheme available in dodgr. Saves “data/nc_weighted_network.Rds”
3.	**save_sf_network.R** turns the weighted network into an sf object. This sf object is utilized to identify routable locations for census centroids. Creates “data/nc_sf_network.shp”

**Routable Location Workflow:**
1.	**Upload “data/nc_sf_network.shp” into QGIS and filter to only two-way roads on the largest connected component.
2.	**Use “Join by Nearest” processing tool between all locations and two-way largest components.
3.	**Export as .csv  “blockgroup_routable_nodeid.csv”
4.	**prep_routable_locations.R** to identify a single two-way, largest component node for each location. Creates “data/bg_simp_routable.csv”
5.	**baseline_local_routing.R** to identify un-routable locations.
6.	**Use QGIS to manually identify an acceptable node for each un-routable location. The un-routable locations were due to small topological errors. 
7.	**Export final routable files. “data/bg_simp_routable_manual.csv”

**Safegraph Workflow:**
1.	**create_yearly_safegraph_OD.R** to aggregate monthly Safegraph data into yearly files. Creates “data/safegraph/[year]_2010cbg_home.csv”
2.	**aggregate_yearly_safegraph.R** to aggregate yearly safegraph data to the full study period. Creates “data/safegraph/fullsafegraph.csv”
3.	**local_area_2010_2020.R** to convert 2010 home census block groups to 2020 and select habitual destinations. Creates "data/local_travel_od.csv".

**Analysis Workflow:**
1.  **prep_travel_matrices.R** to create a demand-weighted flow matrix for each analysis census block group. Creates "data/travel_matrices/XX".
2.  **redundancy.sh** Shell script to run each origin block group as a separate instance.
3.  **redundancy.R** called by **redundancy.sh** to run redundancy metric for each origin. Creates "output/redundancy_output/XX".
4.  **redundancy_analysis.R** analyzes redundancy results. 
