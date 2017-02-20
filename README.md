## NOTE: THIS DESCRIPTION IS OUTDATED
### For a more updated version of the model description, [see the google doc](https://docs.google.com/document/d/1rHDwDdoDr5V9fItE8GAzPFWKjjoYDXK4E_rGHOxSRCs/edit?usp=sharing).



# Intermediate Complexity Catchment-Based Routing Model for LPJ-Guess


## Overview
The purpose of this model is to route surface and subsurface runoff given by the LPJ-Guess DGVM, through a river network created in ArcGIS using the ArcHydro package. The structure of this model consists of a routingFunctions.r file that defines the functions used in routing, and a routeWater.r script in which parameters are configured and the model is excecuted.

The functions used in this model are defined below:

**AggregateRunoff()** - Reads in runoff NetCDFs and aggregates to catchments.

**RouteWater()** - Routes water through edges.

**GetGaugeData()** - Finds NWIS river gauges, downloads, and processes data for plotting.

**GetShapesInBounds()** - Subsetts catchments or edges by HUC code, checks if any upstream polygons are missing, and adds them. The three functions below are used.

**getParents()** - Returns the two parents of an edge given it's ID.

**getOrder()** - Returns the order of an edge given it's ID.

**findAllParents()** - Recursive function, returns all the upstream edges given it's ID.

**GetShapesById()** - Subsetts catchments or edges by ID. Useful if **GetShapesInBounds()** is not properly subsetting or if are to simulate is smaller than HUC 10.

**CorrectEdgeSlopes()** - Because many slopes calculated in ArcHydro are negative or zero, this function sets them to a minimum slope value.

**AssignContribArea()** - Assigns contributing area to edges.

**AssignBfWidth()** - Assigns width dimensions to edges using equation (6).

**AssignAcoeff()** - Assigns "a" coefficient used in non-linear groundwater discharge relationship given in equation (4).

**MakeHydrographs()** - Creates hydrographs automatically given flow and gauge data.

## Running the Model

To run the model:

1. Clone the repository onto your computer. 
2. Download the shapfiles and data from [here](dummy), and move them into the directory of the repository.
3. Start an R session and set the working directory to the directory of the cloned repository.
4. Ensure you have all the necessary libraries installed, listed at the top of routeWater.r script.
5. Configure the parameters in the routeWater.r file to match your data, if not using sample data.
6. Execute commands in the rest of the routeWater.r file sequentially.

## Mechanics of Model

First, runoff is aggregated from the NetCDFs using the AggregateRunoff fucntion.
The NetCDF is converted to a raster brick and aggregated using the **extract()** function and catchment polygons.
A raster pixel is considered to be inside the catchment polygon if the center of the pixel falls within the boundaries of the catchment.
It may be possible to use the "weights" option of **extract()** in order to get only the portion of the raster that the polygon covers if they are not line up exactly.
This may also make it possible to use other catchment polygons and edges such as the NHD dataset.

After surface and subsurface runoff have been aggregated, the routing can be done. The routing is governed primarily by the following equation (1).

![eq1] <sub>(1)</sub>

The term ![dsdt] represents the change in storage for edges at each timestep.
This is equal to the inflow of surface runoff, R<sub>s</sub>, plus groundwater discharge, Q<sub>gw</sub>, from stored subsurface runoff, plus inflow from upstream edges, Q<sub>in</sub>, minus edge discharge, Q<sub>out</sub>, and loss to infiltration, irrigation, evaporation, etc., Q<sub>loss</sub>.
Q<sub>loss</sub> is currently set to zero for simplicity.

Q<sub>out</sub> is governed by equations (2) and (3).
L is the length of a stream reach, in km, v is the velocity of that stream reach at time t, in the same units as L and &Delta;t.
Assuming &Delta;t is 1, and that Q<sub>in</sub> comes in evenly throughout the reach, the term ![1-L/v] gives fraction of water that will leave the reach at each timestep. 
Assuming inflows R<sub>s</sub> and Q<sub>gw</sub> are distrubuted evenly throughout the reach and that they too come in evenly througout the day, the fraction that will leave the reach in a timestep is given by ![1-L/2v].
If ![L/v] is less than 1, all storage in the river, S<sub>riv</sub>, from the previous time step will exit the reach.

![qOut1] <sub>(2)</sub>

Equation (2) is stable and conserves mass given that ![L/v <= 1] , but when it isn't, given a very L, or very low velocity, equation (2) is used.
In this case, the distance water could move in a given timestep is less than the reach length, so none of the Q<sub>in</sub> will exit the reach in a timestep.
The term ![v/L] gives the fraction of the reach for which ![L/v <= 1] is true, and all of S<sub>riv</sub> in that sement of the reach exits in a given timestep.
Using the same ![1-L/2v] routing method as above, we find that for the fraction of the reach given by ![v/L] , the L, in this case is equal to v&Delta;t , equating the term to 1.
Therefore, one half of the surface runoff and groundwater discharge entering the reach sub-segment, will exit in a timestep. Above that sub-segment, none will exit.
This gives us the term ![v/2L] .
 
![qOut2] <sub>(3)</sub>

Groundwater dishcharge is based on a  non-linear storage discharge relationship (Gan and Luo, 2013).
At each timestep, subsurface runoff is added to groundwater storage, S<sub>gw</sub>, represented in equation (10), and the discharge, Q<sub>gw</sub>, is calculated the following timestep from equation (4).
A is a dimensionless parameter that is currently based off of catchment area, but needs to be calibrated with stream gauges.
In this case, b has been fixed to .5, giving exponential relationsip between storage and discharge. 

![qGw] <sub>(4)</sub>

![dSgw/dt]

At each timestep, velocity is calculated from the previous timestep's values using a modified Manning's equation.
R is the hydraulic radius given by equation (7). S is the slope, calculated in ArcGis using the ArcHydro toolset. 
Mannings coefficient, n,  is is set for the entire watershed, and is currently fixed to 0.07.
This could be calibrated in the future and made variable for each edge.

![vMann] <sub>(5)</sub>


Stream dimensions are rectangular, but in the future may be modified to be trapezoidal.
Width is calculated with an empirical power law function (Li et al., 2013) and calibrated to match observations.
Currently, a and b are fixed to 0.3, and 0.6, respectively.

![width] <sub>(6)</sub>

Height is calculated using equation (8) from previous timestep's storage and dimensions.
From this and width, hydraulic radius is calculated in equation (7) for use in Mannings equation.

![radius] <sub>(7)</sub>

![height] <sub>(8)</sub>

In the future, flood situations may be included where if heigh exceeds a bankfull height, calculated using an empirical power law equation (9), width will 5 times original width to account for flood plain, and mannings n could be increased.
This is not currently part of the model.

![Hbf] <sub>(9)</sub>

## Generation of ArcHydro Edges and Catchments

Some details and instructions for generating the arcHydro network are found in NoteBooks/waterCenterNotebook_5-19-2015.ipynb.
Will be cleaned up and added to README.md in the future

### References
Li HY, Wigmosta MS, Wu H, Huang M, Ke Y, Coleman AM, Leung LR. 2013a. A physically based runoff routing model for land surface and earth system models. Journal of Hydrometeorology 14:808–828.

Gan, R., Luo, Y., 2013. Using the nonlinear aquifer storage–discharge relationship to simulate the baseflow of glacier and snowmelt dominated basins in northwest China. Hydrol. Earth Syst. Sci. 17, 3577–3586. 

## ToDo:

Notes:

To encode latex:
\ - %5C
{ - %7B
} - %7D
= - %3D
+ - &plus;


[eq1]: https://latex.codecogs.com/gif.latex?%5Cfrac%7B%5Cmathrm%7Bd%7DS%7D%7B%5Cmathrm%7Bd%7Dt%7D%3DR_%7Bs%7D&plus;Q_%7Bgw%7D&plus;Q_%7Bin%7D-Q_%7Bout%7D-Q_%7Bloss%7D

[dSgw/dt]: https://latex.codecogs.com/gif.latex?%5Cfrac%7B%5Cmathrm%7Bd%7DS_%7Bgw%7D%7D%7B%5Cmathrm%7Bd%7Dt%7D%3DR_%7Bsub%7D-Q_%7Bgw%7D-Q_%7Bgwloss%7D

[qOut1]: https://latex.codecogs.com/gif.latex?Q_%7Bout1%7D%3DS_%7Briv%7D&plus;(1-%5Cfrac%7BL%7D%7Bv%5CDelta&space;t%7D)%5Csum&space;Q_%7Bin%7D&plus;(1-%5Cfrac%7BL%7D%7B2v%5CDelta&space;t%7D)(R_%7Bs%7D&plus;Q_%7Bgw%7D)

[qOut2]: https://latex.codecogs.com/gif.latex?Q_%7Bout2%7D%3D%5Cfrac%7Bv%5CDelta&space;t%7D%7BL%7DS_%7Briv%7D&plus;(%5Cfrac%7Bv%5CDelta&space;t%7D%7B2L%7D)(R_%7Bs%7D&plus;Q_%7Bgw%7D)

[qGw]: https://latex.codecogs.com/gif.latex?Q_%7Bgw%7D%3D(%5Cfrac%7BS_%7Bgw%7D%7D%7Ba%7D)^%7B%5Cfrac%7B1%7D%7Bb%7D%7D 

[vMann]: https://latex.codecogs.com/gif.latex?v%3D%5Cfrac%7BR^%7B%5Cfrac%7B2%7D%7B3%7D%7DS^%7B%5Cfrac%7B1%7D%7B2%7D%7D%7D%7Bn%7D

[width]: https://latex.codecogs.com/gif.latex?W%3Da(A_%7Btotal%7D)^b

[radius]: https://latex.codecogs.com/gif.latex%5Cdpi%7B100%7D?R%3D%5Cfrac%7BA%7D%7BP%7D%3D%5Cfrac%7BHW%7D%7B2H&plus;W%7D

[height]: https://latex.codecogs.com/gif.latex%5Cdpi%7B100%7D?H%3D%5Cfrac%7BS_%7Briv%7D%7D%7BLW%7D

[Hbf]: https://latex.codecogs.com/gif.latex%5Cdpi%7B100%7D?H_%7Bbf%7D%3Da(A_%7Btotal%7D)^b


[dsdt]: https://latex.codecogs.com/gif.latex?%5Cfrac%7B%5Cmathrm%7Bd%7DS%7D%7B%5Cmathrm%7Bd%7Dt%7D

[1-L/v]: https://latex.codecogs.com/gif.latex?(1-%5Cfrac%7BL%7D%7Bv%5CDelta&space;t%7D)

[1-L/2v]: https://latex.codecogs.com/gif.latex%5Cdpi%7B100%7D?(1-%5Cfrac%7BL%7D%7B2v%5CDelta&space;t%7D)

[L/v]: https://latex.codecogs.com/gif.latex%5Cdpi%7B100%7D?%5Cfrac%7BL%7D%7Bv%5CDelta&space;t%7D

[L/v <= 1]: https://latex.codecogs.com/gif.latex%5Cdpi%7B100%7D?%5Cfrac%7BL%7D%7Bv%5CDelta&space;t%7D&space;%5Cleq&space;1

[v/L]: https://latex.codecogs.com/gif.latex%5Cdpi%7B100%7D?%5Cfrac%7Bv%5CDelta&space;t%7D%7Bl%7D

[v/2L]: https://latex.codecogs.com/gif.latex%5Cdpi%7B100%7D?%5Cfrac%7Bv%5CDelta&space;t%7D%7B2L%7D
