# Random forest importance operator

##### Description

`rfsrc` operator computes random forest importance values for a given dataset. It supports both numerical and categorical variables and can automatically impute missing values.

##### Usage

Input projection|.
---|---
`labels`   | indicates the groups to compare 
`row`  | indicates the variables (e.g. genes, markers) 
`col`  | indicates the observations (e.g. samples, cells, individuals) 
`y-axis`| measurement value

Input projection|.
---|---
`find_interactions`   | compute interaction importance between variables

Output relations|.
---|---
`importance`| numeric, importance value calculated per row (i.e. variable)

##### Details

`rfsrc` operator computes random forest importance values for each independant variable (i.e. row) of the dataset. An importance value is a value representing how important the variable is for the seperation of the groups. 
