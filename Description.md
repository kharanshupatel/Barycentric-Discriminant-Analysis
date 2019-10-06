### Method : Barycentric Discriminant Analysis (BADA)
```
Barycentric discriminant analysis is a robust version of discriminant analysis that is used like discriminant analysis when multiple measurements describe a set of observations in which each observation belongs to one category or group from a set of a-priori defined categories. 

The goal of BADA is to combine the measurements to create new variables called components or discriminant variables that best separate the categories. These discriminant variables are also used to assign the original observations or
“new” observations to the a-priori defined categories.

BADA combines the original variables to create new variables that best separate the groups and that can also be used to optimally assign old or new observations to these categories. The quality of the performance is evaluated by cross-validation techniques that estimate the performance of the classification model for new observations.
```

### BADA - Dataset : oasis_cross-sectional
```
The Open Access Series of Imaging Studies (OASIS) is a project aimed at making MRI data sets of the brain freely available to the scientific community.
It is a cross-sectional MRI Data in Young, Middle Aged, Nondemented and Demented Older Adults.
This set consists of a cross-sectional collection of 216 subjects (rows) aged 33 to 96. The subjects include both men and women. 
It measures 216 subjects (rows) on 4 quanitiative variables (columns). The 5 qualitative variables have been used to interpret the data. 

Measures included in the study:

Age: Age (years)

Gender: Male or Female

Education: Years of Education - Recoded into categories from 1 (highest) to 5 (lowest)

SES: Socioeconomic status classified into categories from 1 (highest) to 5 (lowest)

MMSE: Mini-Mental State Examination score – range from 0 (worst) to 30 (best)

CDR: Clinical Dementia Rating - (0 = no dementia, 0.5 = very mild AD, 1 = mild AD, 2 = moderate AD)

ASF : Atlas scaling factor (unitless). Computed scaling factor that transforms native-space brain and skull to the atlas target (i.e. the determinant of the transform matrix)

eTIV: Estimated total intracranial volume (cm3)

nWBV: Normalized whole brain volume, expressed as a percent of all voxels in the atlas-masked image that are labeled as gray or white matter by the automated tissue segmentation process 
```
