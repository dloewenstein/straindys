# straindys
Functions for CMR feature tracking strain analysis

## Introduction
This R package implements different methods for analyzing CMR feature-tracking derived data from the [strain module](http://medviso.com/products/strain/) of the software [Segment](http://www.medviso.com) (v2.0, Medviso&reg;, Lund, Sweden)

DISCLAIMER: This package has no official association with Medviso&reg;
## Currently implemented methods

- Septal-to-Lateral wall Delay (SLD)
    - perc: strainpeak criteria as percent of maximum strain (SLD<sub>perc</sub>)
    - point: strainpeak criteria with threshold and increase in percentage point strain (SLD<sub>point</sub>)
- Earliest-to-Latest Segment Delay (ELSD)
    - perc: strainpeak criteria as percent of maximum strain (ELSD<sub>perc</sub>)
    - point: strainpeak criteria with threshold and increase in percentage point strain (ELSD<sub>point</sub>)
- Circumferential Uniformity Ratio Estimate (CURE)
- Circumferential Uniformity Ratio Estimate by Singular Value Decomposition (CURE<sub>SVD</sub>)
