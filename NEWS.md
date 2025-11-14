## PHEindicatormethods v2.1.1
* No updates to code functionality.
* amended GitHub referencing for Bug Reports in DESCRIPTION as code repository 
now owned and hosted by DHSC not UKHSA-collaboration.
* Corrections to documentation where Fingertips URLs have been signposted as 
some of these have changed.

## PHEindicatormethods v2.1.0
This release makes changes to the DSR function
* `calculate_dsr` function added to replace `phe_dsr`. The new calculate_dsr function has three new features:
1. Users can now specify whether events are independent.  For non-independent events the data must be broken down by event frequency and the function will perform an adjustment to the confidence interval calculation. 
2. The stdpoptype argument that was available in `phe_dsr` is not available in `calculate_dsr` and the user is now required to provide the standard populations as a field in the input dataframe. This is to negate the risk of incorrectly joining the standard population vector by position that was possible when using the function with stdpoptype = "vector".
3. Because of point 2 above, users can now calculate DSRs for groups containing different numbers of age bands in a single execution (for example DSRs for all age populations and DSRs for under 75s).   
* `phe_dsr` has been soft deprecated - please replace all calls to this function with `calculate_dsr` before September 2025, after which date the phe_dsr function will be removed from the package. Previous calls to `phe_dsr` with stdpoptype set to "field" can be replaced directly with `calculate_dsr`, however, the stdpoptype argument has been deprectaed so will need to be removed and the stdpop argument no longer has a default so this column name will need to be provided explicitly. For previous calls to `phe_dsr` with stdpoptype = "vector", the user will need to append the standard populations to their input data frame before calling the `calculate_dsr` function and provide the stdpop argument explicitly.

## PHEindicatormethods v2.0.2
* Amended phe_quantile function so it will not produce quantiles when the number of small areas within a group is less than the number of quantiles requested. A warning will be generated when quantiles cannot be produced for this reason.
* removed the highergeog argument from phe_quantile function, previously soft-deprecated in v1.2.0.
* `phe_sii` amended to allow data to be transformed prior to calculation of the SII, and to allow the intercept value to be output.

## PHEindicatormethods v2.0.1
* `calculate_ISRate` and `calculate_ISRatio` updated so observed events can be passed as total without age breakdowns
* amended GitHub referencing as code repository now owned and hosted by UKHSA-collaboration not PublicHealthEngland

## PHEindicatormethods v2.0.0
* `phe_smr` and `phe_isr` functions which were previously soft-deprecated have now been removed - see NEWS from release version 1.4.0 for replacements for these functions.
* `phe_proportion` amended so option to request no confidence intervals is allowed.

## PHEindicatormethods v1.4.2
* updated for compatibility with tidyselect v1.2.0 and purrr releases due December 2022

## PHEindicatormethods v1.4.0

* `calculate_funnel_limits()`, `assign_funnel_significance()` and `prepare_funnel_plot()` functions added. This provides ability to calculate funnel plot lines and significance for provided data in line with [Fingertips](https://fingertips.phe.org.uk/profile/guidance/supporting-information/PH-methods) published technical guidance.
* `calculate_ISRate` and `calculate_ISRatio` functions replace `phe_isr` and `phe_smr` respectively. `phe_isr` and `phe_smr` functions remain available in this release but are lifecycle badged as deprecated and function documentation signposts users to the new replacement functions.  This is purely a renaming exercise to overcome ambiguous acronyms used in the original function names and there is no change to the functionality of the code. `phe_isr` and `phe_smr` are scheduled to be removed in a future release of the package, not before December 2022.
* `phe_life_expectancy` now forces suppression of LEs when the 95% confidence interval is greater than 20 years.


## PHEindicatormethods v1.3.2

`phe_dsr`, `phe_isr`, `phe_mean`, `phe_proportion`, `phe_rate`, `phe_smr`, `phe_life_expectancy` amended so output data frame has the same grouping attributes as the input data frame.

## PHEindicatormethods v1.3.1

`phe_proportion`, `phe_rate`, `phe_quantile`, `phe_life_expectancy`, `phe_sii`:  
Functions amended to ensure continued compatibility with dplyr when v1.0.0 is released.
These changes will not be noticeable to end users.

`phe_life_expectancy`: dropped population and death columns from output as these are no longer applicable to the final LE statistic.  Added pops_used and dths_used columns to output when type = 'full' which reflect the cumulative populations and deaths used in each LE calculation (ie the pops and deaths for all ages equal to or above the Life_Expectancy_At age)


## PHEindicatormethods v1.3.0
* `phe_sii function` updated to be able to output multiple confidence intervals
* `phe_life_expectancy()` function previously calculated life expectancy and confidence levels inaccurately when the number of deaths in all age groups except the final one was 40% or more of the size of the population in any age group (apart from the 1–4 age group where it’s 50%). It had an affect in very few scenarios, but was inaccurate nonetheless. This has now been corrected to agree with the published methodology
* `phe_life_expectancy()` now accepts a numeric vector as an input to the `confidence` argument so users can calculate multiple confidence limits in one command
* `phe_dsr` The standardised_pop output variable added in v1.2.0 has been removed as this was not providing a meaningful value. This may result in some backwards incompatibility if code has been written to reference (eg drop) this column as it will no longer be included in the phe_dsr output.


## PHEindicatormethods v1.2.0
The following changes may affect backwards compatibility with earlier versions of the package:  

* phe_quantile - highergeog argument deprecated.  Argument wasn't providing any additional functionality and its use was affecting grouping sets on output data frame.  Input dataframe must now be pre-grouped to reproduce the results previously obtained using the highergeog argument.  Also documentation references to geographies removed as function is applicable to other subgroups.

<br>

The following changes will not affect backwards compatibility with earlier versions of the package: 

* Amended functions to use new embrace interpolation features of rlang 0.4.0: dsr, isr, smr, rate, proportion, mean, quantile.  Package dependencies now require Rlang version 0.4.0 or higher   

* Added ability to output both 95% and 99.8% confidence intervals in a single execution to dsr, isr, smr, rate, proportion, mean functions

* Amended phe_dsr to also output the standardised population used in the calculation when type="full"

* Some warnings that occurred with `phe_life_expectancy()` have been fixed

## PHEindicatormethods v1.1.5
phe_sii function updated to be compatible with nest and unnest functions from tidyr version 1.0

## PHEindicatormethods v1.1.4
No significant changes.
Removed confusing line of commented out code from phe_life_expectancy calculation
Amended vignette title

## PHEindicatormethods v1.1.3

Minor amendments to package testing scripts to work on platforms using clang as compiler.  
Updates will not affect end users using other platforms.  

## PHEindicatormethods v1.1.2

Minor amendments to package testing scripts to work on platforms using clang as compiler.
Updates will not affect end users using other platforms.  


## PHEindicatormethods v1.1.1

The following changes may affect backwards compatibility with earlier versions of the package:  

* phe_proportion: Replaced logical PERCENTAGE argument with a numeric MULTIPLIER argument to enable proportions to be expressed more flexibly (eg per 1000). Default is now multiplier = 1 which gives equivalent output to the previous default of percentage = FALSE.  There is loss of backwards compatibility where the percentage argument has previously been specified in the function call - please replace 'percentage=TRUE' with 'multiplier=100' and 'percentage=FALSE' with 'multiplier=1'.

* all functions using the TYPE argument: Altered the default value for the TYPE argument from 'standard' to 'full'.  This may affect backwards compatibility with earlier versions of the package - where type has not been specified output will now contain additional metadata columns - to remove these please specify type = "standard" as a function argument.

* `byars_lower()`, `byars_upper()`, `wilson_lower()` and `wilson_upper()` functions have been moved to the package utilities and are less readily available to be used as stand-alone functions within the package.  These functions are intended to be called using other 'phe-' prefixed package functions.  

<br>

The following changes will not affect backwards compatibility with earlier versions of the package:  

* `phe_life_expectancy()` function added for calculation of life expectancy at different ages based on population and death data

* `phe_quantile()` function added for assigning data to quantiles

* `phe_sii()` function added for calculation of slope index of inequality and relative index of inequality

* `phe_proportion()` and `phe_rate()` functions now return aggregate data when grouped data is passed

* Altered handling of NA values in `phe_proportion()`, `phe_rate()` and `phe_mean()` functions to enable results to be returned for the records which do not contain invalid NAs 

* Altered handling of NA values in `phe_dsr()` function such that NA values in the age-specific count data are assumed to be equal to zero and no longer cause the function to error

* Altered handling of NA values in `phe_isr()` and `phe_smr()` functions such that NA values in the age-specific count, age-specific population and/or reference count data are assumed to be equal to zero and no longer cause the functions to error

 


## PHEindicatormethods V1.0.8
This is the first release of this package to CRAN
