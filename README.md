Vignette on classifying household density categories using logistic regression and random forest models with California Household Travel Survey (CHTS) data; created as a class project for PSTAT 197A in Fall 2024.

# vignette-householdclassification

### Contributors

-   Rebecca Chang

-   Valerie De La Fuente

-   Mai Uyen Huynh

-   Tess Ivinjack

-   Shirley Wang

## Abstract

Vignette abstract: a brief description in a few sentences of your vignette topic, example data, and **outcomes**.

This vignette explores how logistic regression and random forest models can classify household density categories (urban, suburban, exurban, rural) using data from the 2010-2012 California Household Travel Survey (CHTS). The dataset includes variables such as household demographics, travel behaviors, vehicle ownership, parking preferences, work schedules, toll lane usage, and active travel modes. By employing Principal Component Analysis (PCA), we reduce the dataset's high dimensionality, distilling numerous variables into a manageable set of key components that retain the most significant variance. Logistic regression, known for its interpretability, is then used to model the relationship between these principal components and household density categories, providing insights into how demographic and travel behavior variables influence classification. In contrast, random forest, an ensemble learning method, is employed to explore complex, non-linear relationships within the data, leveraging decision tree structures to enhance prediction accuracy.

## Repository Contents

Repository contents: an explanation of the directory structure of the repository

-   `data` contains

    -   `counties` - a folder storing and managing spatial data in geographic information systems (GIS) for geospatial data processing

    -   `DataDictionary.xlsx` - a data file guide spreadsheet detailing each of the files listed below, with descriptions of each variable and the possible values they can take

    -   `hh_bgDensity.Rds` - contains cleaned block group density data that characterizes the urbanicity of areas surrounding the homes of CHTS respondents

    -   `HHData_111A.Rds` - contains cleaned household-level demographics, survey date, and home county information

    -   `PersonData_111A.Rds` - contains cleaned per-person data, including basic demographics, employment/student status, and travel behavior variables

-   `scripts` contains

    -   `PCA` contains principal component analysis, logistic regression, and random forest
    -   `EDA` contains exploratory data analysis of the data
-   `vignette` contains the final vignette document

## Reference List

Reference list: 2 or more references to learn more about your topic.

-   Dataset Source/Information

    -   [California Department of Transportation Final Report](https://lede-admin.cal.streetsblog.org/wp-content/uploads/sites/52/2015/04/FinalReport.pdf)

    -   [InfrastructureUSA](https://infrastructureusa.org/california-household-travel-survey-2/)

    -   [Transportation Research Board](https://trid.trb.org/view/1308918)

    -   [DOE Data Explorer - U.S. Department of Energy Office of Scientific and Technical Information](https://www.osti.gov/dataexplorer/biblio/dataset/1924686)

-   Principal Component Analysis (PCA)

    -   [Principal Component Analysis](https://www.geeksforgeeks.org/principal-component-analysis-pca/)

-   Logistic Regression Model

    -   [ML from Scratch - Multinomial Logistic Regression](https://towardsdatascience.com/ml-from-scratch-multinomial-logistic-regression-6dda9cbacf9d)

-   Random Forest Model

    -   [Random Forest Algorithm in Machine Learning](https://www.geeksforgeeks.org/random-forest-algorithm-in-machine-learning/)

A typical README file would also contain instructions on use and instructions on contributing to the repository.
