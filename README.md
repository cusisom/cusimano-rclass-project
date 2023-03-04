# Overview

I am intentionally keeping most of the original content in here until the project is completed. 


# History

2023-02-16 First release with Data and Code folders with materials for data cleaning and initial exploration.

# Acknowledgement

 This template was cloned and modified from <https://github.com/andreashandel/dataanalysis-template>

# Software requirements

This template lays out a data analysis project and report writing using R, Quarto, Github and a reference manager for bibtex. A plain text editor is also necessary, and wordprocessing software to open .docx if you wish to use that format (e.g. MS Word, MacOS Pages, or [LibreOffice](https://www.libreoffice.org/)). 

For more R packages supporting reproducible research check out the taskview <https://cran.r-project.org/web/views/ReproducibleResearch.html>

# Template structure

The template supports the notion that there should _only be one copy of code and any outputs_. Any time the intellectual content needs to be reused, it should be referred to or linked from the one copy. This way we can prevent different copies of the same content from accidentally diverging, and it is easier to maintain the project. 

This template also uses the convention that Folder names begin with a capital letter. 

* All data goes into the `Data` folder and any subfolders.
* All code goes into the `Code` folder or subfolders.
* All results (figures, tables, computed values) go into `Results` folder or subfolders.
* All products (manuscripts, supplement, presentation slides, web apps, etc.) go into `Products` subfolders.
* See the various `README.md` files in those folders for some more information.

# Template content

All of the folders contain template files that should filled with some content required for your analysis. Look inside each folder for template files that provide examples of the types of content that go there. 

The template files also demonstrate how information is linked together across folders. 

Please see the `README.md` files in each folder for more details.

* This template has been populated with _Palmer Penguins_ morphology and life history data in the `Raw_data` folder. 
* The `Processing_code` folder contains several files that load the raw data, perform a bit of cleaning, and save the result in the `Processed_data` folder. 
* The `Analysis_code` folder contains several files that load the processed data, do an exploratory analysis, and fit a simple model. These files produce figures and some numeric output (tables), which are saved to the `results` folder.
* The `Products` folder contains an example `bibtex` and CSL style files for references. Those files are used by the example manuscript and slides.
* The  `Manuscript` folder contains a template for a report written as Quarto file. There is also a sub-folder containing an example template for a supplementary material file as is common in scientific articles these days.
* The `slides` folder contains a basic example of slides made with Quarto. 

**For the first project, we only have Data and Code folders.**
The rest will come soon. 


