# Code Folder

There are two folders associated with Code. These correspond with the projects assigned in class. Project 1 focused on data cleaning. The code used to clean the penguins_raw_dirty file (found in the `Data` folder) can be found in the Processing_code folder. Project 2 focuses on data analysis. The code used for this assignment will be found in the `Analysis_code` folder. 

Users exploring this data are encouraged to follow the structure of the assignment by reviewing the Project 1 material before that of Project 2. 

# Code Documentation

## `processingcode.R` - cleans raw data, outputs clean data

### Inputs
		reads in the following files from `../../Data/Raw_data/`
		`penguins_raw_dirty.csv` - Raw data
		`datadictionary.csv` - original data dictionary for raw data

### Outputs
		outputs to `../../Data/Processed_data/`
		`penguins.rds` - clean data in rds (R) format
		`penguins.csv` - clean data in .csv format
		`datadictionary2.0.csv` - Updated data dictionary 

