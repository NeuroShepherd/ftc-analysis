# Data Folders

In approximate order of usage

## `google-sheets`

Contains a markdown version of Tony Holler's master Google Doc for linking out to his yearly track and field practice results. There is a file for additional links I located that were not on the master sheet. The csv file is a tabular format of the information from the links in the markdown document, and some information about the download status of the files

## `downloads` *Local Only*

These are the downloaded data files. I have opted to not reshare them here for privacy purposes. The code for actually downloading the data is found in the [`scripts/`](scripts/) folder

## `manifests`

Provides an overview of the documents in `downloads/` such as the file names, the names nand number of sheets within each Excel workbook, and so on.

## `extraction`

Contains the R code for extracting rosters, 10m fly, and (possibly in the future) 30 yard or other sprint data that has been extracted from the data in `downloads/`.

Additional, the anonymized .Rds files can be found here.