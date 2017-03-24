# JurkatQCScraper

See live:

https://ozshiny.shinyapps.io/JurkatQCscraper/

## JurkatQCscraper is a comprehensive online analytics tool to monitor Mass Spectrometer Performance. It is build by using Rshiny package.

- Mass Spectrometry users inject 1 ug tryptic Jurkat cell peptide mixture to instruments and run a 110 min gradient with a standardized LC-MS/MS run method. 
- This provides an objective quality control that helps to monitor instrument performance by using powerful Quality Metrics provided by Spectrum Mill. 
- JurkatQC Scraper actively monitors servers to detect new Jurkat Quality Metrics, provided that the raw files were searched in Spectrum Mill. 
- JurkatQC Scraper archives Quality Metrics data, extracts useful LC_labels, time and user attributes, and present helpful data analytics. 
- JurkatQC Scraper also provides a user interface to obtain, store and present mass spectrometry user comments and maintanence records.

## Evolution of the JurkatQC Scraper
### Version 0.3
- Calibrated Distinct Peptide Gauges with the historical performance of each instrument
- Included a color key to interpret peptide gauges
- Included a new app to monitor Instrument-specific QC performance along with users, in a defined time window
- Completed integration of the new instrument: Yoda
- Included a video tutorial for using JurkatQC Scraper

### Version 0.2
- Included a new app to monitor LC-specific analytics
- Incorporated Java Script definition for the dynamic resizing of the most apps based on screen size, aimed to improve user performance
- Improved the server logic for faster launching of the user interface during data scanning
- Fixed a bug that arises during the simultaneous access of multiple users
- Started integrating new instrument Yoda and fixed bugs arising from this addition

### Version 0.1
- Beta-version for limited use
