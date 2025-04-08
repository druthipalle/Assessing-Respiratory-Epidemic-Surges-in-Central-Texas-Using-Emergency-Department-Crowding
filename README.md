# Assessing-Respiratory-Epidemic-Surges-in-Central-Texas-Using-Emergency-Department-Crowding

Respiratory illness surges, such as influenza and COVID-19, can overwhelm emergency departments (EDs), leading to increased wait times, resource strain, and compromised patient care. During the COVID-19 pandemic, the Capital Area Trauma Regional Advisory Council (CATRAC) required hospitals to report National Emergency Department Overcrowding Scale (NEDOCS) scores for real-time situational awareness. NEDOCS quantifies overcrowding using factors like wait times and hospital capacity, scaled from 0 (Normal) to 180+ (Disaster).

## Poster Presentation 
https://docs.google.com/presentation/d/1pJxs1qC1V8_n4cWPNZJVVEqMqgfloWWx/edit?usp=sharing&ouid=106943682813591117218&rtpof=true&sd=true

## Research Question 
Does NEDOCS predict COVID-19 and influenza inpatient and ICU admissions? Does hospital size matter?

## Usage
To run the full analysis pipeline, use the following workflow:
1. **Generate Hourly and Daily Processed Data**  
   Run `code/nedocs_timeseries_analysis.R` to convert raw NEDOCS input into:
   
   - Hourly data per hospital
   - Aggregated daily data across all hospitals  
   - These outputs are saved in the `produced_data/` folder.

   You can execute this script from the command line using: `Rscript code/nedocs_timeseries_analysis.R 1,2,3,4,5,6 TRUE`

2. **Correlation Analysis by Hospital Size**  
   Run `code/correlate_timeseries.R` to analyze correlations between NEDOCS scores and respiratory epidemic surges (COVID-19 and flu) across different hospital sizes.
   - The resulting correlation plot is saved in the `figures/` folder.

   *Note: This script cannot be executed from the command line and should be run interactively in an R environment.*

## Contributions
Feel free to fork this repository, submit issues, and contribute enhancements or bug fixes!

## License
Copyright (c) 2025 Druthi Palle and Emily Javan

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

