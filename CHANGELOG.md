# Changelog

## 0.2.0
2026-10-03

Mainly updated translation keys, as well as R and renv.

Updated:
- R 4.5.2 to R 4.6.1
- renv 1.0.9 to 1.3.0
- Updated instructions with clearer meaning and formatting
- About tab uses markdown

Removed:
- renv env in .Rprofile as it is already defined in the Dockerfile

## 0.1.0
2026-09-26

Introduced initial app functionality.

Added:
- Calculating milestones based on previous fixed activity and an average prolongation moving forward.
    - Can show activity as either a timeline or when reaching percent of total activity
- Calculating milestones based on semester specific prolongation/activity values. Gives a slightly more accurate milestone estimate.
    - Can show activity as either a timeline or when reaching percent of total activity
- Added advanced mode
    - Able to change current date to a different one
    - Able to enable a comparison scenario, i.e., comparing a 10% to 20% prolongation.
- Instructions tab
- Changelog tab
- About tab
- Language selector
    - Added languages are English and Swedish.