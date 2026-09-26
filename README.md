# PhD-forecast

> **⚠️ Beta software — projections may be inaccurate. Use for guidance only.**

**PhD-forecast** is an interactive R Shiny application for estimating timelines, milestones, and projected completion dates for PhD studies primarily in Sweden based on recorded prolongation (extensions) and individual circumstances.

The tool is at heart intended for doctoral students, but also for any person who need a transparent way to explore how different prolongation scenarios affect the total duration of doctoral education.

---

## What this app does

Swedish doctoral education typically has a nominal duration (e.g., four years full-time), but actual timelines are often extended due to teaching duties, departmental service, parental leave, sick leave, or other approved prolongations.

This app helps you:

- Estimate adjusted end dates based on prolongation  
- Explore “what-if” scenarios interactively  
- Visualize timeline impacts  
- Understand how different factors accumulate over time  

---

## Features

- Interactive calculation of projected PhD completion date  
- Visualization of individual prolongation scenarios  
- Scenario exploration without modifying underlying data  
- Multilingual interface (via JSON language files)  
- Fully reproducible environment using `renv`

---

## Intended use

This application is designed as an informational and planning aid only.

It **does not replace official decisions, regulations, or university records.**  
Always consult your department (e.g. Head of department or Director of studies third cycle) or HR for authoritative information about your doctoral timeline.

---

## Requirements

### For local execution

- R ≥ 4.4.2  
- RStudio (recommended)  
- Internet connection (first run only, to install packages)  

All R dependencies are managed using `renv`, ensuring reproducibility.

---

## Running locally (RStudio)

Clone the repository:

```bash
git clone https://github.com/thoroo/phd-forecast.git
cd phd-forecast
```

```r
install.packages("")
```

## Limitations

- Results depend entirely on user-provided data
- Institutional or department rules may vary between universities
- Edge cases may not be handled correctly
- The model may evolve over time

## Licence

This app and corresponding code is licences under MIT.

## Author

Developed by [Thomas Roosdorp](https://orcid.org/0009-0002-5146-2777), a current doctoral student at the Department of Food Studies, Nutrition and Dietetics at Uppsala university.

## Disclaimer

This tool is provided “as is”, without warranty of any kind.
The author assumes no responsibility for decisions made based on its output.
