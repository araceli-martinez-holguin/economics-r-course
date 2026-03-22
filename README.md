# R Programming for Economists

**Instructor:** Araceli Martínez Holguín  
**Institution:** School of Economics, National Autonomous University of Mexico (UNAM)  
**Course:** Economics Programming with R  
**Semester:** February–June 2026 (Semester 2026-2)

🌐 **[View Course Website](https://araceli-martinez-holguin.github.io/economics-r-course/)**

---

## 📖 Course Overview

This repository contains R code, laboratories, datasets, and assignments for the **Economics Programming with R** course at UNAM's School of Economics.

The objective of the course is to introduce students to **empirical economic analysis using modern data science tools**. Students learn how to clean, visualize, and analyze economic datasets using **R and RStudio**, while developing reproducible workflows through **R Markdown**.

Laboratories combine programming techniques with **real economic datasets**, allowing students to apply computational tools to topics such as climate data, agricultural production, and macroeconomic indicators.

---

## 🎯 Learning Objectives

By the end of the course students should be able to:

- Understand and use **basic R programming structures**
- Manipulate and transform datasets using the **tidyverse**
- Produce **clear and informative data visualizations**
- Work with **economic time series**
- Estimate and interpret **linear regression models**
- Conduct **basic empirical economic analysis**
- Produce **reproducible reports using R Markdown**

---

## 📁 Folder Structure

```
economics-r-course/
├── Laboratories/              # Guided R Markdown labs (.Rmd + rendered .html)
│   ├── Laboratory-0/          # Types of objects in R
│   └── Laboratory-1/          # Analyzing climate variables (CRU data)
├── Assignments/               # Student assignment prompts and templates
│   └── Assignment-1.Rmd       # Homework based on Laboratory 1
├── Data/                      # Datasets used across labs and assignments
│   └── cultivos.xlsx          # SIAP agricultural production data (Mexico)
├── index.html                 # Course website homepage
└── README.md                  # You are here
```

---

## 🧪 Laboratories

| Lab          | Topic                              | Status        | Link |
|--------------|------------------------------------|---------------|------|
| Laboratory 0 | Types of objects in R              | ✅ Available  | [View](https://araceli-martinez-holguin.github.io/economics-r-course/Laboratories/Laboratory-0/Laboratory-0.html) |
| Laboratory 1 | Analyzing climate variables (CRU)  | ✅ Available  | [View](https://araceli-martinez-holguin.github.io/economics-r-course/Laboratories/Laboratory-1/Laboratory-1.html) |
| Laboratory 2 | Agricultural data with dplyr       | 🔜 Coming soon | — |
| Laboratory 3 | Spatial analysis in R              | 🔜 Coming soon | — |
| Laboratory 4 | Time series analysis               | 🔜 Coming soon | — |

> More laboratories will be added throughout the semester.

---

## 📝 Assignments

| Assignment   | Topic                              | Status        | Based on     |
|--------------|------------------------------------|---------------|--------------|
| Assignment 1 | Climate variables — homework       | ✅ Available  | Laboratory 1 |
| Assignment 2 | Agricultural data — homework       | 🔜 Coming soon | Laboratory 2 |
| Assignment 3 | Spatial analysis — homework        | 🔜 Coming soon | Laboratory 3 |

Assignments are distributed as `.Rmd` templates. Students fill in the code and interpretation, then submit the knitted `.html` file.

---

## 🗄️ Data

| Dataset         | Description                              | Used in      |
|-----------------|------------------------------------------|--------------|
| CRU TS 4.09     | Climate Research Unit · monthly climate data 1901–2024 | Laboratory 1, Assignment 1 |
| cultivos.xlsx   | SIAP · agricultural production in Mexico | Laboratory 2, Assignment 2 |
| Macroeconomic indicators | Inflation · GDP · monetary policy | Laboratory 4 (coming soon) |

The CRU dataset is downloaded directly from the [CRU website](https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.09/) during the lab session. All other datasets are included in the `Data/` folder.

---

## 📚 Topics Covered

1. Introduction to R and RStudio
2. Data types and data structures
3. Data manipulation with `dplyr` and `tidyr`
4. Economic data visualization with `ggplot2`
5. Time series analysis
6. Linear regression models
7. Economic forecasting
8. Causal inference

---

## 🛠️ Requirements

- [R](https://cran.r-project.org/) (version 4.0 or higher)
- [RStudio](https://posit.co/download/rstudio-desktop/)

Main packages used in the course:

- `tidyverse`
- `readr`
- `ggplot2`
- `dplyr`
- `forecast`
- `lmtest`

Install all packages at once:

```r
install.packages(c("tidyverse", "readr", "ggplot2", "dplyr", "forecast", "lmtest"))
```

---

## 🚀 How to Use

1. Clone this repository:

```bash
git clone https://github.com/araceli-martinez-holguin/economics-r-course.git
```

2. Open the project in **RStudio**

3. Navigate to any `Laboratories/` folder and open the `.Rmd` file

4. Run chunks in order or **Knit to HTML** to reproduce the full laboratory report

5. For assignments, open the corresponding `.Rmd` file in `Assignments/`, complete the code and interpretations, and knit to HTML

---

## 📈 Student Projects

Toward the end of the semester, students complete a short **empirical economic project** using R.

Projects typically include:

- Data cleaning and preparation
- Exploratory data analysis
- Data visualization
- Basic econometric analysis
- Interpretation of results in a reproducible report

Students submit their work as **R Markdown reports**.

---

## 🔁 Reproducibility

All laboratories and assignments are written in **R Markdown**, allowing students to combine code, results, figures, and economic interpretation in a single reproducible document.

---

## 📄 License

MIT License — Free for educational use.
