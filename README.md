# R Programming for Economists

**Instructor:** Araceli Martínez Holguín  
**Institution:** School of Economics, National Autonomous University of Mexico (UNAM)  
**Course:** Economics Programming with R  
**Semester:** August–November 2026 (Semester 2027-1)

🌐 **[View Course Website](https://araceli-martinez-holguin.github.io/economics-r-course/)**

---

## 📖 Course Overview

This repository contains R code, laboratories, and exercises for the **Economics Programming with R** course at UNAM's School of Economics. Assignments and additional datasets will be added to the repository as the course progresses.

The objective of the course is to introduce students to **empirical economic analysis using modern data science tools**. Students learn how to clean, visualize, and analyze economic datasets using **R and RStudio**, while developing reproducible workflows through **R Markdown**.

Laboratories combine programming techniques with **real economic datasets**, allowing students to apply computational tools to topics such as climate data, agricultural exports, household income and expenditure, and macroeconomic indicators.

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
│   └── Laboratory-0/          # Data types and basic manipulation in base R
├── Exercises/                 # Short practice exercises (.Rmd + rendered .html)
│   └── Exercise-0/            # Objects in R with the Gapminder dataset
├── Data/                      # Datasets used across labs (populated as labs are added)
├── index.html                 # Course website homepage
├── styles.css                 # Course website stylesheet
└── README.md                  # You are here
```

> More laboratories, exercises, and datasets will be added as the semester
> progresses, following the weekly program listed below.

---

## 🧪 Laboratories

| Lab          | Topic                                       | Status       | Link |
|--------------|----------------------------------------------|--------------|------|
| Laboratory 0 | Data types and basic manipulation in base R | ✅ Available | [View](https://araceli-martinez-holguin.github.io/economics-r-course/Laboratories/Laboratory-0/Laboratory-0.html) |

New laboratories are released weekly as each topic is covered in class (see the full 14-week program under **Topics Covered**).

---

## 🧩 Exercises

| Exercise    | Topic                              | Status       | Link |
|-------------|-------------------------------------|--------------|------|
| Exercise 00 | Objects in R with Gapminder data   | ✅ Available | [View](https://araceli-martinez-holguin.github.io/economics-r-course/Exercises/Exercise-0/Ejercicio0_Objetos_R_Gapminder.html) |

---

## 🗄️ Data

No datasets have been added to the repository yet. Datasets will be included alongside each new laboratory as it is released.

---

## 📚 Topics Covered

This is the full 14-week program for the course:

1. **First Steps in R** — installing R and RStudio; data types (numeric, character, logical); vectors, lists, and data frames; why R instead of Excel (reproducibility).
2. **Descriptive Statistics with the Economic Census** — categorical variables (sector, establishment size, state); frequency tables, measures of central tendency and dispersion.
3. **Getting Data via API** — programmatic download of economic series using Banxico's SIE API (`httr2`, `jsonlite`).
4. **Data Manipulation** — `dplyr` and `tidyr` (filter, group_by, summarise, mutate, left_join, pivot_longer/wider) with SIAP agricultural data.
5. **Data Visualization** — publication-ready graphics with `ggplot2`; best practices; static maps with `sf`.
6. **Climate Time Series** — downloading, cleaning, and visualizing historical CRU series (precipitation, temperature) 1901–2024; reading data with `readr`.
7. **Quarterly Macroeconomic Series** — the implicit GDP deflator: series manipulation, custom functions, dates with `lubridate`, deflator vs. CPI comparison.
8. **Complex Survey Design** — poverty estimation with ENIGH 2024: sample design (`survey`/`srvyr`), income deciles, the Gini coefficient, poverty lines.
9. **Linear Regression: Foundations and Interpretation** — simple and multiple models with Penn World Table data; Gauss-Markov assumptions, interpreting coefficients, goodness of fit (R²).
10. **Linear Regression: Robustness Checks** — on the same Penn World Table model: multicollinearity (VIF), heteroskedasticity (Breusch-Pagan, robust HC errors), functional form (RESET test).
11. **Binary Choice Models: Logit and Probit** — labor force participation and informality with the ENOE; marginal effects, confusion matrix, and the ROC curve.
12. **Causal Inference: Foundations and Difference-in-Differences** — correlation vs. causation, omitted variable bias, natural experiments; the Bangladesh microfinance case (Pitt & Khandker, 1998): cross-sectional and panel DiD, the parallel trends assumption.
13. **Reproducibility and Course Wrap-up** — best practices with R Markdown and Git/GitHub; building a student's own lab from a dataset of their choice.
14. **Final Integrative Project** — a short applied analysis (teams of 2–3 students) combining at least two tools from the course on a real dataset.

---

## 🛠️ Requirements

- [R](https://cran.r-project.org/) (version 4.0 or higher)
- [RStudio](https://posit.co/download/rstudio-desktop/)

Main packages used in the course:

- `tidyverse`
- `readr`
- `ggplot2`
- `dplyr`
- `survey`
- `srvyr`
- `forecast`
- `lmtest`

Install all packages at once:

```r
install.packages(c("tidyverse", "readr", "ggplot2", "dplyr",
                   "survey", "srvyr", "forecast", "lmtest"))
```

---

## 🚀 How to Use

1. Clone this repository:

```bash
git clone https://github.com/araceli-martinez-holguin/economics-r-course.git
```

2. Open the project in **RStudio**

3. Navigate to any `Laboratories/` or `Exercises/` folder and open the `.Rmd` file

4. Run chunks in order or **Knit to HTML** to reproduce the full report

---

## 📈 Student Projects

Toward the end of the semester, students complete a short **empirical economic project** using R (see Week 14 of the program).

Projects typically include:

- Data cleaning and preparation
- Exploratory data analysis
- Data visualization
- Basic econometric analysis
- Interpretation of results in a reproducible report

Students submit their work as **R Markdown reports**.

---

## 🔁 Reproducibility

All laboratories and exercises are written in **R Markdown**, allowing students to combine code, results, figures, and economic interpretation in a single reproducible document.

---

## 📄 License

MIT License — Free for educational use.
