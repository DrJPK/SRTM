SRTM: Self‑Referenced Trajectory Modelling
================

# SRTM: Self‑Referenced Trajectory Modelling

*A teacher‑proof workflow for analysing student change over time*

This package implements **Self‑Referenced Trajectory Modelling (SRTM)**
—  
a practical, classroom‑ready alternative to randomised control trials,  
designed for real educational settings where true randomisation,  
“standard treatments,” or controlled environments are rarely feasible.

SRTM helps teachers and researchers:

- model each student *as their own control*  
- estimate expected (counterfactual) outcomes  
- detect meaningful deviations from expectation  
- compare groups of similar students  
- interpret change transparently and locally

------------------------------------------------------------------------

# 1. Installation

``` r
# install devtools if not already installed
install.packages("devtools")

# install SRTM from GitHub
devtools::install_github("DrJPK/SRTM")
```

Load the package:

``` r
library(SRTM)
```

------------------------------------------------------------------------

# 2. Getting data into SRTM

You have two options:

## Option A: Use the included synthetic demo dataset

``` r
data("SRTM_synth_data")
x <- SRTM_synth_data
```

## Option B: Import your own Excel file

If you have collected your own y0, y1, y2 data:

``` r
x <- importSRTMExcel("mydata.xlsx")
```

This expects the standard SRTM format  
(columns for `y0`, `y1`, `y2`, and any grouping IDs).

------------------------------------------------------------------------

# 3. Running the analysis

``` r
res <- SRTMAnalyse(x)
```

During analysis you may be asked:

- to specify **time intervals** (e.g., weeks between y0–y1 and y1–y2),  
- or to provide dates (Historical / Pre / Post),  
- or to confirm grouping decisions.

Each step includes on‑screen guidance.

------------------------------------------------------------------------

# 4. Understanding what SRTMAnalyse does

SRTMAnalyse:

1.  Computes each student’s **historical-to-baseline slope** (`m01`)
2.  Clusters students into trajectory groups  
    (similar patterns of pre‑intervention change)
3.  Splits each trajectory group into **baseline groups**  
    (similar starting positions)
4.  Builds group‑specific linear models
5.  Predicts each student’s **expected y2 score** (`exp_y2`)
6.  Calculates observed − expected differences for each group
7.  Runs statistical tests using `compareOutcomes()`
8.  Returns all results in a structured object

Output object:

``` r
str(res)
```

------------------------------------------------------------------------

# 5. Visualising the outcomes

``` r
plot(res)
```

This produces an easy‑to‑read visual summary showing:

- baseline groups  
- trajectory groups  
- expected vs observed outcomes  
- statistical significance indicators  
- group‑level captions

------------------------------------------------------------------------

# 6. Summaries and comparisons

``` r
summary(res)
```

This lists:

- group means  
- expected vs observed differences  
- t‑tests  
- pretty‑printed p-values  
- counts of students in each cluster

This is the most “teacher‑friendly” summary table.

------------------------------------------------------------------------

# 7. Theoretical foundations (simplified)

SRTM avoids the unrealistic assumptions of RCTs in schools:

- random student assignment is impractical  
- “standard treatments” do not exist in classrooms  
- real learning environments are dynamic and complex

Instead, SRTM uses **principles of behavioural change**:

## The 0th Principle — Behavioural Complexity

Student behaviour emerges from a dynamic, recursive network of  
attitudes, values, beliefs, and contextual influences.  
We cannot measure these individually, but we *can* measure outcomes.

## The 1st Principle — Behavioural Inertia

In the absence of a new influence, a student’s behaviour tends to  
continue developing at an approximately stable rate (short time scales).

## The 2nd Principle — Behavioural Momentum

When a meaningful influence occurs (e.g., a new teaching approach),  
behaviour changes at a rate proportional to the *perceived* strength  
of the influence and inversely proportional to behavioural inertia.

## The 3rd Principle — Behavioural Impulse

Influences accumulate over time; duration × perceived strength  
creates observable change.

SRTM relies mainly on Principles 1 and 2:

- use y0→y1 change to estimate a student’s personal “inertia”
- predict where they would likely have been at y2 without the influence
- compare that to where they actually ended up

This provides a **transparent, student‑centred evidence base** for  
professional decision‑making.

------------------------------------------------------------------------

# 8. Example complete workflow

``` r
library(SRTM)

# 1. Load demo data
data("SRTM_synth_data")
x <- SRTM_synth_data

# 2. Run the analysis
res <- SRTMAnalyse(x)

# 3. Visualise
plot(res)

# 4. Inspect detailed summaries
summary(res)

# 5. Extract core tables
comparisons   <- res$Comparisons
group_summary <- res$GroupSummary
clean_data    <- res$data
```

------------------------------------------------------------------------

# 9. Citing SRTM

If you use SRTM in a publication, please cite:

*Kennedy, J.P. (forthcoming).*  
*Self‑Referenced Trajectory Modelling (SRTM): A pragmatic  
approach to evaluating educational interventions.*

------------------------------------------------------------------------

# 10. Feedback & Contributions

Please post issues, suggestions, or feature requests at:  
<https://github.com/DrJPK/SRTM>

------------------------------------------------------------------------

*This README is auto‑generated as an .Rmd file for the SRTM package.*
