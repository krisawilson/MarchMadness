# March Madness Simulator & Predictor

A reproducible R-based pipeline to simulate and predict outcomes for both Men’s and Women’s March Madness tournaments. This repository ingests historical data, cleans and preprocesses it, engineers predictive features, fits and compares statistical and machine learning models, and then runs Monte Carlo–style simulations to forecast the upcoming year’s bracket.

---

## Features

- **Men’s & Women’s Tournaments**: Parallel workflows for both divisions.  
- **End-to-End Pipeline**: From raw data scraping through to simulation.  
- **Reproducible**: All dependencies captured in `requirements.R`.  
- **Modular Structure**: Clear folder separation by task and division.  
- **Configurable**: Easily swap in new models or add custom feature engineering.  

---

## Repository Structure

```text
.
├── data
│   ├── men
│   │   ├── brackets.csv
│   │   ├── clean-data.csv
│   │   ├── full-data.csv
│   │   └── team-stats.csv
│   └── women
│       ├── brackets.csv
│       ├── clean-data.csv
│       ├── full-data.csv
│       └── team-stats.csv
│
├── preprocessing
│   ├── functions
│   │   └── data-cleaning-functions.R
│   └── data-cleaning.R
│
├── modeling
│   ├── men
│   │   ├── feature-engineering.R
│   │   ├── modeling.R
│   │   └── models.RData
│   └── women
│       ├── feature-engineering.R
│       ├── modeling.R
│       └── models.RData
│
├── simulation
│   ├── men
│   │   ├── functions
│   │   │   └── simulation-functions.R
│   │   ├── input-data-prep.R
│   │   ├── input-data.csv
│   │   └── simulation.R
│   └── women
│       ├── functions
│       │   └── simulation-functions.R
│       ├── input-data-prep.R
│       ├── input-data.csv
│       └── simulation.R
│
├── requirements.R
└── README.md
````

---

## Installation

1. **Clone the repo**

```bash
git clone https://github.com/yourusername/march-madness-simulator.git
cd march-madness-simulator
```

2. **Install dependencies**

```r
source("requirements.R")
```

---

## Data Description

All raw and processed data lives under `data/<men|women>/`.

* **brackets.csv**: Records of every game in historical brackets, containing:

  * `team_1`, `team_2`
  * `seed_1`, `seed_2`
  * `score_1`, `score_2`
  * `winner` (which team won)
  * `round` (e.g., “Round of 64”, “Final Four”)
  * `year`

* **full-data.csv**: Raw matchup statistics (offensive/defensive metrics, efficiency, etc.) for each team in each game.

* **clean-data.csv**: Differenced statistics between two teams (team1 − team2) for model inputs.

* **team-stats.csv**: Season-long team-level metrics (conference, record, rate statistics, advanced statistics).

---

## Preprocessing

* **Functions**:
  `preprocessing/functions/data-cleaning-functions.R` houses web-scraping and cleaning utilities.
* **Workflow**:

  ```bash
  Rscript preprocessing/data-cleaning.R
  ```

  This refreshes the raw CSVs under `data/`.

---

## Modeling

Under `modeling/<men|women>/`:

1. **Feature Engineering**

   ```bash
   Rscript modeling/men/feature-engineering.R
   ```

   Constructs and selects predictive features from `clean-data.csv`.

2. **Model Fitting & Comparison**

   ```bash
   Rscript modeling/men/modeling.R
   ```

   Fits multiple models and compares via cross-validation.

3. **Saved Models**

   * `models.RData` contains the finalized model objects ready for simulation.

---

## Simulation

Under `simulation/<men|women>/`:

1. **Function Library**
   `simulation/functions/simulation-functions.R` defines Monte Carlo bracket simulation routines.

2. **Data Preparation**

   ```bash
   Rscript simulation/men/input-data-prep.R
   ```

   Produces `input-data.csv` using the same feature pipeline as modeling.

3. **Run Simulation**

   ```bash
   Rscript simulation/men/simulation.R
   ```

   Runs N simulations, aggregates bracket outcomes, and outputs round-by-round and champion probabilities.

---

## Example Usage

```bash
# 1. Refresh raw data
Rscript preprocessing/data-cleaning.R

# 2. Engineer features & fit models
Rscript modeling/men/feature-engineering.R
Rscript modeling/men/modeling.R

# 3. Prepare inputs & simulate
Rscript simulation/men/input-data-prep.R
Rscript simulation/men/simulation.R
```

Repeat for the women’s workflow by substituting `men` → `women`.

---

## Contributing

1. Fork the repo.
2. Create a feature branch

   ```bash
   git checkout -b feature/awesome
   ```
3. Commit your changes

   ```bash
   git commit -m "Add awesome feature"
   ```
4. Push and open a PR

   ```bash
   git push origin feature/awesome
   ```

Please follow existing code style and document any new functions.


*Happy predicting—and may your bracket survive beyond the Sweet Sixteen!* 🏀🎉

```
```
