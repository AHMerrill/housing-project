# Austin Housing Price Prediction

A comprehensive machine learning project for predicting housing prices in Austin, Texas using advanced feature engineering and multiple predictive models.

## Project Overview

This project was developed as part of an MSBA (Master of Science in Business Analytics) program. It applies machine learning techniques to predict housing prices in Austin using a dataset containing property characteristics, neighborhood information, and historical price data.

### Objective
Build and evaluate multiple machine learning models to accurately predict housing prices, leveraging feature engineering techniques including neighborhood encoding, NLP analysis, and clustering.

## Dataset

The project uses Austin housing data with two primary datasets:

- **austinhouses.csv** - Training dataset with property features and price information
- **austinhouses_holdout.csv** - Holdout/test dataset used for final predictions

### Data Files
- `data/austinhouses.csv` - Original training data
- `data/austinhouses_holdout.csv` - Holdout set for final predictions
- `data/housing_data_with_neighborhoods.csv` - Training data with neighborhood features
- `data/housing_data_with_neighborhoods_encoded.csv` - Encoded neighborhood features
- `data/housing_data_with_clustered_neigh.csv` - Data with neighborhood clustering
- `data/housing_data_with_neigh.csv` - Alternative neighborhood feature engineering
- `data/housing_data_binary_NLP.csv` - NLP features (binary)
- `data/housing_data_numeric_NLP.csv` - NLP features (numeric)

## Repository Structure

```
housing-project/
├── README.md                                    # This file
├── requirements.txt                             # Python dependencies
│
├── data/                                        # All CSV datasets
│   ├── austinhouses.csv
│   ├── austinhouses_holdout.csv
│   ├── housing_data_with_neighborhoods.csv
│   ├── housing_data_with_neighborhoods_encoded.csv
│   ├── housing_data_with_clustered_neigh.csv
│   ├── housing_data_with_neigh.csv
│   ├── housing_data_binary_NLP.csv
│   └── housing_data_numeric_NLP.csv
│
├── notebooks/                                   # Jupyter notebooks organized by purpose
│   ├── exploratory/                             # EDA and analysis notebooks
│   │   ├── Austin Neighborhoods.ipynb           # Neighborhood-level analysis
│   │   ├── Map_Analysis.ipynb                   # Geographic visualization
│   │   ├── Neighborhood_1Hot.ipynb              # One-hot encoding exploration
│   │   └── encoding_neighborhoods.ipynb         # Neighborhood encoding techniques
│   │
│   ├── modeling/                                # Model development and evaluation
│   │   ├── Linear .ipynb                        # Baseline linear regression
│   │   ├── Linear_Neigh.ipynb                   # Linear regression with neighborhoods
│   │   ├── RF.ipynb                             # Random forest models
│   │   ├── random_forest.ipynb                  # Additional forest approaches
│   │   ├── XGB_Neigh.ipynb                      # XGBoost with neighborhood features
│   │   ├── Final_NB.ipynb                       # Final comprehensive model
│   │   └── Prediction_Contest_Submission.ipynb  # Contest submission code
│   │
│   └── final_submission/                        # Official contest materials
│       ├── PredictionContest_Submission.ipynb   # Final submission notebook
│       ├── Final_predictions.csv                # Final predictions on holdout set
│       └── STA_380_Prediction_Contest_Final.pdf # Contest submission report
│
├── scripts/                                     # Utility scripts
│   ├── austinhouses_nlp.py                      # NLP feature extraction (Python)
│   └── austinhouses_nlp.R                       # NLP feature extraction (R)
│
└── reports/                                     # Project documentation
    └── msba_project.pdf                         # Comprehensive project report
```

## Methodology

### 1. Feature Engineering
- **Neighborhood Analysis**: Extracted and encoded neighborhood-level features from address and property data
- **NLP Processing**: Applied natural language processing to property descriptions
  - Binary features from text data
  - Numeric representations of textual features
  - Scripts available in both Python and R
- **Clustering**: Performed neighborhood clustering for alternative groupings and segmentation
- **One-hot Encoding**: Created categorical variables for categorical features and neighborhoods

### 2. Models Developed
- **Linear Regression**: Baseline models with various feature sets (simple and neighborhood-enhanced)
- **Random Forest**: Multiple forest-based approaches for capturing non-linear relationships
- **XGBoost**: Gradient boosting with engineered neighborhood features for enhanced performance
- **Ensemble Approach**: Final model combining insights from multiple algorithms

### 3. Data Processing Pipeline
1. Data exploration and visualization (Austin Neighborhoods, Map Analysis notebooks)
2. Feature encoding and transformation (encoding_neighborhoods, Neighborhood_1Hot notebooks)
3. Model training and validation (Linear, RF, XGB notebooks)
4. Prediction and results export (Final_NB, contest submission notebooks)

## Key Notebooks

### Exploratory Analysis
- **Austin Neighborhoods.ipynb** - Neighborhood-level data analysis and insights
- **Map_Analysis.ipynb** - Geographic visualization and spatial analysis
- **Neighborhood_1Hot.ipynb** - One-hot encoding techniques for categorical features
- **encoding_neighborhoods.ipynb** - Advanced neighborhood encoding methods

### Model Development
- **Linear .ipynb** - Baseline linear regression models for price prediction
- **Linear_Neigh.ipynb** - Linear regression enhanced with neighborhood features
- **RF.ipynb** - Random forest implementation and tuning
- **random_forest.ipynb** - Additional random forest approaches and variants
- **XGB_Neigh.ipynb** - XGBoost gradient boosting with engineered features
- **Final_NB.ipynb** - Final optimized model combining best approaches and techniques

### Prediction & Contest Submission
- **modeling/Prediction_Contest_Submission.ipynb** - Initial contest submission notebook
- **final_submission/PredictionContest_Submission.ipynb** - Final optimized submission
- **final_submission/Final_predictions.csv** - Final predictions on holdout dataset
- **final_submission/STA_380_Prediction_Contest_Final.pdf** - Official submission report

## Results

The final model achieves competitive predictions on the holdout set. Key findings include:

- **Neighborhood Features**: Neighborhood encoding significantly improves model performance
- **Model Comparison**: XGBoost with engineered features outperforms baseline linear models
- **Feature Importance**: NLP features provide marginal improvements in specific contexts
- **Ensemble Benefits**: Combining multiple model approaches improves robustness and accuracy

Detailed results and analysis are available in:
- `reports/msba_project.pdf` - Comprehensive analysis and results
- `notebooks/final_submission/STA_380_Prediction_Contest_Final.pdf` - Contest submission report

## Requirements

```
pandas
numpy
scikit-learn
xgboost
matplotlib
seaborn
jupyter
```

### Installation
```bash
pip install -r requirements.txt
```

## Usage

### Running the Analysis Pipeline

1. **Exploratory Analysis** - Start with exploratory notebooks to understand the data:
   ```
   notebooks/exploratory/
   ```

2. **Model Development** - Train and evaluate models:
   ```
   notebooks/modeling/
   ```

3. **Generate Predictions** - Create final predictions:
   ```
   notebooks/final_submission/
   ```

### Key Points
- All notebooks use relative paths (`../data/`) for portability
- Data automatically loads from the `data/` folder
- Run notebooks in order of model development pipeline
- Final predictions exported to `notebooks/final_submission/Final_predictions.csv`

## Project Files

### Reports
- **msba_project.pdf** - Comprehensive project report with full methodology and results
- **final_submission/STA_380_Prediction_Contest_Final.pdf** - Prediction contest submission report

### Feature Engineering Scripts
- **scripts/austinhouses_nlp.py** - Python implementation of NLP feature extraction
- **scripts/austinhouses_nlp.R** - R implementation of NLP feature extraction

## Key Insights

- The Austin housing market shows strong geographic clustering with neighborhood effects
- Property descriptions (NLP features) provide supplementary predictive value
- Gradient boosting (XGBoost) achieves superior performance compared to linear models
- Neighborhood encoding is critical for accurate price prediction
- Geographic proximity and neighborhood characteristics are strong predictors of price

## Author

Developed by an MSBA student as a comprehensive machine learning capstone project.

## Project Type

- **Course**: STA 380 (Predictive Analytics/Machine Learning)
- **Program**: MSBA (Master of Science in Business Analytics)
- **Task**: Prediction contest with evaluation on holdout set

---

*This project demonstrates end-to-end machine learning pipeline development, from exploratory data analysis and feature engineering through model selection, tuning, and final prediction deployment.*
