# Forecasting Bakery Product Demand for Co-op

This project focuses on building accurate demand forecasting models for bakery products sold at Co-op stores. Using a real-world dataset of daily sales across multiple product-store combinations, I explored a range of statistical and machine learning models implemented in **both R and Python**, aiming to support better inventory planning and reduce waste.

## Data Preprocessing

### In R:
- Removed negative sales and end-of-day-only features.
- One-hot encoded categorical variables.
- Filtered out inactive product-store pairs in the final week.
- Normalised continuous variables (e.g., temperature, transaction date).
- Exported cleaned data as CSV for modelling.

### In Python:
- Dummy-encoded categorical variables.
- Added engineered date features (month, year, sine-cosine transformation for month).
- Normalised numerical values.
- Grouped data by product-store for sequence-based models like LSTM.

## Models & Methodology

### Generalised Linear Model (GLM)
- Negative Binomial family (to account for overdispersion).
- Included interactions between weather, season, and date features.
- **Performance:**
  - RMSE (Training): 2.41 (59% error)
  - RMSE (Test): 2.46 (60% error)
  - RMSE (Validation): **2.28 (56% error)**
  - Pseudo R²: **0.08**

### Random Forest
- Tuned `max_depth` and `mtry` via grid search.
- Final model: 2000 trees, `max_depth = 12`, `mtry = 9`.
- **Performance:**
  - RMSE (Training): 1.98 (49% error)
  - RMSE (Test): 2.41 (59% error)
  - RMSE (Validation): **2.18 (53% error)**
  - Pseudo R²: **0.61**

### Neural Networks (Ensemble MLPs)
- Built and trained using TensorFlow in Google Colab.
- Ensemble of 21 models with varying layer sizes.
- Combined frozen weights into a new NN and retrained.
- **Performance:**
  - RMSE (Training): 2.57 (63% error)
  - RMSE (Test): 2.60 (64% error)
  - RMSE (Validation): **2.05 (50% error)**
  - Pseudo R²: **0.57**

### LSTM
- Architecture: 3 hidden LSTM layers (128 units), ReLU activations.
- Final layer outputs daily sales prediction.
- **Performance:**
  - RMSE (Training): 2.78 (68% error)
  - RMSE (Validation): **2.63 (65% error)**

## Conclusions

- **Random Forest and Neural Networks** outperformed GLM models.
- **LSTM** showed potential but requires refinement and more data.
- Model performance varies by product-store combination.
- Strong emphasis on data preprocessing and feature engineering.
