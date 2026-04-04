# Exploratory Data Analysis & Regression Modelling using Real Estate Valuation Data 

### Description:
The aim of this analysis is to explore the relationship between house prices and their available 
features using a real estate valuation dataset. The goal is to build and evaluate a regression 
model that can predict house prices based on these features. 

### Table of Contents
- Requirements
- Dataset Description
- Explatory Data Analysis
- Modelling Techniques
- Results Summary

### Requirements 
The following Python libraries are required to run the script.

- pandas: For data loading, cleaning, and manipulation.
- numpy: For numerical computations and array operations.
- matplotlib: For creating visulisations and plots.
- seaborn: For advdanced statisitcal visulaisations.
- scikit-learn: For building and evaluating various regression models.
- openpyxl: For reading Excel ```.xlsx``` files through ```pd.read_excel().```

### Dataset Description
- The data consists of 414 houses that have been purchased between the year 2012 and 2013. 
- The variables we will be working with are :
- X1=the house age (unit: year)
- X2=the distance to the nearest MRT station (unit: meter)
- X3=the number of convenience stores in the living circle on foot (integer)
- X4=the geographic coordinate, latitude. (unit: degree)
- X5=the geographic coordinate, longitude. (unit: degree)
- Y=house price of unit area (10000 New Taiwan Dollar/Ping, where Ping is a local unit, 1 Ping = 3.3 meter squared)

### Explatory Data Analysis (EDA)
The exploratory analysis focuses on understanding the distribution of the data and how the features relate to the house price. Key steps in this process include:
- Summary statistics to understand central tendencies and variability
- Correlation analysis to identify relationships between features and house price
- Visualisations, such as:
  - Histograms of numerical features
  - Scatter plots (e.g., house age vs. price, distance to MRT vs. price)
  - Heatmaps to visualise feature correlations
- Outlier detection to identify unusual values that may influence the model
- Missing value checks, although this dataset is generally clean

These steps help reveal which features are most influential in predicting house prices.

### Modelling techniques
Several regression models were implemented to compare performance and identify the most suitable approach for this dataset. The models include:

- **Linear Regression**  
A baseline model to understand linear relationships between features and price.

- **Ridge & Lasso Regression**  
Regularised models to reduce overfitting and handle multicollinearity(when features are highly correlated with each other).

- **Decision Tree Regressor**  
A non‑linear model that captures complex interactions between features.

- **Random Forest Regressor**  
An ensemble method that improves predictive accuracy by combining multiple decision trees.

### Model Evaluation
Each model was evaluated using standard regression metrics. These metrics help assess how well the model predicts house prices and how large the errors are:

- Root Mean Squared Error (RMSE)

  - Penalises larger errors more heavily because each error is squared before averaging.
  - More sensitive to outliers and large prediction mistakes.
  - Lower RMSE = better model performance.

- R² Score (Coefficient of Determination)

  - Indicates how much of the variation in house prices is explained by the model.
  - Ranges from 0 to 1, where:
    - 1 = perfect prediction
    - 0 = model explains none of the variability
  - Higher R² = better fit.

These metrics together give a balanced view of model performance:
- RMSE highlights large errors
- R² shows how well the model captures overall patterns in the data

### Results Summary
The results show clear differences in model performance:
- Linear models provided a good baseline but struggled with non‑linear relationships.
- Tree‑based models, particularly Random Forest, achieved the best predictive accuracy.
- Distance to the nearest MRT station and number of convenience stores were among the most influential features.

| Model             | R^2   | 
|-------------------|-------|
| Linear Regression | 0.6509 | 
| Ridge Regression  | 0.6509 |  
| Lasso Regression  | 0.6499 |
| Decision Tree     | 0.7709 |  
| Random Forest     | 0.8086 |  

**Conclusion**
The linear models (Linear, Ridge, and Lasso) all achieved similar R² values around 0.65, indicating that the relationship between the features and house prices is not well captured by a purely linear approach. Ridge and Lasso did not provide meaningful improvements, suggesting that the dataset does not suffer from strong multicollinearity or coefficient instability.

In contrast, the Decision Tree model achieved a noticeably higher R² of 0.77, showing that non‑linear relationships and threshold effects play a significant role in predicting house prices.

The best performance came from the Random Forest, with an R² of 0.81. This improvement demonstrates the benefit of combining multiple decision trees, allowing the model to capture complex patterns while reducing overfitting. Overall, ensemble tree‑based methods provided the most accurate predictions for this dataset.

 
