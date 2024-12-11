import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import GridSearchCV, StratifiedShuffleSplit
from sklearn.metrics import roc_auc_score
import os
import csv

def train_and_estimate_sample_size(prefix, model_details, save_dir='default', cv=5, scoring='roc_auc', task='classification', seed = 42):
    # Set a consistent random seed for reproducibility
    np.random.seed(seed)
    
    # Determine model and parameter grid
    model = model_details['model']
    param_grid = model_details['param_grid']
    
    # Ensures that the directory exists
    save_path = f'inst/extdata/{save_dir}'
    os.makedirs(save_path, exist_ok=True)
    
    # Load the training dataset
    data = pd.read_csv(f'inst/extdata/{prefix}.csv')
    
    # Separate features and target
    X = data.drop(columns=['outcome', 'id'])  # Exclude the identifier column "id" and the target "outcome"
    y = data['outcome'].astype(float if task == 'regression' else int)

    # Number of predictors
    num_predictors = X.shape[1]

    # Proportions to sample
    proportions = np.linspace(0.1, 0.9, 9)  # This creates an array of proportions from 0.1 to 0.9
    results = []

    # Stratified sampling based on the proportions
    sss = StratifiedShuffleSplit(n_splits=1, test_size=None, random_state=seed)  # test_size will be set in the loop

    for prop in proportions:
        sss.test_size = prop  # Set the proportion for each iteration
        for train_index, test_index in sss.split(X, y):
            X_train, y_train = X.iloc[train_index], y.iloc[train_index]

            # Calculate EPV
            event_count = min(y_train.sum(), len(y_train) - y_train.sum())
            epv = event_count / num_predictors

            # Standardize features
            scaler = StandardScaler()
            X_train_scaled = scaler.fit_transform(X_train)
            
            # Train the model with cross-validation
            model = model_details['model']
            if hasattr(model, 'random_state'):
                model.random_state = seed  # Ensure the model's randomness is controlled
            grid_search = GridSearchCV(estimator=model, param_grid=model_details['param_grid'], cv=cv, scoring=scoring, n_jobs=-1)
            grid_search.fit(X_train_scaled, y_train)
            
            # Best model from grid search
            best_model = grid_search.best_estimator_

            # Predict probabilities
            if hasattr(best_model, "predict_proba"):
                y_scores = best_model.predict_proba(X_train_scaled)[:, 1]  # Probability of the positive class
            else:
                y_scores = best_model.decision_function(X_train_scaled)
            
            # Compute AUC-ROC
            auc_roc = roc_auc_score(y_train, y_scores)
            results.append((prop, epv, auc_roc))

    # Save results to CSV
    results_path = os.path.join(save_path, 'sample_size_estimation.csv')
    with open(results_path, 'w', newline='') as file:
        writer = csv.writer(file)
        writer.writerow(['proportion', 'epv', 'auc_roc'])
        writer.writerows(results)
    print(f"Results saved at {results_path}")

import pandas as pd
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import GridSearchCV
from sklearn.utils import parallel_backend
import os
from joblib import dump

def train_and_save_model(prefix, model_details, save_dir='default', cv=5, scoring='precision', task='classification', seed=42):
    # Set a random seed for reproducibility
    np.random.seed(seed)
    
    # Determine model and parameter grid
    model = model_details['model']
    param_grid = model_details['param_grid']

    # Ensures that the directory exists
    os.makedirs(f'inst/extdata/{save_dir}', exist_ok=True)
    
    # Load the training dataset
    data = pd.read_csv(f'inst/extdata/{prefix}.csv')

    # Separate features and target
    X = data.drop(columns=['outcome', 'id'])  # Exclude the identifier column "id" and the target "outcome"
    y = data['outcome'].astype(float if task == 'regression' else int)

    # Standardize features
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X)
    
    # Save the scaler for future use
    scaler_path = f'inst/extdata/{save_dir}/scaler.joblib'
    dump(scaler, scaler_path)
    print(f"Scaler saved at {scaler_path}")

    # If the model is sklearn's RandomForest or similar, set the random state
    if hasattr(model, 'random_state'):
        model.set_params(random_state=seed)

    # Train the model with cross-validation using a fixed random state for reproducibility
    with parallel_backend('loky', inner_max_num_threads=1):  # Control threading for reproducibility
        grid_search = GridSearchCV(estimator=model, param_grid=param_grid, cv=cv, scoring=scoring, n_jobs=-1)
        grid_search.fit(X_scaled, y)

    # Best model from grid search
    model = grid_search.best_estimator_

    # Save the trained model
    model_path = f'inst/extdata/{save_dir}/model.joblib'
    dump(model, model_path)
    print(f"Model saved at {model_path}")



import pandas as pd
from joblib import load
import os

def load_and_predict(prefix, prediction_type='label', save_dir='default'):
    # Load the scaler and model
    scaler = load(f'inst/extdata/{save_dir}/scaler.joblib')
    model = load(f'inst/extdata/{save_dir}/model.joblib')
    
    # Load the new dataset for prediction
    X_new = pd.read_csv(f'inst/extdata/{prefix}.csv')
    
    # Assume the new data also excludes 'id' and 'outcome' columns
    X_new = X_new.drop(columns=['id', 'outcome'], errors='ignore')
    
    # Standardize the new dataset using the same scaler
    X_new_scaled = scaler.transform(X_new)

    # Determine type of prediction and process accordingly
    if prediction_type == 'label':
        # Make predictions
        predictions = model.predict(X_new_scaled)
        # Convert predictions to a DataFrame
        predictions_df = pd.DataFrame(predictions, columns=['prediction'])
        # Define file suffix and DataFrame to save
        filename = 'predictions.csv'
        result_df = predictions_df
    elif prediction_type == 'probability':
        # Make probability predictions
        if hasattr(model, 'predict_proba'):
            probabilities = model.predict_proba(X_new_scaled)[:, 1]  # Assumes binary classification
            result_df = pd.DataFrame(probabilities, columns=['predicted_probability'])
            filename = 'prob.csv'
        else:
            raise ValueError("This model does not support probability predictions.")
    else:
        raise ValueError("Invalid prediction type specified. Use 'label' or 'probability'.")

    # Save the results to a CSV file
    results_path = f'inst/extdata/{save_dir}/{filename}'
    result_df.to_csv(results_path, index=False)
    print(f"Results saved at {results_path}")



import shap
import pandas as pd
from joblib import load
import os

def compute_shap_values(prefix, explainer_type, save_dir='default'):
    # Load the scaler and model
    scaler_path = f'inst/extdata/{save_dir}/scaler.joblib'
    model_path = f'inst/extdata/{save_dir}/model.joblib'
    scaler = load(scaler_path)
    model = load(model_path)
    
    # Load the new dataset for prediction
    X_new = pd.read_csv(f'inst/extdata/{prefix}.csv')
    X_new = X_new.drop(columns=['id', 'outcome'], errors='ignore')
    X_new_scaled = scaler.transform(X_new)

    # Background data for explainers
    background_data = shap.sample(X_new_scaled, 100)  # Adjust as needed

    # Initialize the SHAP explainer
    if explainer_type == shap.LinearExplainer:
        # For LinearExplainer, a masker is required
        masker = shap.maskers.Independent(data=background_data)
        explainer = explainer_type(model, masker)
    elif explainer_type == shap.TreeExplainer:
        explainer = explainer_type(model)
    elif explainer_type == shap.KernelExplainer:
        # KernelExplainer uses the model prediction function and background data
        explainer = explainer_type(model.predict, background_data)
    
    # Compute SHAP values
    shap_values = explainer.shap_values(X_new_scaled)

    # Handle multiple classes for classifiers
    if isinstance(shap_values, list):
        shap_values = shap_values[0]  # Assuming interest in the first class

    # Convert SHAP values to DataFrame and save
    shap_df = pd.DataFrame(shap_values, columns=X_new.columns)
    shap_csv_filename = os.path.join(f'inst/extdata/{save_dir}', f'shap_values.csv')
    shap_df.to_csv(shap_csv_filename, index=False)
    print(f"Results saved at {shap_csv_filename}")


