from shiny import App, ui, reactive, render
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.impute import KNNImputer
from sklearn.cluster import KMeans
import statsmodels.api as sm
import tempfile
import matplotlib.pyplot as plt

# Load data paths
cleaned_data_path = r"F:\python2\Final-Project\Shiny app\calculateStatistics_data.csv"
labels_data_path = r"F:\python2\Final-Project\Shiny app\labels_data.csv"

# Load cleaned data
def load_cleaned_data():
    data = pd.read_csv(cleaned_data_path)
    data.columns = data.columns.str.upper()  # Ensure all columns are uppercase to match labels_data
    data = data.apply(pd.to_numeric, errors="coerce")  # Ensure all columns are numeric
    return data

# Load labels data
def load_labels():
    labels = pd.read_csv(labels_data_path)
    return labels

# Load data
cleaned_data = load_cleaned_data()
labels_data = load_labels()

# Define UI
app_ui = ui.page_fluid(
    ui.tags.style("""
        .output-separator {
            margin-top: 50px;
            margin-bottom: 300px;
        }
    """),
    ui.h2("Dynamic Variable Selection, PCA Clustering, and Regression Analysis"),
    ui.input_checkbox_group(
        "variable_types",
        "Select Variable Types:",
        {
            "Likert Scale Variables": "Likert Scale Variables",
            "Binary Variables": "Binary Variables",
            "Continuous Variables": "Continuous Variables",
            "Multichoice Variables": "Multichoice Variables",
            "Administration Variable": "Administration Variable",
        },
        selected=["Likert Scale Variables"],
    ),
    ui.input_slider(
        "missing_threshold",
        "Select Missing Value Threshold:",
        min=0,
        max=int(labels_data["missing_count"].max()),
        value=5,
        step=1,
    ),
    ui.output_text_verbatim("selected_variables"),
    ui.input_action_button("generate_plot", "Generate PCA Plot"),
    ui.div(ui.output_image("pca_plot"), class_="output-separator"),
    ui.div(ui.output_text_verbatim("cluster_summary"), class_="output-separator"),
    ui.input_action_button("generate_regression", "Generate Regression Results"),
    ui.div(ui.output_table("regression_results"), class_="output-separator"),
    ui.div(ui.output_image("regression_lines_filtered"), class_="output-separator"),
)

# Server logic
def server(input, output, session):
    # Reactive function to filter variables
    @reactive.Calc
    def filtered_variables():
        selected_types = input.variable_types()
        missing_threshold = input.missing_threshold()

        # Map variable types to columns in the labels_data
        type_column_map = {
            "Likert Scale Variables": "Likert Scale Variables",
            "Binary Variables": "Binary Variables",
            "Continuous Variables": "Continuous Variables",
            "Multichoice Variables": "Multichoice variables",
            "Administration Variable": "Administration variable",
        }

        selected_columns = [
            type_column_map[typ] for typ in selected_types if typ in type_column_map
        ]

        if not selected_columns:
            return []

        # Filter labels_data by selected types and missing value threshold
        filtered_data = labels_data.loc[
            (labels_data[selected_columns].sum(axis=1) > 0) &
            (labels_data["missing_count"] <= missing_threshold)
        ]

        final_variables = [var.upper() for var in filtered_data["variable"].tolist()]
        if len(final_variables) == 0:
            return None
        return final_variables

    @output
    @render.text
    def selected_variables():
        variables = filtered_variables()
        if variables is None:
            return "No variables selected due to filtering."
        return f"Selected Variables: {', '.join(variables)}"

    @reactive.event(input.generate_plot)
    def generate_pca_and_clusters():
        selected_vars = filtered_variables()
        if selected_vars is None or len(selected_vars) < 2:
            return None

        try:
            data_for_pca = cleaned_data[selected_vars]
            imputer = KNNImputer(n_neighbors=5)
            data_imputed = imputer.fit_transform(data_for_pca)

            scaler = StandardScaler()
            data_scaled = scaler.fit_transform(data_imputed)

            pca = PCA(n_components=2)
            pca_result = pca.fit_transform(data_scaled)

            kmeans = KMeans(n_clusters=4, random_state=42)
            clusters = kmeans.fit_predict(pca_result)

            return pca_result, clusters, selected_vars
        except Exception as e:
            print(f"Error in PCA or Clustering: {e}")
            return None

    @output
    @render.image
    def pca_plot():
        pca_data = generate_pca_and_clusters()
        if not pca_data:
            return None

        pca_result, clusters, variable_names = pca_data
        try:
            with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as tmpfile:
                plt.figure(figsize=(10, 7))
                for cluster in np.unique(clusters):
                    cluster_indices = np.where(clusters == cluster)[0]
                    plt.scatter(
                        pca_result[cluster_indices, 0],
                        pca_result[cluster_indices, 1],
                        label=f"Cluster {cluster + 1}",
                        alpha=0.7,
                    )
                    
                    
                    for idx in cluster_indices:
                        plt.annotate(
                            variable_names[idx],  # 添加变量名称
                            (pca_result[idx, 0], pca_result[idx, 1]),  # 在对应的 PCA 点上标注
                            fontsize=8,  # 字体大小
                            alpha=0.7,  # 透明度
                        )

                plt.title("PCA Result and Clustering")
                plt.xlabel("Principal Component 1")
                plt.ylabel("Principal Component 2")
                plt.legend()
                plt.savefig(tmpfile.name)
                plt.close()
                return {"src": tmpfile.name, "mime_type": "image/png"}
        except Exception as e:
            print(f"Error during plotting: {e}")
            return None

    @output
    @render.text
    def cluster_summary():
        pca_data = generate_pca_and_clusters()
        if not pca_data:
            return "No clusters to summarize."

        _, clusters, variable_names = pca_data
        summary = []
        for cluster in np.unique(clusters):
            cluster_vars = [variable_names[idx] for idx in np.where(clusters == cluster)[0]]
            summary.append(f"Cluster {cluster + 1}: {', '.join(cluster_vars)}")
        return "\n".join(summary)

    @reactive.event(input.generate_regression)
    def generate_filtered_regression():
        selected_vars = filtered_variables()
        if selected_vars is None or len(selected_vars) < 2:
            return None, None

        regression_results = []
        regression_lines = []

        try:
            for var in selected_vars:
                lag_var = f"{var}Lag"
                if lag_var not in cleaned_data.columns:
                    cleaned_data[lag_var] = cleaned_data[var].shift(1)

                if "MOBILITYLAG" not in cleaned_data.columns:
                    return "Required variable 'MOBILITYLAG' not found in data.", None

                regression_data = cleaned_data[[var, lag_var, "MOBILITYLAG"]].dropna()

                if regression_data.empty:
                    continue

                X = regression_data[["MOBILITYLAG", lag_var]]
                y = regression_data[var]
                X = sm.add_constant(X)
                model = sm.OLS(y, X).fit()

                # Append results for each regression

                regression_results.append({
                    "Variable": var,
                    "R-squared": model.rsquared,
                    "MOBILITYLAG_coef": model.params["MOBILITYLAG"],
                    "Lag_coef": model.params[lag_var],
                    "p-value (MOBILITYLAG)": model.pvalues["MOBILITYLAG"],
                    "t-value (MOBILITYLAG)": model.tvalues["MOBILITYLAG"],
                })

                x_vals = np.linspace(regression_data["MOBILITYLAG"].min(), regression_data["MOBILITYLAG"].max(), 100)
                y_vals = model.params["const"] + model.params["MOBILITYLAG"] * x_vals
                regression_lines.append((x_vals, y_vals, var))

            # 篩選圖形輸出需要的前 10 個變量
            top_results = sorted(regression_results, key=lambda x: abs(x["MOBILITYLAG_coef"]), reverse=True)[:10]
            top_vars = [result["Variable"] for result in top_results]

            filtered_lines = [(x, y, var) for x, y, var in regression_lines if var in top_vars]
            return pd.DataFrame(regression_results), filtered_lines

        except Exception as e:
            return f"Regression error: {e}", None

    @output
    @render.table
    def regression_results():
        results, _ = generate_filtered_regression()
        if isinstance(results, str):
            return pd.DataFrame({"Error": [results]})
        return results  # 返回完整的回歸結果

    @output
    @render.image
    def regression_lines_filtered():
        _, regression_lines = generate_filtered_regression()
        if not regression_lines:
            return None

        try:
            with tempfile.NamedTemporaryFile(suffix=".png", delete=False) as tmpfile:
                plt.figure(figsize=(10, 7))
                for x_vals, y_vals, var in regression_lines:
                    plt.plot(x_vals, y_vals, label=var, linewidth=2)
                plt.title("Top Variables by Coefficient")
                plt.xlabel("MOBILITYLAG")
                plt.ylabel("Fitted Values")
                plt.legend()
                plt.savefig(tmpfile.name)
                plt.close()
                return {"src": tmpfile.name, "mime_type": "image/png"}
        except Exception as e:
            print(f"Error during plotting: {e}")
            return None

# Create and run the app
import nest_asyncio
nest_asyncio.apply()
app = App(app_ui, server)
app.run(host="127.0.0.1", port=8273)
