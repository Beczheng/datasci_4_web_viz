from flask import Flask, render_template, request
import pandas as pd
import matplotlib
matplotlib.use('Agg') # required for local development and g-shell
import matplotlib.pyplot as plt
import io
import base64

import warnings
warnings.simplefilter("ignore", UserWarning)

app = Flask(__name__)

# Load the dataset
url = "https://raw.githubusercontent.com/Beczheng/datasci_4_web_viz/main/datasets/cdc_places_tx.csv"
df = pd.read_csv(url)
df_arthritis = df[(df['MeasureId'] == 'ARTHRITIS') & (df['Data_Value_Type'] == 'Crude prevalence')]

@app.route('/', methods=['GET', 'POST'])
def index():
    counties = sorted(df_arthritis['LocationName'].unique())
    selected_county = request.form.get('county') or counties[0]
    
    img = create_plot(selected_county)
    
    return render_template("index.html", counties=counties, selected_county=selected_county, img=img)

def create_plot(county):
    overall_avg = df_arthritis['Data_Value'].mean()
    selected_county_avg = df_arthritis[df_arthritis['LocationName'] == county]['Data_Value'].mean()

    fig, ax = plt.subplots(figsize=(10, 6))
    ax.bar(['Selected County', 'Overall Average'], [selected_county_avg, overall_avg], color=['lightgreen', 'lavender'])
    ax.axhline(selected_county_avg, color='dodgerblue', linestyle='dashed', alpha=0.7)
    ax.set_ylabel('Data Value (Crude prevalence) - Percent')
    ax.set_ylim(0, 30)
    ax.set_title('Arthritis Crude Prevalence Comparison')
    
    # Convert plot to PNG image
    img = io.BytesIO()
    plt.savefig(img, format='png')
    img.seek(0)
    
    return base64.b64encode(img.getvalue()).decode()

if __name__ == '__main__':
    app.run(debug=True)