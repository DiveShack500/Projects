import yfinance as yf
import pandas as pd
import matplotlib.pyplot as plt
from prophet import Prophet
from fpdf import FPDF
import warnings

warnings.filterwarnings("ignore")

# 1. Download BTC data from 2022 onward
btc_raw = yf.download("BTC-USD", start="2022-01-01")
btc = pd.DataFrame({
    'Date': btc_raw.index,
    'Close': btc_raw['Close'].values.flatten()  # Ensure 1D array
})
btc['Close'] = pd.to_numeric(btc['Close'], errors='coerce')
btc.dropna(inplace=True)

# 2. Prepare data for Prophet
btc_prophet = pd.DataFrame()
btc_prophet['ds'] = btc['Date']
btc_prophet['y'] = btc['Close']

# 3. Initialize and fit Prophet model
model = Prophet(daily_seasonality=True)
model.fit(btc_prophet)

# 4. Create future dataframe for 60 days
future = model.make_future_dataframe(periods=60)
forecast = model.predict(future)

# 5. Extract forecast
forecast_filtered = forecast[['ds', 'yhat', 'yhat_lower', 'yhat_upper']].tail(60)

# 6. Plot forecast
fig = model.plot(forecast)
plt.title("Bitcoin Price Forecast (Next 60 Days) - Prophet")
plt.xlabel("Date")
plt.ylabel("Price (USD)")
plt.grid(True)
plt.tight_layout()
plt.xlim(pd.to_datetime("2024-01-01"), forecast['ds'].max())
plt.savefig("btc_prophet_forecast.png")
plt.close()

# 7. Generate PDF Report
pdf = FPDF()
pdf.add_page()
pdf.set_font("Arial", size=14)
pdf.cell(200, 10, txt="Bitcoin 60-Day Trend Forecast (Prophet)", ln=True, align='C')
pdf.image("btc_prophet_forecast.png", x=10, y=30, w=190)

pdf.set_xy(10, 140)
pdf.set_font("Arial", size=12)
pdf.cell(200, 10, txt="Next 10-Day Forecast:", ln=True)

for i in range(10):
    row = forecast_filtered.iloc[i]
    line = f"{row['ds'].date()}: ${row['yhat']:,.2f} (Range: ${row['yhat_lower']:,.0f} - ${row['yhat_upper']:,.0f})"
    pdf.cell(200, 10, txt=line, ln=True)

pdf.output("bitcoin_trend_report.pdf")
print("✅ Forecast complete. PDF report saved as 'bitcoin_trend_report.pdf'")
