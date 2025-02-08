# Forecasting and Backtesting Risk Models for FX Portfolios
## 📌 Objective

This repository evaluates the effectiveness of volatility forecasting models (Moving Average, EWMA, GARCH) and Value-at-Risk (VaR) estimation methods (parametric and historical simulation) in capturing FX risk for two distinct portfolios:

1. **Dollar Portfolio:** An equally weighted synthetic portfolio of nine developed market currencies.
2. **Emerging Market Portfolio:** A portfolio incorporating the Argentine Peso (ARS) to analyze risk dynamics in volatile emerging markets.

The project aims to empirically assess how well traditional models adapt to stable (developed) and highly volatile (emerging) currency environments, providing insights for FX risk management.

## 🛠️ How to Use

**Step 1. Clone the Repository**

```bash
git clone https://github.com/shreyasxi/Asset-Pricing-in-the-Bond-Market.git
cd your-repo-name
```

**Step 2. Prepare the Data**

Data_file.xlsx contains the relevant data to run this analysis, just make sure to change the file path in your script. 

**Step 3: Run the Analysis in your system**

Run the script to generate forecasts, VaR estimates, and backtesting results using RStudio or an R-supported IDE like Visual Studio Code.

**Step 4: Customize or Extend**

This framework can be adapted for further research or applications:  

- **Stress Testing**: Introduce extreme historical or hypothetical scenarios (e.g., currency crises, geopolitical shocks) to evaluate model robustness under tail events.  
- **Alternative Distributions**: Replace parametric VaR assumptions (e.g., Normal) with heavy-tailed distributions (Student’s t, Generalized Pareto) to better capture emerging market risk.  
- **Regime-Switching Models**: Account for structural breaks or volatility regimes in emerging markets using Markov-switching GARCH.  

*Feel free to modify the code or dataset to adapt the analysis for your own independent research or projects. You do not need to take my permission at all!*  
