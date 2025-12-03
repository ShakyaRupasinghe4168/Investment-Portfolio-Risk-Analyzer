# Investment Portfolio Risk Analyzer

## Group Members:
- EG/2020/4096 Nethsarani D.W.D
- EG/2020/4168 Rupasinghe A.A.H.S
- EG/2020/4176 Samarakoon P.G.C
- EG/2020/4307 Yaparathna Y.M.S.K


---

## Project Scenario

This project analyzes the risk of a portfolio of assets. Users can provide asset price data either via CSV or manual entry. The system calculates key **risk metrics** such as:

- Returns  
- Volatility  
- Mean returns  
- Correlation matrix across assets  

All calculations are done using **pure functional programming** principles in Haskell, ensuring reliability, modularity, and testability.

Parallel computation is applied to compute per-asset statistics, demonstrating **functional parallelism**.

---


## Project Files
Main.hs – Entry point of the program.  
DataTypes.hs – Defines custom types like AssetReport and FinalReport.  
Processing.hs – Contains pure functions for calculating returns, volatility, correlations, and parallel processing.  
IOHandler.hs – Handles user interaction and printing the final report.  
Utils.hs – Helper functions like CSV parsing, data cleaning, mean, variance, etc.  
sample_portfolio.csv – Example CSV file with asset prices.  
portfolio-risk-analyzer.cabal – Cabal project file with dependencies.
## How to Run

### Prerequisites:

- GHC & Cabal installed  
- Haskell packages: `parallel`, `deepseq`  

### Steps:  
1. Open terminal in the project folder.  
2. Install necessary libraries locally: `cabal install --lib parallel --package-env .` and `cabal install --lib deepseq --package-env .`  
3. Build the project: `cabal build`  
4. Run the executable: `cabal run`  
5. When prompted, choose input method: 1 for CSV file, 2 for manual entry.  
6. If you choose CSV, enter the filename (e.g., sample_portfolio.csv).

## Sample Input (CSV)
AAPL,150,152,151,153,155  
GOOG,2700,2695,2710,2720,2730  
TSLA,700,710,720,715,725  

Note: CSV should have no header row. First column is asset name, rest are prices.

## Sample Output
=== Final Report ===  
Asset: AAPL  
  Mean Return   : 0.0061  
  Volatility    : 0.0049  

Asset: GOOG  
  Mean Return   : 0.0069  
  Volatility    : 0.0057  

Asset: TSLA  
  Mean Return   : 0.0123  
  Volatility    : 0.0081  

Correlation Matrix:  
1.0 0.98 0.92  
0.98 1.0 0.89  
0.92 0.89 1.0  

## Functional Programming Concepts Used
Pure Functions: All computations (returns, volatility, correlation) are deterministic.  
Immutability: Original price data never modified.  
Modularity: Functions are organized into Utils, Processing, IOHandler, and DataTypes.  
Parallelism: Per-asset statistics are computed in parallel using parList rdeepseq and Control.Parallel.Strategies.  
Testable Code: Each function can be tested independently.

## Notes
The system demonstrates safe parallel computation for asset analysis. CSV input allows easy testing with multiple assets. Designed for educational purposes to showcase Haskell FP concepts applied to real-world scenarios.
