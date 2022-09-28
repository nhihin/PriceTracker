# PriceTracker
Automatic scraping and tracking of prices of products of interest using R. 

## Setup

- The `productList.csv` details the products of interest. It must have three columns:
  - **Store**: Only supports `ChemistWarehouse` or `Sephora` at the moment. 
  - **Product**: Alphanumeric name of the product.
  - **URL**: Valid URL to the product on the website. 
  
## Running the script manually

In Terminal, run the following:

```bash
chmod +x main.R
./main.R
```

## Running the script automatically, once per day

This can be done with a cron job, see here for details for setting one up on Mac: https://betterprogramming.pub/https-medium-com-ratik96-scheduling-jobs-with-crontab-on-macos-add5a8b26c30

1. Run `crontab -e` in Terminal; this opens up a vim editing window. Press `i` to get into editing mode. 
2. Add the following cron command: `0 0 * * * cd ~/Documents/Projects/PriceTracker && ./main.R` (change the path to wherever the directory was cloned). 
3. `Escape`, type `:x`, and press `Enter` to save changes. 

## Output Files

- **productPrices.csv**: CSV file that includes columns for today's date, current price, RRP, whether the product is on sale, the discount, and discount as a proportion of RRP. 
- **priceTrackerPlot.pdf**: Plot of the prices of each product over time. 
