#!/usr/bin/env Rscript

# Price Tracker 
# Supports the following websites:
# - Chemist Warehouse
# - Sephora

# Load Packages ---------------------------------------------------------------
library(magrittr)
library(stringr)
library(rvest)
library(dplyr)
library(lubridate)
library(readr)
library(RSelenium)
library(here)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

# Import Product List ---------------------------------------------------------
# Must have three columns: 
# - Store: Supported values: "Chemist Warehouse" or "Sephora"
# - Product: Product name. No special characters allowed. Alphanumeric only. 
# - URL: Valid URL to item on website. 
productList <- read_csv(here("productList.csv")) %>%
  dplyr::mutate(Store = gsub(x = Store, pattern = " ", replacement = "_"))

productLists <- split(x = productList, f = productList$Store)

# CHEMIST WAREHOUSE -----------------------------------------------------------
if("Chemist_Warehouse" %in% names(productLists)){
  
  # Read in the HTML pages from the URLs supplied, and extract out the current 
  # price as well as retail price. 
  productPrices <- productLists$Chemist_Warehouse$URL %>%
    lapply(function(x){
      read_html(x) %>%
        html_elements(".Price, .retailPrice") %>%
        html_elements("span") %>%
        html_text2
    })
  
  # If there is no retail price supplied, assume that the current price is 
  # the retail price. 
  for(i in 1:length(productPrices)){
    if(length(productPrices[[i]]) == 1){
      productPrices[[i]][2] <- productPrices[[i]][1]
    } else {
      productPrices[[i]][2] <- productPrices[[i]][2] %>%
        gsub(x = ., pattern = "Don't Pay RRP: ", replacement = "")
    }
  }
  names(productPrices) <- productLists$Chemist_Warehouse$Product
  
  # Transform the product prices into a data.frame, tidy up columns, and
  # calculate discount, as well as whether product is on sale. 
  # The Date column uses today's date. 
  productPrices_df <- t(as.data.frame(productPrices)) %>%
    as.data.frame %>%
    tibble::rownames_to_column("Product") %>%
    magrittr::set_colnames(c("Product", "Price", "RRP")) %>%
    dplyr::mutate(Product = gsub(x = Product, 
                                 pattern = "\\.", 
                                 replacement = " ")) %>%
    dplyr::mutate(Price = as.numeric(gsub(x = Price, 
                                          pattern = "\\$", 
                                          replacement = ""))) %>%
    dplyr::mutate(RRP = as.numeric(gsub(x = RRP, 
                                        pattern = "\\$",
                                        replacement = ""))) %>%
    dplyr::mutate(onSale = Price < RRP) %>%
    dplyr::mutate(Discount = RRP - Price) %>%
    dplyr::mutate(Discount_Prop = Discount/RRP) %>%
    dplyr::mutate(Date = today())
  
  productPrices_df <- productPrices_df %>%
    left_join(productList, by = "Product")
  
  if(!file.exists(here("productPrices.csv"))){
    productPrices_df %>% readr::write_csv(here("productPrices.csv"))
  } else {
    write.table(
      productPrices_df,
      here("productPrices.csv"),
      append = TRUE,
      sep = ",",
      col.names = FALSE,
      row.names = FALSE
    )
  }
  
} 

# SEPHORA ---------------------------------------------------------------------
if("Sephora" %in% names(productLists)){
  
  # Start up the Selenium server:
  rD <- rsDriver(browser="firefox", port=4545L, verbose=F)
  remDr <- rD[["client"]]
  
  sephoraUrls <- productLists$Sephora$URL
  names(sephoraUrls) <- productLists$Sephora$Product
  
  # Loop through the URLs to retrieve the HTML pages through Selenium
  sephoraHtmls <- list()
  for(i in 1:length(sephoraUrls)){
    remDr$navigate(sephoraUrls[[i]])
    Sys.sleep(7) 
    sephoraHtmls[[i]] <- remDr$getPageSource()[[1]]
  }
  rD[["server"]]$stop()
  names(sephoraHtmls) <- names(sephoraUrls)
  
  # Read in the HTML files retrieved by RSelenium into a format 
  # that rvest can use. 
  sephoraHtmls2 <- lapply(sephoraHtmls, read_html)
  
  checkIfItemOnSale <- function(x){
    res <- x %>%
      html_elements(".basic-information-section .product-price") %>%
      html_elements(".product-price-sale-new, .product-price-sale-old") %>%
      html_text2
    
    if(length(res) == 0){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }
  saleStatus <- lapply(sephoraHtmls2, checkIfItemOnSale)
  
  sephoraPrices <- list()
  for(item in names(sephoraHtmls2)){
    if(saleStatus[[item]] == TRUE){
      sephoraPrices[[item]] <- sephoraHtmls2[[item]] %>%
        html_elements(".basic-information-section .product-price") %>%
        html_elements(".product-price-sale-new, .product-price-sale-old") %>%
        html_text2 %>%
        gsub(x = ., pattern = "\\$", replacement = "") %>%
        as.numeric() %>%
        magrittr::set_names(c("RRP", "Price"))
    } else {
      sephoraPrices[[item]] <- sephoraHtmls2[[item]] %>%
        html_elements(".basic-information-section .product-price span") %>%
        html_text2 
      sephoraPrices[[item]][[2]] <- sephoraPrices[[item]][1]
      sephoraPrices[[item]] <- sephoraPrices[[item]] %>%
        gsub(x = ., pattern = "\\$", replacement = "") %>%
        as.numeric()
      names(sephoraPrices[[item]]) <- c("Price", "RRP")
    }
  }
  sephoraPrices
  
  sephoraPrices_df <- t(as.data.frame(sephoraPrices)) %>%
    as.data.frame %>%
    tibble::rownames_to_column("Product") %>%
    magrittr::set_colnames(c("Product", "RRP", "Price")) %>%
    dplyr::mutate(Product = gsub(x = Product, 
                                 pattern = "\\.", 
                                 replacement = " ")) %>%
    dplyr::mutate(onSale = Price < RRP) %>%
    dplyr::mutate(Discount = RRP - Price) %>%
    dplyr::mutate(Discount_Prop = Discount/RRP) %>%
    dplyr::mutate(Date = today()) %>%
    dplyr::select(Product, Price, RRP, onSale, Discount, Discount_Prop, Date)
  
  sephoraPrices_df <- sephoraPrices_df %>%
    left_join(productLists$Sephora, by = "Product")
  
  if(!file.exists(here( "productPrices.csv"))){
    sephoraPrices_df %>% 
      readr::write_csv(here("productPrices.csv"))
  } else {
    write.table(
      sephoraPrices_df,
      here("productPrices.csv"),
      append = TRUE,
      sep = ",",
      col.names = FALSE,
      row.names = FALSE
    )
  }
  
}

gc()

# PLOTS -----------------------------------------------------------------------

mainData <- readr::read_csv(here("productPrices.csv"))

plotToSave <- mainData %>%
  ggplot(aes(x = Date)) +
  geom_point(aes(y = Price, color = onSale)) +
  geom_smooth(aes(y = Price)) +
  scale_color_manual(values = c("black", "red")) +
  geom_hline(aes(yintercept = RRP), color = "blue") +
  facet_wrap( ~ Product, scales = "free_y", ncol = 3) +
  labs(x = "Date", y = "Price ($)") +
  theme(legend.position = "bottom") +
  ggtitle("Prices of Products of Interest")

export::graph2pdf(
  plotToSave,
  here("priceTrackerPlot.pdf"),
  width = 15,
  height = nrow(mainData) / 4 * 4
)
