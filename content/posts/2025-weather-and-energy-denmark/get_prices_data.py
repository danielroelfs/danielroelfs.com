"""
Get the average price per day
"""

import os
import pandas as pd
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from bs4 import BeautifulSoup
import time
import sqlite3


def get_driver():
    """
    Get the driver
    """

    chrome_options = Options()
    chrome_options.add_argument("--headless=new")
    driver = webdriver.Chrome(options=chrome_options)

    return driver


def get_averages_prices(driver, date):
    """
    Get the average price per day
    """

    url = f"https://data.nordpoolgroup.com/auction/day-ahead/prices?deliveryDate={date}&currency=DKK&aggregation=DeliveryPeriod&deliveryAreas=DK1,DK2"
    driver.get(url)

    time.sleep(1)
    html_day = driver.page_source

    soup = BeautifulSoup(html_day, "html.parser")
    summary_divs = soup.findAll(
        "div", {"class": "dx-datagrid-summary-item dx-datagrid-text-content"}
    )

    avg_dk1 = float(summary_divs[-4].text.replace(",", ".").replace("\xa0", ""))
    avg_dk2 = float(summary_divs[-1].text.replace(",", ".").replace("\xa0", ""))

    result_dict = {"date": date, "avg_dk1": avg_dk1, "avg_dk2": avg_dk2}

    return result_dict


def get_list_of_prices(driver, quiet=False):
    """
    Get a data frame of average energy prices
    """
    dates_oi = pd.date_range(
        start=pd.to_datetime("now") - pd.Timedelta("1d") - pd.Timedelta("61d"),
        end=pd.to_datetime("now") - pd.Timedelta("1d"),
    )
    dates_oi_str = [x.strftime("%Y-%m-%d") for x in dates_oi]

    df_prices = pd.DataFrame()
    for doi in dates_oi_str:
        if not quiet:
            print(f"Collecting for date: {doi}")
        df_tmp = pd.DataFrame([get_averages_prices(driver, date=doi)])
        df_prices = pd.concat([df_prices, df_tmp if not df_tmp.empty else None])

    return df_prices


def main():

    driver = get_driver()

    df_prices = get_list_of_prices(driver)

    conn = sqlite3.connect(os.path.join("data", "energy_data.db"))

    df_prices.to_sql(name="energy_prices", con=conn, if_exists="replace", index=False)


if __name__ == "__main__":
    main()
