"""
Scrape the election results
"""

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import Select
import time
import pandas as pd


def _get_driver(url):
    """
    Start a driver for a prespecified url
    """

    driver = webdriver.Chrome()
    driver.get(url)
    time.sleep(0.5)

    return driver


def _get_parial_table(driver):
    """
    Get the partial results table
    """

    html = driver.page_source

    df_partial_table = pd.read_html(html, header=0)[2]
    df_partial_table.columns = df_partial_table.columns.str.replace(" ", "_")

    return df_partial_table


def _get_results_table(driver):
    """
    Navigate to the results table
    """

    print("Click on cookies banner")
    time.sleep(5)

    select = Select(
        driver.find_element(By.XPATH, '//*[@id="dataTable_length"]/label/select')
    )
    select.select_by_value("100")

    print("Click away ad")
    time.sleep(5)

    last_element = driver.find_element(
        By.XPATH, '//*[@id="dataTable_paginate"]/ul/li[5]/a'
    ).get_attribute("innerHTML")
    last_element = int(last_element)

    df_results = pd.DataFrame()
    for i in range(1, last_element + 1):
        driver.switch_to.window(driver.current_window_handle)

        time.sleep(4)
        next_element = driver.find_element(
            By.XPATH, f'//*[@id="dataTable_paginate"]/ul/li[{i + 1}]/a'
        )
        next_element.click()

        df_tmp = _get_parial_table(driver)
        df_results = pd.concat([df_results, df_tmp])

    return df_results


def scrape_election_results(url):
    """
    Scrape the results
    """

    driver = _get_driver(url)

    df = _get_results_table(driver)

    driver.quit()

    return df


def save_to_file(df, filepath):
    """
    Save the data frame as a csv file
    """

    df.to_csv(filepath, sep=";", index=False)


def main():
    url = (
        "https://allecijfers.nl/uitslag-tweede-kamer-verkiezingen-2023/#tabel_per_regio"
    )

    df = scrape_election_results(url=url)
    save_to_file(df, "./data/election_results.csv")


if __name__ == "__main__":
    main()
