### DOWNLOAD OLYMPIC SPEED SKATING DATA ########################

# -- Libraries -------------------------

from selenium import webdriver
from bs4 import BeautifulSoup
import re
import pandas as pd
import json
import os
import glob

# -- Prologue ------------------------

verbose = True

base_url = 'https://www.olympiandatabase.com'
parent_url = f'{base_url}/index.php?id=6934&L=1'

out_path = './event_files'

# -- Get website ------------------------

options = webdriver.ChromeOptions()
options.add_argument('--headless')
driver = webdriver.Chrome(options=options)
driver.get(parent_url)

html_source = driver.page_source
soup = BeautifulSoup(html_source, 'html.parser')

# -- Get list of games ------------------------

parent_table = soup.find_all('table', attrs={'class': 'frame_space'})[-1]

game_links = []
for link in parent_table.find_all('a'):
    game_links.append(link.get('href'))

game_links = [i for i in game_links if not re.compile(r'http://.*$').match(i)]
game_links = game_links[:-1]

# -- Get list of events per game ------------------------

for i in game_links:
    
    driver.get(f'{base_url}{i}')
    html_source = driver.page_source
    soup = BeautifulSoup(html_source, 'html.parser')
    
    event_table = soup.find_all('table', attrs={'class': 'frame_space'})[-1]
    
    event_links = []
    for link in event_table.find_all('a'):
        if link.find(text=re.compile('0 m|Combined|Mass|Team')):
            event_links.append(link.get('href'))
    
    event_links = [j for j in event_links if not re.compile(r'/index.php\?id=13738&L=1').match(j)]
    
    for j in event_links:
        
        driver.get(f'{base_url}{j}')
        html_source = driver.page_source
        soup = BeautifulSoup(html_source, 'html.parser')
        
        id = re.search('id=(.*)&', j).group(1)
        if id != '11769':
            title = soup.find('h1').text
            year = re.search('Speed Skating (.*) Winter Olympics', title).group(1).split()[-1]
            distance = re.search('\'s (.*) -', title).group(1)
            sex = re.search('^(.*)\'s', title).group(1).split()[0]
            tab = pd.read_html(f'{base_url}{j}', match='Final')[0]
        else:
            year = '1994'
            distance = '5000 m'
            sex = 'Men'
            title = f'{sex}\'s {distance} - Speed Skating Lillehammer {year} Winter Olympics'
            tab = pd.read_html(f'{base_url}{j}')[2]
        
        if verbose:
            print(f'Extracting data for the {sex}\'s {distance} from {year}')
        
        # Write to json
        out_data = {
            'title': title,
            'year': int(year),
            'distance': distance,
            'sex': sex,
            'table': tab.to_json(),
            'id': int(id)
        }
        
        file_name = f'{year}_{distance.lower().replace(" ", "")}_{sex.lower()}.json'
        with open(f'{out_path}/{file_name}', 'w') as file_out:
            json.dump(out_data, file_out, indent=4)
        
    pass
    
# -- Quit browser ------------------------

driver.quit()

# -- Merge json files -------------------------

if verbose:
    print('Merging json files')

json_file_list = []
for file in os.listdir(out_path):
    full_path = os.path.join(out_path, file)
    json_file_list.append(full_path)

# -- Define function to merge json files ------------------------

out_name = "./all_events.json"

result = []
for f in glob.glob(f'{out_path}/*.json'):
    with open(f, "rb") as infile:
        result.append(json.load(infile))

with open(out_name, 'w') as outfile:
     json.dump(result, outfile, indent=4)
