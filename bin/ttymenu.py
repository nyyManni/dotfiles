#!/usr/bin/env python3
""" Simple python script to load and parse menu's from
Tampere University of Technology restaurants"""

from datetime import datetime, timedelta
import re
import requests



DATE = datetime.now()

# After 16:00, give next day's menu
if DATE.hour > 15:
    DATE += timedelta(days=1)


# On weekends, give next monday's menu
if DATE.weekday() > 4:
    DATE += timedelta(days=7-DATE.weekday())

DATE = DATE.strftime("%G-%m-%d")

DATA = requests.get("http://api.ruoka.xyz/%s" % DATE)
MODEL = DATA.json()

# Do not give unnecessary vegetarian meals
EXCLUDE = re.compile(r".*(wok|soup|salad|vitality|kasvis|bistro|ilta|jälki|salaatti|panini|aamiainen|vege|leipä).*", re.IGNORECASE)


for restaurant in MODEL["restaurants"]:
    print('<div class="restaurant">')
    print("  <h1>" + restaurant["name"] + "</h1>")
    for menu in restaurant["menus"]:
        for meal in menu["meals"]:
            if EXCLUDE.match(meal["name"]):
                continue
            mealname = ''.join([i for i in meal["name"] if not i.isdigit()])
            print("    <h2>" + mealname.lower().title() + "</h2>")
            for content in meal["contents"]:
                print("      <h3>" + content["name"]
                      .replace('\r', '')
                      .replace('\n', '') + "</h3>")
    print("</div>")
