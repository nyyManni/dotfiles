#!/usr/bin/env python


from datetime import datetime
import requests
import re


DATE = datetime.now().strftime("%G-%m-%d")

DATA = requests.get("http://api.ruoka.xyz/%s" % DATE)
MODEL = DATA.json()

EXCLUDE = re.compile(r".*(wok|soup|salad|vitality|kasvis|bistro|ilta|jälki|salaatti|panini|aamiainen|vege|leipä).*", re.IGNORECASE)


for restaurant in MODEL["restaurants"]:
    print('<div class="menu">')
    print("  <h1>" + restaurant["name"] + "</h1>")
    for menu in restaurant["menus"]:
        for meal in menu["meals"]:
            if EXCLUDE.match(meal["name"]):
                continue
            mealname = ''.join([i for i in meal["name"] if not i.isdigit()])
            print("    <h2>" + mealname.lower().title() + "</h2>")
            for content in meal["contents"]:
                print("      <h3>" + content["name"].replace('\r', '').replace('\n', '') + "</h3>")
    print("</div>")
