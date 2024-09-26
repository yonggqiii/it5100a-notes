#!/usr/bin/env python3

from datetime import datetime
import os
import re

date = '%20'.join(datetime.today().strftime('%d %b %Y').upper().split())

def update_dir():
    stuff = os.listdir()
    for f in stuff:
        flag = False
        if f[-3:] == '.md':
            with open(f) as file:
                contents = file.read()
                contents = re.sub(r'\[update-shield\]:.*', f'[update-shield]: https://img.shields.io/badge/LAST%20UPDATED-{date}-57ffd8?style=for-the-badge', contents)
                flag = True
            if flag:
                with open(f, 'w') as file:
                    file.write(contents)
                    print(f'{f} updated')
        elif os.path.isdir(f):
            os.chdir(f)
            update_dir()
            os.chdir('..')

update_dir()
