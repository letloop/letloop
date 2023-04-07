#!/usr/bin/env python3
from lxml import html
import requests
import sys


URL = sys.argv[1]
assert URL.startswith('https://uk.lxd.images.canonical.com/images/')

response = requests.get(URL)
html = html.fromstring(response.text)
directories = html.xpath('//tr/td/a/text()')
directories.remove('Parent Directory')

latest = sorted(directories, reverse=True)[0].rstrip('/')
print(latest)
