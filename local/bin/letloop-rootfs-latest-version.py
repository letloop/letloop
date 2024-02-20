#!/usr/bin/env python3
from lxml import html
import requests
import sys


URL = sys.argv[1]
assert URL.startswith('https://images.linuxcontainers.org/images/')

response = requests.get(URL)
html = html.fromstring(response.text)
directories = html.xpath('//a/text()')
directories.remove('../')


latest = sorted(directories, reverse=True)[0].rstrip('/')
print(latest)
