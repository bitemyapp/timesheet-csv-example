#!/usr/bin/python

import csv
from collections import defaultdict
from datetime import timedelta

logged = defaultdict(list)

with open("data.csv", "rb") as csvfile:
    reader = csv.reader(csvfile)
    for row in reader:
        hours, minutes = [int(x) for x in row[13].split(':')]

        logged[row[8]].append(timedelta(minutes=minutes, hours=hours))

for client in logged:
    print client
    print sum(logged[client], timedelta())
