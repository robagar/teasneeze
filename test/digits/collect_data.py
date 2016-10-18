import json
from itertools import count
import numpy as np
from sklearn.datasets import load_digits

counters = []
for i in range(10):
    counters.append(count())

def make_entry(td):
    t,d = td
    i = next(counters[t])
    return {
        'classification': str(t),
        'image_path': 'images/{0}/{0}-{1}.png'.format(t,i),
        'data_vector': list(d)
    }

digits = load_digits()
out = {
    'name': 'digits',
    'data_points': list(map(make_entry, zip(digits.target, digits.data)))
}    

with open('digits.json', 'w') as f:
    json.dump(out, f, indent=4)

    