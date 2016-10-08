import json
from itertools import count
import numpy as np
from sklearn.datasets import load_digits
from sklearn.manifold import TSNE

counters = []
for i in range(10):
    counters.append(count())


digits = load_digits()
tsne = TSNE(n_components=3)
rs = tsne.fit_transform(digits.data)

# normalize
rs -= rs.min(axis=0)
rs /= rs.max(axis=0)

def make_result(tr):
    t,r = tr
    i = next(counters[t])
    return {
        'classification': str(t),
        'image_path': 'images/{0}/{0}-{1}.png'.format(t,i),
        'x': float(r[0]), 
        'y': float(r[1]),
        'z': float(r[2])
    }

out = {
    'name': 'digits',
    'data_points': list(map(make_result, zip(digits.target, rs)))
}    

with open('digits_tsne.json', 'w') as f:
    json.dump(out, f, indent=4)

    