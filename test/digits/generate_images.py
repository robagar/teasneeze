import numpy as np
from sklearn.datasets import load_digits
from PIL import Image
from itertools import count
from os import makedirs

counters = []
for i in range(10):
    counters.append(count())
    makedirs('images/{0}'.format(i), exist_ok=True)


digits = load_digits()

for t,d in zip(digits.target, digits.data):
    i = next(counters[t])
    f = 'images/{0}/{0}-{1}.png'.format(t,i)
    print(f)

    a = ((d / 16.0) * 255).reshape(8,8)
    i = Image.fromarray(np.uint8(a))
    i.convert('RGB').save(f)

    