#! /usr/bin/env python
import numpy as np 
import matplotlib.pylab as plt
import matplotlib.patches
import jet

plt.ion()

Te, ne, rho, rho_norm = jet.solve_Te(nr=10)

plt.clf()
a = plt.subplot(111, aspect='equal')
plt.ylim(-2.5,2.5)
plt.xlim(0,5)

for i, c in enumerate([matplotlib.patches.Circle((3,0), p+rho[0]) for p in rho]):
    c.set_clip_box(a.bbox)
    c.set_alpha(Te[i]/Te[0])
    a.add_artist(c)
a.add_artist(matplotlib.patches.Circle((3,0), rho[-1]+rho[0], fill=False, alpha=1, ls=':'))
a.add_artist(matplotlib.patches.Ellipse((3,0), 2*1, 2*1*1.5, fill=False, alpha=1))
plt.xlabel('R')
plt.ylabel('Z')
plt.savefig('cyl_jet.png')

