import numpy as np
from matplotlib import pyplot as plt
from matplotlib import animation

class BioScapeAnimate(object):
    def __init__(self, resfile='/tmp/bioscape.out'):
        self.resfile = resfile
        self.species = set() # entities
        self.results = self.read_results()
        self.scolors = self.generate_colors()
        self.dstream = self.data_stream()

        self.scat = None
        self.fig, self.ax = plt.subplots()
        self.ani = animation.FuncAnimation(self.fig, self.update,
                                           init_func=self.init_plot,
                                           blit=True, repeat=False)

    def read_results(self):
        """Read results from a BioScape output file."""
        results = dict()
        cur = None
        with open(self.resfile) as f:
            for i, line in enumerate(f):
                if i == 0: # First line contains step size of simulation
                    self.simstep = int(line.strip())
                    continue
                if not line.startswith('\t'):
                    cur = int(line.strip())
                    results[cur] = []
                else:
                    assert(cur is not None)
                    dat = list(filter(bool, map(lambda x: x.strip(),
                                                line.split('\t'))))
                    dat[1:] = list(map(float, dat[1:]))
                    self.species.add(dat[0])
                    results[cur].append(dat)
        return results

    def generate_colors(self):
        """Return a dictionary mapping each species to a color."""
        return {s: np.random.random() for s in self.species}

    def data_stream(self):
        """Stream of values used for plotting.  Yields an Nx2 array of
        points, an Nx1 array of sizes, and an Nx3 array of colors."""
        for data in self.results.values():
            xs = np.array([np.array(xi[1]) for xi in data])
            ys = np.array([np.array(xi[2]) for xi in data])
            sizes = np.array([np.array(xi[3]) for xi in data])
            colors = np.array(list(map(lambda x: self.scolors[x],
                                       [xi[0] for xi in data])))
            yield xs, ys, sizes, colors

    def init_plot(self):
        """Initial scatter plot."""
        x, y, s, c = next(self.dstream)
        self.scat = self.ax.scatter(x, y, c=c, s=s, vmin=0, vmax=1,
                                    cmap="jet", edgecolor="k")
        self.ax.axis([-50, 50, -50, 50])
        return self.scat,

    def update(self, i):
        """Update scatter plot."""
        x, y, s, c = next(self.dstream)
        self.scat.set_offsets(np.column_stack((x, y)))
        self.scat.set_sizes(s * 100)
        self.scat.set_array(np.array(c))
        return self.scat,