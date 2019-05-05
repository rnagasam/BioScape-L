import argparse
from matplotlib import animation
from matplotlib import pyplot as plt
import numpy as np

class BioScapeAnimate(object):
    def __init__(self, resfile):
        self.resfile = resfile
        self.species = set() # entities
        self.minx = self.miny = float('inf')
        self.maxx = self.maxy = float('-inf')
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
                    self.minx = min(self.minx, dat[1])
                    self.miny = min(self.minx, dat[2])
                    self.maxx = max(self.maxx, dat[1])
                    self.maxy = max(self.maxy, dat[2])
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
        scale_factor = 1.25
        self.ax.axis(np.array([self.minx, self.maxx,
                               self.miny, self.maxy]) * scale_factor)
        return self.scat,

    def update(self, i):
        """Update scatter plot."""
        x, y, s, c = next(self.dstream)
        self.scat.set_offsets(np.column_stack((x, y)))
        self.scat.set_sizes(s * 100)
        self.scat.set_array(np.array(c))
        return self.scat,

    def save_animation(self, path, fps):
        """Save animation in as mp4."""
        try:
            self.ani.save(path, fps=fps, writer='ffmpeg')
        except StopIteration:
            pass

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='BioScape^L plotting utility.')
    parser.add_argument('infile', metavar='file', type=str,
                        help='Input file containing BioScape^L simulation '
                        'results')
    parser.add_argument('-o', '--output', metavar='output', type=str,
                        default=None, help='Output file (mp4)')
    parser.add_argument('-f', '--frame-rate', metavar='fps', type=int,
                        default=24, help='Frame-rate of output video')
    args = parser.parse_args()
    b = BioScapeAnimate(args.infile)
    if args.output:
        b.save_animation(args.output, args.frame_rate)
    else:
        plt.show()
