from memory_profiler import profile
import chaospy as cp
import numpy   as np

@profile
def test_gen_quad(dist):
    N, w = cp.generate_quadrature(5, dist, "G")
    return N, w

if __name__ == '__main__':
    d = [cp.Normal(0, i+1) for i in range(8)]
    dist = cp.J(*d)
    N, w = test_gen_quad(dist)
    print(np.shape(N), np.shape(w))
