from scipy.io import FortranFile
import h5py

def read_time_dat_fortran(filneame):
    f = FortranFile(filneame, 'r')
    times = []
    for i in range(10):
        times.append(f.read_reals(dtype='float32'))
    return times

def read_time_dat_hdf5(filename):
    f = h5py.File(filename, 'r')
    fl = list(f.keys())
    print(fl)

    return f

foldername = "../workflows/AUG_28906_6_1ft_restart/"
timefile = "t00.dat"
timefile = 'd00.dat'

#t = read_time_dat(foldername + timefile)
t = read_time_dat_hdf5(foldername + timefile)

#print(t)
