import os
import numpy as np

from ascii_cpo import read
import easymfw.utils.io_tools

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

import pandas as pd

def get_rho(filename):
    return read(filename, "coreprof").rho.values

def get_te(filename):
    coreprof = read(filename, "coreprof")
    te = coreprof.te.value
    return te

def get_ti(filename):
    coreprof = read(filename, "coreprof")
    ti = coreprof.ti.value
    return ti

def get_tegrad(filename):
    coreprof = read(filename, "coreprof")
    tegrad = coreprof.te.ddrho
    return tegrad

def get_tigrad(filename):
    coreprof = read(filename, "coreprof")
    tigrad = coreprof.ti.ddrho
    return tigrad

def get_te_flux(filename):
    coretransp = read(filename, "coretransp")
    tetransp = coretransp.values[0]
    teflux = tetransp.te_transp.flux
    #print(teflux)
    return teflux

def get_ti_flux(filename):
    coretransp = read(filename, "coretransp")
    titransp = coretransp.values[0]
    tiflux = titransp.ti_transp.flux
    #print(tiflux)
    return tiflux

def check_equal(prof1, prof2):
    if prof1.shape != prof2.shape:
        return False
    dif = np.fabs(prof1-prof2)
    #print(dif)
    if np.any(dif > 1e-10):
        return False
    return True

def walklevel(some_dir, level=1):
    some_dir = some_dir.rstrip(os.path.sep)
    assert os.path.isdir(some_dir)
    num_sep = some_dir.count(os.path.sep)
    for root, dirs, files in os.walk(some_dir):
        yield root, dirs, files
        num_sep_this = root.count(os.path.sep)
        if num_sep + level <= num_sep_this:
            del dirs[:]

def get_camp_data(foldname, input_index=[61]):
    input_filename = "gem0_coreprof_in.cpo"
    result_filename = "gem0_coretransp_out.cpo"
    run_foldername = foldname + 'runs/'
    Te_prof_vals = []
    Ti_prof_vals = []
    Te_grad_vals = []
    Ti_grad_vals = []
    Te_flux_vals = []
    Ti_flux_vals = []
    #print(run_foldername)
    for _,runs,_ in walklevel(run_foldername):
        for run in runs:
            #print(run)
            run_in_path  = os.path.join(run_foldername, run, input_filename)
            run_out_path = os.path.join(run_foldername, run, result_filename)
            Te_prof_vals.append(get_te(run_in_path)[input_index])
            Ti_prof_vals.append(get_ti(run_in_path)[input_index])
            Te_grad_vals.append(get_tegrad(run_in_path)[input_index])
            Ti_grad_vals.append(get_tigrad(run_in_path)[input_index]) 
            Te_flux_vals.append(get_te_flux(run_out_path))
            Ti_flux_vals.append(get_ti_flux(run_out_path))    

    return Te_prof_vals, Ti_prof_vals, Te_grad_vals, Ti_grad_vals, Te_flux_vals, Ti_flux_vals    


def plot_prof(prof, rho, name):
    fig = plt.plot(rho, prof, label='Te profile')
    #plt.legend()
    plt.xlabel('rho[m]')
    plt.ylabel('Te[eV]')
    plt.title('Te profile for '+ name)
    plt.savefig('te_prof' + name + '.png')
    #plt.show()
    plt.close()


def plot_prof_seq(runfolder, nruns, name, targind=0):

    rho = range(100)
    for i in range(nruns):
        prof = get_te(runfolder+"runs/Run_"+str(i+1)+"/gem0_coreprof_in.cpo")
        plt.plot(rho, prof)
    plt.xlabel('rho[ind]')
    plt.ylabel('Te[eV]')
    if targind !=0:
        plt.axvline(targind,0,3000)
    plt.title("Te profile for " + name)
    plt.savefig('te_prof_all_' + name + '.png')
    plt.close()


def plot_scatter_2D(profvals, resvals, campname):
    fig = plt.scatter(profvals, resvals, label='Te_flux(Te|flux)')
    plt.xlabel('Te[eV]')
    plt.ylabel('Te_flux')
    #plt.ylim(83820,83840)
    #plt.autoscale(enable=True, axis='both', tight=True)
    plt.savefig('Te_scatter_' + campname + '.png')
    plt.close()


def read_data_totensor(folder):
    Tes, Tis, Tegs, Tigs, Tefs, Tifs = get_camp_data(folder)
    X = pd.DataFrame(list(zip(Tes, Tis, Tigs, Tegs)))
    Xmat = np.array([np.unique(X[:, i]) for i in range(X.shape[1])]) 
    return X, Xmat


def plot_2d_map(X, Y, inds=[2,3]):
    Xlabels = ['Te_val', 'Ti_val', 'Te_grad', 'Ti_grad']
    Ylabels = ['Te_flux', 'Ti_flux']
    
    plt.pcolormesh(X[:, inds[0]], X[:, inds[1]], Y[:, 1])

    plt.savefig("plot2d_" + Xlabels[inds[0]] + "_" + Xlabels[inds[1]] + ".png")
    plt.close()


#def get_slice(Xmat, n_slice = [3]):
#     Xsl = np.array([i for i in itertools.product([],[],[],np.linspace(Xmat))
 
        
###--------------------------------------------------------- 
	
ft1_indx = 61 # 69


scratch_folder = "/marconi_scratch/userexternal/yyudin00/"
#basefolder = "/ptmp/yyudin/UQ_GEM0_wvkryt88_sequential/runs/"
#basefolder = "/ptmp/yyudin/UQ_GEM0_jh2q6ts1/runs/"
basefolder = "/u/yyudin/codes/MFW/workflows/AUG_28906_6/"
basefolder = "/u/yyudin00/code/MFW/workflow/AUG_28906_6_1ft_restart/"
#basefolder = "/ptmp/yyudin/Fusion_Inputs/UQ_GEM_Data/runs/"
#basefolder = "/ptmp/yyudin/single_tries/gem0/b9e9pzco/"
#basefolder = "/marconi_scratch/userexternal/yyudin00/Fusion_Inputs/UQ_GEM_Data/"
#basefolder = "/marconi_scratch/userexternal/yyudin00/UQ_GEM0_LHC_hafbz8o3/"
#basefolder = "/marconi_scratch/userexternal/yyudin00/UQ_GEM0_QMC_dbg7gjbl/"
#basefolder = "/marconi_scratch/userexternal/yyudin00/UQ_GEM0_186yhlbk/"
#basefolder = scratch_folder + "UQ_GEM0_LVR_m9qiu5tm/"
#basefolder = scratch_folder + "UQ_GEM0_LVR_37os6gq0/"
basefolder = scratch_folder + "UQ_GEM0_61_e9zvw66q/"
basefolder = scratch_folder + "gemuq_qmc_tjpqqq_4/"

#filename = "Run_1/gem0_coreprof_in.cpo" 
#filename = "gem0_coreprof_in.cpo"
#filename = "ets_coreprof_in.cpo"

runfold1 = "Run_1"
runfold2 = "Run_15"
filename = "gem0_coreprof_in.cpo"
filename_res = "gem0_coretransp_out.cpo"
filename_res = "gem0_coretransp_out.cpo"

#pr11 = get_te(basefolder + runfold1 + "/" + filename)
#pr12 = get_tegrad(basefolder + runfold1 + "/" + filename)
#pr21 = get_te(basefolder + runfold2 + "/" + filename)
#pr22 = get_tegrad(basefolder + runfold2 + "/" + filename)

#res1 = get_te_flux(basefolder + runfold1 + "/" + filename_res)[0]
#res2 = get_te_flux(basefolder + runfold2 + "/" + filename_res)[0]

###---Print the values in selected folders-----
#print(runfold1 + " te: " + str(pr11[ft1_indx]) + " ; " + runfold2 + " te: " + str(pr21[ft1_indx]) + " ; " + runfold1 + " gradte: " + str(pr12[ft1_indx]) + " ; " + runfold2 + " gradte: " + str(pr22[ft1_indx]) + " ; " + runfold1 + " teflux: " + str(res1) + " ; " + run/fold2 + "teflux: " + str(res2))

###---Print all fluxes in a campaing---
#for i in range(1,200):
#    pr2 = get_te_flux(basefolder +  "Run_" + str(i) + "/" + filename_res)[0]
#    #print(check_equal(pr1,pr2))
#    print(str(pr2) + "\n")

###---Plot profiles for a case------
#for i in range(16):    
#    rho = get_rho(basefolder + "Run_" + str(i) + "/gem0_coreprof_in.cpo")
#    plot_prof(pr11, np.linspace(0,1,100), "_gem0_aug_seq_run_input_1")

#plot_prof_seq(basefolder, 16, "_gem0_aug_par_tes_at61_", ft1_indx)

#print("flux for the case " + str(1) + " : " + '%.3g'%(res1))
#plot_prof(pr21, np.linspace(0,1,100), "_gem0_aug_sew_tun_input_2")
#print("flux for the case " + str(2) + " : " + '%.3g'%(res2))

###---Plot scatter plot of Te and Te_flux ofr a  campaing----
teprvals, tiprvals, tegrvals, tigrvals, teflvals, tiflvals = get_camp_data(basefolder, ft1_indx)
print(1)
plot_scatter_2D(teprvals, teflvals, 'fuswfGEM0_ft61_PCE')
print(2)
#print("Te flux values: ")
#print(teflvals)

###---Plot color mesh---
#X = read_data_totensor(basefolder)
X = np.array([teprvals, tiprvals, tegrvals, tigrvals])
X = np.transpose(X)
Y = np.array([teflvals, tiflvals])
Y = np.transpose(Y)
print(3)
plot_2d_map(X,Y)


###---Check flux tube indices--


