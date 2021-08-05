import os
import numpy as np
import pandas as pd

from ascii_cpo import read
import easymfw.utils.io_tools

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

#from mpl_toolkits.axes_grid1 import host_subplot
#import mpl_toolkits.axisartist as AA

#from basicda.da_utils import read_sim_csv, walklevel

#from utils import l3interp, l3deriv

def get_rho(filename):
    return read(filename, "coreprof").rho_tor[0].values

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
    return teflux

def get_ti_flux(filename):
    coretransp = read(filename, "coretransp")
    titransp = coretransp.values[0]
    tiflux = titransp.ti_transp.flux
    return tiflux

def check_equal(prof1, prof2, esp=1e-10):
    """
    Retrun True if two profile are identical up to eps
    """
    if prof1.shape != prof2.shape:
        return False
    dif = np.fabs(prof1-prof2)
    if np.any(dif > eps):
        return False
    return True

def get_run_data(foldname, codename='gem0', input_index=[61]):
    """
    Gets input and output values for a single simulation run
    """
    Xlabels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
    Ylabels = ['te_transp_flux', 'ti_transp_flux']
    input_filename = codename + "_coreprof_in.cpo"
    output_filename = codename + "_coretransp_out.cpo"
    coretransp = read(os.path.join(foldname, output_filename), "coretransp")
    tiflux = coretransp.values[0].ti_transp.flux[0]
    teflux = coretransp.values[0].te_transp.flux
    coreprof = read(os.path.join(foldname, input_filename), "coreprof")
    teval = coreprof.te.value[input_index]
    tival = coreprof.ti.value[input_index][0]
    teddrho = coreprof.te.ddrho[input_index]    
    tiddrho = coreprof.ti.ddrho[input_index][0]
    
    return pd.DataFrame([[teval[0], tival[0], teddrho[0], tiddrho[0], teflux[0], tiflux[0]]], columns=Xlabels + Ylabels)

def get_camp_dataframe(foldname, codename='gem0', input_index=[61]):
    """
    Get set of input-outputs for a campaign of simualtion runs
    as a pandas dataframe
    """
    Xlabels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
    Ylabels = ['te_transp_flux', 'ti_transp_flux']
    simdata = pd.DataFrame(columns=Xlabels + Ylabels)

    for _,runs,_ in walklevel(os.path.join(foldname, 'runs/')):
        for run in runs:
            runfolder = os.path.join(foldname, 'runs/', run)
            runres = get_run_data(runfolder, codename, input_index)
            #print(runres)
            simdata = pd.concat([simdata, runres], ignore_index=True)
            #simdata = simdata.append(runres, ignore_index=True)
    
    #print(simdata)
    simdata.to_csv('campaign_data.csv', index=False)
    return simdata
            
def get_camp_data(foldname, input_index=[61]):
    Xlabels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
    Ylabels = ['te_transp_flux', 'ti_transp_flux']
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

    data = pd.DataFrame({Xlabels[0]: Te_prof_vals, Xlabels[1]: Ti_prof_vals, Xlabels[2]: Te_grad_vals,
                         Xlabels[3]: Ti_grad_vals, Ylabels[2]: Te_flux_vals, Ylabels[1]: Ti_flux_vals, })
    data.to_csv(foldname + "campaign_basic_data" +'.csv')
    #return Te_prof_vals, Ti_prof_vals, Te_grad_vals, Ti_grad_vals, Te_flux_vals, Ti_flux_vals    
    return data

def compare_response_pointwise(data1, data2, res_lab_num=0):
    """
    For two simulation campaigns compares output values pointwise and returns RMSD
    """
      
    Xlabels = ['te_value', 'ti_value', 'te_ddrho', 'ti_ddrho']
    Ylabels = ['te_transp_flux', 'ti_transp_flux']

    #data1.reset_index()
    #data2.reset_index()
    #print((data1[[Xlabels[0]]].iloc[0] - data2[[Xlabels[0]]].iloc[0]))

    diff = pd.DataFrame()
    diff_rel = pd.DataFrame()
    for i in range(len(data1.index)):
        if all( [ (abs(data1.iloc[i][label] - data2.iloc[i][label]) < 1e-12) for label in Xlabels] ):
             diff_row = abs(data1.loc[Ylabels[res_lab_num]].iloc[i] - data2.loc[Ylabels[res_lab_num]].iloc[i])
             diff = diff.append(diff_row*diff_row)
             diff_rel = diff_rel.append(diff_row / data2.loc[Ylabels[res_lab_num]].iloc[i])
    
    diff_rel.to_csv("camp_comp_dat" + Ylabels[res_lab_num] + ".scv")
    return np.sqrt(diff.sum())

def plot_prof(prof, rho, name):
    """
    Plots profile values (agains rho)
    """
    fig = plt.plot(rho, prof, label='Te profile')
    #plt.legend()
    plt.xlabel('rho[m]')
    plt.ylabel('Te[eV]')
    plt.title('Te profile for '+ name)
    plt.savefig('prof_' + name + '.png')
    #plt.show()
    plt.close()

def plot_prof_all(profs, rho, name, proflabels):
    """
    Plots a list of profiles of different parameters for the same scenario
    """

    colorchoice = ['b','g','r','c','m','y','k',]
           
    fig, ax1 = plt.subplots()
    ax1.set_ylabel('T')

    ax2 = ax1.twinx()    
    ax2.set_label('gradT')

    for i, (prof, label) in enumerate(zip(profs, proflabels)):
        if label.startswith('grad'):
            ax2.plot(rho, prof, label=label, color=colorchoice[i])
        else:
            ax1.plot(rho, prof, label=label, color=colorchoice[i])

    ax1.tick_params(axis='y', labelcolor='tab:red')
    ax2.tick_params(axis='y', labelcolor='tab:blue')

    ax1.set_xlabel('rho[m]i')

    ax1.legend(loc=0)
    ax2.legend(loc=3)

    plt.title('Profiles')
    plt.savefig('profs_' + name + '.png')
    plt.close()

def plot_prof_seq(runfolder, nruns, name, targind=0):
    """
    Plots multiple profiles (val-s vs rho) at the sam figure
    """
    rho = range(100)
    for i in range(1, nruns):
        #prof = get_te(runfolder+"/runs/Run_"+str(i+1)+"/gem0_coreprof_in.cpo")
        prof = get_tegrad(runfolder+"/runs/Run_"+str(i+1)+"/gem0_coreprof_in.cpo")
        plt.plot(rho, prof, linewidth=0.5)
        plt.plot(rho, prof, 'ko', markersize=8)
    plt.xlabel('rho[ind]')
    plt.ylabel('grTe[eV/m]')
    if targind !=0:
        plt.axvline(targind,0,3000)
    plt.title("gradTe profile for " + name)
    plt.savefig('tegrad_prof_all_' + name + '.pdf')
    plt.close()

def plot_scatter_2D(profvals, resvals, campname):
    fig = plt.scatter(profvals, resvals, label='Ti_flux(DTi|flux)')
    plt.xlabel('Ti[eV/m?]')
    plt.ylabel('Ti_flux')
    #plt.yscale('log')
    #plt.ylim(83820,83840)
    #plt.autoscale(enable=True, axis='both', tight=True)
    plt.savefig('Te_scatter_' + campname + '.png')
    plt.close()

def plot_scatter_2D_mult(profvals, resvals, labels, campname):
    """
    Plots sequence of 2d scatter graphs on a single figure
    """
    fig = plt.figure()
    ax1 = fig.add_subplot(111)

    for i in range(len(profvals)):
        ax1.scatter(profvals[i], resvals[i], alpha=0.4, label=labels[i] )
    
    plt.title('Ti_flux(Ti|flux)')
    plt.xlabel('Ti[eV/m?]')
    plt.ylabel('Ti_flux')
    plt.yscale('log')
    plt.legend(loc='upper left')
    plt.savefig('T_mult_scatter_' + campname + '.pdf')
    plt.close()

def print_sep_vals(basefolder, runfold1, runfold2):
    """Prints the values (at profiles) for selected folders
    """
    pr11 = get_te(basefolder + runfold1 + "/" + filename)
    pr12 = get_tegrad(basefolder + runfold1 + "/" + filename)
    pr21 = get_te(basefolder + runfold2 + "/" + filename)
    pr22 = get_tegrad(basefolder + runfold2 + "/" + filename)

    res1 = get_te_flux(basefolder + runfold1 + "/" + filename_res)[0]
    res2 = get_te_flux(basefolder + runfold2 + "/" + filename_res)[0]

    print(runfold1 + " te: " + str(pr11[ft1_indx]) + " ; " + runfold2 + " te: " + str(pr21[ft1_indx]) + " ; " 
        + runfold1 + " gradte: " + str(pr12[ft1_indx]) + " ; " + runfold2 + " gradte: " + str(pr22[ft1_indx]) + " ; "
        + runfold1 + " teflux: " + str(res1) + " ; " + runfold2 + "teflux: " + str(res2))
    return

def print_camp_fluxes(basefolder, pr1):
    """
    Prints all flux values of a campaign
    """
    for i in range(1,200):
        pr2 = get_te_flux(basefolder +  "Run_" + str(i) + "/" + filename_res)[0]
        print(check_equal(pr1,pr2))
        print(str(pr2) + "\n")

def plot_run_profiles(basefolder, name="_gem0_aug_par_tes_at69_"):
    """
    Plot profiles for a single run
    """
    n_runs = 2048
    targind = 0 # 69
    for i in range(1, n_runs): 
        continue   
        #rho = get_rho(basefolder + "/runs/Run_" + str(i) + "/gem0_coreprof_in.cpo")
        #plot_prof(pr11, np.linspace(0,1,100), "_gem0_aug_seq_run_input_1")

    plot_prof_seq(basefolder, n_runs, name, targind)

    #print("flux for the case " + str(1) + " : " + '%.3g'%(res1))
    #plot_prof(pr21, np.linspace(0,1,100), "_gem0_aug_sew_tun_input_2")
    #print("flux for the case " + str(2) + " : " + '%.3g'%(res2))

def two_camp_compare(filename1="data/gem_uq_inoutput.csv", filename2="campaign_data.csv"):
    """
    Compare GEM and GEM0 results for PCE
    """

    #gem0data = get_camp_dataframe(basefolder, [ft1_indx])
    gemdata, _, _ = read_sim_csv(filename1)
    gem0data, _, _ = read_sim_csv(filename2)

    #plot_camp_vals(gem0data, "gem0")
    #plot_camp_vals(gemdata, "gem")

    gemdata_slice = gemdata[ (gemdata['te_ddrho'] == np.median(gemdata['te_ddrho'].unique())) & 
                         (gemdata['ti_ddrho'] == np.median(gemdata['ti_ddrho'].unique())) &
                         (gemdata['te_value'] == np.median(gemdata['te_value'].unique())) &
                         (gemdata['ti_value'] == np.median(gemdata['ti_value'].unique())) ] 
                        
    gem0data_slice = gem0data[gem0data['te_value'] == np.median(gem0data['te_value'].unique())]

    #print(len(gem0data.groupby(['te_value', 'ti_value']).groups)) # check how many unique values are there in {Te}x{Ti}         
    print(gemdata_slice[['ti_transp_flux', 'te_transp_flux']])

    #plot_scatter_2D(gem0data_slice['ti_value'], gem0data_slice['ti_transp_flux'], 'gem0_titi_single')
    #plot_scatter_2D(gemdata['ti_value'], gemdata['ti_transp_flux'], 'gem_tiddrho_')

    #plot_scatter_2D_mult([gem0data_slice['ti_value'], gemdata_slice['ti_value']] , 
    #                     [gem0data_slice['ti_transp_flux'], gemdata_slice['ti_transp_flux']] ,
    #                     ['gem0', 'gem'], '_multgemgem0titi_med')

    diff = compare_response_pointwise(gem0data, gemdata)
    return diff

def get_int_camp_data(basefolder, paramname='te.ddrho'):
    df = pd.Empty(columns=['te_ddrho'])
    # TODO: same as get_camp_dataframe() but get new get_run_data() has to read profiles or the whole cpo and call interpolation routines
    return df

def deriv(prof, delta):
    prof_deriv = prof[1:] - prof[0:-1] / delta
    return prof_deriv

###---------------------------------------------------------
	
# ft1_indx = 69 # 61
#
# scratch_folder = "/marconi_scratch/userexternal/yyudin00/"
# #basefolder = "/ptmp/yyudin/UQ_GEM0_wvkryt88_sequential/runs/"
# #basefolder = "/ptmp/yyudin/UQ_GEM0_jh2q6ts1/runs/"
# basefolder = "/u/yyudin/codes/MFW/workflows/AUG_28906_6/"
# basefolder = "/u/yyudin00/code/MFW/workflow/AUG_28906_6_1ft_restart/"
# #basefolder = "/ptmp/yyudin/Fusion_Inputs/UQ_GEM_Data/runs/"
# #basefolder = "/ptmp/yyudin/single_tries/gem0/b9e9pzco/"
# #basefolder = "/marconi_scratch/userexternal/yyudin00/Fusion_Inputs/UQ_GEM_Data/"
# #basefolder = "/marconi_scratch/userexternal/yyudin00/UQ_GEM0_LHC_hafbz8o3/"
# #basefolder = "/marconi_scratch/userexternal/yyudin00/UQ_GEM0_QMC_dbg7gjbl/"
# #basefolder = "/marconi_scratch/userexternal/yyudin00/UQ_GEM0_186yhlbk/"
# #basefolder = scratch_folder + "UQ_GEM0_LVR_m9qiu5tm/"
# #basefolder = scratch_folder + "UQ_GEM0_LVR_37os6gq0/"
# basefolder = scratch_folder + "UQ_GEM0_61_e9zvw66q/"
# basefolder = scratch_folder + "gemuq_qmc_tjpqqq_4/"
#
# basefolder = os.path.join(scratch_folder,"gemuq_qmc_hjwchjla") # gem0 campaign run with 3e+3 samples (QMC, 1ft @69) on 21.09.2020
#
# basefolder = os.path.join(scratch_folder, "gem0uq_pce_i67og8gy") # gem0 campaign run with 625 runs (PCE, const gradients, 1ft @69) on 22.09.2020
# basefolder = os.path.join(scratch_folder, "gem0uq_pce_gbw0uhl3") # gem0 campaing with 625 runc (PCE, const gradients, 1dt @69) on 25.09.2020

#filename = "Run_1/gem0_coreprof_in.cpo" 
#filename = "ets_coreprof_in.cpo"

# runfold1 = "Run_1"
# runfold2 = "Run_15"
# filename = "gem0_coreprof_in.cpo"
# filename_res = "gem0_coretransp_out.cpo"
#
#
# gem0data, _, _ = read_sim_csv("campaign_data.csv")
#plot_camp_vals(gem0data, 'gem0')


# compare gem and gem0 campaign
#two_camp_compare()

#os.system("diff" + os.path.join(scratch_folder, "gem08ftuq_pce_v0ij1gtr/common/res0.csv " + 
#          os.path.join(scratch_folder, "gem08ftuq_python_pce_8o_og7c8/common/res0.csv")

#gem0van, _, _ = read_sim_csv("gem08ftuq_pce_v0ij1gtr/common/res0.csv")
#gem0pyt, _, _ = read_sim_csv("gem08ftuq_python_pce_8o_og7c8/common/res0.csv")


# see how profiles are sampled for 2ft
#folder2ftrun = os.path.join(scratch_folder, "gem08ftuq_pce_z85mi86u") # a 2ft gem0 campaing on 13.10.2020
#folder8ftrun = os.path.join(scratch_folder, "gem08ftuq_pce_v0ij1gtr") # a 8ft gem0 campaing on ...10.2020

#plot_run_profiles(folder8ftrun, "_gem0_aug_8ft_")

#num = 2047 
#plot_prof(get_te(folder8ftrun+'/runs/Run_'+str(num)+'/gem0_coreprof_in.cpo'), np.linspace(0,1,100), 'prof_8ftrun_at_'+str(8))

#check current AUG profile steepest points
exp_folder = "../workflows/AUG_28906_6_1ft_restart"
prof_file = "ets_coreprof_in.cpo"
prof_file_path = os.path.join(exp_folder, prof_file)
rho = np.linspace(0, 100, 100)
prof_te = get_te(prof_file_path)
# #print(rho)
# plot_prof(prof_te, rho, 'te_aug6')
prof_ti =  get_ti(prof_file_path)
prof_gradte = get_tegrad(prof_file_path)
prof_gradti = get_tigrad(prof_file_path)
profs = [prof_te, prof_ti, prof_gradte, prof_gradti]
prlabels = ['te', 'ti', 'gradte', 'gradti']
plot_prof_all(profs, rho, 'aug6_r', prlabels)
# print(prof_gradte[65:73])
# print(prof_gradti[65:73].reshape(1,-1))
# gradgradti = deriv(prof_gradti, 0.1)
# core_gradti_maxloc = np.argmax(gradgradti[:-6])
# print('Steepest gradTi point is {} with gradTi={}'.format(core_gradti_maxloc, prof_gradti[core_gradti_maxloc]))
#
# two_camp_compare()

