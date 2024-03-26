import pylab
import ascii_cpo

def plot_profs(corep_list, label_list, plotfilename):

    pylab.figure(figsize=(7,7))

    #pylab.suptitle("Initial profiles for JET#92436/270")
    #pylab.suptitle("Initial profiles for AUG#28906/6")
    #pylab.suptitle("Initial profiles for AUG#28906/6/restart(8ft)")

    pylab.suptitle("Final profiles for MFW simulation of AUG#28906/6/")

    # pylab.subplot(321)
    # for corep,label in zip(corep_list,label_list):
    #     pylab.plot(corep.rho_tor_norm,corep.profiles1d.q.value, label=label, alpha=0.5)
    # pylab.yscale('symlog')
    # #pylab.xlabel("rho_tor_norm")
    # pylab.xlabel(f"$\\rho_{{tor}}^{{norm}}$")
    # pylab.ylabel(f"$q$")
    # pylab.legend(loc='best')

    # pylab.subplot(322)
    # for corep,label in zip(corep_list,label_list):
    #     pylab.plot(corep.rho_tor_norm,corep.profiles1d.pr_th.value, label=label, alpha=0.5)
    # #pylab.xlabel("rho_tor_norm")
    # pylab.xlabel(f"$\\rho_{{tor}}^{{norm}}$")
    # pylab.ylabel(f"$pr_{{th}}$")
    # pylab.legend(loc='best')

    # pylab.subplot(322)
    # for corep,label in zip(corep_list,label_list):
    #     pylab.plot(corep.rho_tor_norm,corep.profiles1d.gm3.value, label=label, alpha=0.5)
    # pylab.yscale('symlog')
    # pylab.xlabel(f"$\\rho_{{tor}}^{{norm}}$")
    # pylab.ylabel(f"$pr_{{th}}$")
    # pylab.legend(loc='best')

    #scale = 'symlog'
    scale = 'linear'

    pylab.subplot(321)
    for corep,label in zip(corep_list,label_list):
        pylab.plot(corep.rho_tor_norm, corep.te.value, label=label, alpha=0.5)
    pylab.yscale('linear')
    pylab.grid()
    #pylab.xlabel("rho_tor_norm")
    pylab.xlabel(f"$\\rho_{{tor}}^{{norm}}$")
    pylab.ylabel(f"$T_{{e}}, eV$")
    pylab.legend(loc='best')

    pylab.subplot(322)
    for corep,label in zip(corep_list,label_list):
        pylab.plot(corep.rho_tor_norm, corep.ti.value, label=label, alpha=0.5)
    pylab.yscale(scale)
    pylab.grid()
    #pylab.xlabel("rho_tor_norm")
    pylab.xlabel(f"$\\rho_{{tor}}^{{norm}}$")
    pylab.ylabel(f"$T_{{i}}, eV$")
    pylab.legend(loc='best')

    pylab.subplot(323)
    for corep,label in zip(corep_list,label_list):
        pylab.plot(corep.rho_tor_norm, corep.te.ddrho, label=label, alpha=0.5)
    pylab.yscale(scale)
    pylab.grid()
    #pylab.xlabel("rho_tor_norm")
    pylab.xlabel(f"$\\rho_{{tor}}^{{norm}}$")
    pylab.ylabel(f"$\\nabla T_{{e}}, eV/m$")
    pylab.legend(loc='best')

    pylab.subplot(324)
    for corep,label in zip(corep_list,label_list):
        pylab.plot(corep.rho_tor_norm,corep.ti.ddrho, label=label, alpha=0.5)
    pylab.yscale(scale)
    pylab.grid()
    #pylab.xlabel("rho_tor_norm")
    pylab.xlabel(f"$\\rho_{{tor}}^{{norm}}$")
    pylab.ylabel(f"$\\nabla T_{{i}}$, eV/m")
    pylab.legend(loc='best')

    pylab.subplot(325)
    for corep,label in zip(corep_list,label_list):
        pylab.plot(corep.rho_tor_norm,corep.ne.value, label=label, alpha=0.5)
    pylab.yscale(scale)
    pylab.grid()
    #pylab.xlabel("rho_tor_norm")
    pylab.xlabel(f"$\\rho_{{tor}}^{{norm}}$")
    pylab.ylabel(f"$n_{{e}}$")
    pylab.legend(loc='best')

    pylab.subplot(326)
    for corep,label in zip(corep_list,label_list):
        pylab.plot(corep.rho_tor_norm,corep.ni.value, label=label, alpha=0.5)
    pylab.yscale(scale)
    pylab.grid()
    pylab.tick_params(axis='y')
    #pylab.xlabel("rho_tor_norm")
    pylab.xlabel(f"$\\rho_{{tor}}^{{norm}}$")
    pylab.ylabel(f"$n_{{i}}$")
    pylab.legend(loc='best')

    pylab.tight_layout()
    #pylab.show(block=True)

    pylab.savefig(plotfilename)

    return 0

if __name__ == "__main__":

    #corep = ascii_cpo.read("ets_coreprof_in.cpo","coreprof")
    #corep = ascii_cpo.read("gem0_coreprof_in_2.cpo","coreprof")
    #corep = ascii_cpo.read("gem_resume_coreprof_in.cpo","coreprof")
    #corep = ascii_cpo.read("ets_coreprof_in_gem0_20231023.cpo", "coreprof")
    #corep = ascii_cpo.read("gem_coreprof_aug28906_6rest_in.cpo", "coreprof")

    # label_list = ['surrogate', 'GEM0']
    # cpo_filename_list = ['ets_coreprof_out_surr_wf_20240216.cpo', 'ets_coreprof_out_gem0_wf_20240216.cpo']
    # corep_list = [ascii_cpo.read(cpo_filename, "coreprof") for cpo_filename in cpo_filename_list]
    # plotfilename='prof_gem0_20240216.pdf'

    label_list = ['28906_6_restart']
    cpo_filename_list = ['ets_coreprof_out_surr_wf_20240216.cpo']
    corep_list = [ascii_cpo.read(cpo_filename, "coreprof") for cpo_filename in cpo_filename_list]
    plotfilename='prof_gem0_20240426.pdf'

    # TODO get these from sys.argv !

    plot_profs(corep_list, label_list, plotfilename=plotfilename)
