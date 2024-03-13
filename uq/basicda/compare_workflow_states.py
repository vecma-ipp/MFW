import sys

#muscle3path='../../muscle3/'
#muscle3path='~/code/MFW/muscle3/'
#muscle3path='/u/yyudin/code/MFW/muscle3/'
muscle3path='/u/yyudin/code/MFW/muscle3/src/'
sys.path.append(muscle3path)

#from read_profs import compare_states
from muscle_utils.utils import compare_states

def comp_states_fromcli(coreprof_1, coreprof_2, plot_diff=True):
      
    crit = 'srRMSE'

    yscale_plot ='log'

    # Prepare a pair of core profile states
    state_1 = {'coreprof': coreprof_1}
    state_2 = {'coreprof': coreprof_2}

    cpo_types = ['coreprof']

    # Define if equilibrium part of state is used
    use_equilibrium = True if len(sys.argv) > 3 else False
    if use_equilibrium:
            equilibrium_1 = sys.argv[3] if len(sys.argv)>3 else 'ets_equilibrium_in.cpo'
            equilibrium_2 = sys.argv[4] if len(sys.argv)>4 else 'ets_equilibrium_out.cpo'
            state_1['equilibrium'] = equilibrium_1
            state_2['equilibrium'] = equilibrium_2
            cpo_types.append('equilibrium')

            title_plot = str(sys.argv[5]) if len(sys.argv)>5 else None  

    d = compare_states(state_1, state_2, cpo_types=cpo_types, plot_diff=plot_diff, yscale_plot=yscale_plot, title_plot=title_plot)

    print(f" !> distance {crit} between {coreprof_1} and {coreprof_2} is: \n{d:.12f}\n")

    return 0

if __name__ == "__main__":

        plot_diff = True
    
        coreprof_1 = sys.argv[1] if len(sys.argv)>1 else 'ets_coreprof_in.cpo'
        coreprof_2 = sys.argv[2] if len(sys.argv)>2 else 'ets_coreprof_out.cpo'
            
        comp_states_fromcli(coreprof_1, coreprof_2, plot_diff=plot_diff,)
