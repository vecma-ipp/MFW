import sys

#muscle3path='../../muscle3/'
#muscle3path='~/code/MFW/muscle3/'
#muscle3path='/u/yyudin/code/MFW/muscle3/'
muscle3path='/u/yyudin/code/MFW/muscle3/src/'
sys.path.append(muscle3path)

#from read_profs import compare_states
from muscle_utils.utils import compare_states

if __name__ == "__main__":
    
    crit = 'srRMSE'
    plot_diff = False

    # Prepare a pair of core profile states
    coreprof_1 = sys.argv[1] if len(sys.argv)>1 else 'ets_coreprof_in.cpo'
    coreprof_2 = sys.argv[2] if len(sys.argv)>2 else 'ets_coreprof_out.cpo'

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

    d = compare_states(state_1, state_2, cpo_types=cpo_types, plot_diff=plot_diff)

    print(f" !> distance {crit} between {coreprof_1} and {coreprof_2} is: {d:.5f}")

