import sys

#muscle3path='../../muscle3/'
#muscle3path='~/code/MFW/muscle3/'
#muscle3path='/u/yyudin/code/MFW/muscle3/'
muscle3path='/u/yyudin/code/MFW/muscle3/src/'
sys.path.append(muscle3path)

#from read_profs import compare_states
from muscle_utils.utils import compare_states

if __name__ == "__main__":

    coreprof_1 = sys.argv[1] if len(sys.argv)>1 else 'ets_coreprof_in.cpo'
    coreprof_2 = sys.argv[2] if len(sys.argv)>2 else 'ets_coreprof_out.cpo'

    state_1 = {'coreprof': coreprof_1}
    state_2 = {'coreprof': coreprof_2}

    d = compare_states(state_1, state_2)

    print(f" !> distance between {coreprof_1} and {coreprof_2} is : {d}")

