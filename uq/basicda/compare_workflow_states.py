import sys

sys.path.append('../../muscle/')

from read_profs import compare_states

if __name__ == "__main__":

    coreprof_1 = sys.argv[1] if len(sys.argv)>1 else 'ets_coreprof_in.cpo'
    coreprof_2 = sys.argv[2] if len(sys.argv)>2 else 'ets_coreprof_out.cpo'

    state_1 = {'coreprof': coreprof_1}
    state_2 = {'coreprof': coreprof_2}

    d = compare_states(state_1, state_2)

    print(f" !> distance between two iterations is : {d}")

