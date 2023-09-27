from ascii_cpo import read, write, read_fstream
import ual
import base


test_file_name = "../../workflows/AUG_28906_6/ets_coreprof_in.cpo"
profiles_cpo_obj_test = read(test_file_name, "coreprof")

write(profiles_cpo_obj_test, "../../workflows/AUG_28906_6/ets_coreprof_in_test.cpo", "coreprof")

