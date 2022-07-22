from basicda.da_utils import plot_sobols_pie, read_sobols_from_logs

import numpy as np
#import glob
#import ast


#test_line = "{'te.value': array([0.30395316]), 'ti.value': array([0.0195003]), 'te.ddrho': array([0.0445635]), 'ti.ddrho': array([0.63198303])}"
#test_line = "{'te.value': 0.30395316, 'ti.value': 0.0195003, 'te.ddrho': 0.0445635, 'ti.ddrho': 0.63198303}"
#test_dict = ast.literal_eval(test_line)


labels=['te_val', 'ti_val', 'te_grad', 'ti_grad']

aa = read_sobols_from_logs(labels=labels)

a_avg = np.array(aa).mean(axis=0)

print(len(aa))

plot_sobols_pie(a_avg, labels, '010622')

