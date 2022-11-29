from basicda.da_utils import plot_sobols_pie, read_sobols_from_logs

import numpy as np
#import glob
#import ast


#test_line = "{'te.value': array([0.30395316]), 'ti.value': array([0.0195003]), 'te.ddrho': array([0.0445635]), 'ti.ddrho': array([0.63198303])}"
#test_line = "{'te.value': 0.30395316, 'ti.value': 0.0195003, 'te.ddrho': 0.0445635, 'ti.ddrho': 0.63198303}"
#test_dict = ast.literal_eval(test_line)

res_akgbbn1a_9 = {'te.value': 0.00031439,
                  'ti.value': 0.94327257,
                  'te.ddrho': 0.00171975,
                  'ti.ddrho': 0.03674999}

sobols_first_sum = sum([v for k,v in res_akgbbn1a_9.items()])
sobols_higher_sum = 1. - sobols_first_sum

res_akgbbn1a_9_exp = res_akgbbn1a_9.copy()
res_akgbbn1a_9_exp['S.higher'] = sobols_higher_sum

res_akgbbn1a_9_tot = {'te.value': 0.00101375,
                      'ti.value': 0.96069695,
                      'te.ddrho': 0.00283654,
                      'ti.ddrho': 0.05339608}

#labels=['te_val', 'ti_val', 'te_grad', 'ti_grad']
labels=['te.value', 'ti.value', 'te.ddrho', 'ti.ddrho']
labels_exp=[k for k,v in res_akgbbn1a_9_exp.items()]

#aa = read_sobols_from_logs(labels=labels)
#a_avg = np.array(aa).mean(axis=0)
#print(len(aa))

a_avg = [v for k,v in res_akgbbn1a_9_exp.items()]

#plot_sobols_pie(a_avg, labels, '090922')
plot_sobols_pie(a_avg, labels_exp, '181122')

