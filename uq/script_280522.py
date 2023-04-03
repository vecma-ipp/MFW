# Script started on 28.05.2022.
# The purpose is to go over logs of several UQ campaigns with same function and similar input distributions
# And read out the corresponding Sobol index values


from basicda.da_utils import plot_sobols_pie, read_sobols_from_logs

import numpy as np
#import glob
#import ast


#test_line = "{'te.value': array([0.30395316]), 'ti.value': array([0.0195003]), 'te.ddrho': array([0.0445635]), 'ti.ddrho': array([0.63198303])}"
#test_line = "{'te.value': 0.30395316, 'ti.value': 0.0195003, 'te.ddrho': 0.0445635, 'ti.ddrho': 0.63198303}"
#test_dict = ast.literal_eval(test_line)

res_akgbbn1a_9_titr = {
                'te.ddrho': 0.00171975,
                'te.value': 0.00031439,
                'ti.ddrho': 0.03674999,
                'ti.value': 0.94327257,
                      }

sobols_first_sum = sum([v for k,v in res_akgbbn1a_9_titr.items()])
sobols_higher_sum = 1. - sobols_first_sum

res_akgbbn1a_9_titr_exp = res_akgbbn1a_9_titr.copy()
res_akgbbn1a_9_titr_exp['S.higher'] = sobols_higher_sum

res_akgbbn1a_9_titr_tot = {'te.value': 0.00101375,
                      'ti.value': 0.96069695,
                      'te.ddrho': 0.00283654,
                      'ti.ddrho': 0.05339608}

label_list = ["$\\nabla T_{{e}}$", "$T_{{e}}$", "$\\nabla T_{{i}}$", "$T_{{i}}$"]
#labels=['te_val', 'ti_val', 'te_grad', 'ti_grad']
labels=['te.value', 'ti.value', 'te.ddrho', 'ti.ddrho']
labels_exp=[k for k,v in res_akgbbn1a_9_titr_exp.items()]
labels_exp = label_list

#aa = read_sobols_from_logs(labels=labels)
#a_avg = np.array(aa).mean(axis=0)
#print(len(aa))

a_avg = [v for k,v in res_akgbbn1a_9_titr_exp.items()]

#plot_sobols_pie(a_avg, labels, '090922')
plot_sobols_pie(a_avg, labels_exp, '030423_lpl')

##### Addition 20.02.2023 ######

sobols_first = {'te_transp.flux': {'te.ddrho': 0.04331507,
                                     'te.value': 0.68019685,
                                     'ti.ddrho': 0.00031151,
                                     'ti.value': 0.16493948},
                  'ti_transp.flux': {'te.ddrho': 0.00171975,
                                     'te.value': 0.00031439,
                                     'ti.ddrho': 0.03674999,
                                     'ti.value': 0.94327257}}

sobols_total = {'te_transp.flux': {'te.ddrho': 0.06019673,
                                     'te.value': 0.77437712,
                                     'ti.ddrho': 0.00116995,
                                     'ti.value': 0.2754933},
                  'ti_transp.flux': {'te.ddrho': 0.00283654,
                                     'te.value': 0.00101375,
                                     'ti.ddrho': 0.05339608,
                                     'ti.value': 0.96069695}}

res_akgbbn1a_9_tetr_exp = sobols_first['te_transp.flux'].copy()
sobols_first_tetr_sum = sum([v for k,v in res_akgbbn1a_9_tetr_exp.items()])
res_akgbbn1a_9_tetr_exp['S.higher'] = 1. - sobols_first_tetr_sum
s_tetr =   [v for k,v in res_akgbbn1a_9_tetr_exp.items()]
labels_exp=[k for k,v in res_akgbbn1a_9_tetr_exp.items()]
labels_exp = label_list
plot_sobols_pie(s_tetr, labels_exp, 'tetr_030423_lpl')
