err = [np.array(mid[1])-np.array(lo[1]), np.array(up[1]) np.array(mid[1])]                                                                                    

plt.errorbar(rho, mean, yerr=err,  fmt='.', color='b', ecolor='lightgray', elinewidth=2, capsize=0, label="Iterp. Data") 
