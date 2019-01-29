import numpy                                                                           
import subprocess
import multiprocessing

tclog = False

def valcoef6plot(t,i):
    suptitle("shot {0} run {1}, GEM ".format(shot,run)+
             r"$\delta t={0}$".format(delta_t))

    print 'plot #'+str(i)

    ##print 'subplot te prof'
    subplot(321)
    axis([amin,amax,minval[0],maxval[0]])
    plot(corep[t].rho_tor,corep[t].te.value,label="t=%0.2f"%corep[t].time)
    title('te value')

    ##print 'subplot te coef'
    subplot(322)
    axis([amin,amax,minval[1],maxval[1]])
    if tclog:
        l = semilogy(corep[t].rho_tor, corep[t].te.transp_coef.diff,label="t=%0.2f"%corep[t].time)
    else:
        l = plot(corep[t].rho_tor, corep[t].te.transp_coef.diff,label="t=%0.2f"%corep[t].time)
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].te_transp.diff_eff,(l.pop()).get_color()+'o')
    title('te transp coef')

    ##print 'subplot ti prof'
    subplot(323)
    axis([amin,amax,minval[2],maxval[2]])
    plot(corep[t].rho_tor,corep[t].ti.value[:,0],label="t=%0.2f"%corep[t].time)
    title('ti value')

    ##print 'subplot ti coef'
    subplot(324)
    axis([amin,amax,minval[3],maxval[3]])
    if tclog:
        l = semilogy(corep[t].rho_tor, corep[t].ti.transp_coef.diff[:,0],label="t=%0.2f"%corep[t].time)
    else:
        l = plot(corep[t].rho_tor, corep[t].ti.transp_coef.diff[:,0],label="t=%0.2f"%corep[t].time)
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].ti_transp.diff_eff[:,0],(l[0].get_color()+'o')) 
#(l.pop()).get_color()+'o')
    title('ti transp coef')

    ##print 'subplot ni prof'
    subplot(325)
    axis([amin,amax,minval[4],maxval[4]])
    plot(corep[t].rho_tor,corep[t].ni.value,label="t=%0.2f"%corep[t].time)
    title('ni value')

    ##print 'subplot ni coef'
    subplot(326)
    axis([amin,amax,minval[5],maxval[5]])
    if tclog:
        l = semilogy(corep[t].rho_tor, corep[t].ni.transp_coef.diff[:,0],label="t=%0.2f"%corep[t].time)
    else:
        l = plot(corep[t].rho_tor, corep[t].ni.transp_coef.diff[:,0],label="t=%0.2f"%corep[t].time)
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].ni_transp.diff_eff[:,0,1],(l[0].get_color()+'o'))
    title('ni transp coef')

    ##print 'legend'
    figlegend((l[0],),(l[0].get_label(),),'upper left', bbox_to_anchor=(0.75,0.98))

    ##print 'savefig'
    filename = str('%04d' % i) + '.png'
    savefig(filename, dpi=100)

    ##print 'clf'
    clf()



#flux = numpy.zeros((tsize,8))
#for i in range(tsize-1):
#    for j in range(8):
#        flux[i,j] = coret[i].te_transp.flux[j]


if 'freq' in locals():
    if type(freq)==int:
        print "plot curves each "+str(freq)+" time steps"
    if type(freq)==list:
        print "plot curves for steps "+str(freq)
else:
    freq = 1

#figure(figsize=(10,14))

f = figure(figsize=(8,8))
#f.tight_layout()

maxval = [sys.float_info.min,sys.float_info.min,sys.float_info.min,sys.float_info.min,sys.float_info.min,sys.float_info.min]
minval = [sys.float_info.max,sys.float_info.max,sys.float_info.max,sys.float_info.max,sys.float_info.max,sys.float_info.max]


print 'tsize = '+str(tsize)

for i in range(tsize-1):
    t = 1+i
    maxval[0] = max(max(corep[t].te.value),maxval[0])
    minval[0] = min(min(corep[t].te.value),minval[0])
    maxval[2] = max(max(corep[t].ti.value[:,0]),maxval[2])
    minval[2] = min(min(corep[t].ti.value[:,0]),minval[2])
    maxval[4] = max(max(corep[t].ni.value),maxval[4])
    minval[4] = min(min(corep[t].ni.value),minval[4])    
    maxval[1] = max(max(corep[t].te.transp_coef.diff),maxval[1])
    minval[1] = min(min(corep[t].te.transp_coef.diff),minval[1])
    maxval[1] = max(max(coret[t-1].values[0].te_transp.diff_eff),maxval[1])
    minval[1] = min(min(coret[t-1].values[0].te_transp.diff_eff),minval[1])
    maxval[3] = max(max(corep[t].ti.transp_coef.diff[:,0]),maxval[3])
    minval[3] = min(min(corep[t].ti.transp_coef.diff[:,0]),minval[3])
    maxval[3] = max(max(coret[t-1].values[0].ti_transp.diff_eff[:,0]),maxval[3])
    minval[3] = min(min(coret[t-1].values[0].ti_transp.diff_eff[:,0]),minval[3])
    maxval[5] = max(max(corep[t].ni.transp_coef.diff[:,0]),maxval[5])
    minval[5] = min(min(corep[t].ni.transp_coef.diff[:,0]),minval[5])
    maxval[5] = max(max(coret[t-1].values[0].ni_transp.diff_eff[:,0,1]),maxval[5])
    minval[5] = min(min(coret[t-1].values[0].ni_transp.diff_eff[:,0,1]),minval[5])

print maxval
print minval

amin = min(corep[0].rho_tor)
amax = max(corep[0].rho_tor)


pool = multiprocessing.Pool()

for i in range(tsize-1):
    t = 1+i
    #print "plot #"+str(i)
    #valcoef6plot(t,i)
    pool.apply_async(valcoef6plot,[t,i])
    
pool.close()
pool.join()


#subplot(327)
#for i in range(8): 
    #plot(flux[0:tsize-1,i],label="flux "+str(i+1))
#    semilogy(flux[0:tsize-1,i],label="flux "+str(i))
#title('te transp flux')


command = ('mencoder',
           'mf://*.png',
           '-mf',
           'type=png:w=800:h=600:fps=10',
           '-ovc',
           'lavc',
           '-lavcopts',
           'vcodec=mpeg4',
           '-oac',
           'copy',
           '-o',
           'output.avi')

#os.spawnvp(os.P_WAIT, 'mencoder', command)

print "\n\nabout to execute:\n%s\n\n" % ' '.join(command)
subprocess.check_call(command)

print "\n\n The movie was written to 'output.avi'"

print "\n\n You may want to delete *.png now.\n\n"




