import numpy
import subprocess
import multiprocessing

tclog = False
tflog = False

def valcoef6plot(t,i):
    suptitle("shot {0} run {1}".format(shot,run))

    print 'plot #'+str(i)
    
    subplot(331)
    axis([amin,amax,minval[0],maxval[0]])
    l = plot(corep[t].rho_tor,corep[t].te.value,label="t=%0.2f"%corep[t].time)
    title('Te')

    figlegend((l[0],),(l[0].get_label(),),'upper left', bbox_to_anchor=(0.75,0.98))

    subplot(332)
    axis([amin,amax,minval[1],maxval[1]])
    if tclog:
        l = semilogy(corep[t].rho_tor, corep[t].te.transp_coef.diff,label="diff")
    else:
        l = plot(corep[t].rho_tor, corep[t].te.transp_coef.diff,label="diff")
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].te_transp.diff_eff,(l.pop()).get_color()+'o')
    if tclog:
        l = semilogy(corep[t].rho_tor, corep[t].te.transp_coef.vconv,label="vconv")
    else:
        l = plot(corep[t].rho_tor, corep[t].te.transp_coef.vconv,label="vconv")
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].te_transp.vconv_eff,(l.pop()).get_color()+'o')
    title('Te:coef')
#    legend(loc='best')

    a=subplot(333)
    axis([amin,amax,minval[2],maxval[2]])
#     if tflog:
#         l = semilogy(corep[t].rho_tor,corep[t].te.transp_coef.flux)#,label="t=%0.2f"%corep[t].time)
#     else:
#         l = plot(corep[t].rho_tor,corep[t].te.transp_coef.flux)#,label="t=%0.2f"%corep[t].time)
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].te_transp.flux,'o-')
    a.yaxis.tick_right()
    title('Te:flux')

    subplot(334)
    axis([amin,amax,minval[3],maxval[3]])
    plot(corep[t].rho_tor,corep[t].ti.value[:,0])#,label="t=%0.2f"%corep[t].time)
    title('Ti')

    subplot(335)
    axis([amin,amax,minval[4],maxval[4]])
    if tclog:
        l = semilogy(corep[t].rho_tor, corep[t].ti.transp_coef.diff[:,0],label="diff")
    else:
        l = plot(corep[t].rho_tor, corep[t].ti.transp_coef.diff[:,0],label="diff")
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].ti_transp.diff_eff[:,0],(l.pop()).get_color()+'o')
    if tclog:
        l = semilogy(corep[t].rho_tor, corep[t].ti.transp_coef.vconv[:,0],label="vconv")
    else:
        l = plot(corep[t].rho_tor, corep[t].ti.transp_coef.vconv[:,0],label="vconv")
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].ti_transp.vconv_eff[:,0],(l.pop()).get_color()+'o')
    title('Ti:coef')
#    legend(loc='best')

    a=subplot(336)
    axis([amin,amax,minval[5],maxval[5]])
#     if tflog:
#         l = semilogy(corep[t].rho_tor,corep[t].ti.transp_coef.flux[:,0])#,label="t=%0.2f"%corep[t].time)
#     else:
#         l = plot(corep[t].rho_tor,corep[t].ti.transp_coef.flux[:,0])#,label="t=%0.2f"%corep[t].time)
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].ti_transp.flux[:,0],'o-')
    a.yaxis.tick_right()
    title('Ti:flux')

    subplot(337)
    axis([amin,amax,minval[6],maxval[6]])
    plot(corep[t].rho_tor,corep[t].ne.value)#,label="t=%0.2f"%corep[t].time)
    title('Ni (e in fact)')

    subplot(338)
    axis([amin,amax,minval[7],maxval[7]])
    if tclog:
        l = semilogy(corep[t].rho_tor, corep[t].ni.transp_coef.diff[:,0],label="diff")
    else:
        l = plot(corep[t].rho_tor, corep[t].ni.transp_coef.diff[:,0],label="diff")
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].ni_transp.diff_eff[:,0,1],(l[0].get_color()+'o'))
    if tclog:
        l = semilogy(corep[t].rho_tor, corep[t].ni.transp_coef.vconv[:,0],label="vconv")
    else:
        l = plot(corep[t].rho_tor, corep[t].ni.transp_coef.vconv[:,0],label="vconv")
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].ni_transp.vconv_eff[:,0,1],(l[0].get_color()+'o'))
    title('Ni:coef')
#    legend(loc='best')

    a=subplot(339)
    axis([amin,amax,minval[8],maxval[8]])
#     if tflog:
#         l = semilogy(corep[t].rho_tor,corep[t].ni.transp_coef.flux[:,0])#,label="t=%0.2f"%corep[t].time)
#     else:
#         l = plot(corep[t].rho_tor,corep[t].ni.transp_coef.flux[:,0])#,label="t=%0.2f"%corep[t].time)
    plot(coret[t-1].values[0].rho_tor, coret[t-1].values[0].ni_transp.flux[:,0],'o-')
    a.yaxis.tick_right()
    title('Ni:flux')

    #tight_layout()
    filename = str('%04d' % i) + '.png'
    savefig(filename, dpi=100)
    clf()

nft = size(coret[0].values[0].rho_tor)
print "number of fluxtubes = "+str(nft)

flux = numpy.zeros((tsize,nft))
for i in range(tsize-1):
    for j in range(nft):
        flux[i,j] = coret[i].values[0].te_transp.flux[j]


if 'freq' in locals():
    if type(freq)==int:
        print "plot curves each "+str(freq)+" time steps"
    if type(freq)==list:
        print "plot curves for steps "+str(freq)
else:
    freq = 1

#figure(figsize=(10,14))

f = figure(figsize=(12,8))

maxval = [sys.float_info.min,sys.float_info.min,sys.float_info.min,sys.float_info.min,sys.float_info.min,sys.float_info.min,sys.float_info.min,sys.float_info.min,sys.float_info.min]
minval = [sys.float_info.max,sys.float_info.max,sys.float_info.max,sys.float_info.max,sys.float_info.max,sys.float_info.max,sys.float_info.max,sys.float_info.max,sys.float_info.max]

for i in range(tsize-1):
    t = 1+i
    maxval[0] = max(max(corep[t].te.value),maxval[0])
    minval[0] = min(min(corep[t].te.value),minval[0])
    maxval[1] = max(max(corep[t].te.transp_coef.diff),maxval[1])
    minval[1] = min(min(corep[t].te.transp_coef.diff),minval[1])
    maxval[1] = max(max(coret[t-1].values[0].te_transp.diff_eff),maxval[1])
    minval[1] = min(min(coret[t-1].values[0].te_transp.diff_eff),minval[1])
    maxval[1] = max(max(coret[t-1].values[0].te_transp.vconv_eff),maxval[1])
    minval[1] = min(min(coret[t-1].values[0].te_transp.vconv_eff),minval[1])
    maxval[2] = max(max(coret[t-1].values[0].te_transp.flux),maxval[2])
    minval[2] = min(min(coret[t-1].values[0].te_transp.flux),minval[2])
    maxval[3] = max(max(corep[t].ti.value[:,0]),maxval[3])
    minval[3] = min(min(corep[t].ti.value[:,0]),minval[3])
    maxval[4] = max(max(corep[t].ti.transp_coef.diff[:,0]),maxval[4])
    minval[4] = min(min(corep[t].ti.transp_coef.diff[:,0]),minval[4])
    maxval[4] = max(max(coret[t-1].values[0].ti_transp.diff_eff[:,0]),maxval[4])
    minval[4] = min(min(coret[t-1].values[0].ti_transp.diff_eff[:,0]),minval[4])
    maxval[4] = max(max(coret[t-1].values[0].ti_transp.vconv_eff[:,0]),maxval[4])
    minval[4] = min(min(coret[t-1].values[0].ti_transp.vconv_eff[:,0]),minval[4])
    maxval[5] = max(max(coret[t-1].values[0].ti_transp.flux[:,0]),maxval[5])
    minval[5] = min(min(coret[t-1].values[0].ti_transp.flux[:,0]),minval[5])
    maxval[6] = max(max(corep[t].ne.value),maxval[6])
    minval[6] = min(min(corep[t].ne.value),minval[6])    
    maxval[7] = max(max(corep[t].ni.transp_coef.diff[:,0]),maxval[7])
    minval[7] = min(min(corep[t].ni.transp_coef.diff[:,0]),minval[7])
    maxval[7] = max(max(coret[t-1].values[0].ni_transp.diff_eff[:,0,1]),maxval[7])
    minval[7] = min(min(coret[t-1].values[0].ni_transp.diff_eff[:,0,1]),minval[7])
    maxval[7] = max(max(coret[t-1].values[0].ni_transp.vconv_eff[:,0,1]),maxval[7])
    minval[7] = min(min(coret[t-1].values[0].ni_transp.vconv_eff[:,0,1]),minval[7])
    maxval[8] = max(max(coret[t-1].values[0].ni_transp.flux[:,0]),maxval[8])
    minval[8] = min(min(coret[t-1].values[0].ni_transp.flux[:,0]),minval[8])

print maxval
print minval

amin = min(corep[0].rho_tor)
amax = max(corep[0].rho_tor)



pool = multiprocessing.Pool()

for i in range(tsize-1):
    t = 1+i
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




