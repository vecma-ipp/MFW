import ascii_cpo
import pylab
import argparse
import subprocess

parser = argparse.ArgumentParser()
parser.add_argument("path", help="Path to the directory with CPO data")
parser.add_argument("step", help="Step number (four digits)")
parser.add_argument("-te", "--Te", help="Shows Te", action="store_true")
parser.add_argument("-ti", "--Ti", help="Shows Ti", action="store_true")
parser.add_argument("--title", help="Plot title")
parser.add_argument("--out", help="Output filename")
parser.add_argument("--ext", help="Extension for output file")
parser.add_argument("--endstep", help="Last step for animation (four digits)")
args = parser.parse_args()



def gen_movie(args,field):
    movname = field+'_'+args.out+'_evo:'+args.step+'-'+args.endstep+'.avi'
    command = ('mencoder',
               'mf://'+field+'*.png',
               '-mf',
               'type=png:w=800:h=600:fps=10',
               '-ovc',
               'lavc',
               '-lavcopts',
               'vcodec=mpeg4',
               '-oac',
               'copy',
               '-o',
               movname)
    print "\nabout to execute:\n%s\n" % ' '.join(command)
    subprocess.check_call(command)
    print "\n The movie was written to "+movname
    print "\n You may want to delete "+field+"_"+args.out+"*.png now.\n\n"



def plot_dv(args,step,ext='pdf',show=True):
    if args.title == None:
        title = "Static=0-20, Lim=0.2/0.09, Tau=0.01, Step=99"
    else:
        title = args.title

    sstep = str(step).zfill(4)

    corep = ascii_cpo.read(args.path+"/ets_coreprof_"+sstep+".cpo", "coreprof")
    coretw = ascii_cpo.read(args.path+"/ets_coretransp-work_in_"+sstep+".cpo", "coretransp")
    coretdv = ascii_cpo.read(args.path+"/imp4dv_coretransp_"+sstep+".cpo", "coretransp")
    coretgem = ascii_cpo.read(args.path+"/gem_coretransp_"+sstep+".cpo", "coretransp")

    fulltitle = title+", Step="+sstep

    if args.Te:
        f, (ax1, ax2) = pylab.subplots(2, figsize=(8,12), sharex=True, sharey=False)
        ax1.set_title(fulltitle)
        ax1.plot(corep.rho_tor_norm,coretw.values[0].te_transp.diff_eff,'b',label="used in ETS")
        ax1.plot(coretdv.values[0].rho_tor_norm,coretdv.values[0].te_transp.diff_eff,'bo',label="IMP4dv coefs")
        ax1.plot(coretgem.values[0].rho_tor_norm,coretgem.values[0].te_transp.diff_eff,'g^',label="GEM coefs")
        ax1.set_ylabel("Te/Diff_eff")
        ax1.legend()
        ax2.plot(corep.rho_tor_norm,coretw.values[0].te_transp.vconv_eff,'b',label="used in ETS")
        ax2.plot(coretdv.values[0].rho_tor_norm,coretdv.values[0].te_transp.vconv_eff,'bo',label="IMP4dv coefs")
        ax2.plot(coretgem.values[0].rho_tor_norm,coretgem.values[0].te_transp.vconv_eff,'g^',label="GEM coefs")
        ax2.set_xlabel("rho_tor_norm")
        ax2.set_ylabel("Te/Vconv_eff")
        f.subplots_adjust(hspace=0)
        #pylab.tight_layout()
        if show:
            pylab.show()
        if args.out != None:
            f.savefig("Te_"+sstep+"_"+args.out+"."+ext)

    if args.Ti:
        f, (ax1, ax2) = pylab.subplots(2, figsize=(8,12), sharex=True, sharey=False)
        ax1.set_title(fulltitle)
        ax1.plot(corep.rho_tor_norm,coretw.values[0].ti_transp.diff_eff,'b',label="used in ETS")
        ax1.plot(coretdv.values[0].rho_tor_norm,coretdv.values[0].ti_transp.diff_eff,'bo',label="IMP4dv coefs")
        ax1.plot(coretgem.values[0].rho_tor_norm,coretgem.values[0].ti_transp.diff_eff,'g^',label="GEM coefs")
        ax1.set_ylabel("Ti/Diff_eff")
        ax1.legend()
        ax2.plot(corep.rho_tor_norm,coretw.values[0].ti_transp.vconv_eff,'b',label="used in ETS")
        ax2.plot(coretdv.values[0].rho_tor_norm,coretdv.values[0].ti_transp.vconv_eff,'bo',label="IMP4dv coefs")
        ax2.plot(coretgem.values[0].rho_tor_norm,coretgem.values[0].ti_transp.vconv_eff,'g^',label="GEM coefs")
        ax2.set_xlabel("rho_tor_norm")
        ax2.set_ylabel("Ti/Vconv_eff")
        f.subplots_adjust(hspace=0)
        #pylab.tight_layout()
        if show:
            pylab.show()
        if args.out != None:
            f.savefig("Ti_"+sstep+"_"+args.out+"."+ext)



if args.endstep != None:
    for i in range(int(args.step),int(args.endstep)+1,1):
        plot_dv(args,i,ext='png',show=False)

    if args.Te:
        gen_movie(args,"Te")
    if args.Ti:
        gen_movie(args,"Ti")

else:
    if args.ext != None:
        plot_dv(args,int(args.step),ext=args.ext)
    else:
        plot_dv(args,int(args.step))



