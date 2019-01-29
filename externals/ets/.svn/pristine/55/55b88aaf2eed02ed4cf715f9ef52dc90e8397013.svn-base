import ual
import numpy
import scipy
import scipy.interpolate
import matplotlib.pyplot as plt
import math

def psi_bz(shot, run):

    cpo = ual.itm(shot,run)
    cpo.open()
    cpo.equilibriumArray.get()

    nrho=len(cpo.equilibriumArray.array[0].profiles_1d.psi)
    psi_0=cpo.equilibriumArray.array[0].profiles_1d.psi[0]
    psi_a=cpo.equilibriumArray.array[0].profiles_1d.psi[nrho-1]
    r=cpo.equilibriumArray.array[0].coord_sys.position.r
    z=cpo.equilibriumArray.array[0].coord_sys.position.z
    n1=r.shape[0]
    n2=r.shape[1]
    R0=cpo.equilibriumArray.array[0].global_param.toroid_field.r0
    B0=cpo.equilibriumArray.array[0].global_param.toroid_field.b0
    psi=numpy.reshape(numpy.repeat(cpo.equilibriumArray.array[0].profiles_1d.psi,n2),[n1,n2])
    f_dia=numpy.reshape(numpy.repeat(cpo.equilibriumArray.array[0].profiles_1d.F_dia,n2),[n1,n2])
    
    rmin=r.min()-(r.max()-r.min())/20
    rmax=r.max()+(r.max()-r.min())/20
    zmin=z.min()-(z.max()-z.min())/20
    zmax=z.max()+(z.max()-z.min())/20
    dr=(rmax-rmin)/64
    dz=(zmax-zmin)/128

    R=numpy.arange(rmin,rmax+dr,dr); nR=len(R)
    Z=numpy.arange(zmin,zmax+dz,dz); nZ=len(Z)

    PSI = (scipy.interpolate.bisplev(R, Z, scipy.interpolate.bisplrep(r,z,psi,xb=rmin,xe=rmax,yb=zmin,ye=zmax,kx=3,ky=3,task=0,s=numpy.sqrt(n1*n2)/100)))
    F_DIA = (scipy.interpolate.bisplev(R, Z, scipy.interpolate.bisplrep(r,z,f_dia,xb=rmin,xe=rmax,yb=zmin,ye=zmax,kx=3,ky=3,task=0,s=numpy.sqrt(n1*n2)/10000)))
    # swapped signs for BR and BZ to match equal
    BR=-(scipy.interpolate.bisplev(R, Z, scipy.interpolate.bisplrep(r,z,psi,xb=rmin,xe=rmax,yb=zmin,ye=zmax,kx=3,ky=3,task=0,s=numpy.sqrt(n1*n2)/100), dy=1))/2/math.pi
    BZ=(scipy.interpolate.bisplev(R, Z, scipy.interpolate.bisplrep(r,z,psi,xb=rmin,xe=rmax,yb=zmin,ye=zmax,kx=3,ky=3,task=0,s=numpy.sqrt(n1*n2)/100), dx=1))/2/math.pi
    BPHI=F_DIA
    if (psi_a-psi_0) > 0 : sign=1
    else : sign=-1

    for i in range(len(R)) :
        for j in range(len(Z)) :
            if (PSI[i,j]*sign > psi_a*sign) : F_DIA[i,j]=R0*B0
            BR[i,j]=BR[i,j]/R[i]
            BZ[i,j]=BZ[i,j]/R[i]
            BPHI[i,j]=BPHI[i,j]/R[i]


    return [R, Z, PSI, BR, BZ, BPHI]

width=297/25.4
height=210/25.4
plt.figure(1, figsize=(width,height))
plt.gcf().subplots_adjust(wspace=0.25, hspace=0.4)

cpo = ual.itm(78092,2)
cpo.open()
cpo.equilibriumArray.get()
eq1 = cpo.equilibriumArray.array[0]
cpo.close()

cpo = ual.itm(78092,10003)
cpo.open()
cpo.equilibriumArray.get()
eq2 = cpo.equilibriumArray.array[0]
cpo.close()

[R, Z, PSI, BR, BZ, BPHI] = psi_bz(78092, 3)

NR1=32
NR2=36
NZ1=32
NZ2=57

print eq1.profiles_2d.grid.dim1
print eq2.profiles_2d.grid.dim1
print eq1.profiles_2d.grid.dim2
print eq2.profiles_2d.grid.dim2

print eq1.profiles_2d.grid.dim1[NR1], eq2.profiles_2d.grid.dim1[NR2], R[NR2]
print eq1.profiles_2d.grid.dim2[NZ1], eq2.profiles_2d.grid.dim2[NZ2], Z[NZ2]

i=0
while eq1.profiles_2d.psi_grid[NR1,i] > eq1.global_param.psi_bound : i = i+1
z1L=eq1.profiles_2d.grid.dim2[i]
i=len(eq1.profiles_2d.grid.dim2)-1
while eq1.profiles_2d.psi_grid[NR1,i] > eq1.global_param.psi_bound : i = i-1
z1R=eq1.profiles_2d.grid.dim2[i]

i=0
while eq1.profiles_2d.psi_grid[i,NZ1] > eq1.global_param.psi_bound : i = i+1
r1L=eq1.profiles_2d.grid.dim1[i]
i=len(eq1.profiles_2d.grid.dim1)-1
while eq1.profiles_2d.psi_grid[i,NZ1] > eq1.global_param.psi_bound : i = i-1
r1R=eq1.profiles_2d.grid.dim1[i]

#print r1L, r1R, z1L, z1R

i=0
while eq2.profiles_2d.psi_grid[NR2,i] > eq2.global_param.psi_bound : i = i+1
z2L=eq2.profiles_2d.grid.dim2[i]
i=len(eq2.profiles_2d.grid.dim2)-1
while eq2.profiles_2d.psi_grid[NR2,i] > eq2.global_param.psi_bound : i = i-1
z2R=eq2.profiles_2d.grid.dim2[i]

i=0
while eq2.profiles_2d.psi_grid[i,NZ2] > eq2.global_param.psi_bound : i = i+1
r2L=eq2.profiles_2d.grid.dim1[i]
i=len(eq2.profiles_2d.grid.dim1)-1
while eq2.profiles_2d.psi_grid[i,NZ2] > eq2.global_param.psi_bound : i = i-1
r2R=eq2.profiles_2d.grid.dim1[i]

#print r2L, r2R, z2L, z2R

rL=min(r1L,r2L)
rR=max(r1R,r2R)
zL=min(z1L,z2L)
zR=max(z1R,z2R)

#print rL, rR, zL, zR

x=numpy.zeros([2]); y=numpy.zeros([2])

plt.suptitle('Comparison of original psi, br, bz and those coming from augmentation', fontsize=18)

plt.subplot(221)
plt.plot(eq1.profiles_2d.grid.dim2,eq1.profiles_2d.psi_grid[NR1,:])
plt.plot(eq2.profiles_2d.grid.dim2,eq2.profiles_2d.psi_grid[NR2,:])
plt.plot(Z,PSI[NR2,:])
y[0]=min(min(eq1.profiles_2d.psi_grid[NR1,:]),min(eq2.profiles_2d.psi_grid[NR2,:]))
y[1]=max(max(eq1.profiles_2d.psi_grid[NR1,:]),max(eq2.profiles_2d.psi_grid[NR2,:]))
x[0]=zL; x[1]=zL; plt.plot(x,y,'k')
x[0]=zR; x[1]=zR; plt.plot(x,y,'k')
plt.xlabel('Z')
plt.ylabel('PSI')

plt.subplot(222)
plt.plot(eq1.profiles_2d.grid.dim1,eq1.profiles_2d.psi_grid[:,NZ1])
plt.plot(eq2.profiles_2d.grid.dim1,eq2.profiles_2d.psi_grid[:,NZ2])
plt.plot(R,PSI[:,NZ2])
y[0]=min(min(eq1.profiles_2d.psi_grid[:,NZ1]),min(eq2.profiles_2d.psi_grid[:,NZ2]))
y[1]=max(max(eq1.profiles_2d.psi_grid[:,NZ1]),max(eq2.profiles_2d.psi_grid[:,NZ2]))
x[0]=rL; x[1]=rL; plt.plot(x,y,'k')
x[0]=rR; x[1]=rR; plt.plot(x,y,'k')
plt.xlabel('R')
plt.ylabel('PSI')

plt.subplot(223)
plt.plot(eq1.profiles_2d.grid.dim2,eq1.profiles_2d.br[NR1,:])
plt.plot(eq2.profiles_2d.grid.dim2,eq2.profiles_2d.br[NR2,:])
plt.plot(Z,BR[NR2,:])
y[0]=min(min(eq1.profiles_2d.br[NR1,:]),min(eq2.profiles_2d.br[NR2,:]))
y[1]=max(max(eq1.profiles_2d.br[NR1,:]),max(eq2.profiles_2d.br[NR2,:]))
x[0]=zL; x[1]=zL; plt.plot(x,y,'k')
x[0]=zR; x[1]=zR; plt.plot(x,y,'k')
plt.xlabel('Z')
plt.ylabel('BR')

plt.subplot(224)
plt.plot(eq1.profiles_2d.grid.dim1,eq1.profiles_2d.bz[:,NZ1])
plt.plot(eq2.profiles_2d.grid.dim1,eq2.profiles_2d.bz[:,NZ2])
plt.plot(R,BZ[:,NZ2])
y[0]=min(min(eq1.profiles_2d.bz[:,NZ1]),min(eq2.profiles_2d.bz[:,NZ2]))
y[1]=max(max(eq1.profiles_2d.bz[:,NZ1]),max(eq2.profiles_2d.bz[:,NZ2]))
x[0]=rL; x[1]=rL; plt.plot(x,y,'k')
x[0]=rR; x[1]=rR; plt.plot(x,y,'k')
plt.xlabel('R')
plt.ylabel('BZ')


plt.savefig('check_augmentation.ps',orientation='landscape',papertype='a4')
plt.savefig('check_augmentation.png')
