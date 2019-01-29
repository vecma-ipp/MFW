import ual
import numpy
import scipy
import scipy.interpolate
import matplotlib.pyplot as plt
import math

shot=71827
run=23
s=50

width=210/25.4
height=297/25.4
plt.figure(1, figsize=(width,height))
plt.gcf().subplots_adjust(wspace=0.25, hspace=0.4)

cpo = ual.itm(shot,run)
cpo.open()
cpo.equilibriumArray.get()

nrho=len(cpo.equilibriumArray.array[s].profiles_1d.psi)
psi_0=cpo.equilibriumArray.array[s].profiles_1d.psi[0]
psi_a=cpo.equilibriumArray.array[s].profiles_1d.psi[nrho-1]
r=cpo.equilibriumArray.array[s].coord_sys.position.r
z=cpo.equilibriumArray.array[s].coord_sys.position.z
n1=r.shape[0]
n2=r.shape[1]
R0=cpo.equilibriumArray.array[s].global_param.toroid_field.r0
B0=cpo.equilibriumArray.array[s].global_param.toroid_field.b0
psi=numpy.reshape(numpy.repeat(cpo.equilibriumArray.array[s].profiles_1d.psi,n2),[n1,n2])
f_dia=numpy.reshape(numpy.repeat(cpo.equilibriumArray.array[s].profiles_1d.F_dia,n2),[n1,n2])

rmin=r.min()-(r.max()-r.min())/20
rmax=r.max()+(r.max()-r.min())/20
zmin=z.min()-(z.max()-z.min())/20
zmax=z.max()+(z.max()-z.min())/20
dr=(rmax-rmin)/64
dz=(zmax-zmin)/128

R=numpy.arange(rmin,rmax+dr,dr); nR=len(R)
Z=numpy.arange(zmin,zmax+dz,dz); nZ=len(Z)

PSI = (scipy.interpolate.bisplev(R, Z, scipy.interpolate.bisplrep(r,z,psi,xb=rmin,xe=rmax,yb=zmin,ye=zmax,kx=3,ky=3,task=0,s=numpy.sqrt(n1*n2)/10)))
F_DIA = (scipy.interpolate.bisplev(R, Z, scipy.interpolate.bisplrep(r,z,f_dia,xb=rmin,xe=rmax,yb=zmin,ye=zmax,kx=3,ky=3,task=0,s=numpy.sqrt(n1*n2)/1000)))
# swapped signs for BR and BZ to match equal
BR=-(scipy.interpolate.bisplev(R, Z, scipy.interpolate.bisplrep(r,z,psi,xb=rmin,xe=rmax,yb=zmin,ye=zmax,kx=3,ky=3,task=0,s=numpy.sqrt(n1*n2)/10), dy=1))/2/math.pi
BZ=(scipy.interpolate.bisplev(R, Z, scipy.interpolate.bisplrep(r,z,psi,xb=rmin,xe=rmax,yb=zmin,ye=zmax,kx=3,ky=3,task=0,s=numpy.sqrt(n1*n2)/10), dx=1))/2/math.pi
BPHI=F_DIA
if (psi_a-psi_0) > 0 : sign=1
else : sign=-1

for i in range(len(R)) :
    for j in range(len(Z)) :
        if (PSI[i,j]*sign > psi_a*sign) : F_DIA[i,j]=R0*B0
        BR[i,j]=BR[i,j]/R[i]
        BZ[i,j]=BZ[i,j]/R[i]
        BPHI[i,j]=BPHI[i,j]/R[i]

plt.suptitle('%s/%s s=%s'%(shot,run,s), fontsize=20)

plt.subplot(221)
plt.axis('equal')
plt.xlabel('R')
plt.ylabel('Z')
plt.title('PSI')
plt.contourf(R,Z,PSI.transpose(),16)
plt.colorbar()
for i in range(n1) :
    plt.plot(numpy.append(r[i,:],r[i,0]),numpy.append(z[i,:],z[i,0]),'w')

plt.subplot(222)
plt.axis('equal')
plt.xlabel('R')
plt.ylabel('Z')
plt.title('BPHI')
plt.contourf(R,Z,BPHI.transpose(),16)
plt.colorbar()
for i in range(n1) :
    plt.plot(numpy.append(r[i,:],r[i,0]),numpy.append(z[i,:],z[i,0]),'w')

plt.subplot(223)
plt.axis('equal')
plt.xlabel('R')
plt.ylabel('Z')
plt.title('BR')
plt.contourf(R,Z,BR.transpose(),16)
plt.colorbar()
for i in range(n1) :
    plt.plot(numpy.append(r[i,:],r[i,0]),numpy.append(z[i,:],z[i,0]),'w')

plt.subplot(224)
plt.axis('equal')
plt.xlabel('R')
plt.ylabel('Z')
plt.title('BZ')
plt.contourf(R,Z,BZ.transpose(),16)
plt.colorbar()
for i in range(n1) :
    plt.plot(numpy.append(r[i,:],r[i,0]),numpy.append(z[i,:],z[i,0]),'w')

plt.savefig('psi_RZ.ps',orientation='portrait',papertype='a4')
