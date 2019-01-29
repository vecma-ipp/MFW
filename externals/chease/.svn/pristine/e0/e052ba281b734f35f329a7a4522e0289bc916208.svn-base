function status=write_namelist_caxe(Niter,nsttp,q0, fname,cpress,Npsi_in,Npol_in);
% preliminary script to write namelist of CAXE

% check input
if nargin<1
  Niter=0;
end

if exist('Npsi_in')
  Npsi = Npsi_in
else
  switch Niter
   case 0
    Npsi=64;
   case 1
    Npsi=128;
   otherwise
    error('write_namelist_caxe: Wrong Niter!')
  end 
end
if exist('Npol_in')
  Npol = Npol_in
else
  switch Niter
   case 0
    Npol=64;
   case 1
    Npol=128;
   otherwise
    error('write_namelist_caxe: Wrong Niter!')
  end 
end

switch nsttp
    case 1
        ifc=0;
    case 2
        ifc=1;
    case 3
        ifc=3;
end

if ~exist('cpress') || isempty(cpress)
  cpress=1.0;
end

% write namelist
fid=fopen(fname,'w');

fprintf(fid,'e14463.eq\n');
fprintf(fid,'dummy\n');
fprintf(fid,'0        IREQ !0 no read-standard init. guess, 1 read eq.\n');
fprintf(fid,'%d      NA11 !number of magnetic surfaces\n',Npsi);
fprintf(fid,'%d      NT1  !number of poloidal points\n',Npol);
fprintf(fid,'0         NA2  !dummy\n');
fprintf(fid,'0.00,0.0,  0.000,  1.0,.0,-.00 !RM(default if 0),ZM, dummy, EK1,DK1,SK1\n');
fprintf(fid,'0.000,0.0,  0.33333, 1.0,.0,-.00 !RK(read if 0),ZK,    AK,    EK2,DK2,SK2\n');
fprintf(fid,'0         IPA1 !flux mesh: 0 - s, 1 - psi\n');
fprintf(fid,'0         NPA1 !number of mesh packing points\n');
fprintf(fid,'0         NPT1 !packing at the boundary\n');
fprintf(fid,'0         IPA2 !dummy\n');
fprintf(fid,'0         NPA2 !dummy\n');
fprintf(fid,'0         NPT2 !dummy\n');
fprintf(fid,'2.000     ROM  !CG solver    : preconditioner choice =2.0(ILU), =other(D)\n');
fprintf(fid,'100       ITMAX!CG solver    : max number of iterations, ITMAX=ITMAX*5 if IREQ=0\n');
fprintf(fid,'1.E-6     EPS  !CG solver    : accuracy in norm(residual)/norm(r.h.s.)\n');
fprintf(fid,'%d,-3     IFC,ICU !IFC=0,1,2 - given ffprime,i*,jb  IFC=0&ICU=1,2 - computed i*,jb\n', ifc);
fprintf(fid,'0.,2.0,1. CF1, PFE11, PFE12 !params for ffprime or i* or jb\n');
fprintf(fid,'0.,2.,1.  CP1, PPE11, PPE12 !params for pprime! read profiles if CF1+CF2+CP1+CP2=0\n');
fprintf(fid,'0.,0.,1.  CF2, PFE21, PFE22 !PFE21=1. for eq. profiles, 0. - from dcx1ib.pro\n');
fprintf(fid,'0.,1.,0.  CP2, PPE21, PPE22 !if PFE21=1. then PPE21 scales pprime\n');
% $$$ fprintf(fid,'0.,1.,1.  CF2, PFE21, PFE22 !PFE21=1. for eq. profiles, 0. - from dcx1ib.pro\n');
% $$$ fprintf(fid,'0.,%6.5f,0.  CP2, PPE21, PPE22 !if PFE21=1. then PPE21 scales pprime\n',cpress);
fprintf(fid,'0	IQAE !0 - for qaxis, 1 - for qbound\n');
fprintf(fid,'%6.5f		QAE  !q value\n',q0);
fprintf(fid,'2        KPA  !diff. scheme parameters\n');
fprintf(fid,'2        KPB\n');
fprintf(fid,'1        KPG\n');
fprintf(fid,'400      IGLOB !max number of iterations\n');
fprintf(fid,'10      IGLOUT !output frequency\n');
fprintf(fid,'1.E-5   EPSLEV !accuracy in level lines\n');
fprintf(fid,'1.E-5   EPSRO  !accuracy in geometry change\n');
fprintf(fid,'2       ISKIP ! make calculations of ffp from current at every ISKIP iter.\n');
fprintf(fid,'0.5,0.5 ruw,raxwm!weights for Picard iterations\n');

status=fclose(fid);
