function status=write_namelist_kinx(Niter,Ntoroidal,initial_guess,q0,fname,Npsi_in,Npol_in,IDW,CRW);
% preliminary script to write namelist of KINX

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
    error('write_namelist_kinx: Wrong Niter!')
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
    error('write_namelist_kinx: Wrong Niter!')
  end 
end

if ~exist('IDW')
  IDW = -1;
end
if ~exist('CRW')
  CRW = 1.0;
end

% write namelist (following the example namelist inputKINX2000
fid=fopen(fname,'w');

fprintf(fid,'e14463.eq\n');
fprintf(fid,'dummy\n');
fprintf(fid,'%d     NA11  !NUMBER OF FLUX INTERVALS\n', Npsi);
fprintf(fid,'%d     NT11  !NUMBER OF POLOIDAL INTERVALS\n',Npol);
fprintf(fid,'0   SNSCR !NUMBER OF SCRAPED SURFACES FROM READ EQ. (neg.-> EDGE, pos.-> CORE)\n');
fprintf(fid,'2       IPA1  !FLUX MESH SWITCH: -2 READ, 0 S=SQRT(PSI), 1 PSI, 2 MATCHED\n');
fprintf(fid,'0       NPA1  !number of cluster points\n');
fprintf(fid,'2       IPT1  !ARCLENGTH MESH SWITCH:-1 READ, 0 ARCLENGTH, 2 MATCHED\n');
fprintf(fid,'0       NPT1  !number of cluster points\n');
fprintf(fid,'0       IRES  !RESONANCES FIX SWITCH: 1 FIX, 0 NO FIX (OFF for N>5?)\n');
fprintf(fid,'0	IQAE  q 0 axis 1 boundary switch\n');
fprintf(fid,'%8.7f	QAE   Q AXIS OR BOUNDARY\n',q0);
fprintf(fid,'0	IBAL  compute ballooning\n');
fprintf(fid,'%d	WN  toroidal wave number\n',Ntoroidal);
fprintf(fid,'1.67    GA    !adiabata\n');
fprintf(fid,'0.      EQSC  !=1.(eq. normalization)\n');
fprintf(fid,'2       IOR   !=2\n');
fprintf(fid,'0       IZD   !=0\n');
fprintf(fid,'%d      IDW   DELTA DW SWITCH 0 FULL NORM, 1 DW NORM, -1 DW WITH RO=PSI (DW sufficiant for stab., FULL for eigenvalues)\n',IDW);
fprintf(fid,'%f    CRW   !STABILIZING CORRECTION WEIGHT, DEFAULT=1, PUT CRW>1 FOR H-MODE PROF. TO IMPROVE CONVERGENCE\n',CRW);
fprintf(fid,'0       ISN   !SPECTRAL SHIFT SWITCH: 1 ON, 0 OFF (OFF WHEN CRW>0)\n');
fprintf(fid,'-4      NAV   !NUMBER OF VACUUM INTERVALS\n');
fprintf(fid,'10.     WAC   !WALL DISTANCE, WAC<=1. NO VACUUM\n');
fprintf(fid,'0,1.0,0.,.928,3.00,.45,-0.0 IWAC,RWP,ZWP,AWP,EWP,DWP,SWP   !wall type,wall par\n');
fprintf(fid,'%3.2e   AL0   EIGENVALUE INITIAL GUESS\n',initial_guess);
fprintf(fid,'-10     NITMAX!MAX ITERATIONS IN PAMERA\n');
fprintf(fid,'1.E-3   EPSPAM!EPS IN PAMERA\n');
fprintf(fid,'1       IG    !GRAPHICS SWITCH\n');
fprintf(fid,'-1,-3,-4,0      ! j0bf switch\n');

status=fclose(fid);

