function [globalsvalues,ocols_struct] = extractdatachease(outputchease,outputcheasecols,varargin)
%
% [globalsvalues,ocols_struct] = extractdatachease(outputchease,outputcheasecols,varargin);
%
% extract data from output chease file, outputchease, and output chease in cols with headers: outputcheasecols
%
% globalsvalues: structure containing main global results extracted from output: q0, qedge, q95, betaN, betap, li, kappa, delta, etc
%
% ocols_struct: contains the columns from outputcheasecols including headers in .label.
%               The data in.data and in .labelname
%

globalsvalues.q0=NaN;
globalsvalues.qedge=NaN;
globalsvalues.qmin=NaN;
globalsvalues.rhopsiqmin=NaN;
globalsvalues.q95=NaN;
globalsvalues.epsilon_edge=NaN;
globalsvalues.kappa_edge=NaN;
globalsvalues.delta_bottom=NaN;
globalsvalues.delta_upper=NaN;
globalsvalues.delta=NaN;

% first extract cols and labels:
%
ocols_struct = read_ocols(outputcheasecols);
%[ocols_struct] = plotdatafile(outputcheasecols,0); ocols_struct.data = ocols_struct.pdata_in;

ismesh=find(strcmp('S-MESH',ocols_struct.labels)>0);
iq=find(strcmp('Qprofile',ocols_struct.labels)>0);
ikappa=find(strcmp('elongation',ocols_struct.labels)>0);
idelta_bottom=find(strcmp('delta_bottom',ocols_struct.labels)>0);
idelta_upper=find(strcmp('delta_upper',ocols_struct.labels)>0);
ieps=find(strcmp('a/Rgeom',ocols_struct.labels)>0);

globalsvalues.q0=ocols_struct.data(1,iq);
globalsvalues.qedge=ocols_struct.data(end,iq);
[globalsvalues.qmin,iqmin]=min(ocols_struct.data(:,iq));
globalsvalues.rhopsiqmin=ocols_struct.data(iqmin,ismesh);

rhopsi95=sqrt(0.95);
% FF Feb2012: removed interpos so that run_chease_expeq works also when interpos mex
% not installed (this is the only place where it is called...)
%globalsvalues.q95=interpos(ocols_struct.data(:,ismesh),ocols_struct.data(:,iq), ...
%         rhopsi95,-0.1,[2 2],[globalsvalues.q0 globalsvalues.qedge]);
globalsvalues.q95 = interp1(ocols_struct.data(:,ismesh),ocols_struct.data(:,iq),...
    rhopsi95,'spline'); % use interp1 instead (result practically the same, BC not necessary)
      
globalsvalues.epsilon_edge=ocols_struct.data(end,ieps);
globalsvalues.kappa_edge=ocols_struct.data(end,ikappa);

globalsvalues.delta_bottom=ocols_struct.data(end,idelta_bottom);
globalsvalues.delta_upper=ocols_struct.data(end,idelta_upper);
globalsvalues.delta=0.5*(globalsvalues.delta_bottom+globalsvalues.delta_upper);

% extract from output file
fid=fopen(outputchease);
S = textscan(fid,'%s','Delimiter','\n','whitespace','');
ocheaselines = S{1};
fclose(fid);

% scan through lines and get what you want (could replace perl function)
globalsvalues.scale=NaN;
globalsvalues.psiaxis=NaN;
globalsvalues.psiaxis2pi=NaN;
globalsvalues.r0exp=NaN;
globalsvalues.b0exp=NaN;
globalsvalues.ipchease=NaN;
globalsvalues.ip_phys=NaN;
globalsvalues.li=NaN;
globalsvalues.beta_exp=NaN;
globalsvalues.betap=NaN;
globalsvalues.betan_exp =NaN;
globalsvalues.betan =NaN;
globalsvalues.volumchease=NaN;
globalsvalues.rgeomchease=NaN;
globalsvalues.achease=NaN;
globalsvalues.ibs_fraction_0coll =NaN;
globalsvalues.ibs_fraction_fullcoll =NaN;
for ij=1:length(ocheaselines)
  if ~isempty(strfind(ocheaselines{ij},'SCALE'))
    [aa,bb,cc,dd]=sscanf(ocheaselines{ij},'%s',2);
    globalsvalues.scale=sscanf(ocheaselines{ij}(dd:end),'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'abs(PSI-AXIS)'))
    globalsvalues.psiaxis=-sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'R0 [M]'))
    globalsvalues.r0exp=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'B0 [T]'))
    globalsvalues.b0exp=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'TOTAL CURRENT'))
    globalsvalues.ipchease=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'LI'))
    globalsvalues.li=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'BETA_EXP'))
    globalsvalues.beta_exp=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'GEXP'))
    ij_k=strfind(ocheaselines{ij},'=');
    globalsvalues.betan_exp=sscanf(ocheaselines{ij}(ij_k(1)+1:end),'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'F0=IB.S./ITOT (NUE*=0, all ne,Te)'))
    ij_k=strfind(ocheaselines{ij},'=');
    globalsvalues.ibs_fraction_0coll = sscanf(ocheaselines{ij}(ij_k(end)+1:end),'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'IB.S./ITOT (NUE*.NE.0, all ne,Te)'))
    ij_k=strfind(ocheaselines{ij},'=');
    globalsvalues.ibs_fraction_fullcoll=sscanf(ocheaselines{ij}(ij_k(end)+1:end),'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'POLOIDAL BETA'))
    globalsvalues.betap=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'VOLUM'))
    globalsvalues.volumchease=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'RGEOM'))
    globalsvalues.rgeomchease=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'MINOR RADIUS'))
    globalsvalues.achease=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'AREA   ->'))
    globalsvalues.areachease=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'SURFACE   ->'))
    globalsvalues.surfacechease=sscanf(ocheaselines{ij},'%f',1);
  end
  if ~isempty(strfind(ocheaselines{ij},'LENGTH ->'))
    globalsvalues.poloidallengthchease=sscanf(ocheaselines{ij},'%f',1);
  end

end
globalsvalues.ip_phys=globalsvalues.ipchease/(4e-7*pi/globalsvalues.r0exp/globalsvalues.b0exp);
globalsvalues.betan = globalsvalues.beta_exp*100 / (globalsvalues.ip_phys/1e6/globalsvalues.achease/globalsvalues.r0exp/globalsvalues.b0exp);
globalsvalues.psiaxis2pi = 2.*pi.*globalsvalues.psiaxis;

% $$$ % get R0 and B0
% $$$ [a,b]=unix(['grep ''R0EXP   ='' ' outputchease]);
% $$$ if ~isempty(b)
% $$$   k=strfind(b,'=');
% $$$   globalsvalues.R0EXP=sscanf(b(k(1)+1:end),'%f');
% $$$ else
% $$$   globalsvalues.R0EXP=1.;
% $$$ end
% $$$ [a,b]=unix(['grep ''B0EXP   ='' ' outputchease]);
% $$$ if ~isempty(b)
% $$$   k=strfind(b,'=');
% $$$   globalsvalues.B0EXP=sscanf(b(k(1)+1:end),'%f');
% $$$ else
% $$$   globalsvalues.B0EXP=1.;
% $$$ end
  
