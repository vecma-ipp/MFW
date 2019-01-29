% Compares the hamada.dat and neoart.dat of two CHEASE runs
%    function [H1,H2,Hdiff,N1,N2,Ndiff]=chease_diff(th,flnm1,flpth1,flnm2,flpth2);
% Inputs:
%		flnm1,flnm2	file name roots
%		flpth1,flpth2   file paths
%               th              threshold: 2*abs(a1-a2)/(abs(a1)+abs(a2))>th triggers a signicative difference
%                               if th<0, the diff is not performed
% This routine is not written at all in a general way, therefore it only works for a specific output file structure

function [H1,H2,Hdiff,N1,N2,Ndiff]=chease_diff(th,flnm1,flpth1,flnm2,flpth2);


% default paths
if ~exist('flpth1')||isempty(flpth1)
   flpth1='./';
end
if ~exist('flpth2')||isempty(flpth2)
   flpth2='./';
end

% file 1
if ~isempty(flnm1)
 H1=read_hamada(['hamada.' flnm1 '.dat'],flpth1);
 N1=read_neoart(['neoart.' flnm1 '.dat'],flpth1);
else
 H1=read_hamada(['hamada.dat'],flpth1);
 N1=read_neoart(['neoart.dat'],flpth1);
end 

% file 2
if ~isempty(flnm1)
 H2=read_hamada(['hamada.' flnm2 '.dat'],flpth2);
 N2=read_neoart(['neoart.' flnm2 '.dat'],flpth2);
else
 H2=read_hamada(['hamada.dat'],flpth2);
 N2=read_neoart(['neoart.dat'],flpth2);
end

str={'H','N'};

% diff
if th>0
  try
    for ii=1:length(str),
      F=fieldnames(eval([str{ii} '1']));
      for jj=1:length(F)
        eval([str{ii} 'diff.' F{jj} '=2*abs(' str{ii} '1.' F{jj} '-' str{ii} '2.' F{jj} ')./(abs(' str{ii} '1.' F{jj} ')+abs(' str{ii} '2.' F{jj} '));']);
        if eval(['any(' str{ii} 'diff.' F{jj} '>th)'])
          disp(['Significative difference in ' str{ii} 'diff.' F{jj}])
        end
      end
    end
  catch
    disp('could not calculate diffs, probably because of different sizes')
    Ediff=0;
    Hdiff=0;
    Ndiff=0;
  end
else 
  Ediff=0;
  Hdiff=0;
  Ndiff=0;
end

%return
% plots for hamada.dat
F=fieldnames(H1);
for jj=8:22
  figure
  eval(['plot(sqrt(H1.psi./max(H1.psi)),H1.' F{jj} ',''b'');']);
  hold on
  eval(['plot(sqrt(H2.psi./max(H2.psi)),H2.' F{jj} ',''r--'');']);
  title(['H1.' F{jj} '(:,1:4:end)'])
end
for jj=23:length(F)
  figure
  eval(['plot(sqrt(H1.psi./max(H1.psi)),H1.' F{jj} '(:,1:4:end),''b'');']);
  hold on
  eval(['plot(sqrt(H2.psi./max(H2.psi)),H2.' F{jj} '(:,1:4:end),''r--'');']);
  title(['H1.' F{jj} '(:,1:4:end)'])
end

% plots for neoart.dat
F=fieldnames(N1);
for jj=7:length(F)
  figure
  eval(['plot(sqrt(N1.psi./max(N1.psi)),N1.' F{jj} ',''b'');']);
  hold on
  eval(['plot(sqrt(N2.psi./max(N2.psi)),N2.' F{jj} ',''r--'');']);
  title(['N1.' F{jj} '(:,1:4:end)'])
end
