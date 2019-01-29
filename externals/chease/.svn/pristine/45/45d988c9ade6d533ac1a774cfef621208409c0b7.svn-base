function [expeq_out]=eqdsk2expeq(eqdsk_input);
%
% [expeq_out]=eqdsk2expeq(eqdsk_input_structure);
%
% Transforms an eqdsk structure, assuming the cocos in the eqdsk structure into an expeq structure for CHEASE
%         (that is in normalized CHEASE units and with cocos=2, see http://crpp.epfl.ch/chease)
% If cocos not a field of eqdsk_input_structure, add it before calling this function
%

expeq_out=[];

% First transform into an COCOS=2 system and Ip>0, B0>0 as CHEASE assumes
if isfield(eqdsk_input,'cocos')
  [eqdsk_cocosout, eqdsk_cocosout_IpB0pos,cocos_inout]=eqdsk_cocos_transform(eqdsk_input,[eqdsk_input.cocos 2]);
else
  disp('add cocos as a field of the input eqdsk structure, with the expected cocos value')
  return
end

Ipsign=sign(eqdsk_cocosout.ip);
B0sign=sign(eqdsk_cocosout.b0);

expeq_out.fname=['EXPEQ_from_' eqdsk_cocosout.fname];
expeq_out.fnamefull = expeq_out.fname;
expeq_out.pathname = eqdsk_cocosout.pathname;

r0exp = eqdsk_cocosout.r0;
b0exp = abs(eqdsk_cocosout.b0);
mu0=4.e-7*pi;

expeq_out.epsilon = (max(eqdsk_cocosout.rplas)-min(eqdsk_cocosout.rplas)) ./ (max(eqdsk_cocosout.rplas)+min(eqdsk_cocosout.rplas));
expeq_out.zgeom = (max(eqdsk_cocosout.zplas)+min(eqdsk_cocosout.zplas)) / 2. / r0exp;
expeq_out.pedge = eqdsk_cocosout_IpB0pos.p(end) .* mu0 ./ b0exp.^2;
expeq_out.n_psi = length(eqdsk_cocosout.rplas);
expeq_out.RZ_psi = [reshape(eqdsk_cocosout.rplas,expeq_out.n_psi,1)  reshape(eqdsk_cocosout.zplas,expeq_out.n_psi,1)] ./ r0exp;
expeq_out.n_rho = length(eqdsk_cocosout.rhopsi);
expeq_out.nppfun = 4;
expeq_out.nsttp = 1;
expeq_out.nrhotype = 0;
expeq_out.rho = eqdsk_cocosout.rhopsi;
expeq_out.Pprime = eqdsk_cocosout_IpB0pos.pprime .* mu0 .* r0exp.^2 / b0exp;
expeq_out.TTprime = eqdsk_cocosout_IpB0pos.FFprime ./ b0exp;
expeq_out.Istar = [];
expeq_out.Jparallel = [];
expeq_out. footer(1)= {['From eqdsk file with COCOS = ' num2str(eqdsk_input.cocos)]};
expeq_out. footer(2)= {['From eqdsk file with Ip = ' num2str(eqdsk_input.ip,'%.4e')]};
expeq_out. footer(3)= {['From eqdsk file with B0 = ' num2str(eqdsk_input.b0)]};
expeq_out. footer(4)= {['But with CHEASE COCOS=2 should run with Ipsign=' num2str(Ipsign) ' and B0sign=' num2str(B0sign)]};
expeq_out.extralines = {''};

