function [eqdsk_cocosout, eqdsk_cocosout_IpB0pos,cocos_inout]=eqdsk_cocos_transform(eqdsk_input,cocos_inout,Ipsign_out,B0sign_out);
%
% [eqdsk_cocosout, eqdsk_cocosout_IpB0pos,cocos_inout]=eqdsk_cocos_transform(eqdsk_input,cocos_inout,Ipsign_out,B0sign_out);
%
% Transform input eqdsk_input with COCOS=cocos_inout(1) to eqdsk_cocosout with COCOS=cocos_inout(2)
% Also provides output eqdsk with COCOS=cocos_inout(2) AND Ip>0 AND B0>0 in eqdsk_cocosout_IpB0pos
%
% Inputs:
%  cocos_inout: [cocos_in cocos_out], if only one value, assumes cocos_out=cocos_in=cocos_inout{1}. If empty cocos=2 by default
%  Ipsign_out: Ip sign for output. If empty, keep same effective value in real space (default) thus Ip_in * sig_RphiZ_in*sig_RphiZ_out
%              If a specific sign in output coordinates is wanted, then Ip_in * (Ip_sign_in*Ip_sign_out)
%  B0sign_out: Same as Ip but for B0 sign
%
% Outputs:
%  eqdsk_cocosout using cocos_out and full signs
%  eqdsk_cocosout_IpB0pos using cocos_out convention but setting Ip>0 AND B0>0
%  cocos_inout, with cocos_inout(1)=cocos_in and cocos_inout(2)=cocos_out
%

if nargin < 1;
  error('At least 1 input arguments is expected: eqdsk_input=eqdsk_structure')
end

cocos_in = 2;
cocos_out = 2;
if exist('cocos_inout') && ~isempty(cocos_inout)
  cocos_in = cocos_inout(1);
  if length(cocos_inout) == 2
    cocos_out = cocos_inout(2);
  else
    cocos_out = cocos_in;
  end
end

if ~exist('Ipsign_out')
  Ipsign_out = [];
elseif ~isempty(Ipsign_out)
  if Ipsign_out~=1 && Ipsign_out~=-1
    error(['Ipsign_out should be +1 or -1, it is equal to: ' num2str(Ipsign_out)]);
  end
end

if ~exist('B0sign_out')
  B0sign_out = [];
elseif ~isempty(B0sign_out)
  if B0sign_out~=1 && B0sign_out~=-1
    error(['B0sign_out should be +1 or -1, it is equal to: ' num2str(B0sign_out)]);
  end
end

% Get parameters related to the cocos choices and check input eqdsk consistency
cocos_in_struct = cocos(cocos_in);
cocos_out_struct = cocos(cocos_out);

% check cocos_in consistency
% check q sign
qsign=sign(eqdsk_input.q(end));
if qsign*cocos_in_struct.sigma_rhothetaphi*sign(eqdsk_input.ip)*sign(eqdsk_input.b0) < 0
  warning(['sign of q is: ' num2str(qsign) char(10) 'But is should be equal to sign_rhothetaphi*sign(Ip)*sign(B0) = ' num2str(cocos_in_struct.sigma_rhothetaphi) '*' ...
        num2str(sign(eqdsk_input.ip)) '*' num2str(sign(eqdsk_input.b0)) ' = ' ...
        num2str(cocos_in_struct.sigma_rhothetaphi*sign(eqdsk_input.ip)*sign(eqdsk_input.b0))])
end
if sign(eqdsk_input.F)*sign(eqdsk_input.b0) < 0
  warning('Signs of F and B0 are not consistent')
end
if sign(eqdsk_input.psiedge-eqdsk_input.psiaxis)*cocos_in_struct.sigma_Bp*sign(eqdsk_input.ip) < 0
  if eqdsk_input.psiedge > eqdsk_input.psiaxis
    warning(['psi should be decreasing  with sign(Ip) = ' num2str(sign(eqdsk_input.ip)) ' for COCOS_IN = ' num2str(cocos_in_struct.cocos)]);
  else
    warning(['psi should be increasing  with sign(Ip) = ' num2str(sign(eqdsk_input.ip)) ' for COCOS_IN = ' num2str(cocos_in_struct.cocos)]);
  end
elseif sign(mean(eqdsk_input.pprime))*sign(eqdsk_input.ip)*cocos_in_struct.sigma_Bp > 0
  warning(['sign(pprime) should be ' num2str(-sign(eqdsk_input.ip)*cocos_in_struct.sigma_Bp)])
end

eqdsk_cocosout = eqdsk_input;
eqdsk_cocosout_IpB0pos = eqdsk_input;
if isfield(eqdsk_input,'cocos')
  if eqdsk_input.cocos ~= cocos_in
    warning(['input cocos= ' num2str(cocos_in) ' is different from input eqdsk.cocos= ' num2str(eqdsk_input.cocos)]);
  end
end
eqdsk_cocosout.cocos = cocos_out;
eqdsk_cocosout_IpB0pos.cocos = cocos_out;

% Define effective variables: sigma_Ip_eff, sigma_B0_eff, sigma_Bp_eff, exp_Bp_eff as in Appendix C
if isempty(Ipsign_out)
  % translate sign(Ip) relative to new coordinate conventions
  sigma_Ip_eff = cocos_in_struct.sigma_RphiZ * cocos_out_struct.sigma_RphiZ;
else
  % impose sign of Ip in new system to be Ipsign_out
  sigma_Ip_eff = sign(eqdsk_input.ip) * Ipsign_out;
end
Ipsign_out_eff = sign(eqdsk_input.ip) * sigma_Ip_eff;
if isempty(B0sign_out)
  % translate sign(B0) relative to new coordinate conventions
  sigma_B0_eff = cocos_in_struct.sigma_RphiZ * cocos_out_struct.sigma_RphiZ;
else
  % impose sign of B0 in new system to be B0sign_out
  sigma_B0_eff = sign(eqdsk_input.b0) * B0sign_out;
end
B0sign_out_eff = sign(eqdsk_input.b0) * sigma_B0_eff;
% impose sign of Ip>0 and B0>0 in IpB0pos output
sigma_Ippos_eff = sign(eqdsk_input.ip) * (+1);
sigma_B0pos_eff = sign(eqdsk_input.b0) * (+1);

sigma_Bp_eff = cocos_in_struct.sigma_Bp * cocos_out_struct.sigma_Bp;

% Eq. (15) as Ref with "out" being "cocos" and "in" being "chease,2", then re-written in Appendix C
exp_Bp_eff = cocos_out_struct.exp_Bp - cocos_in_struct.exp_Bp;

sigma_rhothetaphi_eff = cocos_in_struct.sigma_rhothetaphi .* cocos_out_struct.sigma_rhothetaphi;

% Transformation for cocos_out case and cocos_out_IpB0pos case
eqdsk_cocosout.F = eqdsk_input.F .* sigma_B0_eff;
eqdsk_cocosout_IpB0pos.F = eqdsk_input.F .* sigma_B0pos_eff;

eqdsk_cocosout.FFprime = eqdsk_input.FFprime .* sigma_Ip_eff .* sigma_Bp_eff ./ (2.*pi).^exp_Bp_eff;
eqdsk_cocosout_IpB0pos.FFprime = eqdsk_input.FFprime .* sigma_Ippos_eff .* sigma_Bp_eff ./ (2.*pi).^exp_Bp_eff;

eqdsk_cocosout.pprime = eqdsk_input.pprime .* sigma_Ip_eff .* sigma_Bp_eff ./ (2.*pi).^exp_Bp_eff;
eqdsk_cocosout_IpB0pos.pprime = eqdsk_input.pprime .* sigma_Ippos_eff .* sigma_Bp_eff ./ (2.*pi).^exp_Bp_eff;

fact_psi = sigma_Ip_eff .* sigma_Bp_eff .* (2.*pi).^exp_Bp_eff;
fact_psi_pos = sigma_Ippos_eff .* sigma_Bp_eff .* (2.*pi).^exp_Bp_eff;

eqdsk_cocosout.psirz = eqdsk_input.psirz .* fact_psi;
eqdsk_cocosout_IpB0pos.psirz = eqdsk_input.psirz .* fact_psi_pos;
eqdsk_cocosout.psi = eqdsk_input.psi .* fact_psi;
eqdsk_cocosout_IpB0pos.psi = eqdsk_input.psi .* fact_psi_pos;
eqdsk_cocosout.psiaxis = eqdsk_input.psiaxis .* fact_psi;
eqdsk_cocosout_IpB0pos.psiaxis = eqdsk_input.psiaxis .* fact_psi_pos;
eqdsk_cocosout.psiedge = eqdsk_input.psiedge .* fact_psi;
eqdsk_cocosout_IpB0pos.psiedge = eqdsk_input.psiedge .* fact_psi_pos;

eqdsk_cocosout.q = eqdsk_input.q .* sigma_Ip_eff .* sigma_B0_eff .* sigma_rhothetaphi_eff;
eqdsk_cocosout_IpB0pos.q = eqdsk_input.q .* sigma_Ippos_eff .* sigma_B0pos_eff .* sigma_rhothetaphi_eff;

eqdsk_cocosout.b0 = eqdsk_input.b0 .* sigma_B0_eff;
eqdsk_cocosout_IpB0pos.b0 = eqdsk_input.b0 .* sigma_B0pos_eff;
eqdsk_cocosout.ip = eqdsk_input.ip .* sigma_Ip_eff;
eqdsk_cocosout_IpB0pos.ip = eqdsk_input.ip .* sigma_Ippos_eff;

if isfield(eqdsk_cocosout,'extralines')
  iend = length(eqdsk_cocosout.extralines);
else
  iend=0;
end

if cocos_in ~= cocos_out
  eqdsk_cocosout.extralines{iend+1} = ['From eqdsk_cocos_transform, assuming COCOS_in=' num2str(cocos_in) ' and COCOS_out = ' num2str(cocos_out)]; iend = iend + 1;
  if isempty(Ipsign_out)
    eqdsk_cocosout.extralines{iend+1} = ['Ip sign translated from coordinate conventions transformation from sign(Ip_in)= ' num2str(sign(eqdsk_input.ip)) ...
                    ' to sign(Ip_out)= ' num2str(sign(eqdsk_cocosout.ip))]; iend = iend + 1;
  else
    eqdsk_cocosout.extralines{iend+1} = ['Ip sign imposed to be ' num2str(Ipsign_out)]; iend = iend + 1;
  end
  if isempty(B0sign_out)
    eqdsk_cocosout.extralines{iend+1} = ['B0 sign translated from coordinate conventions transformation from sign(B0_in)= ' num2str(sign(eqdsk_input.b0)) ...
                    ' to sign(B0_out)= ' num2str(sign(eqdsk_cocosout.b0))]; iend = iend + 1;
  else
    eqdsk_cocosout.extralines{iend+1} = ['B0 sign imposed to be ' num2str(B0sign_out)]; iend = iend + 1;
  end
else
  if ~isempty(Ipsign_out)
    eqdsk_cocosout.extralines{iend+1} = ['Ip sign imposed to be ' num2str(Ipsign_out)]; iend = iend + 1;
  end
  if ~isempty(B0sign_out)
    eqdsk_cocosout.extralines{iend+1} = ['B0 sign imposed to be ' num2str(B0sign_out)]; iend = iend + 1;
  end
end
