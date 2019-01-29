function [cocos_struct]=cocos(cocosin);
%
% [cocos_struct]=cocos(cocosin);
%
% Given the COCOS=cocosin index (See O. Sauter and S. Yu. Medvedev's paper in CHEASE trunk)
% It returns a structure corresponding to the values of (from Table I of paper):
%
% cocos_struct.exp_Bp:             0 or 1 depending if psi is already divided by 2pi or not, respectively
% cocos_struct.sigma_Bp:          +1 or -1, depending if psi is increasing or decreasing with Ip and B0 positive
% cocos_struct.sigma_RphiZ:       +1 or -1 depending if (R, phi, Z) is right-handed or (R, Z, phi), resp.
% cocos_struct.sigma_rhothetaphi: +1 or -1 depending if (rho, theta, phi) is right-handed or (rho, phi, theta), resp.
% cocos_struct.sign_q_pos:        +1 or -1 depending if q is positive or negative with Ip and B0 positive
% cocos_struct.sign_pprime_pos:   +1 or -1 depending if dp/dpsi is positive or negative with Ip and B0 positive
%
% Note that table III gives the sign transformations when Ip and B0 are not positive
%
cocos_struct.cocos = cocosin;
%
cocos_struct.exp_Bp = 0;
if cocosin >= 11
  cocos_struct.exp_Bp = 1;
end

%
% Other parameters from Table I
%
switch cocosin
 case {1, 11}
  % ITER, Boozer are COCOS=11
  cocos_struct.sigma_Bp = +1;
  cocos_struct.sigma_RphiZ = +1;
  cocos_struct.sigma_rhothetaphi = +1;
  cocos_struct.sign_q_pos = +1;
  cocos_struct.sign_pprime_pos = -1;
  %
 case {2, 12}
  % CHEASE, ONETWO, Hinton-Hazeltine, LION is COCOS=2
  cocos_struct.sigma_Bp = +1;
  cocos_struct.sigma_RphiZ = -1;
  cocos_struct.sigma_rhothetaphi = +1;
  cocos_struct.sign_q_pos = +1;
  cocos_struct.sign_pprime_pos = -1;
  %
 case {3, 13}
  % Freidberg, CAXE, KINX are COCOS=3
  % EU-ITM up to end of 2011 is COCOS=13
  cocos_struct.sigma_Bp = -1;
  cocos_struct.sigma_RphiZ = +1;
  cocos_struct.sigma_rhothetaphi = -1;
  cocos_struct.sign_q_pos = -1;
  cocos_struct.sign_pprime_pos = +1;
  %
 case {4, 14}
  % 
  cocos_struct.sigma_Bp = -1;
  cocos_struct.sigma_RphiZ = -1;
  cocos_struct.sigma_rhothetaphi = -1;
  cocos_struct.sign_q_pos = -1;
  cocos_struct.sign_pprime_pos = +1;
  %
 case {5, 15}
  % 
  cocos_struct.sigma_Bp = +1;
  cocos_struct.sigma_RphiZ = +1;
  cocos_struct.sigma_rhothetaphi = -1;
  cocos_struct.sign_q_pos = -1;
  cocos_struct.sign_pprime_pos = -1;
  %
 case {6, 16}
  % 
  cocos_struct.sigma_Bp = +1;
  cocos_struct.sigma_RphiZ = -1;
  cocos_struct.sigma_rhothetaphi = -1;
  cocos_struct.sign_q_pos = -1;
  cocos_struct.sign_pprime_pos = -1;
  %
 case {7, 17}
  % TCV psitbx is COCOS=7
  cocos_struct.sigma_Bp = -1;
  cocos_struct.sigma_RphiZ = +1;
  cocos_struct.sigma_rhothetaphi = +1;
  cocos_struct.sign_q_pos = +1;
  cocos_struct.sign_pprime_pos = +1;
  %
 case {8, 18}
  % 
  cocos_struct.sigma_Bp = -1;
  cocos_struct.sigma_RphiZ = -1;
  cocos_struct.sigma_rhothetaphi = +1;
  cocos_struct.sign_q_pos = +1;
  cocos_struct.sign_pprime_pos = +1;
  %
 otherwise
  % should not be here since all cases defined
  error([' ERROR IN COCOS: COCOS = ' num2str(cocosin) ' DOES NOT EXIST']);
end
%
