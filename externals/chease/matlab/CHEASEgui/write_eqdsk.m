function [eqdsk_cocosout, eqdsk_cocosout_IpB0pos,cocos_inout]=write_eqdsk(fnamefull,eqdsk_input,cocos_inout,Ipsign_out,B0sign_out,varargin);
%
% Write eqdsk to file fnamefull (if absent use eqdsk_input.fnamefull)
%
% function [eqdsk_cocosout, eqdsk_cocosout_IpB0pos,cocos_inout]=write_eqdsk(fnamefull,eqdsk_input[, cocos_inout, Ipsign_out, B0sign_out,varargin]);
%
%  cocos_inout: [cocos_in cocos_out], if only one value, assumes cocos_out=cocos_in=cocos_inout{1}. If empty cocos=2 by default
%  Ipsign_out: Ip sign for output. If empty, keep same effective value in real space (default) thus Ip_in * sig_RphiZ_in*sig_RphiZ_out
%              If a specific sign in output coordinates is wanted, then Ip_in * (Ip_sign_in*Ip_sign_out)
%  B0sign_out: Same as Ip but for B0 sign
%  varargin{1}: Cell array of extra comment lines to add to end of EQDSK file
%  varargin{2}: nverbose: 0 (no displays), 1 (warnings), 3 (detailed information, file written, etc (default))
%
%  eqdsk_cocosout using cocos_out and full signs
%  eqdsk_cocosout_IpB0pos using cocos_out convention but setting Ip>0 AND B0>0
%  cocos_inout, with cocos_inout(1)=cocos_in and cocos_inout(2)=cocos_out
%
%  Saves eqdsk_cocosout to fnamefull_COCOSxx
%  Saves eqdsk_cocosout_IpB0pos to fnamefull_COCOSxx_IpB0positive
%
% Examples:
%       write_eqdsk(eqdsk_struct); % writes eqdsk using eqdsk_struct.fnamefull for the filename and eqdsk.cocos for cocos
%       [eqdsk_cocosout, eqdsk_cocosout_IpB0pos,cocos_inout]=write_eqdsk(fnamefull,eqdsk_input); assumes cocos_in=cocos_out=2
%

eqdsk_cocosout = [];
eqdsk_cocosout_IpB0pos = [];
if ~exist('cocos_inout'); cocos_inout = []; end

if nargin==0
  disp('needs at least 1 input: eqdsk_structure with the field "fnamefull" provided, or 2 inputs');
  return
elseif nargin==1
  if ~isstruct(fnamefull)
    disp('needs at least 1 input: eqdsk_structure with the field "fnamefull" provided, or 2 inputs');
    return
  end
  % take fnamefull from input structure
  eqdsk_input = fnamefull;
  if isfield(eqdsk_input,'fnamefull') && ~isempty(eqdsk_input.fnamefull)
    fnamefull = eqdsk_input.fnamefull;
  else
    disp('needs at least 1 input: eqdsk_structure with the field "fnamefull" provided, or 2 inputs');
    return
  end
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
elseif isfield(eqdsk_input,'cocos') && ~isempty(eqdsk_input.cocos)
  cocos_in = eqdsk_input.cocos;
  cocos_out = cocos_in;
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

if nargin>=6 && ~isempty(varargin{1})
  extracomments=varargin{1};
else
  extracomments=[];
end
if nargin>=7 && ~isempty(varargin{2})
  nverbose = varargin{2};
else
  nverbose = 3;
end

% transform and construct eqdsk_outs
[eqdsk_cocosout, eqdsk_cocosout_IpB0pos,cocos_inout]=eqdsk_cocos_transform(eqdsk_input,[cocos_in cocos_out],Ipsign_out,B0sign_out);


[pathname,fname,fext]=fileparts(fnamefull);
if isempty(pathname); pathname=pwd; end
fext = [fext '_COCOS' num2str(cocos_out,'%2.2d')];
eqdsk_cocosout.fname=[fname fext];
eqdsk_cocosout.fnamefull=fullfile(pathname,eqdsk_cocosout.fname);
eqdsk_cocosout.pathname=pathname;
eqdsk_cocosout_IpB0pos.fname=[fname fext '_IpB0positive'];
eqdsk_cocosout_IpB0pos.fnamefull=fullfile(pathname,eqdsk_cocosout_IpB0pos.fname);
eqdsk_cocosout_IpB0pos.pathname=pathname;

eqdskin{1}=eqdsk_cocosout;
eqdskin{2}=eqdsk_cocosout_IpB0pos;

for ieq=1:length(eqdskin)
  % disp(['Write to file: ' eqdskin{ieq}.fnamefull])
  if ~exist(fileparts(eqdskin{ieq}.fnamefull),'dir'); unix(['mkdir ' fileparts(eqdskin{ieq}.fnamefull)]); end
  fid=fopen(eqdskin{ieq}.fnamefull,'w');
  %
  dumzer0=0.0;
  % TITLE, IDUM, NR: FORMAT 48A, 3I4
  fprintf(fid,'%48s%4d%4d%4d\n',eqdskin{ieq}.stitle,eqdskin{ieq}.ind1,eqdskin{ieq}.nr,eqdskin{ieq}.nz);
  % RBOXLEN, ZBOXLEN, R0EXP, RBOXLFT, ZMID: FORMAT 5E16.9
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',eqdskin{ieq}.rboxlen,eqdskin{ieq}.zboxlen,eqdskin{ieq}.r0,eqdskin{ieq}.rboxleft,eqdskin{ieq}.zmid);
  % RAXIS, ZAXIS, PSIAXIS, PSIEDGE, B0EXP: FORMAT 5E16.9
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',eqdskin{ieq}.raxis,eqdskin{ieq}.zaxis,eqdskin{ieq}.psiaxis,eqdskin{ieq}.psiedge,eqdskin{ieq}.b0);
  % IP, PSIAXIS, ZERO, RAXIS, ZERO: FORMAT 5E16.9
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',eqdskin{ieq}.ip,eqdskin{ieq}.psiaxis,dumzer0,eqdskin{ieq}.raxis,dumzer0);
  % ZAXIS, ZERO, PSIEDGE, ZERO, ZERO: FORMAT 5E16.9
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',eqdskin{ieq}.zaxis,dumzer0,eqdskin{ieq}.psiedge,dumzer0,dumzer0);
  % T(NR): FORMAT 5E16.9
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',eqdskin{ieq}.F(1:end-1));fprintf(fid,'%16.9E\n',eqdskin{ieq}.F(end));
  % P(NR): FORMAT 5E16.9
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',eqdskin{ieq}.p(1:end-1));fprintf(fid,'%16.9E\n',eqdskin{ieq}.p(end));
  % TT_PRIME(NR): FORMAT 5E16.9
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',eqdskin{ieq}.FFprime(1:end-1));fprintf(fid,'%16.9E\n',eqdskin{ieq}.FFprime(end));
  % P_PRIME(NR): FORMAT 5E16.9
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',eqdskin{ieq}.pprime(1:end-1));fprintf(fid,'%16.9E\n',eqdskin{ieq}.pprime(end));
  % PSI(R,Z): FORMAT 5E16.9
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',eqdskin{ieq}.psirz(1:end-1));fprintf(fid,'%16.9E\n',eqdskin{ieq}.psirz(end));
  % Q(NR): FORMAT 5E16.9
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',eqdskin{ieq}.q(1:end-1));fprintf(fid,'%16.9E\n',eqdskin{ieq}.q(end));
  % NPLASMA_BOUND, NLIMITER_BOUND: FORMAT 2I5
  fprintf(fid,'%5d%5d\n',eqdskin{ieq}.nbbound,eqdskin{ieq}.nblim);
  % RPLASMA_BOUND(NPLASMA_BOUND), ZPLASMA_BOUND(NPLASMA_BOUND)
  aa=reshape([eqdskin{ieq}.rplas' ; eqdskin{ieq}.zplas'],2*eqdskin{ieq}.nbbound,1);
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',aa(1:end-1));fprintf(fid,'%16.9E\n',aa(end));
  % RLIMITER_BOUND(NLIMITER_BOUND), ZLIMITER_BOUND(NLIMITER_BOUND)
  aa=reshape([eqdskin{ieq}.rlim' ; eqdskin{ieq}.zlim'],2*eqdskin{ieq}.nblim,1);
  fprintf(fid,'%16.9E%16.9E%16.9E%16.9E%16.9E\n',aa(1:end-1));fprintf(fid,'%16.9E\n',aa(end));
  % EXTRA LINES
  if isfield(eqdskin{ieq},'extralines')
    for i=1:length(eqdskin{ieq}.extralines)
      fprintf(fid,'%s\n',eqdskin{ieq}.extralines{i});
    end
  end
  
  % add extra comment lines
  if ~isempty(extracomments)
    fprintf(fid,'\n');
    for i=1:length(extracomments)
      fprintf(fid,'%s\n',extracomments{i});
    end
  end

  fclose_out=fclose(fid);
  if ~fclose_out
    if nverbose>=3; disp(['Wrote ',eqdskin{ieq}.fnamefull]); end
  else
    if nverbose>=1; warning(['something went wrong writing ',eqdskin{ieq}.fnamefull]); end
  end

end
