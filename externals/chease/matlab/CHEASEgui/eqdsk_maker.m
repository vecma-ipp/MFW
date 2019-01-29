function [eqdsk_signsRphiZ, eqdsk_IpB0pos]=eqdsk_maker(varargin);
%%
%  [eqdskval_new]=eqdsk_maker(varargin);
%
% makes eqdsk with specified number from R,Z mesh points from an existing eqdsk file or from TCV shot and Liuqe psi
%
%  For a given shot and time from TCV:
%          varargin{1} : shot
%          varargin{2} : time
%          varargin{3} : Liuqe type (default=1)
%          varargin{4} and {5}: NR and NZ points for R,Z mesh
%          varargin{6} : 0 (default) no z shift, 1: shift such that zaxis=0
%          varargin{7} : cocos_out value (default=2)
%
%  From eqdsk file
%          varargin{1} : eqdsk filename
%          varargin{2} and {3}: NR and NZ points for R,Z mesh
%          varargin{4} : 0 (default) no z shift, 1: shift such that zaxis=0
%          varargin{5} : [B0sign Ipsign]  If input is an eqdsk with Ip>0, B>0 and q>0, thus an eqdsk_IpB0pos, 
%                     we need the effective signs of Ip and B0 to be able to reconstruct eqdsk_signsRphiZ
%                     Use +1 by default if not provided and write a warning message
%          varargin{6} : cocos_out value (default=2)
%
% [eqdsk_signsRphiZ, eqdsk_IpB0pos]=eqdsk_maker(shot,time); % default Liuqe 1 and output mesh is 129x129
% [eqdsk_signsRphiZ, eqdsk_IpB0pos]=eqdsk_maker(eqdsk_filename); % default output mesh is 129x129
% [eqdsk_signsRphiZ, eqdsk_IpB0pos]=eqdsk_maker(eqdsk_filename,257,257); % 257x257
% [eqdsk_signsRphiZ, eqdsk_IpB0pos]=eqdsk_maker(shot,time,3,257,257); % Liuqe 3 and 257x257
% [eqdsk_signsRphiZ, eqdsk_IpB0pos]=eqdsk_maker(shot,time,[],257,257); % Liuqe 1 and 257x257
% [eqdsk_signsRphiZ, eqdsk_IpB0pos]=eqdsk_maker(shot,time,[],257,257,1); % Liuqe 1 and 257x257 and shifts to zaxis=0
% 

if nargin==0
  disp('needs input arguments:')
  help eqdsk_maker
  return
end

dozshift = 0;
cocos_out = 2;
if ischar(varargin{1})
  eqdsk_filename=varargin{1};
  eqdskval=read_eqdsk(eqdsk_filename);
  if nargin >=3
    if ~isempty(varargin{2}); nrmesh=varargin{2}; end
    if ~isempty(varargin{3}); nzmesh=varargin{3}; end
    if nargin>=4 && ~isempty(varargin{4}); dozshift=varargin{4}; end
    if nargin>=6 && ~isempty(varargin{6}); cocos_out=varargin{6}; end
  else
    nrmesh=129;
    nzmesh=129;
  end
  textin=['''' eqdsk_filename ''',' num2str(nrmesh) ',' num2str(nzmesh)];
  if eqdskval.ip>0 && eqdskval.b0>0 && sign(eqdskval.q(end))>0
    if nargin>=5 && ~isempty(varargin{5}) && length(varargin{5})==2
      B0Ipsigns = varargin{5};
    else
      disp('should provide B0 and Ip signs, assume they are +1')
      B0Ipsigns = [1 1];
    end
    eqdskval.b0 = abs(eqdskval.b0) * B0Ipsigns(1);
    eqdskval.ip = abs(eqdskval.ip) * B0Ipsigns(2);
  else
    B0Ipsigns = [sign(eqdskval.b0) sign(eqdskval.ip)]; % even if not used in write_eqdsk
  end
else
  if nargin<2
    disp('need to specify time')
    return
  else
    shot=varargin{1};
    time=varargin{2};
    iLiuqe=1;
    if nargin>=3 && ~isempty(varargin{3}); iLiuqe=varargin{3}; end
    if nargin>=5
      nrmesh=varargin{4};
      nzmesh=varargin{5};
      if nargin>=6 && ~isempty(varargin{6}); dozshift=varargin{6}; end
    else
      nrmesh=129;
      nzmesh=129;
    end
    if nargin>=7 && ~isempty(varargin{7}); cocos_out=varargin{7}; end
    textin=[num2str(shot) ',' num2str(time) ',' num2str(iLiuqe) ',' num2str(nrmesh) ',' num2str(nzmesh)];
    username=getenv('USER');
    [a,b]=unix(['test -d /tmp/' username]);
    if a==1; unix(['mkdir /tmp/' username]); end
    dozshift
    fnames_readresults=read_results_for_chease(shot,time,iLiuqe,3,['/tmp/' username],[],[],dozshift,0);
    % eqdsk_filename=['/tmp/' username '/eqdsksigns.' num2str(shot) 't' num2str(time,'%.4f')];
    eqdsk_filename=fnames_readresults{4};
    eqdskval=read_eqdsk(eqdsk_filename,7); % LIUQE is 17 but read_results divided psi by 2pi thus 7
    % transform to cocos=2 since read_results originally assumed it was cocos=2
    [eqdsk_cocosout, eqdsk_cocosout_IpB0pos,cocos_inout]=eqdsk_cocos_transform(eqdskval,[7 2]);
    eqdskval = eqdsk_cocosout;
    B0Ipsigns = [sign(eqdskval.b0) sign(eqdskval.ip)];
  end
end

eqdskval_new=eqdsk_transform(eqdskval,nrmesh,nzmesh,[],1);
eqdskval_new.extralines{end+1}=['Through: eqdsk_new=eqdsk_maker(' textin ');'];
fname = [eqdsk_filename '_' num2str(nrmesh) 'x' num2str(nzmesh)];

% assumes signs of Ip and B0 are in .ip and .b0
[eqdsk_signsRphiZ, eqdsk_IpB0pos,cocos_inout]=write_eqdsk(fname,eqdskval_new,[2 cocos_out]);

disp(['Wrote to file ' eqdsk_signsRphiZ.fname ' the eqdsk with signs and COCOS=' num2str(cocos_inout(2)) ' conventions'])
disp(['Wrote to file ' eqdsk_IpB0pos.fname ' the eqdsk with Ip>0, B0>0 and COCOS=' num2str(cocos_inout(2)) ' conventions'])
