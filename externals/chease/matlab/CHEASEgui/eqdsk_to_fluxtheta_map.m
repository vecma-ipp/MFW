function [eqdsk_new]=eqdsk_to_fluxtheta_map(fname_or_eqdskstruct,rho_out_type,rho_out_mesh,varargin);
%
% [eqdsk_new]=eqdsk_to_fluxtheta_map(fname_or_eqdskstruct,rho_out_type,rho_out_mesh,varargin);
%
% Assume eqdsk_structure obtained from read_eqdsk(file,cocos,1); (with the extra "1" option to calculate various quantities like rhotor)
%
% if fname_or_eqdskstruct is empty or not a struct, calls read_eqdsk(fname_or_eqdskstruct,varargin{1},1) which prompts for an eqdsk file if needed
%
% rho_out_type:  1 (default): related to normalized sqrt(poloidal flux)
%                2 : related to normalized sqrt(toroidal flux)
%                3 : related to normalized psi (so square of option 1)
%                4 : related to normlaized toroidal flux (so square of option 2)
%
% rho_out_mesh: []: use radial mesh of eqdsk for profiles (eqdsk.rhopsi or eqdsk.rhotor)
%               NN1: use an equidistant mesh with NN1 points related to rho_out_type choice
%               array: use input array (should be an array between 0 and 1)
%
% varargin{1}: COCOS index value required if fname is an eqdsk filename. Assume 2 (as CHEASE/EFIT) if not given
%

% 
% Use profiles_2d psi(grid) assuming grid is dim1=R, dim2=Z rectangular mesh
% Find flux surfaces inside the LCFS (at this stage but could interpolate outside)
% Flux surfaces are chosen from a rhopsi_norm_in mesh or a rhotor_norm_in (here using nflux points)
% The theta_mesh is equidistant using ntheta points
%
% Interpolates also br, bz and bphi on the flux surface mesh
%
% Needs also:
% plasma boundary (Rbnd, Zbnd) taken from equil.eqgeometry.boundary.r/z
% plasma axis from : equil.global_param.mag_axis.position.r/z
% Either: psi_axis/psi_edge from equil.global_param.psi_ax/bound to normalize equil.profiles_2d.psi
%         phi_edge from equil.profiles_1d.phi(end) to normalize equil.profiles_2d.phi
%

nargin_eff = nargin - 3;

if ~isstruct(fname_or_eqdskstruct)
  if nargin_eff>=1
    eqdsk_new=read_eqdsk(fname_or_eqdskstruct,varargin{1},1);
  else
    eqdsk_new=read_eqdsk(fname_or_eqdskstruct,[],1);
  end
else
  eqdsk_new = fname_or_eqdskstruct;
end

if ~exist('rho_out_type') || isempty(rho_out_type)
  rho_out_type_eff = 1;
else
  rho_out_type_eff = rho_out_type;
end

if ~exist('rho_out_mesh') || isempty(rho_out_mesh)
  switch rho_out_type_eff
   case {1,3}
    flux_norm_out = eqdsk_new.rhopsi.^2;
   case {2,4}
    flux_norm_out = eqdsk_new.rhotor.^2;
   otherwise
    disp(['should not be in this case with no rho_out_mesh and rho_out_type_eff = ',num2str(rho_out_type_eff)]);
    return
  end
elseif isnumeric(rho_out_mesh) && length(rho_out_mesh)==1
  switch rho_out_type_eff
   case {1,2}
    % the surfaces will be derived from the normalized fluxes psi or Phi
    flux_norm_out = linspace(0,1,rho_out_mesh).^2;
   case {3,4}
    flux_norm_out = linspace(0,1,rho_out_mesh);
   otherwise
    disp(['should not be in this case with rho_out_mesh = ',num2str(rho_out_mesh), ...
	  ' and rho_out_type_eff = ',num2str(rho_out_type_eff)]);
    return
  end
else
  if (min(rho_out_mesh) >= 0) & (max(rho_out_mesh) <=1)
    flux_norm_out =  rho_out_mesh;
  else
    disp('rho_out_mesh should be within 0 and 1, since normalized values are used and outside LCFS not yet tested')
    disp('uses values inside 0,1')
    ij=find(rho_out_mesh>= 0 & rho_out_mesh<=1);
    if isempty(ij); return; end
    flux_norm_out = rho_out_mesh(ij);
  end
  if rho_out_type_eff>=1 && rho_out_type_eff<=2
    flux_norm_out = flux_norm_out.^2;
  else
    flux_norm_out = flux_norm_out;
  end
end

RR=eqdsk_new.rmesh;
ZZ=eqdsk_new.zmesh;
Rbnd=eqdsk_new.rplas;
Zbnd=eqdsk_new.zplas;

Raxis = eqdsk_new.raxis;
Zaxis = eqdsk_new.zaxis;

nflux_out=length(flux_norm_out);
ntheta_out=max(nflux_out,121);

psi_axis = eqdsk_new.psiaxis;
psi_edge = eqdsk_new.psiedge;
psi_rz=eqdsk_new.psi;
flux_rz_norm = (psi_rz-psi_axis) ./ (psi_edge-psi_axis);

% if toroidal flux is the reference for the output mesh, compute the relevant psi_norm values
if rho_out_type_eff==2 || rho_out_type_eff==4
  flux_rz_norm_eff = interpos(eqdsk_new.rhotor_norm.^2,eqdsk_new.rhopsi.^2,flux_rz_norm);
else
  flux_rz_norm_eff = flux_rz_norm;
end

% 2D quantity to get flux surface in (:,:,1) and then quantities to interpolate in farray_in(:,:,2:end)
farray_in(:,:,1) = flux_rz_norm;
farray_in(:,:,2) = eqdsk_new.BR;
farray_in(:,:,3) = eqdsk_new.BZ;
farray_in(:,:,4) = eqdsk_new.Bphi;
farray_in(:,:,5) = eqdsk_new.Bpol;

% numerics choices
tension_default = -0.1;

% Compute a rho,theta mesh of plasma boundary. Make theta in 0,2pi and increasing for fitting
rhobnd=sqrt((Rbnd-Raxis).^2 + (Zbnd-Zaxis).^2);
thetabnd=atan2(Zbnd-Zaxis,Rbnd-Raxis);
ii=find(thetabnd<0);
thetabnd(ii) = thetabnd(ii) + 2*pi;
[thetabndsort,isort] = sort(thetabnd);
rhobndsort = rhobnd(isort);

% theta mesh
thetamesh=linspace(0.,2.*pi,ntheta_out);
rhobound_thetamesh=interpos(thetabndsort,rhobndsort,thetamesh,-0.1,[-1 -1],2.*pi);

nsigma=120; % create a polar rho mesh as fraction of rhobound for each thetamesh
sigma=linspace(0.,1.,nsigma);
for i=1:length(thetamesh)
  rhomesh(1:nsigma,i) = sigma.*rhobound_thetamesh(i);
end

% Compute flux_norm on (rho,theta) mesh by interpolating psi(R,Z) on these Rrho,Rtheta points
thetamesh2D = ones(nsigma,1) * reshape(thetamesh,1,length(thetamesh));
Rrhotheta = Raxis + rhomesh.*cos(thetamesh2D);
Zrhotheta = Zaxis + rhomesh.*sin(thetamesh2D);

tic
[farray_out,varargout]= interpos2Dcartesian(RR,ZZ,farray_in,Rrhotheta,Zrhotheta,tension_default);
toc
% make sure of edge values of normalized flux
farray_out(1,:,1) = 0.;
farray_out(end,:,1) = 1.;

% compute rho polar for each theta corresponding to desired flux surfaces, now that flux_norm known on rho,theta
% use sqrt(flux) since flux not good for inverse interpolation near axis
clear rho_polar
for j=1:ntheta_out
  [rho_polar(:,j)]=interpos(sqrt(farray_out(:,j,1)),rhomesh(:,j),sqrt(flux_norm_out),tension_default,[2 2],[rhomesh(1,j) rhomesh(end,j)]);
  farray_out_fluxnormtheta(:,j,1) = flux_norm_out;
  for k=2:size(farray_out,3)
    farray_out_fluxnormtheta(:,j,k) = interpos(rhomesh(:,j),farray_out(:,j,k),rho_polar(:,j),tension_default, ...
          [2 2],[farray_out(1,j,k) farray_out(end,j,k)]);
  end
end
thetamesh2D = ones(nflux_out,1)*thetamesh;

% rho_polar and farray_out_fluxnormtheta(:,:,k) are now known on (flux_norm_out,thetamesh) mesh
%

% test with plotting flux contours
figure
plot((Raxis + rho_polar.*cos(thetamesh2D))',(Zaxis + rho_polar.*sin(thetamesh2D))')
%keyboard
hold on
contour(RR,ZZ,farray_in(:,:,1)',flux_norm_out,'k--')

for k=1:size(farray_out,3)
  figure
  contour(Raxis + rho_polar.*cos(thetamesh2D),Zaxis + rho_polar.*sin(thetamesh2D),farray_out_fluxnormtheta(:,:,k),40)
  hold on
  contour(RR,ZZ,farray_in(:,:,k)',40,'k--')
  title([num2str(k)])
end

eqdsk_new.fluxtheta.rho_out_type = rho_out_type_eff;
eqdsk_new.fluxtheta.flux_norm_out = flux_norm_out;
eqdsk_new.fluxtheta.rho_norm_out = sqrt(flux_norm_out);
eqdsk_new.fluxtheta.thetamesh = thetamesh;
eqdsk_new.fluxtheta.Raxis = Raxis;
eqdsk_new.fluxtheta.Zaxis = Zaxis;
eqdsk_new.fluxtheta.R_rho_norm_theta = Raxis + rho_polar.*cos(thetamesh2D);
eqdsk_new.fluxtheta.Z_rho_norm_theta = Zaxis + rho_polar.*sin(thetamesh2D);
eqdsk_new.fluxtheta.rho_polar = rho_polar;
eqdsk_new.fluxtheta.thetamesh_2d = thetamesh2D;
eqdsk_new.fluxtheta.psi = farray_out_fluxnormtheta(:,:,1);
eqdsk_new.fluxtheta.BR = farray_out_fluxnormtheta(:,:,2);
eqdsk_new.fluxtheta.BZ = farray_out_fluxnormtheta(:,:,3);
eqdsk_new.fluxtheta.Bphi = farray_out_fluxnormtheta(:,:,4);
eqdsk_new.fluxtheta.Bpol = farray_out_fluxnormtheta(:,:,5);
