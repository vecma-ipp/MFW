function [farray_out,varargout]= ...
    interpos2Dcartesian(x_in,y_in,farray_in,x_out,y_out,tension_x,tension_y,kextrapol,nbc_x,nbc_y,varargin);
%
% function [farray_out,varargout]= ...
%    interpos2Dcartesian(x_in,y_in,farray_in,x_out,y_out,tension_x,tension_y,kextrapol,nbc_x,nbc_y,varargin);
%
% Assume 1-D input meshes x_in(i), y_in(j) and series of 2-D inputs farray_in(i,j,k), k=1:nin. If farray_in(i,j) => nin=1.
%
% Compute the values of each 2D input function farray_in(:,:,k) at all points (x_out, y_out).
% x_out/y_out can be a scalar, 1D or 2D arrays. They need to have both the same size
%
% The interpolation is performed with two successive series of 1-D cubic spline interpolation with tension. Along x then along y.
% First one calculates the values at (x_in(i),y_out), using nbc_y B.C. and then one interpolates at the x_out values, using nbc_out.
%
% extrapolation is performed according to kextrapol (same for x and y at this stage):  
%     kextrapol = 0 => constant outside the (x_in,y_in) domain with f_extrapol = f_edge
%     kextrapol = 1 => linear extrapolation with continuous derivative along the direction (either x or y)
%     kextrapol = 2 => quadratic extrapolation with continuous derivative
%     kextrapol = 3 (default)=> cubic extrapolation nearby and then quadratic
%     kextrapol = 4 => cubic extrapolation (only safe if does not extrapolate very far)
%     kextrapol = 5 => no extrapolation. Get a warning if it does extrapolate
%
% The boundary conditions are given for nbc_x and nbc_y as follows:
% [0 0] (default): zero second derivative at the boundary of the input domain (x(1), x(end) for x direction for example). 
%                  This is the standard cubic spline boundary condition
% [1 1]          : zero 1st derivative at the boundary
% [2 2]          : fixed value at the boundary according to the input function
% [i j]          : i for the left boundary as above and j for the right boundary, for example [2 0], value at left, 2nd der=0 at right
% One can add periodic BC if requested (would be -1), ask olivier.sauter@epfl.ch
%
% Inputs:
% x_in(1:ndim_xin): input x mesh (1D)
% y_in(1:ndim_yin): input y mesh (1D)
% farray_in(1:ndim_xin,1:ndim_yin[,1:nin]): input functions to be fitted
% x_out, y_out: output mesh on which farray_in are requested. Output mesh can be 1D or 2D
%
% For tension_x/y, kextrapol and nbc_x/y. The same value is used for each k=1:nin functions if only k=1 is given, that is if tension_x/y, kextrapol are scalars and nbc_x/y are 2D:
%
% tension_x(1[:nin]): (default=-0.1 for each f) tension used with interpos in the x direction for farray_in(:,:,k). Use -1, -3 or -10 if function not very smooth
% tension_y(1[:nin]): (default=tension_x; scalar or 1D) tension used with interpos in the y direction for farray_in(:,:,k)
% kextrapol(1[:nin]): (default=0; scalar or 1D) see above for description for each kextrapol(k) related to interpolation of farray_in(:,:,k)
% nbc_x(2[,1:nin]): (default=[0 0]; 2D or 3D) see above for description for each nbc_x(1:2,k) related to interpolation of farray_in(:,:,k)
%
% varargin: not used so far
%
% Outputs:
%
% farray_out(size(x_out)[,1:nin]): Interpolated function(s) on (x_out(i,j), y_out(i,j)). If x_out is 1D, then farray_out(length(x_out),1[,1:nin])
%
% varargout: not used so far
%
farray_out = [];
varargout = cell(1);
%
% Check inputs:
% 
if prod(size(x_in)) ~= length(x_in)
  warning('expects 1-D x_in array')
  return
end
ndim_xin = length(x_in);
if prod(size(y_in)) ~= length(y_in)
  warning('expects 1-D x_in array')
  return
end
ndim_yin = length(y_in);
if length(size(farray_in)>=2) && length(size(farray_in)<=3)
  if length(size(farray_in)==3)
    nin = size(farray_in,3);
  else
    % 2D farray_in
    nin=1;
  end
else
  warning('expects 2-D or 3-D farray_in array')
  return
end
if size(farray_in,1) ~= ndim_xin
  warning('1st dim of farray_in should be same as x_in')
  return
end
if size(farray_in,2) ~= ndim_yin
  warning('2nd dim of farray_in should be same as y_in')
  return
end
if (size(x_out,1) ~= size(y_out,1)) || (size(x_out,2) ~= size(y_out,2))
  warning('bad number of out mesh points. x_out and y_out should be identical in size')
  return
end
%
tension_x_eff = zeros(1,nin);
if ~exist('tension_x') || isempty(tension_x)
  tension_x_eff(:) = -0.1;
elseif length(tension_x)==1
  % note: keep explicit indices on LHS for tension_x_eff so that if tension_x is single, tension_x_eff remains double (required for interpos)
  tension_x_eff(1:nin) = tension_x;
elseif length(tension_x)==nin
  tension_x_eff(:,:) = reshape(tension_x,size(tension_x_eff));
else
  warning('bad size for tension_x. It should be empty, scalar of length(nin). Check tension_x')
  return
end
tension_y_eff = zeros(1,nin);
if ~exist('tension_y') || isempty(tension_y)
  tension_y_eff(:) = tension_x_eff(:);
elseif length(tension_y)==1
  % note: keep eyplicit indices on LHS for tension_y_eff so that if tension_y is single, tension_y_eff remains double (required for interpos)
  tension_y_eff(1:nin) = tension_y;
elseif length(tension_y)==nin
  tension_y_eff(:,:) = reshape(tension_y,size(tension_y_eff));
else
  warning('bad size for tension_y. It should be empty, scalar of length(nin). Check tension_y')
  return
end
%
kextrapol_eff = zeros(1,nin);
if ~exist('kextrapol') || isempty(kextrapol)
  kextrapol_eff(:) = 3;
elseif length(kextrapol)==1
  % note: keep explicit indices on LHS for kextrapol_eff so that if kextrapol is single, kextrapol_eff remains double (required for interpos)
  kextrapol_eff(1:nin) = kextrapol;
elseif length(kextrapol)==nin
  kextrapol_eff(1:nin) = kextrapol(1:nin);
else
  warning('bad size for kextrapol. It should be empty, scalar of length(nin). Check kextrapol')
  return
end
for k=1:nin
  switch kextrapol_eff(k)
   case 0
    opt_interpos(k) = 63; % constant f_extrapo = f_edge
   case 1
    opt_interpos(k) = 23; % linear with continuous derivative
   case 2
    opt_interpos(k) = 33; % quadratic with continuous derivative
   case 3
    opt_interpos(k) = 13; % cubic nearby then quadratic (default interpos)
   case 4
    opt_interpos(k) = 43; % cubic 
   case 5
    opt_interpos(k) = 3; % no extrapolation, get warning from interpos if extrapolating
  end
end
%
nbc_x_eff=zeros(2,nin);
if ~exist('nbc_x') || isempty(nbc_x)
  % keep 0 as defaults
elseif length(nbc_x) == 2
  nbc_x_eff(1,:) = nbc_x(1);
  nbc_x_eff(2,:) = nbc_x(2);
elseif (size(nbc_x,1)==2 && size(nbc_x,2)==nin)
  nbc_x_eff = nbc_x;
else
  warning('bad size for boundary conditions for x, needs size=(1,2), (2,1) or (2,nin), check nbc_x')
  return
end
nbc_y_eff=zeros(2,nin);
if ~exist('nbc_x') || isempty(nbc_y)
  % keep 0 as defaults
elseif length(nbc_y) == 2
  nbc_y_eff(1,:) = nbc_y(1);
  nbc_y_eff(2,:) = nbc_y(2);
elseif (size(nbc_y,1)==2 && size(nbc_y,2)==nin)
  nbc_y_eff = nbc_y;
else
  warning('bad size for boundary conditions for y, needs size=(1,2), (2,1) or (2,nin), check nbc_y')
  return
end

% variables used as argument to interpos need to be double
x_in_eff = double(x_in);
y_in_eff = double(y_in);
f_in_eff = double(farray_in);

% Needs 2-D x_out, y_out for generality
if numel(x_out) == max(size(x_out))
  ndim_xout=length(x_out);
  ndim_yout=1;
  x_out_eff = reshape(double(x_out),ndim_xout,ndim_yout);
  y_out_eff = reshape(double(y_out),ndim_xout,ndim_yout);
else
  ndim_xout = size(x_out,1);
  ndim_yout = size(x_out,2);
  x_out_eff = double(x_out);
  y_out_eff = double(y_out);
end
farray_out = NaN * ones(ndim_xout,ndim_yout,nin);

%
% 1st interpolate on x. Could choose depending on mesh or from input choice, but then complicates the function below to become more general
%
for k=1:nin
  % since boundary conditions, tensio, etc are allowed to depend on each function being interpolated, cannot use "multiple" interpolation to save time (and not yet implemented in interpos in any case...)
  tension_x_k = tension_x_eff(k);
  tension_y_k = tension_y_eff(k);
  kextrapol_k = kextrapol_eff(k);
  nbc_x_k = nbc_x_eff(1:2,k);
  ybc_x_k = zeros(2,ndim_yin);
  nbc_y_k = nbc_y_eff(1:2,k);
  ybc_y_k = [0 0];
  for jout=1:ndim_yout
    % get values on f_temp(x_out,y_in) by 1D interpolation of (x_in,f_in)
    if nbc_x_k(1) == 2; ybc_x_k(1,1:ndim_yin)=f_in_eff(1,:,k); end
    if nbc_x_k(2) == 2; ybc_x_k(2,1:ndim_yin)=f_in_eff(end,:,k); end
    for jin=1:ndim_yin
      f_xout_yin(1:ndim_xout,jin,k) = interpos(opt_interpos(k),x_in_eff,f_in_eff(:,jin,k),x_out_eff(:,jout),tension_x_k,nbc_x_k,ybc_x_k(1:2,jin));
    end
    % then interpolate on y using (y_in,f_xout_yin) onto y_out
    for iout=1:ndim_xout
      if nbc_y_k(1) == 2; ybc_y_k(1)=f_xout_yin(iout,1,k); end
      if nbc_y_k(2) == 2; ybc_y_k(2)=f_xout_yin(iout,end,k); end
      f_out_eff(iout,jout,k) = interpos(opt_interpos(k),y_in_eff,f_xout_yin(iout,:,k),y_out_eff(iout,jout),tension_y_k,nbc_y_k,ybc_y_k(1:2));
    end
  end
end
  
farray_out = f_out_eff;
