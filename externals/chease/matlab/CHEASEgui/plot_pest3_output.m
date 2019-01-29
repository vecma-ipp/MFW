function plot_pest3_output(varargin)
%function plot_pest3_output
%function plot_pest3_output(filename)
% plots data from pest3 output file .cdf
% uses read_pest3_output.m

% Adapted from A.Pletzer's original version supplied with PEST3;
% F. Felici CRPP 2009
	
if nargin == 0
	o = read_pest3_output;
elseif nargin == 1
	o = read_pest3_output(varargin{1});
else
	error('wrong number of input arguments')
end

fprintf('psi_s^(2 mu) Delta-prime = %10.5f + i*%10.5f\n', real(o.dprim), imag(o.dprim))
fprintf('psi_s^(2 mu) Gamma-prime = %10.5f + i*%10.5f\n', real(o.gprim), imag(o.gprim))

figure(1)
s = o.psinew/max(o.psinew);
subplot(2,2,1), plot(s, o.pa), title('pressure vs \psi/\psi_a [\mu_0 Pa]')
subplot(2,2,2), plot(s, o.qa), title('q vs \psi/\psi_a')
subplot(2,2,3), plot(s, o.ga), title('g vs \psi/\psi_a [T m]')
subplot(2,2,4), plot(s, o.di, s, o.dr, '--'), title('D_I(-) and D_R(--) vs \psi/\psi_a'), axis([0 1 -1.25 1.])

figure(2)
pcolor(o.xa, o.za, o.total_solution)
cmax = max(max(o.total_solution))/5; cmin = -cmax;
axis('image'), shading('interp'), caxis([cmin cmax])
hold on
contour(o.xa, o.za, o.total_solution, linspace(cmin, cmax, 11), 'k')
title('\xi . \nabla \psi')
xlabel('X'), ylabel('Z')

end
