function [x,y] = cubic_profile (x1,x2,y1,y2,y1prime,y2prime);
%this function builds the cubic profile starting from (x1,y1), (x2,y2) points which
%belong to the function, y1prime and y2prime, value of the derivative of
%the funciton in x=x1 and x=x2
%SYNTAX:
%cubic_profile (x1,x2,y1,y2,y1prime,y2prime)

%define coefficients
delta =x2-x1;
d = y1;
a = y2;
c =  delta.*y1prime + 3*d;
b = -delta.*y2prime + 3*a;

%build the profile

x = linspace (x1,x2,33);
tmp1=(x-x1)/delta;
tmp2=(x2-x)/delta;
y=a * tmp1.^3 + b * (tmp1.^2).*tmp2 + c * tmp1 .* (tmp2.^2) + d * tmp2.^3;
%plot (x,y);
