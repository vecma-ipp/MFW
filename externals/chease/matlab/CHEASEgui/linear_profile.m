function [x,y]= linear_profile (x1,x2,yinput1,yinput2, kopt);
%this function builds the linear pfofile starting from the values of x1,x2.
%Depending on the value of kopt, yinput1 and yinput 2 have different
%meanings: ig kopt== 1 yinput1 is the value of the function when x=x1,
%yinput2 is the value of the derivative of the function (the slope).
%If kopt==2, yinput1 is the value of the function at x=x1, yinput2 is the
%value of the function at x=x2. If kopt==3, yinput1 is the value of the
%function at x=x2, yinput2 is the value of the derivative of the function
%(the slope)
%SYNTAX:
%[x,y]=linear_profile(x1,x2,yinput1,yinput2,kopt)

delta=(x2-x1);
if kopt==1
  % yinput1 is value at x1
  b = yinput1 ;
  % yinput2 is slope
  a = yinput2*delta + b;
elseif kopt==2
  % yinput1 is value at x1
  b = yinput1 ;
  % yinput2 is value at x2
  a = yinput2;
elseif kopt==3
  % yinput1 is value at x2
  a = yinput1 ;
  % yinput2 is slope
  b = a - yinput2*delta;
else
    errordlg ('Error in building linear profile: wrong value of kopt!');
end

%building the profile

x= linspace (x1,x2,33);
y = a *(x-x1)/delta + b * (x2-x)/delta;
%plot (x,y);
