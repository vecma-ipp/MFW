function [x,y] = quadratic_profile(x1,x2, yinput1, yinput2, yinput3, kopt);
%this function builds the quadratic profile starting from (x1,y1), (x2,y2) (points that belong to the function) 
%and the value of the derivative of the function either in the point x1 (case Kopt=1) or in the point x2 
%(case kopt=2). if kopt=3, the yinputs have the following meaning: yinput1
%is the value of the function when x=x1,yinput2 is the value of the
%derivative of the function when x=x1, yinput3 is the value of the
%derivative of the function when x=x2
%SYNTAX :
%[x,y] = quadratic_profile (x1,x2,yinout1,yinput2, yprime, kopt);

%defining the coefficients
    

c = yinput1/((x1-x2)^2);

if kopt==1
    a = yinput2 / ((x1-x2)^2);
    b= (yinput3/(x1-x2))-2*c;

elseif kopt==2
    a = yinput2 / ((x1-x2)^2);
    b= (yinput3/(x2-x1))-2*a;
elseif kopt==3
    b = (yinput2 / (x1-x2) ) - (2*yinput1 / ((x1 - x2)^2) );
    a = (yinput3 / (2 * (x2 - x1) ) ) - (yinput2 / (2 * (x1 - x2) ) ) + (yinput1 / ((x1-x2)^2)); 
    
else errordlg ('Error in building quadratic profile: wrong value of kopt!');
end

%build the profile
x = linspace(x1,x2,34);

tmp1 = x-x1;
tmp2 = x-x2;
y = a *  (tmp1.^2) + b * tmp1.* tmp2 + c * (tmp2.^2)  ;


%plot(x,y);

