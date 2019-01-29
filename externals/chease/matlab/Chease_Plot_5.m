%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Chease_Plot_5.m : Graphic output from CHEASE %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% STABILITY 1
clf;
figure(HandleList(4));
set(HandleList(4),...
    'Position',[150,50,500,500],...
    'PaperUnits','centimeters',...
    'Paperposition',[0. 0. 21. 29.7],...
    'PaperOrientation','portrait')

subplot(2,1,1)
% PLOT FLUX SURFACE
plot(R0EXP*CR(:,jmax:-stepsurf:max(1,jmax-stepsurf*maxsurf)), ...
     R0EXP*CZ(:,jmax:-stepsurf:max(1,jmax-stepsurf*maxsurf)),'c')
hold on
plot(R0EXP*ZRSUR,R0EXP*ZZSUR);
lmax = R0EXP*max(max(abs(ZRSUR)),max(abs(ZZSUR))) ;
axis([-lmax lmax -lmax lmax]) ;
axis('square');

% PLOT CHI LINES
plot(R0EXP*CR(1:stepchi:min(imax,stepchi*maxchi+1),:)', ...
     R0EXP*CZ(1:stepchi:min(imax,stepchi*maxchi+1),:)','c')

% PLOT DOT WHERE LOCAL SHEAR IS NEGATIVE 
i=find(RSHEAR<0.);
plot(R0EXP*CR(i),R0EXP*CZ(i),'.k')

% PLOT PROFILE DEFINITION LINE
ZR=R0EXP*[min(ZRSUR) max(ZRSUR)];
ZZ=[0.0 0.0];
plot(ZR,ZZ,'g')

% PLOT ZERO-CURVATIVE LINE
plot(R0EXP*ZRCURV,R0EXP*ZZCURV,'.r')

subplot(2,1,2)
hold on
ZRmax=R0EXP*max(ZRSUR);
ZRmin=R0EXP*min(ZRSUR);
ZZmax=R0EXP*(max(ZZSUR)-min(ZZSUR));
ZRscale=max(ZZmax,ZRmax-ZRmin)/(ZRmax-ZRmin);

% PLOT BALLOONING AND MERCIER UNSTABLE REGIONS
ii=find(IBALL>0);
jj=find(ZODIS<0);
kk=find(ZODRS<0);

plot([ZRmin ZRmax],ZZmax*[0.8 0.8],'g')
text(ZRmin,ZZmax*0.9,'Ballooning  (infinite n)')
if size(ii)~=0
   ZZ=ones(size(ZABR))*ZZmax*0.8;
   plot(R0EXP*ZABR(NPSI1+ii),ZZ(ii),'*')
%   plot(R0EXP*ZABR(NPSI1-ii+2),ZZ(ii),'*')
end

plot([ZRmin ZRmax],ZZmax*[0.5 0.5],'g')
text(ZRmin,ZZmax*0.6,'Ideal interchange (Mercier)')
if size(jj)~=0   
   ZZ=ones(size(ZABR))*ZZmax*0.5;
   plot(R0EXP*ZABR(NPSI1+jj),ZZ(jj),'o')
   plot(R0EXP*ZABR(NPSI1-jj+2),ZZ(jj),'o')
end

plot([ZRmin ZRmax],ZZmax*[0.2 0.2],'g')
text(ZRmin,ZZmax*0.3,'Resistive interchange (GGJ)')
if size(kk)~=0   
   ZZ=ones(size(ZABR))*ZZmax*0.2;
   plot(R0EXP*ZABR(NPSI1+kk),ZZ(kk),'+')
   plot(R0EXP*ZABR(NPSI1-kk+2),ZZ(kk),'+')
end

axis('square');
axis('equal');
axis('off');
plot(ZRscale*[ZRmin ZRmax],[0.0 0.0],'k')
plot(ZRscale*[ZRmin ZRmax],[ZZmax ZZmax],'k')
plot(ZRscale*[ZRmin ZRmin],[0.0 ZZmax],'k')
plot(ZRscale*[ZRmax ZRmax],[0.0 ZZmax],'k')

% Push-button to print graph
uicontrol(HandleList(4),...
  'String','Print',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[50 5 100 20],...
  'CallBack',[...
    'print ;'...
    'Chease_Plot_6']) ;

% Push-button to plot previous graph
uicontrol(HandleList(4),...
  'String','Previous',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[200 5 100 20],...
  'CallBack',[...
    'Chease_Plot_4']) ;
    
% Push-button to plot next graph
uicontrol(HandleList(4),...
  'String','Next',...
  'BackgroundColor','red',...
  'ForegroundColor','black',...
  'Position',[350 5 100 20],...
  'CallBack',[...
    'Chease_Plot_6']) ;
