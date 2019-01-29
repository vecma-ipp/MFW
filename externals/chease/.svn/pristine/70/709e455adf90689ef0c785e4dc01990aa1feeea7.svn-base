%
%
% starting from shot=48158, t=1.65s, ran CHEASE from GUIprofs
shot=48158;
time=1.65;

% in /tmp:
expeq=read_expeq('EXPEQ.OUT.48158t1.6503');
exptnz=read_exptnz('EXPTNZ.OUT.48158t1.6503');
plot_expeq(expeq);
plot_exptnz(exptnz);

% get zeff from conf
z_eff=gdat(shot,'\RESULTS::CONF:Z_EFF',1);
zeff=1.6;
% change zeff in GUIprofs to get consistent exptnz
% compute total pressure
ptot=1.602e-19.*(exptnz.ne.*exptnz.te + exptnz.ni.*exptnz.ti);
figure;plot(exptnz.rhopsi,ptot);

[globalsvalues,ocols_struct] = extractdatachease('o.48158t1.6503','o.48158t1.6503.cols');
r0exp = globalsvalues.r0exp;
b0exp = globalsvalues.b0exp;

mu0=4e-7*pi;
ptot_chease = interpos(exptnz.rhopsi,ptot.*mu0./b0exp.^2,expeq.rho,-1,[1 0],[0 0]);
figure;plot(expeq.rho,ptot_chease);
% note p(end) not necessarily 0, might need to force through fit

expeq.Pprime=ptot_chease;
expeq.nppfun=8;

write_expeq(expeq,'EXPEQ.48158t1.6503_pTCV');

% reran using CHEASEgui and setting "pressure", ncscal=4

johmall=gdat(shot,'\RESULTS::IBS:JOHMAV');
jparall=gdat(shot,'\RESULTS::IBS:JPARAV');
jcdall=gdat(shot,'\RESULTS::IBS:JCDAV');
jbsall=gdat(shot,'\RESULTS::IBS:JBSAV');
it=iround_os(johmall.t,time);

johm=johmall.data(:,it);
jpar=jparall.data(:,it);
jcd=jcdall.data(:,it);
jbs=jbsall.data(:,it);

figure;
plot(jparall.x,jpar,'-');
hold all
plot(johmall.x,johm,'-');
plot(jbsall.x,jbs,'-');
plot(jcdall.x,jcd,'-');
legend('jpar','johm','jbs','jcd')

ii=find(jparall.x<0.95);
jpar_chease = interpos([jparall.x(ii); 1.],[jpar(ii); 0.].*mu0.*r0exp./b0exp,expeq.rho,-0.1,[1 2],[0 0]);
figure
plot(expeq.rho,jpar_chease)
expeq.Jparallel=abs(jpar_chease);
ipxpsign=sign(jpar_chease(1));
expeq.nsttp=3;
write_expeq(expeq,'EXPEQ.48158t1.6503_pTCV_Ipar');
plot_expeq(expeq);
write_exptnz(exptnz,'EXPTNZ.48158t1.6503_pTCV_Ipar');

cpress_for_same_beta=1.2;
expeq.Pprime=cpress_for_same_beta.*expeq.Pprime;
write_expeq(expeq,'EXPEQ.48158t1.6503_1.2pTCV_Ipar');
write_exptnz(exptnz,'EXPTNZ.48158t1.6503_1.2pTCV_Ipar');

% ran chease with both above files into: o.chease.48158t1.6503_1.2pTCV_Ipar, o.chease.48158t1.6503_1.2pTCV_Ipar.cols
% forLiuqe_fromCHEASE.m

