
;	profile plot, 4 or 6 plots 
;	every p step

imresoln=900

infile=dataroot+'plprof.out'

letterlabel=0

letterlabels=['a','b','c','d','e','f','g','h','i','j','k','l']

ipfix=0

resfrac=imresoln/1200.

xlabel='!8r!Da'
xlabel='!7q!D!6tor'

;	declarations

ncases=1

iu=14
aa=' '
aloop=' '
atyp=' '
atime=' '
ltyp=' '
np=1
x0=0.
x9=0.
iystyle=0

gifn=0
giffile=' '

;	open read file

openr,iu,infile

readf,iu,aa
readf,iu,x0,x9,ncases

;	preliminaries

lvars=['!7w','!8P','!8J','!7dw']

;	set plot limits

nxticks=4
xnames=strarr(nxticks+1)
for icurv=0,nxticks do xnames(icurv)=' '
xnames(0)='!6'+strtrim(string(x0,FORMAT='(F4.2)'),1)
xnames(nxticks/2)='!6'+strtrim(string(0.5*(x0+x9),FORMAT='(F4.2)'),1)
xnames(nxticks)='!6'+strtrim(string(x9,FORMAT='(F4.2)'),1)

;	color table

@colortable

;	set plot boundary -- unchanging

aspect=1.5

dy2=0.18
dx2=dy2/aspect
ytop=0.74
ybottom=0.25

dx=0.50/aspect
xleft=0.02+0.25/aspect
xright=xleft+dx
xfar=xright+dx

xctrs=[xleft,xright,xfar,xleft,xright,xfar]
yctrs=[ytop,ytop,ytop,ybottom,ybottom,ybottom]

	if (ncases eq 4) then begin
            xctrs=[xleft,xright,xleft,xright]
            yctrs=[ytop,ytop,ybottom,ybottom]
        endif

if (ncases eq 8) then begin
nynx=1.
@cbndydefs
endif

;	set plot device

x_wsize = imresoln
y_wsize = imresoln/aspect

if (idops eq 1) then begin
	set_plot,'ps'
endif else begin
	set_plot,'x'
	window,0,xsize=x_wsize,ysize=y_wsize,title='GEM'
	window,1, /pixmap, $
		xsize=x_wsize,ysize=y_wsize,title='GEM'
endelse



ylos=fltarr(ncases)
yhis=fltarr(ncases)

if (ipfix eq 1) then begin
    iystyle=1
    yhis=[30.,60.,60.,60.,0.4,40.]
    ylos=[0.,0.,0.,-120.,-0.4,-40.]
    yhis=[50.,120.,120.,120.,0.4,120.]
    ylos=[0.,0.,0.,-120.,-0.4,-120.]
    yhis=[100.,200.,200.,200.,0.5,300.]
    ylos=[0.,0.,0.,-400.,-0.5,-100.]
y9=100.*CEIL(0.06*x9)
y93=100.*CEIL(0.03*x9)
y9=3.
y93=2.
y9=2.
y93=1.
yvor=1.
ycur=0.01
    yhis=[y93,10.,y9,y9,1.,0.]
    ylos=[0.,0.,0.,-2.*y9,-1.,-2.]
    yhis=[y93,y9,y9,0,1.,y93]
    ylos=[0.,0.,0.,-2.*y9,-1.,0.]
    yhis=[y93,y9,y9,0,1.,0.]
    ylos=[0.,0.,0.,-2.*y9,-1.,-2.*y9]
    yhis=[y93,y9,y9,isol*y9,yvor,ycur]
    ylos=[0.,0.,0.,-2.*(1-isol)*y9,-yvor,-ycur]
    yhis=[y93,y9,y9,4.,yvor,ycur]
    ylos=[0.,0.,0.,-4,-yvor,-ycur]
;	three profiles and three gradients for ELMs
;    y9=1.5
;    yhis=[y9,y9,y9,0.,0.,0.]
;    ylos=[0.,0.,0.,-y9,-y9,-y9]
    if (ncases eq 4) then begin
        yhis=[80.,200.,0.5,0.3]
        ylos=[0.,-100.,-0.5,-0.3]
    endif
endif

;	time loop

ipg=0

fr1: readf,iu,aa
   if (aa eq 'AUS') then goto,fr9
	atime=strcompress(strtrim(aa,2))
	print, format='($,A,A)', '> Plotting to buffer... '

	ipg=ipg+1

;if (idops eq 1) then device,filename='plprof'+strtrim(string(ipg),1)+'.eps',$
if (idops eq 1) then device,filename='plprof.eps',$
		/times,/italic,/encapsulated,/color,/inches, $
		xsize=7.*aspect,ysize=7.,xoffset=0.75,yoffset=5.

if (idops eq 0) then erase

;	var loop

for icase=0,ncases-1 do begin

;	get profile

   readf,iu,ltyp
   readf,iu,np
   xx=fltarr(np)
   pfx=fltarr(np)
   readf,iu,xx
   readf,iu,pfx

   alabel=lvars(icase)

;	plot frame

	xctr=xctrs(icase)
	yctr=yctrs(icase)

	xlo=xctr-dx2
	xhi=xctr+dx2
	ylo=yctr-dy2
	yhi=yctr+dy2

   csiz=2.0*resfrac

   y9=max([0.,max(pfx)])
   y0=min([0.,min(pfx)])

   if (yhis(icase) ne 0.) then begin
       y9=yhis(icase)
       y0=-y9
       if (icase eq 1) then y0=0.
   endif

   y9=yhis(icase)
   y0=ylos(icase)

   plot,xx,pfx,xrange=[x0,x9],yrange=[y0,y9],$
	xtickname=xnames,xticks=nxticks,$
	xticklen=0.05,yticklen=0.05,$
	xstyle=1,yminor=1,position=[xlo,ylo,xhi,yhi],$
        ystyle=iystyle,$
	xthick=2*resfrac,ythick=2*resfrac,$
	charsize=csiz,color=dcolor,background=bcolor,/noerase

   if (isol eq 1) then begin
       xsol=x9-asol*(x9-x0)
       ysol=y0+0.1*(y9-y0)
       oplot,[xsol,xsol],[y0,ysol],thick=2,line=0,color=dcolor
   endif

;	annotation

   csiz=2.5*resfrac
   xmsg=xlo+0.5*(xhi-xlo)
   ymsg=yhi+.03
   xyouts,xmsg,ymsg,alabel+'!N!8(x)', $
	alignment=0.5,/noclip,/normal,size=csiz,color=dcolor

   xmsg=0.75*(xhi-xlo)+xlo
   ymsg=ylo-.05-0.02*idops
   xyouts,xmsg,ymsg,xlabel,$
   	alignment=0.5,/noclip,/normal,size=csiz,color=dcolor

   if (letterlabel eq 1) then begin
      xmsg=xhi-0.02
      ymsg=yhi-0.06
      xyouts,xmsg,ymsg,'!6('+letterlabels(icase)+')',$
	alignment=1.0,/noclip,/normal,size=csiz,color=dcolor
   endif

alabel='!8'+atime

   if (icase eq 0) then begin
	if (idops ne 1) then $
          polyfill,[.85,1.,1.,.85],[.95,.95,1.,1.],color=bcolor,/normal
      csiz=1.5*resfrac
      xmsg=0.98
      ymsg=0.95
      xyouts,xmsg,ymsg,alabel,$
	alignment=1.0,/noclip,/normal,size=csiz,color=dcolor
   endif

;	end var loop

endfor

; display - copy from buffer to visible window

if ( idops eq 0 ) then begin
  wset, 0
  device, copy=[0,0,x_wsize,y_wsize,0,0,1]
  wset, 1
endif else device,/close_file
print, atime

;	end time loop

time=strtrim(strmid(atime,4,4),2)
if (float(time) eq snaptime) then begin
;        print,'    holding snap frame... '
;        aloop=get_kbrd(1)
        print,'    done snap frame... '
        goto,fr9
endif

keystroke,aloop,'plprof'

if (idops eq 2) then begin
    giffile='plprof'+strcompress(string(gifn),/remove_all)+'.gif'
    image=tvrd()
    write_gif,giffile,image
    gifn=gifn+1
endif

goto,fr1
fr9:  aa=' '

;	aus

print,'all done'
close,iu
if (idops eq 0) then begin
	aa=get_kbrd(1)
	wdelete,0
endif

end
