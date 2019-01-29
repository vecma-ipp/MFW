
;	several 2d ctr polar plots

resfrac=0.7

infile=dataroot+'plpol.out'

letterlabel=0

letterlabels=['a','b','c','d','e','f','g','h','i','j','k','l']

;	declarations

ncases=1

iu=14
aa=' '
aloop=' '
atyp=' '
atime=' '
alabel=' '
jlevel=1
jshift=1
ndash=1
nsolid=1
np=1
x0=0.
x9=0.
y0=0.
y9=0.
nynx=1.
rsep=1.
tsep=0.
ivar=1
ivar1=1
ivar2=2
	iskip=0

;	preliminaries

lvars=['!7w','!8P','!8J','!7dw']

;	open read file

openr,iu,infile

;	set plot limits

readf,iu,aa
readf,iu,x0,x9,y0,y9,nynx,ncases

nxticks=4
xnames=strarr(nxticks+1)
for icurv=0,nxticks do xnames(icurv)=' '
xnames(0)='!6'+strtrim(string(format='(i2)',x0),1)
xnames(nxticks/2)='!6'+strtrim(string(format='(i2)',0.5*(x0+x9)),1)
xnames(nxticks)='!6'+strtrim(string(format='(i2)',x9),1)

nyticks=4
ynames=strarr(nyticks+1)
for icurv=0,nyticks do ynames(icurv)=' '
ynames(0)='!6'+strtrim(string(format='(i2)',y0),1)
ynames(nyticks/2)='!6'+strtrim(string(format='(i2)',0.5*(y0+y9)),1)
ynames(nyticks)='!6'+strtrim(string(format='(i2)',y9),1)

;	set plot boundary -- unchanging

psiz=2.0
csiz=2.5
tsiz=1.5

@cbndydefs

x_wsize = resfrac*768.
y_wsize = x_wsize/aspect

;	set plot device

if (idops eq 1) then begin
	set_plot,'ps'
endif else begin
	set_plot,'x'
	window,0,xsize=x_wsize,ysize=y_wsize,title='GEMR'
	window,1, /pixmap, $
		xsize=x_wsize,ysize=y_wsize,title='GEMR'
endelse

;	color table

@bluegrayred

;	time loop

ipg=0

fr1: readf,iu,aa
   if (aa eq 'AUS') then goto,fr9
	atime=strcompress(strtrim(aa,2))
	print, format='($,A)', '> Plotting to buffer : '
	alabel='!8'+atime

	ipg=ipg+1

if (iskip eq 0 and idops ne 1) then erase

;if (idops eq 1) then device,filename='plpol'+strtrim(string(ipg),1)+'.eps',$
if (idops eq 1) then device,filename='plpol.eps',$
		/times,/italic,/encapsulated,/color,/inches, $
		xsize=7.,ysize=7./1.5

;	var loop

for icase=0,ncases-1 do begin

;	plot frame

	xctr=xctrs(icase)
	yctr=yctrs(icase)

	xlo=xctr-dx2
	xhi=xctr+dx2
	ylo=yctr-dy2
	yhi=yctr+dy2

if (iskip eq 0) then $
   plot,[x0,x9],[y0,y9],xrange=[x0,x9],$
	xtickname=xnames,xticks=nxticks,$
	title=lvars(icase)+'!N!8(R,Z)',$
	yrange=[y0,y9],ytickname=ynames,yticks=nyticks,$
	xticklen=0.03,yticklen=0.03*nynx,$
	xstyle=5,ystyle=5,position=[xlo,ylo,xhi,yhi],$
	xthick=2,ythick=2,$
	charsize=psiz,background=bcolor,color=dcolor,/nodata,/noerase

;	number of dashed and solid lines, and local color shift

   readf,iu,aa
      if (aa eq 'quit') then goto,st9
   readf,iu,ndash,nsolid
   if (nsolid lt 10) then jshift=nlines+1-ndash else jshift=1

;	get contour lines

st1:  readf,iu,atyp
      if (atyp eq 'done') then goto,st9
      if (atyp eq '    contour line') then begin
	readf,iu,jlevel
	readf,iu,aa
	readf,iu,aa
	readf,iu,atyp
      endif
      if (atyp eq 'next') then goto,st1

      if (atyp eq 'soli') then begin
	ltyp=0
	pcolor=poscolor
      endif
      if (atyp eq 'dash') then begin 
	ltyp=2
	pcolor=negcolor
      endif

      if (docolor eq 0) then pcolor=dcolor else begin
	pcolor=jlevel+jshift
	ltyp=0
      endelse
   if (docolor eq 1 and (pcolor lt 2 or pcolor gt ncolors)) then $
	print,'pcolor is ',pcolor
      readf,iu,np
      xp=fltarr(np)
      yp=xp
      readf,iu,xp
      readf,iu,yp
      if (iskip eq 0) then $
          oplot,xp,yp,line=ltyp,thick=2,color=pcolor
      goto,st1

st9:  aa=' '

;	get the interval
   readf,iu,aa
   ainterval=strtrim(strmid(aa,18,9),2)

;	get the boundary surface

   readf,iu,atyp
      if (atyp eq 'soli') then ltyp=0
      if (atyp eq 'dash') then ltyp=2
   readf,iu,np
   rr=fltarr(np)
   zz=fltarr(np)
   readf,iu,rr
   readf,iu,zz
   oplot,rr,zz,line=ltyp

;	annotation

   ymsg=ylo-0.05

   xmsg=0.5*(xlo+xhi)
   xmsg1=xmsg+0.14
   ymsg0=ymsg-.005
   ymsg1=ymsg+0.017
   if (idops eq 0) then $
	polyfill,[xmsg,xmsg1,xmsg1,xmsg],[ymsg0,ymsg0,ymsg1,ymsg1],$
	color=bcolor,/normal
   if (iskip eq 0) then $
	xyouts,xmsg,ymsg,'!7D!6 = '+ainterval, $
	alignment=0.5,/noclip,/normal,size=0.65*csiz,color=dcolor

   if (letterlabel eq 1) then begin
      xmsg=xhi-0.02
      ymsg=yhi-0.06
if (iskip eq 0) then $
      xyouts,xmsg,ymsg,'!6('+letterlabels(icase)+')',$
	alignment=1.0,/noclip,/normal,size=csiz,color=dcolor
   endif

   if (icase eq 0) then begin
	if (idops eq 0) then $
	polyfill,[.85,1.,1.,.85],[.95,.95,1.,1.],color=bcolor,/normal
      xmsg=0.98
      ymsg=0.965
      if (idops eq 1) then ymsg=ymsg-0.01
      xyouts,xmsg,ymsg,alabel,$
	alignment=1.0,/noclip,/normal,size=tsiz,color=dcolor
   endif

;	end var loop

endfor

; display - copy from buffer to visible window

if ( iskip eq 0 ) then begin
if ( idops eq 0 ) then begin
  wset, 0
  device, copy=[0,0,x_wsize,y_wsize,0,0,1]
  wset, 1
endif
  print, atime
endif else if ( iskip eq 1 ) then print, ' (skipped)'

if (idops eq 1) then device,/close_file

;	end time loop

if (idops eq 2) then begin
    giffile='plpol'+strcompress(string(ipg),/remove_all)+'.gif'
    image=tvrd()
    write_gif,giffile,image,redg,greeng,blueg
endif

keystroke,aloop,'plpol'
if ((aloop eq 'S') or (aloop eq 's')) then begin
  iskip=1
endif else iskip=0

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
