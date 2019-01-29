subroutine assign_code_parameters(codeparameters, return_status)

!-----------------------------------------------------------------------
! calls the XML parser for the code parameters and assign the
! resulting values to the corresponding variables
!TODO: check an alternative and more elegant solution in Perl
!-----------------------------------------------------------------------
  
  use prec_const
  
  use euitm_schemas
  use euitm_xml_parser  
  use globals

  implicit none

  type (type_codeparam), intent(in) :: codeparameters
  integer(ikind), intent(out) :: return_status 

  type(tree) :: parameter_list
  type(element), pointer :: temp_pointer
  integer(ikind) :: i, nparm, n_values
  character(len = 132) :: cname
!  integer(ikind) :: ns, NEQDXTPO

!-- set path to XML schema
  file_xml_schema = 'EQDATA_schema.xml'

  return_status = 0      ! no error

!-- parse xml-string codeparameters%parameters

  call euitm_xml_parse(codeparameters%parameters, nparm, parameter_list)
  print *,'codeparameters%parameters= ',codeparameters%parameters
  print *,' nparm= ',nparm
!  print *,'parameter_list= ',parameter_list
  print *,'salut 1'

!-- assign variables

  temp_pointer => parameter_list%first

  outer: do
    cname = char2str(temp_pointer%cname)   ! necessary for AIX
    print *,'cname = ', trim(cname)
    select case (cname)
      case ("parameters")
        temp_pointer => temp_pointer%child
        cycle
!!$      case ("NS")
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call char2num(temp_pointer%cvalue, ns)
!!$        print *,' ns= ',ns
!!$      case ("NEQDXTPO")
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call char2num(temp_pointer%cvalue, neqdxtpo)
!!$        print *,' neqdxtpo= ',neqdxtpo
!!$      case ("ELONG")
!!$         ! integer/real constant
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call char2num(temp_pointer%cvalue, elong)
!!$        print *,' elong= ',elong
!!$      case ("APLACE")
!!$         ! array
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call scan_str2num(char2str(temp_pointer%cvalue), aplace, n_values)
!!$        print *,' aplace= ',aplace
!!$        print *,' n_values= ',n_values
!!$      case ("NITMSHOT")
!!$         ! array
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call scan_str2num(char2str(temp_pointer%cvalue), nitmshot, n_values)
!!$        print *,' nitmshot= ',nitmshot
!!$        print *,' n_values= ',n_values
!!$!--   
!!$      case ("")
!!$        temp_pointer => temp_pointer%child
!!$        cycle
!!$      case ("")
!!$         ! integer/real constant
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call char2num(temp_pointer%cvalue, )
!!$        print *,' = ',
!!$      case ("")
!!$         ! array
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call scan_str2num(char2str(temp_pointer%cvalue), , n_values)
!!$        print *,' = ',
!!$        print *,' n_values= ',n_values
!!$
!!$      case ("ntgaus")
!!$         ! integer/real constant
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call char2num(temp_pointer%cvalue, ntgaus)
!!$        print *,' ntgaus= ',ntgaus

!--   run_control_param
      case ("run_control_param")
        temp_pointer => temp_pointer%child
        cycle
      case ("epslon")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, epslon)
        print *,' epslon= ',epslon
      case ("relax")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, relax)
        print *,' relax= ',relax
      case ("ninmap")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ninmap)
        print *,' ninmap= ',ninmap
      case ("ninsca")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ninsca)
        print *,' ninsca= ',ninsca
      case ("nmgaus")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmgaus)
        print *,' nmgaus= ',nmgaus
      case ("nsgaus")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsgaus)
        print *,' nsgaus= ',nsgaus
      case ("ntgaus")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ntgaus)
        print *,' ntgaus= ',ntgaus

!--   meshes_param
      case ("meshes_param")
        temp_pointer => temp_pointer%child
        cycle
      case ("aplace")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), aplace, n_values)
        print *,' aplace= ',aplace
        print *,' n_values= ',n_values
      case ("awidth")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), awidth, n_values)
        print *,' awidth= ',awidth
        print *,' n_values= ',n_values
      case ("bplace")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), bplace, n_values)
        print *,' bplace= ',bplace
        print *,' n_values= ',n_values
      case ("bwidth")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), bwidth, n_values)
        print *,' bwidth= ',bwidth
        print *,' n_values= ',n_values
      case ("cplace")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), cplace, n_values)
        print *,' cplace= ',cplace
        print *,' n_values= ',n_values
      case ("cwidth")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), cwidth, n_values)
        print *,' cwidth= ',cwidth
        print *,' n_values= ',n_values
      case ("dplace")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), dplace, n_values)
        print *,' dplace= ',dplace
        print *,' n_values= ',n_values
      case ("dwidth")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), dwidth, n_values)
        print *,' dwidth= ',dwidth
        print *,' n_values= ',n_values
      case ("eplace")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), eplace, n_values)
        print *,' eplace= ',eplace
        print *,' n_values= ',n_values
      case ("ewidth")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), ewidth, n_values)
        print *,' ewidth= ',ewidth
        print *,' n_values= ',n_values
      case ("qplace")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), qplace, n_values)
        print *,' qplace= ',qplace
        print *,' n_values= ',n_values
      case ("qwidth")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), qwidth, n_values)
        print *,' qwidth= ',qwidth
        print *,' n_values= ',n_values

      case ("solpda")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpda)
        print *,' solpda= ',solpda
      case ("solpdb")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpdb)
        print *,' solpdb= ',solpdb
      case ("solpdc")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpdc)
        print *,' solpdc= ',solpdc
      case ("solpdd")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpdd)
        print *,' solpdd= ',solpdd
      case ("solpde")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpde)
        print *,' solpde= ',solpde
      case ("msmax")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, msmax)
        print *,' msmax= ',msmax
      case ("nchi")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nchi)
        print *,' nchi= ',nchi
      case ("ndift")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ndift)
        print *,' ndift= ',ndift
      case ("negp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, negp)
        print *,' negp= ',negp
      case ("ner")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ner)
        print *,' ner= ',ner
      case ("niso")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, niso)
        print *,' niso= ',niso
      case ("nmesha")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmesha)
        print *,' nmesha= ',nmesha
      case ("nmeshb")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmeshb)
        print *,' nmeshb= ',nmeshb
      case ("nmeshc")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmeshc)
        print *,' nmeshc= ',nmeshc
      case ("nmeshd")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmeshd)
        print *,' nmeshd= ',nmeshd
      case ("nmeshe")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmeshe)
        print *,' nmeshe= ',nmeshe
      case ("npoida")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoida)
        print *,' npoida= ',npoida
      case ("npoidb")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoidb)
        print *,' npoidb= ',npoidb
      case ("npoidc")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoidc)
        print *,' npoidc= ',npoidc
      case ("npoidd")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoidd)
        print *,' npoidd= ',npoidd
      case ("npoide")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoide)
        print *,' npoide= ',npoide
      case ("npoidq")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoidq)
        print *,' npoidq= ',npoidq
      case ("npsi")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npsi)
        print *,' npsi= ',npsi
      case ("ns")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ns)
        print *,' ns= ',ns
      case ("nt")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nt)
        print *,' nt= ',nt
      case ("ntnova")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ntnova)
        print *,' ntnova= ',ntnova
      case ("nv")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nv)
        print *,' nv= ',nv
      case ("nvexp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nvexp)
        print *,' nvexp= ',nvexp

!--   equilibrium_control_param
      case ("equilibrium_control_param")
        temp_pointer => temp_pointer%child
        cycle
      case ("bsfrac")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, bsfrac)
        print *,' bsfrac= ',bsfrac
      case ("cfbal")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cfbal)
        print *,' cfbal= ',cfbal
      case ("cfnress")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cfnress)
        print *,' cfnress= ',cfnress
      case ("cfnresso")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cfnresso)
        print *,' cfnresso= ',cfnresso
      case ("cpress")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cpress)
        print *,' cpress= ',cpress
      case ("cpresso")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cpresso)
        print *,' cpresso= ',cpresso
      case ("cq0")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cq0)
        print *,' cq0= ',cq0
      case ("csspec")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, csspec)
        print *,' csspec= ',csspec
      case ("currt")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, currt)
        print *,' currt= ',currt
      case ("etaei")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, etaei)
        print *,' etaei= ',etaei
      case ("gamma")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, gamma)
        print *,' gamma= ',gamma
      case ("pangle")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, pangle)
        print *,' pangle= ',pangle
      case ("predge")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, predge)
        print *,' predge= ',predge
      case ("psiscl")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, psiscl)
        print *,' psiscl= ',psiscl
      case ("qspec")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, qspec)
        print *,' qspec= ',qspec
      case ("rzion")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rzion)
        print *,' rzion= ',rzion
      case ("scalne")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, scalne)
        print *,' scalne= ',scalne
      case ("scexp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, scexp)
        print *,' scexp= ',scexp
      case ("nbsfun")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbsfun)
        print *,' nbsfun= ',nbsfun
      case ("nbsopt")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbsopt)
        print *,' nbsopt= ',nbsopt
      case ("nbstrp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbstrp)
        print *,' nbstrp= ',nbstrp
      case ("nfunrho")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nfunrho)
        print *,' nfunrho= ',nfunrho
      case ("nrfp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nrfp)
        print *,' nrfp= ',nrfp

!--   plasma_boundary_param
      case ("plasma_boundary_param")
        temp_pointer => temp_pointer%child
        cycle
      case ("aspct")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, aspct)
        print *,' aspct= ',aspct
      case ("beans")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, beans)
        print *,' beans= ',beans
      case ("ceta")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ceta)
        print *,' ceta= ',ceta
      case ("delta")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, delta)
        print *,' delta= ',delta
      case ("elong")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, elong)
        print *,' elong= ',elong
      case ("rc")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rc)
        print *,' rc= ',rc
      case ("rnu")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rnu)
        print *,' rnu= ',rnu
      case ("rz0")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rz0)
        print *,' rz0= ',rz0
      case ("r0")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, r0)
        print *,' r0= ',r0
      case ("sgma")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, sgma)
        print *,' sgma= ',sgma
      case ("theta0")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, theta0)
        print *,' theta0= ',theta0
      case ("triang")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, triang)
        print *,' triang= ',triang
      case ("triplt")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, triplt)
        print *,' triplt= ',triplt
      case ("xi")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, xi)
        print *,' xi= ',xi
      case ("nsurf")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsurf)
        print *,' nsurf= ',nsurf
      case ("nsym")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsym)
        print *,' nsym= ',nsym

!--   profiles_param
      case ("profiles_param")
        temp_pointer => temp_pointer%child
        cycle
      case ("afbs")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), afbs, n_values)
        print *,' afbs= ',afbs
        print *,' n_values= ',n_values
      case ("afbs2")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), afbs2, n_values)
        print *,' afbs2= ',afbs2
        print *,' n_values= ',n_values
      case ("ap")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), ap, n_values)
        print *,' ap= ',ap
        print *,' n_values= ',n_values
      case ("ap2")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), ap2, n_values)
        print *,' ap2= ',ap2
        print *,' n_values= ',n_values
      case ("at")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), at, n_values)
        print *,' at= ',at
        print *,' n_values= ',n_values
      case ("at2")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), at2, n_values)
        print *,' at2= ',at2
        print *,' n_values= ',n_values
      case ("at3")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), at3, n_values)
        print *,' at3= ',at3
        print *,' n_values= ',n_values
      case ("at4")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), at4, n_values)
        print *,' at4= ',at4
        print *,' n_values= ',n_values

      case ("nfunc")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nfunc)
        print *,' nfunc= ',nfunc
      case ("nipr")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nipr)
        print *,' nipr= ',nipr
      case ("npp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npp)
        print *,' npp= ',npp
      case ("nppfun")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nppfun)
        print *,' nppfun= ',nppfun
      case ("nppr")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nppr)
        print *,' nppr= ',nppr
      case ("nprofz")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nprofz)
        print *,' nprofz= ',nprofz
      case ("npropt")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npropt)
        print *,' npropt= ',npropt
      case ("nsour")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsour)
        print *,' nsour= ',nsour
      case ("nsttp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsttp)
        print *,' nsttp= ',nsttp

!--   in_out_control_param
      case ("in_out_control_param")
        temp_pointer => temp_pointer%child
        cycle
      case ("bentaxis")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, bentaxis)
        print *,' bentaxis= ',bentaxis
      case ("bentqprofile")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, bentqprofile)
        print *,' bentqprofile= ',bentqprofile
      case ("bentradius")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, bentradius)
        print *,' bentradius= ',bentradius
      case ("b0exp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, b0exp)
        print *,' b0exp= ',b0exp

      case ("comments")
         ! char array
        if (allocated(temp_pointer%cvalue)) &
             comments = char2str(temp_pointer%cvalue)
        n_values = size(comments)
        ! n_values = len(comments)
        do i=1,n_values
           print *,'comments(',i,') = ',comments(i)
        end do
        print *,' size(comments)= ',size(comments)
        print *,' len(comments)= ',len(comments)

      case ("qvalneo")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), qvalneo, n_values)
        print *,' qvalneo= ',qvalneo
        print *,' n_values= ',n_values

      case ("rboxlen")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rboxlen)
        print *,' rboxlen= ',rboxlen
      case ("rboxlft")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rboxlft)
        print *,' rboxlft= ',rboxlft
      case ("rext")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rext)
        print *,' rext= ',rext
      case ("rpeop")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rpeop)
        print *,' rpeop= ',rpeop
      case ("rz0w")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rz0w)
        print *,' rz0w= ',rz0w
      case ("r0exp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, r0exp)
        print *,' r0exp= ',r0exp
      case ("r0w")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, r0w)
        print *,' r0w= ',r0w
      case ("signb0xp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, signb0xp)
        print *,' signb0xp= ',signb0xp
      case ("signipxp")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, signipxp)
        print *,' signipxp= ',signipxp
      case ("slimit")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, slimit)
        print *,' slimit= ',slimit
      case ("snumber")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, snumber)
        print *,' snumber= ',snumber
      case ("spitzer")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, spitzer)
        print *,' spitzer= ',spitzer

      case ("treeitm")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             treeitm = char2str(temp_pointer%cvalue)
             n_values = len(treeitm)
        print *,' treeitm= ',treeitm
        print *,' n_values= ',n_values

      case ("zboxlen")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, zboxlen)
        print *,' zboxlen= ',zboxlen
      case ("nanal")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nanal)
        print *,' nanal= ',nanal
      case ("nbal")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbal)
        print *,' nbal= ',nbal
      case ("nblc0")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nblc0)
        print *,' nblc0= ',nblc0
      case ("nblopt")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nblopt)
        print *,' nblopt= ',nblopt
      case ("nbpsout")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbpsout)
        print *,' nbpsout= ',nbpsout
      case ("nbsexpq")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbsexpq)
        print *,' nbsexpq= ',nbsexpq
      case ("ncscal")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ncscal)
        print *,' ncscal= ',ncscal
      case ("ndiagop")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ndiagop)
        print *,' ndiagop= ',ndiagop
      case ("ndifps")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ndifps)
        print *,' ndifps= ',ndifps
      case ("neonbqs")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, neonbqs)
        print *,' neonbqs= ',neonbqs
      case ("neqdsk")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, neqdsk)
        print *,' neqdsk= ',neqdsk
      case ("neqdxtpo")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, neqdxtpo)
        print *,' neqdxtpo= ',neqdxtpo
      case ("nfftopt")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nfftopt)
        print *,' nfftopt= ',nfftopt
      case ("nideal")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nideal)
        print *,' nideal= ',nideal
      case ("nitmopt")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nitmopt)
        print *,' nitmopt= ',nitmopt

      case ("nitmrun")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), nitmrun, n_values)
        print *,' nitmrun= ',nitmrun
        print *,' n_values= ',n_values
      case ("nitmshot")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), nitmshot, n_values)
        print *,' nitmshot= ',nitmshot
        print *,' n_values= ',n_values

      case ("nopt")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nopt)
        print *,' nopt= ',nopt
      case ("nplot")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nplot)
        print *,' nplot= ',nplot
      case ("nprpsi")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nprpsi)
        print *,' nprpsi= ',nprpsi
      case ("nrbox")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nrbox)
        print *,' nrbox= ',nrbox
      case ("nrscal")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nrscal)
        print *,' nrscal= ',nrscal
      case ("nsmooth")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsmooth)
        print *,' nsmooth= ',nsmooth
      case ("ntcase")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ntcase)
        print *,' ntcase= ',ntcase
      case ("ntest")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ntest)
        print *,' ntest= ',ntest
      case ("ntmf0")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ntmf0)
        print *,' ntmf0= ',ntmf0
      case ("nturn")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nturn)
        print *,' nturn= ',nturn
      case ("nzbox")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nzbox)
        print *,' nzbox= ',nzbox
      case default
        write(*, *) 'ERROR: invalid parameter', cname
        return_status = 1
 !       exit
    end select
    do
      if (associated(temp_pointer%sibling)) then
        temp_pointer => temp_pointer%sibling
        exit
      end if
      if (associated(temp_pointer%parent, parameter_list%first )) &
        exit outer
      if (associated(temp_pointer%parent)) then
        temp_pointer => temp_pointer%parent
      else
        write(*, *) 'ERROR: broken list.'
        return
      end if
    end do
  end do outer

!-- destroy tree
  call destroy_xml_tree(parameter_list)

  return

end subroutine assign_code_parameters
