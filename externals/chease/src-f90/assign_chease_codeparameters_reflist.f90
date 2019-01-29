subroutine assign_chease_codeparameters_reflist(codeparameters, return_status)

  !-----------------------------------------------------------------------
  ! calls the XML parser for the code parameters and assign the
  ! resulting values to the corresponding variables
  ! NEEDS TO HAVE VARIABLES IN "lower case"
  !TODO: check an alternative and more elegant solution in Perl
  !-----------------------------------------------------------------------

  ! This is the C. Konz option with its own parser but assuming all variable names are those with "ref=" in the xsd file
  ! It does not allow choices and options with defaults. For this use the "choice" chease_schema_choice.xsd and related xml
  ! The latter is used with the module assign_chease_codeparam_xml2eg.f90 and routine assign_codeparameters_choices

  use prec_const

  use euitm_schemas
  use euitm_xml_parser  
  use globals

  implicit none

  type (type_param), intent(in) :: codeparameters
  integer(ikind), intent(out) :: return_status 

  type(tree) :: parameter_list
  type(element), pointer :: temp_pointer
  integer(itm_i4) :: nparm
  integer(itm_i4) :: i, n_values
  character(len = 132) :: cname
  !  integer(ikind) :: ns, NEQDXTPO

  !-- set path to XML schema
  ! file_xml_schema = 'chease_schema.xml'

  return_status = 0      ! no error

  !-- parse xml-string codeparameters%parameters

  call euitm_xml_parse(codeparameters, nparm, parameter_list)
  !  IF (NVERBOSE .GE. 2) print *,'codeparameters%parameters= ',codeparameters%parameters
  IF (NVERBOSE .GE. 2) print *,' nparm= ',nparm
  !  IF (NVERBOSE .GE. 2) print *,'parameter_list= ',parameter_list

  !-- assign variables

  temp_pointer => parameter_list%first

  outer: do
     cname = char2str(temp_pointer%cname)   ! necessary for AIX
     ! IF (NVERBOSE .GE. 2) print *,'cname = ', trim(cname)
     select case (cname)
     case ("parameters")
        temp_pointer => temp_pointer%child
        cycle
     case ("epslon")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, epslon)
        ! IF (NVERBOSE .GE. 2) print *,' epslon= ',epslon
     case ("relax")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, relax)
        ! IF (NVERBOSE .GE. 2) print *,' relax= ',relax
     case ("cocos_in")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, COCOS_IN)
        ! IF (NVERBOSE .GE. 2) print *,' COCOS_IN= ',COCOS_IN
     case ("cocos_out")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, COCOS_OUT)
        ! IF (NVERBOSE .GE. 2) print *,' COCOS_OUT= ',COCOS_OUT
     case ("nverbose")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, NVERBOSE)
        ! IF (NVERBOSE .GE. 2) print *,' NVERBOSE= ',NVERBOSE
     case ("ninmap")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ninmap)
        ! IF (NVERBOSE .GE. 2) print *,' ninmap= ',ninmap
     case ("ninsca")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ninsca)
        ! IF (NVERBOSE .GE. 2) print *,' ninsca= ',ninsca
     case ("nmgaus")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmgaus)
        ! IF (NVERBOSE .GE. 2) print *,' nmgaus= ',nmgaus
     case ("nsgaus")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsgaus)
        ! IF (NVERBOSE .GE. 2) print *,' nsgaus= ',nsgaus
     case ("ntgaus")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ntgaus)
        ! IF (NVERBOSE .GE. 2) print *,' ntgaus= ',ntgaus
        !
        ! Arrays
     case ("aplace")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), aplace, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' aplace= ',aplace
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("awidth")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), awidth, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' awidth= ',awidth
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("bplace")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), bplace, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' bplace= ',bplace
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("bwidth")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), bwidth, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' bwidth= ',bwidth
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("cplace")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), cplace, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' cplace= ',cplace
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("cwidth")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), cwidth, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' cwidth= ',cwidth
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("dplace")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), dplace, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' dplace= ',dplace
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("dwidth")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), dwidth, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' dwidth= ',dwidth
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("eplace")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), eplace, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' eplace= ',eplace
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("ewidth")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), ewidth, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' ewidth= ',ewidth
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("qplace")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), qplace, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' qplace= ',qplace
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("qwidth")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), qwidth, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' qwidth= ',qwidth
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("solpda")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpda)
        ! IF (NVERBOSE .GE. 2) print *,' solpda= ',solpda
     case ("solpdb")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpdb)
        ! IF (NVERBOSE .GE. 2) print *,' solpdb= ',solpdb
     case ("solpdc")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpdc)
        ! IF (NVERBOSE .GE. 2) print *,' solpdc= ',solpdc
     case ("solpdd")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpdd)
        ! IF (NVERBOSE .GE. 2) print *,' solpdd= ',solpdd
     case ("solpde")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpde)
        ! IF (NVERBOSE .GE. 2) print *,' solpde= ',solpde
     case ("solpdpol")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, solpdpol)
        ! IF (NVERBOSE .GE. 2) print *,' solpdpol= ',solpdpol
     case ("msmax")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, msmax)
        ! IF (NVERBOSE .GE. 2) print *,' msmax= ',msmax
     case ("nchi")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nchi)
        ! IF (NVERBOSE .GE. 2) print *,' nchi= ',nchi
     case ("ndift")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ndift)
        ! IF (NVERBOSE .GE. 2) print *,' ndift= ',ndift
     case ("negp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, negp)
        ! IF (NVERBOSE .GE. 2) print *,' negp= ',negp
     case ("ner")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ner)
        ! IF (NVERBOSE .GE. 2) print *,' ner= ',ner
     case ("niso")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, niso)
        ! IF (NVERBOSE .GE. 2) print *,' niso= ',niso
     case ("nmesha")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmesha)
        ! IF (NVERBOSE .GE. 2) print *,' nmesha= ',nmesha
     case ("nmeshb")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmeshb)
        ! IF (NVERBOSE .GE. 2) print *,' nmeshb= ',nmeshb
     case ("nmeshc")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmeshc)
        ! IF (NVERBOSE .GE. 2) print *,' nmeshc= ',nmeshc
     case ("nmeshd")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmeshd)
        ! IF (NVERBOSE .GE. 2) print *,' nmeshd= ',nmeshd
     case ("nmeshe")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmeshe)
        ! IF (NVERBOSE .GE. 2) print *,' nmeshe= ',nmeshe
     case ("nmeshpol")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmeshpol)
        ! IF (NVERBOSE .GE. 2) print *,' nmeshpol= ',nmeshpol
     case ("nmeshpolexp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nmeshpolexp)
        ! IF (NVERBOSE .GE. 2) print *,' nmeshpolexp= ',nmeshpolexp
     case ("npoida")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoida)
        ! IF (NVERBOSE .GE. 2) print *,' npoida= ',npoida
     case ("npoidb")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoidb)
        ! IF (NVERBOSE .GE. 2) print *,' npoidb= ',npoidb
     case ("npoidc")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoidc)
        ! IF (NVERBOSE .GE. 2) print *,' npoidc= ',npoidc
     case ("npoidd")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoidd)
        ! IF (NVERBOSE .GE. 2) print *,' npoidd= ',npoidd
     case ("npoide")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoide)
        ! IF (NVERBOSE .GE. 2) print *,' npoide= ',npoide
     case ("npoidq")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npoidq)
        ! IF (NVERBOSE .GE. 2) print *,' npoidq= ',npoidq
     case ("npsi")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npsi)
        ! IF (NVERBOSE .GE. 2) print *,' npsi= ',npsi
     case ("ns")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ns)
        ! IF (NVERBOSE .GE. 2) print *,' ns= ',ns
     case ("nt")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nt)
        ! IF (NVERBOSE .GE. 2) print *,' nt= ',nt
     case ("ntnova")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ntnova)
        ! IF (NVERBOSE .GE. 2) print *,' ntnova= ',ntnova
     case ("nv")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nv)
        ! IF (NVERBOSE .GE. 2) print *,' nv= ',nv
     case ("nvexp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nvexp)
        ! IF (NVERBOSE .GE. 2) print *,' nvexp= ',nvexp

     case ("bsfrac")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, bsfrac)
        ! IF (NVERBOSE .GE. 2) print *,' bsfrac= ',bsfrac
     case ("cfbal")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cfbal)
        ! IF (NVERBOSE .GE. 2) print *,' cfbal= ',cfbal
     case ("cfnress")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cfnress)
        ! IF (NVERBOSE .GE. 2) print *,' cfnress= ',cfnress
     case ("cfnresso")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cfnresso)
        ! IF (NVERBOSE .GE. 2) print *,' cfnresso= ',cfnresso
     case ("cpress")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cpress)
        ! IF (NVERBOSE .GE. 2) print *,' cpress= ',cpress
     case ("cpresso")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cpresso)
        ! IF (NVERBOSE .GE. 2) print *,' cpresso= ',cpresso
     case ("cq0")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, cq0)
        ! IF (NVERBOSE .GE. 2) print *,' cq0= ',cq0
     case ("csspec")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, csspec)
        ! IF (NVERBOSE .GE. 2) print *,' csspec= ',csspec
     case ("currt")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, currt)
        ! IF (NVERBOSE .GE. 2) print *,' currt= ',currt
     case ("etaei")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, etaei)
        ! IF (NVERBOSE .GE. 2) print *,' etaei= ',etaei
     case ("gamma")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, gamma)
        ! IF (NVERBOSE .GE. 2) print *,' gamma= ',gamma
     case ("pangle")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, pangle)
        ! IF (NVERBOSE .GE. 2) print *,' pangle= ',pangle
     case ("predge")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, predge)
        ! IF (NVERBOSE .GE. 2) print *,' predge= ',predge
     case ("psiscl")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, psiscl)
        ! IF (NVERBOSE .GE. 2) print *,' psiscl= ',psiscl
     case ("qspec")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, qspec)
        ! IF (NVERBOSE .GE. 2) print *,' qspec= ',qspec
     case ("rzion")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rzion)
        ! IF (NVERBOSE .GE. 2) print *,' rzion= ',rzion
     case ("scalne")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, scalne)
        ! IF (NVERBOSE .GE. 2) print *,' scalne= ',scalne
     case ("scexp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, scexp)
        ! IF (NVERBOSE .GE. 2) print *,' scexp= ',scexp
     case ("nbsfun")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbsfun)
        ! IF (NVERBOSE .GE. 2) print *,' nbsfun= ',nbsfun
     case ("nbsopt")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbsopt)
        ! IF (NVERBOSE .GE. 2) print *,' nbsopt= ',nbsopt
     case ("nbstrp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbstrp)
        ! IF (NVERBOSE .GE. 2) print *,' nbstrp= ',nbstrp
     case ("nfunrho")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nfunrho)
        IF (NVERBOSE .GE. 2) print *,' nfunrho= ',nfunrho
     case ("nrhomesh")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nrhomesh)
        IF (NVERBOSE .GE. 2) print *,' nrhomesh= ',nrhomesh
     case ("nrfp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nrfp)
        ! IF (NVERBOSE .GE. 2) print *,' nrfp= ',nrfp

     case ("aspct")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, aspct)
        ! IF (NVERBOSE .GE. 2) print *,' aspct= ',aspct
     case ("beans")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, beans)
        ! IF (NVERBOSE .GE. 2) print *,' beans= ',beans
     case ("ceta")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ceta)
        ! IF (NVERBOSE .GE. 2) print *,' ceta= ',ceta
     case ("delta")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, delta)
        ! IF (NVERBOSE .GE. 2) print *,' delta= ',delta
     case ("elong")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, elong)
        ! IF (NVERBOSE .GE. 2) print *,' elong= ',elong
     case ("rc")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rc)
        ! IF (NVERBOSE .GE. 2) print *,' rc= ',rc
     case ("rnu")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rnu)
        ! IF (NVERBOSE .GE. 2) print *,' rnu= ',rnu
     case ("rz0")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rz0)
        ! IF (NVERBOSE .GE. 2) print *,' rz0= ',rz0
     case ("r0")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, r0)
        ! IF (NVERBOSE .GE. 2) print *,' r0= ',r0
     case ("sgma")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, sgma)
        ! IF (NVERBOSE .GE. 2) print *,' sgma= ',sgma
     case ("tensprof")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, tensprof)
        ! IF (NVERBOSE .GE. 2) print *,' tensprof= ',tensprof
     case ("tensbnd")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, tensbnd)
        ! IF (NVERBOSE .GE. 2) print *,' tensbnd= ',tensbnd
     case ("theta0")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, theta0)
        ! IF (NVERBOSE .GE. 2) print *,' theta0= ',theta0
     case ("triang")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, triang)
        ! IF (NVERBOSE .GE. 2) print *,' triang= ',triang
     case ("triplt")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, triplt)
        ! IF (NVERBOSE .GE. 2) print *,' triplt= ',triplt
     case ("xi")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, xi)
        ! IF (NVERBOSE .GE. 2) print *,' xi= ',xi
     case ("nsurf")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsurf)
        ! IF (NVERBOSE .GE. 2) print *,' nsurf= ',nsurf
     case ("nsym")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsym)
        ! IF (NVERBOSE .GE. 2) print *,' nsym= ',nsym

     case ("afbs")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), afbs, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' afbs= ',afbs
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("afbs2")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), afbs2, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' afbs2= ',afbs2
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("ap")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), ap, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' ap= ',ap
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("ap2")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), ap2, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' ap2= ',ap2
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("at")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), at, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' at= ',at
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("at2")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), at2, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' at2= ',at2
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("at3")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), at3, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' at3= ',at3
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("at4")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), at4, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' at4= ',at4
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values

     case ("nfunc")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nfunc)
        ! IF (NVERBOSE .GE. 2) print *,' nfunc= ',nfunc
     case ("nipr")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nipr)
        ! IF (NVERBOSE .GE. 2) print *,' nipr= ',nipr
     case ("npp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npp)
        ! IF (NVERBOSE .GE. 2) print *,' npp= ',npp
     case ("nppfun")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nppfun)
        ! IF (NVERBOSE .GE. 2) print *,' nppfun= ',nppfun
     case ("nppr")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nppr)
        ! IF (NVERBOSE .GE. 2) print *,' nppr= ',nppr
     case ("nprofz")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nprofz)
        ! IF (NVERBOSE .GE. 2) print *,' nprofz= ',nprofz
     case ("nprof2d")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nprof2d)
        ! IF (NVERBOSE .GE. 2) print *,' nprof2d= ',nprof2d
     case ("npropt")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, npropt)
        ! IF (NVERBOSE .GE. 2) print *,' npropt= ',npropt
     case ("nsour")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsour)
        ! IF (NVERBOSE .GE. 2) print *,' nsour= ',nsour
     case ("nsttp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsttp)
        ! IF (NVERBOSE .GE. 2) print *,' nsttp= ',nsttp

     case ("bentaxis")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, bentaxis)
        ! IF (NVERBOSE .GE. 2) print *,' bentaxis= ',bentaxis
     case ("bentqprofile")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, bentqprofile)
        ! IF (NVERBOSE .GE. 2) print *,' bentqprofile= ',bentqprofile
     case ("bentradius")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, bentradius)
        ! IF (NVERBOSE .GE. 2) print *,' bentradius= ',bentradius
     case ("b0exp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, b0exp)
        ! IF (NVERBOSE .GE. 2) print *,' b0exp= ',b0exp

     case ("comments")
        ! char array
        if (allocated(temp_pointer%cvalue)) &
             comments = char2str(temp_pointer%cvalue)
        n_values = size(comments)
        ! n_values = len(comments)
        do i=1,n_values
           ! IF (NVERBOSE .GE. 2) print *,'comments(',i,') = ',comments(i)
        end do
        ! IF (NVERBOSE .GE. 2) print *,' size(comments)= ',size(comments)
        ! IF (NVERBOSE .GE. 2) print *,' len(comments)= ',len(comments)

     case ("qvalneo")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), qvalneo, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' qvalneo= ',qvalneo
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values

     case ("rboxlen")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rboxlen)
        ! IF (NVERBOSE .GE. 2) print *,' rboxlen= ',rboxlen
     case ("rboxlft")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rboxlft)
        ! IF (NVERBOSE .GE. 2) print *,' rboxlft= ',rboxlft
     case ("rext")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rext)
        ! IF (NVERBOSE .GE. 2) print *,' rext= ',rext
     case ("rpeop")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rpeop)
        ! IF (NVERBOSE .GE. 2) print *,' rpeop= ',rpeop
     case ("rz0w")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, rz0w)
        ! IF (NVERBOSE .GE. 2) print *,' rz0w= ',rz0w
     case ("r0exp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, r0exp)
        ! IF (NVERBOSE .GE. 2) print *,' r0exp= ',r0exp
     case ("r0w")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, r0w)
        ! IF (NVERBOSE .GE. 2) print *,' r0w= ',r0w
     case ("signb0xp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, signb0xp)
        ! IF (NVERBOSE .GE. 2) print *,' signb0xp= ',signb0xp
     case ("signipxp")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, signipxp)
        ! IF (NVERBOSE .GE. 2) print *,' signipxp= ',signipxp
     case ("slimit")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, slimit)
        ! IF (NVERBOSE .GE. 2) print *,' slimit= ',slimit
     case ("snumber")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, snumber)
        ! IF (NVERBOSE .GE. 2) print *,' snumber= ',snumber
     case ("shift_p")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, shift_p)
        ! IF (NVERBOSE .GE. 2) print *,' shift_p= ',shift_p

     case ("treeitm")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             treeitm = char2str(temp_pointer%cvalue)
        n_values = len(treeitm)
        ! IF (NVERBOSE .GE. 2) print *,' treeitm= ',treeitm
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values

     case ("zboxlen")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, zboxlen)
        ! IF (NVERBOSE .GE. 2) print *,' zboxlen= ',zboxlen
     case ("zboxmid")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, zboxmid)
        ! IF (NVERBOSE .GE. 2) print *,' zboxmid= ',zboxmid
     case ("nanal")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nanal)
        ! IF (NVERBOSE .GE. 2) print *,' nanal= ',nanal
     case ("nbal")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbal)
        ! IF (NVERBOSE .GE. 2) print *,' nbal= ',nbal
     case ("nblc0")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nblc0)
        ! IF (NVERBOSE .GE. 2) print *,' nblc0= ',nblc0
     case ("nblopt")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nblopt)
        ! IF (NVERBOSE .GE. 2) print *,' nblopt= ',nblopt
     case ("nbpsout")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbpsout)
        ! IF (NVERBOSE .GE. 2) print *,' nbpsout= ',nbpsout
     case ("nbsexpq")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nbsexpq)
        ! IF (NVERBOSE .GE. 2) print *,' nbsexpq= ',nbsexpq
     case ("ncscal")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ncscal)
        ! IF (NVERBOSE .GE. 2) print *,' ncscal= ',ncscal
     case ("ndiagop")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ndiagop)
        ! IF (NVERBOSE .GE. 2) print *,' ndiagop= ',ndiagop
     case ("ndifps")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ndifps)
        ! IF (NVERBOSE .GE. 2) print *,' ndifps= ',ndifps
     case ("neonbqs")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, neonbqs)
        ! IF (NVERBOSE .GE. 2) print *,' neonbqs= ',neonbqs
     case ("neqdsk")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, neqdsk)
        ! IF (NVERBOSE .GE. 2) print *,' neqdsk= ',neqdsk
     case ("neqdxtpo")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, neqdxtpo)
        ! IF (NVERBOSE .GE. 2) print *,' neqdxtpo= ',neqdxtpo
     case ("nfftopt")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nfftopt)
        ! IF (NVERBOSE .GE. 2) print *,' nfftopt= ',nfftopt
     case ("nideal")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nideal)
        ! IF (NVERBOSE .GE. 2) print *,' nideal= ',nideal
     case ("nitmopt")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nitmopt)
        ! IF (NVERBOSE .GE. 2) print *,' nitmopt= ',nitmopt

     case ("nitmrun")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), nitmrun, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' nitmrun= ',nitmrun
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values
     case ("nitmshot")
        ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), nitmshot, n_values)
        ! IF (NVERBOSE .GE. 2) print *,' nitmshot= ',nitmshot
        ! IF (NVERBOSE .GE. 2) print *,' n_values= ',n_values

     case ("nopt")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nopt)
        ! IF (NVERBOSE .GE. 2) print *,' nopt= ',nopt
     case ("nplot")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nplot)
        ! IF (NVERBOSE .GE. 2) print *,' nplot= ',nplot
     case ("nprpsi")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nprpsi)
        ! IF (NVERBOSE .GE. 2) print *,' nprpsi= ',nprpsi
     case ("nrbox")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nrbox)
        ! IF (NVERBOSE .GE. 2) print *,' nrbox= ',nrbox
     case ("nrscal")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nrscal)
        ! IF (NVERBOSE .GE. 2) print *,' nrscal= ',nrscal
     case ("nsmooth")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nsmooth)
        ! IF (NVERBOSE .GE. 2) print *,' nsmooth= ',nsmooth
     case ("ntcase")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ntcase)
        ! IF (NVERBOSE .GE. 2) print *,' ntcase= ',ntcase
     case ("ntest")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ntest)
        ! IF (NVERBOSE .GE. 2) print *,' ntest= ',ntest
     case ("ntmf0")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ntmf0)
        ! IF (NVERBOSE .GE. 2) print *,' ntmf0= ',ntmf0
     case ("nturn")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nturn)
        ! IF (NVERBOSE .GE. 2) print *,' nturn= ',nturn
     case ("nzbox")
        ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, nzbox)
        ! IF (NVERBOSE .GE. 2) print *,' nzbox= ',nzbox
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

end subroutine assign_chease_codeparameters_reflist
