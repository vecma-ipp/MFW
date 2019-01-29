! FORTRAN 90 subroutine calls is text - It can be viewed as source in a browser and then saved

module euITM_putroutines
  
!---------------------------------------------------------------------------------------------------------
! module containing euITM subroutines to put structured data
!  Version 1 - updated 24 November 2005, Jo Lister, Guido Huysmans
!---------------------------------------------------------------------------------------------------------

  use euITM_access
  use euITM_utilities
  
  interface euitmput  ! defines the generic names for the reading routines
     module procedure euitmput_eq, &
          euitmput_vessel,  euitmput_toroidalfield, euitmput_tiles,      euitmput_thomson, &
          euitmput_channel, euitmput_pfsupplies,    euitmput_supply,     euitmput_pfpassive, &
          euitmput_pfcoils, euitmput_coil,          euitmput_pfcircuits, euitmput_circuit, &
          euitmput_msediag, euitmput_magdiag,       euitmput_bpol,       euitmput_flux, &
          euitmput_lineintegraldiag, euitmput_chorddata, euitmput_limiter, euitmput_ironmodel, &
          euitmput_signalslice, euitmput_segment,   euitmput_emcalc,      euitmput_plasmagrid, euitmput_divertor, &
          euitmput_controllers, euitmput_controller, euitmput_pfgeometry, euitmput_signal
  endinterface
  
contains
  
  subroutine euitmput_case( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_case ) :: thisstructure   ! Synonym definitions
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'name1' , thisstructure%name1 , ifail )    !C
    call euitmput( trim(path)//'name2' , thisstructure%name2 , ifail )    !C

    return
  endsubroutine euitmput_case

  subroutine euitmput_update( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_update ) :: thisstructure   ! Modification made to this block
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'author' , thisstructure%author , ifail )    !C
    call euitmput( trim(path)//'date' , thisstructure%date , ifail )    !C
    call euitmput( trim(path)//'comment' , thisstructure%comment , ifail )    !C

    return
  endsubroutine euitmput_update

  subroutine euitmput_synonymtable( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_synonymtable ) :: thisstructure   ! List of synonyms in this block
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'title' , thisstructure%title , ifail )    !C
    call euitmput( trim(path)//'ncase.dat' , thisstructure%ncase%dat , ifail )    !D
    allocate( thisstructure%case( thisstructure%ncase%dat ) )
    do iloop = 1 , thisstructure%ncase%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_case( trim(path)//'case.' //NNN//'.', thisstructure%case(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_synonymtable

  subroutine euitmput_tags( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_tags ) :: thisstructure   ! List of tags in this block
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'name1' , thisstructure%name1 , ifail )    !C
    call euitmput( trim(path)//'name2' , thisstructure%name2 , ifail )    !C

    return
  endsubroutine euitmput_tags

  subroutine euitmput_ref( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_ref ) :: thisstructure   ! Reference for online documentation
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'descrip' , thisstructure%descrip , ifail )    !C
    call euitmput( trim(path)//'url_type' , thisstructure%url_type , ifail )    !C
    call euitmput( trim(path)//'urlaccess' , thisstructure%urlaccess , ifail )    !C

    return
  endsubroutine euitmput_ref

  subroutine euitmput_datainfo( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_datainfo ) :: thisstructure   ! Generic information on a data item
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'sourcemethod' , thisstructure%sourcemethod , ifail )    !C
    call euitmput( trim(path)//'sourceexpres' , thisstructure%sourceexpression , ifail )    !C

    return
  endsubroutine euitmput_datainfo

  subroutine euitmput_putinfo( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_putinfo ) :: thisstructure   ! Generic put information for a data item or structure
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'rights' , thisstructure%rights , ifail )    !C
    call euitmput( trim(path)//'putmethod' , thisstructure%putmethod , ifail )    !C
    call euitmput( trim(path)//'putaccess' , thisstructure%putaccess , ifail )    !C
    call euitmput( trim(path)//'putlocation' , thisstructure%putlocation , ifail )    !C
    call euitmput( trim(path)//'putby' , thisstructure%putby , ifail )    !C
    call euitmput( trim(path)//'putdate' , thisstructure%putdate , ifail )    !C

    return
  endsubroutine euitmput_putinfo

  subroutine euitmput_header( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_header ) :: thisstructure   ! Generic header
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'title' , thisstructure%title , ifail )    !C
    call euitmput( trim(path)//'shortname' , thisstructure%shortname , ifail )    !C
    call euitmput( trim(path)//'briefdescrip' , thisstructure%briefdescrip , ifail )    !C
    call euitmput( trim(path)//'longdescrip' , thisstructure%longdescrip , ifail )    !C
    call euitmput( trim(path)//'nupdate.dat' , thisstructure%nupdate%dat , ifail )    !D
    allocate( thisstructure%update( thisstructure%nupdate%dat ) )
    do iloop = 1 , thisstructure%nupdate%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_update( trim(path)//'update.' //NNN//'.', thisstructure%update(iloop) )        !B2  multiple structure
    enddo
    call euitmput( trim(path)//'nsynonymtabl.dat' , thisstructure%nsynonymtable%dat , ifail )    !D
    allocate( thisstructure%synonymtable( thisstructure%nsynonymtable%dat ) )
    do iloop = 1 , thisstructure%nsynonymtable%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_synonymtable( trim(path)//'synonymtable.' //NNN//'.', thisstructure%synonymtable(iloop) )        !B2  multiple structure
    enddo
    call euitmput( trim(path)//'ntags.dat' , thisstructure%ntags%dat , ifail )    !D
    allocate( thisstructure%tags( thisstructure%ntags%dat ) )
    do iloop = 1 , thisstructure%ntags%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_tags( trim(path)//'tags.' //NNN//'.', thisstructure%tags(iloop) )        !B2  multiple structure
    enddo
    call euitmput( trim(path)//'nref.dat' , thisstructure%nref%dat , ifail )    !D
    allocate( thisstructure%ref( thisstructure%nref%dat ) )
    do iloop = 1 , thisstructure%nref%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_ref( trim(path)//'ref.' //NNN//'.', thisstructure%ref(iloop) )        !B2  multiple structure
    enddo
    call euitmput( trim(path)//'nnorm.dat' , thisstructure%nnorm%dat , ifail )    !D
    call euitmput( trim(path)//'norm' , thisstructure%norm , ifail )    !C

    return
  endsubroutine euitmput_header

  subroutine euitmput_reggrid( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_reggrid ) :: thisstructure   ! 
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'ndim1.dat' , thisstructure%ndim1%dat , ifail )    !D
    call euitmput( trim(path)//'ndim2.dat' , thisstructure%ndim2%dat , ifail )    !D
    call euitmput( trim(path)//'dim1.dat' , thisstructure%dim1%dat , ifail )    !D
    call euitmput( trim(path)//'dim2.dat' , thisstructure%dim2%dat , ifail )    !D

    return
  endsubroutine euitmput_reggrid

  subroutine euitmput_rzvector( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_rzvector ) :: thisstructure   ! 
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'npoints.dat' , thisstructure%npoints%dat , ifail )    !D

    if (thisstructure%npoints%dat .le. 0 ) return

    call euitmput( trim(path)//'rz.dat' , thisstructure%rz%dat , ifail )    !D

    return
  endsubroutine euitmput_rzvector

  subroutine euitmput_signal( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_signal ) :: thisstructure   ! 
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'signalname.dat' , thisstructure%signalname%dat , ifail )    !D
    call euitmput( trim(path)//'signaltype.dat' , thisstructure%signaltype%dat , ifail )    !D
    call euitmput( trim(path)//'id' , thisstructure%id , ifail )    !C
    call euitmput( trim(path)//'time.dat' , thisstructure%time%dat , ifail )    !D
    call euitmput( trim(path)//'values.dat' , thisstructure%values%dat , ifail )    !D
    call euitmput( trim(path)//'sourcemethod.dat' , thisstructure%sourcemethod%dat , ifail )    !D
    call euitmput( trim(path)//'sourceexpres.dat' , thisstructure%sourceexpression%dat , ifail )    !D
    call euitmput( trim(path)//'errortype.dat' , thisstructure%errortype%dat , ifail )    !D
    call euitmput( trim(path)//'range.dat' , thisstructure%range%dat , ifail )    !D
    call euitmput( trim(path)//'fractioneps.dat' , thisstructure%fractioneps%dat , ifail )    !D
    call euitmput( trim(path)//'sigma.dat' , thisstructure%sigma%dat , ifail )    !D
    call euitmput( trim(path)//'tobecomplete.dat' , thisstructure%tobecompleted%dat , ifail )    !D

    return
  endsubroutine euitmput_signal

  subroutine euitmput_pfgeometry( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_pfgeometry ) :: thisstructure   ! Shape of a PF Coil or Element
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'type.dat' , thisstructure%type%dat , ifail )    !D
    call euitmput_rzvector( trim(path)//'rzcoordinate.' , thisstructure%rzcoordinates )  !M1
    call euitmput( trim(path)//'rzdrdz.dat' , thisstructure%rzdrdz%dat , ifail )    !D

    return
  endsubroutine euitmput_pfgeometry


  subroutine euitmput_pfelement( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_pfelement ) :: thisstructure   ! Axisymmetric conductor description
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'name.dat' , thisstructure%name%dat , ifail )    !D
    call euitmput( trim(path)//'turnsign.dat' , thisstructure%turnsign%dat , ifail )    !D
    call euitmput( trim(path)//'id' , thisstructure%id , ifail )    !C
    call euitmput( trim(path)//'area.dat' , thisstructure%area%dat , ifail )    !D
    call euitmput( trim(path)//'res.dat' , thisstructure%res%dat , ifail )    !D
    call euitmput( trim(path)//'resistivity.dat' , thisstructure%resistivity%dat , ifail )    !D
    call euitmput_pfgeometry(trim(path)//'pfgeometry.' , thisstructure%pfgeometry )      !A

    return
  endsubroutine euitmput_pfelement

  subroutine euitmput_methods( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_methods ) :: thisstructure   ! A particular method
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'methodname' , thisstructure%methodname , ifail )    !C
    call euitmput( trim(path)//'methodinit' , thisstructure%methodinit , ifail )    !C
    call euitmput( trim(path)//'methodclose' , thisstructure%methodclose , ifail )    !C
    call euitmput( trim(path)//'methodid' , thisstructure%methodid , ifail )    !C

    return
  endsubroutine euitmput_methods

  subroutine euitmput_localaccess( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_localaccess ) :: thisstructure   ! Access methods for the local data. This data has to be 
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'nmethods.dat' , thisstructure%nmethods%dat , ifail )    !D
    allocate( thisstructure%methods( thisstructure%nmethods%dat ) )
    do iloop = 1 , thisstructure%nmethods%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_methods( trim(path)//'methods.' //NNN//'.', thisstructure%methods(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_localaccess

  subroutine euitmput_statespace( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_statespace ) :: thisstructure   ! Statespace controller in discrete time
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,*) ' euitmput_statespace : ',trim(path0)


    path = path0

    call euitmput( trim(path)//'description.dat' , thisstructure%description%dat , ifail )    !D
    call euitmput( trim(path)//'A.dat' , thisstructure%A%dat , ifail )    !D
    call euitmput( trim(path)//'B.dat' , thisstructure%B%dat , ifail )    !D
    call euitmput( trim(path)//'C.dat' , thisstructure%C%dat , ifail )    !D
    call euitmput( trim(path)//'D.dat' , thisstructure%D%dat , ifail )    !D
    call euitmput( trim(path)//'deltat.dat' , thisstructure%deltat%dat , ifail )    !D

    return
  endsubroutine euitmput_statespace

  subroutine euitmput_controller( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_controller ) :: thisstructure   ! An example of a controller
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_controller : ',trim(path0)

    path = path0

    call euitmput( trim(path)//'name.dat' , thisstructure%name%dat , ifail )    !D
    !call euitmput( trim(path)//'class.dat' , thisstructure%class%dat , ifail )    !D
    !call euitmput( trim(path)//'input.dat' , thisstructure%input%dat , ifail )    !D
    !call euitmput( trim(path)//'output.dat' , thisstructure%output%dat , ifail )    !D
    call euitmput_statespace( trim(path)//'statespace.' , thisstructure%statespace )        !B1  single structure 
    call euitmput( trim(path)//'purpose.dat' , thisstructure%purpose%dat , ifail )    !D

    return
  endsubroutine euitmput_controller

  subroutine euitmput_controllers( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_controllers ) :: thisstructure   ! Feedback controllers
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_controllers : ',trim(path0)

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'ncontroller.dat' , thisstructure%ncontroller%dat , ifail )    !D
    allocate( thisstructure%controller( thisstructure%ncontroller%dat ) )
    do iloop = 1 , thisstructure%ncontroller%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_controller( trim(path)//'controller.' //NNN//'.', thisstructure%controller(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_controllers

  subroutine euitmput_divertor( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_divertor ) :: thisstructure   ! Mechanical structure of the divertor - to be completed
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_divertor : ',trim(path0)

    path = path0

    call euitmput_header( path , thisstructure%header )      !A

    return
  endsubroutine euitmput_divertor

  subroutine euitmput_plasmagrid( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_plasmagrid ) :: thisstructure   ! Basic plasma grid
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_plasmagrid : ',trim(path0)

    path = path0

    call euitmput_reggrid( trim(path)//'grid.' , thisstructure%grid )  !M1
    call euitmput( trim(path)//'name' , thisstructure%name , ifail )    !C
    call euitmput( trim(path)//'dflux_grid.dat' , thisstructure%dflux_grid%dat , ifail )    !D
    call euitmput( trim(path)//'bpol_grid.dat' , thisstructure%bpol_grid%dat , ifail )    !D
    call euitmput( trim(path)//'pol_grid.dat' , thisstructure%pol_grid%dat , ifail )    !D
    call euitmput( trim(path)//'grid_grid.dat' , thisstructure%grid_grid%dat , ifail )    !D

    return
  endsubroutine euitmput_plasmagrid

  subroutine euitmput_emcalc( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_emcalc ) :: thisstructure   ! Block containing calculated electromagnetic values
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_emcalc : ',trim(path0)

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'modelnrnz.dat' , thisstructure%modelnrnz%dat , ifail )    !D
    call euitmput( trim(path)//'npolcurrents.dat' , thisstructure%npolcurrents%dat , ifail )    !D
    call euitmput( trim(path)//'mut_pol_pol.dat' , thisstructure%mut_pol_pol%dat , ifail )    !D
    call euitmput( trim(path)//'res_pol.dat' , thisstructure%res_pol%dat , ifail )    !D
    call euitmput( trim(path)//'br_pol_pol.dat' , thisstructure%br_pol_pol%dat , ifail )    !D
    call euitmput( trim(path)//'bz_pol_pol.dat' , thisstructure%bz_pol_pol%dat , ifail )    !D
    call euitmput( trim(path)//'bpol_pol.dat' , thisstructure%bpol_pol%dat , ifail )    !D
    call euitmput( trim(path)//'dflux_pol.dat' , thisstructure%dflux_pol%dat , ifail )    !D
    call euitmput( trim(path)//'nplasmagrid.dat' , thisstructure%nplasmagrid%dat , ifail )    !D
    allocate( thisstructure%plasmagrid( thisstructure%nplasmagrid%dat ) )
    do iloop = 1 , thisstructure%nplasmagrid%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_plasmagrid( trim(path)//'plasmagrid.' //NNN//'.', thisstructure%plasmagrid(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_emcalc

  subroutine euitmput_eqconstrain( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_eqconstrain ) :: thisstructure   ! Additional constraints for reconstruction needs more da
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_eqconstrain : ',trim(path0)

    path = path0

    call euitmput( trim(path)//'q0.dat' , thisstructure%q0%dat , ifail )    !D
    call euitmput( trim(path)//'totalpressur.dat' , thisstructure%totalpressure%dat , ifail )    !D
    call euitmput( trim(path)//'bpoloidal.dat' , thisstructure%bpoloidal%dat , ifail )    !D
    call euitmput( trim(path)//'modb.dat' , thisstructure%modb%dat , ifail )    !D
    call euitmput_rzvector( trim(path)//'lcfs.' , thisstructure%lcfs )  !M1
    call euitmput_rzvector( trim(path)//'isofluxpts.' , thisstructure%isofluxpts(1) )  !M2
    call euitmput( trim(path)//'xptlower.dat' , thisstructure%xptlower%dat , ifail )    !D
    call euitmput( trim(path)//'xptupper.dat' , thisstructure%xptupper%dat , ifail )    !D

    return
  endsubroutine euitmput_eqconstrain

  subroutine euitmput_coord_sys( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_coord_sys ) :: thisstructure   ! flux surface coordinate system on a square grid
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_coord_sys : ',trim(path0)

    path = path0

    call euitmput( trim(path)//'type.dat' , thisstructure%type%dat , ifail )    !D
    call euitmput_reggrid( trim(path)//'grid.' , thisstructure%grid )  !M1
    call euitmput( trim(path)//'R.dat' , thisstructure%R%dat , ifail )    !D
    call euitmput( trim(path)//'Z.dat' , thisstructure%Z%dat , ifail )    !D
    call euitmput( trim(path)//'jacobian.dat' , thisstructure%jacobian%dat , ifail )    !D
    call euitmput( trim(path)//'g_11.dat' , thisstructure%g_11%dat , ifail )    !D
    call euitmput( trim(path)//'g_12.dat' , thisstructure%g_12%dat , ifail )    !D
    call euitmput( trim(path)//'g_22.dat' , thisstructure%g_22%dat , ifail )    !D
    call euitmput( trim(path)//'g_33.dat' , thisstructure%g_33%dat , ifail )    !D

    return
  endsubroutine euitmput_coord_sys

  subroutine euitmput_eqfittedparam( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_eqfittedparam ) :: thisstructure   ! Work to be done - include all the values of the adjuste
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_eqfittedparam : ',trim(path0)

    path = path0

    call euitmput( trim(path)//'TBD.dat' , thisstructure%TBD%dat , ifail )    !D

    return
  endsubroutine euitmput_eqfittedparam

  subroutine euitmput_fixbound( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_fixbound ) :: thisstructure   ! Work to be done - include the definition of the fixed b
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_fixbound : ',trim(path0)

    path = path0

    call euitmput_rzvector( trim(path)//'boundary.' , thisstructure%boundary )  !M1

    return
  endsubroutine euitmput_fixbound

  subroutine euitmput_eqgeometry( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_eqgeometry ) :: thisstructure   ! Geometry of the plasma boundary
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_eqgeometry : ',trim(path0)

    path = path0

    call euitmput( trim(path)//'boundarytype.dat' , thisstructure%boundarytype%dat , ifail )    !D
    call euitmput_rzvector( trim(path)//'boundary.' , thisstructure%boundary )  !M1
    call euitmput( trim(path)//'geom_axis.dat' , thisstructure%geom_axis%dat , ifail )    !D
    call euitmput( trim(path)//'a_minor.dat' , thisstructure%a_minor%dat , ifail )    !D
    call euitmput( trim(path)//'elongation.dat' , thisstructure%elongation%dat , ifail )    !D
    call euitmput( trim(path)//'tria_upper.dat' , thisstructure%tria_upper%dat , ifail )    !D
    call euitmput( trim(path)//'tria_lower.dat' , thisstructure%tria_lower%dat , ifail )    !D
    call euitmput( trim(path)//'nxpts.dat' , thisstructure%nxpts%dat , ifail )    !D
    call euitmput_rzvector( trim(path)//'xpts.' , thisstructure%xpts )  !M1
    call euitmput( trim(path)//'left_low_str.dat' , thisstructure%left_low_strike%dat , ifail )    !D
    call euitmput( trim(path)//'right_low_st.dat' , thisstructure%right_low_strike%dat , ifail )    !D
    call euitmput( trim(path)//'left_up_stri.dat' , thisstructure%left_up_strike%dat , ifail )    !D
    call euitmput( trim(path)//'right_up_str.dat' , thisstructure%right_up_strike%dat , ifail )    !D
    call euitmput( trim(path)//'active_limit.dat' , thisstructure%active_limiter%dat , ifail )    !D

    return
  endsubroutine euitmput_eqgeometry

  subroutine euitmput_mag_axis( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_mag_axis ) :: thisstructure   ! Magnetic axis values
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_global_parameters : ',trim(path0)

    path = path0

    call euitmput( trim(path)//'position.dat' , thisstructure%position%dat , ifail )    !D
    call euitmput( trim(path)//'bphi_mag_ax.dat' , thisstructure%bphi_mag_ax%dat , ifail )    !D
    call euitmput( trim(path)//'q_mag_ax.dat' , thisstructure%q_mag_ax%dat , ifail )    !D

    return
  endsubroutine euitmput_mag_axis

  subroutine euitmput_global_parameters( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_global_parameters ) :: thisstructure   ! 0d parameters
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput__global_parameters : ',trim(path0)

    path = path0

    call euitmput( trim(path)//'bphi_geo_vac.dat' , thisstructure%bphi_geo_vac%dat , ifail )    !D
    call euitmput( trim(path)//'i_plasma.dat' , thisstructure%i_plasma%dat , ifail )    !D
    call euitmput( trim(path)//'beta_pol_2.dat' , thisstructure%beta_pol_2%dat , ifail )    !D
    call euitmput( trim(path)//'beta_pol_3.dat' , thisstructure%beta_pol_3%dat , ifail )    !D
    call euitmput( trim(path)//'beta_tor.dat' , thisstructure%beta_tor%dat , ifail )    !D
    call euitmput( trim(path)//'beta_normal.dat' , thisstructure%beta_normal%dat , ifail )    !D
    call euitmput( trim(path)//'li_2.dat' , thisstructure%li_2%dat , ifail )    !D
    call euitmput( trim(path)//'li_3.dat' , thisstructure%li_3%dat , ifail )    !D
    call euitmput( trim(path)//'volume.dat' , thisstructure%volume%dat , ifail )    !D
    call euitmput( trim(path)//'area.dat' , thisstructure%area%dat , ifail )    !D
    call euitmput( trim(path)//'psi_ax.dat' , thisstructure%psi_ax%dat , ifail )    !D
    call euitmput( trim(path)//'psi_bound.dat' , thisstructure%psi_bound%dat , ifail )    !D
    call euitmput( trim(path)//'psi_sep.dat' , thisstructure%psi_sep%dat , ifail )    !D
    call euitmput_mag_axis( trim(path)//'mag_axis.' , thisstructure%mag_axis )        !B1  single structure 
    call euitmput( trim(path)//'q_95.dat' , thisstructure%q_95%dat , ifail )    !D
    call euitmput( trim(path)//'q_min.dat' , thisstructure%q_min%dat , ifail )    !D

    return
  endsubroutine euitmput_global_parameters

  subroutine euitmput_profiles_2d( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_profiles_2d ) :: thisstructure   ! profiles in the poloidal plane
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_profiles_2d : ',trim(path0)

    path = path0

    call euitmput( trim(path)//'grid_type.dat' , thisstructure%grid_type%dat , ifail )    !D
    call euitmput_reggrid( trim(path)//'grid.' , thisstructure%grid )  !M1
    call euitmput( trim(path)//'psi_grid.dat' , thisstructure%psi_grid%dat , ifail )    !D
    call euitmput( trim(path)//'jphi_grid.dat' , thisstructure%jphi_grid%dat , ifail )    !D
    call euitmput( trim(path)//'br.dat' , thisstructure%br%dat , ifail )    !D
    call euitmput( trim(path)//'bz.dat' , thisstructure%bz%dat , ifail )    !D

    return
  endsubroutine euitmput_profiles_2d

  subroutine euitmput_profiles_1d( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_profiles_1d ) :: thisstructure   ! profiles as a function of the poloidal flux
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_profiles_1d : ',trim(path0)

    path = path0

    call euitmput( trim(path)//'npoints.dat' , thisstructure%npoints%dat , ifail )    !D
    call euitmput( trim(path)//'psi.dat' , thisstructure%psi%dat , ifail )    !D
    call euitmput( trim(path)//'pressure.dat' , thisstructure%pressure%dat , ifail )    !D
    call euitmput( trim(path)//'F_dia.dat' , thisstructure%F_dia%dat , ifail )    !D
    call euitmput( trim(path)//'pprime.dat' , thisstructure%pprime%dat , ifail )    !D
    call euitmput( trim(path)//'ffprime.dat' , thisstructure%ffprime%dat , ifail )    !D
    call euitmput( trim(path)//'jphi.dat' , thisstructure%jphi%dat , ifail )    !D
    call euitmput( trim(path)//'jparallel.dat' , thisstructure%jparallel%dat , ifail )    !D
    call euitmput( trim(path)//'q.dat' , thisstructure%q%dat , ifail )    !D
    call euitmput( trim(path)//'r_inboard.dat' , thisstructure%r_inboard%dat , ifail )    !D
    call euitmput( trim(path)//'r_outboard.dat' , thisstructure%r_outboard%dat , ifail )    !D
    call euitmput( trim(path)//'rho_rtvol.dat' , thisstructure%rho_rtvol%dat , ifail )    !D
    call euitmput( trim(path)//'rho_rttorfl.dat' , thisstructure%rho_rttorfl%dat , ifail )    !D
    call euitmput( trim(path)//'elongation.dat' , thisstructure%elongation%dat , ifail )    !D
    call euitmput( trim(path)//'triangularit.dat' , thisstructure%triangularity%dat , ifail )    !D

    return
  endsubroutine euitmput_profiles_1d


  subroutine euitmput_eq( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_eq ) :: thisstructure   ! Description of a 2D, axi-symmetric, tokamak equilibrium
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    write(*,'(A,A)') ' euitmput_eq : ',trim(path0)

    path = path0

    call euitmput_putinfo( path , thisstructure%putinfo )      !A
    call euitmput( trim(path)//'time.dat' , thisstructure%time%dat , ifail )    !D
    call euitmput( trim(path)//'briefdescrip.dat' , thisstructure%briefdescrip%dat , ifail )    !D
    !call euitmput_eqconstrain( trim(path)//'eqconstrain.' , thisstructure%eqconstrain )      !A
    call euitmput_eqgeometry( trim(path)//'eqgeometry.' , thisstructure%eqgeometry )      !A
    call euitmput_global_parameters( trim(path)//'global_param.' , thisstructure%global_parameters )      !A
    call euitmput_profiles_1d( trim(path)//'profiles_1d.' , thisstructure%profiles_1d )      !A
    call euitmput_profiles_2d( trim(path)//'profiles_2d.'  , thisstructure%profiles_2d )      !A
    call euitmput_eqfittedparam( trim(path)//'eqfittedparam.' , thisstructure%eqfittedparam )      !A
    call euitmput_fixbound( trim(path)//'fixbound.' , thisstructure%fixbound )      !A
    call euitmput_coord_sys( trim(path)//'coord_sys.', thisstructure%coord_sys )      !A

    return
  endsubroutine euitmput_eq

  subroutine euitmput_equilibria( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_equilibria ) :: thisstructure   ! Set of equilibria
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'neq.dat' , thisstructure%neq%dat , ifail )    !D
    allocate( thisstructure%eq( thisstructure%neq%dat ) )
    do iloop = 1 , thisstructure%neq%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_eq( trim(path)//'eq.' //NNN//'.', thisstructure%eq(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_equilibria

  subroutine euitmput_segment( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_segment ) :: thisstructure   ! A single segment 
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'name.dat' , thisstructure%name%dat , ifail )    !D
    call euitmput( trim(path)//'id' , thisstructure%id , ifail )    !C
    call euitmput( trim(path)//'permeability.dat' , thisstructure%permeabilitymodel%dat , ifail )    !D
    call euitmput_rzvector( trim(path)//'rz.' , thisstructure%rz )  !M1

    return
  endsubroutine euitmput_segment

  subroutine euitmput_desc( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_desc ) :: thisstructure   ! Title of the block, for display
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'nsegment.dat' , thisstructure%nsegment%dat , ifail )    !D
    allocate( thisstructure%segment( thisstructure%nsegment%dat ) )
    do iloop = 1 , thisstructure%nsegment%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_segment( trim(path)//'segment.' //NNN//'.', thisstructure%segment(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_desc

  subroutine euitmput_signalslice( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_signalslice ) :: thisstructure   ! A single timeslice
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'meas.dat' , thisstructure%meas%dat , ifail )    !D
    call euitmput( trim(path)//'weight.dat' , thisstructure%weight%dat , ifail )    !D
    call euitmput( trim(path)//'exact.dat' , thisstructure%exact%dat , ifail )    !D
    call euitmput( trim(path)//'relerr.dat' , thisstructure%relerr%dat , ifail )    !D
    call euitmput( trim(path)//'abserr.dat' , thisstructure%abserr%dat , ifail )    !D
    call euitmput( trim(path)//'calc.dat' , thisstructure%calc%dat , ifail )    !D
    call euitmput( trim(path)//'chi.dat' , thisstructure%chi%dat , ifail )    !D
    call euitmput_signal( trim(path)//'signal.' , thisstructure%signal )  !M1

    return
  endsubroutine euitmput_signalslice

  subroutine euitmput_ironmodel( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_ironmodel ) :: thisstructure   ! Block containing a model of the iron circuit
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_desc( trim(path)//'desc.' , thisstructure%desc )        !B1  single structure 
    allocate( thisstructure%signalslice( thisstructure%nsignalslice%dat ) )
    do iloop = 1 , thisstructure%nsignalslice%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_signalslice( trim(path)//'signalslice.' //NNN//'.', thisstructure%signalslice(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_ironmodel

  subroutine euitmput_limiter( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_limiter ) :: thisstructure   ! Description of the immobile limiting surface for defini
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput_rzvector( trim(path)//'outline.' , thisstructure%outline )  !M1

    return
  endsubroutine euitmput_limiter

  subroutine euitmput_chorddata( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_chorddata ) :: thisstructure   ! Geometric description of the lines of sight
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'nchords.dat' , thisstructure%nchords%dat , ifail )    !D
    call euitmput( trim(path)//'rzpivot.dat' , thisstructure%rzpivot%dat , ifail )    !D
    call euitmput( trim(path)//'phipivot.dat' , thisstructure%phipivot%dat , ifail )    !D
    call euitmput( trim(path)//'polchordang.dat' , thisstructure%polchordang%dat , ifail )    !D
    call euitmput( trim(path)//'torchordang.dat' , thisstructure%torchordang%dat , ifail )    !D
    call euitmput( trim(path)//'nchordpoints.dat' , thisstructure%nchordpoints%dat , ifail )    !D

    return
  endsubroutine euitmput_chorddata

  subroutine euitmput_lineintegraldiag( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_lineintegraldiag ) :: thisstructure   ! General line integral diagnostic
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'function.dat' , thisstructure%function%dat , ifail )    !D
    call euitmput_chorddata( trim(path)//'chorddata.' , thisstructure%chorddata )        !B1  single structure 
    call euitmput_signal( trim(path)//'signal.' , thisstructure%signal )  !M1

    return
  endsubroutine euitmput_lineintegraldiag

  subroutine euitmput_flux( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_flux ) :: thisstructure   ! Poloidal flux loops RZ coordinates have 1 component for
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'name.dat' , thisstructure%name%dat , ifail )    !D
    call euitmput( trim(path)//'id' , thisstructure%id , ifail )    !C
    call euitmput_rzvector( trim(path)//'centres.' , thisstructure%centres )  !M1
    call euitmput( trim(path)//'torangle.dat' , thisstructure%torangle%dat , ifail )    !D
    call euitmput_signal( trim(path)//'values.' , thisstructure%values )  !M1

    return
  endsubroutine euitmput_flux

  subroutine euitmput_bpol( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_bpol ) :: thisstructure   ! Poloidal field probe
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'name.dat' , thisstructure%name%dat , ifail )    !D
    call euitmput( trim(path)//'id' , thisstructure%id , ifail )    !C
    call euitmput( trim(path)//'centre.dat' , thisstructure%centre%dat , ifail )    !D
    call euitmput( trim(path)//'polangle.dat' , thisstructure%polangle%dat , ifail )    !D
    call euitmput( trim(path)//'area.dat' , thisstructure%area%dat , ifail )    !D
    call euitmput( trim(path)//'length.dat' , thisstructure%length%dat , ifail )    !D
    call euitmput( trim(path)//'torangle.dat' , thisstructure%torangle%dat , ifail )    !D
    call euitmput( trim(path)//'turns.dat' , thisstructure%turns%dat , ifail )    !D
    call euitmput_signal( trim(path)//'value.' , thisstructure%value )  !M1

    return
  endsubroutine euitmput_bpol

  subroutine euitmput_magdiag( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_magdiag ) :: thisstructure   ! Magnetic diagnostics
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput_signal( trim(path)//'ip.' , thisstructure%ip )  !M1
    call euitmput( trim(path)//'nflux.dat' , thisstructure%nflux%dat , ifail )    !D
    allocate( thisstructure%flux( thisstructure%nflux%dat ) )
    do iloop = 1 , thisstructure%nflux%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_flux( trim(path)//'flux.' //NNN//'.', thisstructure%flux(iloop) )        !B2  multiple structure
    enddo
    call euitmput_signal( trim(path)//'diamagneticf.' , thisstructure%diamagneticflux )  !M1
    call euitmput( trim(path)//'nbpol.dat' , thisstructure%nbpol%dat , ifail )    !D
    allocate( thisstructure%bpol( thisstructure%nbpol%dat ) )
    do iloop = 1 , thisstructure%nbpol%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_bpol( trim(path)//'bpol.' //NNN//'.', thisstructure%bpol(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_magdiag

  subroutine euitmput_msediag( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_msediag ) :: thisstructure   ! MSE Diagnostic perhaps this should be broken down as N-
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_rzvector( trim(path)//'rzgamma.' , thisstructure%rzgamma )  !M1
    call euitmput( trim(path)//'tgamma.dat' , thisstructure%tgamma%dat , ifail )    !D
    call euitmput( trim(path)//'a1gamma.dat' , thisstructure%a1gamma%dat , ifail )    !D
    call euitmput( trim(path)//'a2gamma.dat' , thisstructure%a2gamma%dat , ifail )    !D
    call euitmput( trim(path)//'a3gamma.dat' , thisstructure%a3gamma%dat , ifail )    !D
    call euitmput( trim(path)//'a4gamma.dat' , thisstructure%a4gamma%dat , ifail )    !D
    call euitmput( trim(path)//'a5gamma.dat' , thisstructure%a5gamma%dat , ifail )    !D
    call euitmput( trim(path)//'a6gamma.dat' , thisstructure%a6gamma%dat , ifail )    !D
    call euitmput_signal( trim(path)//'signal.' , thisstructure%signal )  !M1

    return
  endsubroutine euitmput_msediag

  subroutine euitmput_circuit( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_circuit ) :: thisstructure   ! PF circuit description
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'name.dat' , thisstructure%name%dat , ifail )    !D
    call euitmput( trim(path)//'id' , thisstructure%id , ifail )    !C
    call euitmput( trim(path)//'type.dat' , thisstructure%type%dat , ifail )    !D
    call euitmput( trim(path)//'coilconnect.dat' , thisstructure%coilconnect%dat , ifail )    !D
    call euitmput( trim(path)//'supplyconnec.dat' , thisstructure%supplyconnect%dat , ifail )    !D

    return
  endsubroutine euitmput_circuit

  subroutine euitmput_pfcircuits( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_pfcircuits ) :: thisstructure   ! Circuits, connected to multiple coils and to multiple s
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'ncircuit.dat' , thisstructure%ncircuit%dat , ifail )    !D
    allocate( thisstructure%circuit( thisstructure%ncircuit%dat ) )
    do iloop = 1 , thisstructure%ncircuit%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_circuit( trim(path)//'circuit.' //NNN//'.', thisstructure%circuit(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_pfcircuits


  subroutine euitmput_coil( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_coil ) :: thisstructure   ! Single PF coil description
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'name.dat' , thisstructure%name%dat , ifail )    !D
    call euitmput( trim(path)//'id.dat' , thisstructure%id%dat , ifail )    !D
    call euitmput( trim(path)//'res.dat' , thisstructure%res%dat , ifail )    !D
    call euitmput( trim(path)//'turns.dat' , thisstructure%turns%dat , ifail )    !D
    call euitmput( trim(path)//'modelnrnz.dat' , thisstructure%modelnrnz%dat , ifail )    !D
    call euitmput_pfgeometry( trim(path)//'pfgeometry.' , thisstructure%pfgeometry )      !A
    call euitmput_pfelement( trim(path)//'pfelement.' , thisstructure%pfelement )      !A
    call euitmput_signal( trim(path)//'coilcurrent.' , thisstructure%coilcurrent )  !M1
    call euitmput_signal( trim(path)//'coilvoltage.' , thisstructure%coilvoltage )  !M1

    return
  endsubroutine euitmput_coil

  subroutine euitmput_pfcoils( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_pfcoils ) :: thisstructure   ! Active poloidal field coils
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'ncoil.dat' , thisstructure%ncoil%dat , ifail )    !D
    allocate( thisstructure%coil( thisstructure%ncoil%dat ) )
    do iloop = 1 , thisstructure%ncoil%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_coil( trim(path)//'coil.' //NNN//'.', thisstructure%coil(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_pfcoils


  subroutine euitmput_pfpassive( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_pfpassive ) :: thisstructure   ! Passive axisymmetric conductor description
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'npfelement.dat' , thisstructure%npfelement%dat , ifail )    !D
    call euitmput_pfelement( trim(path)//'pfelement.' , thisstructure%pfelement )      !A

    return
  endsubroutine euitmput_pfpassive

  subroutine euitmput_supply( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_supply ) :: thisstructure   ! A single PF supply
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'name.dat' , thisstructure%name%dat , ifail )    !D
    call euitmput( trim(path)//'id' , thisstructure%id , ifail )    !C
    call euitmput( trim(path)//'type.dat' , thisstructure%type%dat , ifail )    !D
    call euitmput( trim(path)//'imax.dat' , thisstructure%imax%dat , ifail )    !D
    call euitmput( trim(path)//'imin.dat' , thisstructure%imin%dat , ifail )    !D
    call euitmput( trim(path)//'umax.dat' , thisstructure%umax%dat , ifail )    !D
    call euitmput( trim(path)//'umin.dat' , thisstructure%umin%dat , ifail )    !D
    call euitmput( trim(path)//'delay.dat' , thisstructure%delay%dat , ifail )    !D
    call euitmput( trim(path)//'filter.dat' , thisstructure%filter%dat , ifail )    !D
    call euitmput( trim(path)//'res.dat' , thisstructure%res%dat , ifail )    !D
    call euitmput_signal( trim(path)//'voltage.' , thisstructure%voltage )  !M1
    call euitmput_signal( trim(path)//'current.' , thisstructure%current )  !M1

    return
  endsubroutine euitmput_supply

  subroutine euitmput_pfsupplies( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_pfsupplies ) :: thisstructure   ! PF power supplies
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'nsupply.dat' , thisstructure%nsupply%dat , ifail )    !D
    allocate( thisstructure%supply( thisstructure%nsupply%dat ) )
    do iloop = 1 , thisstructure%nsupply%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_supply( trim(path)//'supply.' //NNN//'.', thisstructure%supply(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_pfsupplies

  subroutine euitmput_channel( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_channel ) :: thisstructure   ! To Be developed
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'rzpoint.dat' , thisstructure%rzpoint%dat , ifail )    !D
    call euitmput_signal( trim(path)//'signal.' , thisstructure%signal )  !M1
    call euitmput( trim(path)//'id' , thisstructure%id , ifail )    !C

    return
  endsubroutine euitmput_channel

  subroutine euitmput_thomson( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_thomson ) :: thisstructure   ! Thomson scattering diagnostic - Work to be done 
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput( trim(path)//'nchannel.dat' , thisstructure%nchannel%dat , ifail )    !D
    allocate( thisstructure%channel( thisstructure%nchannel%dat ) )
    do iloop = 1 , thisstructure%nchannel%dat
       write( NNN , '( A3 , i3.3 ) ' )  'NNN' , iloop
       call euitmput_channel( trim(path)//'channel.' //NNN//'.', thisstructure%channel(iloop) )        !B2  multiple structure
    enddo

    return
  endsubroutine euitmput_thomson

  subroutine euitmput_tiles( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_tiles ) :: thisstructure   ! mechanical structure of the tiles - to be done
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A

    return
  endsubroutine euitmput_tiles

  subroutine euitmput_toroidalfield( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_toroidalfield ) :: thisstructure   ! Toroidal field definitions, note there is physical redu
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput( trim(path)//'nturns.dat' , thisstructure%nturns%dat , ifail )    !D
    call euitmput( trim(path)//'ncoils.dat' , thisstructure%ncoils%dat , ifail )    !D
    call euitmput_signal( trim(path)//'current.' , thisstructure%current )  !M1
    call euitmput_signal( trim(path)//'bvac_r.' , thisstructure%bvac_r )  !M1

    return
  endsubroutine euitmput_toroidalfield

  subroutine euitmput_vessel( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_vessel ) :: thisstructure   ! mechanical structure of the vacuum vessel - to be done
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput_rzvector( trim(path)//'vesselwall.' , thisstructure%vesselwall )  !M1

    return
  endsubroutine euitmput_vessel

  subroutine euitmput_level2( path0 , thisstructure )
    use euITM_schemas
    implicit none
    include 'mdslib.inc'
    type( type_level2 ) :: thisstructure   ! Demo ITM databus
    integer :: ifail , iloop
    character*(*) :: path0
    character*120 :: path
    character*6 NNN

    path = path0

    call euitmput_header( path , thisstructure%header )      !A
    call euitmput_localaccess( path , thisstructure%localaccess )      !A
    call euitmput_emcalc( path , thisstructure%emcalc )      !A
    call euitmput_equilibria( path , thisstructure%equilibria )      !A
    call euitmput_limiter( path , thisstructure%limiter )      !A
    call euitmput_magdiag( path , thisstructure%magdiag )      !A
    call euitmput_pfcircuits( path , thisstructure%pfcircuits )      !A
    call euitmput_pfcoils( path , thisstructure%pfcoils )      !A
    call euitmput_pfpassive( path , thisstructure%pfpassive )      !A
    call euitmput_pfsupplies( path , thisstructure%pfsupplies )      !A
    call euitmput_toroidalfield( path , thisstructure%toroidalfield )      !A
    call euitmput_controllers( path , thisstructure%controllers )      !A
    call euitmput_tiles( path , thisstructure%tiles )      !A
    call euitmput_msediag( path , thisstructure%msediag )      !A
    call euitmput_lineintegraldiag( path , thisstructure%lineintegraldiag )      !A
    call euitmput_thomson( path , thisstructure%thomson )      !A
    call euitmput_vessel( path , thisstructure%vessel )      !A
    call euitmput_divertor( path , thisstructure%divertor )      !A

    return
  endsubroutine euitmput_level2

end module euITM_putroutines
