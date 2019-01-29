module assign_chease_codeparameters_choices_xml2eg

  implicit none

contains

  subroutine assign_chease_codepar_choices(codeparam,istatus)
    use euitm_schemas, only: type_param
    use xml2eg_mdl, only: xml2eg_parse_memory, xml2eg_get, type_xml2eg_document
    use globals

    ! Input
    type(type_param) :: codeparam
    integer :: istatus

    ! Local
    type(type_xml2eg_document) :: doc
    character(30) :: str
    logical :: error_flag
    character(132) :: strpaths(10)
    integer :: nb_strpaths, i

    ! Temporary read variables
    integer :: temp_int

    istatus = 0

    ! This call translated (parses) the xml data, stored in the string codeparam%parameters, into
    ! the DOM format used by libxml2.
    ! The output is the DOM document "dom".
    call xml2eg_parse_memory( codeparam%parameters , doc )

    ! Below fetch data fields from the DOM document using the xml2eg_get, which is overloads reading 
    ! routines for strings, reals, doubles and integers, as well as vectors of reals, doubles and integers.

    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! from for_assign_chease_choices.f90:
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    strpaths(1) = 'mesh_characteristics/number_intervals_mapping_mesh/small/NCHI'
    strpaths(2) = 'mesh_characteristics/number_intervals_mapping_mesh/very_small/NCHI'
    strpaths(3) = 'mesh_characteristics/number_intervals_mapping_mesh/fast_standard/NCHI'
    strpaths(4) = 'mesh_characteristics/number_intervals_mapping_mesh/standard/NCHI'
    strpaths(5) = 'mesh_characteristics/number_intervals_mapping_mesh/large/NCHI'
    strpaths(6) = 'mesh_characteristics/number_intervals_mapping_mesh/other_choice/NCHI'
    strpaths(7) = 'NCHI'
    nb_strpaths = 7
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , NCHI, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'NCHI = ',NCHI
    
    strpaths(1) = 'mapping_mesh_output/equal_arc_length/NEGP'
    strpaths(2) = 'mapping_mesh_output/straight-field-line/NEGP'
    strpaths(3) = 'mapping_mesh_output/Hamada/NEGP'
    strpaths(4) = 'mapping_mesh_output/other_choice/NEGP'
    strpaths(5) = 'NEGP'
    nb_strpaths = 5
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , NEGP, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'NEGP = ',NEGP
    
    strpaths(1) = 'mapping_mesh_output/equal_arc_length/NER'
    strpaths(2) = 'mapping_mesh_output/straight-field-line/NER'
    strpaths(3) = 'mapping_mesh_output/Hamada/NER'
    strpaths(4) = 'mapping_mesh_output/other_choice/NER'
    strpaths(5) = 'NER'
    nb_strpaths = 5
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , NER, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'NER = ',NER
    
    strpaths(1) = 'mesh_characteristics/number_intervals_mapping_mesh/small/NISO'
    strpaths(2) = 'mesh_characteristics/number_intervals_mapping_mesh/very_small/NISO'
    strpaths(3) = 'mesh_characteristics/number_intervals_mapping_mesh/fast_standard/NISO'
    strpaths(4) = 'mesh_characteristics/number_intervals_mapping_mesh/standard/NISO'
    strpaths(5) = 'mesh_characteristics/number_intervals_mapping_mesh/large/NISO'
    strpaths(6) = 'mesh_characteristics/number_intervals_mapping_mesh/other_choice/NISO'
    strpaths(7) = 'NISO'
    nb_strpaths = 7
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , NISO, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'NISO = ',NISO
    
    strpaths(1) = 'mesh_characteristics/number_intervals_mapping_mesh/small/NPSI'
    strpaths(2) = 'mesh_characteristics/number_intervals_mapping_mesh/very_small/NPSI'
    strpaths(3) = 'mesh_characteristics/number_intervals_mapping_mesh/fast_standard/NPSI'
    strpaths(4) = 'mesh_characteristics/number_intervals_mapping_mesh/standard/NPSI'
    strpaths(5) = 'mesh_characteristics/number_intervals_mapping_mesh/large/NPSI'
    strpaths(6) = 'mesh_characteristics/number_intervals_mapping_mesh/other_choice/NPSI'
    strpaths(7) = 'NPSI'
    nb_strpaths = 7
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , NPSI, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'NPSI = ',NPSI
    
    strpaths(1) = 'mesh_characteristics/number_intervals_equil_mesh/very_small/NS'
    strpaths(2) = 'mesh_characteristics/number_intervals_equil_mesh/small/NS'
    strpaths(3) = 'mesh_characteristics/number_intervals_equil_mesh/standard/NS'
    strpaths(4) = 'mesh_characteristics/number_intervals_equil_mesh/large/NS'
    strpaths(5) = 'mesh_characteristics/number_intervals_equil_mesh/other_choice/NS'
    strpaths(6) = 'NS'
    nb_strpaths = 6
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , NS, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'NS = ',NS
    
    strpaths(1) = 'mesh_characteristics/number_intervals_equil_mesh/very_small/NT'
    strpaths(2) = 'mesh_characteristics/number_intervals_equil_mesh/small/NT'
    strpaths(3) = 'mesh_characteristics/number_intervals_equil_mesh/standard/NT'
    strpaths(4) = 'mesh_characteristics/number_intervals_equil_mesh/large/NT'
    strpaths(5) = 'mesh_characteristics/number_intervals_equil_mesh/other_choice/NT'
    strpaths(6) = 'NT'
    nb_strpaths = 6
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , NT, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'NT = ',NT
    
    strpaths(1) = 'Others/afbs'
    strpaths(2) = 'afbs'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , afbs, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'afbs = ',afbs
    
    strpaths(1) = 'Others/afbs2'
    strpaths(2) = 'afbs2'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , afbs2, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'afbs2 = ',afbs2
    
    strpaths(1) = 'Others/ap'
    strpaths(2) = 'ap'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ap, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ap = ',ap
    
    strpaths(1) = 'Others/ap2'
    strpaths(2) = 'ap2'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ap2, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ap2 = ',ap2
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/aplace'
    strpaths(2) = 'aplace'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , aplace, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'aplace = ',aplace
    
    strpaths(1) = 'plasma_boundary/analytical/aspct'
    strpaths(2) = 'aspct'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , aspct, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'aspct = ',aspct
    
    strpaths(1) = 'Others/at'
    strpaths(2) = 'at'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , at, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'at = ',at
    
    strpaths(1) = 'Others/at2'
    strpaths(2) = 'at2'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , at2, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'at2 = ',at2
    
    strpaths(1) = 'Others/at3'
    strpaths(2) = 'at3'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , at3, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'at3 = ',at3
    
    strpaths(1) = 'Others/at4'
    strpaths(2) = 'at4'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , at4, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'at4 = ',at4
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/awidth'
    strpaths(2) = 'awidth'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , awidth, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'awidth = ',awidth
    
    strpaths(1) = 'Others/b0exp'
    strpaths(2) = 'b0exp'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , b0exp, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'b0exp = ',b0exp
    
    strpaths(1) = 'plasma_boundary/analytical/beans'
    strpaths(2) = 'beans'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , beans, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'beans = ',beans
    
    strpaths(1) = 'Others/bentaxis'
    strpaths(2) = 'bentaxis'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , bentaxis, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'bentaxis = ',bentaxis
    
    strpaths(1) = 'Others/bentqprofile'
    strpaths(2) = 'bentqprofile'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , bentqprofile, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'bentqprofile = ',bentqprofile
    
    strpaths(1) = 'Others/bentradius'
    strpaths(2) = 'bentradius'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , bentradius, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'bentradius = ',bentradius
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshb_intermediate_s_mesh/bplace'
    strpaths(2) = 'bplace'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , bplace, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'bplace = ',bplace
    
    strpaths(1) = 'Others/bsfrac'
    strpaths(2) = 'bsfrac'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , bsfrac, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'bsfrac = ',bsfrac
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshb_intermediate_s_mesh/bwidth'
    strpaths(2) = 'bwidth'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , bwidth, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'bwidth = ',bwidth
    
    strpaths(1) = 'plasma_boundary/analytical/ceta'
    strpaths(2) = 'ceta'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ceta, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ceta = ',ceta
    
    strpaths(1) = 'Others/cfbal'
    strpaths(2) = 'cfbal'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , cfbal, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'cfbal = ',cfbal
    
    strpaths(1) = 'Others/cfnress'
    strpaths(2) = 'cfnress'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , cfnress, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'cfnress = ',cfnress
    
    strpaths(1) = 'Others/cfnresso'
    strpaths(2) = 'cfnresso'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , cfnresso, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'cfnresso = ',cfnresso
    
    strpaths(1) = 'COCOS_verbose/cocos_in'
    strpaths(2) = 'cocos_in'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , cocos_in, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'cocos_in = ',cocos_in
    
    strpaths(1) = 'COCOS_verbose/cocos_out'
    strpaths(2) = 'cocos_out'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , cocos_out, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'cocos_out = ',cocos_out
    
    strpaths(1) = 'Others/comments'
    strpaths(2) = 'comments'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , comments(1), error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'comments = ',comments
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshc_equil_sigma_mesh_packing/cplace'
    strpaths(2) = 'cplace'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , cplace, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'cplace = ',cplace
    
    strpaths(1) = 'Others/cpress'
    strpaths(2) = 'cpress'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , cpress, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'cpress = ',cpress
    
    strpaths(1) = 'Others/cpresso'
    strpaths(2) = 'cpresso'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , cpresso, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'cpresso = ',cpresso
    
    strpaths(1) = 'plasma_boundary/analytical/cq0'
    strpaths(2) = 'cq0'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , cq0, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'cq0 = ',cq0
    
    strpaths(1) = 'rescaling_options/specify_q_value_at_csspec/csspec'
    strpaths(2) = 'rescaling_options/any_choice/csspec'
    strpaths(3) = 'csspec'
    nb_strpaths = 3
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , csspec, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'csspec = ',csspec
    
    strpaths(1) = 'rescaling_options/specify_tot_current_in_chease_units/currt'
    strpaths(2) = 'rescaling_options/any_choice/currt'
    strpaths(3) = 'currt'
    nb_strpaths = 3
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , currt, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'currt = ',currt
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshc_equil_sigma_mesh_packing/cwidth'
    strpaths(2) = 'cwidth'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , cwidth, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'cwidth = ',cwidth
    
    strpaths(1) = 'plasma_boundary/analytical/delta'
    strpaths(2) = 'delta'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , delta, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'delta = ',delta
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshd_equil_theta_mesh_packing/dplace'
    strpaths(2) = 'dplace'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , dplace, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'dplace = ',dplace
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshd_equil_theta_mesh_packing/dwidth'
    strpaths(2) = 'dwidth'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , dwidth, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'dwidth = ',dwidth
    
    strpaths(1) = 'plasma_boundary/analytical/elong'
    strpaths(2) = 'elong'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , elong, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'elong = ',elong
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshe_chi_mesh_packing/eplace'
    strpaths(2) = 'eplace'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , eplace, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'eplace = ',eplace
    
    strpaths(1) = 'mesh_characteristics/number_intervals_equil_mesh/very_small/epslon'
    strpaths(2) = 'mesh_characteristics/number_intervals_equil_mesh/small/epslon'
    strpaths(3) = 'mesh_characteristics/number_intervals_equil_mesh/standard/epslon'
    strpaths(4) = 'mesh_characteristics/number_intervals_equil_mesh/large/epslon'
    strpaths(5) = 'mesh_characteristics/number_intervals_equil_mesh/other_choice/epslon'
    strpaths(6) = 'epslon'
    nb_strpaths = 6
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , epslon, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'epslon = ',epslon
    
    strpaths(1) = 'Others/etaei'
    strpaths(2) = 'etaei'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , etaei, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'etaei = ',etaei
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshe_chi_mesh_packing/ewidth'
    strpaths(2) = 'ewidth'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ewidth, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ewidth = ',ewidth
    
    strpaths(1) = 'Others/gamma'
    strpaths(2) = 'gamma'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , gamma, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'gamma = ',gamma
    
    strpaths(1) = 'Others/msmax'
    strpaths(2) = 'msmax'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , msmax, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'msmax = ',msmax
    
    strpaths(1) = 'Others/nanal'
    strpaths(2) = 'nanal'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nanal, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nanal = ',nanal
    
    strpaths(1) = 'Others/nbal'
    strpaths(2) = 'nbal'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nbal, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nbal = ',nbal
    
    strpaths(1) = 'Others/nblc0'
    strpaths(2) = 'nblc0'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nblc0, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nblc0 = ',nblc0
    
    strpaths(1) = 'Others/nblopt'
    strpaths(2) = 'nblopt'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nblopt, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nblopt = ',nblopt
    
    strpaths(1) = 'Others/nbpsout'
    strpaths(2) = 'nbpsout'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nbpsout, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nbpsout = ',nbpsout
    
    strpaths(1) = 'Others/nbsexpq'
    strpaths(2) = 'nbsexpq'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nbsexpq, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nbsexpq = ',nbsexpq
    
    strpaths(1) = 'Others/nbsfun'
    strpaths(2) = 'nbsfun'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nbsfun, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nbsfun = ',nbsfun
    
    strpaths(1) = 'Others/nbsopt'
    strpaths(2) = 'nbsopt'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nbsopt, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nbsopt = ',nbsopt
    
    strpaths(1) = 'rescaling_options/no_rescaling/ncscal'
    strpaths(2) = 'rescaling_options/specify_q_value_at_csspec/ncscal'
    strpaths(3) = 'rescaling_options/specify_tot_current_in_chease_units/ncscal'
    strpaths(4) = 'rescaling_options/any_choice/ncscal'
    strpaths(5) = 'ncscal'
    nb_strpaths = 5
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ncscal, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ncscal = ',ncscal
    
    strpaths(1) = 'Others/ndiagop'
    strpaths(2) = 'ndiagop'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ndiagop, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ndiagop = ',ndiagop
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/no_packing/ndifps'
    strpaths(2) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/ndifps'
    strpaths(3) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmesha_stability_s_packing_on_q_values/ndifps'
    strpaths(4) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmesha_stability_s_rhogen_equidistant/ndifps'
    strpaths(5) = 'ndifps'
    nb_strpaths = 5
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ndifps, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ndifps = ',ndifps
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/no_packing/ndift'
    strpaths(2) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshd_equil_theta_mesh_auto_packing/ndift'
    strpaths(3) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshd_equil_theta_mesh_packing/ndift'
    strpaths(4) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshd_equil_chi_mesh_no_packing/ndift'
    strpaths(5) = 'ndift'
    nb_strpaths = 5
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ndift, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ndift = ',ndift
    
    strpaths(1) = 'Others/neonbqs'
    strpaths(2) = 'neonbqs'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , neonbqs, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'neonbqs = ',neonbqs
    
    strpaths(1) = 'Others/neqdsk'
    strpaths(2) = 'neqdsk'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , neqdsk, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'neqdsk = ',neqdsk
    
    strpaths(1) = 'Others/neqdxtpo'
    strpaths(2) = 'neqdxtpo'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , neqdxtpo, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'neqdxtpo = ',neqdxtpo
    
    strpaths(1) = 'Others/nfftopt'
    strpaths(2) = 'nfftopt'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nfftopt, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nfftopt = ',nfftopt
    
    strpaths(1) = 'choice_of_input_profiles/second_source/Istar_jphi/nfunc'
    strpaths(2) = 'choice_of_input_profiles/second_source/ffprime/nfunc'
    strpaths(3) = 'choice_of_input_profiles/second_source/Iparallel_chease/nfunc'
    strpaths(4) = 'choice_of_input_profiles/second_source/jparallel_jB_over_B0/nfunc'
    strpaths(5) = 'choice_of_input_profiles/second_source/parameteric/nfunc'
    strpaths(6) = 'nfunc'
    nb_strpaths = 6
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nfunc, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nfunc = ',nfunc
    
    strpaths(1) = 'radial_input_mesh/rho_pol_norm_standard_chease/nfunrho'
    strpaths(2) = 'radial_input_mesh/rho_tor_norm/nfunrho'
    strpaths(3) = 'nfunrho'
    nb_strpaths = 3
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nfunrho, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nfunrho = ',nfunrho
    
    strpaths(1) = 'nideal_output_mapping_choice/standard_kepler_eqdsk/nideal'
    strpaths(2) = 'nideal_output_mapping_choice/MARS/nideal'
    strpaths(3) = 'nideal_output_mapping_choice/LION/nideal'
    strpaths(4) = 'nideal_output_mapping_choice/XTOR/nideal'
    strpaths(5) = 'nideal_output_mapping_choice/ORB5_NEMORB_GENE_straightfieldline/nideal'
    strpaths(6) = 'nideal_output_mapping_choice/GKW_NEOART_hamada/nideal'
    strpaths(7) = 'nideal_output_mapping_choice/others_1ERATO_8ELITE_4PENN_3NOVA_W_7GYRO/nideal'
    strpaths(8) = 'Others/nideal'
    strpaths(9) = 'nideal'
    nb_strpaths = 9
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nideal, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nideal = ',nideal
    
    strpaths(1) = 'Others/ninmap'
    strpaths(2) = 'ninmap'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ninmap, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ninmap = ',ninmap
    
    strpaths(1) = 'Others/ninsca'
    strpaths(2) = 'ninsca'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ninsca, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ninsca = ',ninsca
    
    strpaths(1) = 'Others/nipr'
    strpaths(2) = 'nipr'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nipr, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nipr = ',nipr
    
    strpaths(1) = 'link_to_database_ala_itm_imas/no_link/nitmopt'
    strpaths(2) = 'link_to_database_ala_itm_imas/input_from_database_output_on_files/nitmopt'
    strpaths(3) = 'link_to_database_ala_itm_imas/input_from_files_output_on_database/nitmopt'
    strpaths(4) = 'link_to_database_ala_itm_imas/input_output_from_to_database/nitmopt'
    strpaths(5) = 'nitmopt'
    nb_strpaths = 5
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nitmopt, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nitmopt = ',nitmopt
    
    strpaths(1) = 'link_to_database_ala_itm_imas/input_from_database_output_on_files/nitmrun'
    strpaths(2) = 'link_to_database_ala_itm_imas/input_from_files_output_on_database/nitmrun'
    strpaths(3) = 'link_to_database_ala_itm_imas/input_output_from_to_database/nitmrun'
    strpaths(4) = 'nitmrun'
    nb_strpaths = 4
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nitmrun, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nitmrun = ',nitmrun
    
    strpaths(1) = 'link_to_database_ala_itm_imas/input_from_database_output_on_files/nitmshot'
    strpaths(2) = 'link_to_database_ala_itm_imas/input_from_files_output_on_database/nitmshot'
    strpaths(3) = 'link_to_database_ala_itm_imas/input_output_from_to_database/nitmshot'
    strpaths(4) = 'nitmshot'
    nb_strpaths = 4
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nitmshot, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nitmshot = ',nitmshot
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/no_packing/nmesha'
    strpaths(2) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmesha'
    strpaths(3) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmesha_stability_s_packing_on_q_values/nmesha'
    strpaths(4) = 'nmesha'
    nb_strpaths = 4
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nmesha, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nmesha = ',nmesha
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/no_packing/nmeshb'
    strpaths(2) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshb_intermediate_s_mesh_nopacking/nmeshb'
    strpaths(3) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshb_intermediate_s_mesh_nopacking/nmeshb'
    strpaths(4) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshb_intermediate_s_mesh/nmeshb'
    strpaths(5) = 'nmeshb'
    nb_strpaths = 5
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nmeshb, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nmeshb = ',nmeshb
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/no_packing/nmeshc'
    strpaths(2) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshc_equil_sigma_mesh_packing/nmeshc'
    strpaths(3) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshc_equil_sigma_mesh_no_packing/nmeshc'
    strpaths(4) = 'nmeshc'
    nb_strpaths = 4
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nmeshc, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nmeshc = ',nmeshc
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/no_packing/nmeshd'
    strpaths(2) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshd_equil_theta_mesh_auto_packing/nmeshd'
    strpaths(3) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshd_equil_theta_mesh_packing/nmeshd'
    strpaths(4) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshd_equil_chi_mesh_no_packing/nmeshd'
    strpaths(5) = 'nmeshd'
    nb_strpaths = 5
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nmeshd, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nmeshd = ',nmeshd
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/no_packing/nmeshe'
    strpaths(2) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshe_chi_mesh_packing/nmeshe'
    strpaths(3) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshe_chi_mesh_no_packing/nmeshe'
    strpaths(4) = 'nmeshe'
    nb_strpaths = 4
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nmeshe, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nmeshe = ',nmeshe
    
    strpaths(1) = 'Others/nmgaus'
    strpaths(2) = 'nmgaus'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nmgaus, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nmgaus = ',nmgaus
    
    strpaths(1) = 'Others/nopt'
    strpaths(2) = 'nopt'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nopt, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nopt = ',nopt
    
    strpaths(1) = 'Others/nplot'
    strpaths(2) = 'nplot'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nplot, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nplot = ',nplot
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/npoida'
    strpaths(2) = 'npoida'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , npoida, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'npoida = ',npoida
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshb_intermediate_s_mesh/npoidb'
    strpaths(2) = 'npoidb'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , npoidb, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'npoidb = ',npoidb
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshc_equil_sigma_mesh_packing/npoidc'
    strpaths(2) = 'npoidc'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , npoidc, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'npoidc = ',npoidc
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshd_equil_theta_mesh_packing/npoidd'
    strpaths(2) = 'npoidd'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , npoidd, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'npoidd = ',npoidd
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshe_chi_mesh_packing/npoide'
    strpaths(2) = 'npoide'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , npoide, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'npoide = ',npoide
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmesha_stability_s_packing_on_q_values/npoidq'
    strpaths(2) = 'npoidq'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , npoidq, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'npoidq = ',npoidq
    
    strpaths(1) = 'Others/npp'
    strpaths(2) = 'npp'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , npp, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'npp = ',npp
    
    strpaths(1) = 'choice_of_input_profiles/first_source/pprime/nppfun'
    strpaths(2) = 'choice_of_input_profiles/first_source/pressure/nppfun'
    strpaths(3) = 'choice_of_input_profiles/first_source/pprime_parametric/nppfun'
    strpaths(4) = 'nppfun'
    nb_strpaths = 4
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nppfun, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nppfun = ',nppfun
    
    strpaths(1) = 'Others/nppr'
    strpaths(2) = 'nppr'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nppr, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nppr = ',nppr
    
    strpaths(1) = 'Others/nprof2d'
    strpaths(2) = 'nprof2d'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nprof2d, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nprof2d = ',nprof2d
    
    strpaths(1) = 'Others/nprofz'
    strpaths(2) = 'nprofz'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nprofz, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nprofz = ',nprofz
    
    strpaths(1) = 'Others/npropt'
    strpaths(2) = 'npropt'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , npropt, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'npropt = ',npropt
    
    strpaths(1) = 'Others/nprpsi'
    strpaths(2) = 'nprpsi'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nprpsi, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nprpsi = ',nprpsi
    
    strpaths(1) = 'mesh_characteristics/number_eqdskRZ_mesh_and_box/small/nrbox'
    strpaths(2) = 'mesh_characteristics/number_eqdskRZ_mesh_and_box/standard/nrbox'
    strpaths(3) = 'mesh_characteristics/number_eqdskRZ_mesh_and_box/large/nrbox'
    strpaths(4) = 'mesh_characteristics/number_eqdskRZ_mesh_and_box/other_choice/nrbox'
    strpaths(5) = 'nrbox'
    nb_strpaths = 5
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nrbox, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nrbox = ',nrbox
    
    strpaths(1) = 'Others/nrfp'
    strpaths(2) = 'nrfp'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nrfp, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nrfp = ',nrfp
    
    strpaths(1) = 'radial_input_mesh/rho_pol_norm_standard_chease/nrhomesh'
    strpaths(2) = 'radial_input_mesh/rho_tor_norm/nrhomesh'
    strpaths(3) = 'nrhomesh'
    nb_strpaths = 3
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nrhomesh, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nrhomesh = ',nrhomesh
    
    strpaths(1) = 'Others/nrscal'
    strpaths(2) = 'nrscal'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nrscal, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nrscal = ',nrscal
    
    strpaths(1) = 'Others/nsgaus'
    strpaths(2) = 'nsgaus'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nsgaus, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nsgaus = ',nsgaus
    
    strpaths(1) = 'Others/nsmooth'
    strpaths(2) = 'nsmooth'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nsmooth, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nsmooth = ',nsmooth
    
    strpaths(1) = 'Others/nsour'
    strpaths(2) = 'nsour'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nsour, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nsour = ',nsour
    
    strpaths(1) = 'choice_of_input_profiles/second_source/Istar_jphi/nsttp'
    strpaths(2) = 'choice_of_input_profiles/second_source/ffprime/nsttp'
    strpaths(3) = 'choice_of_input_profiles/second_source/Iparallel_chease/nsttp'
    strpaths(4) = 'choice_of_input_profiles/second_source/jparallel_jB_over_B0/nsttp'
    strpaths(5) = 'choice_of_input_profiles/second_source/parametric/nsttp'
    strpaths(6) = 'nsttp'
    nb_strpaths = 6
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nsttp, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nsttp = ',nsttp
    
    strpaths(1) = 'plasma_boundary/from_input_RZ_array/nsurf'
    strpaths(2) = 'plasma_boundary/analytical/nsurf'
    strpaths(3) = 'nsurf'
    nb_strpaths = 3
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nsurf, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nsurf = ',nsurf
    
    strpaths(1) = 'Others/nsym'
    strpaths(2) = 'nsym'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nsym, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nsym = ',nsym
    
    strpaths(1) = 'Others/ntcase'
    strpaths(2) = 'ntcase'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ntcase, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ntcase = ',ntcase
    
    strpaths(1) = 'Others/ntest'
    strpaths(2) = 'ntest'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ntest, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ntest = ',ntest
    
    strpaths(1) = 'Others/ntgaus'
    strpaths(2) = 'ntgaus'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ntgaus, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ntgaus = ',ntgaus
    
    strpaths(1) = 'Others/ntmf0'
    strpaths(2) = 'ntmf0'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ntmf0, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ntmf0 = ',ntmf0
    
    strpaths(1) = 'Others/ntnova'
    strpaths(2) = 'ntnova'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , ntnova, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'ntnova = ',ntnova
    
    strpaths(1) = 'Others/nturn'
    strpaths(2) = 'nturn'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nturn, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nturn = ',nturn
    
    strpaths(1) = 'Others/nv'
    strpaths(2) = 'nv'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nv, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nv = ',nv
    
    strpaths(1) = 'COCOS_verbose/nverbose'
    strpaths(2) = 'nverbose'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nverbose, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nverbose = ',nverbose
    
    strpaths(1) = 'Others/nvexp'
    strpaths(2) = 'nvexp'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nvexp, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nvexp = ',nvexp
    
    strpaths(1) = 'mesh_characteristics/number_eqdskRZ_mesh_and_box/small/nzbox'
    strpaths(2) = 'mesh_characteristics/number_eqdskRZ_mesh_and_box/standard/nzbox'
    strpaths(3) = 'mesh_characteristics/number_eqdskRZ_mesh_and_box/large/nzbox'
    strpaths(4) = 'mesh_characteristics/number_eqdskRZ_mesh_and_box/other_choice/nzbox'
    strpaths(5) = 'nzbox'
    nb_strpaths = 5
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , nzbox, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'nzbox = ',nzbox
    
    strpaths(1) = 'Others/pangle'
    strpaths(2) = 'pangle'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , pangle, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'pangle = ',pangle
    
    strpaths(1) = 'Others/predge'
    strpaths(2) = 'predge'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , predge, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'predge = ',predge
    
    strpaths(1) = 'Others/psiscl'
    strpaths(2) = 'psiscl'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , psiscl, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'psiscl = ',psiscl
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmesha_stability_s_packing_on_q_values/qplace'
    strpaths(2) = 'qplace'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , qplace, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'qplace = ',qplace
    
    strpaths(1) = 'rescaling_options/specify_q_value_at_csspec/qspec'
    strpaths(2) = 'rescaling_options/any_choice/qspec'
    strpaths(3) = 'qspec'
    nb_strpaths = 3
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , qspec, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'qspec = ',qspec
    
    strpaths(1) = 'Others/qvalneo'
    strpaths(2) = 'qvalneo'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , qvalneo, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'qvalneo = ',qvalneo
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmesha_stability_s_packing_on_q_values/qwidth'
    strpaths(2) = 'qwidth'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , qwidth, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'qwidth = ',qwidth
    
    strpaths(1) = 'plasma_boundary/analytical/r0'
    strpaths(2) = 'r0'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , r0, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'r0 = ',r0
    
    strpaths(1) = 'Others/r0exp'
    strpaths(2) = 'r0exp'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , r0exp, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'r0exp = ',r0exp
    
    strpaths(1) = 'Others/r0w'
    strpaths(2) = 'r0w'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , r0w, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'r0w = ',r0w
    
    strpaths(1) = 'Others/rboxlen'
    strpaths(2) = 'rboxlen'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , rboxlen, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'rboxlen = ',rboxlen
    
    strpaths(1) = 'Others/rboxlft'
    strpaths(2) = 'rboxlft'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , rboxlft, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'rboxlft = ',rboxlft
    
    strpaths(1) = 'plasma_boundary/analytical/rc'
    strpaths(2) = 'rc'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , rc, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'rc = ',rc
    
    strpaths(1) = 'Others/relax'
    strpaths(2) = 'relax'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , relax, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'relax = ',relax
    
    strpaths(1) = 'Others/rext'
    strpaths(2) = 'rext'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , rext, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'rext = ',rext
    
    strpaths(1) = 'plasma_boundary/analytical/rnu'
    strpaths(2) = 'rnu'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , rnu, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'rnu = ',rnu
    
    strpaths(1) = 'Others/rpeop'
    strpaths(2) = 'rpeop'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , rpeop, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'rpeop = ',rpeop
    
    strpaths(1) = 'plasma_boundary/analytical/rz0'
    strpaths(2) = 'rz0'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , rz0, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'rz0 = ',rz0
    
    strpaths(1) = 'Others/rz0w'
    strpaths(2) = 'rz0w'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , rz0w, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'rz0w = ',rz0w
    
    strpaths(1) = 'Others/rzion'
    strpaths(2) = 'rzion'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , rzion, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'rzion = ',rzion
    
    strpaths(1) = 'Others/scalne'
    strpaths(2) = 'scalne'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , scalne, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'scalne = ',scalne
    
    strpaths(1) = 'Others/scexp'
    strpaths(2) = 'scexp'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , scexp, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'scexp = ',scexp
    
    strpaths(1) = 'plasma_boundary/analytical/sgma'
    strpaths(2) = 'sgma'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , sgma, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'sgma = ',sgma
    
    strpaths(1) = 'Others/shift_p'
    strpaths(2) = 'shift_p'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , shift_p, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'shift_p = ',shift_p
    
    strpaths(1) = 'Others/signb0xp'
    strpaths(2) = 'signb0xp'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , signb0xp, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'signb0xp = ',signb0xp
    
    strpaths(1) = 'Others/signipxp'
    strpaths(2) = 'signipxp'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , signipxp, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'signipxp = ',signipxp
    
    strpaths(1) = 'Others/slimit'
    strpaths(2) = 'slimit'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , slimit, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'slimit = ',slimit
    
    strpaths(1) = 'Others/snumber'
    strpaths(2) = 'snumber'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , snumber, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'snumber = ',snumber
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/solpda'
    strpaths(2) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmesha_stability_s_packing_on_q_values/solpda'
    strpaths(3) = 'solpda'
    nb_strpaths = 3
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , solpda, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'solpda = ',solpda
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshb_intermediate_s_mesh/solpdb'
    strpaths(2) = 'solpdb'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , solpdb, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'solpdb = ',solpdb
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshc_equil_sigma_mesh_packing/solpdc'
    strpaths(2) = 'solpdc'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , solpdc, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'solpdc = ',solpdc
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshd_equil_theta_mesh_packing/solpdd'
    strpaths(2) = 'solpdd'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , solpdd, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'solpdd = ',solpdd
    
    strpaths(1) = 'mesh_characteristics/various_mesh_packing/standard/nmesha_stability_s_mesh_packing_on_s_values/nmeshe_chi_mesh_packing/solpde'
    strpaths(2) = 'solpde'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , solpde, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'solpde = ',solpde
    
    strpaths(1) = 'Others/tensbnd'
    strpaths(2) = 'tensbnd'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , tensbnd, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'tensbnd = ',tensbnd
    
    strpaths(1) = 'Others/tensprof'
    strpaths(2) = 'tensprof'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , tensprof, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'tensprof = ',tensprof
    
    strpaths(1) = 'plasma_boundary/analytical/theta0'
    strpaths(2) = 'theta0'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , theta0, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'theta0 = ',theta0
    
    strpaths(1) = 'link_to_database_ala_itm_imas/input_from_database_output_on_files/treeitm'
    strpaths(2) = 'link_to_database_ala_itm_imas/input_from_files_output_on_database/treeitm'
    strpaths(3) = 'link_to_database_ala_itm_imas/input_output_from_to_database/treeitm'
    strpaths(4) = 'treeitm'
    nb_strpaths = 4
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , treeitm(1), error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    treeitm(2)=treeitm(1)
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'treeitm = ',treeitm
    
    strpaths(1) = 'plasma_boundary/analytical/triang'
    strpaths(2) = 'triang'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , triang, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'triang = ',triang
    
    strpaths(1) = 'plasma_boundary/analytical/triplt'
    strpaths(2) = 'triplt'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , triplt, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'triplt = ',triplt
    
    strpaths(1) = 'plasma_boundary/analytical/xi'
    strpaths(2) = 'xi'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , xi, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'xi = ',xi
    
    strpaths(1) = 'Others/zboxlen'
    strpaths(2) = 'zboxlen'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , zboxlen, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'zboxlen = ',zboxlen
    
    strpaths(1) = 'Others/zboxmid'
    strpaths(2) = 'zboxmid'
    nb_strpaths = 2
    do i=1,nb_strpaths
      call xml2eg_get(doc , strpaths(i) , zboxmid, error_flag)
      if (.not. error_flag) then
        exit
      endif
    end do
    if ((nverbose .ge. 3) .and. .not. error_flag) write(*,*) strpaths(i),'zboxmid = ',zboxmid
    
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! end of for_assign_chease_choices.f90:
    ! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    print *,'nchi= ',nchi
    print *,'aplace= ',aplace
  end subroutine assign_chease_codepar_choices

end module assign_chease_codeparameters_choices_xml2eg
