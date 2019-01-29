
module euITM_routines

! GET/PUT routines for ITM Phase 4.05a data structure
! Generated 12/03/2008

use euITM_schemas


! Declaration of the generic CPO GET routine 
interface euITM_get        
   module procedure &  
   euITM_get_topinfo, &
   euITM_get_summary, &
   euITM_get_controllers, &
   euITM_get_coreprof, &
   euITM_get_equilibrium, &
   euITM_get_ironmodel, &
   euITM_get_interfdiag, & 
   euITM_get_limiter, &
   euITM_get_mhd, &
   euITM_get_magdiag, &
   euITM_get_msediag, &
   euITM_get_pfsystems, &
   euITM_get_toroidfield, &
   euITM_get_vessel 
end interface

! Declaration of the generic CPO GET_SLICE routine 
interface euITM_get_slice        
   module procedure &  
   euITM_get_slice_topinfo, &
   euITM_get_slice_summary, &
   euITM_get_slice_controllers, &
   euITM_get_slice_coreprof, &
   euITM_get_slice_equilibrium, &
   euITM_get_slice_ironmodel, &
   euITM_get_slice_interfdiag, & 
   euITM_get_slice_limiter, &
   euITM_get_slice_mhd, &
   euITM_get_slice_magdiag, &
   euITM_get_slice_msediag, &
   euITM_get_slice_pfsystems, &
   euITM_get_slice_toroidfield, &
   euITM_get_slice_vessel 
end interface

! Declaration of the generic CPO PUT_SLICE routine 
interface euITM_put_slice        
   module procedure &  
   euITM_put_slice_controllers, &
   euITM_put_slice_coreprof, &
   euITM_put_slice_equilibrium, &
   euITM_put_slice_ironmodel, &
   euITM_put_slice_interfdiag, & 
   euITM_put_slice_mhd, &
   euITM_put_slice_magdiag, &
   euITM_put_slice_msediag, &
   euITM_put_slice_pfsystems, &
   euITM_put_slice_toroidfield 
end interface


! Declaration of the generic CPO PUT routine 
interface euITM_put        
   module procedure &  
   euITM_put_topinfo, &
   euITM_put_summary, &
   euITM_put_controllers, &
   euITM_put_coreprof, &
   euITM_put_equilibrium, &
   euITM_put_ironmodel, &
   euITM_put_interfdiag, & 
   euITM_put_limiter, &
   euITM_put_mhd, &
   euITM_put_magdiag, &
   euITM_put_msediag, &
   euITM_put_pfsystems, &
   euITM_put_toroidfield, &
   euITM_put_vessel 
end interface

! Declaration of the generic CPO PUT_NON_TIMED routine 
interface euITM_put_non_timed        
   module procedure &  
   euITM_put_topinfo, &
   euITM_put_summary, &
   euITM_put_non_timed_controllers, &
   euITM_put_non_timed_coreprof, &
   euITM_put_non_timed_equilibrium, &
   euITM_put_non_timed_ironmodel, &
   euITM_put_non_timed_interfdiag, & 
   euITM_put_limiter, &
   euITM_put_non_timed_mhd, &
   euITM_put_non_timed_magdiag, &
   euITM_put_non_timed_msediag, &
   euITM_put_non_timed_pfsystems, &
   euITM_put_non_timed_toroidfield, &
   euITM_put_vessel 
end interface


contains
! All routines specialised for each CPO

!!!!!! Routines to GET the full CPOs (including the various time indices if time-dependent)
 
subroutine euITM_get_topinfo(idx,path,  cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)
character*(*) :: path
integer :: idx, status, lenstring, istring
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4

character(len=132)::stringans      ! Temporary way of getting short strings
character(len=100000)::longstring
character(len=132), dimension(:), pointer ::stringpointer        

integer :: int0d
real(DP) :: double0d



type(type_topinfo) :: cpo       
call begin_cpo_get(idx, path,0,dum1)
      
! Get dataprovider  
longstring = ' '
call get_string(idx,path, "dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%dataprovider'
endif   

! Get description  
longstring = ' '
call get_string(idx,path, "description",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%description(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%description = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%description(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%description'
endif   

! Get firstputdate  
longstring = ' '
call get_string(idx,path, "firstputdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%firstputdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%firstputdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%firstputdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%firstputdate'
endif   

! Get lastupdate  
longstring = ' '
call get_string(idx,path, "lastupdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%lastupdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%lastupdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%lastupdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%lastupdate'
endif   

! Get source  
longstring = ' '
call get_string(idx,path, "source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%source'
endif   

! Get comment  
longstring = ' '
call get_string(idx,path, "comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%comment'
endif   

! Get dataversion  
longstring = ' '
call get_string(idx,path, "dataversion",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%dataversion(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%dataversion = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%dataversion(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%dataversion'
endif   

! Get workflow  
longstring = ' '
call get_string(idx,path, "workflow",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%workflow(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%workflow = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%workflow(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%workflow'
endif   

! Get user  
longstring = ' '
call get_string(idx,path, "user",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%user(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%user = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%user(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%user'
endif   

! Get shot 
call get_int(idx,path, "shot",int0d,status)
if (status.EQ.0) then
   cpo%shot = int0d
   write(*,*) 'Get cpo%shot'
endif                 

! Get entry 
call get_int(idx,path, "entry",int0d,status)
if (status.EQ.0) then
   cpo%entry = int0d
   write(*,*) 'Get cpo%entry'
endif                 

! Get machine  
longstring = ' '
call get_string(idx,path, "machine",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%machine(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%machine = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%machine(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%machine'
endif   

! Get treename  
longstring = ' '
call get_string(idx,path, "treename",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%treename(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%treename = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%treename(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%treename'
endif   

! Get ref_user  
longstring = ' '
call get_string(idx,path, "ref_user",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%ref_user(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%ref_user = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%ref_user(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%ref_user'
endif   

! Get ref_shot 
call get_int(idx,path, "ref_shot",int0d,status)
if (status.EQ.0) then
   cpo%ref_shot = int0d
   write(*,*) 'Get cpo%ref_shot'
endif                 

! Get ref_entry 
call get_int(idx,path, "ref_entry",int0d,status)
if (status.EQ.0) then
   cpo%ref_entry = int0d
   write(*,*) 'Get cpo%ref_entry'
endif                 

! Get children        
call get_dimension(idx,path, "children",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%children(dim1))
   call get_vect1d_int(idx,path,"children",cpo%children,dim1,dum1,status)
   write(*,*) 'Get cpo%children'
endif 

! Get mdinfo/shot_min 
call get_int(idx,path, "mdinfo/shot_min",int0d,status)
if (status.EQ.0) then
   cpo%mdinfo%shot_min = int0d
   write(*,*) 'Get cpo%mdinfo%shot_min'
endif                 

! Get mdinfo/shot_max 
call get_int(idx,path, "mdinfo/shot_max",int0d,status)
if (status.EQ.0) then
   cpo%mdinfo%shot_max = int0d
   write(*,*) 'Get cpo%mdinfo%shot_max'
endif                 

! Get mdinfo/md_entry 
call get_int(idx,path, "mdinfo/md_entry",int0d,status)
if (status.EQ.0) then
   cpo%mdinfo%md_entry = int0d
   write(*,*) 'Get cpo%mdinfo%md_entry'
endif                 

call end_cpo_get(idx, path)      
     
return
endsubroutine

subroutine euITM_get_summary(idx,path,  cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)
character*(*) :: path
integer :: idx, status, lenstring, istring
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4

character(len=132)::stringans      ! Temporary way of getting short strings
character(len=100000)::longstring
character(len=132), dimension(:), pointer ::stringpointer        

integer :: int0d
real(DP) :: double0d



type(type_summary) :: cpo       
call begin_cpo_get(idx, path,0,dum1)
      
! Get ip/value 
call get_double(idx,path, "ip/value",double0d,status)
if (status.EQ.0) then
   cpo%ip%value = double0d
   write(*,*) 'Get cpo%ip%value'
endif

! Get ip/source  
longstring = ' '
call get_string(idx,path, "ip/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%ip%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%ip%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%ip%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%ip%source'
endif   

! Get ip/time 
call get_double(idx,path, "ip/time",double0d,status)
if (status.EQ.0) then
   cpo%ip%time = double0d
   write(*,*) 'Get cpo%ip%time'
endif

! Get bvac_r/value 
call get_double(idx,path, "bvac_r/value",double0d,status)
if (status.EQ.0) then
   cpo%bvac_r%value = double0d
   write(*,*) 'Get cpo%bvac_r%value'
endif

! Get bvac_r/source  
longstring = ' '
call get_string(idx,path, "bvac_r/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%bvac_r%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%bvac_r%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%bvac_r%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%bvac_r%source'
endif   

! Get bvac_r/time 
call get_double(idx,path, "bvac_r/time",double0d,status)
if (status.EQ.0) then
   cpo%bvac_r%time = double0d
   write(*,*) 'Get cpo%bvac_r%time'
endif

! Get geom_axis_r/value 
call get_double(idx,path, "geom_axis_r/value",double0d,status)
if (status.EQ.0) then
   cpo%geom_axis_r%value = double0d
   write(*,*) 'Get cpo%geom_axis_r%value'
endif

! Get geom_axis_r/source  
longstring = ' '
call get_string(idx,path, "geom_axis_r/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%geom_axis_r%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%geom_axis_r%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%geom_axis_r%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%geom_axis_r%source'
endif   

! Get geom_axis_r/time 
call get_double(idx,path, "geom_axis_r/time",double0d,status)
if (status.EQ.0) then
   cpo%geom_axis_r%time = double0d
   write(*,*) 'Get cpo%geom_axis_r%time'
endif

! Get a_minor/value 
call get_double(idx,path, "a_minor/value",double0d,status)
if (status.EQ.0) then
   cpo%a_minor%value = double0d
   write(*,*) 'Get cpo%a_minor%value'
endif

! Get a_minor/source  
longstring = ' '
call get_string(idx,path, "a_minor/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%a_minor%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%a_minor%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%a_minor%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%a_minor%source'
endif   

! Get a_minor/time 
call get_double(idx,path, "a_minor/time",double0d,status)
if (status.EQ.0) then
   cpo%a_minor%time = double0d
   write(*,*) 'Get cpo%a_minor%time'
endif

! Get elongation/value 
call get_double(idx,path, "elongation/value",double0d,status)
if (status.EQ.0) then
   cpo%elongation%value = double0d
   write(*,*) 'Get cpo%elongation%value'
endif

! Get elongation/source  
longstring = ' '
call get_string(idx,path, "elongation/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%elongation%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%elongation%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%elongation%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%elongation%source'
endif   

! Get elongation/time 
call get_double(idx,path, "elongation/time",double0d,status)
if (status.EQ.0) then
   cpo%elongation%time = double0d
   write(*,*) 'Get cpo%elongation%time'
endif

! Get tria_lower/value 
call get_double(idx,path, "tria_lower/value",double0d,status)
if (status.EQ.0) then
   cpo%tria_lower%value = double0d
   write(*,*) 'Get cpo%tria_lower%value'
endif

! Get tria_lower/source  
longstring = ' '
call get_string(idx,path, "tria_lower/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%tria_lower%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%tria_lower%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%tria_lower%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%tria_lower%source'
endif   

! Get tria_lower/time 
call get_double(idx,path, "tria_lower/time",double0d,status)
if (status.EQ.0) then
   cpo%tria_lower%time = double0d
   write(*,*) 'Get cpo%tria_lower%time'
endif

! Get tria_upper/value 
call get_double(idx,path, "tria_upper/value",double0d,status)
if (status.EQ.0) then
   cpo%tria_upper%value = double0d
   write(*,*) 'Get cpo%tria_upper%value'
endif

! Get tria_upper/source  
longstring = ' '
call get_string(idx,path, "tria_upper/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%tria_upper%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%tria_upper%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%tria_upper%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%tria_upper%source'
endif   

! Get tria_upper/time 
call get_double(idx,path, "tria_upper/time",double0d,status)
if (status.EQ.0) then
   cpo%tria_upper%time = double0d
   write(*,*) 'Get cpo%tria_upper%time'
endif

! Get tev/value 
call get_double(idx,path, "tev/value",double0d,status)
if (status.EQ.0) then
   cpo%tev%value = double0d
   write(*,*) 'Get cpo%tev%value'
endif

! Get tev/source  
longstring = ' '
call get_string(idx,path, "tev/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%tev%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%tev%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%tev%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%tev%source'
endif   

! Get tev/time 
call get_double(idx,path, "tev/time",double0d,status)
if (status.EQ.0) then
   cpo%tev%time = double0d
   write(*,*) 'Get cpo%tev%time'
endif

! Get tiv/value 
call get_double(idx,path, "tiv/value",double0d,status)
if (status.EQ.0) then
   cpo%tiv%value = double0d
   write(*,*) 'Get cpo%tiv%value'
endif

! Get tiv/source  
longstring = ' '
call get_string(idx,path, "tiv/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%tiv%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%tiv%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%tiv%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%tiv%source'
endif   

! Get tiv/time 
call get_double(idx,path, "tiv/time",double0d,status)
if (status.EQ.0) then
   cpo%tiv%time = double0d
   write(*,*) 'Get cpo%tiv%time'
endif

! Get nev/value 
call get_double(idx,path, "nev/value",double0d,status)
if (status.EQ.0) then
   cpo%nev%value = double0d
   write(*,*) 'Get cpo%nev%value'
endif

! Get nev/source  
longstring = ' '
call get_string(idx,path, "nev/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%nev%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%nev%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%nev%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%nev%source'
endif   

! Get nev/time 
call get_double(idx,path, "nev/time",double0d,status)
if (status.EQ.0) then
   cpo%nev%time = double0d
   write(*,*) 'Get cpo%nev%time'
endif

! Get zeffv/value 
call get_double(idx,path, "zeffv/value",double0d,status)
if (status.EQ.0) then
   cpo%zeffv%value = double0d
   write(*,*) 'Get cpo%zeffv%value'
endif

! Get zeffv/source  
longstring = ' '
call get_string(idx,path, "zeffv/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%zeffv%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%zeffv%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%zeffv%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%zeffv%source'
endif   

! Get zeffv/time 
call get_double(idx,path, "zeffv/time",double0d,status)
if (status.EQ.0) then
   cpo%zeffv%time = double0d
   write(*,*) 'Get cpo%zeffv%time'
endif

! Get beta_pol/value 
call get_double(idx,path, "beta_pol/value",double0d,status)
if (status.EQ.0) then
   cpo%beta_pol%value = double0d
   write(*,*) 'Get cpo%beta_pol%value'
endif

! Get beta_pol/source  
longstring = ' '
call get_string(idx,path, "beta_pol/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%beta_pol%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%beta_pol%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%beta_pol%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%beta_pol%source'
endif   

! Get beta_pol/time 
call get_double(idx,path, "beta_pol/time",double0d,status)
if (status.EQ.0) then
   cpo%beta_pol%time = double0d
   write(*,*) 'Get cpo%beta_pol%time'
endif

! Get beta_tor/value 
call get_double(idx,path, "beta_tor/value",double0d,status)
if (status.EQ.0) then
   cpo%beta_tor%value = double0d
   write(*,*) 'Get cpo%beta_tor%value'
endif

! Get beta_tor/source  
longstring = ' '
call get_string(idx,path, "beta_tor/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%beta_tor%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%beta_tor%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%beta_tor%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%beta_tor%source'
endif   

! Get beta_tor/time 
call get_double(idx,path, "beta_tor/time",double0d,status)
if (status.EQ.0) then
   cpo%beta_tor%time = double0d
   write(*,*) 'Get cpo%beta_tor%time'
endif

! Get beta_normal/value 
call get_double(idx,path, "beta_normal/value",double0d,status)
if (status.EQ.0) then
   cpo%beta_normal%value = double0d
   write(*,*) 'Get cpo%beta_normal%value'
endif

! Get beta_normal/source  
longstring = ' '
call get_string(idx,path, "beta_normal/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%beta_normal%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%beta_normal%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%beta_normal%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%beta_normal%source'
endif   

! Get beta_normal/time 
call get_double(idx,path, "beta_normal/time",double0d,status)
if (status.EQ.0) then
   cpo%beta_normal%time = double0d
   write(*,*) 'Get cpo%beta_normal%time'
endif

! Get li/value 
call get_double(idx,path, "li/value",double0d,status)
if (status.EQ.0) then
   cpo%li%value = double0d
   write(*,*) 'Get cpo%li%value'
endif

! Get li/source  
longstring = ' '
call get_string(idx,path, "li/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%li%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%li%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%li%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%li%source'
endif   

! Get li/time 
call get_double(idx,path, "li/time",double0d,status)
if (status.EQ.0) then
   cpo%li%time = double0d
   write(*,*) 'Get cpo%li%time'
endif

! Get volume/value 
call get_double(idx,path, "volume/value",double0d,status)
if (status.EQ.0) then
   cpo%volume%value = double0d
   write(*,*) 'Get cpo%volume%value'
endif

! Get volume/source  
longstring = ' '
call get_string(idx,path, "volume/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%volume%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%volume%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%volume%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%volume%source'
endif   

! Get volume/time 
call get_double(idx,path, "volume/time",double0d,status)
if (status.EQ.0) then
   cpo%volume%time = double0d
   write(*,*) 'Get cpo%volume%time'
endif

! Get area/value 
call get_double(idx,path, "area/value",double0d,status)
if (status.EQ.0) then
   cpo%area%value = double0d
   write(*,*) 'Get cpo%area%value'
endif

! Get area/source  
longstring = ' '
call get_string(idx,path, "area/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%area%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%area%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%area%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%area%source'
endif   

! Get area/time 
call get_double(idx,path, "area/time",double0d,status)
if (status.EQ.0) then
   cpo%area%time = double0d
   write(*,*) 'Get cpo%area%time'
endif

! Get main_ion1_z/value 
call get_double(idx,path, "main_ion1_z/value",double0d,status)
if (status.EQ.0) then
   cpo%main_ion1_z%value = double0d
   write(*,*) 'Get cpo%main_ion1_z%value'
endif

! Get main_ion1_z/source  
longstring = ' '
call get_string(idx,path, "main_ion1_z/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%main_ion1_z%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%main_ion1_z%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%main_ion1_z%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%main_ion1_z%source'
endif   

! Get main_ion1_z/time 
call get_double(idx,path, "main_ion1_z/time",double0d,status)
if (status.EQ.0) then
   cpo%main_ion1_z%time = double0d
   write(*,*) 'Get cpo%main_ion1_z%time'
endif

! Get main_ion1_a/value 
call get_double(idx,path, "main_ion1_a/value",double0d,status)
if (status.EQ.0) then
   cpo%main_ion1_a%value = double0d
   write(*,*) 'Get cpo%main_ion1_a%value'
endif

! Get main_ion1_a/source  
longstring = ' '
call get_string(idx,path, "main_ion1_a/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%main_ion1_a%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%main_ion1_a%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%main_ion1_a%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%main_ion1_a%source'
endif   

! Get main_ion1_a/time 
call get_double(idx,path, "main_ion1_a/time",double0d,status)
if (status.EQ.0) then
   cpo%main_ion1_a%time = double0d
   write(*,*) 'Get cpo%main_ion1_a%time'
endif

! Get main_ion2_z/value 
call get_double(idx,path, "main_ion2_z/value",double0d,status)
if (status.EQ.0) then
   cpo%main_ion2_z%value = double0d
   write(*,*) 'Get cpo%main_ion2_z%value'
endif

! Get main_ion2_z/source  
longstring = ' '
call get_string(idx,path, "main_ion2_z/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%main_ion2_z%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%main_ion2_z%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%main_ion2_z%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%main_ion2_z%source'
endif   

! Get main_ion2_z/time 
call get_double(idx,path, "main_ion2_z/time",double0d,status)
if (status.EQ.0) then
   cpo%main_ion2_z%time = double0d
   write(*,*) 'Get cpo%main_ion2_z%time'
endif

! Get main_ion2_a/value 
call get_double(idx,path, "main_ion2_a/value",double0d,status)
if (status.EQ.0) then
   cpo%main_ion2_a%value = double0d
   write(*,*) 'Get cpo%main_ion2_a%value'
endif

! Get main_ion2_a/source  
longstring = ' '
call get_string(idx,path, "main_ion2_a/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%main_ion2_a%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%main_ion2_a%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%main_ion2_a%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%main_ion2_a%source'
endif   

! Get main_ion2_a/time 
call get_double(idx,path, "main_ion2_a/time",double0d,status)
if (status.EQ.0) then
   cpo%main_ion2_a%time = double0d
   write(*,*) 'Get cpo%main_ion2_a%time'
endif

! Get impur1_z/value 
call get_double(idx,path, "impur1_z/value",double0d,status)
if (status.EQ.0) then
   cpo%impur1_z%value = double0d
   write(*,*) 'Get cpo%impur1_z%value'
endif

! Get impur1_z/source  
longstring = ' '
call get_string(idx,path, "impur1_z/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%impur1_z%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%impur1_z%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%impur1_z%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%impur1_z%source'
endif   

! Get impur1_z/time 
call get_double(idx,path, "impur1_z/time",double0d,status)
if (status.EQ.0) then
   cpo%impur1_z%time = double0d
   write(*,*) 'Get cpo%impur1_z%time'
endif

! Get impur1_a/value 
call get_double(idx,path, "impur1_a/value",double0d,status)
if (status.EQ.0) then
   cpo%impur1_a%value = double0d
   write(*,*) 'Get cpo%impur1_a%value'
endif

! Get impur1_a/source  
longstring = ' '
call get_string(idx,path, "impur1_a/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%impur1_a%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%impur1_a%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%impur1_a%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%impur1_a%source'
endif   

! Get impur1_a/time 
call get_double(idx,path, "impur1_a/time",double0d,status)
if (status.EQ.0) then
   cpo%impur1_a%time = double0d
   write(*,*) 'Get cpo%impur1_a%time'
endif

call end_cpo_get(idx, path)      
     
return
endsubroutine

subroutine euITM_get_controllers(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_controllers),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%dataprovider'
endif

! Get datainfo/putdate        
longstring = ' '
call get_String(idx,path, "datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putdate'
endif

! Get datainfo/source        
longstring = ' '
call get_String(idx,path, "datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%source'
endif

! Get datainfo/comment        
longstring = ' '
call get_String(idx,path, "datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%comment'
endif

! Get datainfo/isref        
call get_Int(idx,path, "datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%datainfo%isref'
endif

! Get datainfo/whatref        
call get_Int(idx,path, "datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%datainfo%whatref'
endif

! Get datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putmethod'
endif

! Get datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putaccess'
endif

! Get datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putlocation'
endif

! Get datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%rights'
endif

! Get name        
longstring = ' '
call get_String(idx,path, "name",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%name(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%name(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%name(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%name'
endif

! Get purpose        
longstring = ' '
call get_String(idx,path, "purpose",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%purpose(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%purpose(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%purpose(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%purpose'
endif

! Get type        
longstring = ' '
call get_String(idx,path, "type",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%type(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%type(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%type(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%type'
endif

! Get input  
call get_dimension(idx,path, "input",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%input(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "input", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%input = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%input'
endif   

! Get output  
call get_dimension(idx,path, "output",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%output(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "output", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%output = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%output'
endif   

! Get statespace/observable  
call get_dimension(idx,path, "statespace/observable",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%statespace%observable(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "statespace/observable", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%statespace%observable = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%statespace%observable'
endif   

! Get statespace/A        
call get_dimension(idx,path, "statespace/A",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "statespace/A", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%statespace%A(dim1,dim2))
      cpos(itime)%statespace%A = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%statespace%A'
   deallocate(vect2DDouble)
endif 

! Get statespace/B        
call get_dimension(idx,path, "statespace/B",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "statespace/B", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%statespace%B(dim1,dim2))
      cpos(itime)%statespace%B = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%statespace%B'
   deallocate(vect2DDouble)
endif 

! Get statespace/C        
call get_dimension(idx,path, "statespace/C",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "statespace/C", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%statespace%C(dim1,dim2))
      cpos(itime)%statespace%C = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%statespace%C'
   deallocate(vect2DDouble)
endif 

! Get statespace/D        
call get_dimension(idx,path, "statespace/D",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "statespace/D", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%statespace%D(dim1,dim2))
      cpos(itime)%statespace%D = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%statespace%D'
   deallocate(vect2DDouble)
endif 

! Get statespace/deltat   
call get_Double(idx,path, "statespace/deltat",double0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%statespace%deltat = double0D   
   write(*,*) 'Get cpos%statespace%deltat'
endif

! Get target     
call get_dimension(idx,path, "target",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"target",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%target(dim1))
      cpos(itime)%target = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%target'
   deallocate(vect2DDouble)
endif   

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_coreprof(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_coreprof),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get rho_tor     
call get_dimension(idx,path, "rho_tor",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"rho_tor",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%rho_tor(dim1))
      cpos(itime)%rho_tor = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%rho_tor'
   deallocate(vect2DDouble)
endif   

! Get psi     
call get_dimension(idx,path, "psi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"psi",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%psi(dim1))
      cpos(itime)%psi = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%psi'
   deallocate(vect2DDouble)
endif   

! Get pressure/value     
call get_dimension(idx,path, "pressure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pressure/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pressure%value(dim1))
      cpos(itime)%pressure%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pressure%value'
   deallocate(vect2DDouble)
endif   

! Get pressure/source  TIME-DEPENDENT STRING : NOT TREATED YET ... (NOT ALLOWED IN SCHEMAS YET !!!) 
! Get jparallel/value     
call get_dimension(idx,path, "jparallel/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"jparallel/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%jparallel%value(dim1))
      cpos(itime)%jparallel%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%jparallel%value'
   deallocate(vect2DDouble)
endif   

! Get jparallel/source  TIME-DEPENDENT STRING : NOT TREATED YET ... (NOT ALLOWED IN SCHEMAS YET !!!) 
! Get q/value     
call get_dimension(idx,path, "q/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"q/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%q%value(dim1))
      cpos(itime)%q%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%q%value'
   deallocate(vect2DDouble)
endif   

! Get q/source  TIME-DEPENDENT STRING : NOT TREATED YET ... (NOT ALLOWED IN SCHEMAS YET !!!) 
! Get codeparam/codename        
longstring = ' '
call get_String(idx,path, "codeparam/codename",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%codename(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%codename(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%codename(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%codename'
endif

! Get codeparam/codeversion        
longstring = ' '
call get_String(idx,path, "codeparam/codeversion",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%codeversion(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%codeversion(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%codeversion(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%codeversion'
endif

! Get codeparam/parameters        
longstring = ' '
call get_String(idx,path, "codeparam/parameters",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%parameters(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%parameters(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%parameters(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%parameters'
endif

! Get codeparam/output_diag        
longstring = ' '
call get_String(idx,path, "codeparam/output_diag",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%output_diag(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%output_diag(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%output_diag(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%output_diag'
endif

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_equilibrium(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_equilibrium),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%dataprovider'
endif

! Get datainfo/putdate        
longstring = ' '
call get_String(idx,path, "datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putdate'
endif

! Get datainfo/source        
longstring = ' '
call get_String(idx,path, "datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%source'
endif

! Get datainfo/comment        
longstring = ' '
call get_String(idx,path, "datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%comment'
endif

! Get datainfo/isref        
call get_Int(idx,path, "datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%datainfo%isref'
endif

! Get datainfo/whatref        
call get_Int(idx,path, "datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%datainfo%whatref'
endif

! Get datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putmethod'
endif

! Get datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putaccess'
endif

! Get datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putlocation'
endif

! Get datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%rights'
endif

! Get eqconstraint/bvac_r/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/bvac_r/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%bvac_r%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%bvac_r%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%bvac_r%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%bvac_r%source'
endif

! Get eqconstraint/bvac_r/time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqconstraint/bvac_r/time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%bvac_r%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqconstraint%bvac_r%time'
endif
deallocate(vect1Ddouble)

! Get eqconstraint/bvac_r/exact        
call get_Int(idx,path, "eqconstraint/bvac_r/exact",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%bvac_r%exact = Int0D   
   write(*,*) 'Get cpos%eqconstraint%bvac_r%exact'
endif

! Get eqconstraint/bvac_r/weight        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqconstraint/bvac_r/weight",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%bvac_r%weight = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqconstraint%bvac_r%weight'
endif
deallocate(vect1Ddouble)

! Get eqconstraint/bvac_r/sigma        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqconstraint/bvac_r/sigma",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%bvac_r%sigma = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqconstraint%bvac_r%sigma'
endif
deallocate(vect1Ddouble)

! Get eqconstraint/bvac_r/calculated        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqconstraint/bvac_r/calculated",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%bvac_r%calculated = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqconstraint%bvac_r%calculated'
endif
deallocate(vect1Ddouble)

! Get eqconstraint/bvac_r/chi2        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqconstraint/bvac_r/chi2",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%bvac_r%chi2 = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqconstraint%bvac_r%chi2'
endif
deallocate(vect1Ddouble)

! Get eqconstraint/i_plasma/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/i_plasma/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%i_plasma%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%i_plasma%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%i_plasma%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%i_plasma%source'
endif

! Get eqconstraint/i_plasma/time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqconstraint/i_plasma/time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%i_plasma%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqconstraint%i_plasma%time'
endif
deallocate(vect1Ddouble)

! Get eqconstraint/i_plasma/exact        
call get_Int(idx,path, "eqconstraint/i_plasma/exact",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%i_plasma%exact = Int0D   
   write(*,*) 'Get cpos%eqconstraint%i_plasma%exact'
endif

! Get eqconstraint/i_plasma/weight        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqconstraint/i_plasma/weight",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%i_plasma%weight = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqconstraint%i_plasma%weight'
endif
deallocate(vect1Ddouble)

! Get eqconstraint/i_plasma/sigma        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqconstraint/i_plasma/sigma",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%i_plasma%sigma = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqconstraint%i_plasma%sigma'
endif
deallocate(vect1Ddouble)

! Get eqconstraint/i_plasma/calculated        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqconstraint/i_plasma/calculated",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%i_plasma%calculated = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqconstraint%i_plasma%calculated'
endif
deallocate(vect1Ddouble)

! Get eqconstraint/i_plasma/chi2        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqconstraint/i_plasma/chi2",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%i_plasma%chi2 = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqconstraint%i_plasma%chi2'
endif
deallocate(vect1Ddouble)

! Get eqconstraint/magnet_iron/mr/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/magnet_iron/mr/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%magnet_iron%mr%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%magnet_iron%mr%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%magnet_iron%mr%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mr%source'
endif

! Get eqconstraint/magnet_iron/mr/time     
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/magnet_iron/mr/time",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mr%time(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mr%time = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mr%time'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/magnet_iron/mr/exact        
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2Dint(dim1,dim2)) 
   call get_vect2d_int(idx,path,"eqconstraint/magnet_iron/mr/exact", &
   vect2Dint,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mr%exact(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mr%exact = vect2Dint(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mr%exact'
   deallocate(vect2Dint)
endif   

! Get eqconstraint/magnet_iron/mr/weight     
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/magnet_iron/mr/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mr%weight(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mr%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mr%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/magnet_iron/mr/sigma     
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/magnet_iron/mr/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mr%sigma(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mr%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mr%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/magnet_iron/mr/calculated     
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/magnet_iron/mr/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mr%calculated(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mr%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mr%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/magnet_iron/mr/chi2     
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/magnet_iron/mr/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mr%chi2(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mr%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mr%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/magnet_iron/mz/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/magnet_iron/mz/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%magnet_iron%mz%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%magnet_iron%mz%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%magnet_iron%mz%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mz%source'
endif

! Get eqconstraint/magnet_iron/mz/time     
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/magnet_iron/mz/time",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mz%time(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mz%time = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mz%time'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/magnet_iron/mz/exact        
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2Dint(dim1,dim2)) 
   call get_vect2d_int(idx,path,"eqconstraint/magnet_iron/mz/exact", &
   vect2Dint,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mz%exact(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mz%exact = vect2Dint(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mz%exact'
   deallocate(vect2Dint)
endif   

! Get eqconstraint/magnet_iron/mz/weight     
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/magnet_iron/mz/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mz%weight(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mz%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mz%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/magnet_iron/mz/sigma     
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/magnet_iron/mz/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mz%sigma(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mz%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mz%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/magnet_iron/mz/calculated     
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/magnet_iron/mz/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mz%calculated(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mz%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mz%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/magnet_iron/mz/chi2     
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/magnet_iron/mz/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%magnet_iron%mz%chi2(dim1))
      cpos(itime)%eqconstraint%magnet_iron%mz%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%magnet_iron%mz%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/bpol/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/bpol/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%bpol%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%bpol%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%bpol%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%bpol%source'
endif

! Get eqconstraint/bpol/time     
call get_dimension(idx,path, "eqconstraint/bpol/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/bpol/time",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%bpol%time(dim1))
      cpos(itime)%eqconstraint%bpol%time = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%bpol%time'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/bpol/exact        
call get_dimension(idx,path, "eqconstraint/bpol/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2Dint(dim1,dim2)) 
   call get_vect2d_int(idx,path,"eqconstraint/bpol/exact", &
   vect2Dint,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%bpol%exact(dim1))
      cpos(itime)%eqconstraint%bpol%exact = vect2Dint(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%bpol%exact'
   deallocate(vect2Dint)
endif   

! Get eqconstraint/bpol/weight     
call get_dimension(idx,path, "eqconstraint/bpol/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/bpol/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%bpol%weight(dim1))
      cpos(itime)%eqconstraint%bpol%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%bpol%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/bpol/sigma     
call get_dimension(idx,path, "eqconstraint/bpol/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/bpol/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%bpol%sigma(dim1))
      cpos(itime)%eqconstraint%bpol%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%bpol%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/bpol/calculated     
call get_dimension(idx,path, "eqconstraint/bpol/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/bpol/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%bpol%calculated(dim1))
      cpos(itime)%eqconstraint%bpol%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%bpol%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/bpol/chi2     
call get_dimension(idx,path, "eqconstraint/bpol/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/bpol/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%bpol%chi2(dim1))
      cpos(itime)%eqconstraint%bpol%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%bpol%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/flux/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/flux/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%flux%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%flux%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%flux%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%flux%source'
endif

! Get eqconstraint/flux/time     
call get_dimension(idx,path, "eqconstraint/flux/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/flux/time",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%flux%time(dim1))
      cpos(itime)%eqconstraint%flux%time = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%flux%time'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/flux/exact        
call get_dimension(idx,path, "eqconstraint/flux/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2Dint(dim1,dim2)) 
   call get_vect2d_int(idx,path,"eqconstraint/flux/exact", &
   vect2Dint,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%flux%exact(dim1))
      cpos(itime)%eqconstraint%flux%exact = vect2Dint(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%flux%exact'
   deallocate(vect2Dint)
endif   

! Get eqconstraint/flux/weight     
call get_dimension(idx,path, "eqconstraint/flux/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/flux/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%flux%weight(dim1))
      cpos(itime)%eqconstraint%flux%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%flux%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/flux/sigma     
call get_dimension(idx,path, "eqconstraint/flux/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/flux/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%flux%sigma(dim1))
      cpos(itime)%eqconstraint%flux%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%flux%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/flux/calculated     
call get_dimension(idx,path, "eqconstraint/flux/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/flux/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%flux%calculated(dim1))
      cpos(itime)%eqconstraint%flux%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%flux%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/flux/chi2     
call get_dimension(idx,path, "eqconstraint/flux/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/flux/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%flux%chi2(dim1))
      cpos(itime)%eqconstraint%flux%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%flux%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/mse/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/mse/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%mse%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%mse%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%mse%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%mse%source'
endif

! Get eqconstraint/mse/time     
call get_dimension(idx,path, "eqconstraint/mse/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/mse/time",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%mse%time(dim1))
      cpos(itime)%eqconstraint%mse%time = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%mse%time'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/mse/exact        
call get_dimension(idx,path, "eqconstraint/mse/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2Dint(dim1,dim2)) 
   call get_vect2d_int(idx,path,"eqconstraint/mse/exact", &
   vect2Dint,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%mse%exact(dim1))
      cpos(itime)%eqconstraint%mse%exact = vect2Dint(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%mse%exact'
   deallocate(vect2Dint)
endif   

! Get eqconstraint/mse/weight     
call get_dimension(idx,path, "eqconstraint/mse/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/mse/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%mse%weight(dim1))
      cpos(itime)%eqconstraint%mse%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%mse%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/mse/sigma     
call get_dimension(idx,path, "eqconstraint/mse/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/mse/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%mse%sigma(dim1))
      cpos(itime)%eqconstraint%mse%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%mse%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/mse/calculated     
call get_dimension(idx,path, "eqconstraint/mse/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/mse/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%mse%calculated(dim1))
      cpos(itime)%eqconstraint%mse%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%mse%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/mse/chi2     
call get_dimension(idx,path, "eqconstraint/mse/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/mse/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%mse%chi2(dim1))
      cpos(itime)%eqconstraint%mse%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%mse%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/faraday/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/faraday/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%faraday%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%faraday%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%faraday%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%faraday%source'
endif

! Get eqconstraint/faraday/time     
call get_dimension(idx,path, "eqconstraint/faraday/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/faraday/time",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%faraday%time(dim1))
      cpos(itime)%eqconstraint%faraday%time = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%faraday%time'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/faraday/exact        
call get_dimension(idx,path, "eqconstraint/faraday/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2Dint(dim1,dim2)) 
   call get_vect2d_int(idx,path,"eqconstraint/faraday/exact", &
   vect2Dint,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%faraday%exact(dim1))
      cpos(itime)%eqconstraint%faraday%exact = vect2Dint(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%faraday%exact'
   deallocate(vect2Dint)
endif   

! Get eqconstraint/faraday/weight     
call get_dimension(idx,path, "eqconstraint/faraday/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/faraday/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%faraday%weight(dim1))
      cpos(itime)%eqconstraint%faraday%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%faraday%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/faraday/sigma     
call get_dimension(idx,path, "eqconstraint/faraday/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/faraday/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%faraday%sigma(dim1))
      cpos(itime)%eqconstraint%faraday%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%faraday%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/faraday/calculated     
call get_dimension(idx,path, "eqconstraint/faraday/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/faraday/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%faraday%calculated(dim1))
      cpos(itime)%eqconstraint%faraday%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%faraday%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/faraday/chi2     
call get_dimension(idx,path, "eqconstraint/faraday/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/faraday/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%faraday%chi2(dim1))
      cpos(itime)%eqconstraint%faraday%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%faraday%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/pfcurrent/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/pfcurrent/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%pfcurrent%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%pfcurrent%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%pfcurrent%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%pfcurrent%source'
endif

! Get eqconstraint/pfcurrent/time     
call get_dimension(idx,path, "eqconstraint/pfcurrent/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/pfcurrent/time",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pfcurrent%time(dim1))
      cpos(itime)%eqconstraint%pfcurrent%time = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pfcurrent%time'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/pfcurrent/exact        
call get_dimension(idx,path, "eqconstraint/pfcurrent/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2Dint(dim1,dim2)) 
   call get_vect2d_int(idx,path,"eqconstraint/pfcurrent/exact", &
   vect2Dint,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pfcurrent%exact(dim1))
      cpos(itime)%eqconstraint%pfcurrent%exact = vect2Dint(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pfcurrent%exact'
   deallocate(vect2Dint)
endif   

! Get eqconstraint/pfcurrent/weight     
call get_dimension(idx,path, "eqconstraint/pfcurrent/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/pfcurrent/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pfcurrent%weight(dim1))
      cpos(itime)%eqconstraint%pfcurrent%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pfcurrent%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/pfcurrent/sigma     
call get_dimension(idx,path, "eqconstraint/pfcurrent/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/pfcurrent/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pfcurrent%sigma(dim1))
      cpos(itime)%eqconstraint%pfcurrent%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pfcurrent%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/pfcurrent/calculated     
call get_dimension(idx,path, "eqconstraint/pfcurrent/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/pfcurrent/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pfcurrent%calculated(dim1))
      cpos(itime)%eqconstraint%pfcurrent%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pfcurrent%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/pfcurrent/chi2     
call get_dimension(idx,path, "eqconstraint/pfcurrent/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/pfcurrent/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pfcurrent%chi2(dim1))
      cpos(itime)%eqconstraint%pfcurrent%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pfcurrent%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/pressure/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/pressure/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%pressure%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%pressure%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%pressure%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%pressure%source'
endif

! Get eqconstraint/pressure/time     
call get_dimension(idx,path, "eqconstraint/pressure/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/pressure/time",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pressure%time(dim1))
      cpos(itime)%eqconstraint%pressure%time = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pressure%time'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/pressure/exact        
call get_dimension(idx,path, "eqconstraint/pressure/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2Dint(dim1,dim2)) 
   call get_vect2d_int(idx,path,"eqconstraint/pressure/exact", &
   vect2Dint,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pressure%exact(dim1))
      cpos(itime)%eqconstraint%pressure%exact = vect2Dint(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pressure%exact'
   deallocate(vect2Dint)
endif   

! Get eqconstraint/pressure/weight     
call get_dimension(idx,path, "eqconstraint/pressure/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/pressure/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pressure%weight(dim1))
      cpos(itime)%eqconstraint%pressure%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pressure%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/pressure/sigma     
call get_dimension(idx,path, "eqconstraint/pressure/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/pressure/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pressure%sigma(dim1))
      cpos(itime)%eqconstraint%pressure%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pressure%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/pressure/calculated     
call get_dimension(idx,path, "eqconstraint/pressure/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/pressure/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pressure%calculated(dim1))
      cpos(itime)%eqconstraint%pressure%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pressure%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/pressure/chi2     
call get_dimension(idx,path, "eqconstraint/pressure/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/pressure/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%pressure%chi2(dim1))
      cpos(itime)%eqconstraint%pressure%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%pressure%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/jsurf/source        
longstring = ' '
call get_String(idx,path, "eqconstraint/jsurf/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqconstraint%jsurf%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqconstraint%jsurf%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqconstraint%jsurf%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqconstraint%jsurf%source'
endif

! Get eqconstraint/jsurf/time     
call get_dimension(idx,path, "eqconstraint/jsurf/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/jsurf/time",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%jsurf%time(dim1))
      cpos(itime)%eqconstraint%jsurf%time = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%jsurf%time'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/jsurf/exact        
call get_dimension(idx,path, "eqconstraint/jsurf/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2Dint(dim1,dim2)) 
   call get_vect2d_int(idx,path,"eqconstraint/jsurf/exact", &
   vect2Dint,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%jsurf%exact(dim1))
      cpos(itime)%eqconstraint%jsurf%exact = vect2Dint(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%jsurf%exact'
   deallocate(vect2Dint)
endif   

! Get eqconstraint/jsurf/weight     
call get_dimension(idx,path, "eqconstraint/jsurf/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/jsurf/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%jsurf%weight(dim1))
      cpos(itime)%eqconstraint%jsurf%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%jsurf%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/jsurf/sigma     
call get_dimension(idx,path, "eqconstraint/jsurf/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/jsurf/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%jsurf%sigma(dim1))
      cpos(itime)%eqconstraint%jsurf%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%jsurf%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/jsurf/calculated     
call get_dimension(idx,path, "eqconstraint/jsurf/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/jsurf/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%jsurf%calculated(dim1))
      cpos(itime)%eqconstraint%jsurf%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%jsurf%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/jsurf/chi2     
call get_dimension(idx,path, "eqconstraint/jsurf/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/jsurf/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%jsurf%chi2(dim1))
      cpos(itime)%eqconstraint%jsurf%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%jsurf%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/q/qvalue     
call get_dimension(idx,path, "eqconstraint/q/qvalue",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/q/qvalue",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%q%qvalue(dim1))
      cpos(itime)%eqconstraint%q%qvalue = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%q%qvalue'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/q/position/r     
call get_dimension(idx,path, "eqconstraint/q/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/q/position/r",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%q%position%r(dim1))
      cpos(itime)%eqconstraint%q%position%r = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%q%position%r'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/q/position/z     
call get_dimension(idx,path, "eqconstraint/q/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/q/position/z",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%q%position%z(dim1))
      cpos(itime)%eqconstraint%q%position%z = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%q%position%z'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/q/exact        
call get_Int(idx,path, "eqconstraint/q/exact",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%eqconstraint%q%exact = Int0D   
   write(*,*) 'Get cpos%eqconstraint%q%exact'
endif

! Get eqconstraint/q/weight     
call get_dimension(idx,path, "eqconstraint/q/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/q/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%q%weight(dim1))
      cpos(itime)%eqconstraint%q%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%q%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/q/sigma     
call get_dimension(idx,path, "eqconstraint/q/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/q/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%q%sigma(dim1))
      cpos(itime)%eqconstraint%q%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%q%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/q/calculated     
call get_dimension(idx,path, "eqconstraint/q/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/q/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%q%calculated(dim1))
      cpos(itime)%eqconstraint%q%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%q%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/q/chi2     
call get_dimension(idx,path, "eqconstraint/q/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/q/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%q%chi2(dim1))
      cpos(itime)%eqconstraint%q%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%q%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/isoflux/position/r     
call get_dimension(idx,path, "eqconstraint/isoflux/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/isoflux/position/r",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%isoflux%position%r(dim1))
      cpos(itime)%eqconstraint%isoflux%position%r = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%isoflux%position%r'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/isoflux/position/z     
call get_dimension(idx,path, "eqconstraint/isoflux/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/isoflux/position/z",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%isoflux%position%z(dim1))
      cpos(itime)%eqconstraint%isoflux%position%z = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%isoflux%position%z'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/isoflux/weight     
call get_dimension(idx,path, "eqconstraint/isoflux/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/isoflux/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%isoflux%weight(dim1))
      cpos(itime)%eqconstraint%isoflux%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%isoflux%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/isoflux/sigma     
call get_dimension(idx,path, "eqconstraint/isoflux/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/isoflux/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%isoflux%sigma(dim1))
      cpos(itime)%eqconstraint%isoflux%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%isoflux%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/isoflux/calculated     
call get_dimension(idx,path, "eqconstraint/isoflux/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/isoflux/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%isoflux%calculated(dim1))
      cpos(itime)%eqconstraint%isoflux%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%isoflux%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/isoflux/chi2     
call get_dimension(idx,path, "eqconstraint/isoflux/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/isoflux/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%isoflux%chi2(dim1))
      cpos(itime)%eqconstraint%isoflux%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%isoflux%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/xpts/position/r     
call get_dimension(idx,path, "eqconstraint/xpts/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/xpts/position/r",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%xpts%position%r(dim1))
      cpos(itime)%eqconstraint%xpts%position%r = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%xpts%position%r'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/xpts/position/z     
call get_dimension(idx,path, "eqconstraint/xpts/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/xpts/position/z",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%xpts%position%z(dim1))
      cpos(itime)%eqconstraint%xpts%position%z = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%xpts%position%z'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/xpts/weight     
call get_dimension(idx,path, "eqconstraint/xpts/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/xpts/weight",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%xpts%weight(dim1))
      cpos(itime)%eqconstraint%xpts%weight = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%xpts%weight'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/xpts/sigma     
call get_dimension(idx,path, "eqconstraint/xpts/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/xpts/sigma",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%xpts%sigma(dim1))
      cpos(itime)%eqconstraint%xpts%sigma = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%xpts%sigma'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/xpts/calculated     
call get_dimension(idx,path, "eqconstraint/xpts/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/xpts/calculated",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%xpts%calculated(dim1))
      cpos(itime)%eqconstraint%xpts%calculated = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%xpts%calculated'
   deallocate(vect2DDouble)
endif   

! Get eqconstraint/xpts/chi2     
call get_dimension(idx,path, "eqconstraint/xpts/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqconstraint/xpts/chi2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqconstraint%xpts%chi2(dim1))
      cpos(itime)%eqconstraint%xpts%chi2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqconstraint%xpts%chi2'
   deallocate(vect2DDouble)
endif   

! Get eqgeometry/datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "eqgeometry/datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqgeometry%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqgeometry%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqgeometry%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqgeometry%datainfo%dataprovider'
endif

! Get eqgeometry/datainfo/putdate        
longstring = ' '
call get_String(idx,path, "eqgeometry/datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqgeometry%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqgeometry%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqgeometry%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqgeometry%datainfo%putdate'
endif

! Get eqgeometry/datainfo/source        
longstring = ' '
call get_String(idx,path, "eqgeometry/datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqgeometry%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqgeometry%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqgeometry%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqgeometry%datainfo%source'
endif

! Get eqgeometry/datainfo/comment        
longstring = ' '
call get_String(idx,path, "eqgeometry/datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqgeometry%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqgeometry%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqgeometry%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqgeometry%datainfo%comment'
endif

! Get eqgeometry/datainfo/isref        
call get_Int(idx,path, "eqgeometry/datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%eqgeometry%datainfo%isref'
endif

! Get eqgeometry/datainfo/whatref        
call get_Int(idx,path, "eqgeometry/datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%eqgeometry%datainfo%whatref'
endif

! Get eqgeometry/datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "eqgeometry/datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqgeometry%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqgeometry%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqgeometry%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqgeometry%datainfo%putinfo%putmethod'
endif

! Get eqgeometry/datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "eqgeometry/datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqgeometry%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqgeometry%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqgeometry%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqgeometry%datainfo%putinfo%putaccess'
endif

! Get eqgeometry/datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "eqgeometry/datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqgeometry%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqgeometry%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqgeometry%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqgeometry%datainfo%putinfo%putlocation'
endif

! Get eqgeometry/datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "eqgeometry/datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%eqgeometry%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%eqgeometry%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%eqgeometry%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%eqgeometry%datainfo%putinfo%rights'
endif

! Get eqgeometry/boundarytype  TIME-DEPENDENT STRING : NOT TREATED YET ... (NOT ALLOWED IN SCHEMAS YET !!!) 
! Get eqgeometry/boundary/r     
call get_dimension(idx,path, "eqgeometry/boundary/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqgeometry/boundary/r",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqgeometry%boundary%r(dim1))
      cpos(itime)%eqgeometry%boundary%r = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqgeometry%boundary%r'
   deallocate(vect2DDouble)
endif   

! Get eqgeometry/boundary/z     
call get_dimension(idx,path, "eqgeometry/boundary/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqgeometry/boundary/z",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqgeometry%boundary%z(dim1))
      cpos(itime)%eqgeometry%boundary%z = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqgeometry%boundary%z'
   deallocate(vect2DDouble)
endif   

! Get eqgeometry/geom_axis/r        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/geom_axis/r",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%geom_axis%r = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%geom_axis%r'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/geom_axis/z        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/geom_axis/z",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%geom_axis%z = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%geom_axis%z'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/a_minor        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/a_minor",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%a_minor = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%a_minor'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/elongation        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/elongation",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%elongation = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%elongation'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/tria_upper        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/tria_upper",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%tria_upper = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%tria_upper'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/tria_lower        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/tria_lower",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%tria_lower = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%tria_lower'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/xpts/r     
call get_dimension(idx,path, "eqgeometry/xpts/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqgeometry/xpts/r",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqgeometry%xpts%r(dim1))
      cpos(itime)%eqgeometry%xpts%r = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqgeometry%xpts%r'
   deallocate(vect2DDouble)
endif   

! Get eqgeometry/xpts/z     
call get_dimension(idx,path, "eqgeometry/xpts/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"eqgeometry/xpts/z",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%eqgeometry%xpts%z(dim1))
      cpos(itime)%eqgeometry%xpts%z = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%eqgeometry%xpts%z'
   deallocate(vect2DDouble)
endif   

! Get eqgeometry/left_low_st/r        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/left_low_st/r",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%left_low_st%r = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%left_low_st%r'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/left_low_st/z        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/left_low_st/z",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%left_low_st%z = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%left_low_st%z'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/right_low_st/r        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/right_low_st/r",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%right_low_st%r = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%right_low_st%r'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/right_low_st/z        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/right_low_st/z",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%right_low_st%z = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%right_low_st%z'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/left_up_st/r        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/left_up_st/r",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%left_up_st%r = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%left_up_st%r'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/left_up_st/z        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/left_up_st/z",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%left_up_st%z = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%left_up_st%z'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/right_up_st/r        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/right_up_st/r",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%right_up_st%r = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%right_up_st%r'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/right_up_st/z        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/right_up_st/z",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%right_up_st%z = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%right_up_st%z'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/active_limit/r        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/active_limit/r",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%active_limit%r = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%active_limit%r'
endif
deallocate(vect1Ddouble)

! Get eqgeometry/active_limit/z        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"eqgeometry/active_limit/z",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%eqgeometry%active_limit%z = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%eqgeometry%active_limit%z'
endif
deallocate(vect1Ddouble)

! Get flush/datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "flush/datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%datainfo%dataprovider'
endif

! Get flush/datainfo/putdate        
longstring = ' '
call get_String(idx,path, "flush/datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%datainfo%putdate'
endif

! Get flush/datainfo/source        
longstring = ' '
call get_String(idx,path, "flush/datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%datainfo%source'
endif

! Get flush/datainfo/comment        
longstring = ' '
call get_String(idx,path, "flush/datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%datainfo%comment'
endif

! Get flush/datainfo/isref        
call get_Int(idx,path, "flush/datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%flush%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%flush%datainfo%isref'
endif

! Get flush/datainfo/whatref        
call get_Int(idx,path, "flush/datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%flush%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%flush%datainfo%whatref'
endif

! Get flush/datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "flush/datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%datainfo%putinfo%putmethod'
endif

! Get flush/datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "flush/datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%datainfo%putinfo%putaccess'
endif

! Get flush/datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "flush/datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%datainfo%putinfo%putlocation'
endif

! Get flush/datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "flush/datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%datainfo%putinfo%rights'
endif

! Get flush/position/r     
call get_dimension(idx,path, "flush/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"flush/position/r",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%flush%position%r(dim1))
      cpos(itime)%flush%position%r = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%flush%position%r'
   deallocate(vect2DDouble)
endif   

! Get flush/position/z     
call get_dimension(idx,path, "flush/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"flush/position/z",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%flush%position%z(dim1))
      cpos(itime)%flush%position%z = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%flush%position%z'
   deallocate(vect2DDouble)
endif   

! Get flush/coef        
call get_dimension(idx,path, "flush/coef",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"flush/coef",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%flush%coef(dim1,dim2))
      cpos(itime)%flush%coef = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%flush%coef'
   deallocate(vect3DDouble)
endif   

! Get flush/codeparam/codename        
longstring = ' '
call get_String(idx,path, "flush/codeparam/codename",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%codeparam%codename(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%codeparam%codename(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%codeparam%codename(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%codeparam%codename'
endif

! Get flush/codeparam/codeversion        
longstring = ' '
call get_String(idx,path, "flush/codeparam/codeversion",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%codeparam%codeversion(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%codeparam%codeversion(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%codeparam%codeversion(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%codeparam%codeversion'
endif

! Get flush/codeparam/parameters        
longstring = ' '
call get_String(idx,path, "flush/codeparam/parameters",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%codeparam%parameters(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%codeparam%parameters(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%codeparam%parameters(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%codeparam%parameters'
endif

! Get flush/codeparam/output_diag        
longstring = ' '
call get_String(idx,path, "flush/codeparam/output_diag",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%flush%codeparam%output_diag(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%flush%codeparam%output_diag(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%flush%codeparam%output_diag(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%flush%codeparam%output_diag'
endif

! Get global_param/beta_pol        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/beta_pol",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%beta_pol = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%beta_pol'
endif
deallocate(vect1Ddouble)

! Get global_param/beta_tor        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/beta_tor",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%beta_tor = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%beta_tor'
endif
deallocate(vect1Ddouble)

! Get global_param/beta_normal        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/beta_normal",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%beta_normal = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%beta_normal'
endif
deallocate(vect1Ddouble)

! Get global_param/i_plasma        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/i_plasma",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%i_plasma = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%i_plasma'
endif
deallocate(vect1Ddouble)

! Get global_param/li        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/li",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%li = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%li'
endif
deallocate(vect1Ddouble)

! Get global_param/volume        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/volume",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%volume = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%volume'
endif
deallocate(vect1Ddouble)

! Get global_param/area        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/area",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%area = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%area'
endif
deallocate(vect1Ddouble)

! Get global_param/psi_ax        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/psi_ax",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%psi_ax = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%psi_ax'
endif
deallocate(vect1Ddouble)

! Get global_param/psi_bound        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/psi_bound",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%psi_bound = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%psi_bound'
endif
deallocate(vect1Ddouble)

! Get global_param/mag_axis/position/r        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/mag_axis/position/r",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%mag_axis%position%r = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%mag_axis%position%r'
endif
deallocate(vect1Ddouble)

! Get global_param/mag_axis/position/z        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/mag_axis/position/z",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%mag_axis%position%z = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%mag_axis%position%z'
endif
deallocate(vect1Ddouble)

! Get global_param/mag_axis/bphi        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/mag_axis/bphi",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%mag_axis%bphi = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%mag_axis%bphi'
endif
deallocate(vect1Ddouble)

! Get global_param/mag_axis/q        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/mag_axis/q",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%mag_axis%q = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%mag_axis%q'
endif
deallocate(vect1Ddouble)

! Get global_param/q_95        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/q_95",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%q_95 = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%q_95'
endif
deallocate(vect1Ddouble)

! Get global_param/q_min        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"global_param/q_min",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%global_param%q_min = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%global_param%q_min'
endif
deallocate(vect1Ddouble)

! Get profiles_1d/psi     
call get_dimension(idx,path, "profiles_1d/psi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/psi",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%psi(dim1))
      cpos(itime)%profiles_1d%psi = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%psi'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/phi     
call get_dimension(idx,path, "profiles_1d/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/phi",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%phi(dim1))
      cpos(itime)%profiles_1d%phi = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%phi'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/pressure     
call get_dimension(idx,path, "profiles_1d/pressure",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/pressure",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%pressure(dim1))
      cpos(itime)%profiles_1d%pressure = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%pressure'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/F_dia     
call get_dimension(idx,path, "profiles_1d/F_dia",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/F_dia",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%F_dia(dim1))
      cpos(itime)%profiles_1d%F_dia = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%F_dia'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/pprime     
call get_dimension(idx,path, "profiles_1d/pprime",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/pprime",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%pprime(dim1))
      cpos(itime)%profiles_1d%pprime = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%pprime'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/ffprime     
call get_dimension(idx,path, "profiles_1d/ffprime",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/ffprime",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%ffprime(dim1))
      cpos(itime)%profiles_1d%ffprime = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%ffprime'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/jphi     
call get_dimension(idx,path, "profiles_1d/jphi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/jphi",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%jphi(dim1))
      cpos(itime)%profiles_1d%jphi = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%jphi'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/jparallel     
call get_dimension(idx,path, "profiles_1d/jparallel",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/jparallel",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%jparallel(dim1))
      cpos(itime)%profiles_1d%jparallel = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%jparallel'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/q     
call get_dimension(idx,path, "profiles_1d/q",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/q",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%q(dim1))
      cpos(itime)%profiles_1d%q = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%q'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/r_inboard     
call get_dimension(idx,path, "profiles_1d/r_inboard",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/r_inboard",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%r_inboard(dim1))
      cpos(itime)%profiles_1d%r_inboard = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%r_inboard'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/r_outboard     
call get_dimension(idx,path, "profiles_1d/r_outboard",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/r_outboard",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%r_outboard(dim1))
      cpos(itime)%profiles_1d%r_outboard = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%r_outboard'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/rho_rtvol     
call get_dimension(idx,path, "profiles_1d/rho_rtvol",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/rho_rtvol",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%rho_rtvol(dim1))
      cpos(itime)%profiles_1d%rho_rtvol = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%rho_rtvol'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/rho_rttorfl     
call get_dimension(idx,path, "profiles_1d/rho_rttorfl",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/rho_rttorfl",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%rho_rttorfl(dim1))
      cpos(itime)%profiles_1d%rho_rttorfl = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%rho_rttorfl'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/elongation     
call get_dimension(idx,path, "profiles_1d/elongation",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/elongation",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%elongation(dim1))
      cpos(itime)%profiles_1d%elongation = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%elongation'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/tria_upper     
call get_dimension(idx,path, "profiles_1d/tria_upper",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/tria_upper",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%tria_upper(dim1))
      cpos(itime)%profiles_1d%tria_upper = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%tria_upper'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/tria_lower     
call get_dimension(idx,path, "profiles_1d/tria_lower",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/tria_lower",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%tria_lower(dim1))
      cpos(itime)%profiles_1d%tria_lower = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%tria_lower'
   deallocate(vect2DDouble)
endif   

! Get profiles_1d/volume     
call get_dimension(idx,path, "profiles_1d/volume",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_1d/volume",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_1d%volume(dim1))
      cpos(itime)%profiles_1d%volume = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_1d%volume'
   deallocate(vect2DDouble)
endif   

! Get profiles_2d/grid_type        
longstring = ' '
call get_String(idx,path, "profiles_2d/grid_type",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%profiles_2d%grid_type(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%profiles_2d%grid_type(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%profiles_2d%grid_type(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%profiles_2d%grid_type'
endif

! Get profiles_2d/grid/dim1     
call get_dimension(idx,path, "profiles_2d/grid/dim1",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_2d/grid/dim1",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_2d%grid%dim1(dim1))
      cpos(itime)%profiles_2d%grid%dim1 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_2d%grid%dim1'
   deallocate(vect2DDouble)
endif   

! Get profiles_2d/grid/dim2     
call get_dimension(idx,path, "profiles_2d/grid/dim2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"profiles_2d/grid/dim2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_2d%grid%dim2(dim1))
      cpos(itime)%profiles_2d%grid%dim2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_2d%grid%dim2'
   deallocate(vect2DDouble)
endif   

! Get profiles_2d/grid/connect
call get_dimension(idx,path, "profiles_2d/grid/connect",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3DInt(dim1,dim2,dim3)) 
   call get_vect3D_Int(idx,path,"profiles_2d/grid/connect",vect3DInt,dim1,dim2,dim3, &
   dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_2d%grid%connect(dim1,dim2))
      cpos(itime)%profiles_2d%grid%connect = vect3DInt(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_2d%grid%connect'
   deallocate(vect3DInt)
endif   

! Get profiles_2d/psi_grid        
call get_dimension(idx,path, "profiles_2d/psi_grid",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"profiles_2d/psi_grid",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_2d%psi_grid(dim1,dim2))
      cpos(itime)%profiles_2d%psi_grid = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_2d%psi_grid'
   deallocate(vect3DDouble)
endif   

! Get profiles_2d/jphi_grid        
call get_dimension(idx,path, "profiles_2d/jphi_grid",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"profiles_2d/jphi_grid",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_2d%jphi_grid(dim1,dim2))
      cpos(itime)%profiles_2d%jphi_grid = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_2d%jphi_grid'
   deallocate(vect3DDouble)
endif   

! Get profiles_2d/jpar_grid        
call get_dimension(idx,path, "profiles_2d/jpar_grid",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"profiles_2d/jpar_grid",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_2d%jpar_grid(dim1,dim2))
      cpos(itime)%profiles_2d%jpar_grid = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_2d%jpar_grid'
   deallocate(vect3DDouble)
endif   

! Get profiles_2d/br        
call get_dimension(idx,path, "profiles_2d/br",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"profiles_2d/br",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_2d%br(dim1,dim2))
      cpos(itime)%profiles_2d%br = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_2d%br'
   deallocate(vect3DDouble)
endif   

! Get profiles_2d/bz        
call get_dimension(idx,path, "profiles_2d/bz",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"profiles_2d/bz",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_2d%bz(dim1,dim2))
      cpos(itime)%profiles_2d%bz = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_2d%bz'
   deallocate(vect3DDouble)
endif   

! Get profiles_2d/bphi        
call get_dimension(idx,path, "profiles_2d/bphi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"profiles_2d/bphi",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%profiles_2d%bphi(dim1,dim2))
      cpos(itime)%profiles_2d%bphi = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%profiles_2d%bphi'
   deallocate(vect3DDouble)
endif   

! Get coord_sys/grid_type        
longstring = ' '
call get_String(idx,path, "coord_sys/grid_type",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%coord_sys%grid_type(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%coord_sys%grid_type(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%coord_sys%grid_type(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%coord_sys%grid_type'
endif

! Get coord_sys/grid/dim1     
call get_dimension(idx,path, "coord_sys/grid/dim1",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"coord_sys/grid/dim1",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%coord_sys%grid%dim1(dim1))
      cpos(itime)%coord_sys%grid%dim1 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%coord_sys%grid%dim1'
   deallocate(vect2DDouble)
endif   

! Get coord_sys/grid/dim2     
call get_dimension(idx,path, "coord_sys/grid/dim2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"coord_sys/grid/dim2",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%coord_sys%grid%dim2(dim1))
      cpos(itime)%coord_sys%grid%dim2 = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%coord_sys%grid%dim2'
   deallocate(vect2DDouble)
endif   

! Get coord_sys/jacobian        
call get_dimension(idx,path, "coord_sys/jacobian",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"coord_sys/jacobian",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%coord_sys%jacobian(dim1,dim2))
      cpos(itime)%coord_sys%jacobian = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%coord_sys%jacobian'
   deallocate(vect3DDouble)
endif   

! Get coord_sys/g_11        
call get_dimension(idx,path, "coord_sys/g_11",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"coord_sys/g_11",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%coord_sys%g_11(dim1,dim2))
      cpos(itime)%coord_sys%g_11 = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%coord_sys%g_11'
   deallocate(vect3DDouble)
endif   

! Get coord_sys/g_12        
call get_dimension(idx,path, "coord_sys/g_12",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"coord_sys/g_12",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%coord_sys%g_12(dim1,dim2))
      cpos(itime)%coord_sys%g_12 = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%coord_sys%g_12'
   deallocate(vect3DDouble)
endif   

! Get coord_sys/g_22        
call get_dimension(idx,path, "coord_sys/g_22",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"coord_sys/g_22",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%coord_sys%g_22(dim1,dim2))
      cpos(itime)%coord_sys%g_22 = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%coord_sys%g_22'
   deallocate(vect3DDouble)
endif   

! Get coord_sys/g_33        
call get_dimension(idx,path, "coord_sys/g_33",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"coord_sys/g_33",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%coord_sys%g_33(dim1,dim2))
      cpos(itime)%coord_sys%g_33 = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%coord_sys%g_33'
   deallocate(vect3DDouble)
endif   

! Get coord_sys/position/r        
call get_dimension(idx,path, "coord_sys/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"coord_sys/position/r",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%coord_sys%position%r(dim1,dim2))
      cpos(itime)%coord_sys%position%r = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%coord_sys%position%r'
   deallocate(vect3DDouble)
endif   

! Get coord_sys/position/z        
call get_dimension(idx,path, "coord_sys/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"coord_sys/position/z",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%coord_sys%position%z(dim1,dim2))
      cpos(itime)%coord_sys%position%z = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%coord_sys%position%z'
   deallocate(vect3DDouble)
endif   

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

! Get codeparam/codename        
longstring = ' '
call get_String(idx,path, "codeparam/codename",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%codename(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%codename(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%codename(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%codename'
endif

! Get codeparam/codeversion        
longstring = ' '
call get_String(idx,path, "codeparam/codeversion",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%codeversion(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%codeversion(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%codeversion(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%codeversion'
endif

! Get codeparam/parameters        
longstring = ' '
call get_String(idx,path, "codeparam/parameters",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%parameters(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%parameters(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%parameters(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%parameters'
endif

! Get codeparam/output_diag        
longstring = ' '
call get_String(idx,path, "codeparam/output_diag",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%output_diag(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%output_diag(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%output_diag(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%output_diag'
endif

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_ironmodel(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_ironmodel),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%dataprovider'
endif

! Get datainfo/putdate        
longstring = ' '
call get_String(idx,path, "datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putdate'
endif

! Get datainfo/source        
longstring = ' '
call get_String(idx,path, "datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%source'
endif

! Get datainfo/comment        
longstring = ' '
call get_String(idx,path, "datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%comment'
endif

! Get datainfo/isref        
call get_Int(idx,path, "datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%datainfo%isref'
endif

! Get datainfo/whatref        
call get_Int(idx,path, "datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%datainfo%whatref'
endif

! Get datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putmethod'
endif

! Get datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putaccess'
endif

! Get datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putlocation'
endif

! Get datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%rights'
endif

! Get desc_iron/name  
call get_dimension(idx,path, "desc_iron/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%desc_iron%name(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "desc_iron/name", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%desc_iron%name = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%desc_iron%name'
endif   

! Get desc_iron/id  
call get_dimension(idx,path, "desc_iron/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%desc_iron%id(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "desc_iron/id", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%desc_iron%id = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%desc_iron%id'
endif   

! Get desc_iron/permeability/B        
call get_dimension(idx,path, "desc_iron/permeability/B",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "desc_iron/permeability/B", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%desc_iron%permeability%B(dim1,dim2))
      cpos(itime)%desc_iron%permeability%B = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%desc_iron%permeability%B'
   deallocate(vect2DDouble)
endif 

! Get desc_iron/permeability/mur        
call get_dimension(idx,path, "desc_iron/permeability/mur",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "desc_iron/permeability/mur", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%desc_iron%permeability%mur(dim1,dim2))
      cpos(itime)%desc_iron%permeability%mur = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%desc_iron%permeability%mur'
   deallocate(vect2DDouble)
endif 

! Get desc_iron/geom_iron/npoints        
call get_dimension(idx,path, "desc_iron/geom_iron/npoints",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1Dint(dim1))        
   call get_vect1D_int(idx,path, "desc_iron/geom_iron/npoints",vect1Dint,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%desc_iron%geom_iron%npoints(dim1))
      cpos(itime)%desc_iron%geom_iron%npoints = vect1Dint   
   enddo
   write(*,*) 'Get cpos%desc_iron%geom_iron%npoints'
   deallocate(vect1Dint)
endif 

! Get desc_iron/geom_iron/rzcoordinate/r        
call get_dimension(idx,path, "desc_iron/geom_iron/rzcoordinate/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "desc_iron/geom_iron/rzcoordinate/r", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%desc_iron%geom_iron%rzcoordinate%r(dim1,dim2))
      cpos(itime)%desc_iron%geom_iron%rzcoordinate%r = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%desc_iron%geom_iron%rzcoordinate%r'
   deallocate(vect2DDouble)
endif 

! Get desc_iron/geom_iron/rzcoordinate/z        
call get_dimension(idx,path, "desc_iron/geom_iron/rzcoordinate/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "desc_iron/geom_iron/rzcoordinate/z", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%desc_iron%geom_iron%rzcoordinate%z(dim1,dim2))
      cpos(itime)%desc_iron%geom_iron%rzcoordinate%z = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%desc_iron%geom_iron%rzcoordinate%z'
   deallocate(vect2DDouble)
endif 

! Get magnetise/mr/value     
call get_dimension(idx,path, "magnetise/mr/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"magnetise/mr/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%magnetise%mr%value(dim1))
      cpos(itime)%magnetise%mr%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%magnetise%mr%value'
   deallocate(vect2DDouble)
endif   

! Get magnetise/mr/abserror     
call get_dimension(idx,path, "magnetise/mr/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"magnetise/mr/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%magnetise%mr%abserror(dim1))
      cpos(itime)%magnetise%mr%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%magnetise%mr%abserror'
   deallocate(vect2DDouble)
endif   

! Get magnetise/mr/relerror     
call get_dimension(idx,path, "magnetise/mr/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"magnetise/mr/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%magnetise%mr%relerror(dim1))
      cpos(itime)%magnetise%mr%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%magnetise%mr%relerror'
   deallocate(vect2DDouble)
endif   

! Get magnetise/mz/value     
call get_dimension(idx,path, "magnetise/mz/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"magnetise/mz/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%magnetise%mz%value(dim1))
      cpos(itime)%magnetise%mz%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%magnetise%mz%value'
   deallocate(vect2DDouble)
endif   

! Get magnetise/mz/abserror     
call get_dimension(idx,path, "magnetise/mz/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"magnetise/mz/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%magnetise%mz%abserror(dim1))
      cpos(itime)%magnetise%mz%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%magnetise%mz%abserror'
   deallocate(vect2DDouble)
endif   

! Get magnetise/mz/relerror     
call get_dimension(idx,path, "magnetise/mz/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"magnetise/mz/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%magnetise%mz%relerror(dim1))
      cpos(itime)%magnetise%mz%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%magnetise%mz%relerror'
   deallocate(vect2DDouble)
endif   

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_interfdiag(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_lineintegraldiag),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%dataprovider'
endif

! Get datainfo/putdate        
longstring = ' '
call get_String(idx,path, "datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putdate'
endif

! Get datainfo/source        
longstring = ' '
call get_String(idx,path, "datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%source'
endif

! Get datainfo/comment        
longstring = ' '
call get_String(idx,path, "datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%comment'
endif

! Get datainfo/isref        
call get_Int(idx,path, "datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%datainfo%isref'
endif

! Get datainfo/whatref        
call get_Int(idx,path, "datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%datainfo%whatref'
endif

! Get datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putmethod'
endif

! Get datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putaccess'
endif

! Get datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putlocation'
endif

! Get datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%rights'
endif

! Get expression        
longstring = ' '
call get_String(idx,path, "expression",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%expression(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%expression(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%expression(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%expression'
endif

! Get setup_line/pivot_point/r
call get_dimension(idx,path, "setup_line/pivot_point/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/pivot_point/r", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%pivot_point%r(dim1))
      cpos(itime)%setup_line%pivot_point%r = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%pivot_point%r'
   deallocate(vect1DDouble)
endif   

! Get setup_line/pivot_point/z
call get_dimension(idx,path, "setup_line/pivot_point/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/pivot_point/z", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%pivot_point%z(dim1))
      cpos(itime)%setup_line%pivot_point%z = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%pivot_point%z'
   deallocate(vect1DDouble)
endif   

! Get setup_line/pivot_point/phi
call get_dimension(idx,path, "setup_line/pivot_point/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/pivot_point/phi", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%pivot_point%phi(dim1))
      cpos(itime)%setup_line%pivot_point%phi = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%pivot_point%phi'
   deallocate(vect1DDouble)
endif   

! Get setup_line/polchordang
call get_dimension(idx,path, "setup_line/polchordang",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/polchordang", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%polchordang(dim1))
      cpos(itime)%setup_line%polchordang = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%polchordang'
   deallocate(vect1DDouble)
endif   

! Get setup_line/torchordang
call get_dimension(idx,path, "setup_line/torchordang",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/torchordang", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%torchordang(dim1))
      cpos(itime)%setup_line%torchordang = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%torchordang'
   deallocate(vect1DDouble)
endif   

! Get setup_line/second_point/r
call get_dimension(idx,path, "setup_line/second_point/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/second_point/r", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%second_point%r(dim1))
      cpos(itime)%setup_line%second_point%r = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%second_point%r'
   deallocate(vect1DDouble)
endif   

! Get setup_line/second_point/z
call get_dimension(idx,path, "setup_line/second_point/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/second_point/z", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%second_point%z(dim1))
      cpos(itime)%setup_line%second_point%z = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%second_point%z'
   deallocate(vect1DDouble)
endif   

! Get setup_line/second_point/phi
call get_dimension(idx,path, "setup_line/second_point/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/second_point/phi", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%second_point%phi(dim1))
      cpos(itime)%setup_line%second_point%phi = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%second_point%phi'
   deallocate(vect1DDouble)
endif   

! Get setup_line/nchordpoints        
call get_Int(idx,path, "setup_line/nchordpoints",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%setup_line%nchordpoints = Int0D   
   write(*,*) 'Get cpos%setup_line%nchordpoints'
endif

! Get measure/value     
call get_dimension(idx,path, "measure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"measure/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%measure%value(dim1))
      cpos(itime)%measure%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%measure%value'
   deallocate(vect2DDouble)
endif   

! Get measure/abserror     
call get_dimension(idx,path, "measure/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"measure/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%measure%abserror(dim1))
      cpos(itime)%measure%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%measure%abserror'
   deallocate(vect2DDouble)
endif   

! Get measure/relerror     
call get_dimension(idx,path, "measure/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"measure/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%measure%relerror(dim1))
      cpos(itime)%measure%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%measure%relerror'
   deallocate(vect2DDouble)
endif   

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_limiter(idx,path,  cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)
character*(*) :: path
integer :: idx, status, lenstring, istring
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4

character(len=132)::stringans      ! Temporary way of getting short strings
character(len=100000)::longstring
character(len=132), dimension(:), pointer ::stringpointer        

integer :: int0d
real(DP) :: double0d



type(type_limiter) :: cpo       
call begin_cpo_get(idx, path,0,dum1)
      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get position/r
call get_dimension(idx,path, "position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%position%r(dim1))
   call get_vect1d_double(idx,path,"position/r", &
   cpo%position%r,dim1,dum1,status)
   write(*,*) 'Get cpo%position%r'
endif        

! Get position/z
call get_dimension(idx,path, "position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%position%z(dim1))
   call get_vect1d_double(idx,path,"position/z", &
   cpo%position%z,dim1,dum1,status)
   write(*,*) 'Get cpo%position%z'
endif        

call end_cpo_get(idx, path)      
     
return
endsubroutine

subroutine euITM_get_mhd(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_mhd),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%dataprovider'
endif

! Get datainfo/putdate        
longstring = ' '
call get_String(idx,path, "datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putdate'
endif

! Get datainfo/source        
longstring = ' '
call get_String(idx,path, "datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%source'
endif

! Get datainfo/comment        
longstring = ' '
call get_String(idx,path, "datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%comment'
endif

! Get datainfo/isref        
call get_Int(idx,path, "datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%datainfo%isref'
endif

! Get datainfo/whatref        
call get_Int(idx,path, "datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%datainfo%whatref'
endif

! Get datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putmethod'
endif

! Get datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putaccess'
endif

! Get datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putlocation'
endif

! Get datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%rights'
endif

! Get n     
call get_dimension(idx,path, "n",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"n",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%n(dim1))
      cpos(itime)%n = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%n'
   deallocate(vect2DDouble)
endif   

! Get m        
call get_dimension(idx,path, "m",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3Ddouble(dim1,dim2,dim3))  
   call get_vect3D_double(idx,path,"m",vect3Ddouble,  &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   do itime=1,lentime
      allocate(cpos(itime)%m(dim1,dim2))
      cpos(itime)%m = vect3DDouble(:,:,itime)   
   enddo
   write(*,*) 'Get cpos%m'
   deallocate(vect3DDouble)
endif   

! Get psi     
call get_dimension(idx,path, "psi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"psi",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%psi(dim1))
      cpos(itime)%psi = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%psi'
   deallocate(vect2DDouble)
endif   

! Get frequency     
call get_dimension(idx,path, "frequency",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"frequency",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%frequency(dim1))
      cpos(itime)%frequency = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%frequency'
   deallocate(vect2DDouble)
endif   

! Get growthrate     
call get_dimension(idx,path, "growthrate",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"growthrate",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%growthrate(dim1))
      cpos(itime)%growthrate = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%growthrate'
   deallocate(vect2DDouble)
endif   

! Get disp_perp        
call get_dimension(idx,path, "disp_perp",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect4Ddouble(dim1,dim2,dim3,dim4)) 
   call get_vect4D_double(idx,path,"disp_perp",vect4Ddouble,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4,status)
   do itime=1,lentime
      allocate(cpos(itime)%disp_perp(dim1,dim2,dim3))
      cpos(itime)%disp_perp = vect4DDouble(:,:,:,itime)   
   enddo
   write(*,*) 'Get cpos%disp_perp'
   deallocate(vect4DDouble)
endif   

! Get disp_par        
call get_dimension(idx,path, "disp_par",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect4Ddouble(dim1,dim2,dim3,dim4)) 
   call get_vect4D_double(idx,path,"disp_par",vect4Ddouble,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4,status)
   do itime=1,lentime
      allocate(cpos(itime)%disp_par(dim1,dim2,dim3))
      cpos(itime)%disp_par = vect4DDouble(:,:,:,itime)   
   enddo
   write(*,*) 'Get cpos%disp_par'
   deallocate(vect4DDouble)
endif   

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

! Get codeparam/codename        
longstring = ' '
call get_String(idx,path, "codeparam/codename",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%codename(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%codename(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%codename(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%codename'
endif

! Get codeparam/codeversion        
longstring = ' '
call get_String(idx,path, "codeparam/codeversion",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%codeversion(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%codeversion(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%codeversion(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%codeversion'
endif

! Get codeparam/parameters        
longstring = ' '
call get_String(idx,path, "codeparam/parameters",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%parameters(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%parameters(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%parameters(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%parameters'
endif

! Get codeparam/output_diag        
longstring = ' '
call get_String(idx,path, "codeparam/output_diag",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%codeparam%output_diag(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%codeparam%output_diag(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%codeparam%output_diag(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%codeparam%output_diag'
endif

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_magdiag(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_magdiag),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%dataprovider'
endif

! Get datainfo/putdate        
longstring = ' '
call get_String(idx,path, "datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putdate'
endif

! Get datainfo/source        
longstring = ' '
call get_String(idx,path, "datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%source'
endif

! Get datainfo/comment        
longstring = ' '
call get_String(idx,path, "datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%comment'
endif

! Get datainfo/isref        
call get_Int(idx,path, "datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%datainfo%isref'
endif

! Get datainfo/whatref        
call get_Int(idx,path, "datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%datainfo%whatref'
endif

! Get datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putmethod'
endif

! Get datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putaccess'
endif

! Get datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putlocation'
endif

! Get datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%rights'
endif

! Get ip/value        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"ip/value",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%ip%value = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%ip%value'
endif
deallocate(vect1Ddouble)

! Get ip/abserror        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"ip/abserror",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%ip%abserror = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%ip%abserror'
endif
deallocate(vect1Ddouble)

! Get ip/relerror        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"ip/relerror",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%ip%relerror = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%ip%relerror'
endif
deallocate(vect1Ddouble)

! Get diamagflux/value        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"diamagflux/value",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%diamagflux%value = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%diamagflux%value'
endif
deallocate(vect1Ddouble)

! Get diamagflux/abserror        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"diamagflux/abserror",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%diamagflux%abserror = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%diamagflux%abserror'
endif
deallocate(vect1Ddouble)

! Get diamagflux/relerror        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"diamagflux/relerror",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%diamagflux%relerror = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%diamagflux%relerror'
endif
deallocate(vect1Ddouble)

! Get flux_loops/setup_floops/name  
call get_dimension(idx,path, "flux_loops/setup_floops/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%flux_loops%setup_floops%name(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "flux_loops/setup_floops/name", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%flux_loops%setup_floops%name = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%flux_loops%setup_floops%name'
endif   

! Get flux_loops/setup_floops/id  
call get_dimension(idx,path, "flux_loops/setup_floops/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%flux_loops%setup_floops%id(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "flux_loops/setup_floops/id", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%flux_loops%setup_floops%id = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%flux_loops%setup_floops%id'
endif   

! Get flux_loops/setup_floops/position/r        
call get_dimension(idx,path, "flux_loops/setup_floops/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "flux_loops/setup_floops/position/r", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%flux_loops%setup_floops%position%r(dim1,dim2))
      cpos(itime)%flux_loops%setup_floops%position%r = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%flux_loops%setup_floops%position%r'
   deallocate(vect2DDouble)
endif 

! Get flux_loops/setup_floops/position/z        
call get_dimension(idx,path, "flux_loops/setup_floops/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "flux_loops/setup_floops/position/z", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%flux_loops%setup_floops%position%z(dim1,dim2))
      cpos(itime)%flux_loops%setup_floops%position%z = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%flux_loops%setup_floops%position%z'
   deallocate(vect2DDouble)
endif 

! Get flux_loops/setup_floops/position/phi        
call get_dimension(idx,path, "flux_loops/setup_floops/position/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "flux_loops/setup_floops/position/phi", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%flux_loops%setup_floops%position%phi(dim1,dim2))
      cpos(itime)%flux_loops%setup_floops%position%phi = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%flux_loops%setup_floops%position%phi'
   deallocate(vect2DDouble)
endif 

! Get flux_loops/setup_floops/npoints        
call get_dimension(idx,path, "flux_loops/setup_floops/npoints",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1Dint(dim1))        
   call get_vect1D_int(idx,path, "flux_loops/setup_floops/npoints",vect1Dint,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%flux_loops%setup_floops%npoints(dim1))
      cpos(itime)%flux_loops%setup_floops%npoints = vect1Dint   
   enddo
   write(*,*) 'Get cpos%flux_loops%setup_floops%npoints'
   deallocate(vect1Dint)
endif 

! Get flux_loops/measure/value     
call get_dimension(idx,path, "flux_loops/measure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"flux_loops/measure/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%flux_loops%measure%value(dim1))
      cpos(itime)%flux_loops%measure%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%flux_loops%measure%value'
   deallocate(vect2DDouble)
endif   

! Get flux_loops/measure/abserror     
call get_dimension(idx,path, "flux_loops/measure/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"flux_loops/measure/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%flux_loops%measure%abserror(dim1))
      cpos(itime)%flux_loops%measure%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%flux_loops%measure%abserror'
   deallocate(vect2DDouble)
endif   

! Get flux_loops/measure/relerror     
call get_dimension(idx,path, "flux_loops/measure/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"flux_loops/measure/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%flux_loops%measure%relerror(dim1))
      cpos(itime)%flux_loops%measure%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%flux_loops%measure%relerror'
   deallocate(vect2DDouble)
endif   

! Get bpol_probes/setup_bprobe/name  
call get_dimension(idx,path, "bpol_probes/setup_bprobe/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%setup_bprobe%name(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "bpol_probes/setup_bprobe/name", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%bpol_probes%setup_bprobe%name = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%bpol_probes%setup_bprobe%name'
endif   

! Get bpol_probes/setup_bprobe/id  
call get_dimension(idx,path, "bpol_probes/setup_bprobe/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%setup_bprobe%id(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "bpol_probes/setup_bprobe/id", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%bpol_probes%setup_bprobe%id = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%bpol_probes%setup_bprobe%id'
endif   

! Get bpol_probes/setup_bprobe/position/r
call get_dimension(idx,path, "bpol_probes/setup_bprobe/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "bpol_probes/setup_bprobe/position/r", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%setup_bprobe%position%r(dim1))
      cpos(itime)%bpol_probes%setup_bprobe%position%r = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%bpol_probes%setup_bprobe%position%r'
   deallocate(vect1DDouble)
endif   

! Get bpol_probes/setup_bprobe/position/z
call get_dimension(idx,path, "bpol_probes/setup_bprobe/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "bpol_probes/setup_bprobe/position/z", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%setup_bprobe%position%z(dim1))
      cpos(itime)%bpol_probes%setup_bprobe%position%z = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%bpol_probes%setup_bprobe%position%z'
   deallocate(vect1DDouble)
endif   

! Get bpol_probes/setup_bprobe/polangle
call get_dimension(idx,path, "bpol_probes/setup_bprobe/polangle",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "bpol_probes/setup_bprobe/polangle", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%setup_bprobe%polangle(dim1))
      cpos(itime)%bpol_probes%setup_bprobe%polangle = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%bpol_probes%setup_bprobe%polangle'
   deallocate(vect1DDouble)
endif   

! Get bpol_probes/setup_bprobe/torangle
call get_dimension(idx,path, "bpol_probes/setup_bprobe/torangle",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "bpol_probes/setup_bprobe/torangle", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%setup_bprobe%torangle(dim1))
      cpos(itime)%bpol_probes%setup_bprobe%torangle = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%bpol_probes%setup_bprobe%torangle'
   deallocate(vect1DDouble)
endif   

! Get bpol_probes/setup_bprobe/area
call get_dimension(idx,path, "bpol_probes/setup_bprobe/area",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "bpol_probes/setup_bprobe/area", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%setup_bprobe%area(dim1))
      cpos(itime)%bpol_probes%setup_bprobe%area = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%bpol_probes%setup_bprobe%area'
   deallocate(vect1DDouble)
endif   

! Get bpol_probes/setup_bprobe/length
call get_dimension(idx,path, "bpol_probes/setup_bprobe/length",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "bpol_probes/setup_bprobe/length", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%setup_bprobe%length(dim1))
      cpos(itime)%bpol_probes%setup_bprobe%length = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%bpol_probes%setup_bprobe%length'
   deallocate(vect1DDouble)
endif   

! Get bpol_probes/setup_bprobe/turns        
call get_dimension(idx,path, "bpol_probes/setup_bprobe/turns",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1Dint(dim1))        
   call get_vect1D_int(idx,path, "bpol_probes/setup_bprobe/turns",vect1Dint,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%setup_bprobe%turns(dim1))
      cpos(itime)%bpol_probes%setup_bprobe%turns = vect1Dint   
   enddo
   write(*,*) 'Get cpos%bpol_probes%setup_bprobe%turns'
   deallocate(vect1Dint)
endif 

! Get bpol_probes/measure/value     
call get_dimension(idx,path, "bpol_probes/measure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"bpol_probes/measure/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%measure%value(dim1))
      cpos(itime)%bpol_probes%measure%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%bpol_probes%measure%value'
   deallocate(vect2DDouble)
endif   

! Get bpol_probes/measure/abserror     
call get_dimension(idx,path, "bpol_probes/measure/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"bpol_probes/measure/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%measure%abserror(dim1))
      cpos(itime)%bpol_probes%measure%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%bpol_probes%measure%abserror'
   deallocate(vect2DDouble)
endif   

! Get bpol_probes/measure/relerror     
call get_dimension(idx,path, "bpol_probes/measure/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"bpol_probes/measure/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%bpol_probes%measure%relerror(dim1))
      cpos(itime)%bpol_probes%measure%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%bpol_probes%measure%relerror'
   deallocate(vect2DDouble)
endif   

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_msediag(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_msediag),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%dataprovider'
endif

! Get datainfo/putdate        
longstring = ' '
call get_String(idx,path, "datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putdate'
endif

! Get datainfo/source        
longstring = ' '
call get_String(idx,path, "datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%source'
endif

! Get datainfo/comment        
longstring = ' '
call get_String(idx,path, "datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%comment'
endif

! Get datainfo/isref        
call get_Int(idx,path, "datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%datainfo%isref'
endif

! Get datainfo/whatref        
call get_Int(idx,path, "datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%datainfo%whatref'
endif

! Get datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putmethod'
endif

! Get datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putaccess'
endif

! Get datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putlocation'
endif

! Get datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%rights'
endif

! Get setup_mse/rzgamma/r
call get_dimension(idx,path, "setup_mse/rzgamma/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_mse/rzgamma/r", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_mse%rzgamma%r(dim1))
      cpos(itime)%setup_mse%rzgamma%r = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_mse%rzgamma%r'
   deallocate(vect1DDouble)
endif   

! Get setup_mse/rzgamma/z
call get_dimension(idx,path, "setup_mse/rzgamma/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_mse/rzgamma/z", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_mse%rzgamma%z(dim1))
      cpos(itime)%setup_mse%rzgamma%z = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_mse%rzgamma%z'
   deallocate(vect1DDouble)
endif   

! Get setup_mse/geom_coef        
call get_dimension(idx,path, "setup_mse/geom_coef",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "setup_mse/geom_coef", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_mse%geom_coef(dim1,dim2))
      cpos(itime)%setup_mse%geom_coef = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%setup_mse%geom_coef'
   deallocate(vect2DDouble)
endif 

! Get measure/value     
call get_dimension(idx,path, "measure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"measure/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%measure%value(dim1))
      cpos(itime)%measure%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%measure%value'
   deallocate(vect2DDouble)
endif   

! Get measure/abserror     
call get_dimension(idx,path, "measure/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"measure/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%measure%abserror(dim1))
      cpos(itime)%measure%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%measure%abserror'
   deallocate(vect2DDouble)
endif   

! Get measure/relerror     
call get_dimension(idx,path, "measure/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"measure/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%measure%relerror(dim1))
      cpos(itime)%measure%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%measure%relerror'
   deallocate(vect2DDouble)
endif   

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_pfsystems(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_pfsystems),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%dataprovider'
endif

! Get datainfo/putdate        
longstring = ' '
call get_String(idx,path, "datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putdate'
endif

! Get datainfo/source        
longstring = ' '
call get_String(idx,path, "datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%source'
endif

! Get datainfo/comment        
longstring = ' '
call get_String(idx,path, "datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%comment'
endif

! Get datainfo/isref        
call get_Int(idx,path, "datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%datainfo%isref'
endif

! Get datainfo/whatref        
call get_Int(idx,path, "datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%datainfo%whatref'
endif

! Get datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putmethod'
endif

! Get datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putaccess'
endif

! Get datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putlocation'
endif

! Get datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%rights'
endif

! Get pfcoils/desc_pfcoils/name  
call get_dimension(idx,path, "pfcoils/desc_pfcoils/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%name(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "pfcoils/desc_pfcoils/name", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%pfcoils%desc_pfcoils%name = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%name'
endif   

! Get pfcoils/desc_pfcoils/id  
call get_dimension(idx,path, "pfcoils/desc_pfcoils/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%id(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "pfcoils/desc_pfcoils/id", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%pfcoils%desc_pfcoils%id = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%id'
endif   

! Get pfcoils/desc_pfcoils/res
call get_dimension(idx,path, "pfcoils/desc_pfcoils/res",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfcoils/desc_pfcoils/res", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%res(dim1))
      cpos(itime)%pfcoils%desc_pfcoils%res = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%res'
   deallocate(vect1DDouble)
endif   

! Get pfcoils/desc_pfcoils/emax
call get_dimension(idx,path, "pfcoils/desc_pfcoils/emax",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfcoils/desc_pfcoils/emax", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%emax(dim1))
      cpos(itime)%pfcoils%desc_pfcoils%emax = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%emax'
   deallocate(vect1DDouble)
endif   

! Get pfcoils/desc_pfcoils/nelement        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/nelement",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1Dint(dim1))        
   call get_vect1D_int(idx,path, "pfcoils/desc_pfcoils/nelement",vect1Dint,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%nelement(dim1))
      cpos(itime)%pfcoils%desc_pfcoils%nelement = vect1Dint   
   enddo
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%nelement'
   deallocate(vect1Dint)
endif 

! Get pfcoils/desc_pfcoils/pfelement/name  
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%pfelement%name(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "pfcoils/desc_pfcoils/pfelement/name", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%pfcoils%desc_pfcoils%pfelement%name = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%pfelement%name'
endif   

! Get pfcoils/desc_pfcoils/pfelement/id  
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%pfelement%id(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "pfcoils/desc_pfcoils/pfelement/id", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%pfcoils%desc_pfcoils%pfelement%id = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%pfelement%id'
endif   

! Get pfcoils/desc_pfcoils/pfelement/turnsign        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/turnsign",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "pfcoils/desc_pfcoils/pfelement/turnsign", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%pfelement%turnsign(dim1,dim2))
      cpos(itime)%pfcoils%desc_pfcoils%pfelement%turnsign = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%pfelement%turnsign'
   deallocate(vect2DDouble)
endif 

! Get pfcoils/desc_pfcoils/pfelement/area        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/area",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "pfcoils/desc_pfcoils/pfelement/area", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%pfelement%area(dim1,dim2))
      cpos(itime)%pfcoils%desc_pfcoils%pfelement%area = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%pfelement%area'
   deallocate(vect2DDouble)
endif 

! Get pfcoils/desc_pfcoils/pfelement/pfgeometry/type        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/type",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DInt(dim1,dim2))        
   call get_vect2D_Int(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/type", &
   vect2DInt,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%pfelement%pfgeometry%type(dim1,dim2))
      cpos(itime)%pfcoils%desc_pfcoils%pfelement%pfgeometry%type = vect2DInt   
   enddo
   deallocate(vect2DInt)
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%pfelement%pfgeometry%type'
endif 

! Get pfcoils/desc_pfcoils/pfelement/pfgeometry/npoints        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/npoints",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DInt(dim1,dim2))        
   call get_vect2D_Int(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/npoints", &
   vect2DInt,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints(dim1,dim2))
      cpos(itime)%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints = vect2DInt   
   enddo
   deallocate(vect2DInt)
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints'
endif 

! Get pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/r        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3DDouble(dim1,dim2,dim3))        
   call get_vect3D_Double(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/r",vect3DDouble, &
   dim1,dim2,dim3,dum1,dum2,dum3,status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r(dim1,dim2,dim3))
      cpos(itime)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r = vect3DDouble   
   enddo
   deallocate(vect3DDouble)
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r'
endif 

! Get pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/z        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3DDouble(dim1,dim2,dim3))        
   call get_vect3D_Double(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/z",vect3DDouble, &
   dim1,dim2,dim3,dum1,dum2,dum3,status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z(dim1,dim2,dim3))
      cpos(itime)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z = vect3DDouble   
   enddo
   deallocate(vect3DDouble)
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z'
endif 

! Get pfcoils/desc_pfcoils/pfelement/pfgeometry/rzdrdz        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzdrdz",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect3DDouble(dim1,dim2,dim3))        
   call get_vect3D_Double(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzdrdz",vect3DDouble, &
   dim1,dim2,dim3,dum1,dum2,dum3,status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz(dim1,dim2,dim3))
      cpos(itime)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz = vect3DDouble   
   enddo
   deallocate(vect3DDouble)
   write(*,*) 'Get cpos%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz'
endif 

! Get pfcoils/coilcurrent/value     
call get_dimension(idx,path, "pfcoils/coilcurrent/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfcoils/coilcurrent/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%coilcurrent%value(dim1))
      cpos(itime)%pfcoils%coilcurrent%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfcoils%coilcurrent%value'
   deallocate(vect2DDouble)
endif   

! Get pfcoils/coilcurrent/abserror     
call get_dimension(idx,path, "pfcoils/coilcurrent/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfcoils/coilcurrent/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%coilcurrent%abserror(dim1))
      cpos(itime)%pfcoils%coilcurrent%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfcoils%coilcurrent%abserror'
   deallocate(vect2DDouble)
endif   

! Get pfcoils/coilcurrent/relerror     
call get_dimension(idx,path, "pfcoils/coilcurrent/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfcoils/coilcurrent/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%coilcurrent%relerror(dim1))
      cpos(itime)%pfcoils%coilcurrent%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfcoils%coilcurrent%relerror'
   deallocate(vect2DDouble)
endif   

! Get pfcoils/coilvoltage/value     
call get_dimension(idx,path, "pfcoils/coilvoltage/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfcoils/coilvoltage/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%coilvoltage%value(dim1))
      cpos(itime)%pfcoils%coilvoltage%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfcoils%coilvoltage%value'
   deallocate(vect2DDouble)
endif   

! Get pfcoils/coilvoltage/abserror     
call get_dimension(idx,path, "pfcoils/coilvoltage/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfcoils/coilvoltage/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%coilvoltage%abserror(dim1))
      cpos(itime)%pfcoils%coilvoltage%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfcoils%coilvoltage%abserror'
   deallocate(vect2DDouble)
endif   

! Get pfcoils/coilvoltage/relerror     
call get_dimension(idx,path, "pfcoils/coilvoltage/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfcoils/coilvoltage/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfcoils%coilvoltage%relerror(dim1))
      cpos(itime)%pfcoils%coilvoltage%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfcoils%coilvoltage%relerror'
   deallocate(vect2DDouble)
endif   

! Get pfpassive/area
call get_dimension(idx,path, "pfpassive/area",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfpassive/area", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfpassive%area(dim1))
      cpos(itime)%pfpassive%area = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfpassive%area'
   deallocate(vect1DDouble)
endif   

! Get pfpassive/res
call get_dimension(idx,path, "pfpassive/res",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfpassive/res", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfpassive%res(dim1))
      cpos(itime)%pfpassive%res = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfpassive%res'
   deallocate(vect1DDouble)
endif   

! Get pfpassive/pfpageometry/type        
call get_dimension(idx,path, "pfpassive/pfpageometry/type",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1Dint(dim1))        
   call get_vect1D_int(idx,path, "pfpassive/pfpageometry/type",vect1Dint,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfpassive%pfpageometry%type(dim1))
      cpos(itime)%pfpassive%pfpageometry%type = vect1Dint   
   enddo
   write(*,*) 'Get cpos%pfpassive%pfpageometry%type'
   deallocate(vect1Dint)
endif 

! Get pfpassive/pfpageometry/npoints        
call get_dimension(idx,path, "pfpassive/pfpageometry/npoints",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1Dint(dim1))        
   call get_vect1D_int(idx,path, "pfpassive/pfpageometry/npoints",vect1Dint,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfpassive%pfpageometry%npoints(dim1))
      cpos(itime)%pfpassive%pfpageometry%npoints = vect1Dint   
   enddo
   write(*,*) 'Get cpos%pfpassive%pfpageometry%npoints'
   deallocate(vect1Dint)
endif 

! Get pfpassive/pfpageometry/rzcoordinate/r        
call get_dimension(idx,path, "pfpassive/pfpageometry/rzcoordinate/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "pfpassive/pfpageometry/rzcoordinate/r", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfpassive%pfpageometry%rzcoordinate%r(dim1,dim2))
      cpos(itime)%pfpassive%pfpageometry%rzcoordinate%r = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%pfpassive%pfpageometry%rzcoordinate%r'
   deallocate(vect2DDouble)
endif 

! Get pfpassive/pfpageometry/rzcoordinate/z        
call get_dimension(idx,path, "pfpassive/pfpageometry/rzcoordinate/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "pfpassive/pfpageometry/rzcoordinate/z", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfpassive%pfpageometry%rzcoordinate%z(dim1,dim2))
      cpos(itime)%pfpassive%pfpageometry%rzcoordinate%z = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%pfpassive%pfpageometry%rzcoordinate%z'
   deallocate(vect2DDouble)
endif 

! Get pfpassive/pfpageometry/rzdrdz        
call get_dimension(idx,path, "pfpassive/pfpageometry/rzdrdz",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "pfpassive/pfpageometry/rzdrdz", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfpassive%pfpageometry%rzdrdz(dim1,dim2))
      cpos(itime)%pfpassive%pfpageometry%rzdrdz = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%pfpassive%pfpageometry%rzdrdz'
   deallocate(vect2DDouble)
endif 

! Get pfcircuits/name  
call get_dimension(idx,path, "pfcircuits/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%pfcircuits%name(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "pfcircuits/name", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%pfcircuits%name = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%pfcircuits%name'
endif   

! Get pfcircuits/id  
call get_dimension(idx,path, "pfcircuits/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%pfcircuits%id(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "pfcircuits/id", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%pfcircuits%id = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%pfcircuits%id'
endif   

! Get pfcircuits/type  
call get_dimension(idx,path, "pfcircuits/type",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%pfcircuits%type(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "pfcircuits/type", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%pfcircuits%type = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%pfcircuits%type'
endif   

! Get pfcircuits/nnodes        
call get_dimension(idx,path, "pfcircuits/nnodes",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1Dint(dim1))        
   call get_vect1D_int(idx,path, "pfcircuits/nnodes",vect1Dint,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfcircuits%nnodes(dim1))
      cpos(itime)%pfcircuits%nnodes = vect1Dint   
   enddo
   write(*,*) 'Get cpos%pfcircuits%nnodes'
   deallocate(vect1Dint)
endif 

 ! Get pfcircuits/connections : PROBLEM : UNIDENTIFIED TYPE !!! 
! Get pfsupplies/desc_supply/name  
call get_dimension(idx,path, "pfsupplies/desc_supply/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%name(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "pfsupplies/desc_supply/name", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%pfsupplies%desc_supply%name = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%pfsupplies%desc_supply%name'
endif   

! Get pfsupplies/desc_supply/id  
call get_dimension(idx,path, "pfsupplies/desc_supply/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%id(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "pfsupplies/desc_supply/id", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%pfsupplies%desc_supply%id = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%pfsupplies%desc_supply%id'
endif   

! Get pfsupplies/desc_supply/type  
call get_dimension(idx,path, "pfsupplies/desc_supply/type",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%type(dim1))  
   enddo
   allocate(stringpointer(dim1))
   call get_Vect1d_string(idx,path, "pfsupplies/desc_supply/type", &
                        stringpointer,dim1,dum1,status)
   do itime=1,lentime                     
      cpos(itime)%pfsupplies%desc_supply%type = stringpointer                                        
   enddo
   deallocate(stringpointer)   
   write(*,*) 'Get cpos%pfsupplies%desc_supply%type'
endif   

! Get pfsupplies/desc_supply/delay
call get_dimension(idx,path, "pfsupplies/desc_supply/delay",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfsupplies/desc_supply/delay", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%delay(dim1))
      cpos(itime)%pfsupplies%desc_supply%delay = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfsupplies%desc_supply%delay'
   deallocate(vect1DDouble)
endif   

! Get pfsupplies/desc_supply/filter/num        
call get_dimension(idx,path, "pfsupplies/desc_supply/filter/num",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "pfsupplies/desc_supply/filter/num", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%filter%num(dim1,dim2))
      cpos(itime)%pfsupplies%desc_supply%filter%num = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%pfsupplies%desc_supply%filter%num'
   deallocate(vect2DDouble)
endif 

! Get pfsupplies/desc_supply/filter/den        
call get_dimension(idx,path, "pfsupplies/desc_supply/filter/den",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect2DDouble(dim1,dim2))        
   call get_vect2D_Double(idx,path, "pfsupplies/desc_supply/filter/den", &
   vect2DDouble,dim1,dim2,dum1,dum2, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%filter%den(dim1,dim2))
      cpos(itime)%pfsupplies%desc_supply%filter%den = vect2DDouble   
   enddo
   write(*,*) 'Get cpos%pfsupplies%desc_supply%filter%den'
   deallocate(vect2DDouble)
endif 

! Get pfsupplies/desc_supply/imin
call get_dimension(idx,path, "pfsupplies/desc_supply/imin",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfsupplies/desc_supply/imin", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%imin(dim1))
      cpos(itime)%pfsupplies%desc_supply%imin = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfsupplies%desc_supply%imin'
   deallocate(vect1DDouble)
endif   

! Get pfsupplies/desc_supply/imax
call get_dimension(idx,path, "pfsupplies/desc_supply/imax",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfsupplies/desc_supply/imax", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%imax(dim1))
      cpos(itime)%pfsupplies%desc_supply%imax = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfsupplies%desc_supply%imax'
   deallocate(vect1DDouble)
endif   

! Get pfsupplies/desc_supply/res
call get_dimension(idx,path, "pfsupplies/desc_supply/res",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfsupplies/desc_supply/res", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%res(dim1))
      cpos(itime)%pfsupplies%desc_supply%res = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfsupplies%desc_supply%res'
   deallocate(vect1DDouble)
endif   

! Get pfsupplies/desc_supply/umin
call get_dimension(idx,path, "pfsupplies/desc_supply/umin",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfsupplies/desc_supply/umin", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%umin(dim1))
      cpos(itime)%pfsupplies%desc_supply%umin = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfsupplies%desc_supply%umin'
   deallocate(vect1DDouble)
endif   

! Get pfsupplies/desc_supply/umax
call get_dimension(idx,path, "pfsupplies/desc_supply/umax",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfsupplies/desc_supply/umax", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%umax(dim1))
      cpos(itime)%pfsupplies%desc_supply%umax = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfsupplies%desc_supply%umax'
   deallocate(vect1DDouble)
endif   

! Get pfsupplies/desc_supply/emax
call get_dimension(idx,path, "pfsupplies/desc_supply/emax",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "pfsupplies/desc_supply/emax", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%desc_supply%emax(dim1))
      cpos(itime)%pfsupplies%desc_supply%emax = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%pfsupplies%desc_supply%emax'
   deallocate(vect1DDouble)
endif   

! Get pfsupplies/voltage/value     
call get_dimension(idx,path, "pfsupplies/voltage/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfsupplies/voltage/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%voltage%value(dim1))
      cpos(itime)%pfsupplies%voltage%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfsupplies%voltage%value'
   deallocate(vect2DDouble)
endif   

! Get pfsupplies/voltage/abserror     
call get_dimension(idx,path, "pfsupplies/voltage/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfsupplies/voltage/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%voltage%abserror(dim1))
      cpos(itime)%pfsupplies%voltage%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfsupplies%voltage%abserror'
   deallocate(vect2DDouble)
endif   

! Get pfsupplies/voltage/relerror     
call get_dimension(idx,path, "pfsupplies/voltage/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfsupplies/voltage/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%voltage%relerror(dim1))
      cpos(itime)%pfsupplies%voltage%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfsupplies%voltage%relerror'
   deallocate(vect2DDouble)
endif   

! Get pfsupplies/current/value     
call get_dimension(idx,path, "pfsupplies/current/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfsupplies/current/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%current%value(dim1))
      cpos(itime)%pfsupplies%current%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfsupplies%current%value'
   deallocate(vect2DDouble)
endif   

! Get pfsupplies/current/abserror     
call get_dimension(idx,path, "pfsupplies/current/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfsupplies/current/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%current%abserror(dim1))
      cpos(itime)%pfsupplies%current%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfsupplies%current%abserror'
   deallocate(vect2DDouble)
endif   

! Get pfsupplies/current/relerror     
call get_dimension(idx,path, "pfsupplies/current/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"pfsupplies/current/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%pfsupplies%current%relerror(dim1))
      cpos(itime)%pfsupplies%current%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%pfsupplies%current%relerror'
   deallocate(vect2DDouble)
endif   

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_polardiag(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_lineintegraldiag),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%dataprovider'
endif

! Get datainfo/putdate        
longstring = ' '
call get_String(idx,path, "datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putdate'
endif

! Get datainfo/source        
longstring = ' '
call get_String(idx,path, "datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%source'
endif

! Get datainfo/comment        
longstring = ' '
call get_String(idx,path, "datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%comment'
endif

! Get datainfo/isref        
call get_Int(idx,path, "datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%datainfo%isref'
endif

! Get datainfo/whatref        
call get_Int(idx,path, "datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%datainfo%whatref'
endif

! Get datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putmethod'
endif

! Get datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putaccess'
endif

! Get datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putlocation'
endif

! Get datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%rights'
endif

! Get expression        
longstring = ' '
call get_String(idx,path, "expression",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%expression(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%expression(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%expression(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%expression'
endif

! Get setup_line/pivot_point/r
call get_dimension(idx,path, "setup_line/pivot_point/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/pivot_point/r", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%pivot_point%r(dim1))
      cpos(itime)%setup_line%pivot_point%r = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%pivot_point%r'
   deallocate(vect1DDouble)
endif   

! Get setup_line/pivot_point/z
call get_dimension(idx,path, "setup_line/pivot_point/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/pivot_point/z", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%pivot_point%z(dim1))
      cpos(itime)%setup_line%pivot_point%z = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%pivot_point%z'
   deallocate(vect1DDouble)
endif   

! Get setup_line/pivot_point/phi
call get_dimension(idx,path, "setup_line/pivot_point/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/pivot_point/phi", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%pivot_point%phi(dim1))
      cpos(itime)%setup_line%pivot_point%phi = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%pivot_point%phi'
   deallocate(vect1DDouble)
endif   

! Get setup_line/polchordang
call get_dimension(idx,path, "setup_line/polchordang",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/polchordang", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%polchordang(dim1))
      cpos(itime)%setup_line%polchordang = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%polchordang'
   deallocate(vect1DDouble)
endif   

! Get setup_line/torchordang
call get_dimension(idx,path, "setup_line/torchordang",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/torchordang", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%torchordang(dim1))
      cpos(itime)%setup_line%torchordang = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%torchordang'
   deallocate(vect1DDouble)
endif   

! Get setup_line/second_point/r
call get_dimension(idx,path, "setup_line/second_point/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/second_point/r", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%second_point%r(dim1))
      cpos(itime)%setup_line%second_point%r = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%second_point%r'
   deallocate(vect1DDouble)
endif   

! Get setup_line/second_point/z
call get_dimension(idx,path, "setup_line/second_point/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/second_point/z", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%second_point%z(dim1))
      cpos(itime)%setup_line%second_point%z = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%second_point%z'
   deallocate(vect1DDouble)
endif   

! Get setup_line/second_point/phi
call get_dimension(idx,path, "setup_line/second_point/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(vect1DDouble(dim1))        
   call get_vect1D_Double(idx,path, "setup_line/second_point/phi", &
   vect1DDouble,dim1,dum1, status)           
   do itime=1,lentime
      allocate(cpos(itime)%setup_line%second_point%phi(dim1))
      cpos(itime)%setup_line%second_point%phi = vect1DDouble   
   enddo
   write(*,*) 'Get cpos%setup_line%second_point%phi'
   deallocate(vect1DDouble)
endif   

! Get setup_line/nchordpoints        
call get_Int(idx,path, "setup_line/nchordpoints",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%setup_line%nchordpoints = Int0D   
   write(*,*) 'Get cpos%setup_line%nchordpoints'
endif

! Get measure/value     
call get_dimension(idx,path, "measure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"measure/value",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%measure%value(dim1))
      cpos(itime)%measure%value = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%measure%value'
   deallocate(vect2DDouble)
endif   

! Get measure/abserror     
call get_dimension(idx,path, "measure/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"measure/abserror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%measure%abserror(dim1))
      cpos(itime)%measure%abserror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%measure%abserror'
   deallocate(vect2DDouble)
endif   

! Get measure/relerror     
call get_dimension(idx,path, "measure/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then 
   allocate(vect2Ddouble(dim1,dim2)) 
   call get_vect2d_double(idx,path,"measure/relerror",vect2Ddouble,dim1,dim2,dum1,dum2,status)
   do itime=1,lentime
      allocate(cpos(itime)%measure%relerror(dim1))
      cpos(itime)%measure%relerror = vect2DDouble(:,itime)   
   enddo
   write(*,*) 'Get cpos%measure%relerror'
   deallocate(vect2DDouble)
endif   

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_toroidfield(idx,path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, status, lentime, lenstring, istring

type(type_toroidfield),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132) :: stringans
character(len=100000)::longstring    
character(len=132), dimension(:), pointer ::stringpointer        


! get time base and allocate the array of cpo structures
call get_dimension(idx,path,"time",ndims, dim1,dim2,dim3,dim4)         
lentime = dim1
allocate(cpos(lentime))

call begin_cpo_get(idx, path,1,dum1)
      
! Get datainfo/dataprovider        
longstring = ' '
call get_String(idx,path, "datainfo/dataprovider",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%dataprovider(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%dataprovider'
endif

! Get datainfo/putdate        
longstring = ' '
call get_String(idx,path, "datainfo/putdate",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putdate(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putdate(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putdate'
endif

! Get datainfo/source        
longstring = ' '
call get_String(idx,path, "datainfo/source",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%source(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%source(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%source'
endif

! Get datainfo/comment        
longstring = ' '
call get_String(idx,path, "datainfo/comment",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%comment(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%comment(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%comment'
endif

! Get datainfo/isref        
call get_Int(idx,path, "datainfo/isref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%isref = Int0D   
   write(*,*) 'Get cpos%datainfo%isref'
endif

! Get datainfo/whatref        
call get_Int(idx,path, "datainfo/whatref",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%datainfo%whatref = Int0D   
   write(*,*) 'Get cpos%datainfo%whatref'
endif

! Get datainfo/putinfo/putmethod        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putmethod",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putmethod(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putmethod'
endif

! Get datainfo/putinfo/putaccess        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putaccess",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putaccess(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putaccess'
endif

! Get datainfo/putinfo/putlocation        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/putlocation",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%putlocation(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%putlocation'
endif

! Get datainfo/putinfo/rights        
longstring = ' '
call get_String(idx,path, "datainfo/putinfo/rights",longstring, status)           
if (status.EQ.0) then
   do itime=1,lentime
      lenstring = len_trim(longstring)      
      allocate(cpos(itime)%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
      if (lenstring <= 132) then             
         cpos(itime)%datainfo%putinfo%rights(1) = trim(longstring)
      else
         do istring=1,floor(real(lenstring/132))+1
             cpos(itime)%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132))  ! Syntax to verify !!!
         enddo
      endif
   enddo
   write(*,*) 'Get cpos%datainfo%putinfo%rights'
endif

! Get nturns        
call get_Int(idx,path, "nturns",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%nturns = Int0D   
   write(*,*) 'Get cpos%nturns'
endif

! Get ncoils        
call get_Int(idx,path, "ncoils",Int0D, status)           
if (status.EQ.0) then
   cpos(1:lentime)%ncoils = Int0D   
   write(*,*) 'Get cpos%ncoils'
endif

! Get current/value        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"current/value",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%current%value = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%current%value'
endif
deallocate(vect1Ddouble)

! Get current/abserror        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"current/abserror",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%current%abserror = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%current%abserror'
endif
deallocate(vect1Ddouble)

! Get current/relerror        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"current/relerror",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%current%relerror = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%current%relerror'
endif
deallocate(vect1Ddouble)

! Get bvac_r/value        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"bvac_r/value",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%bvac_r%value = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%bvac_r%value'
endif
deallocate(vect1Ddouble)

! Get bvac_r/abserror        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"bvac_r/abserror",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%bvac_r%abserror = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%bvac_r%abserror'
endif
deallocate(vect1Ddouble)

! Get bvac_r/relerror        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"bvac_r/relerror",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%bvac_r%relerror = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%bvac_r%relerror'
endif
deallocate(vect1Ddouble)

! Get time        
allocate(vect1Ddouble(lentime))
call get_vect1d_double(idx,path,"time",vect1Ddouble,lentime,dum1,status)
if (status.EQ.0) then
   cpos(1:lentime)%time = vect1Ddouble(1:lentime)
   write(*,*) 'Get cpos%time'
endif
deallocate(vect1Ddouble)

call end_cpo_get(idx, path)
      
return
endsubroutine

subroutine euITM_get_vessel(idx,path,  cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)
character*(*) :: path
integer :: idx, status, lenstring, istring
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4

character(len=132)::stringans      ! Temporary way of getting short strings
character(len=100000)::longstring
character(len=132), dimension(:), pointer ::stringpointer        

integer :: int0d
real(DP) :: double0d



type(type_vessel) :: cpo       
call begin_cpo_get(idx, path,0,dum1)
      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get position/r
call get_dimension(idx,path, "position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%position%r(dim1))
   call get_vect1d_double(idx,path,"position/r", &
   cpo%position%r,dim1,dum1,status)
   write(*,*) 'Get cpo%position%r'
endif        

! Get position/z
call get_dimension(idx,path, "position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%position%z(dim1))
   call get_vect1d_double(idx,path,"position/z", &
   cpo%position%z,dim1,dum1,status)
   write(*,*) 'Get cpo%position%z'
endif        

call end_cpo_get(idx, path)      
     
return
endsubroutine

 
!!!!!! Routines to GET one time slice of a CPO, with time interpolation

subroutine euITM_get_slice_topinfo(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_topinfo) :: cpo      
    
write(*,*) 'Warning : GET_SLICE requested for a time-independent CPO is equivalent to a simple GET'
call euITM_get_topinfo(idx,path,  cpo)
	

return
endsubroutine

subroutine euITM_get_slice_summary(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_summary) :: cpo      
    
write(*,*) 'Warning : GET_SLICE requested for a time-independent CPO is equivalent to a simple GET'
call euITM_get_summary(idx,path,  cpo)
	

return
endsubroutine

subroutine euITM_get_slice_controllers(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_controllers) :: cpo      
    
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get name  
longstring = ' '
call get_string(idx,path, "name",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%name(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%name = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%name(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%name'
endif   

! Get purpose  
longstring = ' '
call get_string(idx,path, "purpose",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%purpose(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%purpose = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%purpose(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%purpose'
endif   

! Get type  
longstring = ' '
call get_string(idx,path, "type",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%type(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%type = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%type(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%type'
endif   

! Get input  
call get_dimension(idx,path, "input",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%input(dim1))
   call get_Vect1d_string(idx,path, "input", &
                        cpo%input,dim1,dum1,status)
   write(*,*) 'Get cpo%input'
endif   

! Get output  
call get_dimension(idx,path, "output",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%output(dim1))
   call get_Vect1d_string(idx,path, "output", &
                        cpo%output,dim1,dum1,status)
   write(*,*) 'Get cpo%output'
endif   

! Get statespace/observable  
call get_dimension(idx,path, "statespace/observable",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%statespace%observable(dim1))
   call get_Vect1d_string(idx,path, "statespace/observable", &
                        cpo%statespace%observable,dim1,dum1,status)
   write(*,*) 'Get cpo%statespace%observable'
endif   

! Get statespace/A        
call get_dimension(idx,path, "statespace/A",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%statespace%A(dim1,dim2))
   call get_vect2d_double(idx,path,"statespace/A", &
   cpo%statespace%A, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%statespace%A'
endif 

! Get statespace/B        
call get_dimension(idx,path, "statespace/B",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%statespace%B(dim1,dim2))
   call get_vect2d_double(idx,path,"statespace/B", &
   cpo%statespace%B, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%statespace%B'
endif 

! Get statespace/C        
call get_dimension(idx,path, "statespace/C",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%statespace%C(dim1,dim2))
   call get_vect2d_double(idx,path,"statespace/C", &
   cpo%statespace%C, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%statespace%C'
endif 

! Get statespace/D        
call get_dimension(idx,path, "statespace/D",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%statespace%D(dim1,dim2))
   call get_vect2d_double(idx,path,"statespace/D", &
   cpo%statespace%D, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%statespace%D'
endif 

! Get statespace/deltat 
call get_double(idx,path, "statespace/deltat",double0d,status)
if (status.EQ.0) then
   cpo%statespace%deltat = double0d
   write(*,*) 'Get cpo%statespace%deltat'
endif

! Get target
call get_dimension(idx,path, "target",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%target(dim1))         
   call get_vect1d_double_Slice(idx,path, "target", &
   cpo%target, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%target'
endif

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_coreprof(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_coreprof) :: cpo      
    
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get rho_tor
call get_dimension(idx,path, "rho_tor",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%rho_tor(dim1))         
   call get_vect1d_double_Slice(idx,path, "rho_tor", &
   cpo%rho_tor, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%rho_tor'
endif

! Get psi
call get_dimension(idx,path, "psi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%psi(dim1))         
   call get_vect1d_double_Slice(idx,path, "psi", &
   cpo%psi, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%psi'
endif

! Get pressure/value
call get_dimension(idx,path, "pressure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pressure%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "pressure/value", &
   cpo%pressure%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pressure%value'
endif

! Get pressure/source        
! TIME DEPENDENT STRINGS NOT TREATED YET !!!

! Get jparallel/value
call get_dimension(idx,path, "jparallel/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%jparallel%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "jparallel/value", &
   cpo%jparallel%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%jparallel%value'
endif

! Get jparallel/source        
! TIME DEPENDENT STRINGS NOT TREATED YET !!!

! Get q/value
call get_dimension(idx,path, "q/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%q%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "q/value", &
   cpo%q%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%q%value'
endif

! Get q/source        
! TIME DEPENDENT STRINGS NOT TREATED YET !!!

! Get codeparam/codename  
longstring = ' '
call get_string(idx,path, "codeparam/codename",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%codename(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%codename = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%codename(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%codename'
endif   

! Get codeparam/codeversion  
longstring = ' '
call get_string(idx,path, "codeparam/codeversion",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%codeversion(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%codeversion = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%codeversion(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%codeversion'
endif   

! Get codeparam/parameters  
longstring = ' '
call get_string(idx,path, "codeparam/parameters",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%parameters(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%parameters = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%parameters(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%parameters'
endif   

! Get codeparam/output_diag  
longstring = ' '
call get_string(idx,path, "codeparam/output_diag",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%output_diag(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%output_diag = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%output_diag(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%output_diag'
endif   

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_equilibrium(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_equilibrium) :: cpo      
    
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get eqconstraint/bvac_r/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/bvac_r/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%bvac_r%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%bvac_r%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%bvac_r%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%bvac_r%source'
endif   

! Get eqconstraint/bvac_r/time
call get_double_Slice(idx,path, "eqconstraint/bvac_r/time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqconstraint%bvac_r%time = double0d
   write(*,*) 'Get cpo%eqconstraint%bvac_r%time'
endif

! Get eqconstraint/bvac_r/exact 
call get_int(idx,path, "eqconstraint/bvac_r/exact",int0d,status)
if (status.EQ.0) then
   cpo%eqconstraint%bvac_r%exact = int0d
   write(*,*) 'Get cpo%eqconstraint%bvac_r%exact'
endif                 

! Get eqconstraint/bvac_r/weight
call get_double_Slice(idx,path, "eqconstraint/bvac_r/weight",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqconstraint%bvac_r%weight = double0d
   write(*,*) 'Get cpo%eqconstraint%bvac_r%weight'
endif

! Get eqconstraint/bvac_r/sigma
call get_double_Slice(idx,path, "eqconstraint/bvac_r/sigma",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqconstraint%bvac_r%sigma = double0d
   write(*,*) 'Get cpo%eqconstraint%bvac_r%sigma'
endif

! Get eqconstraint/bvac_r/calculated
call get_double_Slice(idx,path, "eqconstraint/bvac_r/calculated",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqconstraint%bvac_r%calculated = double0d
   write(*,*) 'Get cpo%eqconstraint%bvac_r%calculated'
endif

! Get eqconstraint/bvac_r/chi2
call get_double_Slice(idx,path, "eqconstraint/bvac_r/chi2",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqconstraint%bvac_r%chi2 = double0d
   write(*,*) 'Get cpo%eqconstraint%bvac_r%chi2'
endif

! Get eqconstraint/i_plasma/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/i_plasma/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%i_plasma%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%i_plasma%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%i_plasma%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%i_plasma%source'
endif   

! Get eqconstraint/i_plasma/time
call get_double_Slice(idx,path, "eqconstraint/i_plasma/time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqconstraint%i_plasma%time = double0d
   write(*,*) 'Get cpo%eqconstraint%i_plasma%time'
endif

! Get eqconstraint/i_plasma/exact 
call get_int(idx,path, "eqconstraint/i_plasma/exact",int0d,status)
if (status.EQ.0) then
   cpo%eqconstraint%i_plasma%exact = int0d
   write(*,*) 'Get cpo%eqconstraint%i_plasma%exact'
endif                 

! Get eqconstraint/i_plasma/weight
call get_double_Slice(idx,path, "eqconstraint/i_plasma/weight",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqconstraint%i_plasma%weight = double0d
   write(*,*) 'Get cpo%eqconstraint%i_plasma%weight'
endif

! Get eqconstraint/i_plasma/sigma
call get_double_Slice(idx,path, "eqconstraint/i_plasma/sigma",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqconstraint%i_plasma%sigma = double0d
   write(*,*) 'Get cpo%eqconstraint%i_plasma%sigma'
endif

! Get eqconstraint/i_plasma/calculated
call get_double_Slice(idx,path, "eqconstraint/i_plasma/calculated",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqconstraint%i_plasma%calculated = double0d
   write(*,*) 'Get cpo%eqconstraint%i_plasma%calculated'
endif

! Get eqconstraint/i_plasma/chi2
call get_double_Slice(idx,path, "eqconstraint/i_plasma/chi2",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqconstraint%i_plasma%chi2 = double0d
   write(*,*) 'Get cpo%eqconstraint%i_plasma%chi2'
endif

! Get eqconstraint/magnet_iron/mr/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/magnet_iron/mr/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%magnet_iron%mr%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%magnet_iron%mr%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%magnet_iron%mr%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mr%source'
endif   

! Get eqconstraint/magnet_iron/mr/time
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mr%time(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/magnet_iron/mr/time", &
   cpo%eqconstraint%magnet_iron%mr%time, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mr%time'
endif

! Get eqconstraint/magnet_iron/mr/exact
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mr%exact(dim1))         
   call get_vect1d_Int_Slice(idx,path, "eqconstraint/magnet_iron/mr/exact", &
   cpo%eqconstraint%magnet_iron%mr%exact, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mr%exact'
endif

! Get eqconstraint/magnet_iron/mr/weight
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mr%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/magnet_iron/mr/weight", &
   cpo%eqconstraint%magnet_iron%mr%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mr%weight'
endif

! Get eqconstraint/magnet_iron/mr/sigma
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mr%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/magnet_iron/mr/sigma", &
   cpo%eqconstraint%magnet_iron%mr%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mr%sigma'
endif

! Get eqconstraint/magnet_iron/mr/calculated
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mr%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/magnet_iron/mr/calculated", &
   cpo%eqconstraint%magnet_iron%mr%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mr%calculated'
endif

! Get eqconstraint/magnet_iron/mr/chi2
call get_dimension(idx,path, "eqconstraint/magnet_iron/mr/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mr%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/magnet_iron/mr/chi2", &
   cpo%eqconstraint%magnet_iron%mr%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mr%chi2'
endif

! Get eqconstraint/magnet_iron/mz/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/magnet_iron/mz/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%magnet_iron%mz%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%magnet_iron%mz%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%magnet_iron%mz%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mz%source'
endif   

! Get eqconstraint/magnet_iron/mz/time
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mz%time(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/magnet_iron/mz/time", &
   cpo%eqconstraint%magnet_iron%mz%time, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mz%time'
endif

! Get eqconstraint/magnet_iron/mz/exact
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mz%exact(dim1))         
   call get_vect1d_Int_Slice(idx,path, "eqconstraint/magnet_iron/mz/exact", &
   cpo%eqconstraint%magnet_iron%mz%exact, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mz%exact'
endif

! Get eqconstraint/magnet_iron/mz/weight
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mz%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/magnet_iron/mz/weight", &
   cpo%eqconstraint%magnet_iron%mz%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mz%weight'
endif

! Get eqconstraint/magnet_iron/mz/sigma
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mz%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/magnet_iron/mz/sigma", &
   cpo%eqconstraint%magnet_iron%mz%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mz%sigma'
endif

! Get eqconstraint/magnet_iron/mz/calculated
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mz%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/magnet_iron/mz/calculated", &
   cpo%eqconstraint%magnet_iron%mz%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mz%calculated'
endif

! Get eqconstraint/magnet_iron/mz/chi2
call get_dimension(idx,path, "eqconstraint/magnet_iron/mz/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%magnet_iron%mz%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/magnet_iron/mz/chi2", &
   cpo%eqconstraint%magnet_iron%mz%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%magnet_iron%mz%chi2'
endif

! Get eqconstraint/bpol/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/bpol/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%bpol%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%bpol%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%bpol%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%bpol%source'
endif   

! Get eqconstraint/bpol/time
call get_dimension(idx,path, "eqconstraint/bpol/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%bpol%time(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/bpol/time", &
   cpo%eqconstraint%bpol%time, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%bpol%time'
endif

! Get eqconstraint/bpol/exact
call get_dimension(idx,path, "eqconstraint/bpol/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%bpol%exact(dim1))         
   call get_vect1d_Int_Slice(idx,path, "eqconstraint/bpol/exact", &
   cpo%eqconstraint%bpol%exact, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%bpol%exact'
endif

! Get eqconstraint/bpol/weight
call get_dimension(idx,path, "eqconstraint/bpol/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%bpol%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/bpol/weight", &
   cpo%eqconstraint%bpol%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%bpol%weight'
endif

! Get eqconstraint/bpol/sigma
call get_dimension(idx,path, "eqconstraint/bpol/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%bpol%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/bpol/sigma", &
   cpo%eqconstraint%bpol%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%bpol%sigma'
endif

! Get eqconstraint/bpol/calculated
call get_dimension(idx,path, "eqconstraint/bpol/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%bpol%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/bpol/calculated", &
   cpo%eqconstraint%bpol%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%bpol%calculated'
endif

! Get eqconstraint/bpol/chi2
call get_dimension(idx,path, "eqconstraint/bpol/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%bpol%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/bpol/chi2", &
   cpo%eqconstraint%bpol%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%bpol%chi2'
endif

! Get eqconstraint/flux/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/flux/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%flux%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%flux%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%flux%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%flux%source'
endif   

! Get eqconstraint/flux/time
call get_dimension(idx,path, "eqconstraint/flux/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%flux%time(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/flux/time", &
   cpo%eqconstraint%flux%time, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%flux%time'
endif

! Get eqconstraint/flux/exact
call get_dimension(idx,path, "eqconstraint/flux/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%flux%exact(dim1))         
   call get_vect1d_Int_Slice(idx,path, "eqconstraint/flux/exact", &
   cpo%eqconstraint%flux%exact, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%flux%exact'
endif

! Get eqconstraint/flux/weight
call get_dimension(idx,path, "eqconstraint/flux/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%flux%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/flux/weight", &
   cpo%eqconstraint%flux%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%flux%weight'
endif

! Get eqconstraint/flux/sigma
call get_dimension(idx,path, "eqconstraint/flux/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%flux%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/flux/sigma", &
   cpo%eqconstraint%flux%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%flux%sigma'
endif

! Get eqconstraint/flux/calculated
call get_dimension(idx,path, "eqconstraint/flux/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%flux%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/flux/calculated", &
   cpo%eqconstraint%flux%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%flux%calculated'
endif

! Get eqconstraint/flux/chi2
call get_dimension(idx,path, "eqconstraint/flux/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%flux%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/flux/chi2", &
   cpo%eqconstraint%flux%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%flux%chi2'
endif

! Get eqconstraint/mse/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/mse/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%mse%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%mse%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%mse%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%mse%source'
endif   

! Get eqconstraint/mse/time
call get_dimension(idx,path, "eqconstraint/mse/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%mse%time(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/mse/time", &
   cpo%eqconstraint%mse%time, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%mse%time'
endif

! Get eqconstraint/mse/exact
call get_dimension(idx,path, "eqconstraint/mse/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%mse%exact(dim1))         
   call get_vect1d_Int_Slice(idx,path, "eqconstraint/mse/exact", &
   cpo%eqconstraint%mse%exact, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%mse%exact'
endif

! Get eqconstraint/mse/weight
call get_dimension(idx,path, "eqconstraint/mse/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%mse%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/mse/weight", &
   cpo%eqconstraint%mse%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%mse%weight'
endif

! Get eqconstraint/mse/sigma
call get_dimension(idx,path, "eqconstraint/mse/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%mse%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/mse/sigma", &
   cpo%eqconstraint%mse%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%mse%sigma'
endif

! Get eqconstraint/mse/calculated
call get_dimension(idx,path, "eqconstraint/mse/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%mse%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/mse/calculated", &
   cpo%eqconstraint%mse%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%mse%calculated'
endif

! Get eqconstraint/mse/chi2
call get_dimension(idx,path, "eqconstraint/mse/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%mse%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/mse/chi2", &
   cpo%eqconstraint%mse%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%mse%chi2'
endif

! Get eqconstraint/faraday/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/faraday/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%faraday%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%faraday%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%faraday%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%faraday%source'
endif   

! Get eqconstraint/faraday/time
call get_dimension(idx,path, "eqconstraint/faraday/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%faraday%time(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/faraday/time", &
   cpo%eqconstraint%faraday%time, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%faraday%time'
endif

! Get eqconstraint/faraday/exact
call get_dimension(idx,path, "eqconstraint/faraday/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%faraday%exact(dim1))         
   call get_vect1d_Int_Slice(idx,path, "eqconstraint/faraday/exact", &
   cpo%eqconstraint%faraday%exact, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%faraday%exact'
endif

! Get eqconstraint/faraday/weight
call get_dimension(idx,path, "eqconstraint/faraday/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%faraday%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/faraday/weight", &
   cpo%eqconstraint%faraday%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%faraday%weight'
endif

! Get eqconstraint/faraday/sigma
call get_dimension(idx,path, "eqconstraint/faraday/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%faraday%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/faraday/sigma", &
   cpo%eqconstraint%faraday%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%faraday%sigma'
endif

! Get eqconstraint/faraday/calculated
call get_dimension(idx,path, "eqconstraint/faraday/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%faraday%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/faraday/calculated", &
   cpo%eqconstraint%faraday%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%faraday%calculated'
endif

! Get eqconstraint/faraday/chi2
call get_dimension(idx,path, "eqconstraint/faraday/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%faraday%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/faraday/chi2", &
   cpo%eqconstraint%faraday%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%faraday%chi2'
endif

! Get eqconstraint/pfcurrent/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/pfcurrent/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%pfcurrent%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%pfcurrent%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%pfcurrent%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%pfcurrent%source'
endif   

! Get eqconstraint/pfcurrent/time
call get_dimension(idx,path, "eqconstraint/pfcurrent/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pfcurrent%time(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/pfcurrent/time", &
   cpo%eqconstraint%pfcurrent%time, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pfcurrent%time'
endif

! Get eqconstraint/pfcurrent/exact
call get_dimension(idx,path, "eqconstraint/pfcurrent/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pfcurrent%exact(dim1))         
   call get_vect1d_Int_Slice(idx,path, "eqconstraint/pfcurrent/exact", &
   cpo%eqconstraint%pfcurrent%exact, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pfcurrent%exact'
endif

! Get eqconstraint/pfcurrent/weight
call get_dimension(idx,path, "eqconstraint/pfcurrent/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pfcurrent%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/pfcurrent/weight", &
   cpo%eqconstraint%pfcurrent%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pfcurrent%weight'
endif

! Get eqconstraint/pfcurrent/sigma
call get_dimension(idx,path, "eqconstraint/pfcurrent/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pfcurrent%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/pfcurrent/sigma", &
   cpo%eqconstraint%pfcurrent%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pfcurrent%sigma'
endif

! Get eqconstraint/pfcurrent/calculated
call get_dimension(idx,path, "eqconstraint/pfcurrent/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pfcurrent%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/pfcurrent/calculated", &
   cpo%eqconstraint%pfcurrent%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pfcurrent%calculated'
endif

! Get eqconstraint/pfcurrent/chi2
call get_dimension(idx,path, "eqconstraint/pfcurrent/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pfcurrent%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/pfcurrent/chi2", &
   cpo%eqconstraint%pfcurrent%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pfcurrent%chi2'
endif

! Get eqconstraint/pressure/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/pressure/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%pressure%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%pressure%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%pressure%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%pressure%source'
endif   

! Get eqconstraint/pressure/time
call get_dimension(idx,path, "eqconstraint/pressure/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pressure%time(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/pressure/time", &
   cpo%eqconstraint%pressure%time, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pressure%time'
endif

! Get eqconstraint/pressure/exact
call get_dimension(idx,path, "eqconstraint/pressure/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pressure%exact(dim1))         
   call get_vect1d_Int_Slice(idx,path, "eqconstraint/pressure/exact", &
   cpo%eqconstraint%pressure%exact, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pressure%exact'
endif

! Get eqconstraint/pressure/weight
call get_dimension(idx,path, "eqconstraint/pressure/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pressure%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/pressure/weight", &
   cpo%eqconstraint%pressure%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pressure%weight'
endif

! Get eqconstraint/pressure/sigma
call get_dimension(idx,path, "eqconstraint/pressure/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pressure%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/pressure/sigma", &
   cpo%eqconstraint%pressure%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pressure%sigma'
endif

! Get eqconstraint/pressure/calculated
call get_dimension(idx,path, "eqconstraint/pressure/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pressure%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/pressure/calculated", &
   cpo%eqconstraint%pressure%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pressure%calculated'
endif

! Get eqconstraint/pressure/chi2
call get_dimension(idx,path, "eqconstraint/pressure/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%pressure%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/pressure/chi2", &
   cpo%eqconstraint%pressure%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%pressure%chi2'
endif

! Get eqconstraint/jsurf/source  
longstring = ' '
call get_string(idx,path, "eqconstraint/jsurf/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqconstraint%jsurf%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqconstraint%jsurf%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqconstraint%jsurf%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqconstraint%jsurf%source'
endif   

! Get eqconstraint/jsurf/time
call get_dimension(idx,path, "eqconstraint/jsurf/time",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%jsurf%time(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/jsurf/time", &
   cpo%eqconstraint%jsurf%time, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%jsurf%time'
endif

! Get eqconstraint/jsurf/exact
call get_dimension(idx,path, "eqconstraint/jsurf/exact",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%jsurf%exact(dim1))         
   call get_vect1d_Int_Slice(idx,path, "eqconstraint/jsurf/exact", &
   cpo%eqconstraint%jsurf%exact, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%jsurf%exact'
endif

! Get eqconstraint/jsurf/weight
call get_dimension(idx,path, "eqconstraint/jsurf/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%jsurf%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/jsurf/weight", &
   cpo%eqconstraint%jsurf%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%jsurf%weight'
endif

! Get eqconstraint/jsurf/sigma
call get_dimension(idx,path, "eqconstraint/jsurf/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%jsurf%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/jsurf/sigma", &
   cpo%eqconstraint%jsurf%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%jsurf%sigma'
endif

! Get eqconstraint/jsurf/calculated
call get_dimension(idx,path, "eqconstraint/jsurf/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%jsurf%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/jsurf/calculated", &
   cpo%eqconstraint%jsurf%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%jsurf%calculated'
endif

! Get eqconstraint/jsurf/chi2
call get_dimension(idx,path, "eqconstraint/jsurf/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%jsurf%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/jsurf/chi2", &
   cpo%eqconstraint%jsurf%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%jsurf%chi2'
endif

! Get eqconstraint/q/qvalue
call get_dimension(idx,path, "eqconstraint/q/qvalue",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%q%qvalue(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/q/qvalue", &
   cpo%eqconstraint%q%qvalue, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%q%qvalue'
endif

! Get eqconstraint/q/position/r
call get_dimension(idx,path, "eqconstraint/q/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%q%position%r(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/q/position/r", &
   cpo%eqconstraint%q%position%r, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%q%position%r'
endif

! Get eqconstraint/q/position/z
call get_dimension(idx,path, "eqconstraint/q/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%q%position%z(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/q/position/z", &
   cpo%eqconstraint%q%position%z, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%q%position%z'
endif

! Get eqconstraint/q/exact 
call get_int(idx,path, "eqconstraint/q/exact",int0d,status)
if (status.EQ.0) then
   cpo%eqconstraint%q%exact = int0d
   write(*,*) 'Get cpo%eqconstraint%q%exact'
endif                 

! Get eqconstraint/q/weight
call get_dimension(idx,path, "eqconstraint/q/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%q%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/q/weight", &
   cpo%eqconstraint%q%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%q%weight'
endif

! Get eqconstraint/q/sigma
call get_dimension(idx,path, "eqconstraint/q/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%q%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/q/sigma", &
   cpo%eqconstraint%q%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%q%sigma'
endif

! Get eqconstraint/q/calculated
call get_dimension(idx,path, "eqconstraint/q/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%q%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/q/calculated", &
   cpo%eqconstraint%q%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%q%calculated'
endif

! Get eqconstraint/q/chi2
call get_dimension(idx,path, "eqconstraint/q/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%q%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/q/chi2", &
   cpo%eqconstraint%q%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%q%chi2'
endif

! Get eqconstraint/isoflux/position/r
call get_dimension(idx,path, "eqconstraint/isoflux/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%isoflux%position%r(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/isoflux/position/r", &
   cpo%eqconstraint%isoflux%position%r, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%isoflux%position%r'
endif

! Get eqconstraint/isoflux/position/z
call get_dimension(idx,path, "eqconstraint/isoflux/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%isoflux%position%z(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/isoflux/position/z", &
   cpo%eqconstraint%isoflux%position%z, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%isoflux%position%z'
endif

! Get eqconstraint/isoflux/weight
call get_dimension(idx,path, "eqconstraint/isoflux/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%isoflux%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/isoflux/weight", &
   cpo%eqconstraint%isoflux%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%isoflux%weight'
endif

! Get eqconstraint/isoflux/sigma
call get_dimension(idx,path, "eqconstraint/isoflux/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%isoflux%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/isoflux/sigma", &
   cpo%eqconstraint%isoflux%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%isoflux%sigma'
endif

! Get eqconstraint/isoflux/calculated
call get_dimension(idx,path, "eqconstraint/isoflux/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%isoflux%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/isoflux/calculated", &
   cpo%eqconstraint%isoflux%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%isoflux%calculated'
endif

! Get eqconstraint/isoflux/chi2
call get_dimension(idx,path, "eqconstraint/isoflux/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%isoflux%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/isoflux/chi2", &
   cpo%eqconstraint%isoflux%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%isoflux%chi2'
endif

! Get eqconstraint/xpts/position/r
call get_dimension(idx,path, "eqconstraint/xpts/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%xpts%position%r(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/xpts/position/r", &
   cpo%eqconstraint%xpts%position%r, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%xpts%position%r'
endif

! Get eqconstraint/xpts/position/z
call get_dimension(idx,path, "eqconstraint/xpts/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%xpts%position%z(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/xpts/position/z", &
   cpo%eqconstraint%xpts%position%z, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%xpts%position%z'
endif

! Get eqconstraint/xpts/weight
call get_dimension(idx,path, "eqconstraint/xpts/weight",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%xpts%weight(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/xpts/weight", &
   cpo%eqconstraint%xpts%weight, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%xpts%weight'
endif

! Get eqconstraint/xpts/sigma
call get_dimension(idx,path, "eqconstraint/xpts/sigma",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%xpts%sigma(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/xpts/sigma", &
   cpo%eqconstraint%xpts%sigma, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%xpts%sigma'
endif

! Get eqconstraint/xpts/calculated
call get_dimension(idx,path, "eqconstraint/xpts/calculated",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%xpts%calculated(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/xpts/calculated", &
   cpo%eqconstraint%xpts%calculated, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%xpts%calculated'
endif

! Get eqconstraint/xpts/chi2
call get_dimension(idx,path, "eqconstraint/xpts/chi2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqconstraint%xpts%chi2(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqconstraint/xpts/chi2", &
   cpo%eqconstraint%xpts%chi2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqconstraint%xpts%chi2'
endif

! Get eqgeometry/datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "eqgeometry/datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqgeometry%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqgeometry%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqgeometry%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqgeometry%datainfo%dataprovider'
endif   

! Get eqgeometry/datainfo/putdate  
longstring = ' '
call get_string(idx,path, "eqgeometry/datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqgeometry%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqgeometry%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqgeometry%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqgeometry%datainfo%putdate'
endif   

! Get eqgeometry/datainfo/source  
longstring = ' '
call get_string(idx,path, "eqgeometry/datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqgeometry%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqgeometry%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqgeometry%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqgeometry%datainfo%source'
endif   

! Get eqgeometry/datainfo/comment  
longstring = ' '
call get_string(idx,path, "eqgeometry/datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqgeometry%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqgeometry%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqgeometry%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqgeometry%datainfo%comment'
endif   

! Get eqgeometry/datainfo/isref 
call get_int(idx,path, "eqgeometry/datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%eqgeometry%datainfo%isref = int0d
   write(*,*) 'Get cpo%eqgeometry%datainfo%isref'
endif                 

! Get eqgeometry/datainfo/whatref 
call get_int(idx,path, "eqgeometry/datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%eqgeometry%datainfo%whatref = int0d
   write(*,*) 'Get cpo%eqgeometry%datainfo%whatref'
endif                 

! Get eqgeometry/datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "eqgeometry/datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqgeometry%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqgeometry%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqgeometry%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqgeometry%datainfo%putinfo%putmethod'
endif   

! Get eqgeometry/datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "eqgeometry/datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqgeometry%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqgeometry%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqgeometry%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqgeometry%datainfo%putinfo%putaccess'
endif   

! Get eqgeometry/datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "eqgeometry/datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqgeometry%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqgeometry%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqgeometry%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqgeometry%datainfo%putinfo%putlocation'
endif   

! Get eqgeometry/datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "eqgeometry/datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%eqgeometry%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%eqgeometry%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%eqgeometry%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%eqgeometry%datainfo%putinfo%rights'
endif   

! Get eqgeometry/boundarytype        
! TIME DEPENDENT STRINGS NOT TREATED YET !!!

! Get eqgeometry/boundary/r
call get_dimension(idx,path, "eqgeometry/boundary/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqgeometry%boundary%r(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqgeometry/boundary/r", &
   cpo%eqgeometry%boundary%r, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqgeometry%boundary%r'
endif

! Get eqgeometry/boundary/z
call get_dimension(idx,path, "eqgeometry/boundary/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqgeometry%boundary%z(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqgeometry/boundary/z", &
   cpo%eqgeometry%boundary%z, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqgeometry%boundary%z'
endif

! Get eqgeometry/geom_axis/r
call get_double_Slice(idx,path, "eqgeometry/geom_axis/r",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%geom_axis%r = double0d
   write(*,*) 'Get cpo%eqgeometry%geom_axis%r'
endif

! Get eqgeometry/geom_axis/z
call get_double_Slice(idx,path, "eqgeometry/geom_axis/z",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%geom_axis%z = double0d
   write(*,*) 'Get cpo%eqgeometry%geom_axis%z'
endif

! Get eqgeometry/a_minor
call get_double_Slice(idx,path, "eqgeometry/a_minor",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%a_minor = double0d
   write(*,*) 'Get cpo%eqgeometry%a_minor'
endif

! Get eqgeometry/elongation
call get_double_Slice(idx,path, "eqgeometry/elongation",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%elongation = double0d
   write(*,*) 'Get cpo%eqgeometry%elongation'
endif

! Get eqgeometry/tria_upper
call get_double_Slice(idx,path, "eqgeometry/tria_upper",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%tria_upper = double0d
   write(*,*) 'Get cpo%eqgeometry%tria_upper'
endif

! Get eqgeometry/tria_lower
call get_double_Slice(idx,path, "eqgeometry/tria_lower",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%tria_lower = double0d
   write(*,*) 'Get cpo%eqgeometry%tria_lower'
endif

! Get eqgeometry/xpts/r
call get_dimension(idx,path, "eqgeometry/xpts/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqgeometry%xpts%r(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqgeometry/xpts/r", &
   cpo%eqgeometry%xpts%r, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqgeometry%xpts%r'
endif

! Get eqgeometry/xpts/z
call get_dimension(idx,path, "eqgeometry/xpts/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%eqgeometry%xpts%z(dim1))         
   call get_vect1d_double_Slice(idx,path, "eqgeometry/xpts/z", &
   cpo%eqgeometry%xpts%z, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%eqgeometry%xpts%z'
endif

! Get eqgeometry/left_low_st/r
call get_double_Slice(idx,path, "eqgeometry/left_low_st/r",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%left_low_st%r = double0d
   write(*,*) 'Get cpo%eqgeometry%left_low_st%r'
endif

! Get eqgeometry/left_low_st/z
call get_double_Slice(idx,path, "eqgeometry/left_low_st/z",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%left_low_st%z = double0d
   write(*,*) 'Get cpo%eqgeometry%left_low_st%z'
endif

! Get eqgeometry/right_low_st/r
call get_double_Slice(idx,path, "eqgeometry/right_low_st/r",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%right_low_st%r = double0d
   write(*,*) 'Get cpo%eqgeometry%right_low_st%r'
endif

! Get eqgeometry/right_low_st/z
call get_double_Slice(idx,path, "eqgeometry/right_low_st/z",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%right_low_st%z = double0d
   write(*,*) 'Get cpo%eqgeometry%right_low_st%z'
endif

! Get eqgeometry/left_up_st/r
call get_double_Slice(idx,path, "eqgeometry/left_up_st/r",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%left_up_st%r = double0d
   write(*,*) 'Get cpo%eqgeometry%left_up_st%r'
endif

! Get eqgeometry/left_up_st/z
call get_double_Slice(idx,path, "eqgeometry/left_up_st/z",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%left_up_st%z = double0d
   write(*,*) 'Get cpo%eqgeometry%left_up_st%z'
endif

! Get eqgeometry/right_up_st/r
call get_double_Slice(idx,path, "eqgeometry/right_up_st/r",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%right_up_st%r = double0d
   write(*,*) 'Get cpo%eqgeometry%right_up_st%r'
endif

! Get eqgeometry/right_up_st/z
call get_double_Slice(idx,path, "eqgeometry/right_up_st/z",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%right_up_st%z = double0d
   write(*,*) 'Get cpo%eqgeometry%right_up_st%z'
endif

! Get eqgeometry/active_limit/r
call get_double_Slice(idx,path, "eqgeometry/active_limit/r",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%active_limit%r = double0d
   write(*,*) 'Get cpo%eqgeometry%active_limit%r'
endif

! Get eqgeometry/active_limit/z
call get_double_Slice(idx,path, "eqgeometry/active_limit/z",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%eqgeometry%active_limit%z = double0d
   write(*,*) 'Get cpo%eqgeometry%active_limit%z'
endif

! Get flush/datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "flush/datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%datainfo%dataprovider'
endif   

! Get flush/datainfo/putdate  
longstring = ' '
call get_string(idx,path, "flush/datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%datainfo%putdate'
endif   

! Get flush/datainfo/source  
longstring = ' '
call get_string(idx,path, "flush/datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%datainfo%source'
endif   

! Get flush/datainfo/comment  
longstring = ' '
call get_string(idx,path, "flush/datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%datainfo%comment'
endif   

! Get flush/datainfo/isref 
call get_int(idx,path, "flush/datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%flush%datainfo%isref = int0d
   write(*,*) 'Get cpo%flush%datainfo%isref'
endif                 

! Get flush/datainfo/whatref 
call get_int(idx,path, "flush/datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%flush%datainfo%whatref = int0d
   write(*,*) 'Get cpo%flush%datainfo%whatref'
endif                 

! Get flush/datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "flush/datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%datainfo%putinfo%putmethod'
endif   

! Get flush/datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "flush/datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%datainfo%putinfo%putaccess'
endif   

! Get flush/datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "flush/datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%datainfo%putinfo%putlocation'
endif   

! Get flush/datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "flush/datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%datainfo%putinfo%rights'
endif   

! Get flush/position/r
call get_dimension(idx,path, "flush/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flush%position%r(dim1))         
   call get_vect1d_double_Slice(idx,path, "flush/position/r", &
   cpo%flush%position%r, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%flush%position%r'
endif

! Get flush/position/z
call get_dimension(idx,path, "flush/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flush%position%z(dim1))         
   call get_vect1d_double_Slice(idx,path, "flush/position/z", &
   cpo%flush%position%z, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%flush%position%z'
endif

! Get flush/coef
call get_dimension(idx,path, "flush/coef",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flush%coef(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "flush/coef", &
   cpo%flush%coef,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%flush%coef'
endif

! Get flush/codeparam/codename  
longstring = ' '
call get_string(idx,path, "flush/codeparam/codename",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%codeparam%codename(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%codeparam%codename = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%codeparam%codename(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%codeparam%codename'
endif   

! Get flush/codeparam/codeversion  
longstring = ' '
call get_string(idx,path, "flush/codeparam/codeversion",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%codeparam%codeversion(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%codeparam%codeversion = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%codeparam%codeversion(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%codeparam%codeversion'
endif   

! Get flush/codeparam/parameters  
longstring = ' '
call get_string(idx,path, "flush/codeparam/parameters",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%codeparam%parameters(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%codeparam%parameters = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%codeparam%parameters(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%codeparam%parameters'
endif   

! Get flush/codeparam/output_diag  
longstring = ' '
call get_string(idx,path, "flush/codeparam/output_diag",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%flush%codeparam%output_diag(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%flush%codeparam%output_diag = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%flush%codeparam%output_diag(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%flush%codeparam%output_diag'
endif   

! Get global_param/beta_pol
call get_double_Slice(idx,path, "global_param/beta_pol",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%beta_pol = double0d
   write(*,*) 'Get cpo%global_param%beta_pol'
endif

! Get global_param/beta_tor
call get_double_Slice(idx,path, "global_param/beta_tor",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%beta_tor = double0d
   write(*,*) 'Get cpo%global_param%beta_tor'
endif

! Get global_param/beta_normal
call get_double_Slice(idx,path, "global_param/beta_normal",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%beta_normal = double0d
   write(*,*) 'Get cpo%global_param%beta_normal'
endif

! Get global_param/i_plasma
call get_double_Slice(idx,path, "global_param/i_plasma",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%i_plasma = double0d
   write(*,*) 'Get cpo%global_param%i_plasma'
endif

! Get global_param/li
call get_double_Slice(idx,path, "global_param/li",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%li = double0d
   write(*,*) 'Get cpo%global_param%li'
endif

! Get global_param/volume
call get_double_Slice(idx,path, "global_param/volume",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%volume = double0d
   write(*,*) 'Get cpo%global_param%volume'
endif

! Get global_param/area
call get_double_Slice(idx,path, "global_param/area",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%area = double0d
   write(*,*) 'Get cpo%global_param%area'
endif

! Get global_param/psi_ax
call get_double_Slice(idx,path, "global_param/psi_ax",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%psi_ax = double0d
   write(*,*) 'Get cpo%global_param%psi_ax'
endif

! Get global_param/psi_bound
call get_double_Slice(idx,path, "global_param/psi_bound",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%psi_bound = double0d
   write(*,*) 'Get cpo%global_param%psi_bound'
endif

! Get global_param/mag_axis/position/r
call get_double_Slice(idx,path, "global_param/mag_axis/position/r",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%mag_axis%position%r = double0d
   write(*,*) 'Get cpo%global_param%mag_axis%position%r'
endif

! Get global_param/mag_axis/position/z
call get_double_Slice(idx,path, "global_param/mag_axis/position/z",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%mag_axis%position%z = double0d
   write(*,*) 'Get cpo%global_param%mag_axis%position%z'
endif

! Get global_param/mag_axis/bphi
call get_double_Slice(idx,path, "global_param/mag_axis/bphi",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%mag_axis%bphi = double0d
   write(*,*) 'Get cpo%global_param%mag_axis%bphi'
endif

! Get global_param/mag_axis/q
call get_double_Slice(idx,path, "global_param/mag_axis/q",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%mag_axis%q = double0d
   write(*,*) 'Get cpo%global_param%mag_axis%q'
endif

! Get global_param/q_95
call get_double_Slice(idx,path, "global_param/q_95",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%q_95 = double0d
   write(*,*) 'Get cpo%global_param%q_95'
endif

! Get global_param/q_min
call get_double_Slice(idx,path, "global_param/q_min",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%global_param%q_min = double0d
   write(*,*) 'Get cpo%global_param%q_min'
endif

! Get profiles_1d/psi
call get_dimension(idx,path, "profiles_1d/psi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%psi(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/psi", &
   cpo%profiles_1d%psi, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%psi'
endif

! Get profiles_1d/phi
call get_dimension(idx,path, "profiles_1d/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%phi(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/phi", &
   cpo%profiles_1d%phi, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%phi'
endif

! Get profiles_1d/pressure
call get_dimension(idx,path, "profiles_1d/pressure",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%pressure(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/pressure", &
   cpo%profiles_1d%pressure, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%pressure'
endif

! Get profiles_1d/F_dia
call get_dimension(idx,path, "profiles_1d/F_dia",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%F_dia(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/F_dia", &
   cpo%profiles_1d%F_dia, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%F_dia'
endif

! Get profiles_1d/pprime
call get_dimension(idx,path, "profiles_1d/pprime",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%pprime(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/pprime", &
   cpo%profiles_1d%pprime, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%pprime'
endif

! Get profiles_1d/ffprime
call get_dimension(idx,path, "profiles_1d/ffprime",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%ffprime(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/ffprime", &
   cpo%profiles_1d%ffprime, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%ffprime'
endif

! Get profiles_1d/jphi
call get_dimension(idx,path, "profiles_1d/jphi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%jphi(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/jphi", &
   cpo%profiles_1d%jphi, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%jphi'
endif

! Get profiles_1d/jparallel
call get_dimension(idx,path, "profiles_1d/jparallel",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%jparallel(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/jparallel", &
   cpo%profiles_1d%jparallel, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%jparallel'
endif

! Get profiles_1d/q
call get_dimension(idx,path, "profiles_1d/q",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%q(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/q", &
   cpo%profiles_1d%q, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%q'
endif

! Get profiles_1d/r_inboard
call get_dimension(idx,path, "profiles_1d/r_inboard",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%r_inboard(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/r_inboard", &
   cpo%profiles_1d%r_inboard, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%r_inboard'
endif

! Get profiles_1d/r_outboard
call get_dimension(idx,path, "profiles_1d/r_outboard",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%r_outboard(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/r_outboard", &
   cpo%profiles_1d%r_outboard, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%r_outboard'
endif

! Get profiles_1d/rho_rtvol
call get_dimension(idx,path, "profiles_1d/rho_rtvol",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%rho_rtvol(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/rho_rtvol", &
   cpo%profiles_1d%rho_rtvol, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%rho_rtvol'
endif

! Get profiles_1d/rho_rttorfl
call get_dimension(idx,path, "profiles_1d/rho_rttorfl",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%rho_rttorfl(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/rho_rttorfl", &
   cpo%profiles_1d%rho_rttorfl, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%rho_rttorfl'
endif

! Get profiles_1d/elongation
call get_dimension(idx,path, "profiles_1d/elongation",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%elongation(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/elongation", &
   cpo%profiles_1d%elongation, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%elongation'
endif

! Get profiles_1d/tria_upper
call get_dimension(idx,path, "profiles_1d/tria_upper",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%tria_upper(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/tria_upper", &
   cpo%profiles_1d%tria_upper, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%tria_upper'
endif

! Get profiles_1d/tria_lower
call get_dimension(idx,path, "profiles_1d/tria_lower",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%tria_lower(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/tria_lower", &
   cpo%profiles_1d%tria_lower, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%tria_lower'
endif

! Get profiles_1d/volume
call get_dimension(idx,path, "profiles_1d/volume",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_1d%volume(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_1d/volume", &
   cpo%profiles_1d%volume, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_1d%volume'
endif

! Get profiles_2d/grid_type  
longstring = ' '
call get_string(idx,path, "profiles_2d/grid_type",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%profiles_2d%grid_type(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%profiles_2d%grid_type = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%profiles_2d%grid_type(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%profiles_2d%grid_type'
endif   

! Get profiles_2d/grid/dim1
call get_dimension(idx,path, "profiles_2d/grid/dim1",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_2d%grid%dim1(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_2d/grid/dim1", &
   cpo%profiles_2d%grid%dim1, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_2d%grid%dim1'
endif

! Get profiles_2d/grid/dim2
call get_dimension(idx,path, "profiles_2d/grid/dim2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_2d%grid%dim2(dim1))         
   call get_vect1d_double_Slice(idx,path, "profiles_2d/grid/dim2", &
   cpo%profiles_2d%grid%dim2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_2d%grid%dim2'
endif

! Get profiles_2d/grid/connect
call get_dimension(idx,path, "profiles_2d/grid/connect",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_2d%grid%connect(dim1,dim2))         
   call get_vect2d_int_Slice(idx,path, "profiles_2d/grid/connect", &
   cpo%profiles_2d%grid%connect,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_2d%grid%connect'
endif

! Get profiles_2d/psi_grid
call get_dimension(idx,path, "profiles_2d/psi_grid",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_2d%psi_grid(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "profiles_2d/psi_grid", &
   cpo%profiles_2d%psi_grid,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_2d%psi_grid'
endif

! Get profiles_2d/jphi_grid
call get_dimension(idx,path, "profiles_2d/jphi_grid",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_2d%jphi_grid(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "profiles_2d/jphi_grid", &
   cpo%profiles_2d%jphi_grid,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_2d%jphi_grid'
endif

! Get profiles_2d/jpar_grid
call get_dimension(idx,path, "profiles_2d/jpar_grid",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_2d%jpar_grid(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "profiles_2d/jpar_grid", &
   cpo%profiles_2d%jpar_grid,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_2d%jpar_grid'
endif

! Get profiles_2d/br
call get_dimension(idx,path, "profiles_2d/br",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_2d%br(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "profiles_2d/br", &
   cpo%profiles_2d%br,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_2d%br'
endif

! Get profiles_2d/bz
call get_dimension(idx,path, "profiles_2d/bz",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_2d%bz(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "profiles_2d/bz", &
   cpo%profiles_2d%bz,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_2d%bz'
endif

! Get profiles_2d/bphi
call get_dimension(idx,path, "profiles_2d/bphi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%profiles_2d%bphi(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "profiles_2d/bphi", &
   cpo%profiles_2d%bphi,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%profiles_2d%bphi'
endif

! Get coord_sys/grid_type  
longstring = ' '
call get_string(idx,path, "coord_sys/grid_type",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%coord_sys%grid_type(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%coord_sys%grid_type = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%coord_sys%grid_type(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%coord_sys%grid_type'
endif   

! Get coord_sys/grid/dim1
call get_dimension(idx,path, "coord_sys/grid/dim1",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%coord_sys%grid%dim1(dim1))         
   call get_vect1d_double_Slice(idx,path, "coord_sys/grid/dim1", &
   cpo%coord_sys%grid%dim1, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%coord_sys%grid%dim1'
endif

! Get coord_sys/grid/dim2
call get_dimension(idx,path, "coord_sys/grid/dim2",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%coord_sys%grid%dim2(dim1))         
   call get_vect1d_double_Slice(idx,path, "coord_sys/grid/dim2", &
   cpo%coord_sys%grid%dim2, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%coord_sys%grid%dim2'
endif

! Get coord_sys/jacobian
call get_dimension(idx,path, "coord_sys/jacobian",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%coord_sys%jacobian(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "coord_sys/jacobian", &
   cpo%coord_sys%jacobian,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%coord_sys%jacobian'
endif

! Get coord_sys/g_11
call get_dimension(idx,path, "coord_sys/g_11",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%coord_sys%g_11(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "coord_sys/g_11", &
   cpo%coord_sys%g_11,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%coord_sys%g_11'
endif

! Get coord_sys/g_12
call get_dimension(idx,path, "coord_sys/g_12",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%coord_sys%g_12(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "coord_sys/g_12", &
   cpo%coord_sys%g_12,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%coord_sys%g_12'
endif

! Get coord_sys/g_22
call get_dimension(idx,path, "coord_sys/g_22",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%coord_sys%g_22(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "coord_sys/g_22", &
   cpo%coord_sys%g_22,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%coord_sys%g_22'
endif

! Get coord_sys/g_33
call get_dimension(idx,path, "coord_sys/g_33",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%coord_sys%g_33(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "coord_sys/g_33", &
   cpo%coord_sys%g_33,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%coord_sys%g_33'
endif

! Get coord_sys/position/r
call get_dimension(idx,path, "coord_sys/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%coord_sys%position%r(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "coord_sys/position/r", &
   cpo%coord_sys%position%r,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%coord_sys%position%r'
endif

! Get coord_sys/position/z
call get_dimension(idx,path, "coord_sys/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%coord_sys%position%z(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "coord_sys/position/z", &
   cpo%coord_sys%position%z,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%coord_sys%position%z'
endif

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

! Get codeparam/codename  
longstring = ' '
call get_string(idx,path, "codeparam/codename",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%codename(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%codename = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%codename(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%codename'
endif   

! Get codeparam/codeversion  
longstring = ' '
call get_string(idx,path, "codeparam/codeversion",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%codeversion(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%codeversion = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%codeversion(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%codeversion'
endif   

! Get codeparam/parameters  
longstring = ' '
call get_string(idx,path, "codeparam/parameters",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%parameters(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%parameters = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%parameters(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%parameters'
endif   

! Get codeparam/output_diag  
longstring = ' '
call get_string(idx,path, "codeparam/output_diag",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%output_diag(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%output_diag = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%output_diag(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%output_diag'
endif   

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_ironmodel(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_ironmodel) :: cpo      
    
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get desc_iron/name  
call get_dimension(idx,path, "desc_iron/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%desc_iron%name(dim1))
   call get_Vect1d_string(idx,path, "desc_iron/name", &
                        cpo%desc_iron%name,dim1,dum1,status)
   write(*,*) 'Get cpo%desc_iron%name'
endif   

! Get desc_iron/id  
call get_dimension(idx,path, "desc_iron/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%desc_iron%id(dim1))
   call get_Vect1d_string(idx,path, "desc_iron/id", &
                        cpo%desc_iron%id,dim1,dum1,status)
   write(*,*) 'Get cpo%desc_iron%id'
endif   

! Get desc_iron/permeability/B        
call get_dimension(idx,path, "desc_iron/permeability/B",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%desc_iron%permeability%B(dim1,dim2))
   call get_vect2d_double(idx,path,"desc_iron/permeability/B", &
   cpo%desc_iron%permeability%B, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%desc_iron%permeability%B'
endif 

! Get desc_iron/permeability/mur        
call get_dimension(idx,path, "desc_iron/permeability/mur",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%desc_iron%permeability%mur(dim1,dim2))
   call get_vect2d_double(idx,path,"desc_iron/permeability/mur", &
   cpo%desc_iron%permeability%mur, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%desc_iron%permeability%mur'
endif 

! Get desc_iron/geom_iron/npoints        
call get_dimension(idx,path, "desc_iron/geom_iron/npoints",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%desc_iron%geom_iron%npoints(dim1))
   call get_vect1d_int(idx,path,"desc_iron/geom_iron/npoints",cpo%desc_iron%geom_iron%npoints,dim1,dum1,status)
   write(*,*) 'Get cpo%desc_iron%geom_iron%npoints'
endif 

! Get desc_iron/geom_iron/rzcoordinate/r        
call get_dimension(idx,path, "desc_iron/geom_iron/rzcoordinate/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%desc_iron%geom_iron%rzcoordinate%r(dim1,dim2))
   call get_vect2d_double(idx,path,"desc_iron/geom_iron/rzcoordinate/r", &
   cpo%desc_iron%geom_iron%rzcoordinate%r, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%desc_iron%geom_iron%rzcoordinate%r'
endif 

! Get desc_iron/geom_iron/rzcoordinate/z        
call get_dimension(idx,path, "desc_iron/geom_iron/rzcoordinate/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%desc_iron%geom_iron%rzcoordinate%z(dim1,dim2))
   call get_vect2d_double(idx,path,"desc_iron/geom_iron/rzcoordinate/z", &
   cpo%desc_iron%geom_iron%rzcoordinate%z, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%desc_iron%geom_iron%rzcoordinate%z'
endif 

! Get magnetise/mr/value
call get_dimension(idx,path, "magnetise/mr/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%magnetise%mr%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "magnetise/mr/value", &
   cpo%magnetise%mr%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%magnetise%mr%value'
endif

! Get magnetise/mr/abserror
call get_dimension(idx,path, "magnetise/mr/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%magnetise%mr%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "magnetise/mr/abserror", &
   cpo%magnetise%mr%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%magnetise%mr%abserror'
endif

! Get magnetise/mr/relerror
call get_dimension(idx,path, "magnetise/mr/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%magnetise%mr%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "magnetise/mr/relerror", &
   cpo%magnetise%mr%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%magnetise%mr%relerror'
endif

! Get magnetise/mz/value
call get_dimension(idx,path, "magnetise/mz/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%magnetise%mz%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "magnetise/mz/value", &
   cpo%magnetise%mz%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%magnetise%mz%value'
endif

! Get magnetise/mz/abserror
call get_dimension(idx,path, "magnetise/mz/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%magnetise%mz%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "magnetise/mz/abserror", &
   cpo%magnetise%mz%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%magnetise%mz%abserror'
endif

! Get magnetise/mz/relerror
call get_dimension(idx,path, "magnetise/mz/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%magnetise%mz%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "magnetise/mz/relerror", &
   cpo%magnetise%mz%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%magnetise%mz%relerror'
endif

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_interfdiag(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_lineintegraldiag) :: cpo  
	
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get expression  
longstring = ' '
call get_string(idx,path, "expression",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%expression(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%expression = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%expression(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%expression'
endif   

! Get setup_line/pivot_point/r
call get_dimension(idx,path, "setup_line/pivot_point/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%pivot_point%r(dim1))
   call get_vect1d_double(idx,path,"setup_line/pivot_point/r", &
   cpo%setup_line%pivot_point%r,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%pivot_point%r'
endif        

! Get setup_line/pivot_point/z
call get_dimension(idx,path, "setup_line/pivot_point/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%pivot_point%z(dim1))
   call get_vect1d_double(idx,path,"setup_line/pivot_point/z", &
   cpo%setup_line%pivot_point%z,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%pivot_point%z'
endif        

! Get setup_line/pivot_point/phi
call get_dimension(idx,path, "setup_line/pivot_point/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%pivot_point%phi(dim1))
   call get_vect1d_double(idx,path,"setup_line/pivot_point/phi", &
   cpo%setup_line%pivot_point%phi,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%pivot_point%phi'
endif        

! Get setup_line/polchordang
call get_dimension(idx,path, "setup_line/polchordang",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%polchordang(dim1))
   call get_vect1d_double(idx,path,"setup_line/polchordang", &
   cpo%setup_line%polchordang,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%polchordang'
endif        

! Get setup_line/torchordang
call get_dimension(idx,path, "setup_line/torchordang",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%torchordang(dim1))
   call get_vect1d_double(idx,path,"setup_line/torchordang", &
   cpo%setup_line%torchordang,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%torchordang'
endif        

! Get setup_line/second_point/r
call get_dimension(idx,path, "setup_line/second_point/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%second_point%r(dim1))
   call get_vect1d_double(idx,path,"setup_line/second_point/r", &
   cpo%setup_line%second_point%r,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%second_point%r'
endif        

! Get setup_line/second_point/z
call get_dimension(idx,path, "setup_line/second_point/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%second_point%z(dim1))
   call get_vect1d_double(idx,path,"setup_line/second_point/z", &
   cpo%setup_line%second_point%z,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%second_point%z'
endif        

! Get setup_line/second_point/phi
call get_dimension(idx,path, "setup_line/second_point/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%second_point%phi(dim1))
   call get_vect1d_double(idx,path,"setup_line/second_point/phi", &
   cpo%setup_line%second_point%phi,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%second_point%phi'
endif        

! Get setup_line/nchordpoints 
call get_int(idx,path, "setup_line/nchordpoints",int0d,status)
if (status.EQ.0) then
   cpo%setup_line%nchordpoints = int0d
   write(*,*) 'Get cpo%setup_line%nchordpoints'
endif                 

! Get measure/value
call get_dimension(idx,path, "measure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%measure%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "measure/value", &
   cpo%measure%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%measure%value'
endif

! Get measure/abserror
call get_dimension(idx,path, "measure/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%measure%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "measure/abserror", &
   cpo%measure%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%measure%abserror'
endif

! Get measure/relerror
call get_dimension(idx,path, "measure/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%measure%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "measure/relerror", &
   cpo%measure%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%measure%relerror'
endif

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_limiter(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_limiter) :: cpo      
    
write(*,*) 'Warning : GET_SLICE requested for a time-independent CPO is equivalent to a simple GET'
call euITM_get_limiter(idx,path,  cpo)
	

return
endsubroutine

subroutine euITM_get_slice_mhd(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_mhd) :: cpo      
    
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get n
call get_dimension(idx,path, "n",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%n(dim1))         
   call get_vect1d_double_Slice(idx,path, "n", &
   cpo%n, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%n'
endif

! Get m
call get_dimension(idx,path, "m",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%m(dim1,dim2))         
   call get_vect2d_double_Slice(idx,path, "m", &
   cpo%m,&
   dim1,dim2,dum1,dum2,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%m'
endif

! Get psi
call get_dimension(idx,path, "psi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%psi(dim1))         
   call get_vect1d_double_Slice(idx,path, "psi", &
   cpo%psi, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%psi'
endif

! Get frequency
call get_dimension(idx,path, "frequency",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%frequency(dim1))         
   call get_vect1d_double_Slice(idx,path, "frequency", &
   cpo%frequency, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%frequency'
endif

! Get growthrate
call get_dimension(idx,path, "growthrate",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%growthrate(dim1))         
   call get_vect1d_double_Slice(idx,path, "growthrate", &
   cpo%growthrate, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%growthrate'
endif

! Get disp_perp        
call get_dimension(idx,path, "disp_perp",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%disp_perp(dim1,dim2,dim3))         
   call get_vect3d_double_Slice(idx,path, "disp_perp", &
   cpo%disp_perp,&
   dim1,dim2,dim3,dum1,dum2,dum3,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%disp_perp'
endif

! Get disp_par        
call get_dimension(idx,path, "disp_par",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%disp_par(dim1,dim2,dim3))         
   call get_vect3d_double_Slice(idx,path, "disp_par", &
   cpo%disp_par,&
   dim1,dim2,dim3,dum1,dum2,dum3,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%disp_par'
endif

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

! Get codeparam/codename  
longstring = ' '
call get_string(idx,path, "codeparam/codename",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%codename(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%codename = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%codename(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%codename'
endif   

! Get codeparam/codeversion  
longstring = ' '
call get_string(idx,path, "codeparam/codeversion",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%codeversion(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%codeversion = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%codeversion(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%codeversion'
endif   

! Get codeparam/parameters  
longstring = ' '
call get_string(idx,path, "codeparam/parameters",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%parameters(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%parameters = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%parameters(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%parameters'
endif   

! Get codeparam/output_diag  
longstring = ' '
call get_string(idx,path, "codeparam/output_diag",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%codeparam%output_diag(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%codeparam%output_diag = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%codeparam%output_diag(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%codeparam%output_diag'
endif   

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_magdiag(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_magdiag) :: cpo      
    
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get ip/value
call get_double_Slice(idx,path, "ip/value",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%ip%value = double0d
   write(*,*) 'Get cpo%ip%value'
endif

! Get ip/abserror
call get_double_Slice(idx,path, "ip/abserror",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%ip%abserror = double0d
   write(*,*) 'Get cpo%ip%abserror'
endif

! Get ip/relerror
call get_double_Slice(idx,path, "ip/relerror",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%ip%relerror = double0d
   write(*,*) 'Get cpo%ip%relerror'
endif

! Get diamagflux/value
call get_double_Slice(idx,path, "diamagflux/value",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%diamagflux%value = double0d
   write(*,*) 'Get cpo%diamagflux%value'
endif

! Get diamagflux/abserror
call get_double_Slice(idx,path, "diamagflux/abserror",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%diamagflux%abserror = double0d
   write(*,*) 'Get cpo%diamagflux%abserror'
endif

! Get diamagflux/relerror
call get_double_Slice(idx,path, "diamagflux/relerror",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%diamagflux%relerror = double0d
   write(*,*) 'Get cpo%diamagflux%relerror'
endif

! Get flux_loops/setup_floops/name  
call get_dimension(idx,path, "flux_loops/setup_floops/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flux_loops%setup_floops%name(dim1))
   call get_Vect1d_string(idx,path, "flux_loops/setup_floops/name", &
                        cpo%flux_loops%setup_floops%name,dim1,dum1,status)
   write(*,*) 'Get cpo%flux_loops%setup_floops%name'
endif   

! Get flux_loops/setup_floops/id  
call get_dimension(idx,path, "flux_loops/setup_floops/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flux_loops%setup_floops%id(dim1))
   call get_Vect1d_string(idx,path, "flux_loops/setup_floops/id", &
                        cpo%flux_loops%setup_floops%id,dim1,dum1,status)
   write(*,*) 'Get cpo%flux_loops%setup_floops%id'
endif   

! Get flux_loops/setup_floops/position/r        
call get_dimension(idx,path, "flux_loops/setup_floops/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flux_loops%setup_floops%position%r(dim1,dim2))
   call get_vect2d_double(idx,path,"flux_loops/setup_floops/position/r", &
   cpo%flux_loops%setup_floops%position%r, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%flux_loops%setup_floops%position%r'
endif 

! Get flux_loops/setup_floops/position/z        
call get_dimension(idx,path, "flux_loops/setup_floops/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flux_loops%setup_floops%position%z(dim1,dim2))
   call get_vect2d_double(idx,path,"flux_loops/setup_floops/position/z", &
   cpo%flux_loops%setup_floops%position%z, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%flux_loops%setup_floops%position%z'
endif 

! Get flux_loops/setup_floops/position/phi        
call get_dimension(idx,path, "flux_loops/setup_floops/position/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flux_loops%setup_floops%position%phi(dim1,dim2))
   call get_vect2d_double(idx,path,"flux_loops/setup_floops/position/phi", &
   cpo%flux_loops%setup_floops%position%phi, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%flux_loops%setup_floops%position%phi'
endif 

! Get flux_loops/setup_floops/npoints        
call get_dimension(idx,path, "flux_loops/setup_floops/npoints",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flux_loops%setup_floops%npoints(dim1))
   call get_vect1d_int(idx,path,"flux_loops/setup_floops/npoints",cpo%flux_loops%setup_floops%npoints,dim1,dum1,status)
   write(*,*) 'Get cpo%flux_loops%setup_floops%npoints'
endif 

! Get flux_loops/measure/value
call get_dimension(idx,path, "flux_loops/measure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flux_loops%measure%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "flux_loops/measure/value", &
   cpo%flux_loops%measure%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%flux_loops%measure%value'
endif

! Get flux_loops/measure/abserror
call get_dimension(idx,path, "flux_loops/measure/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flux_loops%measure%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "flux_loops/measure/abserror", &
   cpo%flux_loops%measure%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%flux_loops%measure%abserror'
endif

! Get flux_loops/measure/relerror
call get_dimension(idx,path, "flux_loops/measure/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%flux_loops%measure%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "flux_loops/measure/relerror", &
   cpo%flux_loops%measure%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%flux_loops%measure%relerror'
endif

! Get bpol_probes/setup_bprobe/name  
call get_dimension(idx,path, "bpol_probes/setup_bprobe/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%setup_bprobe%name(dim1))
   call get_Vect1d_string(idx,path, "bpol_probes/setup_bprobe/name", &
                        cpo%bpol_probes%setup_bprobe%name,dim1,dum1,status)
   write(*,*) 'Get cpo%bpol_probes%setup_bprobe%name'
endif   

! Get bpol_probes/setup_bprobe/id  
call get_dimension(idx,path, "bpol_probes/setup_bprobe/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%setup_bprobe%id(dim1))
   call get_Vect1d_string(idx,path, "bpol_probes/setup_bprobe/id", &
                        cpo%bpol_probes%setup_bprobe%id,dim1,dum1,status)
   write(*,*) 'Get cpo%bpol_probes%setup_bprobe%id'
endif   

! Get bpol_probes/setup_bprobe/position/r
call get_dimension(idx,path, "bpol_probes/setup_bprobe/position/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%setup_bprobe%position%r(dim1))
   call get_vect1d_double(idx,path,"bpol_probes/setup_bprobe/position/r", &
   cpo%bpol_probes%setup_bprobe%position%r,dim1,dum1,status)
   write(*,*) 'Get cpo%bpol_probes%setup_bprobe%position%r'
endif        

! Get bpol_probes/setup_bprobe/position/z
call get_dimension(idx,path, "bpol_probes/setup_bprobe/position/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%setup_bprobe%position%z(dim1))
   call get_vect1d_double(idx,path,"bpol_probes/setup_bprobe/position/z", &
   cpo%bpol_probes%setup_bprobe%position%z,dim1,dum1,status)
   write(*,*) 'Get cpo%bpol_probes%setup_bprobe%position%z'
endif        

! Get bpol_probes/setup_bprobe/polangle
call get_dimension(idx,path, "bpol_probes/setup_bprobe/polangle",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%setup_bprobe%polangle(dim1))
   call get_vect1d_double(idx,path,"bpol_probes/setup_bprobe/polangle", &
   cpo%bpol_probes%setup_bprobe%polangle,dim1,dum1,status)
   write(*,*) 'Get cpo%bpol_probes%setup_bprobe%polangle'
endif        

! Get bpol_probes/setup_bprobe/torangle
call get_dimension(idx,path, "bpol_probes/setup_bprobe/torangle",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%setup_bprobe%torangle(dim1))
   call get_vect1d_double(idx,path,"bpol_probes/setup_bprobe/torangle", &
   cpo%bpol_probes%setup_bprobe%torangle,dim1,dum1,status)
   write(*,*) 'Get cpo%bpol_probes%setup_bprobe%torangle'
endif        

! Get bpol_probes/setup_bprobe/area
call get_dimension(idx,path, "bpol_probes/setup_bprobe/area",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%setup_bprobe%area(dim1))
   call get_vect1d_double(idx,path,"bpol_probes/setup_bprobe/area", &
   cpo%bpol_probes%setup_bprobe%area,dim1,dum1,status)
   write(*,*) 'Get cpo%bpol_probes%setup_bprobe%area'
endif        

! Get bpol_probes/setup_bprobe/length
call get_dimension(idx,path, "bpol_probes/setup_bprobe/length",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%setup_bprobe%length(dim1))
   call get_vect1d_double(idx,path,"bpol_probes/setup_bprobe/length", &
   cpo%bpol_probes%setup_bprobe%length,dim1,dum1,status)
   write(*,*) 'Get cpo%bpol_probes%setup_bprobe%length'
endif        

! Get bpol_probes/setup_bprobe/turns        
call get_dimension(idx,path, "bpol_probes/setup_bprobe/turns",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%setup_bprobe%turns(dim1))
   call get_vect1d_int(idx,path,"bpol_probes/setup_bprobe/turns",cpo%bpol_probes%setup_bprobe%turns,dim1,dum1,status)
   write(*,*) 'Get cpo%bpol_probes%setup_bprobe%turns'
endif 

! Get bpol_probes/measure/value
call get_dimension(idx,path, "bpol_probes/measure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%measure%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "bpol_probes/measure/value", &
   cpo%bpol_probes%measure%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%bpol_probes%measure%value'
endif

! Get bpol_probes/measure/abserror
call get_dimension(idx,path, "bpol_probes/measure/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%measure%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "bpol_probes/measure/abserror", &
   cpo%bpol_probes%measure%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%bpol_probes%measure%abserror'
endif

! Get bpol_probes/measure/relerror
call get_dimension(idx,path, "bpol_probes/measure/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%bpol_probes%measure%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "bpol_probes/measure/relerror", &
   cpo%bpol_probes%measure%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%bpol_probes%measure%relerror'
endif

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_msediag(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_msediag) :: cpo      
    
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get setup_mse/rzgamma/r
call get_dimension(idx,path, "setup_mse/rzgamma/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_mse%rzgamma%r(dim1))
   call get_vect1d_double(idx,path,"setup_mse/rzgamma/r", &
   cpo%setup_mse%rzgamma%r,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_mse%rzgamma%r'
endif        

! Get setup_mse/rzgamma/z
call get_dimension(idx,path, "setup_mse/rzgamma/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_mse%rzgamma%z(dim1))
   call get_vect1d_double(idx,path,"setup_mse/rzgamma/z", &
   cpo%setup_mse%rzgamma%z,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_mse%rzgamma%z'
endif        

! Get setup_mse/geom_coef        
call get_dimension(idx,path, "setup_mse/geom_coef",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_mse%geom_coef(dim1,dim2))
   call get_vect2d_double(idx,path,"setup_mse/geom_coef", &
   cpo%setup_mse%geom_coef, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%setup_mse%geom_coef'
endif 

! Get measure/value
call get_dimension(idx,path, "measure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%measure%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "measure/value", &
   cpo%measure%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%measure%value'
endif

! Get measure/abserror
call get_dimension(idx,path, "measure/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%measure%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "measure/abserror", &
   cpo%measure%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%measure%abserror'
endif

! Get measure/relerror
call get_dimension(idx,path, "measure/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%measure%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "measure/relerror", &
   cpo%measure%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%measure%relerror'
endif

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_pfsystems(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_pfsystems) :: cpo      
    
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get pfcoils/desc_pfcoils/name  
call get_dimension(idx,path, "pfcoils/desc_pfcoils/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%name(dim1))
   call get_Vect1d_string(idx,path, "pfcoils/desc_pfcoils/name", &
                        cpo%pfcoils%desc_pfcoils%name,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%name'
endif   

! Get pfcoils/desc_pfcoils/id  
call get_dimension(idx,path, "pfcoils/desc_pfcoils/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%id(dim1))
   call get_Vect1d_string(idx,path, "pfcoils/desc_pfcoils/id", &
                        cpo%pfcoils%desc_pfcoils%id,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%id'
endif   

! Get pfcoils/desc_pfcoils/res
call get_dimension(idx,path, "pfcoils/desc_pfcoils/res",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%res(dim1))
   call get_vect1d_double(idx,path,"pfcoils/desc_pfcoils/res", &
   cpo%pfcoils%desc_pfcoils%res,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%res'
endif        

! Get pfcoils/desc_pfcoils/emax
call get_dimension(idx,path, "pfcoils/desc_pfcoils/emax",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%emax(dim1))
   call get_vect1d_double(idx,path,"pfcoils/desc_pfcoils/emax", &
   cpo%pfcoils%desc_pfcoils%emax,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%emax'
endif        

! Get pfcoils/desc_pfcoils/nelement        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/nelement",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%nelement(dim1))
   call get_vect1d_int(idx,path,"pfcoils/desc_pfcoils/nelement",cpo%pfcoils%desc_pfcoils%nelement,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%nelement'
endif 

! Get pfcoils/desc_pfcoils/pfelement/name  
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%pfelement%name(dim1))
   call get_Vect1d_string(idx,path, "pfcoils/desc_pfcoils/pfelement/name", &
                        cpo%pfcoils%desc_pfcoils%pfelement%name,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%pfelement%name'
endif   

! Get pfcoils/desc_pfcoils/pfelement/id  
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%pfelement%id(dim1))
   call get_Vect1d_string(idx,path, "pfcoils/desc_pfcoils/pfelement/id", &
                        cpo%pfcoils%desc_pfcoils%pfelement%id,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%pfelement%id'
endif   

! Get pfcoils/desc_pfcoils/pfelement/turnsign        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/turnsign",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%pfelement%turnsign(dim1,dim2))
   call get_vect2d_double(idx,path,"pfcoils/desc_pfcoils/pfelement/turnsign", &
   cpo%pfcoils%desc_pfcoils%pfelement%turnsign, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%pfelement%turnsign'
endif 

! Get pfcoils/desc_pfcoils/pfelement/area        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/area",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%pfelement%area(dim1,dim2))
   call get_vect2d_double(idx,path,"pfcoils/desc_pfcoils/pfelement/area", &
   cpo%pfcoils%desc_pfcoils%pfelement%area, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%pfelement%area'
endif 

! Get pfcoils/desc_pfcoils/pfelement/pfgeometry/type        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/type",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%type(dim1,dim2))
   call get_vect2d_int(idx,path,"pfcoils/desc_pfcoils/pfelement/pfgeometry/type", &
   cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%type, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%type'
endif

! Get pfcoils/desc_pfcoils/pfelement/pfgeometry/npoints        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/npoints",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints(dim1,dim2))
   call get_vect2d_int(idx,path,"pfcoils/desc_pfcoils/pfelement/pfgeometry/npoints", &
   cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints'
endif

! Get pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/r        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r(dim1,dim2,dim3))
   call get_vect3d_double(idx,path,"pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/r",&
   cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r, &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r'
endif

! Get pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/z        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z(dim1,dim2,dim3))
   call get_vect3d_double(idx,path,"pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/z",&
   cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z, &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z'
endif

! Get pfcoils/desc_pfcoils/pfelement/pfgeometry/rzdrdz        
call get_dimension(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzdrdz",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz(dim1,dim2,dim3))
   call get_vect3d_double(idx,path,"pfcoils/desc_pfcoils/pfelement/pfgeometry/rzdrdz",&
   cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz, &
   dim1,dim2,dim3,dum1,dum2,dum3,status)
   write(*,*) 'Get cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz'
endif

! Get pfcoils/coilcurrent/value
call get_dimension(idx,path, "pfcoils/coilcurrent/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%coilcurrent%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfcoils/coilcurrent/value", &
   cpo%pfcoils%coilcurrent%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfcoils%coilcurrent%value'
endif

! Get pfcoils/coilcurrent/abserror
call get_dimension(idx,path, "pfcoils/coilcurrent/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%coilcurrent%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfcoils/coilcurrent/abserror", &
   cpo%pfcoils%coilcurrent%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfcoils%coilcurrent%abserror'
endif

! Get pfcoils/coilcurrent/relerror
call get_dimension(idx,path, "pfcoils/coilcurrent/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%coilcurrent%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfcoils/coilcurrent/relerror", &
   cpo%pfcoils%coilcurrent%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfcoils%coilcurrent%relerror'
endif

! Get pfcoils/coilvoltage/value
call get_dimension(idx,path, "pfcoils/coilvoltage/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%coilvoltage%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfcoils/coilvoltage/value", &
   cpo%pfcoils%coilvoltage%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfcoils%coilvoltage%value'
endif

! Get pfcoils/coilvoltage/abserror
call get_dimension(idx,path, "pfcoils/coilvoltage/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%coilvoltage%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfcoils/coilvoltage/abserror", &
   cpo%pfcoils%coilvoltage%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfcoils%coilvoltage%abserror'
endif

! Get pfcoils/coilvoltage/relerror
call get_dimension(idx,path, "pfcoils/coilvoltage/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcoils%coilvoltage%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfcoils/coilvoltage/relerror", &
   cpo%pfcoils%coilvoltage%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfcoils%coilvoltage%relerror'
endif

! Get pfpassive/area
call get_dimension(idx,path, "pfpassive/area",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfpassive%area(dim1))
   call get_vect1d_double(idx,path,"pfpassive/area", &
   cpo%pfpassive%area,dim1,dum1,status)
   write(*,*) 'Get cpo%pfpassive%area'
endif        

! Get pfpassive/res
call get_dimension(idx,path, "pfpassive/res",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfpassive%res(dim1))
   call get_vect1d_double(idx,path,"pfpassive/res", &
   cpo%pfpassive%res,dim1,dum1,status)
   write(*,*) 'Get cpo%pfpassive%res'
endif        

! Get pfpassive/pfpageometry/type        
call get_dimension(idx,path, "pfpassive/pfpageometry/type",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfpassive%pfpageometry%type(dim1))
   call get_vect1d_int(idx,path,"pfpassive/pfpageometry/type",cpo%pfpassive%pfpageometry%type,dim1,dum1,status)
   write(*,*) 'Get cpo%pfpassive%pfpageometry%type'
endif 

! Get pfpassive/pfpageometry/npoints        
call get_dimension(idx,path, "pfpassive/pfpageometry/npoints",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfpassive%pfpageometry%npoints(dim1))
   call get_vect1d_int(idx,path,"pfpassive/pfpageometry/npoints",cpo%pfpassive%pfpageometry%npoints,dim1,dum1,status)
   write(*,*) 'Get cpo%pfpassive%pfpageometry%npoints'
endif 

! Get pfpassive/pfpageometry/rzcoordinate/r        
call get_dimension(idx,path, "pfpassive/pfpageometry/rzcoordinate/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfpassive%pfpageometry%rzcoordinate%r(dim1,dim2))
   call get_vect2d_double(idx,path,"pfpassive/pfpageometry/rzcoordinate/r", &
   cpo%pfpassive%pfpageometry%rzcoordinate%r, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%pfpassive%pfpageometry%rzcoordinate%r'
endif 

! Get pfpassive/pfpageometry/rzcoordinate/z        
call get_dimension(idx,path, "pfpassive/pfpageometry/rzcoordinate/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfpassive%pfpageometry%rzcoordinate%z(dim1,dim2))
   call get_vect2d_double(idx,path,"pfpassive/pfpageometry/rzcoordinate/z", &
   cpo%pfpassive%pfpageometry%rzcoordinate%z, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%pfpassive%pfpageometry%rzcoordinate%z'
endif 

! Get pfpassive/pfpageometry/rzdrdz        
call get_dimension(idx,path, "pfpassive/pfpageometry/rzdrdz",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfpassive%pfpageometry%rzdrdz(dim1,dim2))
   call get_vect2d_double(idx,path,"pfpassive/pfpageometry/rzdrdz", &
   cpo%pfpassive%pfpageometry%rzdrdz, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%pfpassive%pfpageometry%rzdrdz'
endif 

! Get pfcircuits/name  
call get_dimension(idx,path, "pfcircuits/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcircuits%name(dim1))
   call get_Vect1d_string(idx,path, "pfcircuits/name", &
                        cpo%pfcircuits%name,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcircuits%name'
endif   

! Get pfcircuits/id  
call get_dimension(idx,path, "pfcircuits/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcircuits%id(dim1))
   call get_Vect1d_string(idx,path, "pfcircuits/id", &
                        cpo%pfcircuits%id,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcircuits%id'
endif   

! Get pfcircuits/type  
call get_dimension(idx,path, "pfcircuits/type",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcircuits%type(dim1))
   call get_Vect1d_string(idx,path, "pfcircuits/type", &
                        cpo%pfcircuits%type,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcircuits%type'
endif   

! Get pfcircuits/nnodes        
call get_dimension(idx,path, "pfcircuits/nnodes",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfcircuits%nnodes(dim1))
   call get_vect1d_int(idx,path,"pfcircuits/nnodes",cpo%pfcircuits%nnodes,dim1,dum1,status)
   write(*,*) 'Get cpo%pfcircuits%nnodes'
endif 

 ! Get pfcircuits/connections : PROBLEM : UNIDENTIFIED TYPE !!! 
! Get pfsupplies/desc_supply/name  
call get_dimension(idx,path, "pfsupplies/desc_supply/name",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%name(dim1))
   call get_Vect1d_string(idx,path, "pfsupplies/desc_supply/name", &
                        cpo%pfsupplies%desc_supply%name,dim1,dum1,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%name'
endif   

! Get pfsupplies/desc_supply/id  
call get_dimension(idx,path, "pfsupplies/desc_supply/id",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%id(dim1))
   call get_Vect1d_string(idx,path, "pfsupplies/desc_supply/id", &
                        cpo%pfsupplies%desc_supply%id,dim1,dum1,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%id'
endif   

! Get pfsupplies/desc_supply/type  
call get_dimension(idx,path, "pfsupplies/desc_supply/type",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%type(dim1))
   call get_Vect1d_string(idx,path, "pfsupplies/desc_supply/type", &
                        cpo%pfsupplies%desc_supply%type,dim1,dum1,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%type'
endif   

! Get pfsupplies/desc_supply/delay
call get_dimension(idx,path, "pfsupplies/desc_supply/delay",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%delay(dim1))
   call get_vect1d_double(idx,path,"pfsupplies/desc_supply/delay", &
   cpo%pfsupplies%desc_supply%delay,dim1,dum1,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%delay'
endif        

! Get pfsupplies/desc_supply/filter/num        
call get_dimension(idx,path, "pfsupplies/desc_supply/filter/num",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%filter%num(dim1,dim2))
   call get_vect2d_double(idx,path,"pfsupplies/desc_supply/filter/num", &
   cpo%pfsupplies%desc_supply%filter%num, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%filter%num'
endif 

! Get pfsupplies/desc_supply/filter/den        
call get_dimension(idx,path, "pfsupplies/desc_supply/filter/den",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%filter%den(dim1,dim2))
   call get_vect2d_double(idx,path,"pfsupplies/desc_supply/filter/den", &
   cpo%pfsupplies%desc_supply%filter%den, &
   dim1,dim2,dum1,dum2,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%filter%den'
endif 

! Get pfsupplies/desc_supply/imin
call get_dimension(idx,path, "pfsupplies/desc_supply/imin",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%imin(dim1))
   call get_vect1d_double(idx,path,"pfsupplies/desc_supply/imin", &
   cpo%pfsupplies%desc_supply%imin,dim1,dum1,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%imin'
endif        

! Get pfsupplies/desc_supply/imax
call get_dimension(idx,path, "pfsupplies/desc_supply/imax",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%imax(dim1))
   call get_vect1d_double(idx,path,"pfsupplies/desc_supply/imax", &
   cpo%pfsupplies%desc_supply%imax,dim1,dum1,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%imax'
endif        

! Get pfsupplies/desc_supply/res
call get_dimension(idx,path, "pfsupplies/desc_supply/res",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%res(dim1))
   call get_vect1d_double(idx,path,"pfsupplies/desc_supply/res", &
   cpo%pfsupplies%desc_supply%res,dim1,dum1,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%res'
endif        

! Get pfsupplies/desc_supply/umin
call get_dimension(idx,path, "pfsupplies/desc_supply/umin",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%umin(dim1))
   call get_vect1d_double(idx,path,"pfsupplies/desc_supply/umin", &
   cpo%pfsupplies%desc_supply%umin,dim1,dum1,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%umin'
endif        

! Get pfsupplies/desc_supply/umax
call get_dimension(idx,path, "pfsupplies/desc_supply/umax",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%umax(dim1))
   call get_vect1d_double(idx,path,"pfsupplies/desc_supply/umax", &
   cpo%pfsupplies%desc_supply%umax,dim1,dum1,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%umax'
endif        

! Get pfsupplies/desc_supply/emax
call get_dimension(idx,path, "pfsupplies/desc_supply/emax",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%desc_supply%emax(dim1))
   call get_vect1d_double(idx,path,"pfsupplies/desc_supply/emax", &
   cpo%pfsupplies%desc_supply%emax,dim1,dum1,status)
   write(*,*) 'Get cpo%pfsupplies%desc_supply%emax'
endif        

! Get pfsupplies/voltage/value
call get_dimension(idx,path, "pfsupplies/voltage/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%voltage%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfsupplies/voltage/value", &
   cpo%pfsupplies%voltage%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfsupplies%voltage%value'
endif

! Get pfsupplies/voltage/abserror
call get_dimension(idx,path, "pfsupplies/voltage/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%voltage%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfsupplies/voltage/abserror", &
   cpo%pfsupplies%voltage%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfsupplies%voltage%abserror'
endif

! Get pfsupplies/voltage/relerror
call get_dimension(idx,path, "pfsupplies/voltage/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%voltage%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfsupplies/voltage/relerror", &
   cpo%pfsupplies%voltage%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfsupplies%voltage%relerror'
endif

! Get pfsupplies/current/value
call get_dimension(idx,path, "pfsupplies/current/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%current%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfsupplies/current/value", &
   cpo%pfsupplies%current%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfsupplies%current%value'
endif

! Get pfsupplies/current/abserror
call get_dimension(idx,path, "pfsupplies/current/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%current%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfsupplies/current/abserror", &
   cpo%pfsupplies%current%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfsupplies%current%abserror'
endif

! Get pfsupplies/current/relerror
call get_dimension(idx,path, "pfsupplies/current/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%pfsupplies%current%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "pfsupplies/current/relerror", &
   cpo%pfsupplies%current%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%pfsupplies%current%relerror'
endif

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_polardiag(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_lineintegraldiag) :: cpo  
	
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get expression  
longstring = ' '
call get_string(idx,path, "expression",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%expression(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%expression = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%expression(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%expression'
endif   

! Get setup_line/pivot_point/r
call get_dimension(idx,path, "setup_line/pivot_point/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%pivot_point%r(dim1))
   call get_vect1d_double(idx,path,"setup_line/pivot_point/r", &
   cpo%setup_line%pivot_point%r,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%pivot_point%r'
endif        

! Get setup_line/pivot_point/z
call get_dimension(idx,path, "setup_line/pivot_point/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%pivot_point%z(dim1))
   call get_vect1d_double(idx,path,"setup_line/pivot_point/z", &
   cpo%setup_line%pivot_point%z,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%pivot_point%z'
endif        

! Get setup_line/pivot_point/phi
call get_dimension(idx,path, "setup_line/pivot_point/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%pivot_point%phi(dim1))
   call get_vect1d_double(idx,path,"setup_line/pivot_point/phi", &
   cpo%setup_line%pivot_point%phi,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%pivot_point%phi'
endif        

! Get setup_line/polchordang
call get_dimension(idx,path, "setup_line/polchordang",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%polchordang(dim1))
   call get_vect1d_double(idx,path,"setup_line/polchordang", &
   cpo%setup_line%polchordang,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%polchordang'
endif        

! Get setup_line/torchordang
call get_dimension(idx,path, "setup_line/torchordang",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%torchordang(dim1))
   call get_vect1d_double(idx,path,"setup_line/torchordang", &
   cpo%setup_line%torchordang,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%torchordang'
endif        

! Get setup_line/second_point/r
call get_dimension(idx,path, "setup_line/second_point/r",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%second_point%r(dim1))
   call get_vect1d_double(idx,path,"setup_line/second_point/r", &
   cpo%setup_line%second_point%r,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%second_point%r'
endif        

! Get setup_line/second_point/z
call get_dimension(idx,path, "setup_line/second_point/z",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%second_point%z(dim1))
   call get_vect1d_double(idx,path,"setup_line/second_point/z", &
   cpo%setup_line%second_point%z,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%second_point%z'
endif        

! Get setup_line/second_point/phi
call get_dimension(idx,path, "setup_line/second_point/phi",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%setup_line%second_point%phi(dim1))
   call get_vect1d_double(idx,path,"setup_line/second_point/phi", &
   cpo%setup_line%second_point%phi,dim1,dum1,status)
   write(*,*) 'Get cpo%setup_line%second_point%phi'
endif        

! Get setup_line/nchordpoints 
call get_int(idx,path, "setup_line/nchordpoints",int0d,status)
if (status.EQ.0) then
   cpo%setup_line%nchordpoints = int0d
   write(*,*) 'Get cpo%setup_line%nchordpoints'
endif                 

! Get measure/value
call get_dimension(idx,path, "measure/value",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%measure%value(dim1))         
   call get_vect1d_double_Slice(idx,path, "measure/value", &
   cpo%measure%value, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%measure%value'
endif

! Get measure/abserror
call get_dimension(idx,path, "measure/abserror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%measure%abserror(dim1))         
   call get_vect1d_double_Slice(idx,path, "measure/abserror", &
   cpo%measure%abserror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%measure%abserror'
endif

! Get measure/relerror
call get_dimension(idx,path, "measure/relerror",ndims,dim1,dim2,dim3,dim4)
if (dim1.GT.0) then
   allocate(cpo%measure%relerror(dim1))         
   call get_vect1d_double_Slice(idx,path, "measure/relerror", &
   cpo%measure%relerror, &
   dim1,dum1,twant,tret,interpol,status)        
   write(*,*) 'Get cpo%measure%relerror'
endif

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_toroidfield(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_toroidfield) :: cpo      
    
call begin_CPO_Get_Slice(idx,path, twant,status)
if (status.EQ.0) then
	      
! Get datainfo/dataprovider  
longstring = ' '
call get_string(idx,path, "datainfo/dataprovider",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%dataprovider(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%dataprovider = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%dataprovider(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%dataprovider'
endif   

! Get datainfo/putdate  
longstring = ' '
call get_string(idx,path, "datainfo/putdate",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putdate(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putdate = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putdate(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putdate'
endif   

! Get datainfo/source  
longstring = ' '
call get_string(idx,path, "datainfo/source",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%source(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%source = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%source(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%source'
endif   

! Get datainfo/comment  
longstring = ' '
call get_string(idx,path, "datainfo/comment",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%comment(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%comment = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%comment(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%comment'
endif   

! Get datainfo/isref 
call get_int(idx,path, "datainfo/isref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%isref = int0d
   write(*,*) 'Get cpo%datainfo%isref'
endif                 

! Get datainfo/whatref 
call get_int(idx,path, "datainfo/whatref",int0d,status)
if (status.EQ.0) then
   cpo%datainfo%whatref = int0d
   write(*,*) 'Get cpo%datainfo%whatref'
endif                 

! Get datainfo/putinfo/putmethod  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putmethod",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putmethod(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putmethod = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putmethod(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putmethod'
endif   

! Get datainfo/putinfo/putaccess  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putaccess",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putaccess(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putaccess = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putaccess(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putaccess'
endif   

! Get datainfo/putinfo/putlocation  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/putlocation",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%putlocation(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%putlocation = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%putlocation(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%putlocation'
endif   

! Get datainfo/putinfo/rights  
longstring = ' '
call get_string(idx,path, "datainfo/putinfo/rights",longstring,status)
if (status.EQ.0) then
   lenstring = len_trim(longstring)      
   allocate(cpo%datainfo%putinfo%rights(floor(real(lenstring/132))+1)) 
   if (lenstring <= 132) then             
      cpo%datainfo%putinfo%rights = trim(longstring)
   else
      do istring=1,floor(real(lenstring/132))+1
          cpo%datainfo%putinfo%rights(istring) = trim(longstring(1+(istring-1)*132 : istring*132)) 
      enddo
   endif
   write(*,*) 'Get cpo%datainfo%putinfo%rights'
endif   

! Get nturns 
call get_int(idx,path, "nturns",int0d,status)
if (status.EQ.0) then
   cpo%nturns = int0d
   write(*,*) 'Get cpo%nturns'
endif                 

! Get ncoils 
call get_int(idx,path, "ncoils",int0d,status)
if (status.EQ.0) then
   cpo%ncoils = int0d
   write(*,*) 'Get cpo%ncoils'
endif                 

! Get current/value
call get_double_Slice(idx,path, "current/value",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%current%value = double0d
   write(*,*) 'Get cpo%current%value'
endif

! Get current/abserror
call get_double_Slice(idx,path, "current/abserror",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%current%abserror = double0d
   write(*,*) 'Get cpo%current%abserror'
endif

! Get current/relerror
call get_double_Slice(idx,path, "current/relerror",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%current%relerror = double0d
   write(*,*) 'Get cpo%current%relerror'
endif

! Get bvac_r/value
call get_double_Slice(idx,path, "bvac_r/value",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%bvac_r%value = double0d
   write(*,*) 'Get cpo%bvac_r%value'
endif

! Get bvac_r/abserror
call get_double_Slice(idx,path, "bvac_r/abserror",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%bvac_r%abserror = double0d
   write(*,*) 'Get cpo%bvac_r%abserror'
endif

! Get bvac_r/relerror
call get_double_Slice(idx,path, "bvac_r/relerror",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%bvac_r%relerror = double0d
   write(*,*) 'Get cpo%bvac_r%relerror'
endif

! Get time
call get_double_Slice(idx,path, "time",double0d, twant,tret,interpol,status)        
if (status.EQ.0) then
   cpo%time = double0d
   write(*,*) 'Get cpo%time'
endif

else
   write(*,*) 'Get slice impossible, CPO is missing or requested time slice is not within the time interval of the CPO'
endif
call end_CPO_Get_Slice(idx,path)	      
	

return
endsubroutine

subroutine euITM_get_slice_vessel(idx,path,  cpo, twant, interpol)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: status, interpol, idx, lenstring, istring
real(DP) :: twant,tret

integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: ndims,dim1,dim2,dim3,dim4,dum1,dum2,dum3,dum4
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stringans
character(len=100000)::longstring 


type(type_vessel) :: cpo      
    
write(*,*) 'Warning : GET_SLICE requested for a time-independent CPO is equivalent to a simple GET'
call euITM_get_vessel(idx,path,  cpo)
	

return
endsubroutine
 

!!!!!! Routines to PUT the full CPOs (including the various time indices if time-dependent)

subroutine euITM_put_topinfo(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_topinfo) :: cpo       
call begin_cpo_put_non_timed(idx, path)

! Put dataprovider
if (associated(cpo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%dataprovider',cpo%dataprovider
endif

! Put description
if (associated(cpo%description)) then
   longstring = ' '    
   lenstring = size(cpo%description)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%description(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%description(istring)
      enddo
   endif
   call put_string(idx,path, "description",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%description',cpo%description
endif

! Put firstputdate
if (associated(cpo%firstputdate)) then
   longstring = ' '    
   lenstring = size(cpo%firstputdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%firstputdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%firstputdate(istring)
      enddo
   endif
   call put_string(idx,path, "firstputdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%firstputdate',cpo%firstputdate
endif

! Put lastupdate
if (associated(cpo%lastupdate)) then
   longstring = ' '    
   lenstring = size(cpo%lastupdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%lastupdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%lastupdate(istring)
      enddo
   endif
   call put_string(idx,path, "lastupdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%lastupdate',cpo%lastupdate
endif

! Put source
if (associated(cpo%source)) then
   longstring = ' '    
   lenstring = size(cpo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%source(istring)
      enddo
   endif
   call put_string(idx,path, "source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%source',cpo%source
endif

! Put comment
if (associated(cpo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%comment',cpo%comment
endif

! Put dataversion
if (associated(cpo%dataversion)) then
   longstring = ' '    
   lenstring = size(cpo%dataversion)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%dataversion(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%dataversion(istring)
      enddo
   endif
   call put_string(idx,path, "dataversion",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%dataversion',cpo%dataversion
endif

! Put workflow
if (associated(cpo%workflow)) then
   longstring = ' '    
   lenstring = size(cpo%workflow)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%workflow(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%workflow(istring)
      enddo
   endif
   call put_string(idx,path, "workflow",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%workflow',cpo%workflow
endif

! Put user
if (associated(cpo%user)) then
   longstring = ' '    
   lenstring = size(cpo%user)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%user(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%user(istring)
      enddo
   endif
   call put_string(idx,path, "user",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%user',cpo%user
endif

! Put shot        
if (cpo%shot.NE.-999999999) then
   call put_int(idx,path, "shot",cpo%shot)        
   write(*,*) 'Put cpo%shot',cpo%shot
endif

! Put entry        
if (cpo%entry.NE.-999999999) then
   call put_int(idx,path, "entry",cpo%entry)        
   write(*,*) 'Put cpo%entry',cpo%entry
endif

! Put machine
if (associated(cpo%machine)) then
   longstring = ' '    
   lenstring = size(cpo%machine)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%machine(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%machine(istring)
      enddo
   endif
   call put_string(idx,path, "machine",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%machine',cpo%machine
endif

! Put treename
if (associated(cpo%treename)) then
   longstring = ' '    
   lenstring = size(cpo%treename)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%treename(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%treename(istring)
      enddo
   endif
   call put_string(idx,path, "treename",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%treename',cpo%treename
endif

! Put ref_user
if (associated(cpo%ref_user)) then
   longstring = ' '    
   lenstring = size(cpo%ref_user)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%ref_user(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%ref_user(istring)
      enddo
   endif
   call put_string(idx,path, "ref_user",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%ref_user',cpo%ref_user
endif

! Put ref_shot        
if (cpo%ref_shot.NE.-999999999) then
   call put_int(idx,path, "ref_shot",cpo%ref_shot)        
   write(*,*) 'Put cpo%ref_shot',cpo%ref_shot
endif

! Put ref_entry        
if (cpo%ref_entry.NE.-999999999) then
   call put_int(idx,path, "ref_entry",cpo%ref_entry)        
   write(*,*) 'Put cpo%ref_entry',cpo%ref_entry
endif

! Put children 
if (associated(cpo%children)) then         
   call put_vect1d_int(idx,path, "children",cpo%children,&
   size(cpo%children),0) 
   write(*,*) 'Put cpo%children',cpo%children
endif

! Put mdinfo/shot_min        
if (cpo%mdinfo%shot_min.NE.-999999999) then
   call put_int(idx,path, "mdinfo/shot_min",cpo%mdinfo%shot_min)        
   write(*,*) 'Put cpo%mdinfo%shot_min',cpo%mdinfo%shot_min
endif

! Put mdinfo/shot_max        
if (cpo%mdinfo%shot_max.NE.-999999999) then
   call put_int(idx,path, "mdinfo/shot_max",cpo%mdinfo%shot_max)        
   write(*,*) 'Put cpo%mdinfo%shot_max',cpo%mdinfo%shot_max
endif

! Put mdinfo/md_entry        
if (cpo%mdinfo%md_entry.NE.-999999999) then
   call put_int(idx,path, "mdinfo/md_entry",cpo%mdinfo%md_entry)        
   write(*,*) 'Put cpo%mdinfo%md_entry',cpo%mdinfo%md_entry
endif

call end_cpo_put_non_timed(idx, path)

return
endsubroutine

subroutine euITM_put_summary(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_summary) :: cpo       
call begin_cpo_put_non_timed(idx, path)

! Put ip/value
if (cpo%ip%value.NE.-9.D40) then 
   call put_double(idx,path, "ip/value",cpo%ip%value)  
   write(*,*) 'Put cpo%ip%value',cpo%ip%value
endif

! Put ip/source
if (associated(cpo%ip%source)) then
   longstring = ' '    
   lenstring = size(cpo%ip%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%ip%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%ip%source(istring)
      enddo
   endif
   call put_string(idx,path, "ip/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%ip%source',cpo%ip%source
endif

! Put ip/time
if (cpo%ip%time.NE.-9.D40) then 
   call put_double(idx,path, "ip/time",cpo%ip%time)  
   write(*,*) 'Put cpo%ip%time',cpo%ip%time
endif

! Put bvac_r/value
if (cpo%bvac_r%value.NE.-9.D40) then 
   call put_double(idx,path, "bvac_r/value",cpo%bvac_r%value)  
   write(*,*) 'Put cpo%bvac_r%value',cpo%bvac_r%value
endif

! Put bvac_r/source
if (associated(cpo%bvac_r%source)) then
   longstring = ' '    
   lenstring = size(cpo%bvac_r%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%bvac_r%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%bvac_r%source(istring)
      enddo
   endif
   call put_string(idx,path, "bvac_r/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%bvac_r%source',cpo%bvac_r%source
endif

! Put bvac_r/time
if (cpo%bvac_r%time.NE.-9.D40) then 
   call put_double(idx,path, "bvac_r/time",cpo%bvac_r%time)  
   write(*,*) 'Put cpo%bvac_r%time',cpo%bvac_r%time
endif

! Put geom_axis_r/value
if (cpo%geom_axis_r%value.NE.-9.D40) then 
   call put_double(idx,path, "geom_axis_r/value",cpo%geom_axis_r%value)  
   write(*,*) 'Put cpo%geom_axis_r%value',cpo%geom_axis_r%value
endif

! Put geom_axis_r/source
if (associated(cpo%geom_axis_r%source)) then
   longstring = ' '    
   lenstring = size(cpo%geom_axis_r%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%geom_axis_r%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%geom_axis_r%source(istring)
      enddo
   endif
   call put_string(idx,path, "geom_axis_r/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%geom_axis_r%source',cpo%geom_axis_r%source
endif

! Put geom_axis_r/time
if (cpo%geom_axis_r%time.NE.-9.D40) then 
   call put_double(idx,path, "geom_axis_r/time",cpo%geom_axis_r%time)  
   write(*,*) 'Put cpo%geom_axis_r%time',cpo%geom_axis_r%time
endif

! Put a_minor/value
if (cpo%a_minor%value.NE.-9.D40) then 
   call put_double(idx,path, "a_minor/value",cpo%a_minor%value)  
   write(*,*) 'Put cpo%a_minor%value',cpo%a_minor%value
endif

! Put a_minor/source
if (associated(cpo%a_minor%source)) then
   longstring = ' '    
   lenstring = size(cpo%a_minor%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%a_minor%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%a_minor%source(istring)
      enddo
   endif
   call put_string(idx,path, "a_minor/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%a_minor%source',cpo%a_minor%source
endif

! Put a_minor/time
if (cpo%a_minor%time.NE.-9.D40) then 
   call put_double(idx,path, "a_minor/time",cpo%a_minor%time)  
   write(*,*) 'Put cpo%a_minor%time',cpo%a_minor%time
endif

! Put elongation/value
if (cpo%elongation%value.NE.-9.D40) then 
   call put_double(idx,path, "elongation/value",cpo%elongation%value)  
   write(*,*) 'Put cpo%elongation%value',cpo%elongation%value
endif

! Put elongation/source
if (associated(cpo%elongation%source)) then
   longstring = ' '    
   lenstring = size(cpo%elongation%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%elongation%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%elongation%source(istring)
      enddo
   endif
   call put_string(idx,path, "elongation/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%elongation%source',cpo%elongation%source
endif

! Put elongation/time
if (cpo%elongation%time.NE.-9.D40) then 
   call put_double(idx,path, "elongation/time",cpo%elongation%time)  
   write(*,*) 'Put cpo%elongation%time',cpo%elongation%time
endif

! Put tria_lower/value
if (cpo%tria_lower%value.NE.-9.D40) then 
   call put_double(idx,path, "tria_lower/value",cpo%tria_lower%value)  
   write(*,*) 'Put cpo%tria_lower%value',cpo%tria_lower%value
endif

! Put tria_lower/source
if (associated(cpo%tria_lower%source)) then
   longstring = ' '    
   lenstring = size(cpo%tria_lower%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%tria_lower%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%tria_lower%source(istring)
      enddo
   endif
   call put_string(idx,path, "tria_lower/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%tria_lower%source',cpo%tria_lower%source
endif

! Put tria_lower/time
if (cpo%tria_lower%time.NE.-9.D40) then 
   call put_double(idx,path, "tria_lower/time",cpo%tria_lower%time)  
   write(*,*) 'Put cpo%tria_lower%time',cpo%tria_lower%time
endif

! Put tria_upper/value
if (cpo%tria_upper%value.NE.-9.D40) then 
   call put_double(idx,path, "tria_upper/value",cpo%tria_upper%value)  
   write(*,*) 'Put cpo%tria_upper%value',cpo%tria_upper%value
endif

! Put tria_upper/source
if (associated(cpo%tria_upper%source)) then
   longstring = ' '    
   lenstring = size(cpo%tria_upper%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%tria_upper%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%tria_upper%source(istring)
      enddo
   endif
   call put_string(idx,path, "tria_upper/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%tria_upper%source',cpo%tria_upper%source
endif

! Put tria_upper/time
if (cpo%tria_upper%time.NE.-9.D40) then 
   call put_double(idx,path, "tria_upper/time",cpo%tria_upper%time)  
   write(*,*) 'Put cpo%tria_upper%time',cpo%tria_upper%time
endif

! Put tev/value
if (cpo%tev%value.NE.-9.D40) then 
   call put_double(idx,path, "tev/value",cpo%tev%value)  
   write(*,*) 'Put cpo%tev%value',cpo%tev%value
endif

! Put tev/source
if (associated(cpo%tev%source)) then
   longstring = ' '    
   lenstring = size(cpo%tev%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%tev%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%tev%source(istring)
      enddo
   endif
   call put_string(idx,path, "tev/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%tev%source',cpo%tev%source
endif

! Put tev/time
if (cpo%tev%time.NE.-9.D40) then 
   call put_double(idx,path, "tev/time",cpo%tev%time)  
   write(*,*) 'Put cpo%tev%time',cpo%tev%time
endif

! Put tiv/value
if (cpo%tiv%value.NE.-9.D40) then 
   call put_double(idx,path, "tiv/value",cpo%tiv%value)  
   write(*,*) 'Put cpo%tiv%value',cpo%tiv%value
endif

! Put tiv/source
if (associated(cpo%tiv%source)) then
   longstring = ' '    
   lenstring = size(cpo%tiv%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%tiv%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%tiv%source(istring)
      enddo
   endif
   call put_string(idx,path, "tiv/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%tiv%source',cpo%tiv%source
endif

! Put tiv/time
if (cpo%tiv%time.NE.-9.D40) then 
   call put_double(idx,path, "tiv/time",cpo%tiv%time)  
   write(*,*) 'Put cpo%tiv%time',cpo%tiv%time
endif

! Put nev/value
if (cpo%nev%value.NE.-9.D40) then 
   call put_double(idx,path, "nev/value",cpo%nev%value)  
   write(*,*) 'Put cpo%nev%value',cpo%nev%value
endif

! Put nev/source
if (associated(cpo%nev%source)) then
   longstring = ' '    
   lenstring = size(cpo%nev%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%nev%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%nev%source(istring)
      enddo
   endif
   call put_string(idx,path, "nev/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%nev%source',cpo%nev%source
endif

! Put nev/time
if (cpo%nev%time.NE.-9.D40) then 
   call put_double(idx,path, "nev/time",cpo%nev%time)  
   write(*,*) 'Put cpo%nev%time',cpo%nev%time
endif

! Put zeffv/value
if (cpo%zeffv%value.NE.-9.D40) then 
   call put_double(idx,path, "zeffv/value",cpo%zeffv%value)  
   write(*,*) 'Put cpo%zeffv%value',cpo%zeffv%value
endif

! Put zeffv/source
if (associated(cpo%zeffv%source)) then
   longstring = ' '    
   lenstring = size(cpo%zeffv%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%zeffv%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%zeffv%source(istring)
      enddo
   endif
   call put_string(idx,path, "zeffv/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%zeffv%source',cpo%zeffv%source
endif

! Put zeffv/time
if (cpo%zeffv%time.NE.-9.D40) then 
   call put_double(idx,path, "zeffv/time",cpo%zeffv%time)  
   write(*,*) 'Put cpo%zeffv%time',cpo%zeffv%time
endif

! Put beta_pol/value
if (cpo%beta_pol%value.NE.-9.D40) then 
   call put_double(idx,path, "beta_pol/value",cpo%beta_pol%value)  
   write(*,*) 'Put cpo%beta_pol%value',cpo%beta_pol%value
endif

! Put beta_pol/source
if (associated(cpo%beta_pol%source)) then
   longstring = ' '    
   lenstring = size(cpo%beta_pol%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%beta_pol%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%beta_pol%source(istring)
      enddo
   endif
   call put_string(idx,path, "beta_pol/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%beta_pol%source',cpo%beta_pol%source
endif

! Put beta_pol/time
if (cpo%beta_pol%time.NE.-9.D40) then 
   call put_double(idx,path, "beta_pol/time",cpo%beta_pol%time)  
   write(*,*) 'Put cpo%beta_pol%time',cpo%beta_pol%time
endif

! Put beta_tor/value
if (cpo%beta_tor%value.NE.-9.D40) then 
   call put_double(idx,path, "beta_tor/value",cpo%beta_tor%value)  
   write(*,*) 'Put cpo%beta_tor%value',cpo%beta_tor%value
endif

! Put beta_tor/source
if (associated(cpo%beta_tor%source)) then
   longstring = ' '    
   lenstring = size(cpo%beta_tor%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%beta_tor%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%beta_tor%source(istring)
      enddo
   endif
   call put_string(idx,path, "beta_tor/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%beta_tor%source',cpo%beta_tor%source
endif

! Put beta_tor/time
if (cpo%beta_tor%time.NE.-9.D40) then 
   call put_double(idx,path, "beta_tor/time",cpo%beta_tor%time)  
   write(*,*) 'Put cpo%beta_tor%time',cpo%beta_tor%time
endif

! Put beta_normal/value
if (cpo%beta_normal%value.NE.-9.D40) then 
   call put_double(idx,path, "beta_normal/value",cpo%beta_normal%value)  
   write(*,*) 'Put cpo%beta_normal%value',cpo%beta_normal%value
endif

! Put beta_normal/source
if (associated(cpo%beta_normal%source)) then
   longstring = ' '    
   lenstring = size(cpo%beta_normal%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%beta_normal%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%beta_normal%source(istring)
      enddo
   endif
   call put_string(idx,path, "beta_normal/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%beta_normal%source',cpo%beta_normal%source
endif

! Put beta_normal/time
if (cpo%beta_normal%time.NE.-9.D40) then 
   call put_double(idx,path, "beta_normal/time",cpo%beta_normal%time)  
   write(*,*) 'Put cpo%beta_normal%time',cpo%beta_normal%time
endif

! Put li/value
if (cpo%li%value.NE.-9.D40) then 
   call put_double(idx,path, "li/value",cpo%li%value)  
   write(*,*) 'Put cpo%li%value',cpo%li%value
endif

! Put li/source
if (associated(cpo%li%source)) then
   longstring = ' '    
   lenstring = size(cpo%li%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%li%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%li%source(istring)
      enddo
   endif
   call put_string(idx,path, "li/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%li%source',cpo%li%source
endif

! Put li/time
if (cpo%li%time.NE.-9.D40) then 
   call put_double(idx,path, "li/time",cpo%li%time)  
   write(*,*) 'Put cpo%li%time',cpo%li%time
endif

! Put volume/value
if (cpo%volume%value.NE.-9.D40) then 
   call put_double(idx,path, "volume/value",cpo%volume%value)  
   write(*,*) 'Put cpo%volume%value',cpo%volume%value
endif

! Put volume/source
if (associated(cpo%volume%source)) then
   longstring = ' '    
   lenstring = size(cpo%volume%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%volume%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%volume%source(istring)
      enddo
   endif
   call put_string(idx,path, "volume/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%volume%source',cpo%volume%source
endif

! Put volume/time
if (cpo%volume%time.NE.-9.D40) then 
   call put_double(idx,path, "volume/time",cpo%volume%time)  
   write(*,*) 'Put cpo%volume%time',cpo%volume%time
endif

! Put area/value
if (cpo%area%value.NE.-9.D40) then 
   call put_double(idx,path, "area/value",cpo%area%value)  
   write(*,*) 'Put cpo%area%value',cpo%area%value
endif

! Put area/source
if (associated(cpo%area%source)) then
   longstring = ' '    
   lenstring = size(cpo%area%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%area%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%area%source(istring)
      enddo
   endif
   call put_string(idx,path, "area/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%area%source',cpo%area%source
endif

! Put area/time
if (cpo%area%time.NE.-9.D40) then 
   call put_double(idx,path, "area/time",cpo%area%time)  
   write(*,*) 'Put cpo%area%time',cpo%area%time
endif

! Put main_ion1_z/value
if (cpo%main_ion1_z%value.NE.-9.D40) then 
   call put_double(idx,path, "main_ion1_z/value",cpo%main_ion1_z%value)  
   write(*,*) 'Put cpo%main_ion1_z%value',cpo%main_ion1_z%value
endif

! Put main_ion1_z/source
if (associated(cpo%main_ion1_z%source)) then
   longstring = ' '    
   lenstring = size(cpo%main_ion1_z%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%main_ion1_z%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%main_ion1_z%source(istring)
      enddo
   endif
   call put_string(idx,path, "main_ion1_z/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%main_ion1_z%source',cpo%main_ion1_z%source
endif

! Put main_ion1_z/time
if (cpo%main_ion1_z%time.NE.-9.D40) then 
   call put_double(idx,path, "main_ion1_z/time",cpo%main_ion1_z%time)  
   write(*,*) 'Put cpo%main_ion1_z%time',cpo%main_ion1_z%time
endif

! Put main_ion1_a/value
if (cpo%main_ion1_a%value.NE.-9.D40) then 
   call put_double(idx,path, "main_ion1_a/value",cpo%main_ion1_a%value)  
   write(*,*) 'Put cpo%main_ion1_a%value',cpo%main_ion1_a%value
endif

! Put main_ion1_a/source
if (associated(cpo%main_ion1_a%source)) then
   longstring = ' '    
   lenstring = size(cpo%main_ion1_a%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%main_ion1_a%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%main_ion1_a%source(istring)
      enddo
   endif
   call put_string(idx,path, "main_ion1_a/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%main_ion1_a%source',cpo%main_ion1_a%source
endif

! Put main_ion1_a/time
if (cpo%main_ion1_a%time.NE.-9.D40) then 
   call put_double(idx,path, "main_ion1_a/time",cpo%main_ion1_a%time)  
   write(*,*) 'Put cpo%main_ion1_a%time',cpo%main_ion1_a%time
endif

! Put main_ion2_z/value
if (cpo%main_ion2_z%value.NE.-9.D40) then 
   call put_double(idx,path, "main_ion2_z/value",cpo%main_ion2_z%value)  
   write(*,*) 'Put cpo%main_ion2_z%value',cpo%main_ion2_z%value
endif

! Put main_ion2_z/source
if (associated(cpo%main_ion2_z%source)) then
   longstring = ' '    
   lenstring = size(cpo%main_ion2_z%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%main_ion2_z%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%main_ion2_z%source(istring)
      enddo
   endif
   call put_string(idx,path, "main_ion2_z/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%main_ion2_z%source',cpo%main_ion2_z%source
endif

! Put main_ion2_z/time
if (cpo%main_ion2_z%time.NE.-9.D40) then 
   call put_double(idx,path, "main_ion2_z/time",cpo%main_ion2_z%time)  
   write(*,*) 'Put cpo%main_ion2_z%time',cpo%main_ion2_z%time
endif

! Put main_ion2_a/value
if (cpo%main_ion2_a%value.NE.-9.D40) then 
   call put_double(idx,path, "main_ion2_a/value",cpo%main_ion2_a%value)  
   write(*,*) 'Put cpo%main_ion2_a%value',cpo%main_ion2_a%value
endif

! Put main_ion2_a/source
if (associated(cpo%main_ion2_a%source)) then
   longstring = ' '    
   lenstring = size(cpo%main_ion2_a%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%main_ion2_a%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%main_ion2_a%source(istring)
      enddo
   endif
   call put_string(idx,path, "main_ion2_a/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%main_ion2_a%source',cpo%main_ion2_a%source
endif

! Put main_ion2_a/time
if (cpo%main_ion2_a%time.NE.-9.D40) then 
   call put_double(idx,path, "main_ion2_a/time",cpo%main_ion2_a%time)  
   write(*,*) 'Put cpo%main_ion2_a%time',cpo%main_ion2_a%time
endif

! Put impur1_z/value
if (cpo%impur1_z%value.NE.-9.D40) then 
   call put_double(idx,path, "impur1_z/value",cpo%impur1_z%value)  
   write(*,*) 'Put cpo%impur1_z%value',cpo%impur1_z%value
endif

! Put impur1_z/source
if (associated(cpo%impur1_z%source)) then
   longstring = ' '    
   lenstring = size(cpo%impur1_z%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%impur1_z%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%impur1_z%source(istring)
      enddo
   endif
   call put_string(idx,path, "impur1_z/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%impur1_z%source',cpo%impur1_z%source
endif

! Put impur1_z/time
if (cpo%impur1_z%time.NE.-9.D40) then 
   call put_double(idx,path, "impur1_z/time",cpo%impur1_z%time)  
   write(*,*) 'Put cpo%impur1_z%time',cpo%impur1_z%time
endif

! Put impur1_a/value
if (cpo%impur1_a%value.NE.-9.D40) then 
   call put_double(idx,path, "impur1_a/value",cpo%impur1_a%value)  
   write(*,*) 'Put cpo%impur1_a%value',cpo%impur1_a%value
endif

! Put impur1_a/source
if (associated(cpo%impur1_a%source)) then
   longstring = ' '    
   lenstring = size(cpo%impur1_a%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%impur1_a%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%impur1_a%source(istring)
      enddo
   endif
   call put_string(idx,path, "impur1_a/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%impur1_a%source',cpo%impur1_a%source
endif

! Put impur1_a/time
if (cpo%impur1_a%time.NE.-9.D40) then 
   call put_double(idx,path, "impur1_a/time",cpo%impur1_a%time)  
   write(*,*) 'Put cpo%impur1_a%time',cpo%impur1_a%time
endif

call end_cpo_put_non_timed(idx, path)

return
endsubroutine

subroutine euITM_put_controllers(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_controllers),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! Put datainfo/dataprovider
if (associated(cpos(1)%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%datainfo%dataprovider'
endif

! Put datainfo/putdate
if (associated(cpos(1)%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putdate'
endif

! Put datainfo/source
if (associated(cpos(1)%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%datainfo%source'
endif

! Put datainfo/comment
if (associated(cpos(1)%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%datainfo%comment'
endif

! Put datainfo/isref        
if (cpos(1)%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpos(1)%datainfo%isref)        
   write(*,*) 'Put cpos%datainfo%isref'
endif

! Put datainfo/whatref        
if (cpos(1)%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpos(1)%datainfo%whatref)        
   write(*,*) 'Put cpos%datainfo%whatref'
endif

! Put datainfo/putinfo/putmethod
if (associated(cpos(1)%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putmethod'
endif

! Put datainfo/putinfo/putaccess
if (associated(cpos(1)%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putaccess'
endif

! Put datainfo/putinfo/putlocation
if (associated(cpos(1)%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putlocation'
endif

! Put datainfo/putinfo/rights
if (associated(cpos(1)%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%rights'
endif

! Put name
if (associated(cpos(1)%name)) then
   longstring = ' '    
   lenstring = size(cpos(1)%name)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%name(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%name(istring)
      enddo
   endif
   call put_string(idx,path, "name",trim(longstring))
   write(*,*) 'Put cpos%name'
endif

! Put purpose
if (associated(cpos(1)%purpose)) then
   longstring = ' '    
   lenstring = size(cpos(1)%purpose)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%purpose(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%purpose(istring)
      enddo
   endif
   call put_string(idx,path, "purpose",trim(longstring))
   write(*,*) 'Put cpos%purpose'
endif

! Put type
if (associated(cpos(1)%type)) then
   longstring = ' '    
   lenstring = size(cpos(1)%type)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%type(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%type(istring)
      enddo
   endif
   call put_string(idx,path, "type",trim(longstring))
   write(*,*) 'Put cpos%type'
endif

! Put input
if (associated(cpos(1)%input)) then
   dim1 = size(cpos(1)%input)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%input(i))
   enddo
   call put_Vect1d_String(idx,path, "input", &
         cpos(1)%input,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%input'
endif

! Put output
if (associated(cpos(1)%output)) then
   dim1 = size(cpos(1)%output)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%output(i))
   enddo
   call put_Vect1d_String(idx,path, "output", &
         cpos(1)%output,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%output'
endif

! Put statespace/observable
if (associated(cpos(1)%statespace%observable)) then
   dim1 = size(cpos(1)%statespace%observable)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%statespace%observable(i))
   enddo
   call put_Vect1d_String(idx,path, "statespace/observable", &
         cpos(1)%statespace%observable,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%statespace%observable'
endif

! Put statespace/A        
if (associated(cpos(1)%statespace%A)) then   
   call put_vect2d_double(idx,path, "statespace/A", &
   cpos(1)%statespace%A,&
   size(cpos(1)%statespace%A,1),size(cpos(1)%statespace%A,2),0)  
   write(*,*) 'Put cpos%statespace%A'
endif

! Put statespace/B        
if (associated(cpos(1)%statespace%B)) then   
   call put_vect2d_double(idx,path, "statespace/B", &
   cpos(1)%statespace%B,&
   size(cpos(1)%statespace%B,1),size(cpos(1)%statespace%B,2),0)  
   write(*,*) 'Put cpos%statespace%B'
endif

! Put statespace/C        
if (associated(cpos(1)%statespace%C)) then   
   call put_vect2d_double(idx,path, "statespace/C", &
   cpos(1)%statespace%C,&
   size(cpos(1)%statespace%C,1),size(cpos(1)%statespace%C,2),0)  
   write(*,*) 'Put cpos%statespace%C'
endif

! Put statespace/D        
if (associated(cpos(1)%statespace%D)) then   
   call put_vect2d_double(idx,path, "statespace/D", &
   cpos(1)%statespace%D,&
   size(cpos(1)%statespace%D,1),size(cpos(1)%statespace%D,2),0)  
   write(*,*) 'Put cpos%statespace%D'
endif

! Put statespace/deltat
if (cpos(1)%statespace%deltat.NE.-9.D40) then 
   call put_double(idx,path, "statespace/deltat",cpos(1)%statespace%deltat)  
   write(*,*) 'Put cpos%statespace%deltat'
endif

! put target
if (associated(cpos(1)%target)) then  
   allocate(vect2DDouble(size(cpos(1)%target,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%target
   enddo
   call put_vect2D_Double(idx,path, "target",vect2DDouble, &
   size(cpos(1)%target,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_coreprof(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_coreprof),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! put rho_tor
if (associated(cpos(1)%rho_tor)) then  
   allocate(vect2DDouble(size(cpos(1)%rho_tor,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%rho_tor
   enddo
   call put_vect2D_Double(idx,path, "rho_tor",vect2DDouble, &
   size(cpos(1)%rho_tor,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put psi
if (associated(cpos(1)%psi)) then  
   allocate(vect2DDouble(size(cpos(1)%psi,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%psi
   enddo
   call put_vect2D_Double(idx,path, "psi",vect2DDouble, &
   size(cpos(1)%psi,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pressure/value
if (associated(cpos(1)%pressure%value)) then  
   allocate(vect2DDouble(size(cpos(1)%pressure%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pressure%value
   enddo
   call put_vect2D_Double(idx,path, "pressure/value",vect2DDouble, &
   size(cpos(1)%pressure%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get pressure/source : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put jparallel/value
if (associated(cpos(1)%jparallel%value)) then  
   allocate(vect2DDouble(size(cpos(1)%jparallel%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%jparallel%value
   enddo
   call put_vect2D_Double(idx,path, "jparallel/value",vect2DDouble, &
   size(cpos(1)%jparallel%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get jparallel/source : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put q/value
if (associated(cpos(1)%q%value)) then  
   allocate(vect2DDouble(size(cpos(1)%q%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%q%value
   enddo
   call put_vect2D_Double(idx,path, "q/value",vect2DDouble, &
   size(cpos(1)%q%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get q/source : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! Put codeparam/codename
if (associated(cpos(1)%codeparam%codename)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%codename)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%codename(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%codename(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codename",trim(longstring))
   write(*,*) 'Put cpos%codeparam%codename'
endif

! Put codeparam/codeversion
if (associated(cpos(1)%codeparam%codeversion)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%codeversion)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%codeversion(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%codeversion(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codeversion",trim(longstring))
   write(*,*) 'Put cpos%codeparam%codeversion'
endif

! Put codeparam/parameters
if (associated(cpos(1)%codeparam%parameters)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%parameters)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%parameters(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%parameters(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/parameters",trim(longstring))
   write(*,*) 'Put cpos%codeparam%parameters'
endif

! Put codeparam/output_diag
if (associated(cpos(1)%codeparam%output_diag)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%output_diag)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%output_diag(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%output_diag(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/output_diag",trim(longstring))
   write(*,*) 'Put cpos%codeparam%output_diag'
endif

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_equilibrium(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_equilibrium),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! Put datainfo/dataprovider
if (associated(cpos(1)%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%datainfo%dataprovider'
endif

! Put datainfo/putdate
if (associated(cpos(1)%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putdate'
endif

! Put datainfo/source
if (associated(cpos(1)%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%datainfo%source'
endif

! Put datainfo/comment
if (associated(cpos(1)%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%datainfo%comment'
endif

! Put datainfo/isref        
if (cpos(1)%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpos(1)%datainfo%isref)        
   write(*,*) 'Put cpos%datainfo%isref'
endif

! Put datainfo/whatref        
if (cpos(1)%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpos(1)%datainfo%whatref)        
   write(*,*) 'Put cpos%datainfo%whatref'
endif

! Put datainfo/putinfo/putmethod
if (associated(cpos(1)%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putmethod'
endif

! Put datainfo/putinfo/putaccess
if (associated(cpos(1)%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putaccess'
endif

! Put datainfo/putinfo/putlocation
if (associated(cpos(1)%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putlocation'
endif

! Put datainfo/putinfo/rights
if (associated(cpos(1)%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%rights'
endif

! Put eqconstraint/bvac_r/source
if (associated(cpos(1)%eqconstraint%bvac_r%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%bvac_r%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%bvac_r%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%bvac_r%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/bvac_r/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%bvac_r%source'
endif

! Put eqconstraint/bvac_r/time  
if (any(cpos(1:lentime)%eqconstraint%bvac_r%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqconstraint%bvac_r%time    
   call put_vect1D_double(idx,path, "eqconstraint/bvac_r/time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqconstraint/bvac_r/exact        
if (cpos(1)%eqconstraint%bvac_r%exact.NE.-999999999) then
   call put_int(idx,path, "eqconstraint/bvac_r/exact",cpos(1)%eqconstraint%bvac_r%exact)        
   write(*,*) 'Put cpos%eqconstraint%bvac_r%exact'
endif

! Put eqconstraint/bvac_r/weight  
if (any(cpos(1:lentime)%eqconstraint%bvac_r%weight/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqconstraint%bvac_r%weight    
   call put_vect1D_double(idx,path, "eqconstraint/bvac_r/weight",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqconstraint/bvac_r/sigma  
if (any(cpos(1:lentime)%eqconstraint%bvac_r%sigma/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqconstraint%bvac_r%sigma    
   call put_vect1D_double(idx,path, "eqconstraint/bvac_r/sigma",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqconstraint/bvac_r/calculated  
if (any(cpos(1:lentime)%eqconstraint%bvac_r%calculated/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqconstraint%bvac_r%calculated    
   call put_vect1D_double(idx,path, "eqconstraint/bvac_r/calculated",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqconstraint/bvac_r/chi2  
if (any(cpos(1:lentime)%eqconstraint%bvac_r%chi2/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqconstraint%bvac_r%chi2    
   call put_vect1D_double(idx,path, "eqconstraint/bvac_r/chi2",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqconstraint/i_plasma/source
if (associated(cpos(1)%eqconstraint%i_plasma%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%i_plasma%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%i_plasma%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%i_plasma%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/i_plasma/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%i_plasma%source'
endif

! Put eqconstraint/i_plasma/time  
if (any(cpos(1:lentime)%eqconstraint%i_plasma%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqconstraint%i_plasma%time    
   call put_vect1D_double(idx,path, "eqconstraint/i_plasma/time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqconstraint/i_plasma/exact        
if (cpos(1)%eqconstraint%i_plasma%exact.NE.-999999999) then
   call put_int(idx,path, "eqconstraint/i_plasma/exact",cpos(1)%eqconstraint%i_plasma%exact)        
   write(*,*) 'Put cpos%eqconstraint%i_plasma%exact'
endif

! Put eqconstraint/i_plasma/weight  
if (any(cpos(1:lentime)%eqconstraint%i_plasma%weight/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqconstraint%i_plasma%weight    
   call put_vect1D_double(idx,path, "eqconstraint/i_plasma/weight",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqconstraint/i_plasma/sigma  
if (any(cpos(1:lentime)%eqconstraint%i_plasma%sigma/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqconstraint%i_plasma%sigma    
   call put_vect1D_double(idx,path, "eqconstraint/i_plasma/sigma",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqconstraint/i_plasma/calculated  
if (any(cpos(1:lentime)%eqconstraint%i_plasma%calculated/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqconstraint%i_plasma%calculated    
   call put_vect1D_double(idx,path, "eqconstraint/i_plasma/calculated",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqconstraint/i_plasma/chi2  
if (any(cpos(1:lentime)%eqconstraint%i_plasma%chi2/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqconstraint%i_plasma%chi2    
   call put_vect1D_double(idx,path, "eqconstraint/i_plasma/chi2",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqconstraint/magnet_iron/mr/source
if (associated(cpos(1)%eqconstraint%magnet_iron%mr%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%magnet_iron%mr%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%magnet_iron%mr%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%magnet_iron%mr%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/magnet_iron/mr/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%magnet_iron%mr%source'
endif

! put eqconstraint/magnet_iron/mr/time
if (associated(cpos(1)%eqconstraint%magnet_iron%mr%time)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%magnet_iron%mr%time,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%magnet_iron%mr%time
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/magnet_iron/mr/time",vect2DDouble, &
   size(cpos(1)%eqconstraint%magnet_iron%mr%time,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get eqconstraint/magnet_iron/mr/exact : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put eqconstraint/magnet_iron/mr/weight
if (associated(cpos(1)%eqconstraint%magnet_iron%mr%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%magnet_iron%mr%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%magnet_iron%mr%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/magnet_iron/mr/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%magnet_iron%mr%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/magnet_iron/mr/sigma
if (associated(cpos(1)%eqconstraint%magnet_iron%mr%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%magnet_iron%mr%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%magnet_iron%mr%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/magnet_iron/mr/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%magnet_iron%mr%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/magnet_iron/mr/calculated
if (associated(cpos(1)%eqconstraint%magnet_iron%mr%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%magnet_iron%mr%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%magnet_iron%mr%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/magnet_iron/mr/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%magnet_iron%mr%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/magnet_iron/mr/chi2
if (associated(cpos(1)%eqconstraint%magnet_iron%mr%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%magnet_iron%mr%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%magnet_iron%mr%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/magnet_iron/mr/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%magnet_iron%mr%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqconstraint/magnet_iron/mz/source
if (associated(cpos(1)%eqconstraint%magnet_iron%mz%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%magnet_iron%mz%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%magnet_iron%mz%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%magnet_iron%mz%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/magnet_iron/mz/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%magnet_iron%mz%source'
endif

! put eqconstraint/magnet_iron/mz/time
if (associated(cpos(1)%eqconstraint%magnet_iron%mz%time)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%magnet_iron%mz%time,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%magnet_iron%mz%time
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/magnet_iron/mz/time",vect2DDouble, &
   size(cpos(1)%eqconstraint%magnet_iron%mz%time,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get eqconstraint/magnet_iron/mz/exact : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put eqconstraint/magnet_iron/mz/weight
if (associated(cpos(1)%eqconstraint%magnet_iron%mz%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%magnet_iron%mz%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%magnet_iron%mz%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/magnet_iron/mz/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%magnet_iron%mz%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/magnet_iron/mz/sigma
if (associated(cpos(1)%eqconstraint%magnet_iron%mz%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%magnet_iron%mz%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%magnet_iron%mz%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/magnet_iron/mz/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%magnet_iron%mz%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/magnet_iron/mz/calculated
if (associated(cpos(1)%eqconstraint%magnet_iron%mz%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%magnet_iron%mz%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%magnet_iron%mz%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/magnet_iron/mz/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%magnet_iron%mz%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/magnet_iron/mz/chi2
if (associated(cpos(1)%eqconstraint%magnet_iron%mz%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%magnet_iron%mz%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%magnet_iron%mz%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/magnet_iron/mz/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%magnet_iron%mz%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqconstraint/bpol/source
if (associated(cpos(1)%eqconstraint%bpol%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%bpol%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%bpol%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%bpol%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/bpol/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%bpol%source'
endif

! put eqconstraint/bpol/time
if (associated(cpos(1)%eqconstraint%bpol%time)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%bpol%time,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%bpol%time
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/bpol/time",vect2DDouble, &
   size(cpos(1)%eqconstraint%bpol%time,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get eqconstraint/bpol/exact : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put eqconstraint/bpol/weight
if (associated(cpos(1)%eqconstraint%bpol%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%bpol%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%bpol%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/bpol/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%bpol%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/bpol/sigma
if (associated(cpos(1)%eqconstraint%bpol%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%bpol%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%bpol%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/bpol/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%bpol%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/bpol/calculated
if (associated(cpos(1)%eqconstraint%bpol%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%bpol%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%bpol%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/bpol/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%bpol%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/bpol/chi2
if (associated(cpos(1)%eqconstraint%bpol%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%bpol%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%bpol%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/bpol/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%bpol%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqconstraint/flux/source
if (associated(cpos(1)%eqconstraint%flux%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%flux%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%flux%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%flux%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/flux/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%flux%source'
endif

! put eqconstraint/flux/time
if (associated(cpos(1)%eqconstraint%flux%time)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%flux%time,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%flux%time
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/flux/time",vect2DDouble, &
   size(cpos(1)%eqconstraint%flux%time,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get eqconstraint/flux/exact : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put eqconstraint/flux/weight
if (associated(cpos(1)%eqconstraint%flux%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%flux%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%flux%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/flux/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%flux%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/flux/sigma
if (associated(cpos(1)%eqconstraint%flux%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%flux%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%flux%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/flux/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%flux%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/flux/calculated
if (associated(cpos(1)%eqconstraint%flux%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%flux%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%flux%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/flux/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%flux%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/flux/chi2
if (associated(cpos(1)%eqconstraint%flux%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%flux%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%flux%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/flux/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%flux%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqconstraint/mse/source
if (associated(cpos(1)%eqconstraint%mse%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%mse%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%mse%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%mse%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/mse/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%mse%source'
endif

! put eqconstraint/mse/time
if (associated(cpos(1)%eqconstraint%mse%time)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%mse%time,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%mse%time
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/mse/time",vect2DDouble, &
   size(cpos(1)%eqconstraint%mse%time,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get eqconstraint/mse/exact : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put eqconstraint/mse/weight
if (associated(cpos(1)%eqconstraint%mse%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%mse%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%mse%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/mse/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%mse%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/mse/sigma
if (associated(cpos(1)%eqconstraint%mse%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%mse%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%mse%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/mse/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%mse%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/mse/calculated
if (associated(cpos(1)%eqconstraint%mse%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%mse%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%mse%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/mse/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%mse%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/mse/chi2
if (associated(cpos(1)%eqconstraint%mse%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%mse%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%mse%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/mse/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%mse%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqconstraint/faraday/source
if (associated(cpos(1)%eqconstraint%faraday%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%faraday%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%faraday%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%faraday%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/faraday/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%faraday%source'
endif

! put eqconstraint/faraday/time
if (associated(cpos(1)%eqconstraint%faraday%time)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%faraday%time,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%faraday%time
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/faraday/time",vect2DDouble, &
   size(cpos(1)%eqconstraint%faraday%time,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get eqconstraint/faraday/exact : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put eqconstraint/faraday/weight
if (associated(cpos(1)%eqconstraint%faraday%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%faraday%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%faraday%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/faraday/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%faraday%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/faraday/sigma
if (associated(cpos(1)%eqconstraint%faraday%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%faraday%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%faraday%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/faraday/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%faraday%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/faraday/calculated
if (associated(cpos(1)%eqconstraint%faraday%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%faraday%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%faraday%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/faraday/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%faraday%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/faraday/chi2
if (associated(cpos(1)%eqconstraint%faraday%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%faraday%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%faraday%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/faraday/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%faraday%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqconstraint/pfcurrent/source
if (associated(cpos(1)%eqconstraint%pfcurrent%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%pfcurrent%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%pfcurrent%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%pfcurrent%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/pfcurrent/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%pfcurrent%source'
endif

! put eqconstraint/pfcurrent/time
if (associated(cpos(1)%eqconstraint%pfcurrent%time)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%pfcurrent%time,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%pfcurrent%time
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/pfcurrent/time",vect2DDouble, &
   size(cpos(1)%eqconstraint%pfcurrent%time,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get eqconstraint/pfcurrent/exact : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put eqconstraint/pfcurrent/weight
if (associated(cpos(1)%eqconstraint%pfcurrent%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%pfcurrent%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%pfcurrent%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/pfcurrent/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%pfcurrent%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/pfcurrent/sigma
if (associated(cpos(1)%eqconstraint%pfcurrent%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%pfcurrent%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%pfcurrent%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/pfcurrent/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%pfcurrent%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/pfcurrent/calculated
if (associated(cpos(1)%eqconstraint%pfcurrent%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%pfcurrent%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%pfcurrent%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/pfcurrent/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%pfcurrent%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/pfcurrent/chi2
if (associated(cpos(1)%eqconstraint%pfcurrent%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%pfcurrent%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%pfcurrent%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/pfcurrent/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%pfcurrent%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqconstraint/pressure/source
if (associated(cpos(1)%eqconstraint%pressure%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%pressure%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%pressure%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%pressure%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/pressure/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%pressure%source'
endif

! put eqconstraint/pressure/time
if (associated(cpos(1)%eqconstraint%pressure%time)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%pressure%time,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%pressure%time
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/pressure/time",vect2DDouble, &
   size(cpos(1)%eqconstraint%pressure%time,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get eqconstraint/pressure/exact : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put eqconstraint/pressure/weight
if (associated(cpos(1)%eqconstraint%pressure%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%pressure%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%pressure%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/pressure/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%pressure%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/pressure/sigma
if (associated(cpos(1)%eqconstraint%pressure%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%pressure%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%pressure%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/pressure/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%pressure%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/pressure/calculated
if (associated(cpos(1)%eqconstraint%pressure%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%pressure%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%pressure%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/pressure/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%pressure%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/pressure/chi2
if (associated(cpos(1)%eqconstraint%pressure%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%pressure%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%pressure%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/pressure/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%pressure%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqconstraint/jsurf/source
if (associated(cpos(1)%eqconstraint%jsurf%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqconstraint%jsurf%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqconstraint%jsurf%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqconstraint%jsurf%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/jsurf/source",trim(longstring))
   write(*,*) 'Put cpos%eqconstraint%jsurf%source'
endif

! put eqconstraint/jsurf/time
if (associated(cpos(1)%eqconstraint%jsurf%time)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%jsurf%time,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%jsurf%time
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/jsurf/time",vect2DDouble, &
   size(cpos(1)%eqconstraint%jsurf%time,1),lentime,1)           
   deallocate(vect2DDouble)
endif

 ! Get eqconstraint/jsurf/exact : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put eqconstraint/jsurf/weight
if (associated(cpos(1)%eqconstraint%jsurf%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%jsurf%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%jsurf%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/jsurf/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%jsurf%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/jsurf/sigma
if (associated(cpos(1)%eqconstraint%jsurf%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%jsurf%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%jsurf%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/jsurf/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%jsurf%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/jsurf/calculated
if (associated(cpos(1)%eqconstraint%jsurf%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%jsurf%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%jsurf%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/jsurf/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%jsurf%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/jsurf/chi2
if (associated(cpos(1)%eqconstraint%jsurf%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%jsurf%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%jsurf%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/jsurf/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%jsurf%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/q/qvalue
if (associated(cpos(1)%eqconstraint%q%qvalue)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%q%qvalue,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%q%qvalue
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/q/qvalue",vect2DDouble, &
   size(cpos(1)%eqconstraint%q%qvalue,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/q/position/r
if (associated(cpos(1)%eqconstraint%q%position%r)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%q%position%r,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%q%position%r
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/q/position/r",vect2DDouble, &
   size(cpos(1)%eqconstraint%q%position%r,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/q/position/z
if (associated(cpos(1)%eqconstraint%q%position%z)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%q%position%z,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%q%position%z
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/q/position/z",vect2DDouble, &
   size(cpos(1)%eqconstraint%q%position%z,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqconstraint/q/exact        
if (cpos(1)%eqconstraint%q%exact.NE.-999999999) then
   call put_int(idx,path, "eqconstraint/q/exact",cpos(1)%eqconstraint%q%exact)        
   write(*,*) 'Put cpos%eqconstraint%q%exact'
endif

! put eqconstraint/q/weight
if (associated(cpos(1)%eqconstraint%q%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%q%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%q%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/q/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%q%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/q/sigma
if (associated(cpos(1)%eqconstraint%q%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%q%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%q%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/q/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%q%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/q/calculated
if (associated(cpos(1)%eqconstraint%q%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%q%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%q%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/q/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%q%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/q/chi2
if (associated(cpos(1)%eqconstraint%q%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%q%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%q%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/q/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%q%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/isoflux/position/r
if (associated(cpos(1)%eqconstraint%isoflux%position%r)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%isoflux%position%r,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%isoflux%position%r
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/isoflux/position/r",vect2DDouble, &
   size(cpos(1)%eqconstraint%isoflux%position%r,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/isoflux/position/z
if (associated(cpos(1)%eqconstraint%isoflux%position%z)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%isoflux%position%z,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%isoflux%position%z
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/isoflux/position/z",vect2DDouble, &
   size(cpos(1)%eqconstraint%isoflux%position%z,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/isoflux/weight
if (associated(cpos(1)%eqconstraint%isoflux%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%isoflux%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%isoflux%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/isoflux/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%isoflux%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/isoflux/sigma
if (associated(cpos(1)%eqconstraint%isoflux%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%isoflux%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%isoflux%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/isoflux/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%isoflux%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/isoflux/calculated
if (associated(cpos(1)%eqconstraint%isoflux%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%isoflux%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%isoflux%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/isoflux/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%isoflux%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/isoflux/chi2
if (associated(cpos(1)%eqconstraint%isoflux%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%isoflux%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%isoflux%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/isoflux/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%isoflux%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/xpts/position/r
if (associated(cpos(1)%eqconstraint%xpts%position%r)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%xpts%position%r,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%xpts%position%r
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/xpts/position/r",vect2DDouble, &
   size(cpos(1)%eqconstraint%xpts%position%r,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/xpts/position/z
if (associated(cpos(1)%eqconstraint%xpts%position%z)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%xpts%position%z,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%xpts%position%z
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/xpts/position/z",vect2DDouble, &
   size(cpos(1)%eqconstraint%xpts%position%z,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/xpts/weight
if (associated(cpos(1)%eqconstraint%xpts%weight)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%xpts%weight,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%xpts%weight
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/xpts/weight",vect2DDouble, &
   size(cpos(1)%eqconstraint%xpts%weight,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/xpts/sigma
if (associated(cpos(1)%eqconstraint%xpts%sigma)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%xpts%sigma,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%xpts%sigma
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/xpts/sigma",vect2DDouble, &
   size(cpos(1)%eqconstraint%xpts%sigma,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/xpts/calculated
if (associated(cpos(1)%eqconstraint%xpts%calculated)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%xpts%calculated,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%xpts%calculated
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/xpts/calculated",vect2DDouble, &
   size(cpos(1)%eqconstraint%xpts%calculated,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqconstraint/xpts/chi2
if (associated(cpos(1)%eqconstraint%xpts%chi2)) then  
   allocate(vect2DDouble(size(cpos(1)%eqconstraint%xpts%chi2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqconstraint%xpts%chi2
   enddo
   call put_vect2D_Double(idx,path, "eqconstraint/xpts/chi2",vect2DDouble, &
   size(cpos(1)%eqconstraint%xpts%chi2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqgeometry/datainfo/dataprovider
if (associated(cpos(1)%eqgeometry%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqgeometry%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqgeometry%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqgeometry%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%eqgeometry%datainfo%dataprovider'
endif

! Put eqgeometry/datainfo/putdate
if (associated(cpos(1)%eqgeometry%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqgeometry%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqgeometry%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqgeometry%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%eqgeometry%datainfo%putdate'
endif

! Put eqgeometry/datainfo/source
if (associated(cpos(1)%eqgeometry%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqgeometry%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqgeometry%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqgeometry%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%eqgeometry%datainfo%source'
endif

! Put eqgeometry/datainfo/comment
if (associated(cpos(1)%eqgeometry%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqgeometry%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqgeometry%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqgeometry%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%eqgeometry%datainfo%comment'
endif

! Put eqgeometry/datainfo/isref        
if (cpos(1)%eqgeometry%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "eqgeometry/datainfo/isref",cpos(1)%eqgeometry%datainfo%isref)        
   write(*,*) 'Put cpos%eqgeometry%datainfo%isref'
endif

! Put eqgeometry/datainfo/whatref        
if (cpos(1)%eqgeometry%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "eqgeometry/datainfo/whatref",cpos(1)%eqgeometry%datainfo%whatref)        
   write(*,*) 'Put cpos%eqgeometry%datainfo%whatref'
endif

! Put eqgeometry/datainfo/putinfo/putmethod
if (associated(cpos(1)%eqgeometry%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqgeometry%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqgeometry%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqgeometry%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%eqgeometry%datainfo%putinfo%putmethod'
endif

! Put eqgeometry/datainfo/putinfo/putaccess
if (associated(cpos(1)%eqgeometry%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqgeometry%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqgeometry%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqgeometry%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%eqgeometry%datainfo%putinfo%putaccess'
endif

! Put eqgeometry/datainfo/putinfo/putlocation
if (associated(cpos(1)%eqgeometry%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqgeometry%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqgeometry%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqgeometry%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%eqgeometry%datainfo%putinfo%putlocation'
endif

! Put eqgeometry/datainfo/putinfo/rights
if (associated(cpos(1)%eqgeometry%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%eqgeometry%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%eqgeometry%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%eqgeometry%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%eqgeometry%datainfo%putinfo%rights'
endif

 ! Get eqgeometry/boundarytype : PROBLEM : UNIDENTIFIED TYPE !!!         
        
! put eqgeometry/boundary/r
if (associated(cpos(1)%eqgeometry%boundary%r)) then  
   allocate(vect2DDouble(size(cpos(1)%eqgeometry%boundary%r,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqgeometry%boundary%r
   enddo
   call put_vect2D_Double(idx,path, "eqgeometry/boundary/r",vect2DDouble, &
   size(cpos(1)%eqgeometry%boundary%r,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqgeometry/boundary/z
if (associated(cpos(1)%eqgeometry%boundary%z)) then  
   allocate(vect2DDouble(size(cpos(1)%eqgeometry%boundary%z,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqgeometry%boundary%z
   enddo
   call put_vect2D_Double(idx,path, "eqgeometry/boundary/z",vect2DDouble, &
   size(cpos(1)%eqgeometry%boundary%z,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqgeometry/geom_axis/r  
if (any(cpos(1:lentime)%eqgeometry%geom_axis%r/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%geom_axis%r    
   call put_vect1D_double(idx,path, "eqgeometry/geom_axis/r",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/geom_axis/z  
if (any(cpos(1:lentime)%eqgeometry%geom_axis%z/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%geom_axis%z    
   call put_vect1D_double(idx,path, "eqgeometry/geom_axis/z",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/a_minor  
if (any(cpos(1:lentime)%eqgeometry%a_minor/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%a_minor    
   call put_vect1D_double(idx,path, "eqgeometry/a_minor",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/elongation  
if (any(cpos(1:lentime)%eqgeometry%elongation/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%elongation    
   call put_vect1D_double(idx,path, "eqgeometry/elongation",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/tria_upper  
if (any(cpos(1:lentime)%eqgeometry%tria_upper/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%tria_upper    
   call put_vect1D_double(idx,path, "eqgeometry/tria_upper",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/tria_lower  
if (any(cpos(1:lentime)%eqgeometry%tria_lower/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%tria_lower    
   call put_vect1D_double(idx,path, "eqgeometry/tria_lower",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! put eqgeometry/xpts/r
if (associated(cpos(1)%eqgeometry%xpts%r)) then  
   allocate(vect2DDouble(size(cpos(1)%eqgeometry%xpts%r,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqgeometry%xpts%r
   enddo
   call put_vect2D_Double(idx,path, "eqgeometry/xpts/r",vect2DDouble, &
   size(cpos(1)%eqgeometry%xpts%r,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put eqgeometry/xpts/z
if (associated(cpos(1)%eqgeometry%xpts%z)) then  
   allocate(vect2DDouble(size(cpos(1)%eqgeometry%xpts%z,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%eqgeometry%xpts%z
   enddo
   call put_vect2D_Double(idx,path, "eqgeometry/xpts/z",vect2DDouble, &
   size(cpos(1)%eqgeometry%xpts%z,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put eqgeometry/left_low_st/r  
if (any(cpos(1:lentime)%eqgeometry%left_low_st%r/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%left_low_st%r    
   call put_vect1D_double(idx,path, "eqgeometry/left_low_st/r",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/left_low_st/z  
if (any(cpos(1:lentime)%eqgeometry%left_low_st%z/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%left_low_st%z    
   call put_vect1D_double(idx,path, "eqgeometry/left_low_st/z",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/right_low_st/r  
if (any(cpos(1:lentime)%eqgeometry%right_low_st%r/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%right_low_st%r    
   call put_vect1D_double(idx,path, "eqgeometry/right_low_st/r",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/right_low_st/z  
if (any(cpos(1:lentime)%eqgeometry%right_low_st%z/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%right_low_st%z    
   call put_vect1D_double(idx,path, "eqgeometry/right_low_st/z",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/left_up_st/r  
if (any(cpos(1:lentime)%eqgeometry%left_up_st%r/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%left_up_st%r    
   call put_vect1D_double(idx,path, "eqgeometry/left_up_st/r",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/left_up_st/z  
if (any(cpos(1:lentime)%eqgeometry%left_up_st%z/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%left_up_st%z    
   call put_vect1D_double(idx,path, "eqgeometry/left_up_st/z",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/right_up_st/r  
if (any(cpos(1:lentime)%eqgeometry%right_up_st%r/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%right_up_st%r    
   call put_vect1D_double(idx,path, "eqgeometry/right_up_st/r",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/right_up_st/z  
if (any(cpos(1:lentime)%eqgeometry%right_up_st%z/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%right_up_st%z    
   call put_vect1D_double(idx,path, "eqgeometry/right_up_st/z",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/active_limit/r  
if (any(cpos(1:lentime)%eqgeometry%active_limit%r/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%active_limit%r    
   call put_vect1D_double(idx,path, "eqgeometry/active_limit/r",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put eqgeometry/active_limit/z  
if (any(cpos(1:lentime)%eqgeometry%active_limit%z/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%eqgeometry%active_limit%z    
   call put_vect1D_double(idx,path, "eqgeometry/active_limit/z",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put flush/datainfo/dataprovider
if (associated(cpos(1)%flush%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%flush%datainfo%dataprovider'
endif

! Put flush/datainfo/putdate
if (associated(cpos(1)%flush%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%flush%datainfo%putdate'
endif

! Put flush/datainfo/source
if (associated(cpos(1)%flush%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%flush%datainfo%source'
endif

! Put flush/datainfo/comment
if (associated(cpos(1)%flush%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%flush%datainfo%comment'
endif

! Put flush/datainfo/isref        
if (cpos(1)%flush%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "flush/datainfo/isref",cpos(1)%flush%datainfo%isref)        
   write(*,*) 'Put cpos%flush%datainfo%isref'
endif

! Put flush/datainfo/whatref        
if (cpos(1)%flush%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "flush/datainfo/whatref",cpos(1)%flush%datainfo%whatref)        
   write(*,*) 'Put cpos%flush%datainfo%whatref'
endif

! Put flush/datainfo/putinfo/putmethod
if (associated(cpos(1)%flush%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%flush%datainfo%putinfo%putmethod'
endif

! Put flush/datainfo/putinfo/putaccess
if (associated(cpos(1)%flush%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%flush%datainfo%putinfo%putaccess'
endif

! Put flush/datainfo/putinfo/putlocation
if (associated(cpos(1)%flush%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%flush%datainfo%putinfo%putlocation'
endif

! Put flush/datainfo/putinfo/rights
if (associated(cpos(1)%flush%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%flush%datainfo%putinfo%rights'
endif

! put flush/position/r
if (associated(cpos(1)%flush%position%r)) then  
   allocate(vect2DDouble(size(cpos(1)%flush%position%r,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%flush%position%r
   enddo
   call put_vect2D_Double(idx,path, "flush/position/r",vect2DDouble, &
   size(cpos(1)%flush%position%r,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put flush/position/z
if (associated(cpos(1)%flush%position%z)) then  
   allocate(vect2DDouble(size(cpos(1)%flush%position%z,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%flush%position%z
   enddo
   call put_vect2D_Double(idx,path, "flush/position/z",vect2DDouble, &
   size(cpos(1)%flush%position%z,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put flush/coef
if (associated(cpos(1)%flush%coef)) then  
   allocate(vect3DDouble(size(cpos(1)%flush%coef,1),&
   size(cpos(1)%flush%coef,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%flush%coef
   enddo
   call put_vect3D_Double(idx,path, "flush/coef",vect3DDouble, &
   size(cpos(1)%flush%coef,1),size(cpos(1)%flush%coef,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! Put flush/codeparam/codename
if (associated(cpos(1)%flush%codeparam%codename)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%codeparam%codename)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%codeparam%codename(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%codeparam%codename(istring)
      enddo
   endif
   call put_string(idx,path, "flush/codeparam/codename",trim(longstring))
   write(*,*) 'Put cpos%flush%codeparam%codename'
endif

! Put flush/codeparam/codeversion
if (associated(cpos(1)%flush%codeparam%codeversion)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%codeparam%codeversion)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%codeparam%codeversion(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%codeparam%codeversion(istring)
      enddo
   endif
   call put_string(idx,path, "flush/codeparam/codeversion",trim(longstring))
   write(*,*) 'Put cpos%flush%codeparam%codeversion'
endif

! Put flush/codeparam/parameters
if (associated(cpos(1)%flush%codeparam%parameters)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%codeparam%parameters)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%codeparam%parameters(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%codeparam%parameters(istring)
      enddo
   endif
   call put_string(idx,path, "flush/codeparam/parameters",trim(longstring))
   write(*,*) 'Put cpos%flush%codeparam%parameters'
endif

! Put flush/codeparam/output_diag
if (associated(cpos(1)%flush%codeparam%output_diag)) then
   longstring = ' '    
   lenstring = size(cpos(1)%flush%codeparam%output_diag)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%flush%codeparam%output_diag(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%flush%codeparam%output_diag(istring)
      enddo
   endif
   call put_string(idx,path, "flush/codeparam/output_diag",trim(longstring))
   write(*,*) 'Put cpos%flush%codeparam%output_diag'
endif

! Put global_param/beta_pol  
if (any(cpos(1:lentime)%global_param%beta_pol/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%beta_pol    
   call put_vect1D_double(idx,path, "global_param/beta_pol",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/beta_tor  
if (any(cpos(1:lentime)%global_param%beta_tor/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%beta_tor    
   call put_vect1D_double(idx,path, "global_param/beta_tor",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/beta_normal  
if (any(cpos(1:lentime)%global_param%beta_normal/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%beta_normal    
   call put_vect1D_double(idx,path, "global_param/beta_normal",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/i_plasma  
if (any(cpos(1:lentime)%global_param%i_plasma/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%i_plasma    
   call put_vect1D_double(idx,path, "global_param/i_plasma",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/li  
if (any(cpos(1:lentime)%global_param%li/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%li    
   call put_vect1D_double(idx,path, "global_param/li",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/volume  
if (any(cpos(1:lentime)%global_param%volume/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%volume    
   call put_vect1D_double(idx,path, "global_param/volume",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/area  
if (any(cpos(1:lentime)%global_param%area/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%area    
   call put_vect1D_double(idx,path, "global_param/area",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/psi_ax  
if (any(cpos(1:lentime)%global_param%psi_ax/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%psi_ax    
   call put_vect1D_double(idx,path, "global_param/psi_ax",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/psi_bound  
if (any(cpos(1:lentime)%global_param%psi_bound/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%psi_bound    
   call put_vect1D_double(idx,path, "global_param/psi_bound",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/mag_axis/position/r  
if (any(cpos(1:lentime)%global_param%mag_axis%position%r/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%mag_axis%position%r    
   call put_vect1D_double(idx,path, "global_param/mag_axis/position/r",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/mag_axis/position/z  
if (any(cpos(1:lentime)%global_param%mag_axis%position%z/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%mag_axis%position%z    
   call put_vect1D_double(idx,path, "global_param/mag_axis/position/z",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/mag_axis/bphi  
if (any(cpos(1:lentime)%global_param%mag_axis%bphi/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%mag_axis%bphi    
   call put_vect1D_double(idx,path, "global_param/mag_axis/bphi",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/mag_axis/q  
if (any(cpos(1:lentime)%global_param%mag_axis%q/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%mag_axis%q    
   call put_vect1D_double(idx,path, "global_param/mag_axis/q",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/q_95  
if (any(cpos(1:lentime)%global_param%q_95/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%q_95    
   call put_vect1D_double(idx,path, "global_param/q_95",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put global_param/q_min  
if (any(cpos(1:lentime)%global_param%q_min/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%global_param%q_min    
   call put_vect1D_double(idx,path, "global_param/q_min",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! put profiles_1d/psi
if (associated(cpos(1)%profiles_1d%psi)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%psi,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%psi
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/psi",vect2DDouble, &
   size(cpos(1)%profiles_1d%psi,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/phi
if (associated(cpos(1)%profiles_1d%phi)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%phi,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%phi
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/phi",vect2DDouble, &
   size(cpos(1)%profiles_1d%phi,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/pressure
if (associated(cpos(1)%profiles_1d%pressure)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%pressure,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%pressure
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/pressure",vect2DDouble, &
   size(cpos(1)%profiles_1d%pressure,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/F_dia
if (associated(cpos(1)%profiles_1d%F_dia)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%F_dia,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%F_dia
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/F_dia",vect2DDouble, &
   size(cpos(1)%profiles_1d%F_dia,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/pprime
if (associated(cpos(1)%profiles_1d%pprime)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%pprime,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%pprime
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/pprime",vect2DDouble, &
   size(cpos(1)%profiles_1d%pprime,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/ffprime
if (associated(cpos(1)%profiles_1d%ffprime)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%ffprime,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%ffprime
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/ffprime",vect2DDouble, &
   size(cpos(1)%profiles_1d%ffprime,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/jphi
if (associated(cpos(1)%profiles_1d%jphi)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%jphi,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%jphi
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/jphi",vect2DDouble, &
   size(cpos(1)%profiles_1d%jphi,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/jparallel
if (associated(cpos(1)%profiles_1d%jparallel)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%jparallel,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%jparallel
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/jparallel",vect2DDouble, &
   size(cpos(1)%profiles_1d%jparallel,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/q
if (associated(cpos(1)%profiles_1d%q)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%q,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%q
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/q",vect2DDouble, &
   size(cpos(1)%profiles_1d%q,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/r_inboard
if (associated(cpos(1)%profiles_1d%r_inboard)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%r_inboard,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%r_inboard
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/r_inboard",vect2DDouble, &
   size(cpos(1)%profiles_1d%r_inboard,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/r_outboard
if (associated(cpos(1)%profiles_1d%r_outboard)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%r_outboard,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%r_outboard
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/r_outboard",vect2DDouble, &
   size(cpos(1)%profiles_1d%r_outboard,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/rho_rtvol
if (associated(cpos(1)%profiles_1d%rho_rtvol)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%rho_rtvol,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%rho_rtvol
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/rho_rtvol",vect2DDouble, &
   size(cpos(1)%profiles_1d%rho_rtvol,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/rho_rttorfl
if (associated(cpos(1)%profiles_1d%rho_rttorfl)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%rho_rttorfl,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%rho_rttorfl
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/rho_rttorfl",vect2DDouble, &
   size(cpos(1)%profiles_1d%rho_rttorfl,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/elongation
if (associated(cpos(1)%profiles_1d%elongation)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%elongation,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%elongation
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/elongation",vect2DDouble, &
   size(cpos(1)%profiles_1d%elongation,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/tria_upper
if (associated(cpos(1)%profiles_1d%tria_upper)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%tria_upper,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%tria_upper
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/tria_upper",vect2DDouble, &
   size(cpos(1)%profiles_1d%tria_upper,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/tria_lower
if (associated(cpos(1)%profiles_1d%tria_lower)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%tria_lower,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%tria_lower
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/tria_lower",vect2DDouble, &
   size(cpos(1)%profiles_1d%tria_lower,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_1d/volume
if (associated(cpos(1)%profiles_1d%volume)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_1d%volume,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_1d%volume
   enddo
   call put_vect2D_Double(idx,path, "profiles_1d/volume",vect2DDouble, &
   size(cpos(1)%profiles_1d%volume,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put profiles_2d/grid_type
if (associated(cpos(1)%profiles_2d%grid_type)) then
   longstring = ' '    
   lenstring = size(cpos(1)%profiles_2d%grid_type)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%profiles_2d%grid_type(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%profiles_2d%grid_type(istring)
      enddo
   endif
   call put_string(idx,path, "profiles_2d/grid_type",trim(longstring))
   write(*,*) 'Put cpos%profiles_2d%grid_type'
endif

! put profiles_2d/grid/dim1
if (associated(cpos(1)%profiles_2d%grid%dim1)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_2d%grid%dim1,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_2d%grid%dim1
   enddo
   call put_vect2D_Double(idx,path, "profiles_2d/grid/dim1",vect2DDouble, &
   size(cpos(1)%profiles_2d%grid%dim1,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_2d/grid/dim2
if (associated(cpos(1)%profiles_2d%grid%dim2)) then  
   allocate(vect2DDouble(size(cpos(1)%profiles_2d%grid%dim2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%profiles_2d%grid%dim2
   enddo
   call put_vect2D_Double(idx,path, "profiles_2d/grid/dim2",vect2DDouble, &
   size(cpos(1)%profiles_2d%grid%dim2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put profiles_2d/grid/connect  
if (associated(cpos(1)%profiles_2d%grid%connect)) then  
   allocate(vect3Dint(size(cpos(1)%profiles_2d%grid%connect,1),size(cpos(1)%profiles_2d%grid%connect,2),lentime))
   do itime=1,lentime
      vect3Dint(:,:,itime)  = cpos(itime)%profiles_2d%grid%connect
   enddo
   call put_vect3D_int(idx,path, "profiles_2d/grid/connect",vect3Dint, &
   size(cpos(1)%profiles_2d%grid%connect,1),size(cpos(1)%profiles_2d%grid%connect,2),lentime,1)           
   deallocate(vect3Dint)
endif

! put profiles_2d/psi_grid
if (associated(cpos(1)%profiles_2d%psi_grid)) then  
   allocate(vect3DDouble(size(cpos(1)%profiles_2d%psi_grid,1),&
   size(cpos(1)%profiles_2d%psi_grid,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%profiles_2d%psi_grid
   enddo
   call put_vect3D_Double(idx,path, "profiles_2d/psi_grid",vect3DDouble, &
   size(cpos(1)%profiles_2d%psi_grid,1),size(cpos(1)%profiles_2d%psi_grid,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put profiles_2d/jphi_grid
if (associated(cpos(1)%profiles_2d%jphi_grid)) then  
   allocate(vect3DDouble(size(cpos(1)%profiles_2d%jphi_grid,1),&
   size(cpos(1)%profiles_2d%jphi_grid,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%profiles_2d%jphi_grid
   enddo
   call put_vect3D_Double(idx,path, "profiles_2d/jphi_grid",vect3DDouble, &
   size(cpos(1)%profiles_2d%jphi_grid,1),size(cpos(1)%profiles_2d%jphi_grid,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put profiles_2d/jpar_grid
if (associated(cpos(1)%profiles_2d%jpar_grid)) then  
   allocate(vect3DDouble(size(cpos(1)%profiles_2d%jpar_grid,1),&
   size(cpos(1)%profiles_2d%jpar_grid,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%profiles_2d%jpar_grid
   enddo
   call put_vect3D_Double(idx,path, "profiles_2d/jpar_grid",vect3DDouble, &
   size(cpos(1)%profiles_2d%jpar_grid,1),size(cpos(1)%profiles_2d%jpar_grid,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put profiles_2d/br
if (associated(cpos(1)%profiles_2d%br)) then  
   allocate(vect3DDouble(size(cpos(1)%profiles_2d%br,1),&
   size(cpos(1)%profiles_2d%br,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%profiles_2d%br
   enddo
   call put_vect3D_Double(idx,path, "profiles_2d/br",vect3DDouble, &
   size(cpos(1)%profiles_2d%br,1),size(cpos(1)%profiles_2d%br,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put profiles_2d/bz
if (associated(cpos(1)%profiles_2d%bz)) then  
   allocate(vect3DDouble(size(cpos(1)%profiles_2d%bz,1),&
   size(cpos(1)%profiles_2d%bz,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%profiles_2d%bz
   enddo
   call put_vect3D_Double(idx,path, "profiles_2d/bz",vect3DDouble, &
   size(cpos(1)%profiles_2d%bz,1),size(cpos(1)%profiles_2d%bz,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put profiles_2d/bphi
if (associated(cpos(1)%profiles_2d%bphi)) then  
   allocate(vect3DDouble(size(cpos(1)%profiles_2d%bphi,1),&
   size(cpos(1)%profiles_2d%bphi,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%profiles_2d%bphi
   enddo
   call put_vect3D_Double(idx,path, "profiles_2d/bphi",vect3DDouble, &
   size(cpos(1)%profiles_2d%bphi,1),size(cpos(1)%profiles_2d%bphi,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! Put coord_sys/grid_type
if (associated(cpos(1)%coord_sys%grid_type)) then
   longstring = ' '    
   lenstring = size(cpos(1)%coord_sys%grid_type)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%coord_sys%grid_type(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%coord_sys%grid_type(istring)
      enddo
   endif
   call put_string(idx,path, "coord_sys/grid_type",trim(longstring))
   write(*,*) 'Put cpos%coord_sys%grid_type'
endif

! put coord_sys/grid/dim1
if (associated(cpos(1)%coord_sys%grid%dim1)) then  
   allocate(vect2DDouble(size(cpos(1)%coord_sys%grid%dim1,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%coord_sys%grid%dim1
   enddo
   call put_vect2D_Double(idx,path, "coord_sys/grid/dim1",vect2DDouble, &
   size(cpos(1)%coord_sys%grid%dim1,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put coord_sys/grid/dim2
if (associated(cpos(1)%coord_sys%grid%dim2)) then  
   allocate(vect2DDouble(size(cpos(1)%coord_sys%grid%dim2,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%coord_sys%grid%dim2
   enddo
   call put_vect2D_Double(idx,path, "coord_sys/grid/dim2",vect2DDouble, &
   size(cpos(1)%coord_sys%grid%dim2,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put coord_sys/jacobian
if (associated(cpos(1)%coord_sys%jacobian)) then  
   allocate(vect3DDouble(size(cpos(1)%coord_sys%jacobian,1),&
   size(cpos(1)%coord_sys%jacobian,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%coord_sys%jacobian
   enddo
   call put_vect3D_Double(idx,path, "coord_sys/jacobian",vect3DDouble, &
   size(cpos(1)%coord_sys%jacobian,1),size(cpos(1)%coord_sys%jacobian,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put coord_sys/g_11
if (associated(cpos(1)%coord_sys%g_11)) then  
   allocate(vect3DDouble(size(cpos(1)%coord_sys%g_11,1),&
   size(cpos(1)%coord_sys%g_11,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%coord_sys%g_11
   enddo
   call put_vect3D_Double(idx,path, "coord_sys/g_11",vect3DDouble, &
   size(cpos(1)%coord_sys%g_11,1),size(cpos(1)%coord_sys%g_11,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put coord_sys/g_12
if (associated(cpos(1)%coord_sys%g_12)) then  
   allocate(vect3DDouble(size(cpos(1)%coord_sys%g_12,1),&
   size(cpos(1)%coord_sys%g_12,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%coord_sys%g_12
   enddo
   call put_vect3D_Double(idx,path, "coord_sys/g_12",vect3DDouble, &
   size(cpos(1)%coord_sys%g_12,1),size(cpos(1)%coord_sys%g_12,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put coord_sys/g_22
if (associated(cpos(1)%coord_sys%g_22)) then  
   allocate(vect3DDouble(size(cpos(1)%coord_sys%g_22,1),&
   size(cpos(1)%coord_sys%g_22,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%coord_sys%g_22
   enddo
   call put_vect3D_Double(idx,path, "coord_sys/g_22",vect3DDouble, &
   size(cpos(1)%coord_sys%g_22,1),size(cpos(1)%coord_sys%g_22,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put coord_sys/g_33
if (associated(cpos(1)%coord_sys%g_33)) then  
   allocate(vect3DDouble(size(cpos(1)%coord_sys%g_33,1),&
   size(cpos(1)%coord_sys%g_33,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%coord_sys%g_33
   enddo
   call put_vect3D_Double(idx,path, "coord_sys/g_33",vect3DDouble, &
   size(cpos(1)%coord_sys%g_33,1),size(cpos(1)%coord_sys%g_33,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put coord_sys/position/r
if (associated(cpos(1)%coord_sys%position%r)) then  
   allocate(vect3DDouble(size(cpos(1)%coord_sys%position%r,1),&
   size(cpos(1)%coord_sys%position%r,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%coord_sys%position%r
   enddo
   call put_vect3D_Double(idx,path, "coord_sys/position/r",vect3DDouble, &
   size(cpos(1)%coord_sys%position%r,1),size(cpos(1)%coord_sys%position%r,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put coord_sys/position/z
if (associated(cpos(1)%coord_sys%position%z)) then  
   allocate(vect3DDouble(size(cpos(1)%coord_sys%position%z,1),&
   size(cpos(1)%coord_sys%position%z,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%coord_sys%position%z
   enddo
   call put_vect3D_Double(idx,path, "coord_sys/position/z",vect3DDouble, &
   size(cpos(1)%coord_sys%position%z,1),size(cpos(1)%coord_sys%position%z,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put codeparam/codename
if (associated(cpos(1)%codeparam%codename)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%codename)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%codename(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%codename(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codename",trim(longstring))
   write(*,*) 'Put cpos%codeparam%codename'
endif

! Put codeparam/codeversion
if (associated(cpos(1)%codeparam%codeversion)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%codeversion)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%codeversion(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%codeversion(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codeversion",trim(longstring))
   write(*,*) 'Put cpos%codeparam%codeversion'
endif

! Put codeparam/parameters
if (associated(cpos(1)%codeparam%parameters)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%parameters)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%parameters(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%parameters(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/parameters",trim(longstring))
   write(*,*) 'Put cpos%codeparam%parameters'
endif

! Put codeparam/output_diag
if (associated(cpos(1)%codeparam%output_diag)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%output_diag)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%output_diag(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%output_diag(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/output_diag",trim(longstring))
   write(*,*) 'Put cpos%codeparam%output_diag'
endif

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_ironmodel(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_ironmodel),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! Put datainfo/dataprovider
if (associated(cpos(1)%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%datainfo%dataprovider'
endif

! Put datainfo/putdate
if (associated(cpos(1)%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putdate'
endif

! Put datainfo/source
if (associated(cpos(1)%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%datainfo%source'
endif

! Put datainfo/comment
if (associated(cpos(1)%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%datainfo%comment'
endif

! Put datainfo/isref        
if (cpos(1)%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpos(1)%datainfo%isref)        
   write(*,*) 'Put cpos%datainfo%isref'
endif

! Put datainfo/whatref        
if (cpos(1)%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpos(1)%datainfo%whatref)        
   write(*,*) 'Put cpos%datainfo%whatref'
endif

! Put datainfo/putinfo/putmethod
if (associated(cpos(1)%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putmethod'
endif

! Put datainfo/putinfo/putaccess
if (associated(cpos(1)%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putaccess'
endif

! Put datainfo/putinfo/putlocation
if (associated(cpos(1)%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putlocation'
endif

! Put datainfo/putinfo/rights
if (associated(cpos(1)%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%rights'
endif

! Put desc_iron/name
if (associated(cpos(1)%desc_iron%name)) then
   dim1 = size(cpos(1)%desc_iron%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%desc_iron%name(i))
   enddo
   call put_Vect1d_String(idx,path, "desc_iron/name", &
         cpos(1)%desc_iron%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%desc_iron%name'
endif

! Put desc_iron/id
if (associated(cpos(1)%desc_iron%id)) then
   dim1 = size(cpos(1)%desc_iron%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%desc_iron%id(i))
   enddo
   call put_Vect1d_String(idx,path, "desc_iron/id", &
         cpos(1)%desc_iron%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%desc_iron%id'
endif

! Put desc_iron/permeability/B        
if (associated(cpos(1)%desc_iron%permeability%B)) then   
   call put_vect2d_double(idx,path, "desc_iron/permeability/B", &
   cpos(1)%desc_iron%permeability%B,&
   size(cpos(1)%desc_iron%permeability%B,1),size(cpos(1)%desc_iron%permeability%B,2),0)  
   write(*,*) 'Put cpos%desc_iron%permeability%B'
endif

! Put desc_iron/permeability/mur        
if (associated(cpos(1)%desc_iron%permeability%mur)) then   
   call put_vect2d_double(idx,path, "desc_iron/permeability/mur", &
   cpos(1)%desc_iron%permeability%mur,&
   size(cpos(1)%desc_iron%permeability%mur,1),size(cpos(1)%desc_iron%permeability%mur,2),0)  
   write(*,*) 'Put cpos%desc_iron%permeability%mur'
endif

! Put desc_iron/geom_iron/npoints 
if (associated(cpos(1)%desc_iron%geom_iron%npoints)) then         
   call put_vect1d_int(idx,path, "desc_iron/geom_iron/npoints",cpos(1)%desc_iron%geom_iron%npoints, &
   size(cpos(1)%desc_iron%geom_iron%npoints),0) 
   write(*,*) 'Put cpos%desc_iron%geom_iron%npoints'
endif

! Put desc_iron/geom_iron/rzcoordinate/r        
if (associated(cpos(1)%desc_iron%geom_iron%rzcoordinate%r)) then   
   call put_vect2d_double(idx,path, "desc_iron/geom_iron/rzcoordinate/r", &
   cpos(1)%desc_iron%geom_iron%rzcoordinate%r,&
   size(cpos(1)%desc_iron%geom_iron%rzcoordinate%r,1),size(cpos(1)%desc_iron%geom_iron%rzcoordinate%r,2),0)  
   write(*,*) 'Put cpos%desc_iron%geom_iron%rzcoordinate%r'
endif

! Put desc_iron/geom_iron/rzcoordinate/z        
if (associated(cpos(1)%desc_iron%geom_iron%rzcoordinate%z)) then   
   call put_vect2d_double(idx,path, "desc_iron/geom_iron/rzcoordinate/z", &
   cpos(1)%desc_iron%geom_iron%rzcoordinate%z,&
   size(cpos(1)%desc_iron%geom_iron%rzcoordinate%z,1),size(cpos(1)%desc_iron%geom_iron%rzcoordinate%z,2),0)  
   write(*,*) 'Put cpos%desc_iron%geom_iron%rzcoordinate%z'
endif

! put magnetise/mr/value
if (associated(cpos(1)%magnetise%mr%value)) then  
   allocate(vect2DDouble(size(cpos(1)%magnetise%mr%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%magnetise%mr%value
   enddo
   call put_vect2D_Double(idx,path, "magnetise/mr/value",vect2DDouble, &
   size(cpos(1)%magnetise%mr%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put magnetise/mr/abserror
if (associated(cpos(1)%magnetise%mr%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%magnetise%mr%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%magnetise%mr%abserror
   enddo
   call put_vect2D_Double(idx,path, "magnetise/mr/abserror",vect2DDouble, &
   size(cpos(1)%magnetise%mr%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put magnetise/mr/relerror
if (associated(cpos(1)%magnetise%mr%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%magnetise%mr%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%magnetise%mr%relerror
   enddo
   call put_vect2D_Double(idx,path, "magnetise/mr/relerror",vect2DDouble, &
   size(cpos(1)%magnetise%mr%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put magnetise/mz/value
if (associated(cpos(1)%magnetise%mz%value)) then  
   allocate(vect2DDouble(size(cpos(1)%magnetise%mz%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%magnetise%mz%value
   enddo
   call put_vect2D_Double(idx,path, "magnetise/mz/value",vect2DDouble, &
   size(cpos(1)%magnetise%mz%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put magnetise/mz/abserror
if (associated(cpos(1)%magnetise%mz%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%magnetise%mz%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%magnetise%mz%abserror
   enddo
   call put_vect2D_Double(idx,path, "magnetise/mz/abserror",vect2DDouble, &
   size(cpos(1)%magnetise%mz%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put magnetise/mz/relerror
if (associated(cpos(1)%magnetise%mz%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%magnetise%mz%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%magnetise%mz%relerror
   enddo
   call put_vect2D_Double(idx,path, "magnetise/mz/relerror",vect2DDouble, &
   size(cpos(1)%magnetise%mz%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_interfdiag(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_lineintegraldiag),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! Put datainfo/dataprovider
if (associated(cpos(1)%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%datainfo%dataprovider'
endif

! Put datainfo/putdate
if (associated(cpos(1)%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putdate'
endif

! Put datainfo/source
if (associated(cpos(1)%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%datainfo%source'
endif

! Put datainfo/comment
if (associated(cpos(1)%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%datainfo%comment'
endif

! Put datainfo/isref        
if (cpos(1)%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpos(1)%datainfo%isref)        
   write(*,*) 'Put cpos%datainfo%isref'
endif

! Put datainfo/whatref        
if (cpos(1)%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpos(1)%datainfo%whatref)        
   write(*,*) 'Put cpos%datainfo%whatref'
endif

! Put datainfo/putinfo/putmethod
if (associated(cpos(1)%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putmethod'
endif

! Put datainfo/putinfo/putaccess
if (associated(cpos(1)%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putaccess'
endif

! Put datainfo/putinfo/putlocation
if (associated(cpos(1)%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putlocation'
endif

! Put datainfo/putinfo/rights
if (associated(cpos(1)%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%rights'
endif

! Put expression
if (associated(cpos(1)%expression)) then
   longstring = ' '    
   lenstring = size(cpos(1)%expression)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%expression(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%expression(istring)
      enddo
   endif
   call put_string(idx,path, "expression",trim(longstring))
   write(*,*) 'Put cpos%expression'
endif

! Put setup_line/pivot_point/r
if (associated(cpos(1)%setup_line%pivot_point%r)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/r", &
   cpos(1)%setup_line%pivot_point%r, &
   size(cpos(1)%setup_line%pivot_point%r),0)
   write(*,*) 'Put cpos%setup_line%pivot_point%r'
endif

! Put setup_line/pivot_point/z
if (associated(cpos(1)%setup_line%pivot_point%z)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/z", &
   cpos(1)%setup_line%pivot_point%z, &
   size(cpos(1)%setup_line%pivot_point%z),0)
   write(*,*) 'Put cpos%setup_line%pivot_point%z'
endif

! Put setup_line/pivot_point/phi
if (associated(cpos(1)%setup_line%pivot_point%phi)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/phi", &
   cpos(1)%setup_line%pivot_point%phi, &
   size(cpos(1)%setup_line%pivot_point%phi),0)
   write(*,*) 'Put cpos%setup_line%pivot_point%phi'
endif

! Put setup_line/polchordang
if (associated(cpos(1)%setup_line%polchordang)) then   
   call put_vect1d_double(idx,path, "setup_line/polchordang", &
   cpos(1)%setup_line%polchordang, &
   size(cpos(1)%setup_line%polchordang),0)
   write(*,*) 'Put cpos%setup_line%polchordang'
endif

! Put setup_line/torchordang
if (associated(cpos(1)%setup_line%torchordang)) then   
   call put_vect1d_double(idx,path, "setup_line/torchordang", &
   cpos(1)%setup_line%torchordang, &
   size(cpos(1)%setup_line%torchordang),0)
   write(*,*) 'Put cpos%setup_line%torchordang'
endif

! Put setup_line/second_point/r
if (associated(cpos(1)%setup_line%second_point%r)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/r", &
   cpos(1)%setup_line%second_point%r, &
   size(cpos(1)%setup_line%second_point%r),0)
   write(*,*) 'Put cpos%setup_line%second_point%r'
endif

! Put setup_line/second_point/z
if (associated(cpos(1)%setup_line%second_point%z)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/z", &
   cpos(1)%setup_line%second_point%z, &
   size(cpos(1)%setup_line%second_point%z),0)
   write(*,*) 'Put cpos%setup_line%second_point%z'
endif

! Put setup_line/second_point/phi
if (associated(cpos(1)%setup_line%second_point%phi)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/phi", &
   cpos(1)%setup_line%second_point%phi, &
   size(cpos(1)%setup_line%second_point%phi),0)
   write(*,*) 'Put cpos%setup_line%second_point%phi'
endif

! Put setup_line/nchordpoints        
if (cpos(1)%setup_line%nchordpoints.NE.-999999999) then
   call put_int(idx,path, "setup_line/nchordpoints",cpos(1)%setup_line%nchordpoints)        
   write(*,*) 'Put cpos%setup_line%nchordpoints'
endif

! put measure/value
if (associated(cpos(1)%measure%value)) then  
   allocate(vect2DDouble(size(cpos(1)%measure%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%measure%value
   enddo
   call put_vect2D_Double(idx,path, "measure/value",vect2DDouble, &
   size(cpos(1)%measure%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put measure/abserror
if (associated(cpos(1)%measure%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%measure%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%measure%abserror
   enddo
   call put_vect2D_Double(idx,path, "measure/abserror",vect2DDouble, &
   size(cpos(1)%measure%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put measure/relerror
if (associated(cpos(1)%measure%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%measure%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%measure%relerror
   enddo
   call put_vect2D_Double(idx,path, "measure/relerror",vect2DDouble, &
   size(cpos(1)%measure%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_limiter(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_limiter) :: cpo       
call begin_cpo_put_non_timed(idx, path)

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put position/r
if (associated(cpo%position%r)) then   
   call put_vect1d_double(idx,path, "position/r",cpo%position%r,&
   size(cpo%position%r),0)
   write(*,*) 'Put cpo%position%r',cpo%position%r
endif

! Put position/z
if (associated(cpo%position%z)) then   
   call put_vect1d_double(idx,path, "position/z",cpo%position%z,&
   size(cpo%position%z),0)
   write(*,*) 'Put cpo%position%z',cpo%position%z
endif

call end_cpo_put_non_timed(idx, path)

return
endsubroutine

subroutine euITM_put_mhd(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_mhd),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! Put datainfo/dataprovider
if (associated(cpos(1)%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%datainfo%dataprovider'
endif

! Put datainfo/putdate
if (associated(cpos(1)%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putdate'
endif

! Put datainfo/source
if (associated(cpos(1)%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%datainfo%source'
endif

! Put datainfo/comment
if (associated(cpos(1)%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%datainfo%comment'
endif

! Put datainfo/isref        
if (cpos(1)%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpos(1)%datainfo%isref)        
   write(*,*) 'Put cpos%datainfo%isref'
endif

! Put datainfo/whatref        
if (cpos(1)%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpos(1)%datainfo%whatref)        
   write(*,*) 'Put cpos%datainfo%whatref'
endif

! Put datainfo/putinfo/putmethod
if (associated(cpos(1)%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putmethod'
endif

! Put datainfo/putinfo/putaccess
if (associated(cpos(1)%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putaccess'
endif

! Put datainfo/putinfo/putlocation
if (associated(cpos(1)%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putlocation'
endif

! Put datainfo/putinfo/rights
if (associated(cpos(1)%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%rights'
endif

! put n
if (associated(cpos(1)%n)) then  
   allocate(vect2DDouble(size(cpos(1)%n,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%n
   enddo
   call put_vect2D_Double(idx,path, "n",vect2DDouble, &
   size(cpos(1)%n,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put m
if (associated(cpos(1)%m)) then  
   allocate(vect3DDouble(size(cpos(1)%m,1),&
   size(cpos(1)%m,2),lentime))
   do itime=1,lentime
      vect3DDouble(:,:,itime)  = cpos(itime)%m
   enddo
   call put_vect3D_Double(idx,path, "m",vect3DDouble, &
   size(cpos(1)%m,1),size(cpos(1)%m,2),lentime,1)           
   deallocate(vect3DDouble)
endif

! put psi
if (associated(cpos(1)%psi)) then  
   allocate(vect2DDouble(size(cpos(1)%psi,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%psi
   enddo
   call put_vect2D_Double(idx,path, "psi",vect2DDouble, &
   size(cpos(1)%psi,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put frequency
if (associated(cpos(1)%frequency)) then  
   allocate(vect2DDouble(size(cpos(1)%frequency,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%frequency
   enddo
   call put_vect2D_Double(idx,path, "frequency",vect2DDouble, &
   size(cpos(1)%frequency,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put growthrate
if (associated(cpos(1)%growthrate)) then  
   allocate(vect2DDouble(size(cpos(1)%growthrate,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%growthrate
   enddo
   call put_vect2D_Double(idx,path, "growthrate",vect2DDouble, &
   size(cpos(1)%growthrate,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put disp_perp
if (associated(cpos(1)%disp_perp)) then  
   allocate(vect4dDouble(size(cpos(1)%disp_perp,1),&
   size(cpos(1)%disp_perp,2), &
   size(cpos(1)%disp_perp,3),lentime))
   do itime=1,lentime
      vect4dDouble(:,:,:,itime)  = cpos(itime)%disp_perp
   enddo
   call put_vect4d_Double(idx,path, "disp_perp",vect4dDouble, &
   size(cpos(1)%disp_perp,1),size(cpos(1)%disp_perp,2),&
   size(cpos(1)%disp_perp,3),lentime,1)           
   deallocate(vect4dDouble)
endif

! put disp_par
if (associated(cpos(1)%disp_par)) then  
   allocate(vect4dDouble(size(cpos(1)%disp_par,1),&
   size(cpos(1)%disp_par,2), &
   size(cpos(1)%disp_par,3),lentime))
   do itime=1,lentime
      vect4dDouble(:,:,:,itime)  = cpos(itime)%disp_par
   enddo
   call put_vect4d_Double(idx,path, "disp_par",vect4dDouble, &
   size(cpos(1)%disp_par,1),size(cpos(1)%disp_par,2),&
   size(cpos(1)%disp_par,3),lentime,1)           
   deallocate(vect4dDouble)
endif

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put codeparam/codename
if (associated(cpos(1)%codeparam%codename)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%codename)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%codename(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%codename(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codename",trim(longstring))
   write(*,*) 'Put cpos%codeparam%codename'
endif

! Put codeparam/codeversion
if (associated(cpos(1)%codeparam%codeversion)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%codeversion)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%codeversion(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%codeversion(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codeversion",trim(longstring))
   write(*,*) 'Put cpos%codeparam%codeversion'
endif

! Put codeparam/parameters
if (associated(cpos(1)%codeparam%parameters)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%parameters)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%parameters(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%parameters(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/parameters",trim(longstring))
   write(*,*) 'Put cpos%codeparam%parameters'
endif

! Put codeparam/output_diag
if (associated(cpos(1)%codeparam%output_diag)) then
   longstring = ' '    
   lenstring = size(cpos(1)%codeparam%output_diag)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%codeparam%output_diag(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%codeparam%output_diag(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/output_diag",trim(longstring))
   write(*,*) 'Put cpos%codeparam%output_diag'
endif

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_magdiag(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_magdiag),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! Put datainfo/dataprovider
if (associated(cpos(1)%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%datainfo%dataprovider'
endif

! Put datainfo/putdate
if (associated(cpos(1)%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putdate'
endif

! Put datainfo/source
if (associated(cpos(1)%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%datainfo%source'
endif

! Put datainfo/comment
if (associated(cpos(1)%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%datainfo%comment'
endif

! Put datainfo/isref        
if (cpos(1)%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpos(1)%datainfo%isref)        
   write(*,*) 'Put cpos%datainfo%isref'
endif

! Put datainfo/whatref        
if (cpos(1)%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpos(1)%datainfo%whatref)        
   write(*,*) 'Put cpos%datainfo%whatref'
endif

! Put datainfo/putinfo/putmethod
if (associated(cpos(1)%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putmethod'
endif

! Put datainfo/putinfo/putaccess
if (associated(cpos(1)%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putaccess'
endif

! Put datainfo/putinfo/putlocation
if (associated(cpos(1)%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putlocation'
endif

! Put datainfo/putinfo/rights
if (associated(cpos(1)%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%rights'
endif

! Put ip/value  
if (any(cpos(1:lentime)%ip%value/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%ip%value    
   call put_vect1D_double(idx,path, "ip/value",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put ip/abserror  
if (any(cpos(1:lentime)%ip%abserror/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%ip%abserror    
   call put_vect1D_double(idx,path, "ip/abserror",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put ip/relerror  
if (any(cpos(1:lentime)%ip%relerror/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%ip%relerror    
   call put_vect1D_double(idx,path, "ip/relerror",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put diamagflux/value  
if (any(cpos(1:lentime)%diamagflux%value/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%diamagflux%value    
   call put_vect1D_double(idx,path, "diamagflux/value",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put diamagflux/abserror  
if (any(cpos(1:lentime)%diamagflux%abserror/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%diamagflux%abserror    
   call put_vect1D_double(idx,path, "diamagflux/abserror",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put diamagflux/relerror  
if (any(cpos(1:lentime)%diamagflux%relerror/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%diamagflux%relerror    
   call put_vect1D_double(idx,path, "diamagflux/relerror",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put flux_loops/setup_floops/name
if (associated(cpos(1)%flux_loops%setup_floops%name)) then
   dim1 = size(cpos(1)%flux_loops%setup_floops%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%flux_loops%setup_floops%name(i))
   enddo
   call put_Vect1d_String(idx,path, "flux_loops/setup_floops/name", &
         cpos(1)%flux_loops%setup_floops%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%flux_loops%setup_floops%name'
endif

! Put flux_loops/setup_floops/id
if (associated(cpos(1)%flux_loops%setup_floops%id)) then
   dim1 = size(cpos(1)%flux_loops%setup_floops%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%flux_loops%setup_floops%id(i))
   enddo
   call put_Vect1d_String(idx,path, "flux_loops/setup_floops/id", &
         cpos(1)%flux_loops%setup_floops%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%flux_loops%setup_floops%id'
endif

! Put flux_loops/setup_floops/position/r        
if (associated(cpos(1)%flux_loops%setup_floops%position%r)) then   
   call put_vect2d_double(idx,path, "flux_loops/setup_floops/position/r", &
   cpos(1)%flux_loops%setup_floops%position%r,&
   size(cpos(1)%flux_loops%setup_floops%position%r,1),size(cpos(1)%flux_loops%setup_floops%position%r,2),0)  
   write(*,*) 'Put cpos%flux_loops%setup_floops%position%r'
endif

! Put flux_loops/setup_floops/position/z        
if (associated(cpos(1)%flux_loops%setup_floops%position%z)) then   
   call put_vect2d_double(idx,path, "flux_loops/setup_floops/position/z", &
   cpos(1)%flux_loops%setup_floops%position%z,&
   size(cpos(1)%flux_loops%setup_floops%position%z,1),size(cpos(1)%flux_loops%setup_floops%position%z,2),0)  
   write(*,*) 'Put cpos%flux_loops%setup_floops%position%z'
endif

! Put flux_loops/setup_floops/position/phi        
if (associated(cpos(1)%flux_loops%setup_floops%position%phi)) then   
   call put_vect2d_double(idx,path, "flux_loops/setup_floops/position/phi", &
   cpos(1)%flux_loops%setup_floops%position%phi,&
   size(cpos(1)%flux_loops%setup_floops%position%phi,1),size(cpos(1)%flux_loops%setup_floops%position%phi,2),0)  
   write(*,*) 'Put cpos%flux_loops%setup_floops%position%phi'
endif

! Put flux_loops/setup_floops/npoints 
if (associated(cpos(1)%flux_loops%setup_floops%npoints)) then         
   call put_vect1d_int(idx,path, "flux_loops/setup_floops/npoints",cpos(1)%flux_loops%setup_floops%npoints, &
   size(cpos(1)%flux_loops%setup_floops%npoints),0) 
   write(*,*) 'Put cpos%flux_loops%setup_floops%npoints'
endif

! put flux_loops/measure/value
if (associated(cpos(1)%flux_loops%measure%value)) then  
   allocate(vect2DDouble(size(cpos(1)%flux_loops%measure%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%flux_loops%measure%value
   enddo
   call put_vect2D_Double(idx,path, "flux_loops/measure/value",vect2DDouble, &
   size(cpos(1)%flux_loops%measure%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put flux_loops/measure/abserror
if (associated(cpos(1)%flux_loops%measure%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%flux_loops%measure%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%flux_loops%measure%abserror
   enddo
   call put_vect2D_Double(idx,path, "flux_loops/measure/abserror",vect2DDouble, &
   size(cpos(1)%flux_loops%measure%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put flux_loops/measure/relerror
if (associated(cpos(1)%flux_loops%measure%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%flux_loops%measure%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%flux_loops%measure%relerror
   enddo
   call put_vect2D_Double(idx,path, "flux_loops/measure/relerror",vect2DDouble, &
   size(cpos(1)%flux_loops%measure%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put bpol_probes/setup_bprobe/name
if (associated(cpos(1)%bpol_probes%setup_bprobe%name)) then
   dim1 = size(cpos(1)%bpol_probes%setup_bprobe%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%bpol_probes%setup_bprobe%name(i))
   enddo
   call put_Vect1d_String(idx,path, "bpol_probes/setup_bprobe/name", &
         cpos(1)%bpol_probes%setup_bprobe%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%bpol_probes%setup_bprobe%name'
endif

! Put bpol_probes/setup_bprobe/id
if (associated(cpos(1)%bpol_probes%setup_bprobe%id)) then
   dim1 = size(cpos(1)%bpol_probes%setup_bprobe%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%bpol_probes%setup_bprobe%id(i))
   enddo
   call put_Vect1d_String(idx,path, "bpol_probes/setup_bprobe/id", &
         cpos(1)%bpol_probes%setup_bprobe%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%bpol_probes%setup_bprobe%id'
endif

! Put bpol_probes/setup_bprobe/position/r
if (associated(cpos(1)%bpol_probes%setup_bprobe%position%r)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/position/r", &
   cpos(1)%bpol_probes%setup_bprobe%position%r, &
   size(cpos(1)%bpol_probes%setup_bprobe%position%r),0)
   write(*,*) 'Put cpos%bpol_probes%setup_bprobe%position%r'
endif

! Put bpol_probes/setup_bprobe/position/z
if (associated(cpos(1)%bpol_probes%setup_bprobe%position%z)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/position/z", &
   cpos(1)%bpol_probes%setup_bprobe%position%z, &
   size(cpos(1)%bpol_probes%setup_bprobe%position%z),0)
   write(*,*) 'Put cpos%bpol_probes%setup_bprobe%position%z'
endif

! Put bpol_probes/setup_bprobe/polangle
if (associated(cpos(1)%bpol_probes%setup_bprobe%polangle)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/polangle", &
   cpos(1)%bpol_probes%setup_bprobe%polangle, &
   size(cpos(1)%bpol_probes%setup_bprobe%polangle),0)
   write(*,*) 'Put cpos%bpol_probes%setup_bprobe%polangle'
endif

! Put bpol_probes/setup_bprobe/torangle
if (associated(cpos(1)%bpol_probes%setup_bprobe%torangle)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/torangle", &
   cpos(1)%bpol_probes%setup_bprobe%torangle, &
   size(cpos(1)%bpol_probes%setup_bprobe%torangle),0)
   write(*,*) 'Put cpos%bpol_probes%setup_bprobe%torangle'
endif

! Put bpol_probes/setup_bprobe/area
if (associated(cpos(1)%bpol_probes%setup_bprobe%area)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/area", &
   cpos(1)%bpol_probes%setup_bprobe%area, &
   size(cpos(1)%bpol_probes%setup_bprobe%area),0)
   write(*,*) 'Put cpos%bpol_probes%setup_bprobe%area'
endif

! Put bpol_probes/setup_bprobe/length
if (associated(cpos(1)%bpol_probes%setup_bprobe%length)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/length", &
   cpos(1)%bpol_probes%setup_bprobe%length, &
   size(cpos(1)%bpol_probes%setup_bprobe%length),0)
   write(*,*) 'Put cpos%bpol_probes%setup_bprobe%length'
endif

! Put bpol_probes/setup_bprobe/turns 
if (associated(cpos(1)%bpol_probes%setup_bprobe%turns)) then         
   call put_vect1d_int(idx,path, "bpol_probes/setup_bprobe/turns",cpos(1)%bpol_probes%setup_bprobe%turns, &
   size(cpos(1)%bpol_probes%setup_bprobe%turns),0) 
   write(*,*) 'Put cpos%bpol_probes%setup_bprobe%turns'
endif

! put bpol_probes/measure/value
if (associated(cpos(1)%bpol_probes%measure%value)) then  
   allocate(vect2DDouble(size(cpos(1)%bpol_probes%measure%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%bpol_probes%measure%value
   enddo
   call put_vect2D_Double(idx,path, "bpol_probes/measure/value",vect2DDouble, &
   size(cpos(1)%bpol_probes%measure%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put bpol_probes/measure/abserror
if (associated(cpos(1)%bpol_probes%measure%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%bpol_probes%measure%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%bpol_probes%measure%abserror
   enddo
   call put_vect2D_Double(idx,path, "bpol_probes/measure/abserror",vect2DDouble, &
   size(cpos(1)%bpol_probes%measure%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put bpol_probes/measure/relerror
if (associated(cpos(1)%bpol_probes%measure%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%bpol_probes%measure%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%bpol_probes%measure%relerror
   enddo
   call put_vect2D_Double(idx,path, "bpol_probes/measure/relerror",vect2DDouble, &
   size(cpos(1)%bpol_probes%measure%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_msediag(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_msediag),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! Put datainfo/dataprovider
if (associated(cpos(1)%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%datainfo%dataprovider'
endif

! Put datainfo/putdate
if (associated(cpos(1)%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putdate'
endif

! Put datainfo/source
if (associated(cpos(1)%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%datainfo%source'
endif

! Put datainfo/comment
if (associated(cpos(1)%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%datainfo%comment'
endif

! Put datainfo/isref        
if (cpos(1)%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpos(1)%datainfo%isref)        
   write(*,*) 'Put cpos%datainfo%isref'
endif

! Put datainfo/whatref        
if (cpos(1)%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpos(1)%datainfo%whatref)        
   write(*,*) 'Put cpos%datainfo%whatref'
endif

! Put datainfo/putinfo/putmethod
if (associated(cpos(1)%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putmethod'
endif

! Put datainfo/putinfo/putaccess
if (associated(cpos(1)%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putaccess'
endif

! Put datainfo/putinfo/putlocation
if (associated(cpos(1)%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putlocation'
endif

! Put datainfo/putinfo/rights
if (associated(cpos(1)%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%rights'
endif

! Put setup_mse/rzgamma/r
if (associated(cpos(1)%setup_mse%rzgamma%r)) then   
   call put_vect1d_double(idx,path, "setup_mse/rzgamma/r", &
   cpos(1)%setup_mse%rzgamma%r, &
   size(cpos(1)%setup_mse%rzgamma%r),0)
   write(*,*) 'Put cpos%setup_mse%rzgamma%r'
endif

! Put setup_mse/rzgamma/z
if (associated(cpos(1)%setup_mse%rzgamma%z)) then   
   call put_vect1d_double(idx,path, "setup_mse/rzgamma/z", &
   cpos(1)%setup_mse%rzgamma%z, &
   size(cpos(1)%setup_mse%rzgamma%z),0)
   write(*,*) 'Put cpos%setup_mse%rzgamma%z'
endif

! Put setup_mse/geom_coef        
if (associated(cpos(1)%setup_mse%geom_coef)) then   
   call put_vect2d_double(idx,path, "setup_mse/geom_coef", &
   cpos(1)%setup_mse%geom_coef,&
   size(cpos(1)%setup_mse%geom_coef,1),size(cpos(1)%setup_mse%geom_coef,2),0)  
   write(*,*) 'Put cpos%setup_mse%geom_coef'
endif

! put measure/value
if (associated(cpos(1)%measure%value)) then  
   allocate(vect2DDouble(size(cpos(1)%measure%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%measure%value
   enddo
   call put_vect2D_Double(idx,path, "measure/value",vect2DDouble, &
   size(cpos(1)%measure%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put measure/abserror
if (associated(cpos(1)%measure%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%measure%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%measure%abserror
   enddo
   call put_vect2D_Double(idx,path, "measure/abserror",vect2DDouble, &
   size(cpos(1)%measure%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put measure/relerror
if (associated(cpos(1)%measure%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%measure%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%measure%relerror
   enddo
   call put_vect2D_Double(idx,path, "measure/relerror",vect2DDouble, &
   size(cpos(1)%measure%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_pfsystems(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_pfsystems),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! Put datainfo/dataprovider
if (associated(cpos(1)%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%datainfo%dataprovider'
endif

! Put datainfo/putdate
if (associated(cpos(1)%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putdate'
endif

! Put datainfo/source
if (associated(cpos(1)%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%datainfo%source'
endif

! Put datainfo/comment
if (associated(cpos(1)%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%datainfo%comment'
endif

! Put datainfo/isref        
if (cpos(1)%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpos(1)%datainfo%isref)        
   write(*,*) 'Put cpos%datainfo%isref'
endif

! Put datainfo/whatref        
if (cpos(1)%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpos(1)%datainfo%whatref)        
   write(*,*) 'Put cpos%datainfo%whatref'
endif

! Put datainfo/putinfo/putmethod
if (associated(cpos(1)%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putmethod'
endif

! Put datainfo/putinfo/putaccess
if (associated(cpos(1)%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putaccess'
endif

! Put datainfo/putinfo/putlocation
if (associated(cpos(1)%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putlocation'
endif

! Put datainfo/putinfo/rights
if (associated(cpos(1)%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%rights'
endif

! Put pfcoils/desc_pfcoils/name
if (associated(cpos(1)%pfcoils%desc_pfcoils%name)) then
   dim1 = size(cpos(1)%pfcoils%desc_pfcoils%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%pfcoils%desc_pfcoils%name(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcoils/desc_pfcoils/name", &
         cpos(1)%pfcoils%desc_pfcoils%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%name'
endif

! Put pfcoils/desc_pfcoils/id
if (associated(cpos(1)%pfcoils%desc_pfcoils%id)) then
   dim1 = size(cpos(1)%pfcoils%desc_pfcoils%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%pfcoils%desc_pfcoils%id(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcoils/desc_pfcoils/id", &
         cpos(1)%pfcoils%desc_pfcoils%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%id'
endif

! Put pfcoils/desc_pfcoils/res
if (associated(cpos(1)%pfcoils%desc_pfcoils%res)) then   
   call put_vect1d_double(idx,path, "pfcoils/desc_pfcoils/res", &
   cpos(1)%pfcoils%desc_pfcoils%res, &
   size(cpos(1)%pfcoils%desc_pfcoils%res),0)
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%res'
endif

! Put pfcoils/desc_pfcoils/emax
if (associated(cpos(1)%pfcoils%desc_pfcoils%emax)) then   
   call put_vect1d_double(idx,path, "pfcoils/desc_pfcoils/emax", &
   cpos(1)%pfcoils%desc_pfcoils%emax, &
   size(cpos(1)%pfcoils%desc_pfcoils%emax),0)
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%emax'
endif

! Put pfcoils/desc_pfcoils/nelement 
if (associated(cpos(1)%pfcoils%desc_pfcoils%nelement)) then         
   call put_vect1d_int(idx,path, "pfcoils/desc_pfcoils/nelement",cpos(1)%pfcoils%desc_pfcoils%nelement, &
   size(cpos(1)%pfcoils%desc_pfcoils%nelement),0) 
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%nelement'
endif

! Put pfcoils/desc_pfcoils/pfelement/name
if (associated(cpos(1)%pfcoils%desc_pfcoils%pfelement%name)) then
   dim1 = size(cpos(1)%pfcoils%desc_pfcoils%pfelement%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%pfcoils%desc_pfcoils%pfelement%name(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcoils/desc_pfcoils/pfelement/name", &
         cpos(1)%pfcoils%desc_pfcoils%pfelement%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%pfelement%name'
endif

! Put pfcoils/desc_pfcoils/pfelement/id
if (associated(cpos(1)%pfcoils%desc_pfcoils%pfelement%id)) then
   dim1 = size(cpos(1)%pfcoils%desc_pfcoils%pfelement%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%pfcoils%desc_pfcoils%pfelement%id(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcoils/desc_pfcoils/pfelement/id", &
         cpos(1)%pfcoils%desc_pfcoils%pfelement%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%pfelement%id'
endif

! Put pfcoils/desc_pfcoils/pfelement/turnsign        
if (associated(cpos(1)%pfcoils%desc_pfcoils%pfelement%turnsign)) then   
   call put_vect2d_double(idx,path, "pfcoils/desc_pfcoils/pfelement/turnsign", &
   cpos(1)%pfcoils%desc_pfcoils%pfelement%turnsign,&
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%turnsign,1),size(cpos(1)%pfcoils%desc_pfcoils%pfelement%turnsign,2),0)  
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%pfelement%turnsign'
endif

! Put pfcoils/desc_pfcoils/pfelement/area        
if (associated(cpos(1)%pfcoils%desc_pfcoils%pfelement%area)) then   
   call put_vect2d_double(idx,path, "pfcoils/desc_pfcoils/pfelement/area", &
   cpos(1)%pfcoils%desc_pfcoils%pfelement%area,&
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%area,1),size(cpos(1)%pfcoils%desc_pfcoils%pfelement%area,2),0)  
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%pfelement%area'
endif

! Put pfcoils/desc_pfcoils/pfelement/pfgeometry/type        
if (associated(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%type)) then   
   call put_vect2d_int(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/type", &
   cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%type, &
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%type,1), &
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%type,2),0)  
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%pfelement%pfgeometry%type'
endif

! Put pfcoils/desc_pfcoils/pfelement/pfgeometry/npoints        
if (associated(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints)) then   
   call put_vect2d_int(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/npoints", &
   cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints, &
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints,1), &
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints,2),0)  
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints'
endif

! Put pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/r        
if (associated(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r)) then   
   call put_vect3d_double(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/r", &
   cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r, &
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r,1), &
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r,2),&
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r,3),0)    
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r'
endif

! Put pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/z        
if (associated(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z)) then   
   call put_vect3d_double(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/z", &
   cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z, &
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z,1), &
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z,2),&
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z,3),0)    
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z'
endif

! Put pfcoils/desc_pfcoils/pfelement/pfgeometry/rzdrdz        
if (associated(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz)) then   
   call put_vect3d_double(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzdrdz", &
   cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz, &
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz,1), &
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz,2),&
   size(cpos(1)%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz,3),0)    
   write(*,*) 'Put cpos%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz'
endif

! put pfcoils/coilcurrent/value
if (associated(cpos(1)%pfcoils%coilcurrent%value)) then  
   allocate(vect2DDouble(size(cpos(1)%pfcoils%coilcurrent%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfcoils%coilcurrent%value
   enddo
   call put_vect2D_Double(idx,path, "pfcoils/coilcurrent/value",vect2DDouble, &
   size(cpos(1)%pfcoils%coilcurrent%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pfcoils/coilcurrent/abserror
if (associated(cpos(1)%pfcoils%coilcurrent%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%pfcoils%coilcurrent%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfcoils%coilcurrent%abserror
   enddo
   call put_vect2D_Double(idx,path, "pfcoils/coilcurrent/abserror",vect2DDouble, &
   size(cpos(1)%pfcoils%coilcurrent%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pfcoils/coilcurrent/relerror
if (associated(cpos(1)%pfcoils%coilcurrent%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%pfcoils%coilcurrent%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfcoils%coilcurrent%relerror
   enddo
   call put_vect2D_Double(idx,path, "pfcoils/coilcurrent/relerror",vect2DDouble, &
   size(cpos(1)%pfcoils%coilcurrent%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pfcoils/coilvoltage/value
if (associated(cpos(1)%pfcoils%coilvoltage%value)) then  
   allocate(vect2DDouble(size(cpos(1)%pfcoils%coilvoltage%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfcoils%coilvoltage%value
   enddo
   call put_vect2D_Double(idx,path, "pfcoils/coilvoltage/value",vect2DDouble, &
   size(cpos(1)%pfcoils%coilvoltage%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pfcoils/coilvoltage/abserror
if (associated(cpos(1)%pfcoils%coilvoltage%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%pfcoils%coilvoltage%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfcoils%coilvoltage%abserror
   enddo
   call put_vect2D_Double(idx,path, "pfcoils/coilvoltage/abserror",vect2DDouble, &
   size(cpos(1)%pfcoils%coilvoltage%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pfcoils/coilvoltage/relerror
if (associated(cpos(1)%pfcoils%coilvoltage%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%pfcoils%coilvoltage%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfcoils%coilvoltage%relerror
   enddo
   call put_vect2D_Double(idx,path, "pfcoils/coilvoltage/relerror",vect2DDouble, &
   size(cpos(1)%pfcoils%coilvoltage%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put pfpassive/area
if (associated(cpos(1)%pfpassive%area)) then   
   call put_vect1d_double(idx,path, "pfpassive/area", &
   cpos(1)%pfpassive%area, &
   size(cpos(1)%pfpassive%area),0)
   write(*,*) 'Put cpos%pfpassive%area'
endif

! Put pfpassive/res
if (associated(cpos(1)%pfpassive%res)) then   
   call put_vect1d_double(idx,path, "pfpassive/res", &
   cpos(1)%pfpassive%res, &
   size(cpos(1)%pfpassive%res),0)
   write(*,*) 'Put cpos%pfpassive%res'
endif

! Put pfpassive/pfpageometry/type 
if (associated(cpos(1)%pfpassive%pfpageometry%type)) then         
   call put_vect1d_int(idx,path, "pfpassive/pfpageometry/type",cpos(1)%pfpassive%pfpageometry%type, &
   size(cpos(1)%pfpassive%pfpageometry%type),0) 
   write(*,*) 'Put cpos%pfpassive%pfpageometry%type'
endif

! Put pfpassive/pfpageometry/npoints 
if (associated(cpos(1)%pfpassive%pfpageometry%npoints)) then         
   call put_vect1d_int(idx,path, "pfpassive/pfpageometry/npoints",cpos(1)%pfpassive%pfpageometry%npoints, &
   size(cpos(1)%pfpassive%pfpageometry%npoints),0) 
   write(*,*) 'Put cpos%pfpassive%pfpageometry%npoints'
endif

! Put pfpassive/pfpageometry/rzcoordinate/r        
if (associated(cpos(1)%pfpassive%pfpageometry%rzcoordinate%r)) then   
   call put_vect2d_double(idx,path, "pfpassive/pfpageometry/rzcoordinate/r", &
   cpos(1)%pfpassive%pfpageometry%rzcoordinate%r,&
   size(cpos(1)%pfpassive%pfpageometry%rzcoordinate%r,1),size(cpos(1)%pfpassive%pfpageometry%rzcoordinate%r,2),0)  
   write(*,*) 'Put cpos%pfpassive%pfpageometry%rzcoordinate%r'
endif

! Put pfpassive/pfpageometry/rzcoordinate/z        
if (associated(cpos(1)%pfpassive%pfpageometry%rzcoordinate%z)) then   
   call put_vect2d_double(idx,path, "pfpassive/pfpageometry/rzcoordinate/z", &
   cpos(1)%pfpassive%pfpageometry%rzcoordinate%z,&
   size(cpos(1)%pfpassive%pfpageometry%rzcoordinate%z,1),size(cpos(1)%pfpassive%pfpageometry%rzcoordinate%z,2),0)  
   write(*,*) 'Put cpos%pfpassive%pfpageometry%rzcoordinate%z'
endif

! Put pfpassive/pfpageometry/rzdrdz        
if (associated(cpos(1)%pfpassive%pfpageometry%rzdrdz)) then   
   call put_vect2d_double(idx,path, "pfpassive/pfpageometry/rzdrdz", &
   cpos(1)%pfpassive%pfpageometry%rzdrdz,&
   size(cpos(1)%pfpassive%pfpageometry%rzdrdz,1),size(cpos(1)%pfpassive%pfpageometry%rzdrdz,2),0)  
   write(*,*) 'Put cpos%pfpassive%pfpageometry%rzdrdz'
endif

! Put pfcircuits/name
if (associated(cpos(1)%pfcircuits%name)) then
   dim1 = size(cpos(1)%pfcircuits%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%pfcircuits%name(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcircuits/name", &
         cpos(1)%pfcircuits%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%pfcircuits%name'
endif

! Put pfcircuits/id
if (associated(cpos(1)%pfcircuits%id)) then
   dim1 = size(cpos(1)%pfcircuits%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%pfcircuits%id(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcircuits/id", &
         cpos(1)%pfcircuits%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%pfcircuits%id'
endif

! Put pfcircuits/type
if (associated(cpos(1)%pfcircuits%type)) then
   dim1 = size(cpos(1)%pfcircuits%type)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%pfcircuits%type(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcircuits/type", &
         cpos(1)%pfcircuits%type,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%pfcircuits%type'
endif

! Put pfcircuits/nnodes 
if (associated(cpos(1)%pfcircuits%nnodes)) then         
   call put_vect1d_int(idx,path, "pfcircuits/nnodes",cpos(1)%pfcircuits%nnodes, &
   size(cpos(1)%pfcircuits%nnodes),0) 
   write(*,*) 'Put cpos%pfcircuits%nnodes'
endif

! Put pfcircuits/connections        
if (associated(cpos(1)%pfcircuits%connections)) then   
   call put_vect3d_int(idx,path, "pfcircuits/connections", &
   cpos(1)%pfcircuits%connections, &
   size(cpos(1)%pfcircuits%connections,1), &
   size(cpos(1)%pfcircuits%connections,2),&
   size(cpos(1)%pfcircuits%connections,3),0)    
   write(*,*) 'Put cpos%pfcircuits%connections'
endif

! Put pfsupplies/desc_supply/name
if (associated(cpos(1)%pfsupplies%desc_supply%name)) then
   dim1 = size(cpos(1)%pfsupplies%desc_supply%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%pfsupplies%desc_supply%name(i))
   enddo
   call put_Vect1d_String(idx,path, "pfsupplies/desc_supply/name", &
         cpos(1)%pfsupplies%desc_supply%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%pfsupplies%desc_supply%name'
endif

! Put pfsupplies/desc_supply/id
if (associated(cpos(1)%pfsupplies%desc_supply%id)) then
   dim1 = size(cpos(1)%pfsupplies%desc_supply%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%pfsupplies%desc_supply%id(i))
   enddo
   call put_Vect1d_String(idx,path, "pfsupplies/desc_supply/id", &
         cpos(1)%pfsupplies%desc_supply%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%pfsupplies%desc_supply%id'
endif

! Put pfsupplies/desc_supply/type
if (associated(cpos(1)%pfsupplies%desc_supply%type)) then
   dim1 = size(cpos(1)%pfsupplies%desc_supply%type)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpos(1)%pfsupplies%desc_supply%type(i))
   enddo
   call put_Vect1d_String(idx,path, "pfsupplies/desc_supply/type", &
         cpos(1)%pfsupplies%desc_supply%type,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpos%pfsupplies%desc_supply%type'
endif

! Put pfsupplies/desc_supply/delay
if (associated(cpos(1)%pfsupplies%desc_supply%delay)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/delay", &
   cpos(1)%pfsupplies%desc_supply%delay, &
   size(cpos(1)%pfsupplies%desc_supply%delay),0)
   write(*,*) 'Put cpos%pfsupplies%desc_supply%delay'
endif

! Put pfsupplies/desc_supply/filter/num        
if (associated(cpos(1)%pfsupplies%desc_supply%filter%num)) then   
   call put_vect2d_double(idx,path, "pfsupplies/desc_supply/filter/num", &
   cpos(1)%pfsupplies%desc_supply%filter%num,&
   size(cpos(1)%pfsupplies%desc_supply%filter%num,1),size(cpos(1)%pfsupplies%desc_supply%filter%num,2),0)  
   write(*,*) 'Put cpos%pfsupplies%desc_supply%filter%num'
endif

! Put pfsupplies/desc_supply/filter/den        
if (associated(cpos(1)%pfsupplies%desc_supply%filter%den)) then   
   call put_vect2d_double(idx,path, "pfsupplies/desc_supply/filter/den", &
   cpos(1)%pfsupplies%desc_supply%filter%den,&
   size(cpos(1)%pfsupplies%desc_supply%filter%den,1),size(cpos(1)%pfsupplies%desc_supply%filter%den,2),0)  
   write(*,*) 'Put cpos%pfsupplies%desc_supply%filter%den'
endif

! Put pfsupplies/desc_supply/imin
if (associated(cpos(1)%pfsupplies%desc_supply%imin)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/imin", &
   cpos(1)%pfsupplies%desc_supply%imin, &
   size(cpos(1)%pfsupplies%desc_supply%imin),0)
   write(*,*) 'Put cpos%pfsupplies%desc_supply%imin'
endif

! Put pfsupplies/desc_supply/imax
if (associated(cpos(1)%pfsupplies%desc_supply%imax)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/imax", &
   cpos(1)%pfsupplies%desc_supply%imax, &
   size(cpos(1)%pfsupplies%desc_supply%imax),0)
   write(*,*) 'Put cpos%pfsupplies%desc_supply%imax'
endif

! Put pfsupplies/desc_supply/res
if (associated(cpos(1)%pfsupplies%desc_supply%res)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/res", &
   cpos(1)%pfsupplies%desc_supply%res, &
   size(cpos(1)%pfsupplies%desc_supply%res),0)
   write(*,*) 'Put cpos%pfsupplies%desc_supply%res'
endif

! Put pfsupplies/desc_supply/umin
if (associated(cpos(1)%pfsupplies%desc_supply%umin)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/umin", &
   cpos(1)%pfsupplies%desc_supply%umin, &
   size(cpos(1)%pfsupplies%desc_supply%umin),0)
   write(*,*) 'Put cpos%pfsupplies%desc_supply%umin'
endif

! Put pfsupplies/desc_supply/umax
if (associated(cpos(1)%pfsupplies%desc_supply%umax)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/umax", &
   cpos(1)%pfsupplies%desc_supply%umax, &
   size(cpos(1)%pfsupplies%desc_supply%umax),0)
   write(*,*) 'Put cpos%pfsupplies%desc_supply%umax'
endif

! Put pfsupplies/desc_supply/emax
if (associated(cpos(1)%pfsupplies%desc_supply%emax)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/emax", &
   cpos(1)%pfsupplies%desc_supply%emax, &
   size(cpos(1)%pfsupplies%desc_supply%emax),0)
   write(*,*) 'Put cpos%pfsupplies%desc_supply%emax'
endif

! put pfsupplies/voltage/value
if (associated(cpos(1)%pfsupplies%voltage%value)) then  
   allocate(vect2DDouble(size(cpos(1)%pfsupplies%voltage%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfsupplies%voltage%value
   enddo
   call put_vect2D_Double(idx,path, "pfsupplies/voltage/value",vect2DDouble, &
   size(cpos(1)%pfsupplies%voltage%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pfsupplies/voltage/abserror
if (associated(cpos(1)%pfsupplies%voltage%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%pfsupplies%voltage%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfsupplies%voltage%abserror
   enddo
   call put_vect2D_Double(idx,path, "pfsupplies/voltage/abserror",vect2DDouble, &
   size(cpos(1)%pfsupplies%voltage%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pfsupplies/voltage/relerror
if (associated(cpos(1)%pfsupplies%voltage%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%pfsupplies%voltage%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfsupplies%voltage%relerror
   enddo
   call put_vect2D_Double(idx,path, "pfsupplies/voltage/relerror",vect2DDouble, &
   size(cpos(1)%pfsupplies%voltage%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pfsupplies/current/value
if (associated(cpos(1)%pfsupplies%current%value)) then  
   allocate(vect2DDouble(size(cpos(1)%pfsupplies%current%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfsupplies%current%value
   enddo
   call put_vect2D_Double(idx,path, "pfsupplies/current/value",vect2DDouble, &
   size(cpos(1)%pfsupplies%current%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pfsupplies/current/abserror
if (associated(cpos(1)%pfsupplies%current%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%pfsupplies%current%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfsupplies%current%abserror
   enddo
   call put_vect2D_Double(idx,path, "pfsupplies/current/abserror",vect2DDouble, &
   size(cpos(1)%pfsupplies%current%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put pfsupplies/current/relerror
if (associated(cpos(1)%pfsupplies%current%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%pfsupplies%current%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%pfsupplies%current%relerror
   enddo
   call put_vect2D_Double(idx,path, "pfsupplies/current/relerror",vect2DDouble, &
   size(cpos(1)%pfsupplies%current%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_polardiag(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_lineintegraldiag),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! Put datainfo/dataprovider
if (associated(cpos(1)%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%datainfo%dataprovider'
endif

! Put datainfo/putdate
if (associated(cpos(1)%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putdate'
endif

! Put datainfo/source
if (associated(cpos(1)%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%datainfo%source'
endif

! Put datainfo/comment
if (associated(cpos(1)%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%datainfo%comment'
endif

! Put datainfo/isref        
if (cpos(1)%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpos(1)%datainfo%isref)        
   write(*,*) 'Put cpos%datainfo%isref'
endif

! Put datainfo/whatref        
if (cpos(1)%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpos(1)%datainfo%whatref)        
   write(*,*) 'Put cpos%datainfo%whatref'
endif

! Put datainfo/putinfo/putmethod
if (associated(cpos(1)%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putmethod'
endif

! Put datainfo/putinfo/putaccess
if (associated(cpos(1)%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putaccess'
endif

! Put datainfo/putinfo/putlocation
if (associated(cpos(1)%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putlocation'
endif

! Put datainfo/putinfo/rights
if (associated(cpos(1)%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%rights'
endif

! Put expression
if (associated(cpos(1)%expression)) then
   longstring = ' '    
   lenstring = size(cpos(1)%expression)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%expression(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%expression(istring)
      enddo
   endif
   call put_string(idx,path, "expression",trim(longstring))
   write(*,*) 'Put cpos%expression'
endif

! Put setup_line/pivot_point/r
if (associated(cpos(1)%setup_line%pivot_point%r)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/r", &
   cpos(1)%setup_line%pivot_point%r, &
   size(cpos(1)%setup_line%pivot_point%r),0)
   write(*,*) 'Put cpos%setup_line%pivot_point%r'
endif

! Put setup_line/pivot_point/z
if (associated(cpos(1)%setup_line%pivot_point%z)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/z", &
   cpos(1)%setup_line%pivot_point%z, &
   size(cpos(1)%setup_line%pivot_point%z),0)
   write(*,*) 'Put cpos%setup_line%pivot_point%z'
endif

! Put setup_line/pivot_point/phi
if (associated(cpos(1)%setup_line%pivot_point%phi)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/phi", &
   cpos(1)%setup_line%pivot_point%phi, &
   size(cpos(1)%setup_line%pivot_point%phi),0)
   write(*,*) 'Put cpos%setup_line%pivot_point%phi'
endif

! Put setup_line/polchordang
if (associated(cpos(1)%setup_line%polchordang)) then   
   call put_vect1d_double(idx,path, "setup_line/polchordang", &
   cpos(1)%setup_line%polchordang, &
   size(cpos(1)%setup_line%polchordang),0)
   write(*,*) 'Put cpos%setup_line%polchordang'
endif

! Put setup_line/torchordang
if (associated(cpos(1)%setup_line%torchordang)) then   
   call put_vect1d_double(idx,path, "setup_line/torchordang", &
   cpos(1)%setup_line%torchordang, &
   size(cpos(1)%setup_line%torchordang),0)
   write(*,*) 'Put cpos%setup_line%torchordang'
endif

! Put setup_line/second_point/r
if (associated(cpos(1)%setup_line%second_point%r)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/r", &
   cpos(1)%setup_line%second_point%r, &
   size(cpos(1)%setup_line%second_point%r),0)
   write(*,*) 'Put cpos%setup_line%second_point%r'
endif

! Put setup_line/second_point/z
if (associated(cpos(1)%setup_line%second_point%z)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/z", &
   cpos(1)%setup_line%second_point%z, &
   size(cpos(1)%setup_line%second_point%z),0)
   write(*,*) 'Put cpos%setup_line%second_point%z'
endif

! Put setup_line/second_point/phi
if (associated(cpos(1)%setup_line%second_point%phi)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/phi", &
   cpos(1)%setup_line%second_point%phi, &
   size(cpos(1)%setup_line%second_point%phi),0)
   write(*,*) 'Put cpos%setup_line%second_point%phi'
endif

! Put setup_line/nchordpoints        
if (cpos(1)%setup_line%nchordpoints.NE.-999999999) then
   call put_int(idx,path, "setup_line/nchordpoints",cpos(1)%setup_line%nchordpoints)        
   write(*,*) 'Put cpos%setup_line%nchordpoints'
endif

! put measure/value
if (associated(cpos(1)%measure%value)) then  
   allocate(vect2DDouble(size(cpos(1)%measure%value,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%measure%value
   enddo
   call put_vect2D_Double(idx,path, "measure/value",vect2DDouble, &
   size(cpos(1)%measure%value,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put measure/abserror
if (associated(cpos(1)%measure%abserror)) then  
   allocate(vect2DDouble(size(cpos(1)%measure%abserror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%measure%abserror
   enddo
   call put_vect2D_Double(idx,path, "measure/abserror",vect2DDouble, &
   size(cpos(1)%measure%abserror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! put measure/relerror
if (associated(cpos(1)%measure%relerror)) then  
   allocate(vect2DDouble(size(cpos(1)%measure%relerror,1),lentime))
   do itime=1,lentime
      vect2DDouble(:,itime) = cpos(itime)%measure%relerror
   enddo
   call put_vect2D_Double(idx,path, "measure/relerror",vect2DDouble, &
   size(cpos(1)%measure%relerror,1),lentime,1)           
   deallocate(vect2DDouble)
endif

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_toroidfield(idx, path,  cpos)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime

type(type_toroidfield),pointer :: cpos(:)       
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


! find the length of the time base
lentime = size(cpos)
! find time vector
allocate(time(lentime))        
time(1:lentime) = cpos(1:lentime)%time    

call begin_cpo_put_timed(idx, path,lentime,time)
deallocate(time)

! Put datainfo/dataprovider
if (associated(cpos(1)%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))
   write(*,*) 'Put cpos%datainfo%dataprovider'
endif

! Put datainfo/putdate
if (associated(cpos(1)%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putdate'
endif

! Put datainfo/source
if (associated(cpos(1)%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))
   write(*,*) 'Put cpos%datainfo%source'
endif

! Put datainfo/comment
if (associated(cpos(1)%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))
   write(*,*) 'Put cpos%datainfo%comment'
endif

! Put datainfo/isref        
if (cpos(1)%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpos(1)%datainfo%isref)        
   write(*,*) 'Put cpos%datainfo%isref'
endif

! Put datainfo/whatref        
if (cpos(1)%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpos(1)%datainfo%whatref)        
   write(*,*) 'Put cpos%datainfo%whatref'
endif

! Put datainfo/putinfo/putmethod
if (associated(cpos(1)%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putmethod'
endif

! Put datainfo/putinfo/putaccess
if (associated(cpos(1)%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putaccess'
endif

! Put datainfo/putinfo/putlocation
if (associated(cpos(1)%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%putlocation'
endif

! Put datainfo/putinfo/rights
if (associated(cpos(1)%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpos(1)%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpos(1)%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpos(1)%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))
   write(*,*) 'Put cpos%datainfo%putinfo%rights'
endif

! Put nturns        
if (cpos(1)%nturns.NE.-999999999) then
   call put_int(idx,path, "nturns",cpos(1)%nturns)        
   write(*,*) 'Put cpos%nturns'
endif

! Put ncoils        
if (cpos(1)%ncoils.NE.-999999999) then
   call put_int(idx,path, "ncoils",cpos(1)%ncoils)        
   write(*,*) 'Put cpos%ncoils'
endif

! Put current/value  
if (any(cpos(1:lentime)%current%value/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%current%value    
   call put_vect1D_double(idx,path, "current/value",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put current/abserror  
if (any(cpos(1:lentime)%current%abserror/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%current%abserror    
   call put_vect1D_double(idx,path, "current/abserror",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put current/relerror  
if (any(cpos(1:lentime)%current%relerror/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%current%relerror    
   call put_vect1D_double(idx,path, "current/relerror",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put bvac_r/value  
if (any(cpos(1:lentime)%bvac_r%value/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%bvac_r%value    
   call put_vect1D_double(idx,path, "bvac_r/value",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put bvac_r/abserror  
if (any(cpos(1:lentime)%bvac_r%abserror/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%bvac_r%abserror    
   call put_vect1D_double(idx,path, "bvac_r/abserror",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put bvac_r/relerror  
if (any(cpos(1:lentime)%bvac_r%relerror/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%bvac_r%relerror    
   call put_vect1D_double(idx,path, "bvac_r/relerror",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

! Put time  
if (any(cpos(1:lentime)%time/=-9.D40))  then
   allocate(vect1DDouble(lentime))        
   vect1DDouble(1:lentime) = cpos(1:lentime)%time    
   call put_vect1D_double(idx,path, "time",vect1DDouble,lentime,1)           
   deallocate(vect1DDouble)
endif 

call end_cpo_put_timed(idx, path)

return
endsubroutine

subroutine euITM_put_vessel(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_vessel) :: cpo       
call begin_cpo_put_non_timed(idx, path)

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put position/r
if (associated(cpo%position%r)) then   
   call put_vect1d_double(idx,path, "position/r",cpo%position%r,&
   size(cpo%position%r),0)
   write(*,*) 'Put cpo%position%r',cpo%position%r
endif

! Put position/z
if (associated(cpo%position%z)) then   
   call put_vect1d_double(idx,path, "position/z",cpo%position%z,&
   size(cpo%position%z),0)
   write(*,*) 'Put cpo%position%z',cpo%position%z
endif

call end_cpo_put_non_timed(idx, path)

return
endsubroutine


!!!!!! Routines to PUT_SLICE one time slice of a time-dependent CPO (affects only time-dependent fields)

subroutine euITM_put_slice_controllers(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_controllers) :: cpo
    
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put target
if (associated(cpo%target)) then   
   call put_vect1d_double_slice(idx,path, "target",cpo%target,&
   size(cpo%target),cpo%time)
   write(*,*) 'Put cpo%target',cpo%target
endif

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine

subroutine euITM_put_slice_coreprof(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_coreprof) :: cpo
    
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put rho_tor
if (associated(cpo%rho_tor)) then   
   call put_vect1d_double_slice(idx,path, "rho_tor",cpo%rho_tor,&
   size(cpo%rho_tor),cpo%time)
   write(*,*) 'Put cpo%rho_tor',cpo%rho_tor
endif

! Put psi
if (associated(cpo%psi)) then   
   call put_vect1d_double_slice(idx,path, "psi",cpo%psi,&
   size(cpo%psi),cpo%time)
   write(*,*) 'Put cpo%psi',cpo%psi
endif

! Put pressure/value
if (associated(cpo%pressure%value)) then   
   call put_vect1d_double_slice(idx,path, "pressure/value",cpo%pressure%value,&
   size(cpo%pressure%value),cpo%time)
   write(*,*) 'Put cpo%pressure%value',cpo%pressure%value
endif

! Put pressure/source  ERROR : NO TIME DEPENDENT STRING EXPECTED IN THE DATA STRUCTURE

! Put jparallel/value
if (associated(cpo%jparallel%value)) then   
   call put_vect1d_double_slice(idx,path, "jparallel/value",cpo%jparallel%value,&
   size(cpo%jparallel%value),cpo%time)
   write(*,*) 'Put cpo%jparallel%value',cpo%jparallel%value
endif

! Put jparallel/source  ERROR : NO TIME DEPENDENT STRING EXPECTED IN THE DATA STRUCTURE

! Put q/value
if (associated(cpo%q%value)) then   
   call put_vect1d_double_slice(idx,path, "q/value",cpo%q%value,&
   size(cpo%q%value),cpo%time)
   write(*,*) 'Put cpo%q%value',cpo%q%value
endif

! Put q/source  ERROR : NO TIME DEPENDENT STRING EXPECTED IN THE DATA STRUCTURE

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine

subroutine euITM_put_slice_equilibrium(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_equilibrium) :: cpo
    
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put eqconstraint/bvac_r/time
if (cpo%eqconstraint%bvac_r%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqconstraint/bvac_r/time",cpo%eqconstraint%bvac_r%time,cpo%time)  
   write(*,*) 'Put cpo%eqconstraint%bvac_r%time',cpo%eqconstraint%bvac_r%time
endif

! Put eqconstraint/bvac_r/weight
if (cpo%eqconstraint%bvac_r%weight.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqconstraint/bvac_r/weight",cpo%eqconstraint%bvac_r%weight,cpo%time)  
   write(*,*) 'Put cpo%eqconstraint%bvac_r%weight',cpo%eqconstraint%bvac_r%weight
endif

! Put eqconstraint/bvac_r/sigma
if (cpo%eqconstraint%bvac_r%sigma.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqconstraint/bvac_r/sigma",cpo%eqconstraint%bvac_r%sigma,cpo%time)  
   write(*,*) 'Put cpo%eqconstraint%bvac_r%sigma',cpo%eqconstraint%bvac_r%sigma
endif

! Put eqconstraint/bvac_r/calculated
if (cpo%eqconstraint%bvac_r%calculated.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqconstraint/bvac_r/calculated",cpo%eqconstraint%bvac_r%calculated,cpo%time)  
   write(*,*) 'Put cpo%eqconstraint%bvac_r%calculated',cpo%eqconstraint%bvac_r%calculated
endif

! Put eqconstraint/bvac_r/chi2
if (cpo%eqconstraint%bvac_r%chi2.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqconstraint/bvac_r/chi2",cpo%eqconstraint%bvac_r%chi2,cpo%time)  
   write(*,*) 'Put cpo%eqconstraint%bvac_r%chi2',cpo%eqconstraint%bvac_r%chi2
endif

! Put eqconstraint/i_plasma/time
if (cpo%eqconstraint%i_plasma%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqconstraint/i_plasma/time",cpo%eqconstraint%i_plasma%time,cpo%time)  
   write(*,*) 'Put cpo%eqconstraint%i_plasma%time',cpo%eqconstraint%i_plasma%time
endif

! Put eqconstraint/i_plasma/weight
if (cpo%eqconstraint%i_plasma%weight.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqconstraint/i_plasma/weight",cpo%eqconstraint%i_plasma%weight,cpo%time)  
   write(*,*) 'Put cpo%eqconstraint%i_plasma%weight',cpo%eqconstraint%i_plasma%weight
endif

! Put eqconstraint/i_plasma/sigma
if (cpo%eqconstraint%i_plasma%sigma.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqconstraint/i_plasma/sigma",cpo%eqconstraint%i_plasma%sigma,cpo%time)  
   write(*,*) 'Put cpo%eqconstraint%i_plasma%sigma',cpo%eqconstraint%i_plasma%sigma
endif

! Put eqconstraint/i_plasma/calculated
if (cpo%eqconstraint%i_plasma%calculated.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqconstraint/i_plasma/calculated",cpo%eqconstraint%i_plasma%calculated,cpo%time)  
   write(*,*) 'Put cpo%eqconstraint%i_plasma%calculated',cpo%eqconstraint%i_plasma%calculated
endif

! Put eqconstraint/i_plasma/chi2
if (cpo%eqconstraint%i_plasma%chi2.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqconstraint/i_plasma/chi2",cpo%eqconstraint%i_plasma%chi2,cpo%time)  
   write(*,*) 'Put cpo%eqconstraint%i_plasma%chi2',cpo%eqconstraint%i_plasma%chi2
endif

! Put eqconstraint/magnet_iron/mr/time
if (associated(cpo%eqconstraint%magnet_iron%mr%time)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/magnet_iron/mr/time",cpo%eqconstraint%magnet_iron%mr%time,&
   size(cpo%eqconstraint%magnet_iron%mr%time),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mr%time',cpo%eqconstraint%magnet_iron%mr%time
endif

! Put eqconstraint/magnet_iron/mr/exact 
if (associated(cpo%eqconstraint%magnet_iron%mr%exact)) then         
   call put_vect1d_int_slice(idx,path, "eqconstraint/magnet_iron/mr/exact",cpo%eqconstraint%magnet_iron%mr%exact,&
   size(cpo%eqconstraint%magnet_iron%mr%exact),cpo%time) 
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mr%exact',cpo%eqconstraint%magnet_iron%mr%exact
endif

! Put eqconstraint/magnet_iron/mr/weight
if (associated(cpo%eqconstraint%magnet_iron%mr%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/magnet_iron/mr/weight",cpo%eqconstraint%magnet_iron%mr%weight,&
   size(cpo%eqconstraint%magnet_iron%mr%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mr%weight',cpo%eqconstraint%magnet_iron%mr%weight
endif

! Put eqconstraint/magnet_iron/mr/sigma
if (associated(cpo%eqconstraint%magnet_iron%mr%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/magnet_iron/mr/sigma",cpo%eqconstraint%magnet_iron%mr%sigma,&
   size(cpo%eqconstraint%magnet_iron%mr%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mr%sigma',cpo%eqconstraint%magnet_iron%mr%sigma
endif

! Put eqconstraint/magnet_iron/mr/calculated
if (associated(cpo%eqconstraint%magnet_iron%mr%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/magnet_iron/mr/calculated",cpo%eqconstraint%magnet_iron%mr%calculated,&
   size(cpo%eqconstraint%magnet_iron%mr%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mr%calculated',cpo%eqconstraint%magnet_iron%mr%calculated
endif

! Put eqconstraint/magnet_iron/mr/chi2
if (associated(cpo%eqconstraint%magnet_iron%mr%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/magnet_iron/mr/chi2",cpo%eqconstraint%magnet_iron%mr%chi2,&
   size(cpo%eqconstraint%magnet_iron%mr%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mr%chi2',cpo%eqconstraint%magnet_iron%mr%chi2
endif

! Put eqconstraint/magnet_iron/mz/time
if (associated(cpo%eqconstraint%magnet_iron%mz%time)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/magnet_iron/mz/time",cpo%eqconstraint%magnet_iron%mz%time,&
   size(cpo%eqconstraint%magnet_iron%mz%time),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mz%time',cpo%eqconstraint%magnet_iron%mz%time
endif

! Put eqconstraint/magnet_iron/mz/exact 
if (associated(cpo%eqconstraint%magnet_iron%mz%exact)) then         
   call put_vect1d_int_slice(idx,path, "eqconstraint/magnet_iron/mz/exact",cpo%eqconstraint%magnet_iron%mz%exact,&
   size(cpo%eqconstraint%magnet_iron%mz%exact),cpo%time) 
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mz%exact',cpo%eqconstraint%magnet_iron%mz%exact
endif

! Put eqconstraint/magnet_iron/mz/weight
if (associated(cpo%eqconstraint%magnet_iron%mz%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/magnet_iron/mz/weight",cpo%eqconstraint%magnet_iron%mz%weight,&
   size(cpo%eqconstraint%magnet_iron%mz%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mz%weight',cpo%eqconstraint%magnet_iron%mz%weight
endif

! Put eqconstraint/magnet_iron/mz/sigma
if (associated(cpo%eqconstraint%magnet_iron%mz%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/magnet_iron/mz/sigma",cpo%eqconstraint%magnet_iron%mz%sigma,&
   size(cpo%eqconstraint%magnet_iron%mz%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mz%sigma',cpo%eqconstraint%magnet_iron%mz%sigma
endif

! Put eqconstraint/magnet_iron/mz/calculated
if (associated(cpo%eqconstraint%magnet_iron%mz%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/magnet_iron/mz/calculated",cpo%eqconstraint%magnet_iron%mz%calculated,&
   size(cpo%eqconstraint%magnet_iron%mz%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mz%calculated',cpo%eqconstraint%magnet_iron%mz%calculated
endif

! Put eqconstraint/magnet_iron/mz/chi2
if (associated(cpo%eqconstraint%magnet_iron%mz%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/magnet_iron/mz/chi2",cpo%eqconstraint%magnet_iron%mz%chi2,&
   size(cpo%eqconstraint%magnet_iron%mz%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mz%chi2',cpo%eqconstraint%magnet_iron%mz%chi2
endif

! Put eqconstraint/bpol/time
if (associated(cpo%eqconstraint%bpol%time)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/bpol/time",cpo%eqconstraint%bpol%time,&
   size(cpo%eqconstraint%bpol%time),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%bpol%time',cpo%eqconstraint%bpol%time
endif

! Put eqconstraint/bpol/exact 
if (associated(cpo%eqconstraint%bpol%exact)) then         
   call put_vect1d_int_slice(idx,path, "eqconstraint/bpol/exact",cpo%eqconstraint%bpol%exact,&
   size(cpo%eqconstraint%bpol%exact),cpo%time) 
   write(*,*) 'Put cpo%eqconstraint%bpol%exact',cpo%eqconstraint%bpol%exact
endif

! Put eqconstraint/bpol/weight
if (associated(cpo%eqconstraint%bpol%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/bpol/weight",cpo%eqconstraint%bpol%weight,&
   size(cpo%eqconstraint%bpol%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%bpol%weight',cpo%eqconstraint%bpol%weight
endif

! Put eqconstraint/bpol/sigma
if (associated(cpo%eqconstraint%bpol%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/bpol/sigma",cpo%eqconstraint%bpol%sigma,&
   size(cpo%eqconstraint%bpol%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%bpol%sigma',cpo%eqconstraint%bpol%sigma
endif

! Put eqconstraint/bpol/calculated
if (associated(cpo%eqconstraint%bpol%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/bpol/calculated",cpo%eqconstraint%bpol%calculated,&
   size(cpo%eqconstraint%bpol%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%bpol%calculated',cpo%eqconstraint%bpol%calculated
endif

! Put eqconstraint/bpol/chi2
if (associated(cpo%eqconstraint%bpol%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/bpol/chi2",cpo%eqconstraint%bpol%chi2,&
   size(cpo%eqconstraint%bpol%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%bpol%chi2',cpo%eqconstraint%bpol%chi2
endif

! Put eqconstraint/flux/time
if (associated(cpo%eqconstraint%flux%time)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/flux/time",cpo%eqconstraint%flux%time,&
   size(cpo%eqconstraint%flux%time),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%flux%time',cpo%eqconstraint%flux%time
endif

! Put eqconstraint/flux/exact 
if (associated(cpo%eqconstraint%flux%exact)) then         
   call put_vect1d_int_slice(idx,path, "eqconstraint/flux/exact",cpo%eqconstraint%flux%exact,&
   size(cpo%eqconstraint%flux%exact),cpo%time) 
   write(*,*) 'Put cpo%eqconstraint%flux%exact',cpo%eqconstraint%flux%exact
endif

! Put eqconstraint/flux/weight
if (associated(cpo%eqconstraint%flux%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/flux/weight",cpo%eqconstraint%flux%weight,&
   size(cpo%eqconstraint%flux%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%flux%weight',cpo%eqconstraint%flux%weight
endif

! Put eqconstraint/flux/sigma
if (associated(cpo%eqconstraint%flux%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/flux/sigma",cpo%eqconstraint%flux%sigma,&
   size(cpo%eqconstraint%flux%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%flux%sigma',cpo%eqconstraint%flux%sigma
endif

! Put eqconstraint/flux/calculated
if (associated(cpo%eqconstraint%flux%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/flux/calculated",cpo%eqconstraint%flux%calculated,&
   size(cpo%eqconstraint%flux%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%flux%calculated',cpo%eqconstraint%flux%calculated
endif

! Put eqconstraint/flux/chi2
if (associated(cpo%eqconstraint%flux%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/flux/chi2",cpo%eqconstraint%flux%chi2,&
   size(cpo%eqconstraint%flux%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%flux%chi2',cpo%eqconstraint%flux%chi2
endif

! Put eqconstraint/mse/time
if (associated(cpo%eqconstraint%mse%time)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/mse/time",cpo%eqconstraint%mse%time,&
   size(cpo%eqconstraint%mse%time),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%mse%time',cpo%eqconstraint%mse%time
endif

! Put eqconstraint/mse/exact 
if (associated(cpo%eqconstraint%mse%exact)) then         
   call put_vect1d_int_slice(idx,path, "eqconstraint/mse/exact",cpo%eqconstraint%mse%exact,&
   size(cpo%eqconstraint%mse%exact),cpo%time) 
   write(*,*) 'Put cpo%eqconstraint%mse%exact',cpo%eqconstraint%mse%exact
endif

! Put eqconstraint/mse/weight
if (associated(cpo%eqconstraint%mse%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/mse/weight",cpo%eqconstraint%mse%weight,&
   size(cpo%eqconstraint%mse%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%mse%weight',cpo%eqconstraint%mse%weight
endif

! Put eqconstraint/mse/sigma
if (associated(cpo%eqconstraint%mse%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/mse/sigma",cpo%eqconstraint%mse%sigma,&
   size(cpo%eqconstraint%mse%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%mse%sigma',cpo%eqconstraint%mse%sigma
endif

! Put eqconstraint/mse/calculated
if (associated(cpo%eqconstraint%mse%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/mse/calculated",cpo%eqconstraint%mse%calculated,&
   size(cpo%eqconstraint%mse%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%mse%calculated',cpo%eqconstraint%mse%calculated
endif

! Put eqconstraint/mse/chi2
if (associated(cpo%eqconstraint%mse%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/mse/chi2",cpo%eqconstraint%mse%chi2,&
   size(cpo%eqconstraint%mse%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%mse%chi2',cpo%eqconstraint%mse%chi2
endif

! Put eqconstraint/faraday/time
if (associated(cpo%eqconstraint%faraday%time)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/faraday/time",cpo%eqconstraint%faraday%time,&
   size(cpo%eqconstraint%faraday%time),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%faraday%time',cpo%eqconstraint%faraday%time
endif

! Put eqconstraint/faraday/exact 
if (associated(cpo%eqconstraint%faraday%exact)) then         
   call put_vect1d_int_slice(idx,path, "eqconstraint/faraday/exact",cpo%eqconstraint%faraday%exact,&
   size(cpo%eqconstraint%faraday%exact),cpo%time) 
   write(*,*) 'Put cpo%eqconstraint%faraday%exact',cpo%eqconstraint%faraday%exact
endif

! Put eqconstraint/faraday/weight
if (associated(cpo%eqconstraint%faraday%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/faraday/weight",cpo%eqconstraint%faraday%weight,&
   size(cpo%eqconstraint%faraday%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%faraday%weight',cpo%eqconstraint%faraday%weight
endif

! Put eqconstraint/faraday/sigma
if (associated(cpo%eqconstraint%faraday%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/faraday/sigma",cpo%eqconstraint%faraday%sigma,&
   size(cpo%eqconstraint%faraday%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%faraday%sigma',cpo%eqconstraint%faraday%sigma
endif

! Put eqconstraint/faraday/calculated
if (associated(cpo%eqconstraint%faraday%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/faraday/calculated",cpo%eqconstraint%faraday%calculated,&
   size(cpo%eqconstraint%faraday%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%faraday%calculated',cpo%eqconstraint%faraday%calculated
endif

! Put eqconstraint/faraday/chi2
if (associated(cpo%eqconstraint%faraday%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/faraday/chi2",cpo%eqconstraint%faraday%chi2,&
   size(cpo%eqconstraint%faraday%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%faraday%chi2',cpo%eqconstraint%faraday%chi2
endif

! Put eqconstraint/pfcurrent/time
if (associated(cpo%eqconstraint%pfcurrent%time)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/pfcurrent/time",cpo%eqconstraint%pfcurrent%time,&
   size(cpo%eqconstraint%pfcurrent%time),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%pfcurrent%time',cpo%eqconstraint%pfcurrent%time
endif

! Put eqconstraint/pfcurrent/exact 
if (associated(cpo%eqconstraint%pfcurrent%exact)) then         
   call put_vect1d_int_slice(idx,path, "eqconstraint/pfcurrent/exact",cpo%eqconstraint%pfcurrent%exact,&
   size(cpo%eqconstraint%pfcurrent%exact),cpo%time) 
   write(*,*) 'Put cpo%eqconstraint%pfcurrent%exact',cpo%eqconstraint%pfcurrent%exact
endif

! Put eqconstraint/pfcurrent/weight
if (associated(cpo%eqconstraint%pfcurrent%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/pfcurrent/weight",cpo%eqconstraint%pfcurrent%weight,&
   size(cpo%eqconstraint%pfcurrent%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%pfcurrent%weight',cpo%eqconstraint%pfcurrent%weight
endif

! Put eqconstraint/pfcurrent/sigma
if (associated(cpo%eqconstraint%pfcurrent%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/pfcurrent/sigma",cpo%eqconstraint%pfcurrent%sigma,&
   size(cpo%eqconstraint%pfcurrent%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%pfcurrent%sigma',cpo%eqconstraint%pfcurrent%sigma
endif

! Put eqconstraint/pfcurrent/calculated
if (associated(cpo%eqconstraint%pfcurrent%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/pfcurrent/calculated",cpo%eqconstraint%pfcurrent%calculated,&
   size(cpo%eqconstraint%pfcurrent%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%pfcurrent%calculated',cpo%eqconstraint%pfcurrent%calculated
endif

! Put eqconstraint/pfcurrent/chi2
if (associated(cpo%eqconstraint%pfcurrent%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/pfcurrent/chi2",cpo%eqconstraint%pfcurrent%chi2,&
   size(cpo%eqconstraint%pfcurrent%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%pfcurrent%chi2',cpo%eqconstraint%pfcurrent%chi2
endif

! Put eqconstraint/pressure/time
if (associated(cpo%eqconstraint%pressure%time)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/pressure/time",cpo%eqconstraint%pressure%time,&
   size(cpo%eqconstraint%pressure%time),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%pressure%time',cpo%eqconstraint%pressure%time
endif

! Put eqconstraint/pressure/exact 
if (associated(cpo%eqconstraint%pressure%exact)) then         
   call put_vect1d_int_slice(idx,path, "eqconstraint/pressure/exact",cpo%eqconstraint%pressure%exact,&
   size(cpo%eqconstraint%pressure%exact),cpo%time) 
   write(*,*) 'Put cpo%eqconstraint%pressure%exact',cpo%eqconstraint%pressure%exact
endif

! Put eqconstraint/pressure/weight
if (associated(cpo%eqconstraint%pressure%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/pressure/weight",cpo%eqconstraint%pressure%weight,&
   size(cpo%eqconstraint%pressure%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%pressure%weight',cpo%eqconstraint%pressure%weight
endif

! Put eqconstraint/pressure/sigma
if (associated(cpo%eqconstraint%pressure%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/pressure/sigma",cpo%eqconstraint%pressure%sigma,&
   size(cpo%eqconstraint%pressure%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%pressure%sigma',cpo%eqconstraint%pressure%sigma
endif

! Put eqconstraint/pressure/calculated
if (associated(cpo%eqconstraint%pressure%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/pressure/calculated",cpo%eqconstraint%pressure%calculated,&
   size(cpo%eqconstraint%pressure%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%pressure%calculated',cpo%eqconstraint%pressure%calculated
endif

! Put eqconstraint/pressure/chi2
if (associated(cpo%eqconstraint%pressure%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/pressure/chi2",cpo%eqconstraint%pressure%chi2,&
   size(cpo%eqconstraint%pressure%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%pressure%chi2',cpo%eqconstraint%pressure%chi2
endif

! Put eqconstraint/jsurf/time
if (associated(cpo%eqconstraint%jsurf%time)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/jsurf/time",cpo%eqconstraint%jsurf%time,&
   size(cpo%eqconstraint%jsurf%time),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%jsurf%time',cpo%eqconstraint%jsurf%time
endif

! Put eqconstraint/jsurf/exact 
if (associated(cpo%eqconstraint%jsurf%exact)) then         
   call put_vect1d_int_slice(idx,path, "eqconstraint/jsurf/exact",cpo%eqconstraint%jsurf%exact,&
   size(cpo%eqconstraint%jsurf%exact),cpo%time) 
   write(*,*) 'Put cpo%eqconstraint%jsurf%exact',cpo%eqconstraint%jsurf%exact
endif

! Put eqconstraint/jsurf/weight
if (associated(cpo%eqconstraint%jsurf%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/jsurf/weight",cpo%eqconstraint%jsurf%weight,&
   size(cpo%eqconstraint%jsurf%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%jsurf%weight',cpo%eqconstraint%jsurf%weight
endif

! Put eqconstraint/jsurf/sigma
if (associated(cpo%eqconstraint%jsurf%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/jsurf/sigma",cpo%eqconstraint%jsurf%sigma,&
   size(cpo%eqconstraint%jsurf%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%jsurf%sigma',cpo%eqconstraint%jsurf%sigma
endif

! Put eqconstraint/jsurf/calculated
if (associated(cpo%eqconstraint%jsurf%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/jsurf/calculated",cpo%eqconstraint%jsurf%calculated,&
   size(cpo%eqconstraint%jsurf%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%jsurf%calculated',cpo%eqconstraint%jsurf%calculated
endif

! Put eqconstraint/jsurf/chi2
if (associated(cpo%eqconstraint%jsurf%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/jsurf/chi2",cpo%eqconstraint%jsurf%chi2,&
   size(cpo%eqconstraint%jsurf%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%jsurf%chi2',cpo%eqconstraint%jsurf%chi2
endif

! Put eqconstraint/q/qvalue
if (associated(cpo%eqconstraint%q%qvalue)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/q/qvalue",cpo%eqconstraint%q%qvalue,&
   size(cpo%eqconstraint%q%qvalue),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%q%qvalue',cpo%eqconstraint%q%qvalue
endif

! Put eqconstraint/q/position/r
if (associated(cpo%eqconstraint%q%position%r)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/q/position/r",cpo%eqconstraint%q%position%r,&
   size(cpo%eqconstraint%q%position%r),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%q%position%r',cpo%eqconstraint%q%position%r
endif

! Put eqconstraint/q/position/z
if (associated(cpo%eqconstraint%q%position%z)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/q/position/z",cpo%eqconstraint%q%position%z,&
   size(cpo%eqconstraint%q%position%z),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%q%position%z',cpo%eqconstraint%q%position%z
endif

! Put eqconstraint/q/weight
if (associated(cpo%eqconstraint%q%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/q/weight",cpo%eqconstraint%q%weight,&
   size(cpo%eqconstraint%q%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%q%weight',cpo%eqconstraint%q%weight
endif

! Put eqconstraint/q/sigma
if (associated(cpo%eqconstraint%q%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/q/sigma",cpo%eqconstraint%q%sigma,&
   size(cpo%eqconstraint%q%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%q%sigma',cpo%eqconstraint%q%sigma
endif

! Put eqconstraint/q/calculated
if (associated(cpo%eqconstraint%q%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/q/calculated",cpo%eqconstraint%q%calculated,&
   size(cpo%eqconstraint%q%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%q%calculated',cpo%eqconstraint%q%calculated
endif

! Put eqconstraint/q/chi2
if (associated(cpo%eqconstraint%q%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/q/chi2",cpo%eqconstraint%q%chi2,&
   size(cpo%eqconstraint%q%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%q%chi2',cpo%eqconstraint%q%chi2
endif

! Put eqconstraint/isoflux/position/r
if (associated(cpo%eqconstraint%isoflux%position%r)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/isoflux/position/r",cpo%eqconstraint%isoflux%position%r,&
   size(cpo%eqconstraint%isoflux%position%r),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%isoflux%position%r',cpo%eqconstraint%isoflux%position%r
endif

! Put eqconstraint/isoflux/position/z
if (associated(cpo%eqconstraint%isoflux%position%z)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/isoflux/position/z",cpo%eqconstraint%isoflux%position%z,&
   size(cpo%eqconstraint%isoflux%position%z),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%isoflux%position%z',cpo%eqconstraint%isoflux%position%z
endif

! Put eqconstraint/isoflux/weight
if (associated(cpo%eqconstraint%isoflux%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/isoflux/weight",cpo%eqconstraint%isoflux%weight,&
   size(cpo%eqconstraint%isoflux%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%isoflux%weight',cpo%eqconstraint%isoflux%weight
endif

! Put eqconstraint/isoflux/sigma
if (associated(cpo%eqconstraint%isoflux%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/isoflux/sigma",cpo%eqconstraint%isoflux%sigma,&
   size(cpo%eqconstraint%isoflux%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%isoflux%sigma',cpo%eqconstraint%isoflux%sigma
endif

! Put eqconstraint/isoflux/calculated
if (associated(cpo%eqconstraint%isoflux%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/isoflux/calculated",cpo%eqconstraint%isoflux%calculated,&
   size(cpo%eqconstraint%isoflux%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%isoflux%calculated',cpo%eqconstraint%isoflux%calculated
endif

! Put eqconstraint/isoflux/chi2
if (associated(cpo%eqconstraint%isoflux%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/isoflux/chi2",cpo%eqconstraint%isoflux%chi2,&
   size(cpo%eqconstraint%isoflux%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%isoflux%chi2',cpo%eqconstraint%isoflux%chi2
endif

! Put eqconstraint/xpts/position/r
if (associated(cpo%eqconstraint%xpts%position%r)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/xpts/position/r",cpo%eqconstraint%xpts%position%r,&
   size(cpo%eqconstraint%xpts%position%r),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%xpts%position%r',cpo%eqconstraint%xpts%position%r
endif

! Put eqconstraint/xpts/position/z
if (associated(cpo%eqconstraint%xpts%position%z)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/xpts/position/z",cpo%eqconstraint%xpts%position%z,&
   size(cpo%eqconstraint%xpts%position%z),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%xpts%position%z',cpo%eqconstraint%xpts%position%z
endif

! Put eqconstraint/xpts/weight
if (associated(cpo%eqconstraint%xpts%weight)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/xpts/weight",cpo%eqconstraint%xpts%weight,&
   size(cpo%eqconstraint%xpts%weight),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%xpts%weight',cpo%eqconstraint%xpts%weight
endif

! Put eqconstraint/xpts/sigma
if (associated(cpo%eqconstraint%xpts%sigma)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/xpts/sigma",cpo%eqconstraint%xpts%sigma,&
   size(cpo%eqconstraint%xpts%sigma),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%xpts%sigma',cpo%eqconstraint%xpts%sigma
endif

! Put eqconstraint/xpts/calculated
if (associated(cpo%eqconstraint%xpts%calculated)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/xpts/calculated",cpo%eqconstraint%xpts%calculated,&
   size(cpo%eqconstraint%xpts%calculated),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%xpts%calculated',cpo%eqconstraint%xpts%calculated
endif

! Put eqconstraint/xpts/chi2
if (associated(cpo%eqconstraint%xpts%chi2)) then   
   call put_vect1d_double_slice(idx,path, "eqconstraint/xpts/chi2",cpo%eqconstraint%xpts%chi2,&
   size(cpo%eqconstraint%xpts%chi2),cpo%time)
   write(*,*) 'Put cpo%eqconstraint%xpts%chi2',cpo%eqconstraint%xpts%chi2
endif

! Put eqgeometry/boundarytype  ERROR : NO TIME DEPENDENT STRING EXPECTED IN THE DATA STRUCTURE

! Put eqgeometry/boundary/r
if (associated(cpo%eqgeometry%boundary%r)) then   
   call put_vect1d_double_slice(idx,path, "eqgeometry/boundary/r",cpo%eqgeometry%boundary%r,&
   size(cpo%eqgeometry%boundary%r),cpo%time)
   write(*,*) 'Put cpo%eqgeometry%boundary%r',cpo%eqgeometry%boundary%r
endif

! Put eqgeometry/boundary/z
if (associated(cpo%eqgeometry%boundary%z)) then   
   call put_vect1d_double_slice(idx,path, "eqgeometry/boundary/z",cpo%eqgeometry%boundary%z,&
   size(cpo%eqgeometry%boundary%z),cpo%time)
   write(*,*) 'Put cpo%eqgeometry%boundary%z',cpo%eqgeometry%boundary%z
endif

! Put eqgeometry/geom_axis/r
if (cpo%eqgeometry%geom_axis%r.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/geom_axis/r",cpo%eqgeometry%geom_axis%r,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%geom_axis%r',cpo%eqgeometry%geom_axis%r
endif

! Put eqgeometry/geom_axis/z
if (cpo%eqgeometry%geom_axis%z.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/geom_axis/z",cpo%eqgeometry%geom_axis%z,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%geom_axis%z',cpo%eqgeometry%geom_axis%z
endif

! Put eqgeometry/a_minor
if (cpo%eqgeometry%a_minor.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/a_minor",cpo%eqgeometry%a_minor,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%a_minor',cpo%eqgeometry%a_minor
endif

! Put eqgeometry/elongation
if (cpo%eqgeometry%elongation.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/elongation",cpo%eqgeometry%elongation,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%elongation',cpo%eqgeometry%elongation
endif

! Put eqgeometry/tria_upper
if (cpo%eqgeometry%tria_upper.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/tria_upper",cpo%eqgeometry%tria_upper,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%tria_upper',cpo%eqgeometry%tria_upper
endif

! Put eqgeometry/tria_lower
if (cpo%eqgeometry%tria_lower.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/tria_lower",cpo%eqgeometry%tria_lower,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%tria_lower',cpo%eqgeometry%tria_lower
endif

! Put eqgeometry/xpts/r
if (associated(cpo%eqgeometry%xpts%r)) then   
   call put_vect1d_double_slice(idx,path, "eqgeometry/xpts/r",cpo%eqgeometry%xpts%r,&
   size(cpo%eqgeometry%xpts%r),cpo%time)
   write(*,*) 'Put cpo%eqgeometry%xpts%r',cpo%eqgeometry%xpts%r
endif

! Put eqgeometry/xpts/z
if (associated(cpo%eqgeometry%xpts%z)) then   
   call put_vect1d_double_slice(idx,path, "eqgeometry/xpts/z",cpo%eqgeometry%xpts%z,&
   size(cpo%eqgeometry%xpts%z),cpo%time)
   write(*,*) 'Put cpo%eqgeometry%xpts%z',cpo%eqgeometry%xpts%z
endif

! Put eqgeometry/left_low_st/r
if (cpo%eqgeometry%left_low_st%r.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/left_low_st/r",cpo%eqgeometry%left_low_st%r,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%left_low_st%r',cpo%eqgeometry%left_low_st%r
endif

! Put eqgeometry/left_low_st/z
if (cpo%eqgeometry%left_low_st%z.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/left_low_st/z",cpo%eqgeometry%left_low_st%z,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%left_low_st%z',cpo%eqgeometry%left_low_st%z
endif

! Put eqgeometry/right_low_st/r
if (cpo%eqgeometry%right_low_st%r.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/right_low_st/r",cpo%eqgeometry%right_low_st%r,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%right_low_st%r',cpo%eqgeometry%right_low_st%r
endif

! Put eqgeometry/right_low_st/z
if (cpo%eqgeometry%right_low_st%z.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/right_low_st/z",cpo%eqgeometry%right_low_st%z,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%right_low_st%z',cpo%eqgeometry%right_low_st%z
endif

! Put eqgeometry/left_up_st/r
if (cpo%eqgeometry%left_up_st%r.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/left_up_st/r",cpo%eqgeometry%left_up_st%r,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%left_up_st%r',cpo%eqgeometry%left_up_st%r
endif

! Put eqgeometry/left_up_st/z
if (cpo%eqgeometry%left_up_st%z.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/left_up_st/z",cpo%eqgeometry%left_up_st%z,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%left_up_st%z',cpo%eqgeometry%left_up_st%z
endif

! Put eqgeometry/right_up_st/r
if (cpo%eqgeometry%right_up_st%r.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/right_up_st/r",cpo%eqgeometry%right_up_st%r,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%right_up_st%r',cpo%eqgeometry%right_up_st%r
endif

! Put eqgeometry/right_up_st/z
if (cpo%eqgeometry%right_up_st%z.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/right_up_st/z",cpo%eqgeometry%right_up_st%z,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%right_up_st%z',cpo%eqgeometry%right_up_st%z
endif

! Put eqgeometry/active_limit/r
if (cpo%eqgeometry%active_limit%r.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/active_limit/r",cpo%eqgeometry%active_limit%r,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%active_limit%r',cpo%eqgeometry%active_limit%r
endif

! Put eqgeometry/active_limit/z
if (cpo%eqgeometry%active_limit%z.NE.-9.D40) then 
   call put_double_slice(idx,path, "eqgeometry/active_limit/z",cpo%eqgeometry%active_limit%z,cpo%time)  
   write(*,*) 'Put cpo%eqgeometry%active_limit%z',cpo%eqgeometry%active_limit%z
endif

! Put flush/position/r
if (associated(cpo%flush%position%r)) then   
   call put_vect1d_double_slice(idx,path, "flush/position/r",cpo%flush%position%r,&
   size(cpo%flush%position%r),cpo%time)
   write(*,*) 'Put cpo%flush%position%r',cpo%flush%position%r
endif

! Put flush/position/z
if (associated(cpo%flush%position%z)) then   
   call put_vect1d_double_slice(idx,path, "flush/position/z",cpo%flush%position%z,&
   size(cpo%flush%position%z),cpo%time)
   write(*,*) 'Put cpo%flush%position%z',cpo%flush%position%z
endif

! Put flush/coef        
if (associated(cpo%flush%coef)) then   
   call put_vect2d_double_slice(idx,path, "flush/coef", &
   cpo%flush%coef, &
   size(cpo%flush%coef,1),size(cpo%flush%coef,2),cpo%time)  
   write(*,*) 'Put cpo%flush%coef',cpo%flush%coef
endif

! Put global_param/beta_pol
if (cpo%global_param%beta_pol.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/beta_pol",cpo%global_param%beta_pol,cpo%time)  
   write(*,*) 'Put cpo%global_param%beta_pol',cpo%global_param%beta_pol
endif

! Put global_param/beta_tor
if (cpo%global_param%beta_tor.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/beta_tor",cpo%global_param%beta_tor,cpo%time)  
   write(*,*) 'Put cpo%global_param%beta_tor',cpo%global_param%beta_tor
endif

! Put global_param/beta_normal
if (cpo%global_param%beta_normal.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/beta_normal",cpo%global_param%beta_normal,cpo%time)  
   write(*,*) 'Put cpo%global_param%beta_normal',cpo%global_param%beta_normal
endif

! Put global_param/i_plasma
if (cpo%global_param%i_plasma.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/i_plasma",cpo%global_param%i_plasma,cpo%time)  
   write(*,*) 'Put cpo%global_param%i_plasma',cpo%global_param%i_plasma
endif

! Put global_param/li
if (cpo%global_param%li.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/li",cpo%global_param%li,cpo%time)  
   write(*,*) 'Put cpo%global_param%li',cpo%global_param%li
endif

! Put global_param/volume
if (cpo%global_param%volume.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/volume",cpo%global_param%volume,cpo%time)  
   write(*,*) 'Put cpo%global_param%volume',cpo%global_param%volume
endif

! Put global_param/area
if (cpo%global_param%area.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/area",cpo%global_param%area,cpo%time)  
   write(*,*) 'Put cpo%global_param%area',cpo%global_param%area
endif

! Put global_param/psi_ax
if (cpo%global_param%psi_ax.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/psi_ax",cpo%global_param%psi_ax,cpo%time)  
   write(*,*) 'Put cpo%global_param%psi_ax',cpo%global_param%psi_ax
endif

! Put global_param/psi_bound
if (cpo%global_param%psi_bound.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/psi_bound",cpo%global_param%psi_bound,cpo%time)  
   write(*,*) 'Put cpo%global_param%psi_bound',cpo%global_param%psi_bound
endif

! Put global_param/mag_axis/position/r
if (cpo%global_param%mag_axis%position%r.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/mag_axis/position/r",cpo%global_param%mag_axis%position%r,cpo%time)  
   write(*,*) 'Put cpo%global_param%mag_axis%position%r',cpo%global_param%mag_axis%position%r
endif

! Put global_param/mag_axis/position/z
if (cpo%global_param%mag_axis%position%z.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/mag_axis/position/z",cpo%global_param%mag_axis%position%z,cpo%time)  
   write(*,*) 'Put cpo%global_param%mag_axis%position%z',cpo%global_param%mag_axis%position%z
endif

! Put global_param/mag_axis/bphi
if (cpo%global_param%mag_axis%bphi.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/mag_axis/bphi",cpo%global_param%mag_axis%bphi,cpo%time)  
   write(*,*) 'Put cpo%global_param%mag_axis%bphi',cpo%global_param%mag_axis%bphi
endif

! Put global_param/mag_axis/q
if (cpo%global_param%mag_axis%q.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/mag_axis/q",cpo%global_param%mag_axis%q,cpo%time)  
   write(*,*) 'Put cpo%global_param%mag_axis%q',cpo%global_param%mag_axis%q
endif

! Put global_param/q_95
if (cpo%global_param%q_95.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/q_95",cpo%global_param%q_95,cpo%time)  
   write(*,*) 'Put cpo%global_param%q_95',cpo%global_param%q_95
endif

! Put global_param/q_min
if (cpo%global_param%q_min.NE.-9.D40) then 
   call put_double_slice(idx,path, "global_param/q_min",cpo%global_param%q_min,cpo%time)  
   write(*,*) 'Put cpo%global_param%q_min',cpo%global_param%q_min
endif

! Put profiles_1d/psi
if (associated(cpo%profiles_1d%psi)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/psi",cpo%profiles_1d%psi,&
   size(cpo%profiles_1d%psi),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%psi',cpo%profiles_1d%psi
endif

! Put profiles_1d/phi
if (associated(cpo%profiles_1d%phi)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/phi",cpo%profiles_1d%phi,&
   size(cpo%profiles_1d%phi),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%phi',cpo%profiles_1d%phi
endif

! Put profiles_1d/pressure
if (associated(cpo%profiles_1d%pressure)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/pressure",cpo%profiles_1d%pressure,&
   size(cpo%profiles_1d%pressure),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%pressure',cpo%profiles_1d%pressure
endif

! Put profiles_1d/F_dia
if (associated(cpo%profiles_1d%F_dia)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/F_dia",cpo%profiles_1d%F_dia,&
   size(cpo%profiles_1d%F_dia),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%F_dia',cpo%profiles_1d%F_dia
endif

! Put profiles_1d/pprime
if (associated(cpo%profiles_1d%pprime)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/pprime",cpo%profiles_1d%pprime,&
   size(cpo%profiles_1d%pprime),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%pprime',cpo%profiles_1d%pprime
endif

! Put profiles_1d/ffprime
if (associated(cpo%profiles_1d%ffprime)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/ffprime",cpo%profiles_1d%ffprime,&
   size(cpo%profiles_1d%ffprime),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%ffprime',cpo%profiles_1d%ffprime
endif

! Put profiles_1d/jphi
if (associated(cpo%profiles_1d%jphi)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/jphi",cpo%profiles_1d%jphi,&
   size(cpo%profiles_1d%jphi),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%jphi',cpo%profiles_1d%jphi
endif

! Put profiles_1d/jparallel
if (associated(cpo%profiles_1d%jparallel)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/jparallel",cpo%profiles_1d%jparallel,&
   size(cpo%profiles_1d%jparallel),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%jparallel',cpo%profiles_1d%jparallel
endif

! Put profiles_1d/q
if (associated(cpo%profiles_1d%q)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/q",cpo%profiles_1d%q,&
   size(cpo%profiles_1d%q),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%q',cpo%profiles_1d%q
endif

! Put profiles_1d/r_inboard
if (associated(cpo%profiles_1d%r_inboard)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/r_inboard",cpo%profiles_1d%r_inboard,&
   size(cpo%profiles_1d%r_inboard),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%r_inboard',cpo%profiles_1d%r_inboard
endif

! Put profiles_1d/r_outboard
if (associated(cpo%profiles_1d%r_outboard)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/r_outboard",cpo%profiles_1d%r_outboard,&
   size(cpo%profiles_1d%r_outboard),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%r_outboard',cpo%profiles_1d%r_outboard
endif

! Put profiles_1d/rho_rtvol
if (associated(cpo%profiles_1d%rho_rtvol)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/rho_rtvol",cpo%profiles_1d%rho_rtvol,&
   size(cpo%profiles_1d%rho_rtvol),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%rho_rtvol',cpo%profiles_1d%rho_rtvol
endif

! Put profiles_1d/rho_rttorfl
if (associated(cpo%profiles_1d%rho_rttorfl)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/rho_rttorfl",cpo%profiles_1d%rho_rttorfl,&
   size(cpo%profiles_1d%rho_rttorfl),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%rho_rttorfl',cpo%profiles_1d%rho_rttorfl
endif

! Put profiles_1d/elongation
if (associated(cpo%profiles_1d%elongation)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/elongation",cpo%profiles_1d%elongation,&
   size(cpo%profiles_1d%elongation),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%elongation',cpo%profiles_1d%elongation
endif

! Put profiles_1d/tria_upper
if (associated(cpo%profiles_1d%tria_upper)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/tria_upper",cpo%profiles_1d%tria_upper,&
   size(cpo%profiles_1d%tria_upper),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%tria_upper',cpo%profiles_1d%tria_upper
endif

! Put profiles_1d/tria_lower
if (associated(cpo%profiles_1d%tria_lower)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/tria_lower",cpo%profiles_1d%tria_lower,&
   size(cpo%profiles_1d%tria_lower),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%tria_lower',cpo%profiles_1d%tria_lower
endif

! Put profiles_1d/volume
if (associated(cpo%profiles_1d%volume)) then   
   call put_vect1d_double_slice(idx,path, "profiles_1d/volume",cpo%profiles_1d%volume,&
   size(cpo%profiles_1d%volume),cpo%time)
   write(*,*) 'Put cpo%profiles_1d%volume',cpo%profiles_1d%volume
endif

! Put profiles_2d/grid/dim1
if (associated(cpo%profiles_2d%grid%dim1)) then   
   call put_vect1d_double_slice(idx,path, "profiles_2d/grid/dim1",cpo%profiles_2d%grid%dim1,&
   size(cpo%profiles_2d%grid%dim1),cpo%time)
   write(*,*) 'Put cpo%profiles_2d%grid%dim1',cpo%profiles_2d%grid%dim1
endif

! Put profiles_2d/grid/dim2
if (associated(cpo%profiles_2d%grid%dim2)) then   
   call put_vect1d_double_slice(idx,path, "profiles_2d/grid/dim2",cpo%profiles_2d%grid%dim2,&
   size(cpo%profiles_2d%grid%dim2),cpo%time)
   write(*,*) 'Put cpo%profiles_2d%grid%dim2',cpo%profiles_2d%grid%dim2
endif

! Put profiles_2d/grid/connect        
if (associated(cpo%profiles_2d%grid%connect)) then   
   call put_vect2d_int_slice(idx,path, "profiles_2d/grid/connect",&
   cpo%profiles_2d%grid%connect, &
   size(cpo%profiles_2d%grid%connect,1), &
   size(cpo%profiles_2d%grid%connect,2),cpo%time)  
   write(*,*) 'Put cpo%profiles_2d%grid%connect',cpo%profiles_2d%grid%connect
endif

! Put profiles_2d/psi_grid        
if (associated(cpo%profiles_2d%psi_grid)) then   
   call put_vect2d_double_slice(idx,path, "profiles_2d/psi_grid", &
   cpo%profiles_2d%psi_grid, &
   size(cpo%profiles_2d%psi_grid,1),size(cpo%profiles_2d%psi_grid,2),cpo%time)  
   write(*,*) 'Put cpo%profiles_2d%psi_grid',cpo%profiles_2d%psi_grid
endif

! Put profiles_2d/jphi_grid        
if (associated(cpo%profiles_2d%jphi_grid)) then   
   call put_vect2d_double_slice(idx,path, "profiles_2d/jphi_grid", &
   cpo%profiles_2d%jphi_grid, &
   size(cpo%profiles_2d%jphi_grid,1),size(cpo%profiles_2d%jphi_grid,2),cpo%time)  
   write(*,*) 'Put cpo%profiles_2d%jphi_grid',cpo%profiles_2d%jphi_grid
endif

! Put profiles_2d/jpar_grid        
if (associated(cpo%profiles_2d%jpar_grid)) then   
   call put_vect2d_double_slice(idx,path, "profiles_2d/jpar_grid", &
   cpo%profiles_2d%jpar_grid, &
   size(cpo%profiles_2d%jpar_grid,1),size(cpo%profiles_2d%jpar_grid,2),cpo%time)  
   write(*,*) 'Put cpo%profiles_2d%jpar_grid',cpo%profiles_2d%jpar_grid
endif

! Put profiles_2d/br        
if (associated(cpo%profiles_2d%br)) then   
   call put_vect2d_double_slice(idx,path, "profiles_2d/br", &
   cpo%profiles_2d%br, &
   size(cpo%profiles_2d%br,1),size(cpo%profiles_2d%br,2),cpo%time)  
   write(*,*) 'Put cpo%profiles_2d%br',cpo%profiles_2d%br
endif

! Put profiles_2d/bz        
if (associated(cpo%profiles_2d%bz)) then   
   call put_vect2d_double_slice(idx,path, "profiles_2d/bz", &
   cpo%profiles_2d%bz, &
   size(cpo%profiles_2d%bz,1),size(cpo%profiles_2d%bz,2),cpo%time)  
   write(*,*) 'Put cpo%profiles_2d%bz',cpo%profiles_2d%bz
endif

! Put profiles_2d/bphi        
if (associated(cpo%profiles_2d%bphi)) then   
   call put_vect2d_double_slice(idx,path, "profiles_2d/bphi", &
   cpo%profiles_2d%bphi, &
   size(cpo%profiles_2d%bphi,1),size(cpo%profiles_2d%bphi,2),cpo%time)  
   write(*,*) 'Put cpo%profiles_2d%bphi',cpo%profiles_2d%bphi
endif

! Put coord_sys/grid/dim1
if (associated(cpo%coord_sys%grid%dim1)) then   
   call put_vect1d_double_slice(idx,path, "coord_sys/grid/dim1",cpo%coord_sys%grid%dim1,&
   size(cpo%coord_sys%grid%dim1),cpo%time)
   write(*,*) 'Put cpo%coord_sys%grid%dim1',cpo%coord_sys%grid%dim1
endif

! Put coord_sys/grid/dim2
if (associated(cpo%coord_sys%grid%dim2)) then   
   call put_vect1d_double_slice(idx,path, "coord_sys/grid/dim2",cpo%coord_sys%grid%dim2,&
   size(cpo%coord_sys%grid%dim2),cpo%time)
   write(*,*) 'Put cpo%coord_sys%grid%dim2',cpo%coord_sys%grid%dim2
endif

! Put coord_sys/jacobian        
if (associated(cpo%coord_sys%jacobian)) then   
   call put_vect2d_double_slice(idx,path, "coord_sys/jacobian", &
   cpo%coord_sys%jacobian, &
   size(cpo%coord_sys%jacobian,1),size(cpo%coord_sys%jacobian,2),cpo%time)  
   write(*,*) 'Put cpo%coord_sys%jacobian',cpo%coord_sys%jacobian
endif

! Put coord_sys/g_11        
if (associated(cpo%coord_sys%g_11)) then   
   call put_vect2d_double_slice(idx,path, "coord_sys/g_11", &
   cpo%coord_sys%g_11, &
   size(cpo%coord_sys%g_11,1),size(cpo%coord_sys%g_11,2),cpo%time)  
   write(*,*) 'Put cpo%coord_sys%g_11',cpo%coord_sys%g_11
endif

! Put coord_sys/g_12        
if (associated(cpo%coord_sys%g_12)) then   
   call put_vect2d_double_slice(idx,path, "coord_sys/g_12", &
   cpo%coord_sys%g_12, &
   size(cpo%coord_sys%g_12,1),size(cpo%coord_sys%g_12,2),cpo%time)  
   write(*,*) 'Put cpo%coord_sys%g_12',cpo%coord_sys%g_12
endif

! Put coord_sys/g_22        
if (associated(cpo%coord_sys%g_22)) then   
   call put_vect2d_double_slice(idx,path, "coord_sys/g_22", &
   cpo%coord_sys%g_22, &
   size(cpo%coord_sys%g_22,1),size(cpo%coord_sys%g_22,2),cpo%time)  
   write(*,*) 'Put cpo%coord_sys%g_22',cpo%coord_sys%g_22
endif

! Put coord_sys/g_33        
if (associated(cpo%coord_sys%g_33)) then   
   call put_vect2d_double_slice(idx,path, "coord_sys/g_33", &
   cpo%coord_sys%g_33, &
   size(cpo%coord_sys%g_33,1),size(cpo%coord_sys%g_33,2),cpo%time)  
   write(*,*) 'Put cpo%coord_sys%g_33',cpo%coord_sys%g_33
endif

! Put coord_sys/position/r        
if (associated(cpo%coord_sys%position%r)) then   
   call put_vect2d_double_slice(idx,path, "coord_sys/position/r", &
   cpo%coord_sys%position%r, &
   size(cpo%coord_sys%position%r,1),size(cpo%coord_sys%position%r,2),cpo%time)  
   write(*,*) 'Put cpo%coord_sys%position%r',cpo%coord_sys%position%r
endif

! Put coord_sys/position/z        
if (associated(cpo%coord_sys%position%z)) then   
   call put_vect2d_double_slice(idx,path, "coord_sys/position/z", &
   cpo%coord_sys%position%z, &
   size(cpo%coord_sys%position%z,1),size(cpo%coord_sys%position%z,2),cpo%time)  
   write(*,*) 'Put cpo%coord_sys%position%z',cpo%coord_sys%position%z
endif

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine

subroutine euITM_put_slice_ironmodel(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_ironmodel) :: cpo
    
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put magnetise/mr/value
if (associated(cpo%magnetise%mr%value)) then   
   call put_vect1d_double_slice(idx,path, "magnetise/mr/value",cpo%magnetise%mr%value,&
   size(cpo%magnetise%mr%value),cpo%time)
   write(*,*) 'Put cpo%magnetise%mr%value',cpo%magnetise%mr%value
endif

! Put magnetise/mr/abserror
if (associated(cpo%magnetise%mr%abserror)) then   
   call put_vect1d_double_slice(idx,path, "magnetise/mr/abserror",cpo%magnetise%mr%abserror,&
   size(cpo%magnetise%mr%abserror),cpo%time)
   write(*,*) 'Put cpo%magnetise%mr%abserror',cpo%magnetise%mr%abserror
endif

! Put magnetise/mr/relerror
if (associated(cpo%magnetise%mr%relerror)) then   
   call put_vect1d_double_slice(idx,path, "magnetise/mr/relerror",cpo%magnetise%mr%relerror,&
   size(cpo%magnetise%mr%relerror),cpo%time)
   write(*,*) 'Put cpo%magnetise%mr%relerror',cpo%magnetise%mr%relerror
endif

! Put magnetise/mz/value
if (associated(cpo%magnetise%mz%value)) then   
   call put_vect1d_double_slice(idx,path, "magnetise/mz/value",cpo%magnetise%mz%value,&
   size(cpo%magnetise%mz%value),cpo%time)
   write(*,*) 'Put cpo%magnetise%mz%value',cpo%magnetise%mz%value
endif

! Put magnetise/mz/abserror
if (associated(cpo%magnetise%mz%abserror)) then   
   call put_vect1d_double_slice(idx,path, "magnetise/mz/abserror",cpo%magnetise%mz%abserror,&
   size(cpo%magnetise%mz%abserror),cpo%time)
   write(*,*) 'Put cpo%magnetise%mz%abserror',cpo%magnetise%mz%abserror
endif

! Put magnetise/mz/relerror
if (associated(cpo%magnetise%mz%relerror)) then   
   call put_vect1d_double_slice(idx,path, "magnetise/mz/relerror",cpo%magnetise%mz%relerror,&
   size(cpo%magnetise%mz%relerror),cpo%time)
   write(*,*) 'Put cpo%magnetise%mz%relerror',cpo%magnetise%mz%relerror
endif

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine

subroutine euITM_put_slice_interfdiag(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_lineintegraldiag) :: cpo     
	
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put measure/value
if (associated(cpo%measure%value)) then   
   call put_vect1d_double_slice(idx,path, "measure/value",cpo%measure%value,&
   size(cpo%measure%value),cpo%time)
   write(*,*) 'Put cpo%measure%value',cpo%measure%value
endif

! Put measure/abserror
if (associated(cpo%measure%abserror)) then   
   call put_vect1d_double_slice(idx,path, "measure/abserror",cpo%measure%abserror,&
   size(cpo%measure%abserror),cpo%time)
   write(*,*) 'Put cpo%measure%abserror',cpo%measure%abserror
endif

! Put measure/relerror
if (associated(cpo%measure%relerror)) then   
   call put_vect1d_double_slice(idx,path, "measure/relerror",cpo%measure%relerror,&
   size(cpo%measure%relerror),cpo%time)
   write(*,*) 'Put cpo%measure%relerror',cpo%measure%relerror
endif

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine

subroutine euITM_put_slice_mhd(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_mhd) :: cpo
    
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put n
if (associated(cpo%n)) then   
   call put_vect1d_double_slice(idx,path, "n",cpo%n,&
   size(cpo%n),cpo%time)
   write(*,*) 'Put cpo%n',cpo%n
endif

! Put m        
if (associated(cpo%m)) then   
   call put_vect2d_double_slice(idx,path, "m", &
   cpo%m, &
   size(cpo%m,1),size(cpo%m,2),cpo%time)  
   write(*,*) 'Put cpo%m',cpo%m
endif

! Put psi
if (associated(cpo%psi)) then   
   call put_vect1d_double_slice(idx,path, "psi",cpo%psi,&
   size(cpo%psi),cpo%time)
   write(*,*) 'Put cpo%psi',cpo%psi
endif

! Put frequency
if (associated(cpo%frequency)) then   
   call put_vect1d_double_slice(idx,path, "frequency",cpo%frequency,&
   size(cpo%frequency),cpo%time)
   write(*,*) 'Put cpo%frequency',cpo%frequency
endif

! Put growthrate
if (associated(cpo%growthrate)) then   
   call put_vect1d_double_slice(idx,path, "growthrate",cpo%growthrate,&
   size(cpo%growthrate),cpo%time)
   write(*,*) 'Put cpo%growthrate',cpo%growthrate
endif

! Put disp_perp        
if (associated(cpo%disp_perp)) then   
   call put_vect3d_double_slice(idx,path, "disp_perp", &
   cpo%disp_perp, &
   size(cpo%disp_perp,1),&
   size(cpo%disp_perp,2), &
   size(cpo%disp_perp,3),cpo%time)    
   write(*,*) 'Put cpo%disp_perp'
endif

! Put disp_par        
if (associated(cpo%disp_par)) then   
   call put_vect3d_double_slice(idx,path, "disp_par", &
   cpo%disp_par, &
   size(cpo%disp_par,1),&
   size(cpo%disp_par,2), &
   size(cpo%disp_par,3),cpo%time)    
   write(*,*) 'Put cpo%disp_par'
endif

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine

subroutine euITM_put_slice_magdiag(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_magdiag) :: cpo
    
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put ip/value
if (cpo%ip%value.NE.-9.D40) then 
   call put_double_slice(idx,path, "ip/value",cpo%ip%value,cpo%time)  
   write(*,*) 'Put cpo%ip%value',cpo%ip%value
endif

! Put ip/abserror
if (cpo%ip%abserror.NE.-9.D40) then 
   call put_double_slice(idx,path, "ip/abserror",cpo%ip%abserror,cpo%time)  
   write(*,*) 'Put cpo%ip%abserror',cpo%ip%abserror
endif

! Put ip/relerror
if (cpo%ip%relerror.NE.-9.D40) then 
   call put_double_slice(idx,path, "ip/relerror",cpo%ip%relerror,cpo%time)  
   write(*,*) 'Put cpo%ip%relerror',cpo%ip%relerror
endif

! Put diamagflux/value
if (cpo%diamagflux%value.NE.-9.D40) then 
   call put_double_slice(idx,path, "diamagflux/value",cpo%diamagflux%value,cpo%time)  
   write(*,*) 'Put cpo%diamagflux%value',cpo%diamagflux%value
endif

! Put diamagflux/abserror
if (cpo%diamagflux%abserror.NE.-9.D40) then 
   call put_double_slice(idx,path, "diamagflux/abserror",cpo%diamagflux%abserror,cpo%time)  
   write(*,*) 'Put cpo%diamagflux%abserror',cpo%diamagflux%abserror
endif

! Put diamagflux/relerror
if (cpo%diamagflux%relerror.NE.-9.D40) then 
   call put_double_slice(idx,path, "diamagflux/relerror",cpo%diamagflux%relerror,cpo%time)  
   write(*,*) 'Put cpo%diamagflux%relerror',cpo%diamagflux%relerror
endif

! Put flux_loops/measure/value
if (associated(cpo%flux_loops%measure%value)) then   
   call put_vect1d_double_slice(idx,path, "flux_loops/measure/value",cpo%flux_loops%measure%value,&
   size(cpo%flux_loops%measure%value),cpo%time)
   write(*,*) 'Put cpo%flux_loops%measure%value',cpo%flux_loops%measure%value
endif

! Put flux_loops/measure/abserror
if (associated(cpo%flux_loops%measure%abserror)) then   
   call put_vect1d_double_slice(idx,path, "flux_loops/measure/abserror",cpo%flux_loops%measure%abserror,&
   size(cpo%flux_loops%measure%abserror),cpo%time)
   write(*,*) 'Put cpo%flux_loops%measure%abserror',cpo%flux_loops%measure%abserror
endif

! Put flux_loops/measure/relerror
if (associated(cpo%flux_loops%measure%relerror)) then   
   call put_vect1d_double_slice(idx,path, "flux_loops/measure/relerror",cpo%flux_loops%measure%relerror,&
   size(cpo%flux_loops%measure%relerror),cpo%time)
   write(*,*) 'Put cpo%flux_loops%measure%relerror',cpo%flux_loops%measure%relerror
endif

! Put bpol_probes/measure/value
if (associated(cpo%bpol_probes%measure%value)) then   
   call put_vect1d_double_slice(idx,path, "bpol_probes/measure/value",cpo%bpol_probes%measure%value,&
   size(cpo%bpol_probes%measure%value),cpo%time)
   write(*,*) 'Put cpo%bpol_probes%measure%value',cpo%bpol_probes%measure%value
endif

! Put bpol_probes/measure/abserror
if (associated(cpo%bpol_probes%measure%abserror)) then   
   call put_vect1d_double_slice(idx,path, "bpol_probes/measure/abserror",cpo%bpol_probes%measure%abserror,&
   size(cpo%bpol_probes%measure%abserror),cpo%time)
   write(*,*) 'Put cpo%bpol_probes%measure%abserror',cpo%bpol_probes%measure%abserror
endif

! Put bpol_probes/measure/relerror
if (associated(cpo%bpol_probes%measure%relerror)) then   
   call put_vect1d_double_slice(idx,path, "bpol_probes/measure/relerror",cpo%bpol_probes%measure%relerror,&
   size(cpo%bpol_probes%measure%relerror),cpo%time)
   write(*,*) 'Put cpo%bpol_probes%measure%relerror',cpo%bpol_probes%measure%relerror
endif

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine

subroutine euITM_put_slice_msediag(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_msediag) :: cpo
    
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put measure/value
if (associated(cpo%measure%value)) then   
   call put_vect1d_double_slice(idx,path, "measure/value",cpo%measure%value,&
   size(cpo%measure%value),cpo%time)
   write(*,*) 'Put cpo%measure%value',cpo%measure%value
endif

! Put measure/abserror
if (associated(cpo%measure%abserror)) then   
   call put_vect1d_double_slice(idx,path, "measure/abserror",cpo%measure%abserror,&
   size(cpo%measure%abserror),cpo%time)
   write(*,*) 'Put cpo%measure%abserror',cpo%measure%abserror
endif

! Put measure/relerror
if (associated(cpo%measure%relerror)) then   
   call put_vect1d_double_slice(idx,path, "measure/relerror",cpo%measure%relerror,&
   size(cpo%measure%relerror),cpo%time)
   write(*,*) 'Put cpo%measure%relerror',cpo%measure%relerror
endif

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine

subroutine euITM_put_slice_pfsystems(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_pfsystems) :: cpo
    
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put pfcoils/coilcurrent/value
if (associated(cpo%pfcoils%coilcurrent%value)) then   
   call put_vect1d_double_slice(idx,path, "pfcoils/coilcurrent/value",cpo%pfcoils%coilcurrent%value,&
   size(cpo%pfcoils%coilcurrent%value),cpo%time)
   write(*,*) 'Put cpo%pfcoils%coilcurrent%value',cpo%pfcoils%coilcurrent%value
endif

! Put pfcoils/coilcurrent/abserror
if (associated(cpo%pfcoils%coilcurrent%abserror)) then   
   call put_vect1d_double_slice(idx,path, "pfcoils/coilcurrent/abserror",cpo%pfcoils%coilcurrent%abserror,&
   size(cpo%pfcoils%coilcurrent%abserror),cpo%time)
   write(*,*) 'Put cpo%pfcoils%coilcurrent%abserror',cpo%pfcoils%coilcurrent%abserror
endif

! Put pfcoils/coilcurrent/relerror
if (associated(cpo%pfcoils%coilcurrent%relerror)) then   
   call put_vect1d_double_slice(idx,path, "pfcoils/coilcurrent/relerror",cpo%pfcoils%coilcurrent%relerror,&
   size(cpo%pfcoils%coilcurrent%relerror),cpo%time)
   write(*,*) 'Put cpo%pfcoils%coilcurrent%relerror',cpo%pfcoils%coilcurrent%relerror
endif

! Put pfcoils/coilvoltage/value
if (associated(cpo%pfcoils%coilvoltage%value)) then   
   call put_vect1d_double_slice(idx,path, "pfcoils/coilvoltage/value",cpo%pfcoils%coilvoltage%value,&
   size(cpo%pfcoils%coilvoltage%value),cpo%time)
   write(*,*) 'Put cpo%pfcoils%coilvoltage%value',cpo%pfcoils%coilvoltage%value
endif

! Put pfcoils/coilvoltage/abserror
if (associated(cpo%pfcoils%coilvoltage%abserror)) then   
   call put_vect1d_double_slice(idx,path, "pfcoils/coilvoltage/abserror",cpo%pfcoils%coilvoltage%abserror,&
   size(cpo%pfcoils%coilvoltage%abserror),cpo%time)
   write(*,*) 'Put cpo%pfcoils%coilvoltage%abserror',cpo%pfcoils%coilvoltage%abserror
endif

! Put pfcoils/coilvoltage/relerror
if (associated(cpo%pfcoils%coilvoltage%relerror)) then   
   call put_vect1d_double_slice(idx,path, "pfcoils/coilvoltage/relerror",cpo%pfcoils%coilvoltage%relerror,&
   size(cpo%pfcoils%coilvoltage%relerror),cpo%time)
   write(*,*) 'Put cpo%pfcoils%coilvoltage%relerror',cpo%pfcoils%coilvoltage%relerror
endif

! Put pfsupplies/voltage/value
if (associated(cpo%pfsupplies%voltage%value)) then   
   call put_vect1d_double_slice(idx,path, "pfsupplies/voltage/value",cpo%pfsupplies%voltage%value,&
   size(cpo%pfsupplies%voltage%value),cpo%time)
   write(*,*) 'Put cpo%pfsupplies%voltage%value',cpo%pfsupplies%voltage%value
endif

! Put pfsupplies/voltage/abserror
if (associated(cpo%pfsupplies%voltage%abserror)) then   
   call put_vect1d_double_slice(idx,path, "pfsupplies/voltage/abserror",cpo%pfsupplies%voltage%abserror,&
   size(cpo%pfsupplies%voltage%abserror),cpo%time)
   write(*,*) 'Put cpo%pfsupplies%voltage%abserror',cpo%pfsupplies%voltage%abserror
endif

! Put pfsupplies/voltage/relerror
if (associated(cpo%pfsupplies%voltage%relerror)) then   
   call put_vect1d_double_slice(idx,path, "pfsupplies/voltage/relerror",cpo%pfsupplies%voltage%relerror,&
   size(cpo%pfsupplies%voltage%relerror),cpo%time)
   write(*,*) 'Put cpo%pfsupplies%voltage%relerror',cpo%pfsupplies%voltage%relerror
endif

! Put pfsupplies/current/value
if (associated(cpo%pfsupplies%current%value)) then   
   call put_vect1d_double_slice(idx,path, "pfsupplies/current/value",cpo%pfsupplies%current%value,&
   size(cpo%pfsupplies%current%value),cpo%time)
   write(*,*) 'Put cpo%pfsupplies%current%value',cpo%pfsupplies%current%value
endif

! Put pfsupplies/current/abserror
if (associated(cpo%pfsupplies%current%abserror)) then   
   call put_vect1d_double_slice(idx,path, "pfsupplies/current/abserror",cpo%pfsupplies%current%abserror,&
   size(cpo%pfsupplies%current%abserror),cpo%time)
   write(*,*) 'Put cpo%pfsupplies%current%abserror',cpo%pfsupplies%current%abserror
endif

! Put pfsupplies/current/relerror
if (associated(cpo%pfsupplies%current%relerror)) then   
   call put_vect1d_double_slice(idx,path, "pfsupplies/current/relerror",cpo%pfsupplies%current%relerror,&
   size(cpo%pfsupplies%current%relerror),cpo%time)
   write(*,*) 'Put cpo%pfsupplies%current%relerror',cpo%pfsupplies%current%relerror
endif

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine

subroutine euITM_put_slice_polardiag(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_lineintegraldiag) :: cpo     
	
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put measure/value
if (associated(cpo%measure%value)) then   
   call put_vect1d_double_slice(idx,path, "measure/value",cpo%measure%value,&
   size(cpo%measure%value),cpo%time)
   write(*,*) 'Put cpo%measure%value',cpo%measure%value
endif

! Put measure/abserror
if (associated(cpo%measure%abserror)) then   
   call put_vect1d_double_slice(idx,path, "measure/abserror",cpo%measure%abserror,&
   size(cpo%measure%abserror),cpo%time)
   write(*,*) 'Put cpo%measure%abserror',cpo%measure%abserror
endif

! Put measure/relerror
if (associated(cpo%measure%relerror)) then   
   call put_vect1d_double_slice(idx,path, "measure/relerror",cpo%measure%relerror,&
   size(cpo%measure%relerror),cpo%time)
   write(*,*) 'Put cpo%measure%relerror',cpo%measure%relerror
endif

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine

subroutine euITM_put_slice_toroidfield(idx,path,cpo)

use euITM_schemas
implicit none
integer, parameter :: DP=kind(1.0D0)

character*(*) :: path
integer :: idx, lentime


type(type_toroidfield) :: cpo
    
! internal variables declaration
integer :: itime
integer :: int0D
integer,pointer :: vect1DInt(:), vect2DInt(:,:), vect3DInt(:,:,:)
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
real(DP) :: double0D
real(DP), pointer :: vect1DDouble(:), time(:), vect2DDouble(:,:), vect3DDouble(:,:,:), vect4DDouble(:,:,:,:)
character(len=132), dimension(:), pointer :: stri
character(len=100000)::longstring    


call begin_cpo_put_slice(idx, path,cpo%time)

! Put current/value
if (cpo%current%value.NE.-9.D40) then 
   call put_double_slice(idx,path, "current/value",cpo%current%value,cpo%time)  
   write(*,*) 'Put cpo%current%value',cpo%current%value
endif

! Put current/abserror
if (cpo%current%abserror.NE.-9.D40) then 
   call put_double_slice(idx,path, "current/abserror",cpo%current%abserror,cpo%time)  
   write(*,*) 'Put cpo%current%abserror',cpo%current%abserror
endif

! Put current/relerror
if (cpo%current%relerror.NE.-9.D40) then 
   call put_double_slice(idx,path, "current/relerror",cpo%current%relerror,cpo%time)  
   write(*,*) 'Put cpo%current%relerror',cpo%current%relerror
endif

! Put bvac_r/value
if (cpo%bvac_r%value.NE.-9.D40) then 
   call put_double_slice(idx,path, "bvac_r/value",cpo%bvac_r%value,cpo%time)  
   write(*,*) 'Put cpo%bvac_r%value',cpo%bvac_r%value
endif

! Put bvac_r/abserror
if (cpo%bvac_r%abserror.NE.-9.D40) then 
   call put_double_slice(idx,path, "bvac_r/abserror",cpo%bvac_r%abserror,cpo%time)  
   write(*,*) 'Put cpo%bvac_r%abserror',cpo%bvac_r%abserror
endif

! Put bvac_r/relerror
if (cpo%bvac_r%relerror.NE.-9.D40) then 
   call put_double_slice(idx,path, "bvac_r/relerror",cpo%bvac_r%relerror,cpo%time)  
   write(*,*) 'Put cpo%bvac_r%relerror',cpo%bvac_r%relerror
endif

! Put time
if (cpo%time.NE.-9.D40) then 
   call put_double_slice(idx,path, "time",cpo%time,cpo%time)  
   write(*,*) 'Put cpo%time',cpo%time
endif

call end_cpo_put_slice(idx, path)

 
return
endsubroutine


!!!!!! Routines to PUT_NON_TIMED the time INdependent data of time dependent CPOs 

subroutine euITM_put_non_timed_controllers(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_controllers) :: cpo       
    
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put name
if (associated(cpo%name)) then
   longstring = ' '    
   lenstring = size(cpo%name)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%name(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%name(istring)
      enddo
   endif
   call put_string(idx,path, "name",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%name',cpo%name
endif

! Put purpose
if (associated(cpo%purpose)) then
   longstring = ' '    
   lenstring = size(cpo%purpose)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%purpose(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%purpose(istring)
      enddo
   endif
   call put_string(idx,path, "purpose",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%purpose',cpo%purpose
endif

! Put type
if (associated(cpo%type)) then
   longstring = ' '    
   lenstring = size(cpo%type)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%type(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%type(istring)
      enddo
   endif
   call put_string(idx,path, "type",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%type',cpo%type
endif

! Put input
if (associated(cpo%input)) then
   dim1 = size(cpo%input)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%input(i))
   enddo
   call put_Vect1d_String(idx,path, "input", &
         cpo%input,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%input'
endif

! Put output
if (associated(cpo%output)) then
   dim1 = size(cpo%output)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%output(i))
   enddo
   call put_Vect1d_String(idx,path, "output", &
         cpo%output,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%output'
endif

! Put statespace/observable
if (associated(cpo%statespace%observable)) then
   dim1 = size(cpo%statespace%observable)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%statespace%observable(i))
   enddo
   call put_Vect1d_String(idx,path, "statespace/observable", &
         cpo%statespace%observable,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%statespace%observable'
endif

! Put statespace/A        
if (associated(cpo%statespace%A)) then   
   call put_vect2d_double(idx,path, "statespace/A",&
   cpo%statespace%A, &
   size(cpo%statespace%A,1),size(cpo%statespace%A,2),0)  
   write(*,*) 'Put cpo%statespace%A',cpo%statespace%A
endif

! Put statespace/B        
if (associated(cpo%statespace%B)) then   
   call put_vect2d_double(idx,path, "statespace/B",&
   cpo%statespace%B, &
   size(cpo%statespace%B,1),size(cpo%statespace%B,2),0)  
   write(*,*) 'Put cpo%statespace%B',cpo%statespace%B
endif

! Put statespace/C        
if (associated(cpo%statespace%C)) then   
   call put_vect2d_double(idx,path, "statespace/C",&
   cpo%statespace%C, &
   size(cpo%statespace%C,1),size(cpo%statespace%C,2),0)  
   write(*,*) 'Put cpo%statespace%C',cpo%statespace%C
endif

! Put statespace/D        
if (associated(cpo%statespace%D)) then   
   call put_vect2d_double(idx,path, "statespace/D",&
   cpo%statespace%D, &
   size(cpo%statespace%D,1),size(cpo%statespace%D,2),0)  
   write(*,*) 'Put cpo%statespace%D',cpo%statespace%D
endif

! Put statespace/deltat
if (cpo%statespace%deltat.NE.-9.D40) then 
   call put_double(idx,path, "statespace/deltat",cpo%statespace%deltat)  
   write(*,*) 'Put cpo%statespace%deltat',cpo%statespace%deltat
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine

subroutine euITM_put_non_timed_coreprof(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_coreprof) :: cpo       
    
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put codeparam/codename
if (associated(cpo%codeparam%codename)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%codename)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%codename(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%codename(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codename",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%codename',cpo%codeparam%codename
endif

! Put codeparam/codeversion
if (associated(cpo%codeparam%codeversion)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%codeversion)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%codeversion(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%codeversion(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codeversion",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%codeversion',cpo%codeparam%codeversion
endif

! Put codeparam/parameters
if (associated(cpo%codeparam%parameters)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%parameters)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%parameters(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%parameters(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/parameters",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%parameters',cpo%codeparam%parameters
endif

! Put codeparam/output_diag
if (associated(cpo%codeparam%output_diag)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%output_diag)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%output_diag(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%output_diag(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/output_diag",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%output_diag',cpo%codeparam%output_diag
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine

subroutine euITM_put_non_timed_equilibrium(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_equilibrium) :: cpo       
    
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put eqconstraint/bvac_r/source
if (associated(cpo%eqconstraint%bvac_r%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%bvac_r%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%bvac_r%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%bvac_r%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/bvac_r/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%bvac_r%source',cpo%eqconstraint%bvac_r%source
endif

! Put eqconstraint/bvac_r/exact        
if (cpo%eqconstraint%bvac_r%exact.NE.-999999999) then
   call put_int(idx,path, "eqconstraint/bvac_r/exact",cpo%eqconstraint%bvac_r%exact)        
   write(*,*) 'Put cpo%eqconstraint%bvac_r%exact',cpo%eqconstraint%bvac_r%exact
endif

! Put eqconstraint/i_plasma/source
if (associated(cpo%eqconstraint%i_plasma%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%i_plasma%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%i_plasma%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%i_plasma%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/i_plasma/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%i_plasma%source',cpo%eqconstraint%i_plasma%source
endif

! Put eqconstraint/i_plasma/exact        
if (cpo%eqconstraint%i_plasma%exact.NE.-999999999) then
   call put_int(idx,path, "eqconstraint/i_plasma/exact",cpo%eqconstraint%i_plasma%exact)        
   write(*,*) 'Put cpo%eqconstraint%i_plasma%exact',cpo%eqconstraint%i_plasma%exact
endif

! Put eqconstraint/magnet_iron/mr/source
if (associated(cpo%eqconstraint%magnet_iron%mr%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%magnet_iron%mr%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%magnet_iron%mr%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%magnet_iron%mr%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/magnet_iron/mr/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mr%source',cpo%eqconstraint%magnet_iron%mr%source
endif

! Put eqconstraint/magnet_iron/mz/source
if (associated(cpo%eqconstraint%magnet_iron%mz%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%magnet_iron%mz%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%magnet_iron%mz%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%magnet_iron%mz%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/magnet_iron/mz/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%magnet_iron%mz%source',cpo%eqconstraint%magnet_iron%mz%source
endif

! Put eqconstraint/bpol/source
if (associated(cpo%eqconstraint%bpol%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%bpol%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%bpol%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%bpol%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/bpol/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%bpol%source',cpo%eqconstraint%bpol%source
endif

! Put eqconstraint/flux/source
if (associated(cpo%eqconstraint%flux%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%flux%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%flux%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%flux%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/flux/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%flux%source',cpo%eqconstraint%flux%source
endif

! Put eqconstraint/mse/source
if (associated(cpo%eqconstraint%mse%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%mse%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%mse%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%mse%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/mse/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%mse%source',cpo%eqconstraint%mse%source
endif

! Put eqconstraint/faraday/source
if (associated(cpo%eqconstraint%faraday%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%faraday%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%faraday%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%faraday%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/faraday/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%faraday%source',cpo%eqconstraint%faraday%source
endif

! Put eqconstraint/pfcurrent/source
if (associated(cpo%eqconstraint%pfcurrent%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%pfcurrent%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%pfcurrent%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%pfcurrent%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/pfcurrent/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%pfcurrent%source',cpo%eqconstraint%pfcurrent%source
endif

! Put eqconstraint/pressure/source
if (associated(cpo%eqconstraint%pressure%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%pressure%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%pressure%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%pressure%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/pressure/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%pressure%source',cpo%eqconstraint%pressure%source
endif

! Put eqconstraint/jsurf/source
if (associated(cpo%eqconstraint%jsurf%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqconstraint%jsurf%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqconstraint%jsurf%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqconstraint%jsurf%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqconstraint/jsurf/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqconstraint%jsurf%source',cpo%eqconstraint%jsurf%source
endif

! Put eqconstraint/q/exact        
if (cpo%eqconstraint%q%exact.NE.-999999999) then
   call put_int(idx,path, "eqconstraint/q/exact",cpo%eqconstraint%q%exact)        
   write(*,*) 'Put cpo%eqconstraint%q%exact',cpo%eqconstraint%q%exact
endif

! Put eqgeometry/datainfo/dataprovider
if (associated(cpo%eqgeometry%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%eqgeometry%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqgeometry%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqgeometry%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqgeometry%datainfo%dataprovider',cpo%eqgeometry%datainfo%dataprovider
endif

! Put eqgeometry/datainfo/putdate
if (associated(cpo%eqgeometry%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%eqgeometry%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqgeometry%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqgeometry%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqgeometry%datainfo%putdate',cpo%eqgeometry%datainfo%putdate
endif

! Put eqgeometry/datainfo/source
if (associated(cpo%eqgeometry%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%eqgeometry%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqgeometry%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqgeometry%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqgeometry%datainfo%source',cpo%eqgeometry%datainfo%source
endif

! Put eqgeometry/datainfo/comment
if (associated(cpo%eqgeometry%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%eqgeometry%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqgeometry%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqgeometry%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqgeometry%datainfo%comment',cpo%eqgeometry%datainfo%comment
endif

! Put eqgeometry/datainfo/isref        
if (cpo%eqgeometry%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "eqgeometry/datainfo/isref",cpo%eqgeometry%datainfo%isref)        
   write(*,*) 'Put cpo%eqgeometry%datainfo%isref',cpo%eqgeometry%datainfo%isref
endif

! Put eqgeometry/datainfo/whatref        
if (cpo%eqgeometry%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "eqgeometry/datainfo/whatref",cpo%eqgeometry%datainfo%whatref)        
   write(*,*) 'Put cpo%eqgeometry%datainfo%whatref',cpo%eqgeometry%datainfo%whatref
endif

! Put eqgeometry/datainfo/putinfo/putmethod
if (associated(cpo%eqgeometry%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%eqgeometry%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqgeometry%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqgeometry%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqgeometry%datainfo%putinfo%putmethod',cpo%eqgeometry%datainfo%putinfo%putmethod
endif

! Put eqgeometry/datainfo/putinfo/putaccess
if (associated(cpo%eqgeometry%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%eqgeometry%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqgeometry%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqgeometry%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqgeometry%datainfo%putinfo%putaccess',cpo%eqgeometry%datainfo%putinfo%putaccess
endif

! Put eqgeometry/datainfo/putinfo/putlocation
if (associated(cpo%eqgeometry%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%eqgeometry%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqgeometry%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqgeometry%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqgeometry%datainfo%putinfo%putlocation',cpo%eqgeometry%datainfo%putinfo%putlocation
endif

! Put eqgeometry/datainfo/putinfo/rights
if (associated(cpo%eqgeometry%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%eqgeometry%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%eqgeometry%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%eqgeometry%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "eqgeometry/datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%eqgeometry%datainfo%putinfo%rights',cpo%eqgeometry%datainfo%putinfo%rights
endif

! Put flush/datainfo/dataprovider
if (associated(cpo%flush%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%flush%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%datainfo%dataprovider',cpo%flush%datainfo%dataprovider
endif

! Put flush/datainfo/putdate
if (associated(cpo%flush%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%flush%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%datainfo%putdate',cpo%flush%datainfo%putdate
endif

! Put flush/datainfo/source
if (associated(cpo%flush%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%flush%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%datainfo%source',cpo%flush%datainfo%source
endif

! Put flush/datainfo/comment
if (associated(cpo%flush%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%flush%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%datainfo%comment',cpo%flush%datainfo%comment
endif

! Put flush/datainfo/isref        
if (cpo%flush%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "flush/datainfo/isref",cpo%flush%datainfo%isref)        
   write(*,*) 'Put cpo%flush%datainfo%isref',cpo%flush%datainfo%isref
endif

! Put flush/datainfo/whatref        
if (cpo%flush%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "flush/datainfo/whatref",cpo%flush%datainfo%whatref)        
   write(*,*) 'Put cpo%flush%datainfo%whatref',cpo%flush%datainfo%whatref
endif

! Put flush/datainfo/putinfo/putmethod
if (associated(cpo%flush%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%flush%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%datainfo%putinfo%putmethod',cpo%flush%datainfo%putinfo%putmethod
endif

! Put flush/datainfo/putinfo/putaccess
if (associated(cpo%flush%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%flush%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%datainfo%putinfo%putaccess',cpo%flush%datainfo%putinfo%putaccess
endif

! Put flush/datainfo/putinfo/putlocation
if (associated(cpo%flush%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%flush%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%datainfo%putinfo%putlocation',cpo%flush%datainfo%putinfo%putlocation
endif

! Put flush/datainfo/putinfo/rights
if (associated(cpo%flush%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%flush%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "flush/datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%datainfo%putinfo%rights',cpo%flush%datainfo%putinfo%rights
endif

! Put flush/codeparam/codename
if (associated(cpo%flush%codeparam%codename)) then
   longstring = ' '    
   lenstring = size(cpo%flush%codeparam%codename)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%codeparam%codename(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%codeparam%codename(istring)
      enddo
   endif
   call put_string(idx,path, "flush/codeparam/codename",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%codeparam%codename',cpo%flush%codeparam%codename
endif

! Put flush/codeparam/codeversion
if (associated(cpo%flush%codeparam%codeversion)) then
   longstring = ' '    
   lenstring = size(cpo%flush%codeparam%codeversion)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%codeparam%codeversion(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%codeparam%codeversion(istring)
      enddo
   endif
   call put_string(idx,path, "flush/codeparam/codeversion",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%codeparam%codeversion',cpo%flush%codeparam%codeversion
endif

! Put flush/codeparam/parameters
if (associated(cpo%flush%codeparam%parameters)) then
   longstring = ' '    
   lenstring = size(cpo%flush%codeparam%parameters)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%codeparam%parameters(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%codeparam%parameters(istring)
      enddo
   endif
   call put_string(idx,path, "flush/codeparam/parameters",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%codeparam%parameters',cpo%flush%codeparam%parameters
endif

! Put flush/codeparam/output_diag
if (associated(cpo%flush%codeparam%output_diag)) then
   longstring = ' '    
   lenstring = size(cpo%flush%codeparam%output_diag)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%flush%codeparam%output_diag(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%flush%codeparam%output_diag(istring)
      enddo
   endif
   call put_string(idx,path, "flush/codeparam/output_diag",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%flush%codeparam%output_diag',cpo%flush%codeparam%output_diag
endif

! Put profiles_2d/grid_type
if (associated(cpo%profiles_2d%grid_type)) then
   longstring = ' '    
   lenstring = size(cpo%profiles_2d%grid_type)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%profiles_2d%grid_type(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%profiles_2d%grid_type(istring)
      enddo
   endif
   call put_string(idx,path, "profiles_2d/grid_type",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%profiles_2d%grid_type',cpo%profiles_2d%grid_type
endif

! Put coord_sys/grid_type
if (associated(cpo%coord_sys%grid_type)) then
   longstring = ' '    
   lenstring = size(cpo%coord_sys%grid_type)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%coord_sys%grid_type(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%coord_sys%grid_type(istring)
      enddo
   endif
   call put_string(idx,path, "coord_sys/grid_type",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%coord_sys%grid_type',cpo%coord_sys%grid_type
endif

! Put codeparam/codename
if (associated(cpo%codeparam%codename)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%codename)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%codename(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%codename(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codename",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%codename',cpo%codeparam%codename
endif

! Put codeparam/codeversion
if (associated(cpo%codeparam%codeversion)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%codeversion)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%codeversion(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%codeversion(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codeversion",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%codeversion',cpo%codeparam%codeversion
endif

! Put codeparam/parameters
if (associated(cpo%codeparam%parameters)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%parameters)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%parameters(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%parameters(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/parameters",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%parameters',cpo%codeparam%parameters
endif

! Put codeparam/output_diag
if (associated(cpo%codeparam%output_diag)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%output_diag)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%output_diag(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%output_diag(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/output_diag",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%output_diag',cpo%codeparam%output_diag
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine

subroutine euITM_put_non_timed_ironmodel(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_ironmodel) :: cpo       
    
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put desc_iron/name
if (associated(cpo%desc_iron%name)) then
   dim1 = size(cpo%desc_iron%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%desc_iron%name(i))
   enddo
   call put_Vect1d_String(idx,path, "desc_iron/name", &
         cpo%desc_iron%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%desc_iron%name'
endif

! Put desc_iron/id
if (associated(cpo%desc_iron%id)) then
   dim1 = size(cpo%desc_iron%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%desc_iron%id(i))
   enddo
   call put_Vect1d_String(idx,path, "desc_iron/id", &
         cpo%desc_iron%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%desc_iron%id'
endif

! Put desc_iron/permeability/B        
if (associated(cpo%desc_iron%permeability%B)) then   
   call put_vect2d_double(idx,path, "desc_iron/permeability/B",&
   cpo%desc_iron%permeability%B, &
   size(cpo%desc_iron%permeability%B,1),size(cpo%desc_iron%permeability%B,2),0)  
   write(*,*) 'Put cpo%desc_iron%permeability%B',cpo%desc_iron%permeability%B
endif

! Put desc_iron/permeability/mur        
if (associated(cpo%desc_iron%permeability%mur)) then   
   call put_vect2d_double(idx,path, "desc_iron/permeability/mur",&
   cpo%desc_iron%permeability%mur, &
   size(cpo%desc_iron%permeability%mur,1),size(cpo%desc_iron%permeability%mur,2),0)  
   write(*,*) 'Put cpo%desc_iron%permeability%mur',cpo%desc_iron%permeability%mur
endif

! Put desc_iron/geom_iron/npoints 
if (associated(cpo%desc_iron%geom_iron%npoints)) then         
   call put_vect1d_int(idx,path, "desc_iron/geom_iron/npoints",cpo%desc_iron%geom_iron%npoints,&
   size(cpo%desc_iron%geom_iron%npoints),0) 
   write(*,*) 'Put cpo%desc_iron%geom_iron%npoints',cpo%desc_iron%geom_iron%npoints
endif

! Put desc_iron/geom_iron/rzcoordinate/r        
if (associated(cpo%desc_iron%geom_iron%rzcoordinate%r)) then   
   call put_vect2d_double(idx,path, "desc_iron/geom_iron/rzcoordinate/r",&
   cpo%desc_iron%geom_iron%rzcoordinate%r, &
   size(cpo%desc_iron%geom_iron%rzcoordinate%r,1),size(cpo%desc_iron%geom_iron%rzcoordinate%r,2),0)  
   write(*,*) 'Put cpo%desc_iron%geom_iron%rzcoordinate%r',cpo%desc_iron%geom_iron%rzcoordinate%r
endif

! Put desc_iron/geom_iron/rzcoordinate/z        
if (associated(cpo%desc_iron%geom_iron%rzcoordinate%z)) then   
   call put_vect2d_double(idx,path, "desc_iron/geom_iron/rzcoordinate/z",&
   cpo%desc_iron%geom_iron%rzcoordinate%z, &
   size(cpo%desc_iron%geom_iron%rzcoordinate%z,1),size(cpo%desc_iron%geom_iron%rzcoordinate%z,2),0)  
   write(*,*) 'Put cpo%desc_iron%geom_iron%rzcoordinate%z',cpo%desc_iron%geom_iron%rzcoordinate%z
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine

subroutine euITM_put_non_timed_interfdiag(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_lineintegraldiag) :: cpo      
	
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put expression
if (associated(cpo%expression)) then
   longstring = ' '    
   lenstring = size(cpo%expression)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%expression(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%expression(istring)
      enddo
   endif
   call put_string(idx,path, "expression",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%expression',cpo%expression
endif

! Put setup_line/pivot_point/r
if (associated(cpo%setup_line%pivot_point%r)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/r",cpo%setup_line%pivot_point%r,&
   size(cpo%setup_line%pivot_point%r),0)
   write(*,*) 'Put cpo%setup_line%pivot_point%r',cpo%setup_line%pivot_point%r
endif

! Put setup_line/pivot_point/z
if (associated(cpo%setup_line%pivot_point%z)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/z",cpo%setup_line%pivot_point%z,&
   size(cpo%setup_line%pivot_point%z),0)
   write(*,*) 'Put cpo%setup_line%pivot_point%z',cpo%setup_line%pivot_point%z
endif

! Put setup_line/pivot_point/phi
if (associated(cpo%setup_line%pivot_point%phi)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/phi",cpo%setup_line%pivot_point%phi,&
   size(cpo%setup_line%pivot_point%phi),0)
   write(*,*) 'Put cpo%setup_line%pivot_point%phi',cpo%setup_line%pivot_point%phi
endif

! Put setup_line/polchordang
if (associated(cpo%setup_line%polchordang)) then   
   call put_vect1d_double(idx,path, "setup_line/polchordang",cpo%setup_line%polchordang,&
   size(cpo%setup_line%polchordang),0)
   write(*,*) 'Put cpo%setup_line%polchordang',cpo%setup_line%polchordang
endif

! Put setup_line/torchordang
if (associated(cpo%setup_line%torchordang)) then   
   call put_vect1d_double(idx,path, "setup_line/torchordang",cpo%setup_line%torchordang,&
   size(cpo%setup_line%torchordang),0)
   write(*,*) 'Put cpo%setup_line%torchordang',cpo%setup_line%torchordang
endif

! Put setup_line/second_point/r
if (associated(cpo%setup_line%second_point%r)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/r",cpo%setup_line%second_point%r,&
   size(cpo%setup_line%second_point%r),0)
   write(*,*) 'Put cpo%setup_line%second_point%r',cpo%setup_line%second_point%r
endif

! Put setup_line/second_point/z
if (associated(cpo%setup_line%second_point%z)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/z",cpo%setup_line%second_point%z,&
   size(cpo%setup_line%second_point%z),0)
   write(*,*) 'Put cpo%setup_line%second_point%z',cpo%setup_line%second_point%z
endif

! Put setup_line/second_point/phi
if (associated(cpo%setup_line%second_point%phi)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/phi",cpo%setup_line%second_point%phi,&
   size(cpo%setup_line%second_point%phi),0)
   write(*,*) 'Put cpo%setup_line%second_point%phi',cpo%setup_line%second_point%phi
endif

! Put setup_line/nchordpoints        
if (cpo%setup_line%nchordpoints.NE.-999999999) then
   call put_int(idx,path, "setup_line/nchordpoints",cpo%setup_line%nchordpoints)        
   write(*,*) 'Put cpo%setup_line%nchordpoints',cpo%setup_line%nchordpoints
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine

subroutine euITM_put_non_timed_mhd(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_mhd) :: cpo       
    
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put codeparam/codename
if (associated(cpo%codeparam%codename)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%codename)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%codename(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%codename(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codename",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%codename',cpo%codeparam%codename
endif

! Put codeparam/codeversion
if (associated(cpo%codeparam%codeversion)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%codeversion)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%codeversion(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%codeversion(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/codeversion",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%codeversion',cpo%codeparam%codeversion
endif

! Put codeparam/parameters
if (associated(cpo%codeparam%parameters)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%parameters)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%parameters(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%parameters(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/parameters",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%parameters',cpo%codeparam%parameters
endif

! Put codeparam/output_diag
if (associated(cpo%codeparam%output_diag)) then
   longstring = ' '    
   lenstring = size(cpo%codeparam%output_diag)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%codeparam%output_diag(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%codeparam%output_diag(istring)
      enddo
   endif
   call put_string(idx,path, "codeparam/output_diag",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%codeparam%output_diag',cpo%codeparam%output_diag
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine

subroutine euITM_put_non_timed_magdiag(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_magdiag) :: cpo       
    
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put flux_loops/setup_floops/name
if (associated(cpo%flux_loops%setup_floops%name)) then
   dim1 = size(cpo%flux_loops%setup_floops%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%flux_loops%setup_floops%name(i))
   enddo
   call put_Vect1d_String(idx,path, "flux_loops/setup_floops/name", &
         cpo%flux_loops%setup_floops%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%flux_loops%setup_floops%name'
endif

! Put flux_loops/setup_floops/id
if (associated(cpo%flux_loops%setup_floops%id)) then
   dim1 = size(cpo%flux_loops%setup_floops%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%flux_loops%setup_floops%id(i))
   enddo
   call put_Vect1d_String(idx,path, "flux_loops/setup_floops/id", &
         cpo%flux_loops%setup_floops%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%flux_loops%setup_floops%id'
endif

! Put flux_loops/setup_floops/position/r        
if (associated(cpo%flux_loops%setup_floops%position%r)) then   
   call put_vect2d_double(idx,path, "flux_loops/setup_floops/position/r",&
   cpo%flux_loops%setup_floops%position%r, &
   size(cpo%flux_loops%setup_floops%position%r,1),size(cpo%flux_loops%setup_floops%position%r,2),0)  
   write(*,*) 'Put cpo%flux_loops%setup_floops%position%r',cpo%flux_loops%setup_floops%position%r
endif

! Put flux_loops/setup_floops/position/z        
if (associated(cpo%flux_loops%setup_floops%position%z)) then   
   call put_vect2d_double(idx,path, "flux_loops/setup_floops/position/z",&
   cpo%flux_loops%setup_floops%position%z, &
   size(cpo%flux_loops%setup_floops%position%z,1),size(cpo%flux_loops%setup_floops%position%z,2),0)  
   write(*,*) 'Put cpo%flux_loops%setup_floops%position%z',cpo%flux_loops%setup_floops%position%z
endif

! Put flux_loops/setup_floops/position/phi        
if (associated(cpo%flux_loops%setup_floops%position%phi)) then   
   call put_vect2d_double(idx,path, "flux_loops/setup_floops/position/phi",&
   cpo%flux_loops%setup_floops%position%phi, &
   size(cpo%flux_loops%setup_floops%position%phi,1),size(cpo%flux_loops%setup_floops%position%phi,2),0)  
   write(*,*) 'Put cpo%flux_loops%setup_floops%position%phi',cpo%flux_loops%setup_floops%position%phi
endif

! Put flux_loops/setup_floops/npoints 
if (associated(cpo%flux_loops%setup_floops%npoints)) then         
   call put_vect1d_int(idx,path, "flux_loops/setup_floops/npoints",cpo%flux_loops%setup_floops%npoints,&
   size(cpo%flux_loops%setup_floops%npoints),0) 
   write(*,*) 'Put cpo%flux_loops%setup_floops%npoints',cpo%flux_loops%setup_floops%npoints
endif

! Put bpol_probes/setup_bprobe/name
if (associated(cpo%bpol_probes%setup_bprobe%name)) then
   dim1 = size(cpo%bpol_probes%setup_bprobe%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%bpol_probes%setup_bprobe%name(i))
   enddo
   call put_Vect1d_String(idx,path, "bpol_probes/setup_bprobe/name", &
         cpo%bpol_probes%setup_bprobe%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%bpol_probes%setup_bprobe%name'
endif

! Put bpol_probes/setup_bprobe/id
if (associated(cpo%bpol_probes%setup_bprobe%id)) then
   dim1 = size(cpo%bpol_probes%setup_bprobe%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%bpol_probes%setup_bprobe%id(i))
   enddo
   call put_Vect1d_String(idx,path, "bpol_probes/setup_bprobe/id", &
         cpo%bpol_probes%setup_bprobe%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%bpol_probes%setup_bprobe%id'
endif

! Put bpol_probes/setup_bprobe/position/r
if (associated(cpo%bpol_probes%setup_bprobe%position%r)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/position/r",cpo%bpol_probes%setup_bprobe%position%r,&
   size(cpo%bpol_probes%setup_bprobe%position%r),0)
   write(*,*) 'Put cpo%bpol_probes%setup_bprobe%position%r',cpo%bpol_probes%setup_bprobe%position%r
endif

! Put bpol_probes/setup_bprobe/position/z
if (associated(cpo%bpol_probes%setup_bprobe%position%z)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/position/z",cpo%bpol_probes%setup_bprobe%position%z,&
   size(cpo%bpol_probes%setup_bprobe%position%z),0)
   write(*,*) 'Put cpo%bpol_probes%setup_bprobe%position%z',cpo%bpol_probes%setup_bprobe%position%z
endif

! Put bpol_probes/setup_bprobe/polangle
if (associated(cpo%bpol_probes%setup_bprobe%polangle)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/polangle",cpo%bpol_probes%setup_bprobe%polangle,&
   size(cpo%bpol_probes%setup_bprobe%polangle),0)
   write(*,*) 'Put cpo%bpol_probes%setup_bprobe%polangle',cpo%bpol_probes%setup_bprobe%polangle
endif

! Put bpol_probes/setup_bprobe/torangle
if (associated(cpo%bpol_probes%setup_bprobe%torangle)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/torangle",cpo%bpol_probes%setup_bprobe%torangle,&
   size(cpo%bpol_probes%setup_bprobe%torangle),0)
   write(*,*) 'Put cpo%bpol_probes%setup_bprobe%torangle',cpo%bpol_probes%setup_bprobe%torangle
endif

! Put bpol_probes/setup_bprobe/area
if (associated(cpo%bpol_probes%setup_bprobe%area)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/area",cpo%bpol_probes%setup_bprobe%area,&
   size(cpo%bpol_probes%setup_bprobe%area),0)
   write(*,*) 'Put cpo%bpol_probes%setup_bprobe%area',cpo%bpol_probes%setup_bprobe%area
endif

! Put bpol_probes/setup_bprobe/length
if (associated(cpo%bpol_probes%setup_bprobe%length)) then   
   call put_vect1d_double(idx,path, "bpol_probes/setup_bprobe/length",cpo%bpol_probes%setup_bprobe%length,&
   size(cpo%bpol_probes%setup_bprobe%length),0)
   write(*,*) 'Put cpo%bpol_probes%setup_bprobe%length',cpo%bpol_probes%setup_bprobe%length
endif

! Put bpol_probes/setup_bprobe/turns 
if (associated(cpo%bpol_probes%setup_bprobe%turns)) then         
   call put_vect1d_int(idx,path, "bpol_probes/setup_bprobe/turns",cpo%bpol_probes%setup_bprobe%turns,&
   size(cpo%bpol_probes%setup_bprobe%turns),0) 
   write(*,*) 'Put cpo%bpol_probes%setup_bprobe%turns',cpo%bpol_probes%setup_bprobe%turns
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine

subroutine euITM_put_non_timed_msediag(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_msediag) :: cpo       
    
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put setup_mse/rzgamma/r
if (associated(cpo%setup_mse%rzgamma%r)) then   
   call put_vect1d_double(idx,path, "setup_mse/rzgamma/r",cpo%setup_mse%rzgamma%r,&
   size(cpo%setup_mse%rzgamma%r),0)
   write(*,*) 'Put cpo%setup_mse%rzgamma%r',cpo%setup_mse%rzgamma%r
endif

! Put setup_mse/rzgamma/z
if (associated(cpo%setup_mse%rzgamma%z)) then   
   call put_vect1d_double(idx,path, "setup_mse/rzgamma/z",cpo%setup_mse%rzgamma%z,&
   size(cpo%setup_mse%rzgamma%z),0)
   write(*,*) 'Put cpo%setup_mse%rzgamma%z',cpo%setup_mse%rzgamma%z
endif

! Put setup_mse/geom_coef        
if (associated(cpo%setup_mse%geom_coef)) then   
   call put_vect2d_double(idx,path, "setup_mse/geom_coef",&
   cpo%setup_mse%geom_coef, &
   size(cpo%setup_mse%geom_coef,1),size(cpo%setup_mse%geom_coef,2),0)  
   write(*,*) 'Put cpo%setup_mse%geom_coef',cpo%setup_mse%geom_coef
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine

subroutine euITM_put_non_timed_pfsystems(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_pfsystems) :: cpo       
    
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put pfcoils/desc_pfcoils/name
if (associated(cpo%pfcoils%desc_pfcoils%name)) then
   dim1 = size(cpo%pfcoils%desc_pfcoils%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%pfcoils%desc_pfcoils%name(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcoils/desc_pfcoils/name", &
         cpo%pfcoils%desc_pfcoils%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%name'
endif

! Put pfcoils/desc_pfcoils/id
if (associated(cpo%pfcoils%desc_pfcoils%id)) then
   dim1 = size(cpo%pfcoils%desc_pfcoils%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%pfcoils%desc_pfcoils%id(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcoils/desc_pfcoils/id", &
         cpo%pfcoils%desc_pfcoils%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%id'
endif

! Put pfcoils/desc_pfcoils/res
if (associated(cpo%pfcoils%desc_pfcoils%res)) then   
   call put_vect1d_double(idx,path, "pfcoils/desc_pfcoils/res",cpo%pfcoils%desc_pfcoils%res,&
   size(cpo%pfcoils%desc_pfcoils%res),0)
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%res',cpo%pfcoils%desc_pfcoils%res
endif

! Put pfcoils/desc_pfcoils/emax
if (associated(cpo%pfcoils%desc_pfcoils%emax)) then   
   call put_vect1d_double(idx,path, "pfcoils/desc_pfcoils/emax",cpo%pfcoils%desc_pfcoils%emax,&
   size(cpo%pfcoils%desc_pfcoils%emax),0)
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%emax',cpo%pfcoils%desc_pfcoils%emax
endif

! Put pfcoils/desc_pfcoils/nelement 
if (associated(cpo%pfcoils%desc_pfcoils%nelement)) then         
   call put_vect1d_int(idx,path, "pfcoils/desc_pfcoils/nelement",cpo%pfcoils%desc_pfcoils%nelement,&
   size(cpo%pfcoils%desc_pfcoils%nelement),0) 
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%nelement',cpo%pfcoils%desc_pfcoils%nelement
endif

! Put pfcoils/desc_pfcoils/pfelement/name
if (associated(cpo%pfcoils%desc_pfcoils%pfelement%name)) then
   dim1 = size(cpo%pfcoils%desc_pfcoils%pfelement%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%pfcoils%desc_pfcoils%pfelement%name(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcoils/desc_pfcoils/pfelement/name", &
         cpo%pfcoils%desc_pfcoils%pfelement%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%pfelement%name'
endif

! Put pfcoils/desc_pfcoils/pfelement/id
if (associated(cpo%pfcoils%desc_pfcoils%pfelement%id)) then
   dim1 = size(cpo%pfcoils%desc_pfcoils%pfelement%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%pfcoils%desc_pfcoils%pfelement%id(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcoils/desc_pfcoils/pfelement/id", &
         cpo%pfcoils%desc_pfcoils%pfelement%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%pfelement%id'
endif

! Put pfcoils/desc_pfcoils/pfelement/turnsign        
if (associated(cpo%pfcoils%desc_pfcoils%pfelement%turnsign)) then   
   call put_vect2d_double(idx,path, "pfcoils/desc_pfcoils/pfelement/turnsign",&
   cpo%pfcoils%desc_pfcoils%pfelement%turnsign, &
   size(cpo%pfcoils%desc_pfcoils%pfelement%turnsign,1),size(cpo%pfcoils%desc_pfcoils%pfelement%turnsign,2),0)  
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%pfelement%turnsign',cpo%pfcoils%desc_pfcoils%pfelement%turnsign
endif

! Put pfcoils/desc_pfcoils/pfelement/area        
if (associated(cpo%pfcoils%desc_pfcoils%pfelement%area)) then   
   call put_vect2d_double(idx,path, "pfcoils/desc_pfcoils/pfelement/area",&
   cpo%pfcoils%desc_pfcoils%pfelement%area, &
   size(cpo%pfcoils%desc_pfcoils%pfelement%area,1),size(cpo%pfcoils%desc_pfcoils%pfelement%area,2),0)  
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%pfelement%area',cpo%pfcoils%desc_pfcoils%pfelement%area
endif

! Put pfcoils/desc_pfcoils/pfelement/pfgeometry/type        
if (associated(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%type)) then   
   call put_vect2d_int(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/type",&
   cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%type, &
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%type,1), &
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%type,2),0)  
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%type',cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%type
endif

! Put pfcoils/desc_pfcoils/pfelement/pfgeometry/npoints        
if (associated(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints)) then   
   call put_vect2d_int(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/npoints",&
   cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints, &
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints,1), &
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints,2),0)  
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints',cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%npoints
endif

! Put pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/r        
if (associated(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r)) then   
   call put_vect3d_double(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/r", &
   cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r, &
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r,1),&
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r,2), &
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r,3),0)    
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%r'
endif

! Put pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/z        
if (associated(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z)) then   
   call put_vect3d_double(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzcoordinate/z", &
   cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z, &
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z,1),&
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z,2), &
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z,3),0)    
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzcoordinate%z'
endif

! Put pfcoils/desc_pfcoils/pfelement/pfgeometry/rzdrdz        
if (associated(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz)) then   
   call put_vect3d_double(idx,path, "pfcoils/desc_pfcoils/pfelement/pfgeometry/rzdrdz", &
   cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz, &
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz,1),&
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz,2), &
   size(cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz,3),0)    
   write(*,*) 'Put cpo%pfcoils%desc_pfcoils%pfelement%pfgeometry%rzdrdz'
endif

! Put pfpassive/area
if (associated(cpo%pfpassive%area)) then   
   call put_vect1d_double(idx,path, "pfpassive/area",cpo%pfpassive%area,&
   size(cpo%pfpassive%area),0)
   write(*,*) 'Put cpo%pfpassive%area',cpo%pfpassive%area
endif

! Put pfpassive/res
if (associated(cpo%pfpassive%res)) then   
   call put_vect1d_double(idx,path, "pfpassive/res",cpo%pfpassive%res,&
   size(cpo%pfpassive%res),0)
   write(*,*) 'Put cpo%pfpassive%res',cpo%pfpassive%res
endif

! Put pfpassive/pfpageometry/type 
if (associated(cpo%pfpassive%pfpageometry%type)) then         
   call put_vect1d_int(idx,path, "pfpassive/pfpageometry/type",cpo%pfpassive%pfpageometry%type,&
   size(cpo%pfpassive%pfpageometry%type),0) 
   write(*,*) 'Put cpo%pfpassive%pfpageometry%type',cpo%pfpassive%pfpageometry%type
endif

! Put pfpassive/pfpageometry/npoints 
if (associated(cpo%pfpassive%pfpageometry%npoints)) then         
   call put_vect1d_int(idx,path, "pfpassive/pfpageometry/npoints",cpo%pfpassive%pfpageometry%npoints,&
   size(cpo%pfpassive%pfpageometry%npoints),0) 
   write(*,*) 'Put cpo%pfpassive%pfpageometry%npoints',cpo%pfpassive%pfpageometry%npoints
endif

! Put pfpassive/pfpageometry/rzcoordinate/r        
if (associated(cpo%pfpassive%pfpageometry%rzcoordinate%r)) then   
   call put_vect2d_double(idx,path, "pfpassive/pfpageometry/rzcoordinate/r",&
   cpo%pfpassive%pfpageometry%rzcoordinate%r, &
   size(cpo%pfpassive%pfpageometry%rzcoordinate%r,1),size(cpo%pfpassive%pfpageometry%rzcoordinate%r,2),0)  
   write(*,*) 'Put cpo%pfpassive%pfpageometry%rzcoordinate%r',cpo%pfpassive%pfpageometry%rzcoordinate%r
endif

! Put pfpassive/pfpageometry/rzcoordinate/z        
if (associated(cpo%pfpassive%pfpageometry%rzcoordinate%z)) then   
   call put_vect2d_double(idx,path, "pfpassive/pfpageometry/rzcoordinate/z",&
   cpo%pfpassive%pfpageometry%rzcoordinate%z, &
   size(cpo%pfpassive%pfpageometry%rzcoordinate%z,1),size(cpo%pfpassive%pfpageometry%rzcoordinate%z,2),0)  
   write(*,*) 'Put cpo%pfpassive%pfpageometry%rzcoordinate%z',cpo%pfpassive%pfpageometry%rzcoordinate%z
endif

! Put pfpassive/pfpageometry/rzdrdz        
if (associated(cpo%pfpassive%pfpageometry%rzdrdz)) then   
   call put_vect2d_double(idx,path, "pfpassive/pfpageometry/rzdrdz",&
   cpo%pfpassive%pfpageometry%rzdrdz, &
   size(cpo%pfpassive%pfpageometry%rzdrdz,1),size(cpo%pfpassive%pfpageometry%rzdrdz,2),0)  
   write(*,*) 'Put cpo%pfpassive%pfpageometry%rzdrdz',cpo%pfpassive%pfpageometry%rzdrdz
endif

! Put pfcircuits/name
if (associated(cpo%pfcircuits%name)) then
   dim1 = size(cpo%pfcircuits%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%pfcircuits%name(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcircuits/name", &
         cpo%pfcircuits%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%pfcircuits%name'
endif

! Put pfcircuits/id
if (associated(cpo%pfcircuits%id)) then
   dim1 = size(cpo%pfcircuits%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%pfcircuits%id(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcircuits/id", &
         cpo%pfcircuits%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%pfcircuits%id'
endif

! Put pfcircuits/type
if (associated(cpo%pfcircuits%type)) then
   dim1 = size(cpo%pfcircuits%type)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%pfcircuits%type(i))
   enddo
   call put_Vect1d_String(idx,path, "pfcircuits/type", &
         cpo%pfcircuits%type,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%pfcircuits%type'
endif

! Put pfcircuits/nnodes 
if (associated(cpo%pfcircuits%nnodes)) then         
   call put_vect1d_int(idx,path, "pfcircuits/nnodes",cpo%pfcircuits%nnodes,&
   size(cpo%pfcircuits%nnodes),0) 
   write(*,*) 'Put cpo%pfcircuits%nnodes',cpo%pfcircuits%nnodes
endif

! Put pfcircuits/connections        
if (associated(cpo%pfcircuits%connections)) then   
   call put_vect3d_int(idx,path, "pfcircuits/connections", &
   cpo%pfcircuits%connections, &
   size(cpo%pfcircuits%connections,1),&
   size(cpo%pfcircuits%connections,2),&
   size(cpo%pfcircuits%connections,3),0)    
   write(*,*) 'Put cpo%pfcircuits%connections',cpo%pfcircuits%connections
endif

! Put pfsupplies/desc_supply/name
if (associated(cpo%pfsupplies%desc_supply%name)) then
   dim1 = size(cpo%pfsupplies%desc_supply%name)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%pfsupplies%desc_supply%name(i))
   enddo
   call put_Vect1d_String(idx,path, "pfsupplies/desc_supply/name", &
         cpo%pfsupplies%desc_supply%name,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%pfsupplies%desc_supply%name'
endif

! Put pfsupplies/desc_supply/id
if (associated(cpo%pfsupplies%desc_supply%id)) then
   dim1 = size(cpo%pfsupplies%desc_supply%id)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%pfsupplies%desc_supply%id(i))
   enddo
   call put_Vect1d_String(idx,path, "pfsupplies/desc_supply/id", &
         cpo%pfsupplies%desc_supply%id,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%pfsupplies%desc_supply%id'
endif

! Put pfsupplies/desc_supply/type
if (associated(cpo%pfsupplies%desc_supply%type)) then
   dim1 = size(cpo%pfsupplies%desc_supply%type)
   allocate(dimtab(dim1))
   do i=1,dim1
      dimtab(i) = len_trim(cpo%pfsupplies%desc_supply%type(i))
   enddo
   call put_Vect1d_String(idx,path, "pfsupplies/desc_supply/type", &
         cpo%pfsupplies%desc_supply%type,dim1,dimtab,0)
   deallocate(dimtab)
   write(*,*) 'Put cpo%pfsupplies%desc_supply%type'
endif

! Put pfsupplies/desc_supply/delay
if (associated(cpo%pfsupplies%desc_supply%delay)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/delay",cpo%pfsupplies%desc_supply%delay,&
   size(cpo%pfsupplies%desc_supply%delay),0)
   write(*,*) 'Put cpo%pfsupplies%desc_supply%delay',cpo%pfsupplies%desc_supply%delay
endif

! Put pfsupplies/desc_supply/filter/num        
if (associated(cpo%pfsupplies%desc_supply%filter%num)) then   
   call put_vect2d_double(idx,path, "pfsupplies/desc_supply/filter/num",&
   cpo%pfsupplies%desc_supply%filter%num, &
   size(cpo%pfsupplies%desc_supply%filter%num,1),size(cpo%pfsupplies%desc_supply%filter%num,2),0)  
   write(*,*) 'Put cpo%pfsupplies%desc_supply%filter%num',cpo%pfsupplies%desc_supply%filter%num
endif

! Put pfsupplies/desc_supply/filter/den        
if (associated(cpo%pfsupplies%desc_supply%filter%den)) then   
   call put_vect2d_double(idx,path, "pfsupplies/desc_supply/filter/den",&
   cpo%pfsupplies%desc_supply%filter%den, &
   size(cpo%pfsupplies%desc_supply%filter%den,1),size(cpo%pfsupplies%desc_supply%filter%den,2),0)  
   write(*,*) 'Put cpo%pfsupplies%desc_supply%filter%den',cpo%pfsupplies%desc_supply%filter%den
endif

! Put pfsupplies/desc_supply/imin
if (associated(cpo%pfsupplies%desc_supply%imin)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/imin",cpo%pfsupplies%desc_supply%imin,&
   size(cpo%pfsupplies%desc_supply%imin),0)
   write(*,*) 'Put cpo%pfsupplies%desc_supply%imin',cpo%pfsupplies%desc_supply%imin
endif

! Put pfsupplies/desc_supply/imax
if (associated(cpo%pfsupplies%desc_supply%imax)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/imax",cpo%pfsupplies%desc_supply%imax,&
   size(cpo%pfsupplies%desc_supply%imax),0)
   write(*,*) 'Put cpo%pfsupplies%desc_supply%imax',cpo%pfsupplies%desc_supply%imax
endif

! Put pfsupplies/desc_supply/res
if (associated(cpo%pfsupplies%desc_supply%res)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/res",cpo%pfsupplies%desc_supply%res,&
   size(cpo%pfsupplies%desc_supply%res),0)
   write(*,*) 'Put cpo%pfsupplies%desc_supply%res',cpo%pfsupplies%desc_supply%res
endif

! Put pfsupplies/desc_supply/umin
if (associated(cpo%pfsupplies%desc_supply%umin)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/umin",cpo%pfsupplies%desc_supply%umin,&
   size(cpo%pfsupplies%desc_supply%umin),0)
   write(*,*) 'Put cpo%pfsupplies%desc_supply%umin',cpo%pfsupplies%desc_supply%umin
endif

! Put pfsupplies/desc_supply/umax
if (associated(cpo%pfsupplies%desc_supply%umax)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/umax",cpo%pfsupplies%desc_supply%umax,&
   size(cpo%pfsupplies%desc_supply%umax),0)
   write(*,*) 'Put cpo%pfsupplies%desc_supply%umax',cpo%pfsupplies%desc_supply%umax
endif

! Put pfsupplies/desc_supply/emax
if (associated(cpo%pfsupplies%desc_supply%emax)) then   
   call put_vect1d_double(idx,path, "pfsupplies/desc_supply/emax",cpo%pfsupplies%desc_supply%emax,&
   size(cpo%pfsupplies%desc_supply%emax),0)
   write(*,*) 'Put cpo%pfsupplies%desc_supply%emax',cpo%pfsupplies%desc_supply%emax
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine

subroutine euITM_put_non_timed_polardiag(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_lineintegraldiag) :: cpo      
	
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put expression
if (associated(cpo%expression)) then
   longstring = ' '    
   lenstring = size(cpo%expression)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%expression(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%expression(istring)
      enddo
   endif
   call put_string(idx,path, "expression",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%expression',cpo%expression
endif

! Put setup_line/pivot_point/r
if (associated(cpo%setup_line%pivot_point%r)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/r",cpo%setup_line%pivot_point%r,&
   size(cpo%setup_line%pivot_point%r),0)
   write(*,*) 'Put cpo%setup_line%pivot_point%r',cpo%setup_line%pivot_point%r
endif

! Put setup_line/pivot_point/z
if (associated(cpo%setup_line%pivot_point%z)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/z",cpo%setup_line%pivot_point%z,&
   size(cpo%setup_line%pivot_point%z),0)
   write(*,*) 'Put cpo%setup_line%pivot_point%z',cpo%setup_line%pivot_point%z
endif

! Put setup_line/pivot_point/phi
if (associated(cpo%setup_line%pivot_point%phi)) then   
   call put_vect1d_double(idx,path, "setup_line/pivot_point/phi",cpo%setup_line%pivot_point%phi,&
   size(cpo%setup_line%pivot_point%phi),0)
   write(*,*) 'Put cpo%setup_line%pivot_point%phi',cpo%setup_line%pivot_point%phi
endif

! Put setup_line/polchordang
if (associated(cpo%setup_line%polchordang)) then   
   call put_vect1d_double(idx,path, "setup_line/polchordang",cpo%setup_line%polchordang,&
   size(cpo%setup_line%polchordang),0)
   write(*,*) 'Put cpo%setup_line%polchordang',cpo%setup_line%polchordang
endif

! Put setup_line/torchordang
if (associated(cpo%setup_line%torchordang)) then   
   call put_vect1d_double(idx,path, "setup_line/torchordang",cpo%setup_line%torchordang,&
   size(cpo%setup_line%torchordang),0)
   write(*,*) 'Put cpo%setup_line%torchordang',cpo%setup_line%torchordang
endif

! Put setup_line/second_point/r
if (associated(cpo%setup_line%second_point%r)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/r",cpo%setup_line%second_point%r,&
   size(cpo%setup_line%second_point%r),0)
   write(*,*) 'Put cpo%setup_line%second_point%r',cpo%setup_line%second_point%r
endif

! Put setup_line/second_point/z
if (associated(cpo%setup_line%second_point%z)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/z",cpo%setup_line%second_point%z,&
   size(cpo%setup_line%second_point%z),0)
   write(*,*) 'Put cpo%setup_line%second_point%z',cpo%setup_line%second_point%z
endif

! Put setup_line/second_point/phi
if (associated(cpo%setup_line%second_point%phi)) then   
   call put_vect1d_double(idx,path, "setup_line/second_point/phi",cpo%setup_line%second_point%phi,&
   size(cpo%setup_line%second_point%phi),0)
   write(*,*) 'Put cpo%setup_line%second_point%phi',cpo%setup_line%second_point%phi
endif

! Put setup_line/nchordpoints        
if (cpo%setup_line%nchordpoints.NE.-999999999) then
   call put_int(idx,path, "setup_line/nchordpoints",cpo%setup_line%nchordpoints)        
   write(*,*) 'Put cpo%setup_line%nchordpoints',cpo%setup_line%nchordpoints
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine

subroutine euITM_put_non_timed_toroidfield(idx, path,  cpo)

use euITM_schemas
implicit none

character*(*) :: path
integer :: idx
integer :: i,dim1,dim2,dim3,dim4, lenstring, istring
integer, pointer :: dimtab(:)
character(len=100000)::longstring    


type(type_toroidfield) :: cpo       
    
call begin_cpo_put_non_timed(idx, path)  ! check this is the correct BEGIN to call 

! Put datainfo/dataprovider
if (associated(cpo%datainfo%dataprovider)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%dataprovider)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%dataprovider(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%dataprovider(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/dataprovider",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%dataprovider',cpo%datainfo%dataprovider
endif

! Put datainfo/putdate
if (associated(cpo%datainfo%putdate)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putdate)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putdate(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putdate(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putdate",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putdate',cpo%datainfo%putdate
endif

! Put datainfo/source
if (associated(cpo%datainfo%source)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%source)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%source(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%source(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/source",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%source',cpo%datainfo%source
endif

! Put datainfo/comment
if (associated(cpo%datainfo%comment)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%comment)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%comment(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%comment(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/comment",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%comment',cpo%datainfo%comment
endif

! Put datainfo/isref        
if (cpo%datainfo%isref.NE.-999999999) then
   call put_int(idx,path, "datainfo/isref",cpo%datainfo%isref)        
   write(*,*) 'Put cpo%datainfo%isref',cpo%datainfo%isref
endif

! Put datainfo/whatref        
if (cpo%datainfo%whatref.NE.-999999999) then
   call put_int(idx,path, "datainfo/whatref",cpo%datainfo%whatref)        
   write(*,*) 'Put cpo%datainfo%whatref',cpo%datainfo%whatref
endif

! Put datainfo/putinfo/putmethod
if (associated(cpo%datainfo%putinfo%putmethod)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putmethod)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putmethod(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putmethod(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putmethod",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putmethod',cpo%datainfo%putinfo%putmethod
endif

! Put datainfo/putinfo/putaccess
if (associated(cpo%datainfo%putinfo%putaccess)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putaccess)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putaccess(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putaccess(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putaccess",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putaccess',cpo%datainfo%putinfo%putaccess
endif

! Put datainfo/putinfo/putlocation
if (associated(cpo%datainfo%putinfo%putlocation)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%putlocation)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%putlocation(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%putlocation(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/putlocation",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%putlocation',cpo%datainfo%putinfo%putlocation
endif

! Put datainfo/putinfo/rights
if (associated(cpo%datainfo%putinfo%rights)) then
   longstring = ' '    
   lenstring = size(cpo%datainfo%putinfo%rights)      
   if (lenstring.EQ.1) then             
      longstring = trim(cpo%datainfo%putinfo%rights(1))
   else
      do istring=1,lenstring
          longstring(1+(istring-1)*132 : istring*132) = cpo%datainfo%putinfo%rights(istring)
      enddo
   endif
   call put_string(idx,path, "datainfo/putinfo/rights",trim(longstring))       ! should clean up longstring after that, or send to the put only the right length, which has been updated
   write(*,*) 'Put cpo%datainfo%putinfo%rights',cpo%datainfo%putinfo%rights
endif

! Put nturns        
if (cpo%nturns.NE.-999999999) then
   call put_int(idx,path, "nturns",cpo%nturns)        
   write(*,*) 'Put cpo%nturns',cpo%nturns
endif

! Put ncoils        
if (cpo%ncoils.NE.-999999999) then
   call put_int(idx,path, "ncoils",cpo%ncoils)        
   write(*,*) 'Put cpo%ncoils',cpo%ncoils
endif

call end_cpo_put_non_timed(idx, path)
      
return
endsubroutine


end module
 
