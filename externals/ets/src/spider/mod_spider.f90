module mod_spider

contains

!         program main_astr_fixed
! Create the library ~/MAstra/.lbr/Intel/esp.a as (see makefile)
! $>  cd ~/MAstra/esp/ ; make ALL 

      SUBROUTINE INT_SPIDER(&
! Control:
         key_equil   & 
! Input:
        ,neql	     &  ! radial grid in SPIDER (150)
        ,nteta	     &  ! poloidal grid in SPIDER (100)
        ,nbnd	     &  ! # boundary points (101)
        ,rzbnd	     &  ! array(2*nbnd) of boundary points
        ,na1	     &  ! radial grid for input (dimension of all arrays below)
        ,eqpf	     &  ! 
        ,eqff	     &  ! A
        ,rho	     &  ! S
        ,ipl	     &  ! T
        ,rtor	     &  ! R
        ,ztor	     &  ! Z
        ,btor	     &  ! A
        ,roc	     &  ! 
! Output:
        ,rocnew	     &  ! n
! Input:
        ,nstep	     &  ! o
! Input (and output if key_dmf = -2, -3)
        ,fp	     &  ! t
! Output:
        ,g11	     &  ! a
        ,g22	     &  ! t
        ,g33	     &  ! i
        ,vr	     &  ! o
        ,vrs	     &  ! n
        ,slat	     &  ! s
        ,gradro	     &  ! 
        ,mu	     &  ! &
        ,ipol	     &  ! 
        ,bmaxt       &  
        ,bmint       &  
        ,bdb02       &  
        ,bdb0        &  
        ,b0db2       &  
        ,droda       &  
        ,rout	     &                   
        ,zout	     &                  
        ,shafr_shift &! 
        ,elong       &! 
        ,elong_u     &! 
        ,elong_l     &! 
        ,triang_l    &!
        ,triang_u    &!
        ,amid        &!
        ,ftrap       &!
! Input:
        ,cc	     &  ! u
        ,Te	     &  ! n
        ,cubs	     &  ! i
        ,cd	     &  ! t
! Input:
        ,pres	     &  ! s
        ,cu	     &  ! 
        ) 

      USE ITM_CONSTANTS
      USE PARAMETERS

      IMPLICIT NONE
              

! +++ I/O:
      INTEGER               :: na1
      INTEGER               :: nbnd,    nstep,     key_equil
      INTEGER               :: neql,    nteta

      REAL (R8)             :: Cc(na1), CUbs(na1), Cd(na1),Cu(na1)
      REAL (R8)             :: Te(na1), pres(na1)
      REAL (R8)             :: Cu_out(na1)
      REAL (R8)             :: pres_s(na1)
      REAL (R8)             :: ymu(na1),yfp(na1)
      REAL (R8)             :: rzbnd(2*nbnd),eqpf(na1),eqff(na1),fp(na1),ipl,ybetpl
      REAL (R8)             :: yli3,rtor,ztor,btor,rho(na1),roc, ftrap(na1)
      REAL (R8)             :: g11(na1),g22(na1),g33(na1)
      REAL (R8)             :: vr(na1),vrs(na1),slat(na1),gradro(na1),rocnew
      REAL (R8)             :: mu(na1),ipol(na1),bmaxt(na1),bmint(na1),bdb02(na1)
      REAL (R8)             :: bdb0(na1),b0db2(na1),droda(na1)
      REAL (R8)             :: shafr_shift(na1),elong(na1),elong_u(na1),elong_l(na1),triang_l(na1),triang_u(na1),amid(na1)
      REAL (R8)             :: roc_input                      
      REAL (R8)             :: rout(neql,nteta), zout(neql,nteta)                      


! +++ locals:
      REAL (R8)             :: hro, yreler, platok,time, dt
           

! +++ SPIDER I/O:
      TYPE(TYPE_PARAMETERS) :: PARAMETERS_SPIDER
       



     CALL INPUT2SPIDER2       (neql,nteta,nbnd,rzbnd,na1,eqpf,eqff,fp,ipl,           &
                               rtor,btor,rho,pres,cu,                                &
                               nstep, key_equil,                                     &
                               PARAMETERS_SPIDER)


     CALL SPIDER_RUN2         (PARAMETERS_SPIDER)     

      
     CALL SPIDER2OUTPUT2      (PARAMETERS_SPIDER,                                    &
                               neql,nteta,rtor,ztor,btor,rho,roc,na1,                &
                               g11,g22,g33,vr,vrs,slat,gradro,rocnew,                &
                               ymu,ipol,bmaxt,bmint,bdb02,b0db2,bdb0,droda,          &
      		               yreler,yli3,platok,cu_out,yfp,                        &
                               pres_s,rout,zout,shafr_shift,elong,elong_u,elong_l,   &
                               triang_l,triang_u,amid,ftrap)


     RETURN

     END SUBROUTINE INT_SPIDER
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   







! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
      SUBROUTINE INPUT2SPIDER2(neql,nteta,nbnd,rzbnd,na1,eqpf,eqff,fp,ipl,           &
                               rtor,btor,rho,pres,cu,                                &
                               nstep, key_equil,                                     &             
                               PARAMETERS_SPIDER)   
       
      USE ITM_CONSTANTS
      USE PARAMETERS

      IMPLICIT NONE
 
      
! +++ I/O:
      INTEGER                 :: nbnd, key_equil, na1, nstep
      INTEGER                 :: neql, nteta
      REAL (R8)               :: dt,time,dpsdt,yreler
      REAL (R8)               :: ipl, rtor, btor,roc
      REAL (R8)               :: rzbnd(1:2*nbnd)
      REAL (R8)               :: eqpf(na1), eqff(na1), fp(na1)
      REAL (R8)               :: rho(na1), pres(na1), cu(na1)
     
     
! +++ locals:
      INTEGER                 :: kpr,k_con,kname
      INTEGER                 :: k_fixfree,key_ini,key_start,key_0stp,key_pres,key_dmf
      INTEGER                 :: k_grid,k_auto
      REAL (R8),  ALLOCATABLE :: mu(:), cc(:),Te(:),cubc(:),cd(:)

      CHARACTER*40            :: prename
      CHARACTER*40            :: eqdfn
               
      TYPE(TYPE_PARAMETERS)   :: PARAMETERS_SPIDER
      


      ALLOCATE  ( mu(na1), cc(na1), Te(na1), cubc(na1), cd(na1) )

      mu               = 0.E0_R8 
      cc               = 0.E0_R8 
      Te               = 0.E0_R8 
      cubc             = 0.E0_R8 
      cd               = 0.E0_R8 
      
      

! +++ setup parameters for equilibrium problem to solve
      PARAMETERS_SPIDER%nstep  = nstep


      IF (nstep .NE. 0) THEN
        PARAMETERS_SPIDER%key_dmf      = key_equil
      ELSE
        IF(key_equil .eq. 0) THEN
            PARAMETERS_SPIDER%key_0stp = key_equil
        ELSE IF (key_equil .eq. -10) THEN  
            PARAMETERS_SPIDER%key_0stp = 1
        ELSE
            PARAMETERS_SPIDER%key_0stp = key_equil
        END IF
      END IF



! +++ p' input for key_equil=0 (otherwise the default key_pres is used)
      IF (key_equil .eq. 0) THEN
         PARAMETERS_SPIDER%key_pres = 0
      END IF

      kpr                        = PARAMETERS_SPIDER%kpr     !print in spider
      k_con                      = 0      
      prename                    = PARAMETERS_SPIDER%prename
      kname                      = PARAMETERS_SPIDER%kname      


      CALL  KPR_CALC              (kpr)
      CALL  PUT_KEY_CON           (k_con)
      CALL  PUT_NAME              (prename,kname)

      nstep                      = PARAMETERS_SPIDER%nstep
      time                       = PARAMETERS_SPIDER%time
      dt                         = PARAMETERS_SPIDER%dt
      key_dmf                    = PARAMETERS_SPIDER%key_dmf
      k_grid                     = PARAMETERS_SPIDER%k_grid         ! k_grid= 0   rect. grid; k_grid= 1   adap. grid
      k_auto                     = PARAMETERS_SPIDER%k_auto         ! k_auto= 1->   full initialization; k_auto= 0-> preinitialization is assumed to be done
      k_fixfree                  = PARAMETERS_SPIDER%k_fixfree      !=0->only fixed boundary spider 
      dpsdt                      = PARAMETERS_SPIDER%dpsdt
      key_ini                    = PARAMETERS_SPIDER%key_ini        ! =1 astra profiles, =0 start from EQDSK and SPIDER profiles
      key_start                  = PARAMETERS_SPIDER%key_start       
      eqdfn                      = PARAMETERS_SPIDER%eqdfn
      key_0stp                   = PARAMETERS_SPIDER%key_0stp   
      key_pres                   = PARAMETERS_SPIDER%key_pres   
!      neql                       = PARAMETERS_SPIDER%neql   
!      nteta                      = PARAMETERS_SPIDER%nteta  


!      PARAMETERS_SPIDER%nstep    = nstep
      PARAMETERS_SPIDER%neql     = neql
      PARAMETERS_SPIDER%nteta    = nteta  

      CALL  ASPID_FLAG (1)
!      CALL put_key_fix(k_fixfree)

      roc                        = rho(na1)


             
      CALL ASTRA2SPIDER           (neql,nteta,nbnd,rzbnd,key_dmf,               &     
                                   na1,eqpf,eqff,fp,ipl,                        &
                                   rtor,btor,rho,roc,nstep,yreler,mu,           &
                                   cc,Te,cubc,cd,key_ini,eqdfn,pres,cu,         &
                                   key_0stp,key_pres                    )

      


      DEALLOCATE  ( mu, cc, Te, cubc, cd )



      RETURN

      END SUBROUTINE INPUT2SPIDER2
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   



       
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
      SUBROUTINE SPIDER_RUN2(PARAMETERS_SPIDER)
       
      USE ITM_CONSTANTS
      USE PARAMETERS

      IMPLICIT NONE
 
      !locals
      INTEGER                :: nstep, key_dmf, k_grid, k_auto, k_fixfree, key_ini
      INTEGER                :: kpr, kname
      REAL (R8)              :: time, dt, dpsdt
      INTEGER                :: i,neql
      
      REAL (R8), ALLOCATABLE :: voltpf(:), phi(:)

      TYPE(TYPE_PARAMETERS)  :: PARAMETERS_SPIDER



      ALLOCATE  ( voltpf(100) )
      
      voltpf                  = 0.E0_R8

      nstep                   = PARAMETERS_SPIDER%nstep
      time                    = PARAMETERS_SPIDER%time
      dt                      = PARAMETERS_SPIDER%dt
      key_dmf                 = PARAMETERS_SPIDER%key_dmf
      k_grid                  = PARAMETERS_SPIDER%k_grid
      k_auto                  = PARAMETERS_SPIDER%k_auto
      k_fixfree               = PARAMETERS_SPIDER%k_fixfree
      dpsdt                   = PARAMETERS_SPIDER%dpsdt
      key_ini                 = PARAMETERS_SPIDER%key_ini
      kpr                     = PARAMETERS_SPIDER%kpr
      kname                   = PARAMETERS_SPIDER%kname
 


      CALL  KPR_CALC           (kpr)
      CALL  PUT_NAME           (PARAMETERS_SPIDER%prename,kname)

      CALL  PUT_TIM            (dt,time)
      CALL  SAVEPSI

      CALL  SPIDER             (nstep,time,dt,key_dmf,k_grid,k_auto,k_fixfree, &
                                dpsdt,key_ini,voltpf)  
       
!      CALL  PLA_VOLT(dt)
      CALL  CUR_AVG
      CALL  WRB
      CALL  WR_SPIK


      DEALLOCATE  ( voltpf )


      RETURN

      END SUBROUTINE SPIDER_RUN2
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

      
      
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
      SUBROUTINE SPIDER2OUTPUT2(PARAMETERS_SPIDER,                         &
                               neql,nteta,rtor,ztor,btor,rho,roc,na1,           &
                               g11,g22,g33,vr,vrs,slat,gradro,rocnew,      &
                               mu,ipol,bmaxt,bmint,bdb02,b0db2,bdb0,droda, &
      			       yreler,yli3,platok,cu_out,fp,pres_s,        &
                               rout,zout,shafr_shift,elong,elong_u,elong_l,&
                               triang_l, triang_u,amid,ftrap)

      USE ITM_CONSTANTS
      USE PARAMETERS

      IMPLICIT NONE
 
      
      INTEGER                :: na1,ni_p,nj_p
      INTEGER                :: i, j, neql, nteta, ndim2
      INTEGER                :: k1, k2

      REAL (R8)              :: rtor,ztor,btor, platok,yli3,roc,rocnew,yreler
      REAL (R8)              :: g11(na1), g22(na1), g33(na1)
      REAL (R8)              :: rho(na1), pres_s(na1), cu_out(na1),fp(na1),ftrap(na1)
      REAL (R8)              :: vr(na1),vrs(na1),slat(na1)
      REAL (R8)              :: gradro(na1),droda(na1),mu(na1)
      REAL (R8)              :: ipol(na1),bmaxt(na1),bmint(na1) 
      REAL (R8)              :: bdb02(na1),b0db2(na1),bdb0(na1)
      REAL (R8)              :: rout(neql,nteta), zout(neql,nteta)                      
      REAL (R8)              :: shafr_shift(na1),elong(na1),elong_u(na1),elong_l(na1),triang_l(na1),triang_u(na1),amid(na1)
      REAL (R8)              :: r_in, r_out, z_min, z_max, rz_min, rz_max
      REAL (R8)              :: r1,r2,r3,z1,z2,z3,a,b
      
      !locals
      REAL (R8), ALLOCATABLE :: W_Dj(:), yFOFB(:)
      
      TYPE(TYPE_PARAMETERS)   :: PARAMETERS_SPIDER


      ALLOCATE               ( W_Dj(na1), yFOFB(na1) )


      W_Dj                 = 0.E0_R8
      yFOFB                = 0.E0_R8


      ni_p                 = PARAMETERS_SPIDER%neql
      nj_p                 = PARAMETERS_SPIDER%nteta



      rout                 = 0.E0_R8
      zout                 = 0.E0_R8
      


      CALL SPIDER2ASTRA      (rout,zout,rtor,btor,rho,roc,na1,                 & 
                              g11,g22,g33,vr,vrs,slat,gradro,rocnew,           &
                              mu,ipol,bmaxt,bmint,bdb02,b0db2,bdb0,droda,      &
     		              yreler,yli3,ni_p,nj_p,platok,cu_out,fp,          &
                              pres_s,W_Dj,yFOFB)
 
      ftrap                 = yFOFB

     
      CALL OUTPUT2D          (neql,nteta,na1,rtor,ztor,rout,zout,shafr_shift,elong,elong_u,elong_l,triang_l,triang_u,amid)


      DEALLOCATE             ( W_Dj, yFOFB )



      RETURN

      END SUBROUTINE SPIDER2OUTPUT2
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
      SUBROUTINE OUTPUT2D    (neql,nteta,na1,rtor,ztor,rout,zout,shafr_shift,elong,elong_u,elong_l,triang_l,triang_u,amid)

      USE ITM_CONSTANTS

      IMPLICIT NONE
 
      
      INTEGER                :: na1, neql, nteta
      INTEGER                :: i, j, ndim2
      INTEGER                :: k1, k2

      REAL (R8)              :: rout(neql,nteta), zout(neql,nteta), rtor,ztor                      
      REAL (R8)              :: shafr_shift(na1),elong(na1),elong_u(na1),elong_l(na1),triang_l(na1),triang_u(na1),amid(na1)
      REAL (R8)              :: r_in, r_out, z_in, z_out, z_min, z_max, rz_min, rz_max
      REAL (R8)              :: r1,r2,r3,z1,z2,z3,a,b


      ndim2    =  SIZE(rout, DIM=2)

      DO i = 2, na1
        r_in   =  MINVAL(rout(i,1:ndim2)) 
        r_out  =  MAXVAL(rout(i,1:ndim2))
        z_min  =  MINVAL(zout(i,1:ndim2)) 
        z_max  =  MAXVAL(zout(i,1:ndim2))

        rz_min = 0.5D0*(r_in+r_out)
        rz_max = 0.5D0*(r_in+r_out)

        DO j = 1, ndim2
           IF (zout(i,j).EQ.z_min) THEN
              k1       = 2
              k2       = 2
              IF (ndim2-j.LT.2) k2 = 2 - ndim2
              IF (j.LT.2)       k1 = 2 - ndim2
              r1       = rout(i,j-k1)
              r2       = rout(i,j)
              r3       = rout(i,j+k2)
              z1       = zout(i,j-k1)
              z2       = zout(i,j)
              z3       = zout(i,j+k2)

              a        = ((z2-z3)/(r2-r3)-(z1-z2)/(r1-r2))/(r3-r1)
              b        = (z1-z2)/(r1-r2) - a*(r1+r2)

              rz_min   = - b/2.d0/a
               z_min   = a*(rz_min**2-r2**2) + b*(rz_min - r2)+ z2

              IF (abs(1.d0-rz_min/rout(i,j)).ge.abs(1.d0-rout(i,1)/rout(i,2))) rz_min = rout(i,j)
           END IF
           IF (zout(i,j).EQ.z_max) THEN
              k1       = 2
              k2       = 2
              IF (ndim2-j.LT.2) k2 = 2 - ndim2
              IF (j.LT.2)       k1 = 2 - ndim2
              r1       = rout(i,j-k1)
              r2       = rout(i,j)
              r3       = rout(i,j+k2)
              z1       = zout(i,j-k1)
              z2       = zout(i,j)
              z3       = zout(i,j+k2)

              a        = ((z2-z3)/(r2-r3)-(z1-z2)/(r1-r2))/(r3-r1)
              b        = (z1-z2)/(r1-r2) - a*(r1+r2)

              rz_max   = - b/2.d0/a
               z_max   = a*(rz_max**2-r2**2) + b*(rz_max - r2)+ z2
              IF (abs(1.d0-rz_max/rout(i,j)).ge.abs(1.d0-rout(i,1)/rout(i,2))) rz_max = rout(i,j)
           END IF
           IF (rout(i,j).EQ.r_out) THEN
              k1       = 2
              k2       = 2
              IF (ndim2-j.LT.2) k2 = 2 - ndim2
              IF (j.LT.2)       k1 = 2 - ndim2
              r1       = rout(i,j-k1)
              r2       = rout(i,j)
              r3       = rout(i,j+k2)
              z1       = zout(i,j-k1)
              z2       = zout(i,j)
              z3       = zout(i,j+k2)

              a        = ((r2-r3)/(z2-z3)-(r1-r2)/(z1-z2))/(z3-z1)
              b        = (r1-r2)/(z1-z2) - a*(z1+z2)

               z_out   = - b/2.d0/a
               r_out   = a*(z_out**2-z2**2) + b*(z_out-z2)+ r2
              IF (abs(1.d0-z_out/zout(i,j)).ge.abs(1.d0-zout(i,1)/zout(i,2))) z_out = zout(i,j)
           END IF
           IF (rout(i,j).EQ.r_in) THEN
              k1       = 2
              k2       = 2
              IF (ndim2-j.LT.2) k2 = 2 - ndim2
              IF (j.LT.2)       k1 = 2 - ndim2
              r1       = rout(i,j-k1)
              r2       = rout(i,j)
              r3       = rout(i,j+k2)
              z1       = zout(i,j-k1)
              z2       = zout(i,j)
              z3       = zout(i,j+k2)

              a        = ((r2-r3)/(z2-z3)-(r1-r2)/(z1-z2))/(z3-z1)
              b        = (r1-r2)/(z1-z2) - a*(z1+z2)

              z_in   = - b/2.d0/a
              r_in   = a*(z_in**2-z2**2) + b*(z_in-z2)+ r2
              IF (abs(1.d0-z_in/zout(i,j)).ge.abs(1.d0-zout(i,1)/zout(i,2))) z_in = zout(i,j)
           END IF
        END DO

        amid(i)        = 0.5D0*(r_out-r_in)
        shafr_shift(i) = 0.5D0*(r_in+r_out)-rtor
        elong(i)       = (z_max-z_min)/(r_out-r_in)
        elong_u(i)     = 2.D0*abs(z_max-ztor)/(r_out-r_in)
        elong_l(i)     = 2.D0*abs(ztor-z_min)/(r_out-r_in)
	triang_l(i)    = (0.5D0*(r_in+r_out)-rz_min)/amid(i)
	triang_u(i)    = (0.5D0*(r_in+r_out)-rz_max)/amid(i)
     END DO
     shafr_shift(1)    = rout(1,1)-rtor
     elong(1)          = elong(2)
     triang_l(1)       = 0.D0
     triang_u(1)       = 0.D0
     amid(1)           = 0.D0
     shafr_shift(na1)  = 0.D0

      RETURN

      END SUBROUTINE OUTPUT2D
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

end module mod_spider
