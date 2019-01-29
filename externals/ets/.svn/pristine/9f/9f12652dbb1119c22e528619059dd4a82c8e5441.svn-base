!*********************************************
! Copyright: Edmondo Giovannozzi (2009)
!            ENEA
!  This software is released under the ITM License
!*********************************************
subroutine printcpoargs(shot, run, treename, pscreen, xmloutput)
implicit none
integer :: shot, run
character(len=*) :: treename
logical :: pscreen
logical :: xmloutput

character(len=80) :: arg
integer :: n
integer :: iargc
external iargc

integer :: i, k

shot = 1
run = 1
treename = 'euitm'
pscreen = .false.
xmloutput = .false.

n = iargc()
if (n == 0) then
    call printhelp
    call exit(1)
end if
k = 0
do i=1, n
    call getarg(i, arg)
    if (arg(1:1) == '-') then
        if (arg(2:2) == 'p') then
            pscreen = .true.
        elseif (arg(2:2) == 'x') then
            xmloutput = .true.
        elseif (arg(2:2) == 'h') then
            call printhelp
            call exit(0)
        else
            print *,'Unrecognized options: ',trim(arg)
            call printhelp
            call exit(1)
        endif
    else
        k = k + 1
        if (k==1) then
            read(arg,*) shot
        elseif (k==2) then
            read(arg,*) run
        elseif (k==3) then
            treename  = arg
        else
             print *,'Too many parameters:', trim(arg)
             call printhelp
             call exit(1)
        endif
    endif
enddo
contains
 subroutine printhelp
    print *,'PRINTCPO print the fields different from zero of CPOs stored in a MDSPLUS file'
    print *
    print *,' Usage: '
    print *,''
    print *,' printcpo [options] [shot [run [treename]]]'
    print *
    print *,' Parameters:'
    print *,'    shot:      the shot number. Defaults to 1'
    print *,'    run:       the run number. Defaults to 1'
    print *,'    treename:  the name of the tree. Defaults to "euitm"'
    print *,''
    print *,' Options:'
    print *,'    -p         print some data for vector fields (not yet implemented)'
    print *,'    -x         the output is in XML format'
    print *,'    -h         print this help'
    print *
    print *,'Copyright: Edmondo Giovannozzi (2009)'
    print *,'           ENEA'
    print *,'This software is released under the ITM License'
 end subroutine printhelp
end subroutine printcpoargs
