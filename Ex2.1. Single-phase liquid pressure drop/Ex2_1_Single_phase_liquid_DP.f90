

!****************************************************************************
!
!  PROGRAM: Ex2_1_Single_phase_liquid_DP
!
!  PURPOSE:  To Calculate the pressure change in a awater injection Well 
!  
!  By Alibig71     
!  28th Jun 2020  
!****************************************************************************

    program Ex2_1_Single_phase_liquid_DP
    Implicit None

    !************************
    !Variables
    !************************
    !Input variables 
    REAL :: L, q, Tet, RO, d, mo, E, f 
    
     
    REAL :: V  ! Average Velocity
    REAL :: Nre !  Reynolds Number
    REAL :: RR ! Relative Roughness
    REAL :: DPDL_a, DPDL_ELE, DPDL_f, DPDL_ele_psi, DPDL_f_psi, DPDL_TOTAL ! Pressure gradient 
    REAL :: PC ! Pressure change 
    real :: phi, a, dig_to_Rad 
    !*************************************
    ! Body of Ex2_1_Single_phase_liquid_DP
    !************************************
    
    L = 8000.0 !ft
    q = 20000 !bbl/d
    Tet = -90.0
    RO = 62.4 !lbm/ft3
    d = 5.0 !in
    mo = 1.0 !cp 
    E = 0.00006 !ft
    f= 0.0155 !from fig 2.2 for the given problem
    phi = 3.1415926
    !*************
    ! Eqs 
    !*************
    
    v = ((q)*(5.615))/((phi/4)*(((d/12)**2)*(86400))) !ft/sc 
    
    !rvd/mo    & convert cp to lb/ft s 1488
    
    Nre= ((Ro)*(v)*(d/12))/((mo/1488))
    RR = (e)/(d/12)
    
    
    DPDL_a = 0.0 ! because it's single phase water 
    
    ! Friction = f*ro*v2/2dg
    DPDL_f = (-(f)*ro*((v)**2))/((2*(d/12)*32.2))
    
    ! ele = ro g sin(tetha)/g
    
    Dig_to_rad = phi/180
    
    
    
    !A = sin(tet*Dig_to_rad)
    !print*,"sin ch", a
    !pause 
    
    
    
    DPDL_ele = (-(ro)*(32.2)*(sin(tet*Dig_to_rad)))/(32.2)
    
    ! convert pound square foot to pound square inch
    
    DPDL_ELE_psi = (DPDL_ELE)/144.0
    DPDL_f_psi = (DPDL_f)/144.0
    
    
    DPDL_TOTAL = (DPDL_ELE_psi + DPDL_f_psi)*(L)
    
    
    !print*, v, NRE, RR, DPDL_F_psi, DPDL_ele_psi, DPDL_total
    !pause 
    
    
    print *, "====================================================="
    print *, "                    final reusl                      "
    print *, "-----------------------------------------------------"
    print *, 'v                    ', v,             "ft/s"
    print *, 'Nre                  ', Nre
    print *, 'RR                   ', RR 
    print *, 'DPDL_f               ', DPDL_f_psi,    "psi/f"
    print *, 'DPDL_ELE             ', DPDL_ELE_psi,  "psi/ft"
    print *, 'Total Pressure change', DPDL_TOTAL,    "psi"
    Print *, "====================================================="
    pause
    end program Ex2_1_Single_phase_liquid_DP

