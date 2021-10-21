INCLUDE "toolbox.f90" 
!INCLUDE "link_fnl_static.h"       ! or 'link_f90_static.h'
!DEC$ OBJCOMMENT LIB:'libiomp5md.lib'

MODULE globals 

    USE toolbox 
    !USE NEQNF_INT					  ! nonlinear eq solver: Powell hybrid algorithm
    !USE ERSET_INT					  ! skip error messages
    !USE IERCD_INT					  ! error code
    !USE CPSEC_INT					  ! CPU time
    !USE GAMDF_INT					  ! gamma cumulative distribution function
    !USE LINEAR_OPERATORS

    IMPLICIT NONE  

    ! single precision: log10 2**(24), which is about 7~8 decimal digits
    ! double precision: log10 2**(53), which is about 15~16 decimal digits
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!! MODEL PARAMETERS !!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Time
    INTEGER, PARAMETER :: TT = 30
    INTEGER, PARAMETER :: PYEAR = 18              ! Choose year of FTB parameters

    ! Model period 
    INTEGER, PARAMETER :: mperiod = 5             ! set #year for model periodd

    ! Age (model age begins at 20) 
    INTEGER, PARAMETER :: JJ   = 80/mperiod,    & ! maximum age 
                          JR   = 45/mperiod,    & ! retirement age 
                          JP   = 45/mperiod       ! preservation age 
                          
    ! Demographic
    REAL*8, PARAMETER :: n_p = (1d0 + 0.0156d0)**mperiod - 1d0  

    ! Scale parameter 
    REAL*8, PARAMETER :: scale = 972.6459d0*52 ! scale parameter = the avg annual earnings of male cohort aged 20-24
                                               ! to be used in inctaxfunc and ftb functions 
                                               !  972.6459d0
    ! HOUSEHOLD PARAMETERS
    ! Preferenrce 
    REAL*8, PARAMETER :: beta  = 0.97d0**mperiod, & ! time discount factor 
                         gamma = 0.5d0,           & ! intertemporal elasticity of substitution 
                         egam  = 1d0-1d0/gamma,     & 
                         chi   = 0.05d0,          & ! fixed utility cost of female labour supply 
                         nu    = 0.335d0            ! taste for consumption 

    INTEGER, PARAMETER :: L_max = 1               ! maximum nodes for the discretized female labour choice 
    
    ! Assets 
    INTEGER, PARAMETER :: NA = 25                   ! 80
    REAL*8, PARAMETER  :: a_l = 0d0,              & ! lower bound for asset holdings 
                          a_u = 30d0,             & ! upper bound                     !! CHANGE !! 14/09/2021
                          a_grow = 0.1d0           ! grow rate of asset nodes 
                         ! GIVEN THE SMALL GRID NUMBER, SHOULD INCREASE A_GROW RATE HIGHER TO GET A SMALLER STEP SIZE 
                         ! e.g., for a(0:NA) with a_u = 300d0 and a_grow = 0.2d0 --> stepsize = 300/((1+0.2)**(26)-1) = 2.64
                         !       for a(0:NA) with a_u = 300d0 and a_grow = 0.1d0 --> stepsize = 300/((1+0.1)**(26)-1) = 27.48 (too big)

    ! Human capital (female) 
    INTEGER, PARAMETER :: NH = 25                   ! 60
    REAL*8             :: h_l,                  & ! lower bound of human capital of female (to be determined)
                          h_u,                  & ! upper bound (to be determined)
                          h_grow = 0.05d0,        & ! grow rate of human capital nodes
                          del_h  = 1d0 - (1d0-0.074d0)**mperiod ! human capital depreciation rate 

    ! xi1 and xi2 from age-profile of wage earnigns for female in Australia .0723917  -.0008425
    ! US: REAL*8, PARAMETER  :: xi(2) = (/0.05312d0, -0.00188d0/)*sqrt(dble(mperiod))
    ! AUSTRALIA 1-PERIOD: REAL*8, PARAMETER  :: xi(2) = (/0.0723917, -0.0008425/)
    
    ! AUSTRALIA 5-PERIOD 
    REAL*8, PARAMETER  :: xi(2) = (/0.1906394, -0.0183417/)
    
    ! Exogenous work hours (where age-profiles of work hours for male and female are obtained from HILDA)
    REAL*8, DIMENSION(JJ) :: lab1, lab2 
    
    ! STOCHASTIC PROCESS 
    ! Persistent shocks (education/skill type) 
    INTEGER, PARAMETER :: NP = 2 
    REAL*8, PARAMETER  :: sig2_theta = 0.23d0   ! variance of persistence shocks ! 0.242d0

    ! Transitory shocks (uninsurable idiosyncratic shocks)
    INTEGER, PARAMETER :: NS = 3
    REAL*8, PARAMETER  :: rho = 0.98d0,           & ! Persistence/autocorrelation parameter 
                          sig2_eps = 0.05d0         ! variance of transitory shocks (0.03d0)
    INTEGER            :: is_initial  = 2           ! eta(3) = 0
    
    ! Combined age and shock nodes 
    
    !INTEGER, PARAMETER :: JPSMAX = (NS*NS)*NP*(JR-1) + (JJ-JR+1),     & ! working-age + retirees 
    !                       JPSMAX1 = (NS*NS)*NP*(JR-1)                   ! working-age population only
    
    INTEGER, PARAMETER :: JPSMAX = (NS*NS)*NP*(JJ),     & ! working-age + retirees 
                          JPSMAX1 = (NS*NS)*NP*(JR-1)                   ! working-age population only
    ! FIRM PARAMETERS 
    REAL*8, PARAMETER :: alpha = 0.36d0,          & ! capital share of output t (0.36d0 US, 0.4d0 Aus)
                         TFP   = 0.8959d0,            & ! TFP (1.6d0 US) 
                         ! calibrate: 1 = A*(1-alpha)*(K/L)**alpha = A*(1-alpha)**(1-alpha)*(K/Y)**alpha
                         !            1 =  A*(1-alpha)**(1-alpha)*(3)**alpha ==> A = 0.8959d0 if alpha = 0.36
                         delta = 1d0 - (1d0-0.0823d0)**mperiod ! or change to 0.055d0 for australia (0.0823 US)

    ! Numerical parameters 
    INTEGER, PARAMETER :: itermax = 200 
    REAL*8, PARAMETER  :: outer_tol = 10d0,     & ! tol for outer optitmization routines 
                          inner_tol = 1d-8        ! global tol for inner optimization routines 
                                                  ! (set via settol_min(inner_tol))

    ! Damp parameters 
    REAL*8  :: damp_household,  & 
               damp_aggregation, & 
               damp_LSRA 

    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!! MODEL VARIABLES !!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! Prices 
    REAL*8, DIMENSION(0:TT) :: wm, wf, wn_m, wn_f, r, rn, p 
    
    ! Age-specific labour productivity profile (male) 
    REAL*8, DIMENSION(JJ)   :: eff 
    
    ! Survival probability 
    REAL*8, DIMENSION(JJ+1) :: psi
    
    ! Permanent shocks 
    REAL*8, DIMENSION(NP)   :: dist_theta,      & ! distribution of perm prod. shocks 
                               theta              ! values of shocks 
    
    ! Transitory shocks 
    REAL*8                  :: pi(NS, NS),      & ! transition matrix 
                               eta(NS),         & ! shock nodes 
                               prob(NS)           ! unconditional prob distribution of eta    
    
    ! Children (assume all resident children are dependent)
    INTEGER, DIMENSION(JJ)  :: nchild,          & ! number of dependent children 
                               firstchild         ! 1. first child; 0. not first child
                               
    INTEGER, DIMENSION(5,0:JJ) :: childage        ! age of dependent children (5 children max)

    REAL*8, DIMENSION(JJ)   :: cchild             ! cost of childcare 
    
    
    ! Asset and Human capital nodes
    REAL*8, DIMENSION(JJ)   :: a_max,           & ! max possible assets 
                               h_max              ! max possible human capital
                        
    REAL*8                  :: a(0:NA),         & ! current asset holdings 
                               h(0:NH)            ! current human capital 
    
    ! Decisions and value 
    REAL*8, DIMENSION(:, :, :, :), ALLOCATABLE :: &
                               aplus,           & ! next period assets (savings) 
                               c,               & ! consumption 
                               l,               & ! labour 
                               V                  ! Opitimal utility
    
    
    ! Dynamic arrays to resolve memory problem associated with large matrices of aplus, c, l, and V 
    !REAL*8, DIMENSION(:, :, :, :, :, :, :, :), ALLOCATABLE :: & 
    !                           temp_var           ! write and read c, l, aplus, and V 
    
    
    REAL*8, DIMENSION(0:NA, 0:NH, NP, NS, NS, 0:L_max) :: &
                               EV,              & ! Expected value
                               RHS                ! RHS of the FOC of household problem
    
    ! Distribution 
    REAL*8, DIMENSION(:, :, :, :), ALLOCATABLE :: phi
    
    
    ! Population size and relative size 
    REAL*8                  :: pop(0:TT),    & ! population size 
                               m_lev(JJ, 0:TT),  & ! mass of agents still alive 
                               m(JJ, 0:TT),      & ! share of alive agents 
                               workpop(0:TT),    & ! working-age population 
                               parent_pop(0:TT), & ! parent population 
                               bor_cons(JJ, 0:TT)  ! fraction of borrowing constrained households 
    
    ! Cohort aggregates 
    REAL*8, DIMENSION(JJ, 0:TT) :: c_coh,       & ! average consumption by cohort 
                                   l_coh,       & ! average labour productivity by cohort
                                   a_coh,       & ! average assets by cohort
                                   h_coh,       & ! average female human capital by cohort
                                   v_coh=0d0,   & ! average value by cohort 
                                   comp_coh=0d0, & ! average lsra composition 
                                   ym_coh,      & ! average real male labour income by cohort
                                   yf_coh,      & ! average real female labour income by cohort
                                   nymtau_coh,    & ! average nominal male labour income by cohort (**(1-tau))
                                   nyftau_coh,    & ! average nominal female labour income by cohort (**(1-tau))
                                   y_coh,       & ! average household income by cohort 
                                   beq_share,   & ! share of survivor
                                   bequest,     & ! accidental bequest received per household age ij in time it 
                                   beq_coh,     & ! total accidental bequest endowed from cohort ij in time it 
                                   inctax_coh,  & ! total income tax from cohort ij in time it 
                                   FTBA_coh,    & ! FTB_A amount by cohort 
                                   FTBB_coh,    & ! FTB_B amount by cohort 
                                   CCS_coh        ! Child care subsidy by cohort 
    
    ! Bequest distribution 
    REAL*8, DIMENSION(JJ) :: beq_dist
    
    ! First and second moments of shocks 
    REAL*8, DIMENSION(JJ, 0:TT) :: cv_c, cv_y     ! coefficients of variation for consumption and household income 
                              
    !REAL*8, DIMENSION(2:JJ, 0:TT) :: exp_shock_m, exp_shock_f, &
    !                                 var_shock_m, var_shock_f  ! first and second moments of income shocks

    
    ! GOVERNMENT VARIABLES 
    ! Tax and transfer system 
    REAL*8, DIMENSION(0:TT) :: tauc,            & ! consumption tax 
                               tauw,            & ! labour earnings tax 
                               taur,            & ! capital earnings tax 
                               Tr                 ! lumpsum transfer 
    REAL*8                  :: gy,              & ! gov't consumption to gdp ratio 
                               by,              & ! gov't borrowing to gdp ratio 
                               taxrev(5,0:TT)     ! tax revenue by tax type 
    INTEGER                 :: tax(0:TT)          ! endogenous tax setter 

    LOGICAL                 :: progressive        ! tax switch between progressive and flat tax systems

    ! Parameteric tax function parameters 
    REAL*8, DIMENSION(0:TT) :: tauy,            & ! progressivity parameter of tax function 
                               lambda_tauy        ! scaling parameter of tax function 
    
    ! Social security system 
    REAL*8, DIMENSION(0:TT) :: taup,            & ! payroll tax 
                               kappa              ! replacement rate
    REAL*8                  :: pen(JJ, 0:TT),   & ! pension amount to each cohort 
                               PP(0:TT)           ! Total SS expenditure 
    
    ! LumpSum Redistributive Authority 
    REAL*8, DIMENSION(:, :, :, :), ALLOCATABLE :: & 
                               comp               ! LSRA compensation 
    
    REAL*8                  :: HEV(2-JJ:TT, 2)
    REAL*8                  :: fully_compensated, compensated, Lambda  
    REAL*8                  :: Bold(0:TT)
    LOGICAL                 :: lsra_on 
    
                
    
    ! MACROECONOMIC VARIABLES (AGGREGATION)
    LOGICAL                 :: open_capmarket     ! 0: closed economy; 1: small open capital market 
    
    REAL*8, DIMENSION(0:TT) :: KK,              & ! Capital
                               AA,              & ! Agg. Savings
                               LL,              & ! Labour (in efficiency unit)
                               LLM,             & ! Male Labour (in efficiency unit)
                               LLF,             & ! Female Labour (in efficiency unit)
                               LFP,             & ! Female LFP rate 
                               HH,              & ! Female Human Capital 
                               CC,              & ! Agg. consumption
                               II,              & ! Investment
                               GG,              & ! Gov't Consumption
                               BB,              & ! Gov't Borrowing (i.e., net wealth < 0) 
                               BF,              & ! Total foreign capital borrowed 
                               ITB,             & ! International capital trade balance
                               YY,              & ! Agg. Output
                               INC,             & ! Average National Income
                               BQ,              & ! Accidental bequest 
                               BA=0d0,          & ! LSRA's borrowing
                               SV=0d0,          & ! Total LSRA transfer 
                               INCTAX,          & ! Total income tax 
                               NY,              & ! Average National Income in nominal terms 
                               NYtau,           & ! Agg of nytau_coh (used in computing endogenous scaling parameter of tax function)
                               FTBA,            & ! Total FTBA expenditure in time it 
                               FTBB,            & ! Total FTBB expenditure in time it 
                               CCS                ! Total cost of child care subsidy in time it 

    ! NUMERICAL VARIABLES 
    INTEGER                 :: iter
    REAL*8                  :: DIFF(0:TT)
    
    ! Communication variables with external functions
    INTEGER                 :: ij_com, ia_com, ih_com, ip_com, ism_com, isf_com, il_com, it_com, ijps_com
    REAL*8                  :: cons_com



    ! NEW !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! FTB parameters (2018) !!!!!!!!!!!!!!!!!!!!!

    ! Switch between FTB and lumpsum transfer schemes 
    logical                 :: ftb_on 

    !////////////! 
    ! FTB Part A ! 
    !////////////! 

    ! BASE AND MAXIMUM PAYMENT RATES (INLUDING ANNUAL FTB_A SUPPLEMENT. ADJUSTMENT IS MADE AT THE END OF THE CALCULATION IF
    !                                 NOT ELIGIBLE.)
    REAL*8, DIMENSION(18, 7) :: FTBA_Max  ! Maximum Payment Rate FTB_A by dependent child's age and school attendance 
    REAL*8, DIMENSION(18, 4) :: FTBA_Base ! Base Payment Rate FTB_A by dependent child's age and school attendance 

    ! FAMILY INCOME TEST THRESHOLDS
    REAL*8, DIMENSION(18, 2) :: FTBA_T    ! Threshold for max and base rates 
    REAL*8, DIMENSION(18)    :: FTBA_T2A  ! Addition to base threshold FTBA_T(2) for each additional qualifying child
    !REAL*8, DIMENSION(18)    :: FTBA_T2A2 ! Final threshold value for base payment (after having accounted for FTBA_TA) 

    ! BENEFIT WITHDRAWAL/TAPER RATE 
    REAL*8, DIMENSION(18, 2) :: FTBA_W    ! Withdrawal/Taper rate from: (i) max to base rate, and (ii) base to 0 

    ! LARGE FAMILY SUPPLEMENTS (LFS) (INLUDING ANNUAL FTB_A SUPPLEMENT. ADJUSTMENT IS MADE AT THE END OF THE CALCULATION IF
    !                                 NOT ELIGIBLE.)
    REAL*8, DIMENSION(18)    :: FTBA_S1   ! Supplement per qualifying child 
    INTEGER, DIMENSION(18)   :: FTBA_C1  ! Number of children to be had for a family is deemed qualified for LFS


    ! RENT ASSISTANCE (NOT APPLICABLE IN THIS MODEL)
    REAL*8, DIMENSION(18) :: FTBA_TH1A  ! Minimum rent to qualify for rent assistance if lone parent 
    REAL*8, DIMENSION(18) :: FTBA_TH1B  ! Minimum rent to qualify for rent assistance if partnered parent
    REAL*8, DIMENSION(18) :: FTBA_TH2A  ! Maximum rent assistance if dependent children <= 2
    REAL*8, DIMENSION(18) :: FTBA_TH2B  ! Maximum rent assistance if dependent children >= 3 
    REAL*8, DIMENSION(18) :: FTBA_RPCPI ! Rent price CPI
    REAL*8, DIMENSION(18) :: FTBA_RM    ! Rent multiplier 

    ! NEWBORN SUPPLEMENT (NBS)
    ! NBS may be included as an add-on component for an individual's maximum rate (1.1.M.26) and base rate (1.1.B.10) of FTB Part 
    ! A rate for a period of up to 13 weeks (91 days) for a newborn child or a child aged under one who becomes entrusted to the 
    ! individual's care or a child who becomes entrusted to the individual's care as part an adoption process. See 3.11 for NBS
    ! rates. NBS is indexed annually.
    REAL*8, DIMENSION(18, 2)  :: FTBA_NS    ! Newborn supplement rate for: (i) 1st child, and (ii) 2nd child onwards 


    ! CLEAN ENERGY SUPPLEMENTS 
    ! The ES is included in the standard amounts of FTB Part A and Part B for each FTB child and is calculated depending on 
    ! child age (3.1.1.25). The ES is not indexed. Note: The ES ceased on 20 March 2017 for new FTB recipients.
    ! For individuals who became new recipients of FTB after 20 September 2016, they will no longer be paid ES after 19 March 2017.
    ! However, eligible individuals and ACOs may continue to be paid ES as part of their FTB rate under grandfathering rules. 
    REAL*8, DIMENSION(18, 3)  :: FTBA_CE    ! Clean energy supplement: (1) base rate per child; 
                                            !                          (2) max rate per child aged [0,12]; 
                                            !                          (3) max rate per child aged ([13,15]) or ([16,19] if at school) 

    ! MULTIPLE BIRTH ALLOWANCE 
    ! MBA may be included as an add-on component for an individual's maximum rate of FTB Part A where the individual has 3 or 
    ! more FTB children from the same birth. 
    REAL*8, DIMENSION(18)    :: FTBA_MBA1 ! Allowance for triplet  
    REAL*8, DIMENSION(18)    :: FTBA_MBA2 ! Allowance for quadruplet or more 
    INTEGER, DIMENSION(18)    :: FTBA_MAGE ! Max children age to qualify 
    INTEGER, DIMENSION(18)    :: FTBA_MAGES ! Max children age to qualify if children at school


    ! ANNUAL FTBA SUPPLEMENT ADJUSTMENT 
    ! NOTE: As in HILDA tax benefot model, for the FTB_A supplement which is included in the maximum and base payment rates of the 
    !       FTB A, a family is not eligible if the family adjusted taxable income (ftaxy > FTB_FT1) and therefore an adjustment 
    !       needs to be made.
    REAL*8, DIMENSION(18)     :: FTBA_AS     ! Per child adjustment 
    REAL*8, DIMENSION(18)     :: FTBA_FT1    ! Family income threshold for adjustment 

    
    !////////////! 
    ! FTB Part B ! 
    !////////////! 

    ! MAX PAYMENT RATE (INLUDING ANNUAL FTB_B SUPPLEMENT WHICH IS AVAILABLE IF FAMILY IS ELIGIBLE FOR FTB B; 
    !                   i.e., no income test for supplement)
    ! NOTE: The FTB Part B supplement (2021: $383.25), as well as other FTB top-up payments, may be used to offset an individual's 
    ! outstanding FTB and/or CCS reconciliation debts and may also be used by the ATO to offset a tax debt. 
    REAL*8, DIMENSION(18, 2) :: FTBB_Max    ! Maximum Payment Rate FTB_B by dependent child's age and school attendance 
                                        ! 1. FTBB_Max(1) := Max rate if youngest child aged under 5 
                                        ! 2. FTBB_MAx(2) := Max rate if youngest child aged [5,18] and still at school if age [16,18]
    ! INCOME TEST THRESHOLD 
    REAL*8, DIMENSION(18, 2) :: FTBB_T      ! Threshold for: (1). Primary earner, and (2) Secondary earner 

    ! TAPER/WITHDRAW RATE OF FTB_B 
    REAL*8, DIMENSION(18)    :: FTBB_W      ! Withdrawal/Taper rate on 'SECONDARY EARNER'

    ! CLEAN ENERGY SUUPPLEMENT PART B 
    REAL*8, DIMENSION(18, 2) :: FTBB_CE     !  Clean energy supplement: (1) if youngest child aged [0,4]
                                        !                           (2) if youngest child aged [5,18]


    !////////////////////! 
    ! Child Care Subsidy ! 
    !////////////////////! 
    logical                  :: ccs_on      ! switch between subsidy and no subsidy 

    REAL*8, DIMENSION(18, 5) :: CCS_T       ! Family income test thresholds to determine the rate of subsidy 
    REAL*8, DIMENSION(18, 4) :: CCS_R       ! Subsidy rates (0.85, 0.50, 0.20, 0.00)
    

    ! GRID_GROW INTERFACE 
    INTERFACE grid_grow
        
        ! grid_grow is separated into 
        ! - grid_grow_r4 :: for single precision input/output 
        ! - grid_grow_r8 :: for double precision input/output
        module procedure grid_grow_r4, grid_grow_r8
    
    END INTERFACE 
    
    
    ! ROUWENHORST INTERFACE 
    INTERFACE rouwenhorst_method 
    
        ! rouwenhorst_method is separated into 
        ! - rouwenhorst_method_r4 :: for single precision input/output 
        ! - rouwenhorst_method_r8 :: for double precision input/output
        module procedure rouwenhorst_method_r4, rouwenhorst_method_r8
    
    END INTERFACE 
    
   ! Utlity function 
   INTERFACE utility 
        ! Utility function is set up to take two possible types of labour value
        ! - utility_int: for integer value 
        ! - utility_real: for real value 
        module procedure utility_int, utility_real
    
   END INTERFACE 
   
contains 


    
    ! Bellman equation 
    FUNCTION bellman_equation(a_in) 
    
        implicit none 
        real*8, intent(in) :: a_in 
        real*8 :: bellman_equation
        ! other variables 
        real*8 :: a_plus, linc_m, linc_f, tot_income, income_tax, payroll_tax, ftba, ftbb, transfer, total
        real*8 :: sr, cons_lim
        
        ! set aplus 
        a_plus = a_in 
        
        ! labour income
        !linc_m = wn_m(it_com) * lab1(ij_com) * eff(ij_com) * theta(ip_com) * eta(ism_com) 
        !linc_f = wn_f(it_com) * lab2(ij_com) * exp(h(ih_com)) * theta(ip_com) * eta(isf_com) 
        linc_m = wm(it_com) * lab1(ij_com) * eff(ij_com) * theta(ip_com) * eta(ism_com) 
        linc_f = wf(it_com) * lab2(ij_com) * exp(h(ih_com)) * theta(ip_com) * eta(isf_com) 
        tot_income = linc_m + linc_f !+ r(it_com)*a(ia_com) 

        ! calculate income tax and payroll tax 
        if (progressive) then 
            income_tax = inctaxfunc(a(ia_com), linc_m, linc_f, it_com) 
        else 
            income_tax = tauw(it_com)*(linc_m + linc_f)
        endif 

        payroll_tax = ptaxfunc(linc_m, linc_f, it_com)


        ! calculate transfer 
        if (ftb_on) then 
            ! calculate family tax benefits (FTB A and FTB B) 
            ! FUNCTION FTB_A(ij, fam_income, annual_rent, at_school, get_base_pay, interim)
            ! FTB_B(ij, p1_income, p2_income, married, at_school)  
            ftba = FTB_A(ij_com, tot_income)
            ftbb = FTB_B(ij_com, linc_m, linc_f)
            transfer = ftba + ftbb 
        else 
            transfer = Tr(it_com) * nchild(ij_com) 
        endif

        ! calculate child care subsidy effective rate (sr)
        sr = 0d0 
        if (ccs_on) sr = CCS_rate(ij_com, tot_income, lab1(ij_com), lab2(ij_com))
            
        
        ! total income 
        ! **Uncomment below for the fortran with IMSL library 
        !total =     (1d0+rn(it_com))*a(ia_com) &                                                   ! capital income
        !            + linc_m + abs(real(il_com,kind=8) > 0)*(linc_f - wf(it_com)*lab2(ij_com)*cchild(ij_com)) & ! labour income 
        !            + pen(ij_com, it_com) &                                                        ! old-age pension
        !            + abs(real(nchild(ij_com),kind=8) > 0d0)*Tr(it_com)*nchild(ij_com) &                          ! childcare transfer 
        !            + bequest(ij_com, it_com) &                                                        ! accidental bequest 
        !            + comp(ia_com, ih_com, ijps_com, it_com)                                       ! LSRA compensation
        
  
        ! **Uncomment below for the basic version of fortran without IMSL library 
        total =       (1d0+rn(it_com))*a(ia_com) &                                         ! capital income
                    + linc_m + real(il_com, kind=8)*(linc_f - (1d0-sr)*wf(it_com)*lab2(ij_com)*cchild(ij_com)) & ! labour income 
                    + pen(ij_com, it_com) &                                                ! old-age pension
                    + bequest(ij_com, it_com) &                                            ! accidental bequest 
                    + comp(ia_com, ih_com, ijps_com, it_com) &                             ! LSRA compensation 
                    - income_tax - payroll_tax 

        !if (nchild(ij_com) > 0) total = total + Tr(it_com)*nchild(ij_com)   ! childcare transfer 
        if (nchild(ij_com) > 0) total = total + transfer  
        
        ! bequest? if (beq_no = 0) a_in = a_in/psi(ij+1)
        cons_com = (total - a_plus)/p(it_com)
        
        ! Uncomment below for the basic version of fortran without IMSL library
        cons_lim = max(cons_com, 1d-10)
        
        ! get the negative of the current period value function 
        bellman_equation = -(valuefunc(a_plus, cons_com, il_com, ij_com, ih_com, ip_com, ism_com, isf_com)) 
        
        ! **Uncomment below for the fortran with IMSL library 
        ! penalize if c < c_lim 
        ! bellman_equation = bellman_equation + 1d6*abs(cons_com < 1d-10) 
        
        ! Uncomment below for the standard version of fortran compiler without IMSL library
        bellman_equation = bellman_equation + 1d6*abs(cons_com - cons_lim)
        
    END FUNCTION bellman_equation
    
    
    ! Value function 
    FUNCTION valuefunc(aplus, cons, il, ij, ih, ip, ism, isf) 
    
        implicit none 
        real*8, intent(in) :: aplus, cons 
        integer, intent(in) :: il, ij, ih, ip, ism, isf
        real*8 :: valuefunc
        ! other variables 
        real*8 :: chelp, vplus 
        ! interpolation parameters 
        integer :: iap, ial, iar 
        real*8 :: w0
        
        ! ensure that consumption is not negative 
        chelp = max(cons, 0d0+1d-10) 
        
        ! interpolate for future value (note: it can be shown that Vplus is linear in aplus) 
        vplus = 0 
        if (ij < JJ) then
            do iap = 0, NA-2; if (aplus < a(iap+1)) exit; enddo 
            ial = iap; iar = iap+1;
            w0 = (aplus - a(ial))/(a(iar)-a(ial))
            
            vplus = (1d0-w0)*EV(ial, ih, ip, ism, isf, il) + w0*EV(iar, ih, ip, ism, isf, il)
            vplus = max(vplus, 1d-10)**egam/egam 
        endif 
        ! else, vplus = 0 for ij==JJ
            
        ! add today's part of utility 
        valuefunc = utility(chelp, il, ij, form=1) + beta*psi(ij+1)*vplus
    
    END FUNCTION valuefunc
        
        
        
    ! subroutine for interpolation 
    SUBROUTINE interpolate(ij, it) 
        
        implicit none 
        integer, intent(in) :: ij, it 
        integer :: ia, ih, ip, ism, isf, ism_p, isf_p, il, ijps_p ! counters
        real*8  :: hplus, cplus, Vplus, linc_m, linc_f, exp_r 
        ! interpolation variables
        real*8 :: w1
        integer :: ihp, ihl, ihr
        
        
        do ia = 0, NA
            ! NOTE: the condition is not ij>=JR b/c at ij=JR, the subroutine outputs 
            !       EV and RHS of agents age JR-1. Thus, their human capital and shock nodes 
            !       are still the entire grid nodes. 
            do ih = 0, NH; if (ij>JR .and. ih>0) exit
                do ip = 1, NP; if (ij>JR .and. ip>1) exit 
                    do ism = 1, NS; if (ij>JR .and. ism>1) exit 
                        do isf = 1, NS; if (ij>JR .and. isf>1) exit 
                            do il = 0, L_max; if (ij>JR .and. il>0) exit 
                            
  
                            RHS(ia, ih, ip, ism, isf, il) = 0
                            EV(ia, ih, ip, ism, isf, il)  = 0 

                            
                            hplus = h(ih) + (xi(1) + xi(2)*dble(ij-1))*(real(il,kind=8)/L_max) - del_h*(1d0-real(il,kind=8)/L_max)
                            hplus = max(hplus, h_l) 
                            
                            ! interpolate 
                            do ihp = 0, NH-2; if (hplus < h(ihp+1)) exit; enddo
                            ihl = ihp; ihr = ihp+1; 
                            w1 = (hplus - h(ihl)) / (h(ihr)-h(ihl))

                            
                            do ism_p = 1, NS 
                                do isf_p = 1, NS 

                                    ! male and female labour earnings 
                                    linc_m = wm(it) * lab1(ij) * eff(ij) * theta(ip) * eta(ism_p) 
                                    linc_f = wf(it) * lab2(ij) * exp(h(ih)) * theta(ip) * eta(isf_p) 

                                    ! expected net return on savings
                                    exp_r = rn(it)

                                    ! uncomment if progressive income tax also involves test on interest earnings 
                                    ! (note: when add stock option, must consider franking credit)
                                    if (progressive) exp_r = rn(it) - inctaxfunc(a(ia),linc_m,linc_f,it,mtr=3)
                                      
                                    ! we solve HH problem backward --> ij and it are in fact the next-period age and time 
                                    ijps_p = indexing(ij, ip, ism_p, isf_p) ! indexation
                                          
                                    ! interpolate RHS of the FOC of household
                                    cplus = (1d0-w1)*c(ia, ihl, ijps_p, it) & 
                                                    + w1*c(ia, ihr, ijps_p, it)
                                    cplus = max(cplus, 1d-10) 
                                    
                                    RHS(ia, ih, ip, ism, isf, il) = RHS(ia, ih, ip, ism, isf, il) + & 
                                                pi(ism, ism_p) * pi(isf, isf_p) * &
                                                (1d0+exp_r) * margu(cplus, real(il, kind=8), ij, it, form1=1) 
                                    
                                    ! interpolate EV next period 
                                    Vplus = (1d0-w1)*(egam*V(ia, ihl, ijps_p, it))**(1d0/egam) &
                                                    + w1*(egam*V(ia, ihr, ijps_p, it))**(1d0/egam) 
                                    Vplus = max(Vplus, 1d-10)**egam/egam
                                    
                                    EV(ia, ih, ip, ism, isf, il)  = EV(ia, ih, ip, ism, isf, il) + & 
                                                pi(ism, ism_p) * pi(isf, isf_p) * Vplus 
                                    
                                enddo ! isf_p
                            enddo ! ism_p 
                            
                            RHS(ia, ih, ip, ism, isf, il) = max( beta*psi(ij)* & 
                                                RHS(ia, ih, ip, ism, isf, il), 1d-10)**(-gamma)
                            
                            EV(ia, ih, ip, ism, isf, il) = (egam*EV(ia, ih, ip, ism, isf, il))**(1d0/egam)
                
                            enddo ! il
                        enddo ! isf
                    enddo ! ism
                enddo ! ip
            enddo ! ih
        enddo ! ia         
            
        
    END SUBROUTINE interpolate 
                

    ! euler residuals 
    FUNCTION euler_res(a_err, aplus) 
    
        implicit none 
        REAL*8, intent(in) :: a_err, aplus
        REAL*8 :: euler_res
        REAL*8 :: cons, linc_m, linc_f, total, tomorrow 
        ! interpolation variables
        REAL*8 :: w0
        integer :: ia, ial, iar 

        ! labour income
        linc_m = wn_m(it_com) * lab1(ij_com) * eff(ij_com) * theta(ip_com) * eta(ism_com) 
        linc_f = wn_f(it_com) * lab2(ij_com) * exp(h(ih_com)) * theta(ip_com) * eta(isf_com)
        
        ! total income
        ! Uncomment below for fortran with IMSL library 
        !total = (1d0+rn(it_com))*a_err &                  
        !                + linc_m + abs(real(il_com,kind=8) > 0)*(linc_f-wf(it_com)*lab2(ij_com)*cchild(ij_com)) & 
        !                + pen(ij_com, it_com) & 
        !                + abs(nchild(ij_com) > 0)*Tr(it_com)*nchild(ij_com) & 
        !                + bequest(ij_com, it_com) &
        !                + comp(ia_com, ih_com, ijps_com, it_com)
        
        ! Uncomment below for basic fortran without IMSL library 
        total = (1d0+rn(it_com))*a_err &                  
                + linc_m + real(il_com,kind=8)*(linc_f-wf(it_com)*lab2(ij_com)*cchild(ij_com)) & 
                + pen(ij_com, it_com) & 
                + bequest(ij_com, it_com) &
                + comp(ia_com, ih_com, ijps_com, it_com)
        if (nchild(ij_com) > 0) total = total + Tr(it_com)*nchild(ij_com)   ! childcare transfer 

        ! consumption 
        cons = (total - aplus)/p(it_com)
        cons = max(cons, 1d-10)
        
        ! interpolate
        do ia = 0, NA-2; if (aplus < a(ia)) exit; enddo 
            ial = ia; iar = ia+1; 
            w0 = (aplus - a(ial))/(a(iar)-a(ial)) 
        
        tomorrow = (1d0-w0)*RHS(ial, ih_com, ip_com, ism_com, isf_com, il_com) + &
                            w0*RHS(iar, ih_com, ip_com, ism_com, isf_com, il_com) 
        
        ! calculate euler residual 
        euler_res = 1d0 - (margu(cons, real(il_com, kind=8), ij_com, it_com, form1=1)**(-gamma) / tomorrow)
        
        END FUNCTION euler_res
    
    
    
    ! Utility function 
    ! INTEGER
    FUNCTION utility_int(cons, lab, ij, form)
        
        ! we can add htype (parent vs. non-parent) that can result in different utility 
        implicit none 
        real*8, intent(in) :: cons
        integer, intent(in) :: lab, ij
        integer, optional, intent(in) :: form 
        integer :: select_form 
        real*8 :: u1, u2, utility_int
        ! real*8 :: cons_help
        ! integer :: lab_help
        
        ! Select util form 
        select_form = 0 
        if (present(form)) select_form = form 
        
        ! Ensure cons and lab are not out of bound 
        ! cons_help = max(real(cons,kind=8), 1d-10)
        ! lab_help  = max(lab, 0)      
        
        ! Compute utility based on the selected formula 
        SELECT CASE (select_form)
        CASE (0) 
            select case (lab) 
            case (0:1) 
                !utility_int = (cons/sqrt(2d0+real(nchild(ij),kind=8)))**egam/egam - chi*abs(real(lab,kind=8)/L_max > 0d0) ! or, ceiling(real(lab,4)/L_max), or real(lab,4)/L_max 
                ! Uncomment below for the basic version of fortran without IMSL library
                utility_int = (cons/sqrt(2d0+dble(nchild(ij))))**egam/egam 
                if (lab > 0) utility_int = utility_int - chi ! *dble(lab)/L_max
            case default 
                error stop "Labour is out of bound." 
            end select 
            
        CASE (1) 
            select case (lab) 
            case (0:2) 
                ! assuming consumption is joint consumption (perfectly altruistic household) 
                !u1 = ( (cons/sqrt(1d0+real(nchild(ij),kind=8)))**nu * (1d0-lab1(ij))**(1d0-nu) )**egam/egam 
                !u2 = ( (cons/sqrt(1d0+real(nchild(ij),kind=8)))**nu * (1d0 - abs(real(lab,kind=8) > 0d0)*(lab2(ij)+chi))**(1d0-nu) )**egam/egam 
                !utility_int = u1 + u2 

                ! Uncomment below for the basic version of fortran without IMSL library
                u1 = ( (cons/sqrt(1d0+real(nchild(ij),kind=8)))**nu * (1d0-lab1(ij))**(1d0-nu) )**egam/egam 
                u2 = (cons/sqrt(1d0+real(nchild(ij),kind=8)))**nu 
                if (lab > 0) then 
                    u2 = u2 * (1d0 - (lab2(ij)+chi))**(1d0-nu)
                endif 
                u2 = u2**egam/egam

                utility_int = u1 + u2 
            case default 
                error stop "Labour is out of bound." 
            end select 
            
        CASE DEFAULT 
        error stop "Utility function is not recognized."
            
        END SELECT       
    
    END FUNCTION utility_int
    
    
    
    ! REAL 
    FUNCTION utility_real(cons, lab, ij, form)
        
        ! we can add htype (parent vs. non-parent) that can result in different utility 
        implicit none 
        real*8, intent(in) :: cons, lab
        integer, intent(in) :: ij
        integer, optional, intent(in) :: form 
        integer :: select_form 
        real*8 :: u1, u2, utility_real
        ! real*8 :: cons_help
        ! integer :: lab_help
        
        ! Select util form 
        select_form = 0 
        if (present(form)) select_form = form 
        
        ! Ensure cons and lab are not out of bound 
        ! cons_help = max(real(cons,kind=8), 1d-10)
        ! lab_help  = max(lab, 0)      
        
        ! Compute utility based on the selected formula 
        SELECT CASE (select_form)
        CASE (0) 
                !utility_real = (cons/sqrt(2d0+real(nchild(ij),kind=8)))**egam/egam - chi*abs(lab/L_max > 0d0) ! or, ceiling(real(lab,4)/L_max), or real(lab,4)/L_max 
                ! Uncomment below for the basic version of fortran without IMSL library
                utility_real = (cons/sqrt(2d0+dble(nchild(ij))))**egam/egam 
                if (lab > 0d0) utility_real = utility_real - chi !*dble(lab)/L_max
 
        CASE (1) 
                ! assuming consumption is joint consumption (perfectly altruistic household) 
                !u1 = ( (cons/sqrt(1d0+real(nchild(ij),kind=8)))**nu * (1d0-lab1(ij))**(1d0-nu) )**egam/egam 
                !u2 = ( (cons/sqrt(1d0+real(nchild(ij),kind=8)))**nu * (1d0 - abs(lab > 0d0)*(lab2(ij)+chi))**(1d0-nu) )**egam/egam 
                !utility_real = u1 + u2 

                 ! Uncomment below for the basic version of fortran without IMSL library
                u1 = ( (cons/sqrt(1d0+real(nchild(ij),kind=8)))**nu * (1d0-lab1(ij))**(1d0-nu) )**egam/egam 
                u2 = (cons/sqrt(1d0+real(nchild(ij),kind=8)))**nu 
                if (lab > 0d0) then 
                    u2 = u2 * (1d0 - (lab2(ij)+chi))**(1d0-nu)
                endif 
                u2 = u2**egam/egam

                utility_real = u1 + u2 
        CASE DEFAULT 
        error stop "Utility function is not recognized."
            
        END SELECT       
    
    END FUNCTION utility_real
    
    
    ! PARAMETRIC TAX FUNCTION 
    FUNCTION inctaxfunc(a,y1,y2,it,mtr)
        
        implicit none 
        real*8, intent(in) :: a, y1, y2 
        integer, intent(in) :: it
        integer, optional, intent(in) :: mtr  ! 0:tax amount, 
                                              ! 1:marginal tax rate (capital income)
                                              ! 2:marginal tax rate (husband's labor income)
                                              ! 3:marginal tax rate (wife's labor income)
        real*8, dimension(3) :: tax, taxable_inc
        real*8 :: inctaxfunc 
        integer :: i, flag 

        ! scale by male 20-24 predicted earnings (from panel regression with age, age2, and i.wave)
        ! = 972.6459 per week 

        ! calculate total taxable income 
        ! assume: capital earnings is split evenly between both partners
        taxable_inc = scale*(/0d0, y1+(r(it)*a/2d0), y2+(r(it)*a/2d0)/)

        ! calculate income tax or marginal tax amount
        flag = 0 
        if(present(mtr)) flag = mtr 

        tax = 0d0 
        ! calculate individual tax 
        do i = 1, size(taxable_inc)
            select case (flag)
            case(0) ! total amount    
                tax(i) = taxable_inc(i) - lambda_tauy(it)*taxable_inc(i)**(1d0-tauy(it)) 
            case(1) ! w.r.t y1
                if (i .ne. 2) cycle 
                tax(i) = 1d0 - (1d0-tauy(it))*lambda_tauy(it)*taxable_inc(i)**(-tauy(it))
            case(2) ! w.r.t y2 
                if (i .ne. 3) cycle 
                tax(i) = 1d0 - (1d0-tauy(it))*lambda_tauy(it)*taxable_inc(i)**(-tauy(it))
            case(3) ! w.r.t a 
                tax(i) = 0.5d0 * r(it) * (1d0 - (1d0-tauy(it))*lambda_tauy(it)*taxable_inc(i)**(-tauy(it)))
            case default 
                error stop "Invalid value. 'mtr' is an interger in [0,3]." 
            end select 
        enddo 
        
        ! calculate family level total tax/marginal tax 
        inctaxfunc = sum(tax(:))/scale 

    END FUNCTION inctaxfunc 



    ! Payroll tax function 
    FUNCTION ptaxfunc(y1, y2, it, mtr) 

        implicit none 
        real*8, intent(in) :: y1, y2 
        integer, intent(in) :: it 
        integer, optional, intent(in) :: mtr 
        real*8 :: ptaxfunc 
        integer :: flag 

        flag = 0 
        if(present(mtr)) flag = mtr 

        select case (flag)
        case(0) ! total payroll tax amount 
            ptaxfunc = taup(it)*(y1+y2) 
        case(1) ! margial payroll tax w.r.t y1 or y2 
            ptaxfunc = taup(it)
        case default 
            error stop "Invalid value. 'mtr' is an interger in [0,1]."
        end select 

    END FUNCTION ptaxfunc




    !///////////////////////////////////////!
    !///// FTB A and FTB B Subroutiens /////!
    !///////////////////////////////////////!

    ! Large family supplement at age ij 
    FUNCTION LFS(ij) 

        implicit none 
        integer, intent(in) :: ij
        integer :: ndep024 ! number of dependent children aged [0,24]
        real*8  :: LFS 

        ! we assume all children are dependent children (i.e., FTB children)
        ndep024 = nchild(ij)

        if (ndep024 >= FTBA_C1(pyear)) then 
            LFS = FTBA_S1(pyear)*(ndep024 - FTBA_C1(pyear) + 1) 
        else 
            LFS = 0d0 
        endif 
    END FUNCTION LFS 


    ! Newborn supplement 
    FUNCTION NBS(ij, fppl) 

        implicit none 
        integer, intent(in) :: ij 
        logical, optional, intent(in) :: fppl
        logical :: fppl_aux
        integer :: ic, nbirths ! number of births in age ij  
        real*8  :: NBS

        ! paid parental leave (default is false) 
        fppl_aux = .false. 
        if (present(fppl)) fppl_aux = fppl

        ! note: default values of each childage array is set to -1 
        nbirths = 0 
        do ic = 1, 5 
            if (childage(ic, ij) == 0) nbirths = nbirths + 1 
        enddo 

        ! NOTE: for calibration purpose, if financial year <= 2013-14, then NBS = 0
        !       since it was introduced in 2014-15 financial year 

        ! calculate NBS 
        NBS = 0d0 
        select case (fppl_aux) 
        case (.true.) 
            if (nbirths<=1) then 
                NBS = 0d0 
            else if (nbirths>=2 .and. firstchild(ij)==1) then 
                NBS = FTBA_NS(pyear,1)*(nbirths-1) 
            else if (nbirths>=2 .and. firstchild(ij)==0) then 
                NBS = FTBA_NS(pyear,2)*(nbirths-1) 
            !else 
            !    error stop "Unrecognized condition for NBS."
            endif 
        case (.false.)
            if (nbirths>=1 .and. firstchild(ij)==1) then 
                NBS = FTBA_NS(pyear,1)*nbirths 
            else if (nbirths>=1 .and. firstchild(ij)==0) then
                NBS = FTBA_NS(pyear,2)*nbirths 
            !else 
            !    error stop "Unrecognized condition for NBS."
            endif    
        end select 


        if(pyear <= 14) NBS = 0d0 

    END FUNCTION NBS 


    ! Multiple birth allowance 
    FUNCTION MBA(ij, at_school)

        implicit none 
        integer, intent(in) :: ij 
        logical, optional, intent(in) :: at_school 
        logical :: at_school_aux
        integer :: ic, sameage, age 
        real*8 :: MBA 

        at_school_aux = .true. ! assume every child attends school 
        if (present(at_school)) at_school_aux = at_school 

        sameage = 1
        age = 0
        do ic = 2, 5 
            if (childage(ic, ij) == childage(ic-1, ij)) then 
                sameage = sameage + 1
                if(age==0) age = childage(ic, ij) ! representative's age 
            endif 
        enddo 

        select case (at_school_aux) 
        case (.true.) ! cutoff age is FTBA_MAGES if children attend school
            if (sameage == 3 .and. (age <= FTBA_MAGES(pyear))) then 
                MBA = FTBA_MBA1(pyear)
            else if (sameage >= 3 .and. (age <= FTBA_MAGES(pyear))) then 
                MBA = FTBA_MBA2(pyear)
            endif 
        case (.false.) ! otherwise, cutoff age is FTBA_MAGE 
            if (sameage == 3 .and. (age <= FTBA_MAGE(pyear))) then 
                MBA = FTBA_MBA1(pyear)
            else if (sameage >= 3 .and. (age <= FTBA_MAGE(pyear))) then 
                MBA = FTBA_MBA2(pyear)
            endif 
        end select 

    END FUNCTION MBA 


    ! Clean energy supplement 
    FUNCTION CES(ij, at_school, prev_CES) result(CESout)

        implicit none 
        integer, intent(in) :: ij 
        logical, optional, intent(in) :: at_school, prev_CES
        real*8, dimension(2) :: CESout 
        logical :: at_school_aux, prev_CES_aux 
        integer :: ic, ndep012, ndep1315, ndep017, ndep1619_as, ndep1819_as
        real*8 :: CES_base, CES_max 

        ! whether or not the child attends school
        at_school_aux = .true. 
        if (present(at_school)) at_school_aux = at_school

        ! whether or not the family received CES in the previous year 
        prev_CES_aux = .true.
        if (present(prev_CES)) prev_CES_aux = prev_CES

        ! Determine number of dependent children at specific age ranges 
        ndep012 = 0; ndep1315 = 0; ndep017 = 0; 
        ndep1619_as = 0; ndep1819_as = 0;
        do ic = 1, 5 
            select case (childage(ic, ij)) 
                case(0:12) 
                    ndep012 = ndep012 + 1 
                    ndep017 = ndep017 + 1 
                !case(0:17)
                !    ndep017 = ndep017 + 1 
                case(13:15)
                    ndep1315 = ndep1315 + 1 
                    ndep017  = ndep017 + 1 
                case(16:17) 
                    ndep017  = ndep017 + 1
                    if (at_school_aux) ndep1619_as = ndep1619_as + 1 
                !case(16:19)
                !    if (at_school_aux) ndep1619_as = ndep1619_as + 1
                case(18:19) 
                    if (at_school_aux) ndep1619_as = ndep1619_as + 1
                    if (at_school_aux) ndep1819_as = ndep1819_as + 1 
            end select 

            ! Or, 
            !if (childage(ic, ij) >= 0 .and. childage(ic, ij) <= 12) ndep012 = ndep012 + 1 
            !if (childage(ic, ij) >= 0 .and. childage(ic, ij) <= 17) ndep017 = ndep017 + 1 
            !if (childage(ic, ij) >= 13 .and. childage(ic, ij) <= 15) ndep1315 = ndep1315 + 1 
            !if (childage(ic, ij) >= 16 .and. childage(ic, ij) <= 19 .and. at_school_aux) ndep1619_as = ndep1619_as + 1 
            !if (childage(ic, ij) >= 18 .and. childage(ic, ij) <= 19 .and. at_school_aux) ndep1819_as = ndep1819_as + 1  
        
        enddo 

        ! calculate base CES (added to base payment of FTB A)
        CES_base = (ndep017 * FTBA_CE(pyear,1)) + (ndep1819_as * FTBA_CE(pyear,1))
        
        ! calculate max CES  (added to max payment of FTB A)
        CES_max  = (ndep012 * FTBA_CE(pyear,2)) + (ndep1315 * FTBA_CE(pyear,3)) & 
                   + (ndep1619_as * FTBA_CE(pyear,3))

        CESout(:) = (/CES_base, CES_max/)

        ! set CESout to 0 if financial year >= 2017-18 and the family did not receive 
        ! CES in the previous year 
        if (pyear >= 18 .and. (.not. prev_CES_aux)) CESout(:) = (/0d0, 0d0/)

    END FUNCTION CES


    ! Rent assistance 
    FUNCTION RA(ij, fam_income, annual_rent, married, is_private_rent) 

        implicit none 
        integer, intent(in) :: ij 
        real*8, intent(in)  :: fam_income, annual_rent
        logical, optional, intent(in) :: is_private_rent, married
        real*8 :: RA 
        real*8 :: MIN_FTBA, FTBA, min_rent, RA_max 
        integer :: ic, ndep024
        logical :: is_private_rent_aux 
        logical :: married_aux 

        ! whether or not the potential receipient is married/partnered (default is true) 
        married_aux = .true. 
        if (present(married)) married_aux = married 

        ! whether or not the rent is private
        is_private_rent_aux = .true.
        if (present(is_private_rent)) is_private_rent_aux = is_private_rent

        ! minimum FTB A amount to qualify for rent assistance (set to base payment the family eligible for)
        MIN_FTBA = 0d0 
        if (pyear <= 12) MIN_FTBA = FTB_A(ij, fam_income=fam_income, get_base_pay=.true., interim=.true.)

        ! interim value of FTB A (excluding rent assistance) 
        FTBA = FTB_A(ij, fam_income=fam_income, interim=.true.)

        ! Determine maximum rent assistance 
        do ic = 1, 5 
            select case (childage(ic, ij)) 
                case(0:24)
                    ndep024  = ndep024 + 1 
            end select 
        enddo 

        if (ndep024 <= 2) then 
            RA_max = FTBA_TH2A(pyear)
        else ! if >= 3 
            RA_max = FTBA_TH2B(pyear) 
        endif 

        ! Determine minimum rent to qualify for rent assistance 
        if (.not. married) then 
            min_rent = FTBA_TH1A(pyear) 
        else 
            min_rent = FTBA_TH1B(pyear)
        endif 

        ! calculate rent assistance given the interim FTB A received 
        if (FTBA > 0d0 .and. FTBA >= MIN_FTBA .and. is_private_rent_aux) then 
            RA = max(0d0, &
                     min(0.75d0*(annual_rent - min_rent), RA_max))
        else 
            RA = 0d0 
        endif 


    END FUNCTION RA
    
    

    ! Family Tax Benefit Part A (FTB A) 
    FUNCTION FTB_A(ij, fam_income, annual_rent, at_school, get_base_pay, interim) 

        implicit none 
        integer, intent(in) :: ij 
        real*8, intent(in)  :: fam_income 
        real*8, optional, intent(in)  :: annual_rent
        logical, optional, intent(in) :: at_school, get_base_pay, interim
        real*8 :: FTB_A 
        integer :: ic, ndep012, ndep1315, ndep1617, ndep1619_as, ndep1617_nas, ndep1821, ndep1824, &  ! for max payment calculation
                       ndep017, ndep1819_as, ndep1821_nas, & ! for base payment calculation
                       ndep024 ! for base payment threshold calculation
        real*8 :: scaled_income, base_pay, max_pay, th_base, th_max
        real*8 :: CES2(2) 
        logical :: at_school_aux, get_base_pay_aux, interim_aux

        ! whether or not the function is used to calculate just the base payment of FTB_A (required to compute rent assistance) 
        get_base_pay_aux = .false. 
        if (present(get_base_pay)) get_base_pay_aux = get_base_pay 
        
        ! whether or not the function is used to calculate FTB A's interim value (without rent assistance) 
        interim_aux = .true. 
        if (present(interim)) interim_aux = interim 

        ! whether or not the child attends school
        at_school_aux = .true. 
        if (present(at_school)) at_school_aux = at_school


        ! Determine number of dependent children at specific age ranges 
        ndep012 = 0; ndep1315 = 0; ndep1617 = 0; ndep1619_as = 0; ndep1617_nas = 0; 
        ndep1821 = 0; ndep1824 = 0; ndep017 = 0; ndep1819_as = 0; ndep1821_nas = 0;
        ndep024 = 0; 

        do ic = 1, 5 
            select case (childage(ic, ij)) 
                case(0:12) 
                    ndep012 = ndep012 + 1 
                    ndep017 = ndep017 + 1 
                    ndep024 = ndep024 + 1 
                case(13:15)
                    ndep1315 = ndep1315 + 1 
                    ndep017  = ndep017  + 1 
                    ndep024  = ndep024  + 1 
                case(16:17)
                    ndep1617 = ndep1617 + 1 
                    ndep017  = ndep017  + 1 
                    ndep024  = ndep024  + 1
                    if (at_school_aux) ndep1619_as = ndep1619_as + 1 
                    if (.not. at_school_aux) ndep1617_nas = ndep1617_nas + 1
                case(18:19) 
                    ndep1821 = ndep1821 + 1 
                    ndep1824 = ndep1824 + 1 
                    ndep024  = ndep024  + 1 
                    if (at_school_aux) ndep1619_as = ndep1619_as + 1
                    if (at_school_aux) ndep1819_as = ndep1819_as + 1
                    if (.not. at_school_aux) ndep1821_nas = ndep1821_nas + 1 
                case(20:21) 
                    ndep1821 = ndep1821 + 1 
                    ndep1824 = ndep1824 + 1 
                    ndep024  = ndep024  + 1 
                    if (.not. at_school_aux) ndep1821_nas = ndep1821_nas + 1 
                case(22:24) 
                    ndep1824 = ndep1824 + 1 
                    ndep024  = ndep024 + 1 
            end select 

            ! Or, we can use if statement 
            !if (childage(ic, ij) >= 0 .and. childage(ic, ij) <= 12) ndep012 = ndep012 + 1 
            !if (childage(ic, ij) >= 0 .and. childage(ic, ij) <= 17) ndep017 = ndep017 + 1 
            !if (childage(ic, ij) >= 0 .and. childage(ic, ij) <= 24) ndep024 = ndep024 + 1 
            !if (childage(ic, ij) >= 13 .and. childage(ic, ij) <= 15) ndep1315 = ndep1315 + 1 
            !if (childage(ic, ij) >= 16 .and. childage(ic, ij) <= 17) ndep1617 = ndep1617 + 1 
            !if (childage(ic, ij) >= 18 .and. childage(ic, ij) <= 21) ndep1821 = ndep1821 + 1 
            !if (childage(ic, ij) >= 18 .and. childage(ic, ij) <= 24) ndep1824 = ndep1824 + 1 
            !if (childage(ic, ij) >= 16 .and. childage(ic, ij) <= 19 .and. at_school_aux) ndep1619_as = ndep1619_as + 1 
            !if (childage(ic, ij) >= 16 .and. childage(ic, ij) <= 17 .and. (.not. at_school_aux)) ndep1617_nas = ndep1617_nas + 1 
            !if (childage(ic, ij) >= 18 .and. childage(ic, ij) <= 19 .and. at_school_aux) ndep1819_as = ndep1819_as + 1 
            !if (childage(ic, ij) >= 18 .and. childage(ic, ij) <= 21 .and. (.not. at_school_aux)) ndep1821_nas = ndep1821_nas + 1 
            
        enddo 

        ! Get CES_base and CES_max 
        CES2(:) = CES(ij) 

        ! Calculate FTB A base payment 
        base_pay = LFS(ij) + NBS(ij) + MBA(ij) + CES2(1) & 
                    + ndep017 * FTBA_Base(pyear, 1) & 
                    + ndep1824 * FTBA_Base(pyear, 2) & 
                    + ndep1819_as * FTBA_Base(pyear, 3) & 
                    + ndep1821_nas * FTBA_Base(pyear, 4)

        ! to be used in RentAssistance function
        if (get_base_pay_aux .and. interim_aux) then 
            FTB_A = base_pay 
            return 
        endif 

        ! Calculate FTB A maximum payment 
        max_pay  = LFS(ij) + NBS(ij) + MBA(ij) + CES2(2) & 
                    + ndep012 * FTBA_Max(pyear, 1) &
                    + ndep1315 * FTBA_Max(pyear, 2) & 
                    + ndep1617 * FTBA_Max(pyear, 3) & 
                    + ndep1824 * FTBA_Max(pyear, 4) & 
                    + ndep1619_as * FTBA_Max(pyear, 5) & 
                    + ndep1617_nas * FTBA_Max(pyear, 6) & 
                    + ndep1821 * FTBA_Max(pyear, 7)

        ! Calculate threshold for max and base payment rates 
        th_max = FTBA_T(pyear, 1)                                   ! --> income threshold for maximum payment
        th_base = FTBA_T(pyear, 2) + (ndep024 - 1)*FTBA_T2A(pyear)  ! --> additional adjustment to base payment for
                                                                    !     additional FTB children

        ! Calculate FTB A without rent assistance (interim value) - NOT APPLICABLE here 
        scaled_income = scale*fam_income 

        ! initialize
        FTB_A = 0d0 

        if (scaled_income  <= th_max) then 
            FTB_A = max_pay 
        else if (scaled_income > th_max .and. scaled_income <= th_base) then 
            FTB_A = MAX(base_pay,  & 
                        max_pay - FTBA_W(pyear, 1)*(scaled_income - th_max))
        else if (scaled_income > th_base) then 
            FTB_A = MAX(0d0, & 
                        max_pay - FTBA_W(pyear, 1)*(scaled_income - th_max), & 
                        base_pay - FTBA_W(pyear, 2)*(scaled_income - th_base))
        !else 
        !    error stop "Function error: FTB A." 
        endif 

        ! Adjusting FTB_A values if not eligible for FTB_A's per child supplement
        if (scaled_income > FTBA_FT1(pyear)) then 
            FTB_A = MAX(0d0,  FTB_A - FTBA_AS(pyear)*(ndep012 + ndep1315 + ndep1619_as))
        endif 


        ! FOR THE CURRENT MODEL, THE CALCULATION OF FTB_A DOES NOT INCLUDE RENT ASSISTANCE, 
        ! AND THEREFORE, THE FUNCTION EXECUTION ENDS HERE. 
        !---------------------------------------------------------------------------------------------------------!
        
        ! End function execution here and return to main program if interim_aux is true. 
        IF (interim_aux) then 
            FTB_A = FTB_A / scaled_income
            RETURN 

        else    
            ! Otherwise, calculate rent assistance and the final value of FTB A that accounts for rent assistance. 
            ! (Assuming couple and private rent. See RA function for more info.)
            max_pay = max_pay + RA(ij, fam_income=scaled_income, annual_rent=annual_rent)

            ! Calculate final value of FTB_A given the new maximum pay rate of FTB_A
            if (scaled_income  <= th_max) then 
                FTB_A = max_pay 
            else if (scaled_income > th_max .and. scaled_income <= th_base) then 
                FTB_A = MAX(base_pay,  & 
                            max_pay - FTBA_W(pyear, 1)*(scaled_income - th_max))
            else if (scaled_income > th_base) then 
                FTB_A = MAX(0d0, & 
                            max_pay - FTBA_W(pyear, 1)*(scaled_income - th_max), & 
                            base_pay - FTBA_W(pyear, 2)*(scaled_income - th_base))
            !else 
            !    error stop "Function error: FTB A." 
            endif 

            FTB_A = FTB_A / scaled_income

        endif 
    END FUNCTION FTB_A 




    ! Clean energy supplements for part B 
    FUNCTION CES_B(ij, at_school, prev_CESB) 

        implicit none 
        integer, intent(in) :: ij 
        logical, optional, intent(in) :: at_school, prev_CESB
        real*8  :: CES_B
        integer :: ic, ndep04, ndep515, ndep1618_as 
        logical :: at_school_aux, prev_CESB_aux

        ! whether or not the child attends school
        at_school_aux = .true. 
        if (present(at_school)) at_school_aux = at_school

        ! whether or not was a recipient of CES_B last year 
        prev_CESB_aux = .true. 
        if (present(prev_CESB)) prev_CESB_aux = prev_CESB 


        ! Determine number of dependent children at specific age ranges 
        ndep04 = 0;  ndep515 = 0;  ndep1618_as = 0; 

        do ic = 1, 5 
            select case (childage(ic, ij)) 
            case(0:4) 
                ndep04 = ndep04 + 1 
            case(5:15) 
                ndep515 = ndep515 + 1 
            case(16:18) 
                if (at_school_aux) ndep1618_as = ndep1618_as + 1 
            end select 
        enddo 

        ! calculate CES for FTB part 2 
        if (pyear >= 18 .and. (.not. prev_CESB_aux)) then 
            CES_B = 0d0 
        else
            if (ndep04 >= 1) then 
                CES_B = FTBB_CE(pyear, 1)
            else if ((ndep04 == 0) .and. (ndep515>=1 .or. ndep1618_as>=1)) then 
                CES_B = FTBB_CE(pyear, 2)
            else 
                error stop "ERROR: CES_B function." 
            endif 
        endif 

    END FUNCTION 



    ! Calculate Family Tax Benefit part B 
    FUNCTION FTB_B(ij, p1_income, p2_income, married, at_school) 

        implicit none 
        integer, intent(in) :: ij 
        real*8, intent(in)  :: p1_income ! partner 1's income
        real*8, optional, intent(in) :: p2_income ! partner 2's income 
        logical, optional, intent(in) :: married, at_school
        real*8 :: FTB_B
        real*8 :: p2_income_aux, pe_income, se_income ! primary earner and secondary earner's income
        integer :: ic, ndep04, ndep515, ndep512, ndep1315, ndep1618_as
        logical :: married_aux, at_school_aux 

        ! whether or not children attend school
        at_school_aux = .true. 
        if (present(at_school)) at_school_aux = at_school

        ! whether or not the potential receipient is married/partnered (default is true) 
        married_aux = .true. 
        if (present(married)) married_aux = married 

        ! detect missing partner's value and return error if married 
        p2_income_aux = -999d0 
        if (present(p2_income)) p2_income_aux = p2_income 
        if (married_aux .and. p2_income_aux==-999d0) error stop "Please input partner's income in the FTB_B function."

        ! Calculate income for primary and secondary earners
        if (married_aux) then 
            pe_income = scale * max(p1_income, p2_income)
            se_income = scale * min(p1_income, p2_income) 
        else 
            pe_income = scale * p1_income 
            se_income = 0d0 
        endif 

        ! Determine the number of depenent children 
        ndep04 = 0;  ndep512 = 0;  ndep515 = 0;   
        ndep1315 = 0;  ndep1618_as = 0;

        do ic = 1, 5 
            select case (childage(ic, ij)) 
            case(0:4) 
                ndep04 = ndep04 + 1 
            case(5:12) 
                ndep512 = ndep512 + 1 
                ndep515 = ndep515 + 1 
            case(13:15)
                ndep1315 = ndep1315 + 1
                ndep515 = ndep515 + 1 
            case(16:18) 
                if (at_school_aux) ndep1618_as = ndep1618_as + 1 
            end select 

            ! Or, if statement 
            !if (childage(ic, ij) >= 0 .and. childage(ic, ij) <= 4) ndep04 = ndep04 + 1 
            !if (childage(ic, ij) >= 5 .and. childage(ic, ij) <= 12) ndep512 = ndep512 + 1 
            !if (childage(ic, ij) >= 5 .and. childage(ic, ij) <= 15) ndep515 = ndep515 + 1 
            !if (childage(ic, ij) >= 13 .and. childage(ic, ij) <= 15) ndep1315 = ndep1315 + 1 
            !if (childage(ic, ij) >= 16 .and. childage(ic, ij) <=18 .and. at_school_aux) ndep1618_as = ndep1618_as + 1 
        enddo 


        ! Calculate FTB_B 
        FTB_B = 0d0 

        if (pe_income <= FTBB_T(pyear, 1) .and. se_income <= FTBB_T(pyear, 2)) then 
            if (ndep04 >= 1) then 
                FTB_B = FTBB_Max(pyear, 1) + CES_B(ij) 
            else 
                SELECT CASE (pyear) ! conditions change based on the year of observation 
                CASE (1:16) 
                    if ( (ndep04==0) .and. (ndep515>=1 .or. ndep1618_as>=1) ) then 
                        FTB_B = FTBB_Max(pyear, 2) + CES_B(ij) 
                    endif 

                CASE (17:18)
                    if ( (ndep04==0) .and. (ndep512>=1) ) then 
                        FTB_B = FTBB_Max(pyear, 2) + CES_B(ij) 

                    else if ( (ndep04==0) .and. (ndep512==0) .and. (ndep1315>=1 .or. ndep1618_as>=1) & 
                              .and. (.not. married_aux) ) then 
                        FTB_B = FTBB_Max(pyear, 2) + CES_B(ij) 
                    endif 

                CASE DEFAULT 
                    error stop "Year is out of bound." 
                END SELECT  
            endif 

        else if (pe_income <= FTBB_T(pyear, 1) .and. se_income > FTBB_T(pyear, 2)) then 
            if (ndep04 >= 1) then 
                FTB_B = MAX(0d0, FTBB_Max(pyear, 1)+CES_B(ij) - FTBB_W(pyear)*(se_income-FTBB_T(pyear, 2)))
            else 
                SELECT CASE (pyear) ! eligibility criteria change based on the year of observation 
                CASE (1:16) 
                    if ( (ndep04==0) .and. (ndep515>=1 .or. ndep1618_as>=1) ) then 
                        FTB_B = MAX(0d0, FTBB_Max(pyear, 2)+CES_B(ij) - FTBB_W(pyear)*(se_income-FTBB_T(pyear, 2)))
                    endif 
                
                CASE (17:18)
                    if ( (ndep04==0) .and. (ndep512>=1) ) then 
                        FTB_B = MAX(0d0, FTBB_Max(pyear, 2)+CES_B(ij) - FTBB_W(pyear)*(se_income-FTBB_T(pyear, 2)))

                    else if ( (ndep04==0) .and. (ndep512==0) .and. (ndep1315>=1 .or. ndep1618_as>=1) & 
                              .and. (.not. married_aux) ) then 
                        FTB_B = MAX(0d0, FTBB_Max(pyear, 2)+CES_B(ij) - FTBB_W(pyear)*(se_income-FTBB_T(pyear, 2)))
                    endif 
               
                CASE DEFAULT 
                    error stop "Year is out of bound." 
                END SELECT  
            endif 
        
        endif 


        ! scale back down to model value 
        FTB_B = FTB_B / scale 

    END FUNCTION 



    !/////////////////////////////////////////!
    !///// Child Care Subsidy Subroutine /////!
    !/////////////////////////////////////////!

    ! Child care subsidy's eligibility criteria are: 
    ! 1. Combined family income 
    ! 2. Type of childcare used (muted)
    ! 3. Age of child
    ! 4. Hours worked by parents (the lower of the two)

    FUNCTION CCS_rate(ij, fam_income, lab1, lab2)

        implicit none 
        integer, intent(in) :: ij
        real*8, intent(in) ::  fam_income, lab1, lab2
        real*8 :: CCS_rate 
        real*8 :: scaled_income, taper_rate, taper_unit 
        real*8 :: lab_hours, adj_factor

        ! THREE MAIN ASSUMPTIONS: 
        !
        ! 1. NO CAP ON HOURLY FEE
        ! Since we have no information on the type of childcare 
        ! used by family in our model, we simply assume there is no 
        ! cap on hourly fee of childcare. 
        ! 
        ! What is a cap on hourly fee? 
        ! e.g., Consider a family qualified for 85% subsidy on childcare cost. 
        !       A $100 per day at a care centre for 10 hours --> hourly fee = 100/10 = $10. 
        !       The cap on hourly fee for day care centre service is $12.31 for children below school age. 
        !       Then, the hourly subsidy is calculated as min($10*0.85, $12.31*0.85) = $8.5. 
        !       The weekly subsidy is thus 5*10*8.5 = $425. 
        !
        ! 2. IDENTICAL CHILD CARE SERVICE 
        ! We assume identical child care hourly fee and that family will exhaust all 
        ! the available hours of subsidised care as determined by the level of recognized 
        ! activity each fortnight. 
        ! 
        ! See below for recognized hours and cap on subsidized hours: 
        ! i.   =< 8 hours        --> 0 hours (if earn above $70,015; 24 hours otherwise)
        ! ii.  > 8, <= 16 hours  --> 36 hours 
        ! iii. > 16, <= 48 hours --> 72 hours 
        ! iv.  > 48 hours        --> 100 hours 
        ! 
        ! With assumption 2, this translate into proportionally reduced subsidy rate. 
        ! e.g., in case (iii) --> a family eligible for 85% subsidy rate would then have 
        !       an effective subsidy rate of 72/100 * 85% = 61.2%
        ! 
        ! 3. We also assume no annual cap per child 
        !    (In 2021-22, the cap is $10,655 if family earns more than 190,015. But, 
        !    from July 2022, the govt is removing the cap. 

        scaled_income = scale * fam_income 
        taper_unit    = 3000d0  ! taper rate = 1% per $3000 increase in family income 

        ! Determine subsidy rate (currently use 2021 rate)
        CCS_rate = 0d0 

        if (scaled_income <= CCS_T(pyear, 1)) then
            CCS_rate = CCS_R(pyear, 1) 

        else if (scaled_income > CCS_T(pyear, 1) .and. scaled_income < CCS_T(pyear, 2)) then 
            taper_rate = (scaled_income - CCS_T(pyear, 1)) / (taper_unit * 100d0) 
            CCS_rate = max(CCS_R(pyear, 2), CCS_R(pyear, 1) - taper_rate) 

        else if (scaled_income >= CCS_T(pyear, 2) .and. scaled_income < CCS_T(pyear, 3)) then 
            CCS_rate = CCS_R(pyear, 2) 

        else if (scaled_income >= CCS_T(pyear, 3) .and. scaled_income < CCS_T(pyear, 4)) then 
            taper_rate = (scaled_income - CCS_T(pyear, 3)) / (taper_unit * 100d0) 
            CCS_rate = max(CCS_R(pyear, 3), CCS_R(pyear, 2) - taper_rate)
        
        else if (scaled_income >= CCS_T(pyear, 4) .and. scaled_income < CCS_T(pyear, 5)) then 
            CCS_rate = CCS_R(pyear, 3)

        else if (scaled_income >= CCS_T(pyear, 5)) then 
            CCS_rate = CCS_R(pyear, 4)
        endif 


        ! Acitivity test (work hours for this model) and adjustment factor to subsidy rate 
        ! (note: Some of the recognized activities are working, volunteering, and job seeking.)

        lab_hours = min(lab1, lab2) * 24 * 5 * 2  ! hours worked per fortnight 

        if (lab_hours <= 8d0) then  
            if (scaled_income <= 70015d0) then 
                adj_factor = 24d0/100d0    ! note that max subsidized hours = 100 
            else 
                adj_factor = 0d0 
            endif 
        else if (lab_hours > 8d0 .and. lab_hours <= 16d0) then 
            adj_factor = 36d0/100d0
        else if (lab_hours > 16d0 .and. lab_hours <= 48d0) then 
            adj_factor = 72d0/100d0 
        else if (lab_hours > 48d0) then 
            adj_factor = 1d0 
        else 
            error stop "Error: CSS_rate function."
        endif  

        ! Calculate the effective rate of child care subsidy 
        CCS_rate = adj_factor * CCS_rate

    END FUNCTION  CCS_rate




    
    ! Marginal utility function (per unit price) 
    FUNCTION margu(cons, lab, ij, it, form1, wrt) 
    
        implicit none 
        REAL*8, intent(in) :: cons, lab
        integer, intent(in) :: ij, it
        integer, optional, intent(in) :: form1, wrt 
        REAL*8 :: margu, util
        ! Other variables 
        ! REAL*8 :: cons_help 
        integer :: select_form 
        integer :: select_var 
        
        ! utility function
        select_form = 0 
        if (present(form1)) select_form = form1
        
        ! with respect to? 0: consumption, 1: labour, 2:...
        select_var = 0 
        if (present(wrt)) select_var = wrt
        
        ! Ensure cons is not out of bound 
        ! cons_help = max(cons, 1e-6) 
        
        SELECT CASE (select_form)
        CASE (0) 
            select case (select_var) 
            case (0); margu = (cons**(-1d0/gamma)/sqrt(2d0+real(nchild(ij),kind=8))**egam)/p(it)
            case default; error stop "please set the correct variable."
            end select
        
        CASE (1)
            select case (select_var) 
            case (0) 
                ! assuming consumption is joint consumption (perfectly altruistic household) 
                util = utility(cons, lab, ij, form=form1)
                
                margu = (nu*egam*util/cons)/p(it)
                
            case default; error stop "please set the correct variable."
            end select 
            
        CASE DEFAULT 
            error stop "unrecognized utility function."
        END SELECT 
    
    END FUNCTION margu
        

    ! Compute year agent age ij in time it becomes age ijp 
    FUNCTION year(it, ij, ijp) 
        
        implicit none 
        integer, intent(in) :: it, ij, ijp 
        integer :: year 
        
        year = it + (ijp - ij) 
        
        if(it==0  .or. year<=0 ) year = 0  ! set all years in time 0  (init ss) to 0 
        if(it==TT .or. year>=TT) year = TT ! set all years in time TT (final ss) to TT
    
    END FUNCTION year 
    
    
    ! Compute the total number of years a household lives
    function year2(it, addit) 
        
        implicit none 
        integer, intent(in) :: it, addit 
        integer :: year2 
        
        year2 = it + addit
        
        if (it==0  .or. year2 <= 0 ) year2 = 0
        if (it==TT .or. year2 >= TT) year2 = TT      
    
    end function 


    ! Indexing to reduce the dimension of decision variables    
    FUNCTION indexing(ij, ip, ism, isf) 
        
        integer, intent(in) :: ij, ip, ism, isf
        integer :: indexing 
        
        SELECT CASE (ij) 
        CASE (1:) ! working age 
            indexing = (NS*NS)*NP*(ij-1) 
            select case (ip) 
               case(1:NP); indexing = indexing + (NS*NS)*(ip-1) + NS*(ism-1) + isf
               ! case (1); indexing = indexing + NS*(ism-1) + isf
               ! case (2); indexing = indexing + NS*NS + NS*(ism-1) + isf 
               case default; error stop "ip is out of bound!"
            end select 
        
         !CASE (JR:) ! retirees
         !   indexing = (NS*NS)*NP*(JR-1) + (ij-JR+1)
        
        CASE DEFAULT 
            error stop "ij is out of bound!" 
        END SELECT 
        
    END FUNCTION indexing 
    
    
    !/////////////////////////////////////////////////////////////////////////////////////!
    ! Construct a growing grid using the formula:              
    ! a_i = left + h * ((1+growth)**i - 1) where i = 0, 1, 2, ..., n, and 
    !                                            h = (right-left)/((1+growth)**n - 1)
    SUBROUTINE grid_grow_r4(a, left, right, growth) 
    
        implicit none 
        
        REAL*4, intent(out) :: a(1:)            ! array for storing the result
        REAL*4, intent(in)  :: left, right      ! left and right bounds 
        REAL*4 :: growth                        ! growth rate of grids
        REAL*4 :: h                             ! length of one grid
        integer :: j,                         & ! grid counter
                   n                            ! size of array a
        
        ! number of grid points 
        n = size(a, 1) - 1 
    
        if (right <= left) error stop "upper bound has to be strictly greater than lower bound."
        if (growth <= 0) error stop "growth rate cannot be negative."
        
        ! calculate factor 
        h = (right-left)/((1+growth)**n - 1)
        
        ! calculate grid value 
        a = left + h*((1+growth)**(/(real(j,kind=4), j=0,n)/) - 1)
    
    END SUBROUTINE grid_grow_r4
    
    
    SUBROUTINE grid_grow_r8(a, left, right, growth) 
    
        implicit none 
        
        REAL*8, intent(out) :: a(1:)            ! array for storing the result
        REAL*8, intent(in)  :: left, right      ! left and right bounds 
        REAL*8 :: growth                        ! growth rate of grids
        REAL*8 :: h                             ! length of one grid
        integer :: j,                         & ! grid counter
                   n                            ! size of array a
        
        ! get number of grid points 
        n = size(a, 1) - 1 
    
        if (right <= left) error stop "upper bound has to be strictly greater than lower bound."
        if (growth < 0) error stop "growth rate cannot be negative."
        
        ! calculate factor 
        h = (right-left)/((1d0+growth)**n - 1d0)
        
        ! calculate grid value 
        a = left + h*((1d0+growth)**(/(dble(j), j=0,n)/) - 1d0) 
    
    END SUBROUTINE grid_grow_r8
    
    
    
    !/////////////////////////////////////////////////////////////////////////////////////!
    ! ROUWENHORST METHOD (DISCRETIZING AR1 PROCESS) 
    SUBROUTINE rouwenhorst_method_r4(rho, mu, sigma2_eps, z, pi, w)
    
       implicit none
       REAL*4, intent(in)  :: rho,   & ! autoregression parameter
                               mu,   & ! unconditional mean of the process
                       sigma2_eps      ! variance of the shock 
       REAL*4, intent(out) :: z(:),  & ! discrete shock values
                          pi(:, :)     ! transition matrix 
       REAL*4, intent(out), optional :: w(:) ! the stationary distribution
       integer :: n, i
       REAL*4 :: psi, sigma2_eta
       
       ! Comment out below variables if using standard fortran compiler 
       REAL*4, allocatable :: q(:,:), p2(:, :) ! dynamic arrays to solve for w anlaytically 
       REAL*4, allocatable :: b(:,:)
       integer :: status
       

       ! assert size equality and get approximation points
       n = assert_asequal(size(z), size(pi,1), size(pi,2), 'discretize_AR')
    
       ! calculate variance of the overall process
       sigma2_eta = sigma2_eps/(1-rho**2)
    
       ! determine the transition matrix
       call rouwenhorst_matrix(rho, pi)
    
       ! determine the nodes
       psi = sqrt(real(n-1,kind=4))*sqrt(sigma2_eta)
       do i = 1, n
           z(i) = -psi + 2.0*psi*real(i-1,kind=4)/real(n-1,kind=4)
       enddo
       z = z + mu
    
       ! compute the stationary distribution using IMSL libary
       ! if error (i.e., status/=0), resort to brute force.
       !call get_w(status) 
       ! 
       !if (status /= 0) then 
       !    if(present(w))then
       !        w = 1/real(n,kind=4)
       !        do i = 1, 10000
       !            w = matmul(transpose(pi), w)
       !        enddo
       !    endif
       !endif 
       
       ! comment out the above block (from call get_w(status)) 
       ! and uncomment the below block if the standard fortran compiler is used 
       if(present(w))then
            w = 1/real(n,kind=4)
            do i = 1, 10000
                w = matmul(transpose(pi), w)
            enddo
       endif
       
    
    contains
    
       recursive subroutine rouwenhorst_matrix(rho, pi_new)
    
           implicit none
           REAL*4, intent(in) :: rho
           REAL*4, intent(out) :: pi_new(:, :)
           integer :: n
           REAL*4 :: p, pi_old(size(pi_new,1)-1, size(pi_new,1)-1) 
     
           n = size(pi_new, 1) 
           p = (1+rho)/2
           
           if (n==2) then 
               pi_new(1:2, 1:2) = reshape((/p, 1-p, &
                                            1-p, p/), shape=(/2,2/))
           else 
               call rouwenhorst_matrix(rho, pi_old) 
               pi_new = 0 
    
               pi_new(1:n-1, 1:n-1) = pi_new(1:n-1, 1:n-1) + p*pi_old
               pi_new(1:n-1, 2:n  ) = pi_new(1:n-1, 2:n  ) + (1-p)*pi_old
               pi_new(2:n  , 1:n-1) = pi_new(2:n  , 1:n-1) + (1-p)*pi_old
               pi_new(2:n  , 2:n  ) = pi_new(2:n  , 2:n  ) + p*pi_old
    
               pi_new(2:n-1, :) = pi_new(2:n-1, :)/2
           endif
       end subroutine
       
       ! Comment out this subroutine if using astandard fortran compiler 
       ! solve for w analytically
       !subroutine get_w(status)
       !
       !     implicit none 
       !     integer :: status
       !     integer :: allocate_status
       !     
       !    ! CAUTION! .t. and .ix. rely on FNLMATH libary
       !     allocate (q(n,n), p2(n,1), b(n,1), STAT=allocate_status)
       !     if (allocate_status /= 0) status = 1
       !     
       !     q = eye(n) - .t.pi 
!
       !     ! Or, p2 = b + piT^2 @ b + @ piT^3 @ b + piT^4 @ b + .... where b = [0,0,0,0,0,...,1]
       !     ! --> p2 = (I-piT^2)^(-1) @ b 
       !     q(n, :) = 1 
       !     b = 0
       !     b(n, 1) = 1 
       !     p2 = q.ix.b ! --> matmul(q^-1, b2)
       !     w = p2(:, 1)
       ! 
       !     status = 0
       !     
       !end subroutine get_w
    
    END SUBROUTINE rouwenhorst_method_r4
    
    
    SUBROUTINE rouwenhorst_method_r8(rho, mu, sigma2_eps, z, pi, w)
    
       implicit none
       REAL*8, intent(in) :: rho ! autoregression parameter
       REAL*8, intent(in) :: mu  ! unconditional mean of the process
       REAL*8, intent(in) :: sigma2_eps ! variance of the shock 
       REAL*8, intent(out) :: z(:) ! discrete shock values
       REAL*8, intent(out) :: pi(:, :) ! transition matrix 
       REAL*8, intent(out), optional :: w(:) ! the stationary distribution
       integer :: n, i
       REAL*8 :: psi, sigma2_eta
       
       ! comment out below variables if standard fortran compiler is used 
       REAL*8, allocatable :: q(:,:), p2(:, :)
       REAL*8, allocatable :: b(:,:)
       integer :: status
       

       ! assert size equality and get approximation points
       n = assert_asequal(size(z), size(pi,1), size(pi,2), 'discretize_AR')
    
       ! calculate variance of the overall process
       sigma2_eta = sigma2_eps/(1d0-rho**2d0)
    
       ! determine the transition matrix
       call rouwenhorst_matrix(rho, pi)
    
       ! determine the nodes
       psi = sqrt(dble(n-1))*sqrt(sigma2_eta)
       ! note: total length = 2*psi
       do i = 1, n
           z(i) = -psi + 2d0*psi*dble(i-1)/dble(n-1)
       enddo
       z = z + mu
    
       ! compute the stationary distribution using IMSL libary
       ! if error (i.e., status/=0), resort to brute force.
       !call get_w(status) 
       ! 
       !if (status /= 0) then 
       !    if(present(w))then
       !        w = 1d0/dble(n)
       !        do i = 1, 10000
       !            w = matmul(transpose(pi), w)
       !        enddo
       !    endif
       !endif 
       
       ! comment out the above block (from call get_w(status)) 
       ! and uncomment the below block if the standard fortran compiler is used 
       if(present(w))then
            w = 1/real(n,kind=4)
            do i = 1, 10000
                w = matmul(transpose(pi), w)
            enddo
       endif
       
    
    contains
    
       recursive subroutine rouwenhorst_matrix(rho, pi_new)
    
           implicit none
           REAL*8, intent(in) :: rho
           REAL*8, intent(out) :: pi_new(:, :)
           integer :: n
           REAL*8 :: p, pi_old(size(pi_new,1)-1, size(pi_new,1)-1) 
     
           n = size(pi_new, 1) 
           p = (1d0+rho)/2d0 
           
           if (n==2) then 
               pi_new(1:2, 1:2) = reshape((/p, 1-p, &
                                            1-p, p/), shape=(/2,2/))
           else 
               call rouwenhorst_matrix(rho, pi_old) 
               pi_new = 0 
    
               pi_new(1:n-1, 1:n-1) = pi_new(1:n-1, 1:n-1) + p*pi_old
               pi_new(1:n-1, 2:n  ) = pi_new(1:n-1, 2:n  ) + (1d0-p)*pi_old
               pi_new(2:n  , 1:n-1) = pi_new(2:n  , 1:n-1) + (1d0-p)*pi_old
               pi_new(2:n  , 2:n  ) = pi_new(2:n  , 2:n  ) + p*pi_old
    
               pi_new(2:n-1, :) = pi_new(2:n-1, :)/2d0
           endif
       end subroutine
       
       
       ! Comment out this subroutine if using astandard fortran compiler 
       !subroutine get_w(status)
       !
       !     implicit none 
       !     integer :: status
       !     integer :: allocate_status
       !     
       !    ! CAUTION! .t. and .ix. rely on FNLMATH libary
       !     allocate (q(n,n), p2(n,1), b(n,1), STAT=allocate_status)
       !     if (allocate_status /= 0) status = 1
       !     q = eye(n) - .t.pi 

       !     ! Or, p2 = b + piT @ b + piT^2 @ b + @ piT^3 @ b + piT^4 @ b + .... where b = [0,0,0,0,0,...,1]
       !     ! --> p2 = (I-piT)^(-1) @ b 
       !     q(n, :) = 1 
       !     b = 0
       !     b(n, 1) = 1 
       !     p2 = q.ix.b ! --> equivalent to matmul(q^-1, b2)
       !     w = p2(:, 1)
       ! 
       !     status = 0
       !     
       !end subroutine get_w
    
    END SUBROUTINE rouwenhorst_method_r8  

    
    
    !/////////////////////////////////////////////////////////////////////////////////////!  
    ! assert equality and output n1 
    function assert_asequal(n1, n2, n3, string) 
    
        integer, intent(in) :: n1, n2, n3 
        character(len=*), intent(in) :: string
        integer :: assert_asequal
        
        if (n1==n2 .and. n2==n3) then 
            assert_asequal = n1 
        else 
            error stop 'an assert failed in assert_eq3'
        endif 
    
    end function assert_asequal
    
    
    !/////////////////////////////////////////////////////////////////////////////////////!  
    ! WRITE AND READ DECISIONS 
    !SUBROUTINE wr_decision(flag) 
        
    !    character(len=*), intent(in) :: flag  ! two options: 'write' and 'read'
        
    !    SELECT CASE (flag) 
    !    CASE('write')
    !        temp_var(:, :, :, :, :, :, it, 1) = c(:, :, :, :, :, :) 
    !        temp_var(:, :, :, :, :, :, it, 2) = l(:, :, :, :, :, :)
    !        temp_var(:, :, :, :, :, :, it, 3) = aplus(:, :, :, :, :, :) 
    !        temp_var(:, :, :, :, :, :, it, 4) = v(:, :, :, :, :, :)
    !    CASE('read') 
    !        c(:, :, :, :, :, :)     = temp_var(:, :, :, :, :, :, it, 1)
    !        l(:, :, :, :, :, :)     = temp_var(:, :, :, :, :, :, it, 1)
    !        aplus(:, :, :, :, :, :) = temp_var(:, :, :, :, :, :, it, 1)
    !        v(:, :, :, :, :, :)     = temp_var(:, :, :, :, :, :, it, 1)
    !    CASE DEFAULT 
    !        ERROR STOP "UNRECOGNIZED ARGUMENT. FUNCTION ADMITS ONLY 'write' and 'read'."
    !    END SELECT 
    !    END SUBROUTINE wr_decision
    
        
    
    
    !subroutine brent_r4(xmin, fret, minimum, maximum, func)
    ! 
    !    implicit none
    !    
    !    
    !    !##### INPUT/OUTPUT VARIABLES #############################################
    !    
    !    ! minimum value found
    !    real*4, intent(inout) :: xmin
    !    
    !    ! function value at minimum
    !    real*4, intent(out) :: fret
    !    
    !    ! left, middle and right interval points
    !    real*4, intent(in) :: minimum, maximum
    !    
    !    
    !    !##### OTHER VARIABLES ####################################################
    !    
    !    real*4 :: tol
    !    real*4, parameter :: cgold = 0.381966
    !    real*4, parameter :: zeps = 1.0e-3*epsilon(xmin)
    !    real*4 :: a=0, b=0, d=0, e=0, etemp, fu, fv, fw, fx, p, q, r, tol1, tol2, &
    !        u, v, w, x, xm, ax, bx, cx
    !    integer :: iter
    !    
    !    
    !    !##### INTERFACES #########################################################
    !    
    !    ! interface for the function
    !    interface
    !        function func(p)
    !            implicit none
    !            real*4, intent(in) :: p
    !            real*4 :: func
    !        end function func        
    !    end interface
    !    
    !    
    !    !##### ROUTINE CODE #######################################################
    !    
    !    ! set tolerance level
    !    tol = 1e-8
    !    
    !    ! set ax, bx and cx
    !    ax = minimum
    !    cx = maximum
    !
    !    a = min(ax, cx)
    !    b = max(ax, cx)
    !    
    !    if(abs(xmin-a) <= 1e-6)then
    !        bx = a + 1e-6
    !    elseif(abs(xmin-b) <= 1e-6)then
    !        bx = b - 1e-6
    !    elseif(xmin > a .and. xmin < b)then
    !        bx = xmin
    !    else
    !        bx = (ax+cx)/2d0
    !    endif
    !    
    !    v = bx
    !    w = v
    !    x = v
    !    e = 0
    !    fx = func(x)
    !    fv = fx
    !    fw = fx
    !    
    !    do iter = 1,200
    !        xm = 0.5*(a+b)
    !        tol1 = tol*abs(x)+zeps
    !        tol2 = 2.0d0*tol1
    !    
    !        if(abs(x-xm) <= (tol2-0.5*(b-a)))then
    !            xmin = x
    !            fret = fx
    !            return
    !        endif
    !    
    !        if(abs(e) > tol1)then
    !            r = (x-w)*(fx-fv)
    !            q = (x-v)*(fx-fw)
    !            p = (x-v)*q-(x-w)*r
    !            q = 2.0*(q-r)
    !            if (q > 0.0) p = -p
    !            q = abs(q)
    !            etemp = e
    !            e = d
    !            if(abs(p) >= abs(0.5*q*etemp) .or. &
    !                    p <= q*(a-x) .or. p >= q*(b-x))then
    !                e = merge(a-x, b-x, x >= xm )
    !                d = CGOLD*e
    !            else
    !                d = p/q
    !                u = x+d
    !                if(u-a < tol2 .or. b-u < tol2) d = sign(tol1, xm-x)
    !            endif
    !        
    !        else
    !            e = merge(a-x, b-x, x >= xm )
    !            d = CGOLD*e
    !        endif
    !        
    !        u = merge(x+d, x+sign(tol1, d), abs(d) >= tol1 )
    !        fu = func(u)
    !        if(fu <= fx)then
    !            if(u >= x)then
    !                a = x
    !            else
    !            b = x
    !            endif
    !            call shft(v, w, x, u)
    !            call shft(fv, fw, fx, fu)
    !        else
    !            if(u < x)then
    !                a = u
    !            else
    !                b = u
    !            endif
    !            if(fu <= fw .or. abs(w-x)  <= 1d-100)then
    !                v = w
    !                fv = fw
    !                w = u
    !                fw = fu
    !            elseif(fu <= fv .or. abs(v-x) <= 1d-100 .or. abs(v-w) <= 1d-100)then
    !                v = u
    !                fv = fu
    !            endif
    !        endif
    !    enddo
    !    
    !    call warning('fminsearch', 'maximum iterations exceeded')
    !
    !
    !!##### SUBROUTINES AND FUNCTIONS ##########################################
    !
    !contains
    !
    !
    !    !##########################################################################
    !    ! SUBROUTINE shft
    !    !
    !    ! Shifts b to a, c to b and d to c.
    !    !##########################################################################
    !    subroutine shft(a, b, c, d)
    !
    !        implicit none
    !
    !
    !        !##### INPUT/OUTPUT VARIABLES #########################################
    !
    !        real*4, intent(out)   :: a
    !        real*4, intent(inout) :: b, c
    !        real*4, intent(in   ) :: d
    !
    !
    !        !##### ROUTINE CODE ###################################################
    !        a = b
    !        b = c
    !        c = d
    !    end subroutine shft
    !
    !end subroutine brent_r4
    
    
    
END MODULE globals 