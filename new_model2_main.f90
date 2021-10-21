INCLUDE "new_model2_module.f90"
    
    
PROGRAM MODEL2
    
    use globals 
    
    implicit none 
    
    !NOTE: 
    !
    !THE PROBLEM SEEMS TO BE THE UPDATING PROCESS IN THE LSRA SUBROUTINE WITHIN THE FIRST FEW ITERATIONS
    !THE BAs IN TIME IT = 15 ONWARDS ARE TOO BIG THAT CAPITAL BECOMES NEGATIVE 
    !CAUSING Y TO BECOMES NEGATIVE 
    !
    !SOLUTIONS TO BE TESTED: 
    ! - CHANGE THE UPDATING RULE OF NEW COMPENSATION LEVEL (COMP_NEW) TO LOWER DAMPING PARAMETER LEVEL 
    !  (THIS SLOW UPDATING ENSURES THAT THE LSRA DOES NOT HAVE TO BORROW TO MUCH TO FINANCE THE 
    !   COMPENSATION SCHEME WITHIN THE FIRST FEW ITERATIONS)  --> works with some cases 
    ! - OR, WE CAN LOWER DAMP ONLY FOR THE FIRST FEW ITERATIONS WHEN THE LSRA SUBROUTINE IS THE 
    !   LEASE STABLE (e.g., if (iter <= 5) damp = 0.1d0). THEN, WE CAN RESET THE DAMPING VALUE TO 
    !   ITS INITIAL LEVEL AFTERWARDS. --> works with some cases
    ! - CAP Y VALUES AT SOME ARBITRARY SMALL VALUES 1d-8 (Y = max(Y, 1d-8)) --> does not work (result in exponentially increase BA
    !   since Y is arbitrarily capped)
    ! - TRY A LESS DISRUPTIVE/DISTORTIVE (SMALLER) REFORM 
    !        e.g., set Tr(1:TT) = 0.1d0*INC(0) instead of 0.3d0*INC(0)
    ! - IMPORTANT: GAIN (HEV) IS TOO LARGE FOR NEWBORN IN T=1 to TT 
    !              CHECK LSRA, AGGREGATION AND HOUSEHOLD SUBROUTINES TO SEE IF THERE IS ANY ERROR. 
    
    ! - TRY open_capmarket=.true. to fix interest rate with LSRA on


    !!!!!!!! DO NOW !!!!!!!!!!!
    ! 29/08/2021
    ! - Stop the program (breakpoint) when BA and YY turns NaN and inspects the value of consumption

    ! 30/08/2021
    ! - Try update with BA(it) = damp*BA(it) + (1d0-damp)*Bold(it) ! ---> currently implemented
    ! - Try reducing the time periods to 12 ! --> this seems to work 
    ! - If these measures are ineffective, we probably need to resort to lowering the damp for BA to a much lower value
    
    ! 07/09/2021
    ! - In particular, the problem with the LSRA in closed economy seems to arise from the large compensation which cause the 
    !   maximum possible savings to be greater than the upper bound of asset grid (a_u) when solving for optimal solutions of 
    !   those with large asset holdings (when solving for hosuehold with asset node close to the max node NA). 
    !   NEED TO SOLVE THIS PROBLEM!!! 
    !   ---> (HOW? TRY INCREASING a_u, and the number of asset nodes)
    ! 
    ! - Write child subsidy in child care cost do loop in the initialize subroutine!!! 
    
    ! OTHERS:
    ! - Try individual pension: 
    !        pen_m(ij, it) = wm * eff(ij) * exp(theta) for men 
    !        pen_f(ij, it) = wf * h(ij) * exp(theta) for women 
    ! - DON'T FORGET TO INCREASE THE GROWTH RATE OF THE GRID WHEN REDUCING THE NUMBER OF GRID POINTS ON NA AND NH
    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! 22/09/2021: 
    ! - WRITE NEW TAX FUNCTION USING PARAMETRIC TAX FUNCTION IN TRAN AND ZAKARIYYA (2021) 
    ! - WRITE STATA CODE TO ESTIMATE THE SCALING AND PROGRESSIVITY PARAMETER OF THE TAX FUNCTION 
    ! - SEE C:\Users\darap\OneDrive\Pheak SP5\ANU Research\DarapheakTin\Paper2\Reference\Tax function for Australia
    ! - Since the tax parameters change over time, we pick the most recent one (tau and lambda in 2018). 
    ! - Also, now total income tax can no longer be simply computed with tauw(it)*w(it)*LL(it). 
    !   We have to compute total tax by looping tax payment in the solve_household() or aggregation() subroutines
    !   by either recalculating (looping) income and tax in the aggregation subroutine, or creating a communication 
    !   labour income tax variable and add it to an aggregate tax revenue (for each t) in every decision loop. 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


    ! 30/09/2021: 
    ! - MODIFY LSRA SUBROUTINE GIVEN THE NEW PARAMETRIC TAX FUNCTION 
    ! - INCTAX becomes negative. We need to scale our model income unit to match the real world, 
    !   or scale the real world down to model units. 
    ! - Try scaling model unit to match with male earnings profile; that is 1 model unit = avg earnigns of male age 20-24
    ! - Or, you can make lambda_tauy endogenous (by solving the govt budget constraint for lambda_tauy that 
    !   clears/balances the budget)

    ! 07/10/2021: 
    ! - Write subroutines for FTB A, FTB B, and CHILDCARE SUBSIDY (cc = (1-s_rate)*childcost0 if y < ybar; else cc = childcost0)
    ! - Try copy the FTB code block from HILDA tax benefit model into this model and modify it to work with fortran. 
    ! - Write separate subroutines for LFS, Newborn Supp, MBA and CE 
    ! - Create a dynamic array for children age in the subroutines. Loop over age and populate ndep integers to be defined in the 
    !   subroutine. Example, if age = 1, integer ndep = something something. 
    ! - Scale the FTB model
    ! - FTB test all market incomes since the income definition used is family adjusted taxable income 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! 10/10/2021: 
    ! - FUNCTIONS CALCULATING FTB_A AND FTB_B ARE DONE 
    ! - NEXT STEP: Integrate FTB_A and FTB_B into the main program (e.g., in solve_household subroutine) 
    !              to study the impact of FTB on the dynamics of household decisions and aggregate macro variables
    ! - CORRECTION: CASE internal module does NOT permit overlapping cases. Need to convert to if-statements.
    !               OR, we can write the CASE statement as the one in CES FUNCTION as shown below: 
    ! EXAMPLE!!!!!!
    !do ic = 1, 5 
    !    select case (childage(ic, ij)) 
    !        case(0:12) 
    !            ndep012 = ndep012 + 1 
    !            ndep017 = ndep017 + 1 
    !        !case(0:17)
    !        !    ndep017 = ndep017 + 1 
    !        case(13:15)
    !            ndep1315 = ndep1315 + 1 
    !            ndep017  = ndep017 + 1 
    !        case(16:17) 
    !            ndep017  = ndep017 + 1
    !            if (at_school_aux) ndep1619_as = ndep1619_as + 1 
    !        !case(16:19)
    !        !    if (at_school_aux) ndep1619_as = ndep1619_as + 1
    !        case(18:19) 
    !            ndep017  = ndep017 + 1 
    !            if (at_school_aux) ndep1619_as = ndep1619_as + 1
    !            if (at_school_aux) ndep1819_as = ndep1819_as + 1 
    !    end select 
    !enddo 

    ! 11/10/2021: 
    ! - the followings have been changed for testing purpose: 
    !   + family income for means testing (currently excluding r(it)*a(ia)) (in both solve_household and aggregation subroutines)
    !   + scale =  5000.6459d0*52
    !   + birth_age(1) = 30 ! must switch back to birth_age(2) 
    !   + outer_tol is set to 10 (i.e., 10% tolerance level for convergence) --> for testing purpose
    !   +++ DAMP is set to update more aggressively (damp = 0.7d0) --> need to reset to smaller 0.3d0
    ! 
    ! - It seems too much FTB resuts in 0 asset accumulation (how to reconcile?) 
    ! - FTB_A is based on the assumption that childcare cost is the highest during teenage years 
    !   --> what if this assumption is wrong? What happens to the excess money supplied to family of teenage kids? 
    ! - Try changing the number of years children spent being dependent. (e.g., get rid of age 20, assume leave home at age 15)
    ! - Implement a switch that switches back and forth between progressive and lump-sum tax 
    ! - Implement Childcare subsidy! 
    ! - should add bequest to income test for FTB? 
    ! - Need a switch logical variable between the lumpsum transfer and FTB scheme 
    ! - What if we use lambda_tauy as the instead of 'scale' to scale model's income to real world unit instead? 
    !   (try setting lambda_tauy to some large value?) 
    
    ! 13/10/2021
    ! - We have implemented Childcare subsidy! But, we have yet to test it. 
    ! 
    ! - HOW ABOUT STOP HERE FOR NOW AND COME BACK LATER? 
    ! - LET'S FOCUS ON PAPER 1. DO THE FOLLOWINGS: 
        ! - ***REFINE FIGURES/TABLES AND ADD APPENDIX STATISTICS/FIGURES IN PAPER 1
        !
        ! - ***MAKE PRESENTATION 30MIN and 3MIN PITCH VERSIONS
        ! 
        ! - ***REVISE PAPER 1, and REWRITE/EDIT AND ADD NEW INTERESTING POINTS INTO IT

    ! 14/10/2021
    ! - Next, is incorporating the FTB, Child Care Subsidy, and Progressive tax schemes into the 
    !   LSRA subroutine. We have done so to an extent, but need to make sure it works properly.

    ! 20/10/2021
    ! - Plot histogramm of welfare gain/loss due to policy changes as done in Tina's (Yurui's) presentation 
    ! - Is there a module for histogram plot? 
    !   --> check: subroutine plot_hist(xvalues, nbins, left, right, incl_outside, absolute, color, legend)

    ! fix seed for random number generating process 
    call set_random_seed(my_seed=694409125) 
    

    ! Set type of economy: 
    !open_capmarket = .false. 
    open_capmarket = .true. 

    ! tax regime
    progressive    = .false. 

    ! transfer regime 
    ftb_on         = .true.

    ! child care subsidy 
    ccs_on         = .true. 

    ! choose endogenous tax
    !tax = 1 ! consumption tax 
    tax = 2 ! tauw and taur 
    !tax = 3 ! tauw 
    !tax = 4 ! taur 
    !tax = 5 ! lumpsum trnasfer 

    ! set updating rules (damp parameters) 
    damp_household = 0.5d0 
    damp_aggregation = 0.5d0 
    damp_LSRA = 0.1d0

    ! tax parameters 
    tauy(0:TT) = 0.073093d0        ! slightly higher progressivity might be desirable (at 0.09d0 --> 40% tax rich) 
                                   !                                     (at 0.1d0 --> 50% tax rich)
    lambda_tauy(0:TT) = 1.879417d0 !1.879417d0 (the higher lambda, the more negative tax function will be)
                                   ! try lower value such as 1.2d0 or 0.9d0 in order to raise tax revenue
   

    ! STEADY STATE SUBROUTINE NEEDS TO BE CHANGED TO ALLOW FOR COMPUTATION OF FINAL STEADY STATE 
    ! solve initial steady state 
    call get_SteadyState(0) 
    call plotting1(0)


    !tauy(1:TT) = 0.1d0 ! raise progressivity of the tax scheme
    !progressive = .false. 
    ftb_on = .false. 

    ! set reform parameter and solve final steady state 
    ! (NOTE: by default, we let tau_w and tau_r balance the govt budget)

    !Tr(1:TT) = 0.05d0*INC(0)
    !Tr(1:TT) = 0.05d0      ! introduce a fixed rate of transfer to parents 
    !tax(1:TT) =  3         ! let tau_w balance the govt budget

    !!!!!!Tr(1:TT) = 0.2d0*INC(0)

    !kappa(1:TT) = 0.3d0    ! change replacement rate of social security
    !tax(1:TT) = 5          ! Let transfer balance the govt budget 
    call get_SteadyState(TT)
    call plotting1(0, TT)  

    ! TRANSITION PATH 
    ! Calculate the transition path without LSRA compensation 
    lsra_on = .false. 
    call get_transition() 
    call plotting1(0, TT) 
    call plotting2()
    
    ! calculate transition path with LSRA 
    lsra_on = .true. 
    call get_transition()
    call plotting1(0, TT) 
    call plotting2()
    
    ! deallocate dynamic arrays and close files 
    deallocate(c, l, aplus, V, comp, phi)
    close(1)
    close(2)

    
    
    ! NOTE: NEED TO CREATE RW_DECISION3 AS IN NISHIYAMA'S TRANSITION SUBROUTINE 
    !       THIS IS TO ADDRESS THE CURRENT MEMORY PROBLEM. 
    !       SOLVE EACH TIME PERIOD, WRITE TO RW_DECISION(it), THEN
    !       READ FROM RW_DECISION(it) TO GET DISTRIBUTION AND AGGREGATE (e.g,. COHORT-SPECIFIC). 
    !       REPEAT THE PROCESS FOR ALL TIME PERIODS.
    


CONTAINS 
    
    SUBROUTINE set_random_seed(my_seed)
        
        implicit none 
        integer, intent(in) :: my_seed 
        integer :: seed_size 
        integer, allocatable :: seed(:) 
        
        call random_seed()               ! initialize using system generated seed
        call random_seed(size=seed_size) ! retrieve seed size and store in seed_size 
        allocate (seed(seed_size))
        
        call random_seed(get=seed)       ! retrieve the generated seed and store in array 'seed'
        write(*, *) seed 
        
        ! set seed to desire value 
        seed = my_seed 
        
        call random_seed(put=seed)       ! set current seed to my_seed 
        call random_seed(get=seed)       ! retrieve the generated seed and store in array 'seed'
        write(*, *) seed                 ! check to see if the newly generated seed = my_seed
    
        deallocate(seed)                 ! deallocate the dynamic array seed once system seed has been successfully planted
    
    END SUBROUTINE 
    
    ! Computes intiial steady state of the economy
    SUBROUTINE get_SteadyState(it_in) 
    
        implicit none 
        integer, intent(in), optional :: it_in
        integer :: iter, it
        real*8  :: deviation 
        integer :: alloc_stat
        real*8 :: local_tol 
        
        it = 0 
        if (present(it_in)) it = it_in 
        
        ! initialize and allocate memory for dynamic arrays at the beginning (init SS) 
        if (it==0) then 
            ! allocate dynamic arrays 
            allocate(phi(0:NA, 0:NH, JPSMAX, 0:TT), stat = alloc_stat) 
            if (alloc_stat /= 0) error stop "phi: not enough memory"
        
            allocate(aplus, c, l, V, comp, mold=phi, stat = alloc_stat) 
            if (alloc_stat /= 0) error stop "aplus, c, l, V, comp: not enough memory"
            
            !comp = 0d0
            !V    = 0d0  ! both initialized in subroutine initialize() 
            call initialize(it)
        else 
            call initialize_transition(it)
        endif 
    
    
        ! start timer 
        call tic() 
        
        ! start iteration 
        do iter = 1, itermax 
        
            ! get good and factor prices 
            call prices(it) 
        
            ! solve household problem given prices (for newborn in the init SS) 
            call solve_household(1, it) 
            
            ! get distribution of households 
            call get_distribution(it) 
            
            ! aggregate individual decisions 
            call aggregation(it) 
            
            ! solve for budget balancing tax/transfer 
            call government(it) 
            
            ! write some main results to console 
            deviation = DIFF(it)/YY(it)
            write(*, '(a, 5x, 8(a,9x), a)') 'iter', 'LFP', 'workpop', 'KK/YY', 'CC/YY', 'II/YY', 'r', 'wm', 'Tr', 'DIFF/YY'
            write(*, '(i4, 8(2x,f10.5), f12.5)') iter, LFP(it), workpop(it), (/mperiod*KK(it), CC(it), II(it)/)/YY(it)*100d0, & 
                            ((1d0+r(it))**(1d0/mperiod)-1d0)*100d0, wm(it), Tr(it), deviation*100d0
            
            
            ! check for convergence 
            if ((it==0 .or. it==TT) .and. (outer_tol>=1d0)) then 
                local_tol = outer_tol*0.5d0 ! set a smaller tolerance level for init and final SS 
            else 
                local_tol = outer_tol
            endif 
            
            if (iter > 1 .and. abs(deviation)*100d0 < local_tol) then 
                call toc() 
                call output(it)
                !deallocate(c, l, aplus, V, comp, phi) 
                return ! return to main program 
            endif
            
        enddo 
            
        ! else, call output, deallocate dynamic arrays, and print error message 
        call toc() 
        call output(it) 
        deallocate(c, l, aplus, V, comp, phi) 
        error stop 'ATTENTION: INIT SS NO CONVERGENCE !!!'
        
    END SUBROUTINE get_SteadyState
    
    
    ! REWRITE GET TRANSITION TO REVOLVE AROUND temp_var 
    ! compute the transition path of the economy 
    SUBROUTINE get_transition() 
    
        implicit none 
        integer :: iter, ij, it, itmax, itcheck, status 
        logical :: check
        real*8  :: local_tol
        
        
        local_tol = outer_tol
        
        ! initialize remaining variables
        ! NOTE: we solve transition without LSRA first
        !       given the result, we then solve transition with LSRA 
        if (.not. lsra_on) then 
            call initialize_transition()
        else 
            write(*,'(a)')'ITER     COMP_OLD    EFFICIENCY        DIFF'
        endif 
        
        ! allocate( temp_var(JJ, 0:NA, 0:NH, NP, NS, NS, 0:TT, 4), STAT = status ) 
        ! if (status/=0) error stop "Not enough memory for temp_var"
        
        ! start timer 
        call tic() 
        
        ! iterate until value function converges 
        do iter = 1, itermax 
            
            ! derive prices 
            do it = 1, TT 
                call prices(it) 
            enddo 
            
            ! solve household problem 
            ! - for all generations, except newborn, in time it=1 
            do ij = JJ, 2, -1 
                write(*, '(a, i3)') 'reform period (it=1), ij = ', ij
                call solve_household(ij, 1) 
            enddo 
            
            ! - for all newborns in time it=1, 2,..., TT 
            do it = 1, TT 
                write(*, '(a, i3)') 'newborn in time it = ', it
                call solve_household(1, it) 
            enddo 
            
            ! calculate the distribution of households over state space
            do it = 1, TT 
                call get_distribution(it) 
            enddo   
            
            ! calculate LSRA resource reallocation (tax/transfer) if lsra_on == .true. 
            ! (NOTE: we need to have solved household problem and computed the 
            !       corresponding distributions before we can call LSRA() subroutine 
            !       to solve for the LSRA's transfers since the output of the two subroutines 
            !       are required inputs for the LSRA transfers)
            if(lsra_on) call LSRA1() 
            
            ! aggregate decisions and derive firm market clearing decisions 
            do it = 1, TT 
                call aggregation(it)
            enddo 
            
            ! solve for government budget balancing tax/transfer 
            do it = 1, TT 
                call government(it) 
            enddo
            
            ! update itcheck if markets clear in a specific time period it 
            ! DIFF(it) is computed in the subroutine: aggregation(it)
            itcheck = 0
            do it = 1, TT 
                if (.not. lsra_on) then 
                    if (abs(DIFF(it)/YY(it))*100d0 < outer_tol) itcheck = itcheck + 1
                    write(*,'(a, i2, a, f12.5)') 'time: ', it, ', DIFF/YY: ', (DIFF(it)/YY(it))*100d0
                else 
                    if (abs(DIFF(it)/YY(it))*100d0 < local_tol) itcheck = itcheck + 1
                    write(*,'(a, i2, a, f12.5)') 'time: ', it, ', DIFF/YY: ', (DIFF(it)/YY(it))*100d0
                endif 
            enddo 
            
            write(*,'(a, i4)') 'itcheck = ', itcheck

            ! index corresponding to time period with max percentage deviation
            itmax = maxloc(abs(DIFF(1:TT)/YY(1:TT)), 1) 
            
            ! check for convergence and write results to console
            IF (.not. lsra_on) THEN 
                check = iter > 1 .and. itcheck>=TT-1 .and. (abs(DIFF(itmax)/YY(itmax))*100d0 < outer_tol*100d0)
            
                write(*, '(a, 5x, 8(a,9x), a)') 'iter', 'LFP', 'workpop', 'KK/YY', 'CC/YY', 'II/YY', 'r', 'wm', 'Tr', &
                                'DIFF/YY (itmax)'
                ! write(*, '(i4, 6f8.2, f12.5)') iter, LFP(TT), workpop(TT), (/KK(TT), CC(TT), II(TT)/)/YY(TT)*100.0, & 
                !                ((1d0+r(TT))**(1d0/mperiod)-1d0)*100d0, wm(TT), DIFF(itmax)/YY(itmax)*100.0
            
                write(*, '(i4, 8(5x,f10.5), f12.5)') iter, LFP(TT), workpop(TT), (/mperiod*KK(TT), CC(TT), II(TT)/)/YY(TT)*100d0, & 
                                ((1d0+r(TT))**(1d0/mperiod)-1d0)*100d0, wm(TT), Tr(TT), DIFF(itmax)/YY(itmax)*100d0
            ELSE
                check = iter > 1 .and. itcheck>=TT-1 & ! TT-2
                        .and. (abs(DIFF(itmax)/YY(itmax))*100d0 < outer_tol*100d0) & !local_tol*100d0
                        .and. (fully_compensated/compensated > 0.99d0) !1d0-(outer_tol/100d0)
                                
                write(*, '(a, 5x, 9(a,11x), a)') 'iter', 'LFP', 'KK/YY', 'CC/YY', 'II/YY', 'r', 'wm', 'Tr', &
                                'EFFICIENCY', 'FULL_COMP', 'DIFF/YY (itmax)'
                write(*, '(i4, 7(5x,f10.5), 3(5x,f12.5))') iter, LFP(TT), (/mperiod*KK(TT), CC(TT), II(TT)/)/YY(TT)*100d0, & 
                                ((1d0+r(TT))**(1d0/mperiod)-1d0)*100d0, wm(TT), Tr(TT), & 
                                (Lambda**(1d0/egam)-1d0)*100d0, fully_compensated/compensated*100d0,  &
                                DIFF(itmax)/YY(itmax)*100d0

            ENDIF  
            
            ! check for convergence 
            if (check) then 
                call toc() 
                do it = 1, TT 
                    if(.not. lsra_on) call output(it) 
                enddo 
                call output_summary()
                return ! return to main 
            endif 
            
        enddo 
        
        ! else, if no convergence
        call toc() 
        do it = 1, TT 
            if(.not. lsra_on) call output(it) 
        enddo 
        call output_summary()
        
        error stop ' ATTENTION: TRANSITION NO CONVERGENCE !!! ' 
        
    END SUBROUTINE get_transition 
    
    
    ! initialize all the remaining variables 
    SUBROUTINE initialize(it_in) 
    
        implicit none 
        integer, intent(in) :: it_in
        integer :: birth_age(5) ! wife's ages when children were born (allow 5 children max)
        REAL*8  :: psi_m(JJ*mperiod+1), psi_f(JJ*mperiod+1), psi_HH(JJ*mperiod+1), ccprice      ! market price of childcare service 
        integer :: it, itm, ic, ij, ia, ih, ip, ism, isf, i

    
        ! Population structure (no lifespan uncertainy)
        ! absolute pop size 
        !do it = 0, TT 
        !    if (it==0) then 
        !        do ij = 1, JJ 
        !            ! pop(1, it=0) = 1 
        !            ! pop(2, it=0) = (1 + n_p)**(-1)
        !            ! pop(3, it=0) = (1 + n_p)**(-2)
        !            ! ....
        !            ! That is, N_{t-j+1}*(1+n_p)**(ij-1) = N_t = 1
        !            pop(ij, 0) = 1d0/(1d0+n_p)**(ij-1)
        !        enddo 
        !    else 
        !        pop(1, it) = (1d0 + n_p)*pop(1, it-1) 
        !        do ij = 2, JJ 
        !            pop(ij, it) = pop(ij-1, it-1) 
        !        enddo 
        !    endif 
        !enddo 
        !
        !! Relative pop size (compared to newborn of the same period) 
        !do it = 0, TT 
        !    do ij = 1, JJ 
        !        m(ij, it) = pop(ij,it)/pop(1,it) 
        !    enddo 
        !enddo 
        
        
        ! Survival probabilities (mperiod=1) 
        !psi = (/1.00000d0, 0.99923d0, 0.99914d0, 0.99914d0, 0.99912d0, &
        !        0.99906d0, 0.99908d0, 0.99906d0, 0.99907d0, 0.99901d0, &
        !        0.99899d0, 0.99896d0, 0.99893d0, 0.99890d0, 0.99887d0, &
        !        0.99886d0, 0.99878d0, 0.99871d0, 0.99862d0, 0.99853d0, &
        !        0.99841d0, 0.99835d0, 0.99819d0, 0.99801d0, 0.99785d0, &
        !        0.99757d0, 0.99735d0, 0.99701d0, 0.99676d0, 0.99650d0, &
        !        0.99614d0, 0.99581d0, 0.99555d0, 0.99503d0, 0.99471d0, &
        !        0.99435d0, 0.99393d0, 0.99343d0, 0.99294d0, 0.99237d0, &
        !        0.99190d0, 0.99137d0, 0.99085d0, 0.99000d0, 0.98871d0, &
        !        0.98871d0, 0.98721d0, 0.98612d0, 0.98462d0, 0.98376d0, &
        !        0.98226d0, 0.98062d0, 0.97908d0, 0.97682d0, 0.97514d0, &
        !        0.97250d0, 0.96925d0, 0.96710d0, 0.96330d0, 0.95965d0, &
        !        0.95619d0, 0.95115d0, 0.94677d0, 0.93987d0, 0.93445d0, &
        !        0.92717d0, 0.91872d0, 0.91006d0, 0.90036d0, 0.88744d0, &
        !        0.87539d0, 0.85936d0, 0.84996d0, 0.82889d0, 0.81469d0, &
        !        0.79705d0, 0.78081d0, 0.76174d0, 0.74195d0, 0.72155d0, &
        !        0.00000d0/)
        
        ! Survival probabilities (mperiod=5)
        !psi = (/1.0000d0, & ! 1
        !        0.9991d0, & ! 2
        !        0.9990d0, & ! 3
        !        0.9989d0, & ! 4
        !        0.9984d0, & ! 5
        !        0.9976d0, & ! 6
        !        0.9961d0, & ! 7
        !        0.9944d0, & ! 8
        !        0.9919d0, & ! 9
        !        0.9887d0, & ! 10
        !        0.9823d0, & ! 11
        !        0.9725d0, & ! 12
        !        0.9562d0, & ! 13
        !        0.9272d0, & ! 14
        !        0.8754d0, & ! 15
        !        0.7971d0, & ! 16
        !        0.0000d0/)  ! 17
        
        ! Survival probabilities at the beginning of each period (mperiod=5)
        !psi = (/1.0000d0, & ! 1
        !        0.9991d0, & ! 2
        !        0.9990d0, & ! 3
        !        0.9989d0, & ! 4
        !        0.9984d0, & ! 5
        !        0.9976d0, & ! 6
        !        0.9897d0, & ! 7
        !        0.9819d0, & ! 8
        !        0.9707d0, & ! 9
        !        0.9553d0, & ! 10
        !        0.9342d0, & ! 11
        !        0.9024d0, & ! 12
        !        0.8365d0, & ! 13
        !        0.7105d0, & ! 14
        !        0.5267d0, & ! 15
        !        0.3118d0, & ! 16
        !        0.0000d0/)  ! 17
        
        ! Male avg 'conditional' survival probabilities (2001-2018)
        ! NOTE: 0.999360323d0 --> age 20 (we set this to 1d0)
        psi_m = (/1.000000000d0, 0.999323905d0, 0.999306798d0, 0.999300718d0, 0.99929148d0, & ! 20-24
            0.999279261d0, 0.999261022d0, 0.999237478d0, 0.999211013d0, 0.99918437d0, & ! 25-29
            0.999154687d0, 0.999123335d0, 0.99908936d0, 0.999051213d0, 0.999010682d0, & ! 30-34
            0.998967588d0, 0.998921692d0, 0.998870194d0, 0.998814404d0, 0.998748302d0, & ! 35-39
            0.998672783d0, 0.998586714d0, 0.998488367d0, 0.99837935d0, 0.998257339d0, & ! 40-44
            0.998122871d0, 0.997974038d0, 0.997807264d0, 0.997630477d0, 0.997436345d0, & ! 45-49
            0.997229815d0, 0.997005403d0, 0.996759772d0, 0.996486783d0, 0.996177912d0, & ! 50-54
            0.995840549d0, 0.995465517d0, 0.995056331d0, 0.994606256d0, 0.994109452d0, & ! 55-59
            0.993558586d0, 0.992945552d0, 0.99227196d0, 0.991530716d0, 0.990713418d0, & ! 60-64
            0.989802897d0, 0.98879528d0, 0.98767966d0, 0.986444056d0, 0.985071421d0, & ! 65-69
            0.983545363d0, 0.981828868d0, 0.979892612d0, 0.977713525d0, 0.975251794d0, & ! 70-74
            0.972477853d0, 0.969345391d0, 0.965793788d0, 0.961768448d0, 0.957220554d0, & ! 75-79
            0.952098191d0, 0.94632268d0, 0.939816475d0, 0.93248117d0, 0.924256742d0, & ! 80-84
            0.915140152d0, 0.905058563d0, 0.894009411d0, 0.881896079d0, 0.868624032d0, & ! 85-89
            0.854069412d0, 0.838445902d0, 0.822482526d0, 0.806728959d0, 0.790238142d0, & ! 90-94
            0.773576796d0, 0.758698165d0, 0.74435091d0, 0.729820728d0, 0.714831591d0, & ! 95-99
            0.000000000d0/) ! 0.698223233d0 --> age 100

        ! Female avg 'conditional' survival probabilities (2001-2018)
        ! NOTE: 0.999735773d0 --> age 20 (we set this to 1d0)
        psi_f = (/1.000000000d0, 0.999731779d0, 0.999732912d0, 0.999733806d0, 0.999731362d0, & ! 20-24
            0.999724388d0, 0.99971503d0, 0.999701917d0, 0.999690175d0, 0.999673724d0, & ! 25-29
            0.99965471d0, 0.99963057d0, 0.999604344d0, 0.999575913d0, 0.999546409d0, & ! 30-34
            0.999511063d0, 0.999472201d0, 0.99943161d0, 0.999383867d0, 0.999328434d0, &  ! 35-39
            0.999269962d0, 0.999204278d0, 0.999135494d0, 0.999055922d0, 0.998967826d0, & ! 40-44
            0.99887538d0, 0.998775065d0, 0.998667419d0, 0.998550832d0, 0.998424053d0, & ! 45-49
            0.998290122d0, 0.998150051d0, 0.997995734d0, 0.997832358d0, 0.997657001d0, & ! 50-54
            0.997462988d0, 0.99725157d0 ,0.997016728d0, 0.996753275d0, 0.996456385d0, & ! 55-59
            0.99613142d0 ,0.995769382d0 ,0.995376766d0, 0.994955242d0, 0.994503856d0, & ! 60-64
            0.994008064d0 ,0.993454516d0 ,0.992825329d0, 0.992105603d0, 0.991287708d0, & ! 65-69
            0.990355611d0, 0.989275873d0, 0.988033712d0, 0.986650288d0, 0.985088646d0, & ! 70-74
            0.983319461d0, 0.98129648d0, 0.978973448d0, 0.976287663d0, 0.973166227d0, & ! 75-79
            0.969548166d0, 0.965325236d0, 0.960414946d0, 0.95474118d0, 0.948243976d0, & ! 80-84
            0.940823495d0, 0.932442188d0, 0.922986746d0, 0.912326753d0, 0.900359869d0, & ! 85-89
            0.886793792d0, 0.87197876d0, 0.856175363d0, 0.839429557d0, 0.821781933d0, & ! 90-94
            0.803484678d0, 0.786901653d0, 0.768653512d0, 0.752880096d0, 0.737356246d0, & ! 95-99
            0.000000000d0/) ! 0.721411169d0 --> age 100
                                                                                     
        ! Male and female's joint (household's) avg 'conditional' survival probabilities (2001-2018)
        psi_HH(:) = psi_m(:) ! for the basic model, we set household survival probabilities to match those of male
        ! psi_HH(:) = phi_m(:) * psi_f(:)

        ! Household avg 'conditional' survival probabilities adjusted for model period
        psi = (/(psi_HH(mperiod*i+1), i = 0, JJ)/)
        if (psi(JJ+1) /= 0d0) error stop "incorrect survival probabilities"

        
        ! POPULATION STRUCTURE (WITH LIFESPAN UNCERTAINTY)
        do it = 0, TT 
            ! mass of alive agents age ij in time it 
            ! note: assume psi and n_p are time-invariant for the basic model
            m(1, it) = 1d0 
            pop(it)  = 1d0
            do ij = 2, JJ 
                m(ij, it) = m(ij-1, it)*psi(ij)/(1d0+n_p) 
                pop(it) = pop(it) + m(ij,it)
            enddo 
            
            ! share of agents age ij alive in time it (relative to total pop size) 
            m(:, it) = m(:, it)/pop(it)
            
            ! population size 
            pop(it) = pop(it)*(1d0+n_p)**(it) 
            
            ! mass of population still alive 
            m_lev(:, it) = m(:,it) * pop(it)
        enddo 
        
        
                
        ! Bequest distribution (to all working-age agents)
        beq_dist = 0d0 
        beq_dist(1:JR-1) = 1d0/dble(JR-1)  ! assume uniform distribution of bequest to each cohort ij 
        
        do it = 0, TT 
            beq_share(1, it) = beq_dist(1) 
        
            ! total bequest received per dollar bequeathed in time it 
            do ij = 2, JJ 
                beq_share(1, it) = beq_share(1, it) + beq_dist(ij)*m_lev(ij, it)  !!!!!!!!!!!!!!!!
            enddo 
            
            ! share of bequest to cohort ij in time it 
            do ij = JJ, 1, -1 
                beq_share(ij, it) = beq_dist(ij)/beq_share(1,it) 
            enddo 
        enddo 
        
        
        ! Average male labour hours by cohort (2001-2018, HILDA)
        lab1 = 0d0 
        lab1(1:JR-1) = (/0.3262d0, 0.35462d0, 0.36782d0, 0.37059d0, 0.36828d0, 0.36623d0, 0.36147d0, 0.34928d0/)

        ! Average female labour hours by cohort (2001-2008, HILDA) 
        lab2 = 0d0 
        lab2(1:JR-1) = (/0.27746d0, 0.29805d0, 0.27826d0, 0.26095d0, 0.26773d0, 0.27873d0, 0.28261d0, 0.27514d0/) 
        
        
        
        ! Age-specific productivity level for male (mperiod = 1) 
        !eff(1:JR-1) = (/1.0000d0, 1.0719d0, 1.1438d0, 1.2158d0, 1.2842d0, 1.3527d0, &
        !                1.4212d0, 1.4897d0, 1.5582d0, 1.6267d0, 1.6952d0, 1.7217d0, &
        !                1.7438d0, 1.7748d0, 1.8014d0, 1.8279d0, 1.8545d0, 1.8810d0, &
        !                1.9075d0, 1.9341d0, 1.9606d0, 1.9623d0, 1.9640d0, 1.9658d0, &
        !                1.9675d0, 1.9692d0, 1.9709d0, 1.9726d0, 1.9743d0, 1.9760d0, &
        !                1.9777d0, 1.9700d0, 1.9623d0, 1.9546d0, 1.9469d0, 1.9392d0, &
        !                1.9315d0, 1.9238d0, 1.9161d0, 1.9084d0, 1.9007d0, 1.8354d0, &
        !                1.7701d0, 1.7048d0/)
        !
        !eff(JR:JJ) = 0d0
    
        
        ! Age-specific productivity level for male (mperiod = 5)
        !eff(1) = 1.0000d0
        !eff(2) = 1.3527d0
        !eff(3) = 1.6952d0
        !eff(4) = 1.8279d0
        !eff(5) = 1.9606d0
        !eff(6) = 1.9692d0
        !!eff(7) = 1.9692d0
        !eff(7) = 1.9392d0
        !eff(8) = 1.9007d0
        !eff(JR:JJ) = 0d0
        
        ! Average (2001-2018) age-specific productivity level for male in Australia (mperiod=5) over all skill and marital types 
        ! (NOTE: we use predicted wages from all jobs performed within a year via 
        !        the OLS regression: logwage = cons + age + age^2 + i.gender + i.wave)
        
        ! METHOD 1: Normalized against workers age 20-65 annual average earnings
        !eff(1) = 0.89575
        !eff(2) = 1.10055
        !eff(3) = 1.29042
        !eff(4) = 1.43368
        !eff(5) = 1.50904
        !eff(6) = 1.50475
        !eff(7) = 1.42225
        !eff(8) = 1.27664
        !eff(JR:JJ) = 0d0  
        
        ! METHOD 2: Normalized against the average annual earnings of the first cohort (age 20-24) 
        eff(1) = 1.00000d0
        eff(2) = 1.22811d0
        eff(3) = 1.44047d0
        eff(4) = 1.60149d0
        eff(5) = 1.68558d0
        eff(6) = 1.68055d0
        eff(7) = 1.58755d0
        eff(8) = 1.42400d0
        eff(JR:JJ) = 0d0

        
        ! ASSET GRID 
        call grid_grow(a, a_l, a_u, a_grow)
        
        write(*, *) 'A GRID', a 
        
        ! get initial guess for savings decision (for infinite horizon problem)
        !do ij = 1, JJ 
        !    do ih = 0, NH 
        !        do ip = 1, NP 
        !            do ism = 1, NS 
        !                do isf = 1, NS 
        !                    aplus(ij, :, ih, ip, ism, isf, 0) = max(a(:)/2d0, a(1)/2d0)
        !                enddo 
        !            enddo
        !        enddo
        !    enddo
        !enddo 
        
        ! HUMAN CAPITAL GRID 
        ! get lower bound and upper bound for constructing h's grid
        h_l = 0d0  ! lower bound 
        h_max(1) = 0d0 ! newborn's human capital level = 0 
        do ij = 2, JR-1
            ! note that h := log(h)
            h_max(ij) = h_max(ij-1) + (xi(1) + xi(2)*(ij-1))
        enddo
        h_max(JR:JJ) = h_max(JR-1)
        h_u = maxval(h_max) ! upper bound of h is the greatest possible h
        
        ! construct h's grid 
        call grid_grow(h, h_l, h_u, h_grow)
        
        write (*, *) 'H MAX', h_u 
        write (*, *) 'H GRID:', (/(h(i), i=0, NH)/)
        
        
        
        ! SHOCK PROCESSES 
        ! permanent productivity shocks (Education/skill type) 
        dist_theta = 1d0/real(NP,kind=8) 
        theta(1) = -sqrt(sig2_theta) 
        theta(2) =  sqrt(sig2_theta) 
        theta    = exp(theta) 
        
        ! Transitory shocks 
        call rouwenhorst_method(rho, 0d0, sig2_eps, eta, pi, prob) 
        eta = exp(eta) 
        
        
        ! FACTOR PRICES, TAX, AND SOCIAL SECURITY 
        ! age and rental rate 
        r = 0.04 
        wm = 1d0
        wf = wm
        
        ! Tax and Transfer 
        !tax = 2 ! endogenous tax: tauw and taur
        !tax = 3 ! endogenous income tax: scaling parameter (lamda_tauy)
        !tax = 5  ! set Tr as the budget balancing transfer
        tauc = 0.075d0
        !tax = 4 ! endogenous capital earnings tax (taur)
        !tax  = 1 
        !tauw = 0.2d0
        !taur = 0.3d0 ! or 0.4d0 
        tauw = 0.2d0 !
        if (progressive) tauw = 0d0 
        INCTAX = 0d0 

        taur = 0.4d0 !
        Tr   = 0d0 ! initial guess: no family transfer in the init steady state 
        !Tr   = 0.5d0*INC(0) 
        
        gy   = 0.19d0         ! changed fromi 0.19 t0 0.15 for aussie
        by   = 0.60d0/mperiod  ! changed from 0.6 (US) to 0.4 (AUSTRALIA) 
        
        ! Social Security 
        taup = 0.10d0
        !kappa = 0d0
        kappa = 0.50d0
        pen = 0d0
        pen(JR:JJ, it_in) = kappa(it_in) * sum(wm(it_in) * eff)/real(JR-1,kind=8) !
        
        
        ! CHILDREN
        birth_age = 0 
        !birth_age(1:2) = (/26, 28/) 
        
        !birth_age(1:3) = (/28, 32, 36/) 
        
        !birth_age(1:2) = (/25, 30/) 
        !birth_age(1)  = 25  
        birth_age(1:2) = (/20, 25/) 
        nchild = 0
        childage(:,:) = -1 ! we set default age to -1 to avoid conflicting with age 0  
        cchild = 0d0
        ccprice = exp(h_max(3)) 
        firstchild = 0 
        !ccprice = exp(h_max(5))   
        
        do ic = 1, size(birth_age)
            
            ! employ the if statement below to avoid factoring in the case of birth_age = 0 (no child birth)
            if (birth_age(ic) > 20 .and. birth_age(ic) < 50) then 
                ! corresponding model age
                !ij = birth_age(ic) - 20
            
                ij = (birth_age(ic)-20)/mperiod + 1
                
                ! Add 1 to the number of dependent children from age ij to ij+17
                ! following the assumption that 
                ! 1. Dependent children live with parents until age 18 
                ! 2. Child's age = 1 at birth 
                !nchild(ij:ij+17) = nchild(ij:ij+17) + 1
                
                nchild(ij:ij+3) = nchild(ij:ij+3) + 1

                childage(ic, ij:ij+3) = (/0, 5, 10, 15/)
                if (ic==1) firstchild(ij) = 1 ! mark the birth of the first child
            
                ! Set cost of childcare between ij and ij + 17
                ! - assume childcare expenditure is concave w.r.t. age  
                !cchild(ij:ij+2)     = cchild(ij:ij+2)       + 0.60 * ccprice ! 1-3 years old 
                !cchild(ij+3:ij+5)   = cchild(ij+3:ij+5)     + 0.80 * ccprice ! 4-6 years old 
                !cchild(ij+6:ij+11)  = cchild(ij+6:ij+11)    + 1.00 * ccprice ! 7-12 years old 
                !cchild(ij+12:ij+17) = cchild(ij+12:ij+17)   + 0.40 * ccprice ! 13-18 years old 
                
                cchild(ij)     = cchild(ij)       + 0.80d0 * ccprice ! 1-3 years old 
                cchild(ij+1)   = cchild(ij+1)     + 1.00d0 * ccprice ! 4-6 years old 
                cchild(ij+2)   = cchild(ij+2)     + 0.80d0 * ccprice ! 7-12 years old 
                cchild(ij+3)   = cchild(ij+3)     + 0.60d0 * ccprice ! 13-18 years old 
            
            endif 
        enddo 
        !cchild(:) = 0d0 ! turn off childcare cost


        ! FTB Parameters (2018)
        !////////////! 
        ! FTB Part A ! 
        !////////////! 
        ! MAX RATE INCLUDING ANNUAL FTB A SUPPLEMENT (ADJUSTMENT IS MADE AT THE END OF THE CALCULATION IF NOT ELIGIBLE)
        FTBA_Max(18,:) =  (/5504.20d0, 6938.65d0, 0d0, 0d0, 6938.65d0, 0d0, 0d0/)
        
        ! BASE RATE INCLUDING ANNUAL FTB A SUPPLEMENT (ADJUSTMENT IS MADE AT THE END OF THE CALCULATION IF NOT ELIGIBLE)
        FTBA_Base(18,:)   =  (/2266.65d0, 0d0, 2266.65d0, 0d0/)
        
        ! THRESHOLDS  
        FTBA_T(18,:)    = (/52706d0, 94316d0/)  ! thresholds for max and base rates respectively
        FTBA_T2A(18)   = 0d0                    ! additional base payment threshold adjustment per qualifying child
        !FTBA_T2A2(18)  = 0d0  ! to be calculated in the subroutine 

        ! LARGE FAMILY SUPPLEMENTS INCLUDING ANNUAL FTB A SUPPLEMENT (ADJUSTMENT IS MADE AT THE END OF THE CALCULATION IF NOT ELIGIBLE)
        FTBA_S1(18)    = 0d0 
        FTBA_C1(18)    = 1    ! Eligible from first child onwards 

        ! RENT ASSISTANCE (NOT APPLICABLE IN THIS MODEL)
        FTBA_TH1A(18)   =         4102.28d0  ! Minimum rent to qualify for rent assistance if lone parent 
        FTBA_TH1B(18)   =         6071.52d0  ! Minimum rent to qualify for rent assistance if partnered parent
        FTBA_TH2A(18)   =         4116.84d0  ! Maximum rent assistance if dependent children <= 2
        FTBA_TH2B(18)   =         4648.28d0  ! Maximum rent assistance if dependent children >= 3 
        FTBA_RPCPI(18)  =          112.20d0  ! Rent price CPI
        FTBA_RM(18)     =             .99d0  ! Rent multiplier 

        ! TAPER/WITHDRAW RATE OF FTB A
        FTBA_W(18,:)      = (/0.20d0, 0.30d0/) 
    
        ! NEWBORN SUPPLEMENTS 
        FTBA_NS(18,:)      = (/2158.89d0, 1080.54d0/)

        ! CLEAN ENERGY SUPPLEMENT 
        FTBA_CE(18,:)     = (/36.50d0, 91.25d0, 116.80d0/)

        ! MULTIPLE BIRTH ALLOWANCE
        FTBA_MBA1(18)   =  4044.20d0 ! Allowance for triplet  
        FTBA_MBA2(18)   =  5387.40d0 ! Allowance for quadruplet or more 
        FTBA_MAGE(18)   = 16         ! Max children age to qualify 
        FTBA_MAGES(18)  = 18         ! Max children age to qualify if children at school    

        ! ANNUAL FTBA SUPPLEMENT ADJUSTMENT 
        FTBA_AS(18)     = 737.30d0
        FTBA_FT1(18)    = 80000d0


        !////////////! 
        ! FTB Part B ! 
        !////////////! 

        ! MAX RATE
        FTBB_Max(18,:)   = (/4412.85d0, 3190.10d0/) ! FTB_R8 and FTB_R9 in HILDA tax benefit model, respectively

        ! Threshold for primary and secondary earner 
        FTBB_T(18,:)     = (/100000d0, 5548d0/) 
        FTBB_W(18)    = 0.20d0
        FTBB_CE(18,:)    = (/73.00d0, 51.10d0/) 


        !///////////////////////////////////!
        !///// Child Care Subsidy Rate /////!
        !///////////////////////////////////!

        ! Thresholds 
        CCS_T(pyear, :) = (/70015d0, 175015d0, 254305d0, 344305d0, 354305d0/)

        ! Subsidy rates
        CCS_R(pyear, :) = (/0.85d0, 0.5d0, 0.2d0, 0d0/)

        !/////////////////////////////////////
        ! OPTIMAL UTILITY 
        V(:,:,:,:)    = 0d0 
        
        ! BEQUEST 
        bequest(:, 0) = 0d0 
        BQ(0) = 0d0 
        
        ! LSRA COMPENSATION 
        comp(:,:,:,:) = 0d0 
        
        ! initial guess of macro variables 
        KK = 1d0; LL = 1d0
        !LLM = 0.5d0 
        !LLF = LL - LLM
        YY = tfp * KK**alpha * LL**(1d0-alpha) 
        II = (n_p + delta) * KK ! steady state investment 
        
        GG = gy*YY(0)
        BB = by*YY(0)
        
        BF = 0d0 
        ITB = 0d0 
        
        ! open file
        if (it_in==0) then 
            open(1, file='model1_output.out', status="replace")
            open(2, file='model1_summary.out', status="replace")
        endif 
        
    END SUBROUTINE initialize 
    
    
    
    ! Find mean 
    FUNCTION mean(x) 
    
        implicit none 
        real*8, intent(in) :: x(:)
        real*8 :: mean
        integer :: i, sizex

        ! using size(x) directly means including empty array values
        ! thus, we have to check each element individually first
        sizex = 0 
        do i = 1, size(x) 
            if (x(i) > 1d-30) sizex = sizex + 1
        enddo 

        !print*, sizex
        mean = sum(x)/dble(max(sizex, 1))
    
    END FUNCTION mean 
    
    
    SUBROUTINE initialize_transition(it_in) 
    
        implicit none 
        integer, intent(in), optional :: it_in
        integer :: ij, it 
        logical :: same_it
        
        write(*, '(/a/)')'TRANSITION PATH' 
        
        write(*,'(a)')'ITER       H     K/Y     C/Y     I/Y       r       w        DIFF'
    
        !comp(:, :, :, :) = 0d0 
        
        ! initialize for every period 
        do it = 1, TT
            
            ! select only the time period of interest (it_in)
            ! if not solving for the entire transition path
            !if (present(it_in) .and. it /= it_in) CYCLE 
            
            ! uncomment this block for basic fortran without IMSL library 
            if(present(it_in)) then
                same_it = (it == it_in)
                if (.not. same_it) CYCLE 
            endif 

            
            ! Tax system (depends on the choiceo of endogenous tax)
            select case (tax(it)) 
            case (1);     tauc(it) = mean(tauc((/0,TT/)))
            case (2);     
                          tauw(it) = mean(tauw((/0,TT/)))
                          if (progressive) tauw(it) = 0d0 
                          taur(it) = tauw(it) 
            case (3);     
                          tauw(it) = mean(tauw((/0,TT/)))
                          if (progressive) tauw(it) = 0d0 
            case (4);     taur(it) = mean(taur((/0,TT/)))
            case (5);     Tr(it) = mean(Tr((/0,TT/)))
            case default; error stop "Tax: Unrecognized value. Admissible input: 1-5."
            end select 
            
            ! social security system 
            taup(it)   = mean(taup((/0,TT/)))
            pen(:, it) = sum(pen(:, (/0,TT/)),dim=2)/2 
            PP(it)     = mean(PP((/0,TT/)))
            
            ! Revenue 
            taxrev(:, it) = sum(taxrev(:, (/0,TT/)),dim=2)/2
            
            ! Prices and net prices of goods and factors 
            p(it)    = 1d0 + tauc(it)
            
            wm(it)   = mean(wm((/0,TT/)))
            wf(it)   = mean(wf((/0,TT/)))
            wn_m(it) = (1d0 - tauw(it) - taup(it)) * wm(it) 
            wn_f(it) = (1d0 - tauw(it) - taup(it)) * wf(it) 
            
            r(it)    = mean(r((/0,TT/)))
            rn(it)   = (1d0 - taur(it))*r(it) 
            
            
            ! Household decisions, value, and density  
            aplus(:, :, :, it) = sum(aplus(:, :, :, (/0,TT/)),dim=4)/2
            c(:, :, :, it)     = sum(c(:, :, :, (/0,TT/)),dim=4)/2
            l(:, :, :, it)     = sum(l(:, :, :, (/0,TT/)),dim=4)/2
            V(:, :, :, it)     = sum(V(:, :, :, (/0,TT/)),dim=4)/2 
            phi(:, :, :, it)   = sum(phi(:, :, :, (/0,TT/)),dim=4)/2 

        
            ! Cohort specific aggregates 
            a_coh(:, it) = sum(a_coh(:, (/0,TT/)),dim=2)/2
            c_coh(:, it) = sum(c_coh(:, (/0,TT/)),dim=2)/2 
            l_coh(:, it) = sum(l_coh(:, (/0,TT/)),dim=2)/2 
            !h_coh(:, it) = sum(h_coh(:, (/0,TT/))) 
            ym_coh(:, it) = sum(ym_coh(:, (/0,TT/)),dim=2)/2 
            yf_coh(:, it) = sum(yf_coh(:, (/0,TT/)),dim=2)/2
            y_coh(:, it) = sum(y_coh(:, (/0,TT/)),dim=2)/2
            
            ! Bequest 
            bequest(:, it) = sum(bequest(:, (/0,TT/)), dim=2)/2 
            BQ(it)         = mean(BQ((/0,TT/)))
            
            ! Macro variables 
            KK(it)  = mean(KK((/0,TT/)))
            LL(it)  = mean(LL((/0,TT/))) ! labour in efficiency unit
            LLM(it) = mean(LLM((/0,TT/))) 
            LLF(it) = mean(LLF((/0,TT/)))
            !LFP(it) =mean( LFP(0) 
            HH(it)  = mean(HH((/0,TT/))) ! human capital  
            YY(it)  = mean(YY((/0,TT/))) 
            AA(it)  = mean(AA((/0,TT/))) 
            CC(it)  = mean(CC((/0,TT/))) 
            II(it)  = mean(II((/0,TT/))) 
            GG(it)  = mean(GG((/0,TT/))) 
            BB(it)  = mean(BB((/0,TT/))) ! govt's borrowing
            BF(it)  = mean(BF((/0,TT/)))
            ITB(it) = mean(ITB((/0,TT/))) 
            INC(it) = mean(INC((/0,TT/))) 
            
            ! International Capital Trade (open_capmarket==.true.)
            
            write(*,'(a)') "Initialized Values for TT"
            write(*,'(13f9.3)') wm(it), wf(it), r(it), KK(it), LL(it), HH(it), YY(it), AA(it), &
                                CC(it), II(it), GG(it), BB(it), INC(it)
        enddo 

    END SUBROUTINE initialize_transition
    
    
    
    ! Subroutine for calculating prices (to be fed into the solve_household subroutine) 
    SUBROUTINE prices(it) 
    
        implicit none
        integer, intent(in) :: it
        !logical, optional, intent(in) :: open_capmarket ! small open capital market
        !logical :: is_open 

        !is_open = .false. ! default to closed economy 
        !if (present(open_capmarket)) is_open = open_capmarket 
        
        ! gross rental and wage rates
        if (.not. open_capmarket) then 
            r(it) = alpha * TFP * (KK(it)/LL(it))**(alpha-1d0) - delta
            ! add zero bound to interest rate 
            !r(it) = max(r(it), 1d-10)
        else 
            r(it) = r(0) 
        endif 
        
        wm(it) = (1d0-alpha) * TFP * (KK(it)/LL(it))**alpha 
        wf(it) = wm(it) 
        
        ! net rental and wage rates (of taxes) 
        rn(it) = (1d0 - taur(it)) * r(it) 
        wn_m(it) = (1d0 - tauw(it) - taup(it)) * wm(it) 
        wn_f(it) = (1d0 - tauw(it) - taup(it)) * wf(it) 
        
        ! consumer price 
        p(it) = 1d0 + tauc(it)
        
    END SUBROUTINE prices 
    
    
    
    ! Determines the solution to the household optimization problem 
    ! conditional on female labour supply decision (il = 0,..., l_max) 
    SUBROUTINE solve_household(ij_in, it_in) 
    
        implicit none 
        integer, intent(in) :: ij_in, it_in 
        integer :: ij, ia, ih, ip, ism, isf, il, it, itp, ijps1, ijps2, ijps, ijps_p
        real*8 :: linc_m, linc_f, income_tax, transfer, available, sr
        real*8 :: a_in, V_val
        ! temp variables 
        integer :: l_star
        real*8 :: c_temp(0:L_max), aplus_temp(0:L_max), V_temp(0:L_max)
        ! check max asset and human capital grids used by household
        integer :: ia_max(JJ,0:TT), ih_max(JJ,0:TT) 
        ! custom damping parameter 
        real*8 :: damp 
        logical :: breakpoint_a
        
        breakpoint_a = .false. 
        
        ! damping parameter 
        damp = damp_household ! 0.3d0 
        
        ! year age ij_in household in time it_in reaches age JJ 
        it = year(it_in, ij_in, JJ) 
        
        ! bequest for the olds 
        bequest(JJ, it) = damp*beq_share(JJ, it)*BQ(it) + (1d0-damp)*bequest(JJ, it)
        
        ! solve household problem age JJ 
        ijps1 = indexing(JJ, 1, 1, 1)   
        ijps2 = indexing(JJ, NP, NS, NS)
        !ijps2 = indexing(JJ+1, 1, 1, 1) - 1 
        do ia = 0, NA 
            aplus(ia, :, ijps1:ijps2, it) = 0d0 
            c(ia, :, ijps1:ijps2, it) = ((1d0+rn(it))*a(ia) + pen(JJ, it) & 
                                        + bequest(JJ,it) & ! + Tr(it)*nchild(JJ)
                                        + comp(ia, :, ijps1:ijps2, it))/p(it) ! 
            if (progressive) c(ia, :, ijps1:ijps2, it) = c(ia, :, ijps1:ijps2, it) - inctaxfunc(a(ia),0d0,0d0,it) 
            if (nchild(JJ) > 0 .and. ftb_on) c(ia, :, ijps1:ijps2, it)  = c(ia, :, ijps1:ijps2, it) & 
                                                                        + FTB_A(JJ, 0d0) + FTB_B(JJ, 0d0, 0d0)
            if (nchild(JJ) > 0 .and. (.not. ftb_on)) c(ia, :, ijps1:ijps2, it) = c(ia, :, ijps1:ijps2, it) + Tr(it)*nchild(JJ)

            l(ia, :, ijps1:ijps2, it) = 0d0 
            V(ia, :, ijps1:ijps2, it) = valuefunc(0d0, c(ia,0,ijps1,it), 0, JJ, 0, 1, 1, 1)  
        enddo 
        
    
        ! interpolate to get future EV and RHS (at age JJ) for solving period JJ-1 problem
        call interpolate(JJ, it) 
        
        ! backward iteration until age ij_in 
        do ij = JJ-1, ij_in, -1 
            
            ! year age ij_in household in time it_in becomes age ij 
            it  = year(it_in, ij_in, ij)  ! if it_in==0, the function year returns it=0 (init steady state) 
            ! next year itp given current year = it
            itp = year(it, 1, 2)          ! if it==0, the function year returns itp=0 as well 
            
            ! reindexing 
            ijps1 = indexing(ij, 1, 1, 1)   
            ijps2 = indexing(ij, NP, NS, NS)
            !ijps2 = indexing(ij+1, 1, 1, 1) - 1  ! alternative 
                
            ! bequest update 
            bequest(ij, it) = damp*beq_share(ij, it)*BQ(it) + (1d0-damp)*bequest(ij, it) 
            
            ! a_max 
            a_max(:) = 0d0 ! initialize upper bound on household savings 
            
        
            do ia = 0, NA
                
                
                ! decision for retiress without assets and pension 
                !if (ij >= JR .and. ia==0 .and. pen(ij,it)<=1d-10) then 
                if (ij >= JR .and. ia==0 .and. kappa(it) <= 1d-10) then 
                    aplus(ia, :, ijps1:ijps2, it) = 0d0 
                    c(ia, :, ijps1:ijps2, it)     = 0d0 
                    l(ia, :, ijps1:ijps2, it)     = 0d0 
                    V(ia, :, ijps1:ijps2, it)     = valuefunc(0d0, 0d0, 0, ij, 0, 1, 1, 1) 
                    CYCLE ! proceed to next loop
                endif 
                
                do ih = 0, NH; if (ij>=JR .and. ih>0) exit 
                    ! check whether h(ih) is out of bound (> h_max(ij) 
                    ! h is out of bound --> copy decision rule from ih-1
                    ! (we can also set h(ih) = h_max(ij))
                    if (h(ih) > h_max(ij)) then
                        aplus(ia, ih, ijps1:ijps2, it) = aplus(ia, ih-1, ijps1:ijps2, it)
                        c(ia, ih, ijps1:ijps2, it) = c(ia, ih-1, ijps1:ijps2, it)
                        l(ia, ih, ijps1:ijps2, it) = l(ia, ih-1, ijps1:ijps2, it)
                        V(ia, ih, ijps1:ijps2, it) = V(ia, ih-1, ijps1:ijps2, it)
                        CYCLE 
                    endif 
                    
                    do ip = 1, NP; if (ij>=JR .and. ip>1) exit 
                        do ism = 1, NS; if (ij>=JR .and. ism>1) exit 
                            do isf = 1, NS; if (ij>=JR .and. isf>1) exit 
                                
                                ! reindexing for the specific ij, ip, ism, isf
                                ijps   = indexing(ij, ip, ism, isf) 
                                ijps_p = indexing(ij+1, ip, ism, isf) 
                                    
                                do il = 0, L_max
                                    
                                    if (ij>=JR .and. il>0) then 
                                        V_temp(1:L_max) = -1d100 
                                        exit 
                                    endif 
                                    
                                    ! guess a_in = aplus from ij+1 
                                    ! select case (it) 
                                    ! case (0, TT);  a_in = aplus(ij+1, ia, ih, ip, ism, isf, it)  ! steady states
                                    ! case (1:TT-1); a_in = aplus(ij+1, ia, ih, ip, ism, isf, it+1) ! transition path
                                    ! end select 
                                    
                                    ! NOTE: itp = 0 when it = 0 (see function year for more info) 
                                    a_in = aplus(ia, ih, ijps_p, itp)
                                    
                                    ! find the upper bound of a 
                                    !linc_m = wn_m(it) * lab1(ij)* eff(ij) * theta(ip) * eta(ism) 
                                    !linc_f = wn_f(it) * lab2(ij)* exp(h(ih)) * theta(ip) * eta(isf) 
                                    !a_max(ij) = ((1d0+rn(it))*a(ia) & 
                                    !            + linc_m + abs(real(il,kind=8)>0d0)*(linc_f - wf(it)*lab2(ij)*cchild(ij)) &
                                    !            + abs(real(nchild(ij),kind=8)>0d0)*Tr(it)*nchild(ij) + pen(ij, it) & 
                                    !            + bequest(ij, it) & 
                                    !            + comp(ia, ih, ijps, it))/p(it)
                                    
                                    ! uncomment the code block below if using standard fortran compiler 
                                    linc_m = wm(it) * lab1(ij)* eff(ij) * theta(ip) * eta(ism) 
                                    linc_f = wf(it) * lab2(ij)* exp(h(ih)) * theta(ip) * eta(isf) 

                                    ! tax 
                                    if (progressive) then 
                                        income_tax = inctaxfunc(a(ia), linc_m, linc_f, it) 
                                    else 
                                        income_tax = tauw(it)*(linc_m + linc_f)
                                    endif 

                                    ! transfer 
                                    if (ftb_on) then 
                                        transfer = FTB_A(ij, linc_m + linc_f) + FTB_B(ij, linc_m, linc_f)
                                    else 
                                        transfer = Tr(it) * nchild(ij) 
                                    endif 

                                    ! childcare subsidy 
                                    sr = 0d0 
                                    if (ccs_on) sr = CCS_rate(ij, linc_m + linc_f, lab1(ij), lab2(ij))

                                    ! maximum asset
                                    a_max(ij) = (1d0+rn(it))*a(ia) & 
                                                + linc_m + real(il,kind=8)*(linc_f - (1d0-sr)*wf(it)*lab2(ij)*cchild(ij)) & 
                                                + pen(ij, it) & 
                                                + bequest(ij, it) & 
                                                + comp(ia, ih, ijps, it) & 
                                                - income_tax & 
                                                - ptaxfunc(linc_m, linc_f, it) 
                                                
                                    if (nchild(ij)>0) a_max(ij) = a_max(ij) + transfer 
                                    
                                    ! communication variables with valuefunc 
                                    it_com = it;    ij_com = ij;    ia_com = ia;    ih_com = ih;
                                    ip_com = ip;    ism_com = ism;  isf_com = isf;  il_com = il; 
                                    ijps_com = ijps; 
                                    
                                    ! solve the household problem (using fminsearch for now)
                                    ! (NOTE***: need to test the imsl subroutine BCONF)
                                    call fminsearch(a_in, V_val, a_l, min(a_max(ij), a_u), bellman_equation) 
                                    ! TEST THE FUNCTION BCONF()
                                    
                                    ! stop "a_u is too small!"
                                    !if (.not. lsra_on) then 
                                        if (a_max(ij) >= a_u .and. ia<NA-3) then 
                                            write(*, '(a, i3, 2(a, f12.5))') "a_max(",ij,") = ", a_max(ij), "  a_u = ", a_u
                                            !error stop 9 ! return code 9 when a_u is too small
                                        endif 
                                    !endif 

                                    !if (a_max(ij) >= a_u)  stop "a_u is TOO SMALL!"
                                    !if (a_max(ij) >= a_u .and. ia/=NA) breakpoint_a = .true.
                                    
                                    !else 
                                    !    write(*, '(a, i3, a, f12.5)') "a_max(",ij,") = ", a_max(ij)
                                    
                                    !if (a_max(ij) > a_u) error stop "a_u is too small!"
                                    
                                    ! this is redundant! 
                                    if (a_in < a_l) then 
                                        a_in = a_l
                                        cons_com = a_max(ij)
                                        ! V_val = -(valuefunc(a_in, cons_com, il, ij, ih, ip, ism, isf))
                                    endif 
                                    
                                    ! optimal decision for case il 
                                    c_temp(il) = cons_com
                                    aplus_temp(il) = a_in 
                                    V_temp(il) = valuefunc(a_in, cons_com, il, ij, ih, ip, ism, isf)
                                    ! V_temp(il) = -V_val
                            
                                enddo ! il
                                
                                ! choose optimal labour supply decision and 
                                ! the implied consumption and savings decisions 
                                l_star = maxloc(V_temp(:), 1) - 1 ! maxloc returns the first index as 1, so we substract 1 to get 0 if labour == 0 
                                aplus(ia, ih, ijps, it) = aplus_temp(l_star) 
                                c(ia, ih, ijps, it)     = c_temp(l_star) 
                                l(ia, ih, ijps, it)     = real(l_star,kind=8)/L_max 
                                V(ia, ih, ijps, it)     = V_temp(l_star) 
                            
                            enddo ! isf   
                            
                            !copy decision for retirees 
                            !if (ij >= JR) then 
                            !    aplus(ia, :, ijps1:ijps2, it) = aplus(ia, 0, ijps1, it) 
                            !    c(ia, :, ijps1:ijps2, it)     = c(ia, 0, ijps1, it)
                            !    l(ia, :, ijps1:ijps2, it)     = l(ia, 0, ijps1, it) 
                            !    V(ia, :, ijps1:ijps2, it)     = V(ia, 0, ijps1, it) 
                            !endif
                            
                        enddo ! ism
                    enddo ! ip
                enddo ! ih
                
                !copy decision for retirees 
                if (ij >= JR) then 
                    aplus(ia, :, ijps1:ijps2, it) = aplus(ia, 0, ijps1, it) 
                    c(ia, :, ijps1:ijps2, it)     = c(ia, 0, ijps1, it)
                    l(ia, :, ijps1:ijps2, it)     = l(ia, 0, ijps1, it) 
                    V(ia, :, ijps1:ijps2, it)     = V(ia, 0, ijps1, it) 
                endif
                            
            enddo ! ia
        
            ! interpolate EV and RHS (at age ij) for solving the next iteration age ij-1 
            call interpolate(ij, it) 
            
            call check_grid_a(ia_max, it) ! the largest asset grid point for age ij (for setting upper bound of asset grid)  
            call check_grid_h(ih_max, it) ! the largest human cap grid point used for age ij 
            write(*,'(a, i3/, 2(a, i3, a, f8.3), a)')'Age:', ij, ',  iamax:', ia_max(ij,it), ', a(iamax):',   a(ia_max(ij,it)), &
                                                ',  ihmax:', ih_max(ij,it), ', h(ihmax):', h(ih_max(ij,it)), ' DONE!'
            
        enddo ! ij
    
    END SUBROUTINE solve_household 
    

    
    ! Determine the invariant distribution of households
    SUBROUTINE get_distribution(it) 
    
        integer, intent(in) :: it 
        integer :: ij, ia, ih, ip, ism, isf, itm
        integer :: iap, ihp, ism_p, isf_p 
        integer :: ijps1, ijps2, ijps, ijps_m, ijps_p 
        real*8  :: htoday, labour
        ! interpolation variables 
        real*8  :: w0, w1 
        integer :: ial, iar, ihl, ihr 

        ! previous year 
        itm = year(it, 2, 1) 

        ! initialize 
        !ijps1 = indexing(1, 1, 1, 1) 
        !ijps2 = indexing(JJ, NP, NS, NS) 
        ! change to this ijps2 = indexing(JJ+1, 1, 1, 1) - 1
        !ijps2 = indexing(JJ+1, 1, 1, 1) - 1
        phi(:, :, :, it) = 0d0 
        
        ! initial distribution for newborn household (age ij=1) 
        ! 1. no assets, no human capital 
        ! 2. receives permanent education shock and no transitory shocks i
        !do ip = 1, NP 
        !    ijps = indexing(1, ip, is_initial, is_initial) 
        !    phi(0, 0, ijps, it) = dist_theta(ip) 
        !enddo 
        
         ! initial distribution for newborn household (age ij=1) 
        ! 1. no assets, no human capital 
        ! 2. but receives both permanent education and transitory shocks 
        do ip = 1, NP 
            do ism = 1, NS 
                do isf = 1, NS 
                    ijps = indexing(1, ip, ism, isf) 
                    phi(0, 0, ijps, it) = dist_theta(ip)*prob(ism)*prob(isf) 
                enddo 
            enddo 
        enddo 
        
        ! successively compute distribution over age 
        do ij = 2, JJ
            do ia = 0, NA 
                do ih = 0, NH 
                    do ip = 1, NP
                        do ism = 1, NS 
                            do isf = 1, NS 
                                ! begin from known distribution in age ij-1,
                                ! determine the share of (ij) tomorrow's asset holdings and human capital 
                                ! on their respective grid nodes for each realized shock combination (ip, ism, isf)
                                
                                ! indexing 
                                ijps_m = indexing(ij-1, ip, ism, isf) 
                                
                                ! assets
                                do iap = 0, NA-2; if (aplus(ia, ih, ijps_m, itm) < a(iap+1)) exit; enddo
                                    ial = min(iap, NA); iar = min(iap+1, NA); 
                                    w0  = (aplus(ia, ih, ijps_m, itm) - a(ial))/(a(iar)-a(ial))
                                    w0  = min(w0, 1d0) ! weight for iar
                                
                                ! human capital
                                labour = l(ia, ih, ijps_m, itm) 
                                htoday =  h(ih) + (xi(1) + xi(2)*dble((ij-1)))*labour - del_h*(1d0-labour) 
                                htoday = min( max(htoday, h_l), h_max(ij) )
            
                                do ihp = 0, NH-2; if (htoday < h(ihp+1)) exit; enddo 
                                    ihl = min(ihp, NH); ihr = min(ihp+1, NH); 
                                    w1 = (htoday - h(ihl))/(h(ihr)-h(ihl))
                                    w1 = min(w1, 1d0) ! weight for ihr
                                
                                ! For retirees, ihl = ihr = ih since human cap for ij>=JR is assumed constaint at ih(JR-1) until death 
                                if (ij >= JR) then 
                                    ihl = ih 
                                    ihr = ih
                                endif 
                                
                                ! update distribution 
                                do ism_p = 1, NS 
                                    do isf_p = 1, NS 
                                        
                                        ! indexing 
                                        ijps_p = indexing(ij, ip, ism_p, isf_p) 
                                        
                                        ! update distribution 
                                        phi(ial, ihl, ijps_p, it) = phi(ial, ihl, ijps_p, it) &
                                                + pi(ism, ism_p) * pi(isf, isf_p) & 
                                                  * (1d0-w0) * (1d0-w1) * phi(ia, ih, ijps_m, itm) 
                                        
                                        phi(ial, ihr, ijps_p, it) = phi(ial, ihr, ijps_p, it) &
                                                + pi(ism, ism_p) * pi(isf, isf_p) & 
                                                  * (1d0-w0) * w1 * phi(ia, ih, ijps_m, itm)
                                        
                                        phi(iar, ihl, ijps_p, it) = phi(iar, ihl, ijps_p, it) & 
                                                + pi(ism, ism_p) * pi(isf, isf_p) & 
                                                  * w0 * (1d0-w1) * phi(ia, ih, ijps_m, itm)
                                        
                                        phi(iar, ihr, ijps_p, it) = phi(iar, ihr, ijps_p, it) &
                                                + pi(ism, ism_p) * pi(isf, isf_p) & 
                                                  * w0 * w1 * phi(ia, ih, ijps_m, itm)
                                    enddo ! isf_p
                                enddo ! ism_p
                                
                            enddo ! isf
                        enddo ! ism
                    enddo ! ip
                enddo ! ih
            enddo ! ia
        enddo ! ij
    
    END SUBROUTINE get_distribution
            
            


    ! Aggregate by cohort and over cohorts 
    SUBROUTINE aggregation(it) 
    
        implicit none 
        integer, intent(in) :: it 
        integer :: ij, ia, ih, ip, ism, isf, itp, ijps
        REAL*8 :: prod_m, prod_f, linc_m, linc_f
        REAL*8 :: LL_old, KK_old, BF_old, m_coh(JJ, 0:TT)
        REAL*8 :: exp_c(JJ), exp_y(JJ)
        ! damping parameter 
        REAL*8 :: damp            ! Damping parameter for updating process of capital 
        logical :: breakpoint     ! for debugging mode
    
        breakpoint = .false. 

        ! tomorrow year 
        itp = year(it, 1, 2) 
        
        ! store old LL (labour demand decision by firm), old KK, and old BF 
        LL_old = LL(it) 
        KK_old = KK(it) 
        BF_old = BF(it)
        
        ! initialize cohort aggregates 
        c_coh(:, it) = 0d0
        a_coh(:, it) = 0d0 
        l_coh(:, it) = 0d0  ! total labour efficiency 
        h_coh(:, it) = 0d0  ! human capital
        ym_coh(:, it) = 0d0 
        yf_coh(:, it) = 0d0 
        y_coh(:, it)  = 0d0 
        beq_coh(:, it) = 0d0 
        v_coh(:, it) = 0d0 
        m_coh(:, it) = 0d0
        bor_cons(:, it) = 0d0 
        if (progressive) inctax_coh(:, it) = 0d0 
        if (progressive) nymtau_coh(:, it) = 0d0 
        if (progressive) nyftau_coh(:, it) = 0d0 
        if (ftb_on) FTBA_coh(:, it) = 0d0 
        if (ftb_on) FTBB_coh(:, it) = 0d0 
        if (ccs_on) CCS_coh(:, it)  = 0d0 
        
        do ij = 1, JJ 
            do ia = 0, NA 
                do ih = 0, NH 
                    do ip = 1, NP 
                        do ism = 1, NS
                            do isf = 1, NS
                                
                                ! indexing 
                                ijps = indexing(ij, ip, ism, isf) 
                                
                                prod_m = lab1(ij) * eff(ij) * theta(ip) * eta(ism) 
                                prod_f = lab2(ij) * exp(h(ih))* theta(ip) * eta(isf) * l(ia, ih, ijps, it)
                                
                                ! consumption and labour 
                                c_coh(ij, it)   = c_coh(ij, it) + c(ia, ih, ijps, it)*phi(ia, ih, ijps, it)
                                l_coh(ij, it)   = l_coh(ij, it) + l(ia, ih, ijps, it)*phi(ia, ih, ijps, it)
                                ! hour_coh(ij, it)   = l_coh(ij, it) + l(ia, ih, ijps, it)*lab2(ij)*phi(ia, ih, ijps, it)

                                ! labour efficiency and female human capital 
                                ym_coh(ij, it)  = ym_coh(ij, it) + prod_m * phi(ia, ih, ijps, it)
                                yf_coh(ij, it)  = yf_coh(ij, it) + prod_f * phi(ia, ih, ijps, it)
                                y_coh(ij, it)   = y_coh(ij, it)  + (prod_m + prod_f) * phi(ia, ih, ijps, it)
                                h_coh(ij, it)   = h_coh(ij, it)  + exp(h(ih)) * phi(ia, ih, ijps, it)

                                ! For endogenous scaling parameter calculation 
                                if (progressive) nymtau_coh(ij, it) = nymtau_coh(ij, it) + & 
                                                                     (wm(it)*prod_m)**(1d0-tauy(it)) * phi(ia, ih, ijps, it)
                                if (progressive) nyftau_coh(ij, it) = nyftau_coh(ij, it) + & 
                                                                     (wf(it)*prod_f)**(1d0-tauy(it)) * phi(ia, ih, ijps, it)
                                
                                ! assets 
                                a_coh(ij, it)   = a_coh(ij, it) + a(ia) * phi(ia, ih, ijps, it)
                                
                                ! income tax 
                                if (progressive) inctax_coh(ij, it) = inctax_coh(ij, it) + & 
                                                                      inctaxfunc(a(ia), wm(it)*prod_m, wf(it)*prod_f, it) * &
                                                                      phi(ia, ih, ijps, it)

                                ! ftba and ftbb 
                                if (nchild(ij) > 0 .and. ftb_on) FTBA_coh(ij, it) = FTBA_coh(ij, it) + FTB_A(ij, wm(it)*prod_m + &
                                                                                    wf(it)*prod_f) * phi(ia, ih, ijps, it) 
                                if (nchild(ij) > 0 .and. ftb_on) FTBB_coh(ij, it) = FTBB_coh(ij, it) + & 
                                                                    FTB_B(ij, wm(it)*prod_m, wf(it)*prod_f) * phi(ia, ih, ijps, it)
                                
                                ! child care subsidy 
                                if (nchild(ij) > 0 .and. ccs_on) CCS_coh(ij, it) = CCS_coh(ij, it) + & 
                                                        CCS_rate(ij, wm(it)*prod_m + wf(it)*prod_f, lab1(ij), lab2(ij)) * & 
                                                        l(ia, ih, ijps, it) * lab2(ij) * wf(it) * cchild(ij) * phi(ia, ih, ijps, it) 
                                

                                ! ignore (cycle) if retired without assets and pension 
                                ! (we do not count these households and their optimal values towards V and bor_cons) 
                                if ( ij>=JR .and. ia==0 .and. (kappa(0) <= 1d-10 .or. kappa(1) <= 1d-10) ) then 
                                    CYCLE 
                                endif 
                                
                                ! borrowing constrained household 
                                if (aplus(ia, ih, ijps, it) < 1d-4) then 
                                    bor_cons(ij, it) = bor_cons(ij, it) + phi(ia, ih, ijps, it)
                                endif 
                                
                                ! population size
                                m_coh(ij, it) = m_coh(ij, it) + phi(ia, ih, ijps, it)
                                
                                ! bequest (not survive == not alive in age ij --> no c, s, l (no econ activities))
                                beq_coh(ij, it) = beq_coh(ij, it) + a(ia)*(1d0+rn(it))*(1d0-psi(ij))*phi(ia, ih, ijps, it) !* m(ij, it) !!!
                                !beq_coh(ij, it) = beq_coh(ij, it) + a(ia)*(1d0+rn(it))*(1d0-psi(ij))*phi(ia, ih, ijps, it)/psi(ij)!!! if fully annuitized --> no bequest left over 

                                ! value (optimal utility) 
                                v_coh(ij, it) = v_coh(ij, it) + V(ia, ih, ijps, it) * phi(ia, ih, ijps, it) !* m(ij, it) !!! 
                                
                            enddo ! isf 
                        enddo ! ism
                    enddo ! ip
                enddo ! ih
            enddo ! ia
        enddo ! ij 
        
        
        ! As some households have been excluded, we must first normalize bor_cons and V_coh 
        ! by their corresponding population mass at time it
        bor_cons(:, it) = bor_cons(:, it) / m_coh(:,it)
        v_coh(:, it)    = v_coh(:, it) / m_coh(:,it)
        
        !write(*, '(a)') "AGGREGATION step1 done!"
        
        
        ! Aggregate household decisions
        CC(it) = 0d0
        AA(it) = 0d0 
        LLM(it) = 0d0
        LLF(it) = 0d0 
        LL(it) = 0d0
        LFP(it) = 0d0 ! female labour force participation 
        HH(it) = 0d0  ! female human capital 
        BQ(it) = 0d0 
        parent_pop(it) = 0d0
        workpop(it) = 0d0
        if (progressive) INCTAX(it) = 0d0 
        if (progressive) NYtau(it) = 0d0 
        if (ftb_on) FTBA(it) = 0d0 
        if (ftb_on) FTBB(it) = 0d0 
        if (ccs_on) CCS(it)  = 0d0 
        
        do ij = 1, JJ 
            CC(it)  = CC(it)    + c_coh(ij, it) * m(ij, it)   ! agg. consumption in time it 
            AA(it)  = AA(it)    + a_coh(ij, it) * m(ij, it)   ! agg. wealth in time it 
            !AA(it)  = AA(it)    + a_coh(ij, it)/psi(ij) * m(ij, it) ! if fully annuitized --> without bequest 
            LLM(it) = LLM(it)   + ym_coh(ij, it) * m(ij, it)  ! agg. male labour in efficiency unit at time it 
            LLF(it) = LLF(it)   + yf_coh(ij, it) * m(ij, it)  ! agg. female labour in efficiency unit at time it 
            LL(it)  = LL(it)    + y_coh(ij, it) * m(ij, it)   ! agg. total labour in efficiency unit at time it
            LFP(it) = LFP(it)   + l_coh(ij, it) * m(ij, it)   ! agg. female LFP rate at time it 
            HH(it)  = HH(it)    + h_coh(ij, it) * m(ij, it)   ! agg. female human capital at time it 
            BQ(it)  = BQ(It)    + beq_coh(ij, it) * m(ij, it) !!!!! 
            if (progressive) INCTAX(it) = INCTAX(it) + inctax_coh(ij, it) * m(ij, it) !!! 
            if (progressive) NYtau(it) = NYtau(it) + (nymtau_coh(ij, it) * nyftau_coh(ij,it)) * m(ij, it) 
            if (ftb_on) FTBA(it)  = FTBA(it) + FTBA_coh(ij, it) * m(ij, it)
            if (ftb_on) FTBB(it)  = FTBB(it) + FTBB_coh(ij, it) * m(ij, it)
            if (ccs_on) CCS(it)   = CCS(it)  + CCS_coh(ij, it)  * m(ij, it) 

            if (ij < JR) workpop(it) = workpop(it) + m(ij, it) 
            if (nchild(ij) > 0) parent_pop(it) = parent_pop(it) + m(ij, it)*nchild(ij)  !TEST EXPANDING BY NUMB OF CHILDREN
        enddo 
        
        NY(it) = wm(it)*LLM(it) + wf(it)*LLF(it) 
        !write(*, '(a)') "AGGREGATION step2 done!"
        
        ! SET DAMPING PARMAETER 
        damp = damp_aggregation ! damp = 0.3d0
        ! reduce damping parameter 
        ! when the percentage deviation in output is less than 5%
        if (abs(DIFF(it)/YY(it))*100d0 <= 10d0) damp = 0.3d0 
        if (abs(DIFF(it)/YY(it))*100d0 <= 5d0)  damp = 0.1d0
        if (abs(DIFF(it)/YY(it))*100d0 <= 1d0)  damp = 0.1d0
        
        !if (iter > 30 .and. (abs(DIFF(it)/YY(it))*100d0 > 1d0)) damp = 0.5d0
        
        ! Derive firm decisions from market clearing conditions
        ! 1. LABOUR MARKET 
        !   (note: LL_old is basically LL demand derived from previous iteration
        !LL(it) = damp*(LLM(it)+LLF(it)) + (1d0-damp)*LL_old 
        LL(it) = damp*LL(it) + (1d0-damp)*LL_old 
        if(it==0 .or. it==TT) LL(it) = 0.8*LL(it) + 0.2*LL_old   ! add more weight to KK_old for init SS
        !if(it==TT) LL(it) = 0.8*LL(it) + 0.2*LL_old  ! add more weight to KK_old for final SS 
        
        !write(*, '(a)') "AGGREGATION step3 done!"
        ! 2. CAPITAL MARKET
        !if (AA(it)-BB(it)-BA(it) < 0) BA(it) = (AA(it) - BB(it) - 0.05d0) ! re-adjust BA(it) value if it is too large such that capital = 0.05d0
        ! if (lsra_on) BA(it) = damp*BA(it) + (1d0-damp)*Bold(it) ! Bold is from subroutine LSRA1
        
        if (.not. open_capmarket) then ! closed economy
            KK(it) = damp*(AA(it)-BB(it)-BA(it)) + (1d0-damp)*KK_old
            if(it==0 .or. it==TT) KK(it) = 0.8*KK(it) + 0.2*KK_old   ! add more weight to KK_old for init SS
            !if(it==TT) KK(it) = 0.8*KK(it) + 0.2*KK_old  ! add more weight to KK_old for final SS 
        else  ! small open economy
            KK(it) = LL(it)*((r(it) + delta)/(TFP*alpha))**(1d0/(alpha-1d0))
            BF(it) = AA(it) - KK(it) - BB(it) - BA(it) 
            !BF(it) = damp*BF(it) + (1d0-damp)*BF_old 
            ITB(it) = (1d0+n_p)*BF(itp) - (1d0+r(it))*BF(it)
        endif 
        
        !write(*, '(a)') "AGGREGATION step4 done!"
        
        ! DEBUGGING MODE 
        if (KK(it) < 0d0) breakpoint = .true. ! trigger breakpoint when encountering negative KK(it) <- happens with large BA(it) of LSRA
        
        ! cap capital 
        ! KK(it) = max(KK(it), 0.1d0) ! doees not work!
        

        ! INVESTMENT AND OUTPUT 
        II(it) = (1d0+n_p)*KK(itp) - (1d0-delta)*KK(it) 
        YY(it) = TFP * KK(it)**alpha * LL(it)**(1d0-alpha)
        !YY(it) = max(TFP * KK(it)**alpha * LL(it)**(1d0-alpha), 0.05d0)
        !if (YY(it) < 0d0) error stop "YY(it) < 0"
        
        ! DEBUG MODE 
        if (isnan(YY(it))) breakpoint = .true. ! trigger breakpoint when YY(it) is not a number
        
        ! AVERAGE INCOME PER HOUSEHOLD  
        !INC(it) = (wm(it)*LLM(it) + wf(it)*LLF(it))/workpop(it)  
        INC(it) = (wm(it)*LLM(it))/workpop(it) ! here, INC(it) := average male income per working age pop
        LFP(it) = LFP(it) ! /workpop(it) 
        HH(it)  = HH(it)/workpop(it) 
        
        ! Good market clearing condition 
        ! DIFF(it) = Agg. supply - Agg. demand in time it 
        DIFF(it) = YY(it) - (CC(it) + II(it) + GG(it) + ITB(it))
        
        write(*,'(/a/)')"SUBROUTINE AGGREGATION"
        write(*,'(2(a,i3,a,f12.5))')"BA(", it,") = ", BA(it), "    KK(", it, ") = ", KK(it)
        write(*,'(2(a,i3,a,f12.5)/)')"DIFF(", it,") = ", DIFF(it), "    YY(", it, ") = ", YY(it)
        
        !write(*, '(a)') "AGGREGATION step5 done!"
        ! COEFFICIENT OF VARIATION OF CONSUMPTION AND NET INCOME
        ! Calculate cohort specific coefficients of variation of log consumption and income 
        ! to capture relative changes between periods 
        
        exp_c = 0d0;   exp_y = 0d0
        cv_c(:, it) = 0d0;    cv_y(:, it) = 0d0
        
        do ij = 1, JJ 
            do ia = 0, NA 
                do ih = 0, NH 
                    do ip = 1, NP 
                        do ism = 1, NS 
                            do isf = 1, NS 
                                
                                ! indexing 
                                ijps = indexing(ij, ip, ism, isf) 
                                
                                linc_m = wn_m(it) * lab1(ij) * eff(ij) * theta(ip) * eta(ism) 
                                linc_f = wn_f(it) * lab2(ij) * exp(h(ih)) * theta(ip) * eta(isf) * l(ia, ih, ijps, it)
                                
                                ! compute E(x) 
                                exp_c(ij) = exp_c(ij) + c(ia, ih, ijps, it) * phi(ia, ih, ijps, it)
                                exp_y(ij) = exp_y(ij) + (linc_m + linc_f) * phi(ia, ih, ijps, it)
                                
                                ! compute E(x**2) 
                                cv_c(ij, it)  = cv_c(ij, it) + c(ia, ih, ijps, it)**2 * phi(ia, ih, ijps, it)
                                cv_y(ij, it)  = cv_y(ij, it) + (linc_m + linc_f)**2 * phi(ia, ih, ijps, it)
                                
                            enddo ! isf
                        enddo ! ism
                    enddo ! ip
                enddo ! ih
            enddo ! ia
        enddo ! ij
        
        cv_c(:, it) = sqrt(cv_c(:,it) - exp_c(:)**2)/max(exp_c(:), 1d-10) 
        cv_y(:, it) = sqrt(cv_y(:,it) - exp_y(:)**2)/max(exp_y(:), 1d-10) 
        
        !write(*, '(a)') "AGGREGATION step6 done!"
        
    END SUBROUTINE aggregation
            
    
    
    ! subroutine for calculating government variables 
    SUBROUTINE government(it) 
    
        implicit none 
        integer, intent(in) :: it 
        integer :: ij, itm, itp, ijps, ia, ih, ip, ism, isf
        REAL*8  :: expenditure, prod_m, prod_f, tot_transfer, CCS_cost 
        
        ! last year and next year 
        itp = year(it, 1, 2) 
        itm = year(it, 2, 1) 
        
        ! TAX AND TRANSFER SYSTEM 
        ! fix govt spending and borrowing at the init steady state levels 
        GG(it) = gy * YY(0)  !gy * YY(it)
        BB(it) = by * YY(0)  !by * YY(it)
        
        ! Net non-transfer expenditure to be financed by the tax system 
        ! (after having accounted for govt's borrowings)
        expenditure = GG(it) + (1d0+r(it))*BB(it) - (1d0+n_p)*BB(itp) 
    
        ! Determine total income tax (if flat tax scheme) and transfer 
        if (.not. progressive) INCTAX(it) = tauw(it)*(wm(it)*LLM(it) + wf(it)*LLF(it))
        if (ftb_on) then 
            tot_transfer = FTBA(it) + FTBB(it)
        else 
            tot_transfer = Tr(it) * parent_pop(it) 
        endif 

        ! Total cost of child care subsidy in time it 
        CCS_cost = 0d0 
        if (ccs_on) CCS_cost = CCS(it) 

        ! Get budget balancing tax/transfer rate
        SELECT CASE (tax(it)) 
        CASE (1) ! endogenous consumption tax
            
            !tauc(it)  = (expenditure + Tr(it)*parent_pop(it) - INCTAX(it) - taur(it)*r(it)*AA(it)) / CC(it)
            tauc(it)  = (expenditure + tot_transfer + CCS_cost - INCTAX(it) - taur(it)*r(it)*AA(it)) / CC(it)
            p(it)     = 1d0 + tauc(it)
        CASE (2) ! endogenous wage and rental taxes 
            ! taur(it)  = (expenditure + Tr(it)*parent_pop(it) - tauc(it)*CC(it)) / (wm(it)*LLM(it) + wf(it)*LLF(it) + r(it)*AA(it));
            if (.not. progressive) then 
                taur(it)  = (expenditure + tot_transfer + CCS_cost - tauc(it)*CC(it)) / & 
                            (wm(it)*LLM(it) + wf(it)*LLF(it) + r(it)*AA(it));
                tauw(it)  = taur(it);
            else 
                error stop "tax = 2 is only valid with proportional tax scheme."
            endif 
        CASE (3) ! endogenous labour income tax 
            if (.not. progressive) then 
                tauw(it)  = (expenditure + tot_transfer + CCS_cost - tauc(it)*CC(it) - taur(it)*r(it)*AA(it)) / &
                                (wm(it)*LLM(it) + wf(it)*LLF(it))
            else 
                !lambda_tauy(it) = (NY(it) - (expenditure + Tr(it)*parent_pop(it) - tauc(it)*CC(it) & 
                !                        - taur(it)*r(it)*AA(it))) / NYtau(it)

                lambda_tauy(it) = (NY(it) - (expenditure + tot_transfer + CCS_cost - tauc(it)*CC(it) & 
                                        - taur(it)*r(it)*AA(it))) / NYtau(it)
                ! Recalculate income tax 
                do ij = 1, JJ 
                    do ia = 0, NA 
                        do ih = 0, NH 
                            do ip = 1, NP 
                                do ism = 1, NS
                                    do isf = 1, NS                             
                                        ! indexing 
                                        ijps = indexing(ij, ip, ism, isf) 
                                        
                                        prod_m = lab1(ij) * eff(ij) * theta(ip) * eta(ism) 
                                        prod_f = lab2(ij) * exp(h(ih))* theta(ip) * eta(isf) * l(ia, ih, ijps, it)
                                        
                                        ! income tax 
                                        inctax_coh(ij, it) = inctax_coh(ij, it) + &
                                                        inctaxfunc(a(ia), wm(it)*prod_m, wf(it)*prod_f, it) * phi(ia, ih, ijps, it)                                
                                    enddo ! isf 
                                enddo ! ism
                            enddo ! ip
                        enddo ! ih
                    enddo ! ia
                enddo ! ij 

                INCTAX(it) = 0d0 
                do ij = 1, JJ 
                        INCTAX(it) = INCTAX(it) + inctax_coh(ij, it) * m(ij, it) !!! 
                enddo 

            endif

        CASE (4) ! endogenous capital rental rate 
            !incometax = tauw(it)*(wm(it)*LLM(it) + wf(it)*LLF(it))
            !taur(it)  = (expenditure + Tr(it)*parent_pop(it) - tauc(it)*CC(it) - INCTAX(it)) / (r(it)*AA(it))
            taur(it)  = (expenditure + tot_transfer + CCS_cost - tauc(it)*CC(it) - INCTAX(it)) / (r(it)*AA(it))

        ! need a switch between lumpsum transfer and FTB A and FTB B 
        CASE (5) ! endogenous transfer 
            if (.not. ftb_on) then 
                Tr(it)    = -(expenditure + CCS_cost - tauc(it)*CC(it) - INCTAX(it) - taur(it)*r(it)*AA(it))/parent_pop(it)
            else 
                error stop "tax = 5 is only valid with lumpsum transfer scheme."
            endif 
        CASE DEFAULT ! return error otherwise 
            error stop "Only values 1-5 are admitted for tax(it)."
        END SELECT
        
        inctax_coh(:, it) = 0d0 
        
        ! Tax revenue 
        taxrev(1, it) = tauc(it) * CC(it) 
        taxrev(2, it) = INCTAX(it)
        taxrev(3, it) = taur(it) * r(it) * AA(it) 
        taxrev(4, it) = -tot_transfer - CCS_cost 
        taxrev(5, it) = sum(taxrev(1:4, it))
        
        
        ! SOCIAL SECURITY SYSTEM 
        pen(:, it) = 0d0 
        pen(JR:JJ, it) = kappa(it) * INC(itm) ! INC(itm) := past average annual income per working age pop
        PP(it) = 0d0
        do ij = JR, JJ
            PP(it) = PP(it) + pen(ij, it)*m(ij, it)
        enddo 
        
        ! budget balancing ss contribution (taup(it)) at time it 
        ! (such that taup(it)*(wm(it)*LLM(it) + wf(it)*LLF(it)) = PP(it) 
        taup(it) = PP(it) / (wm(it)*LLM(it) + wf(it)*LLF(it)) 

    END SUBROUTINE government
     
    
    ! NEW LSRA FOR TESTING 
     ! LSRA subroutine 
    SUbROUTINE LSRA1()
    
        implicit none 
        integer :: ij, ia, ih, ip, ism, isf, it, ijps, ijps0, ijps1
        ! variables for compensation to old (:= non-newborn) generations in reform period
        real*8  :: V_0, V_1, dV_da, max_income, comp_new, linc_m, linc_f
        real*8  :: income_tax, payroll_tax, transfer, sr 
        ! variables for compensation to newborns
        real*8  :: EV_0, EV_t, dEV_da
        ! Present values of V_0, V_t, LSRA's transfers 
        ! NOTE: (V_0 := utility of newborn in time 0; V_t := utility of newborn in time it) 
        real*8 :: PV_0, PV_t, PV_comp
        real*8 :: damp 
        
        
        ! NOTE: Compensation can be negative (i.e., households are taxed) 
        
        ! initialize 
        SV(:) = 0d0                 ! total compensation spending by the LSRA at time it 
        comp_coh(:, :) = 0d0        ! total compensation received by cohort at time it
        
        compensated = 0d0           ! fraction of household having received compensation
        fully_compensated = 0d0     ! fraction of household having been fully compensated by the scheme 
        Bold= BA(:) 
        
        
        write(*, '(/a/)') "LSRA SUBROUTINE"

        ! COMPENSATION TO REFORM PERIOD'S OLD GENERATIONS
        ! it = 1 (REFORM PERIOD) 
        do ij = 2, JJ 
            do ia = 0, NA 
                do ih = 0, NH 
                    do ip = 1, NP 
                        do ism = 1, NS
                            do isf = 1, NS 
                                
                                ijps = indexing(ij, ip, ism, isf) 
                                ! no compensation for those retired with no assets, no pensions, and no transfers 
                                if(ij >= JR .and. ia == 0 .and. (kappa(0) <= 1d-10 .or. kappa(1) <= 1d-10))then
                                !if (ij>=JR .and. ia==0 .and. (pen(ij, 0)<=1d-10 .or. pen(ij, 1)<=1d-10)) then 
                                    comp(ia, ih, ijps, 1) = 0d0 
                                    CYCLE 
                                endif 
                                
                                ! reform period's utility 
                                V_1 = V(ia, ih, ijps, 1) 
                                
                                ! initial period's utility (target utility) 
                                V_0 = V(ia, ih, ijps, 0) 
                                
                                ! Calculate maximum income 
                                linc_m = wm(1)*lab1(ij)*eff(ij)*theta(ip)*eta(ism) 
                                linc_f = wf(1)*lab2(ij)*exp(h(ih))*theta(ip)*eta(isf) 


                                ! get income and payroll taxes 
                                if (progressive) then 
                                    income_tax = inctaxfunc(a(ia), linc_m, linc_f, 1)
                                else 
                                    income_tax = tauw(1)*(linc_m + linc_f) 
                                endif 

                                payroll_tax = ptaxfunc(linc_m, linc_f, 1) 

                                ! get ftb_a, ftb_b, and child care subsidy
                                if (ftb_on) then 
                                    transfer = FTB_A(ij, linc_m + linc_f) + FTB_B(ij, linc_m, linc_f)
                                else 
                                    transfer = Tr(1) * nchild(ij) 
                                endif 
                                
                                ! calculate effective rate of child care subsidy 
                                sr = 0d0 
                                if (ccs_on) sr = CCS_rate(ij, linc_m+linc_f, lab1(ij), lab2(ij))
                                

                                ! derivative of value function w.r.t assets 
                                ! (IMPORTANT: we have double taxation on interest earnings)
                                if (progressive) then 
                                    dV_da = margu(c(ia, ih, ijps, 1), l(ia, ih, ijps, 1), ij, 1, form1=1)  * & 
                                                    (1d0 + rn(1) - inctaxfunc(a(ia), linc_m, linc_f, 1, mtr=3)) !
                                else 
                                    dV_da = margu(c(ia, ih, ijps, 1), l(ia, ih, ijps, 1), ij, 1, form1=1)  * & 
                                                    (1d0 + rn(1)) !
                                endif 
                                ! NOTE: the current FTB, and child care subsidy schemes are assumed to include 
                                !       only labour income test. If they involve asset test, then the marginal losses (e.g., 
                                !       effective taper rate of FTB and subsidy) must also be accounted for in the 
                                !       multiplicative terms (1d0 + rn(1)) above. 
                                
                                ! calculate additional LSRA transfer/tax 
                                comp_new = (V_0 - V_1)/dV_da 


                                ! get max_income 
                                max_income = (1d0+rn(1))*a(ia) + linc_m & !
                                                + l(ia, ih, ijps, 1)*(linc_f - (1d0-sr)*wf(1)*lab2(ij)*cchild(ij)) & !+ (linc_f - wf(1)*lab2(ij)*cchild(ij)) & ! 
                                                + pen(ij, 1) & ! + Tr(1)*nchild(ij) & 
                                                + bequest(ij, 1) &
                                                + comp(ia, ih, ijps, 1) & 
                                                - income_tax - payroll_tax 

                                if (nchild(ij) > 0) max_income = max_income + transfer


                                ! Restrict comp_new to maximum income 
                                ! that is, if comp_new < 0 (thus, households are taxed), the tax cannot 
                                ! be more than their max income
                                comp_new = max(comp_new, -max_income) 
                                
                                ! Check whether individual is fully compensated (V_1 = the target V_0) 
                                compensated = compensated + phi(ia, ih, ijps, 1)*m(ij, 1) 
                                if (abs((V_1 - V_0)/V_0)*100d0 < 5d0) then ! outer_tol
                                    fully_compensated = fully_compensated + phi(ia, ih, ijps, 1)*m(ij,1) 
                                endif 
                                
                                ! Calculate total compensation 
                                damp = damp_LSRA
                                if (iter <= 5) damp = damp_LSRA/2d0

                                ! reduce damping parameter 
                                ! when the percentage deviation in output is less than 5%
                                !if (abs((V_1-V_0)/V_0)*100d0 <= 10d0) damp = 0.3d0 
                                !if (abs((V_1-V_0)/V_0)*100d0 <= 5d0 ) damp = 0.3d0 
                                comp(ia, ih, ijps, 1) = comp(ia, ih, ijps, 1) + damp*comp_new 
                                
                                ! aggregate compensation by age/cohort 
                                comp_coh(ij, 1) = comp_coh(ij, 1) + comp(ia, ih, ijps, 1)*phi(ia, ih, ijps, 1)
                                
                                write(*, '(a)') "OLD GENERATION"
                                write(*, '(a, 3(i3,a), f12.5)') "comp(",ia,",",ih,",",ijps,",1) = ", comp(ia, ih, ijps, 1) 

                                
                            enddo ! isf
                        enddo ! ism
                    enddo ! ip
                enddo ! ih
            enddo ! ia
        enddo ! ij 
            
    
        ! Total compensation spending in year 1 to old generations 
        do ij = 2, JJ 
            SV(1) = SV(1) + comp_coh(ij, 1) * m(ij, 1) 
        enddo 
    
    
        ! COMPENSATION TO ALL NEWBORNS FROM REFORM PERIOD ONWARDS (it=1, 2, ..., TT) 
        ! First, compute efficiency factor (Lambda) such that present value of all compensations = 0
        PV_0 = 0d0 
        PV_t = 0d0 
        PV_comp = 0d0 
    
        ijps0 = indexing(1, 1, 1, 1)
        ijps1 = indexing(1, NP, NS, NS) 
            
        do it = TT, 1, -1 
            
            damp = damp_LSRA
            if (iter <= 5) damp = damp_LSRA/2d0
            !itp = year(it, 1, 2) 
            !chi_t = sum(l(:, :, :, it)*phi(:,:,:,it)*chi)
            
            ! get today's ex ante utility of newborn without compensation
            EV_t = damp*V_coh(1, it)
            
            ! get target's ex ante utllity of newborn without compensation (period it = 0) 
            EV_0 = damp*V_coh(1, 0) 
            
            ! derivative of expected utility function 
            dEV_da = 0d0 
            do ip = 1, NP 
                do ism = 1, NS 
                    do isf = 1, NS 
                        ijps = indexing(1, ip, ism, isf)
                        if (progressive) then 
                            dEV_da = dEV_da + margu(c(0, 0, ijps, it), l(0, 0, ijps, it), 1, it, form1=1) * phi(0, 0, ijps, it)  &
                                    * (1d0+rn(it)-inctaxfunc(a(ia), linc_m, linc_f, it, mtr=3))
                            ! NOTE: the current FTB, and child care subsidy schemes are assumed to include 
                                    !       only labour income test. If they involve asset test, then the marginal losses (e.g., 
                                    !       effective taper rate of FTB and subsidy) must also be accounted for in the 
                                    !       multiplicative terms (1d0 + rn(1)) above. 
                        else 
                            dEV_da = dEV_da + margu(c(0, 0, ijps, it), l(0, 0, ijps, it), 1, it, form1=1) * phi(0, 0, ijps, it)  &
                                    * (1d0+rn(it))
                        endif 
                    enddo 
                enddo 
            enddo 
            
            ! Calculate present values 
            if (it==TT) then 
                PV_0 = (EV_0)/dEV_da * (1d0+r(it))/(r(it)-n_p)
                PV_t = (EV_t)/dEV_da * (1d0+r(it))/(r(it)-n_p)
                PV_comp = comp(0, 0, ijps0, it) * (1d0+r(it))/(r(it)-n_p)
            else 
                PV_0 = (EV_0)/dEV_da   + PV_0*(1d0+n_p)/(1d0+r(it+1))
                PV_t = (EV_t)/dEV_da   + PV_t*(1d0+n_p)/(1d0+r(it+1))
                PV_comp = comp(0, 0, ijps0, it)  + PV_comp*(1d0+n_p)/(1d0+r(it+1))
            endif 
        enddo 
        
        ! Calculate the agg efficiency factor (gain/loss) for future generations 
        ! (note: LSRA either taxes from or transfers to future gens such that 
        !        EV_it(comp) = EV* where EV* is a constant. 
        !        We can show that EV_it(comp) = Lambda * EV_0 
        ! Thus, Lambda := aggregate efficiency gain/loss to future gens 
        Lambda = (PV_t - PV_comp - SV(1))/PV_0 
        
        ! Given Lambda, we calculate the compensation/tax on newborns 
        do it = TT, 1, -1
                        
            
            damp = damp_LSRA
            if (iter <= 5) damp = damp_LSRA/2d0 
            
            ! Get today's ex ante utility 
            EV_t = damp*V_coh(1, it) 
            
            ! Get target ex ante utility, EV* = Lambda * EV(it=0)  
            EV_0 = damp*Lambda*V_coh(1,0)
            
            ! derivative of expected utility function 
            dEV_da = 0d0 
            do ip = 1, NP 
                do ism = 1, NS 
                    do isf = 1, NS 
                        ijps = indexing(1, ip, ism, isf)
                        if (progressive) then
                            dEV_da = dEV_da + margu(c(0, 0, ijps, it), l(0, 0, ijps, it), 1, it, form1=1) & 
                                    * phi(0, 0, ijps, it) * (1d0+rn(it)-inctaxfunc(a(ia), linc_m, linc_f, it, mtr=3))
                        else 
                            dEV_da = dEV_da + margu(c(0, 0, ijps, it), l(0, 0, ijps, it), 1, it, form1=1) & 
                                    * phi(0, 0, ijps, it) * (1d0+rn(it))
                        endif 
                    enddo 
                enddo 
            enddo 
            
            ! compute change in compensation 
            comp_new = (EV_0 - EV_t)/dEV_da 
            
            ! calculate total compensation 
            comp(0, 0, ijps0:ijps1, it) = comp(0, 0, ijps0:ijps1, it) + comp_new 
            
            write(*, '(a)') "NEWBORN"
            write(*, '(a, 4(i3,a), f12.5)') "comp(",0,",",0,",",ijps0,",",it,") = ", comp(0, 0, ijps0, it) 

            
            ! aggregate compensation for each cohort and over all cohorts (adjusted for relative size)
            ! for each time period 
            do ip = 1, NP 
                do ism = 1, NS 
                    do isf = 1, NS 
                        ijps = indexing(1, ip, ism, isf) 
                        comp_coh(1, it) = comp_coh(1, it) + comp(0, 0, ijps0, it)*phi(0, 0, ijps, it)
                    enddo 
                enddo 
            enddo
            
            ! alternative: or comp_coh(1, it) = sum(comp(0, 0, ijp0:ijps1, it)*phi(0, 0, ijps0:ijps1, it))
            ! But, since sum(phi(0, 0, ijp0:ijps1, it)) = 1d0 for all it, and given that comp(0, 0, ijp0:ijps1, it) is constant 
            !      then, we can just use the original form right away. 
            
            ! original: comp_coh(1, it) = comp(0, 0, ijps0, it) 
            SV(it) = SV(it) + comp_coh(1, it)*m(1,it)
        enddo 
        
        ! GIVEN comp(:, :), determine the budget balancing debt level of the LSRA
        ! BA(1) = 0d0 (the LSRA has no existing debt reform period) 
        BA(2) = SV(1)/(1d0+n_p) 
        write(*,'(a,i3,a,f12.5/)') 'SV(',1,') =', SV(1)  
        do it = 3, TT 
            BA(it) = ((1d0+r(it-1))*BA(it-1) + SV(it-1))/(1d0+n_p) 
            !BA(it) = min(((1d0+r(it-1))*BA(it-1) + SV(it-1))/(1d0+n_p) , 0.99d0*YY(it))

            write(*,'(a,i3,a,f12.5)') 'BA(',it,') =', BA(it)
            write(*,'(a,i3,a,f12.5/)') 'SV(',it,') =', SV(it-1)  
        enddo 
    END SUBROUTINE LSRA1
            
    
    ! Subroutine for writing output 
    SUBROUTINE output(it) 
        
        implicit none 
        integer, intent(in) :: it 
        integer :: ij, ia, ih, ip, ism, isf, iamax(JJ, 0:TT)
        
        ! OUTPUT TO CONSOLE 
        ! OUTPUT 
        write(*,'(a, i3/)')'EQUILIBRIUM YEAR ', it
        write(*,'(a)')'CAPITAL        K       A       B      BA       r    p.a.'
        write(*,'(8x,6f8.2)')KK(it), AA(it), BB(it), BA(it), r(it) , ((1+r(it))**(1d0/mperiod)-1d0)*100.0
        write(*,'(a,4f8.2/)')'(in %)  ',(/KK(it), AA(it), BB(it), BA(it)/)/YY(it)*100.0*mperiod
              
        write(*,'(a)')'LABOR          LFP      HH     INC       wm       wf'
        write(*,'(8x,5f8.2/)')LFP(it), HH(it)*100d0, INC(it), wm(it), wf(it)
              
              
        write(*,'(a)')'GOODS          Y       C       I       G    DIFF'
        write(*,'(8x,4f8.2,f8.3)')YY(it),CC(it),II(it),GG(it),diff(it)
        write(*,'(a,4f8.2,f8.3/)')'(in %)  ',(/YY(it), CC(it), II(it), GG(it), DIFF(it)/)/YY(it)*100.0
              
              
        write(*,'(a)')'GOV         TAUC    TAUW    TAUR    TR   TOTAL       G       B'
        write(*,'(8x,7f8.2)') taxrev(1:5, it), GG(it), BB(it)
        write(*,'(a,7f8.2)')'(in %)  ',(/taxrev(1:5, it), GG(it), BB(it)/)/YY(it)*100d0
        write(*,'(a,4f8.2/)')'(rate)  ',(/tauc(it), tauw(it), taur(it), Tr(it)/)*100d0
              
              
        write(*,'(a)')'PENS        TAUP     PEN      PP'
        write(*,'(8x,6f8.2)') taup(it)*(wm(it)*LLM(it)+wf(it)*LLF(it)), pen(JR, it), PP(it)
        write(*,'(a,3f8.2/)')'(in %)  ',(/taup(it), kappa(it), PP(it)/YY(it)/)*100d0
              
        write(*,'(a)')'LSRA          SV      BA'
        write(*,'(8x,2f8.2)')SV(it), BA(it)
        write(*,'(a,2f8.2/)')'(in %)  ',(/SV(it), BA(it)/)/YY(it)*100d0

        
        ! check for max gridpoint used 
        call check_grid_a(iamax, it) 
        
        write(*, '(a,a)')' IJ      CONS     LABOR  EARNINGS    INCOME    INCTAX      PENS    ASSETS', &
            '    CV(C)    CV(Y)    LSRA     VALUE     BOR_CON    IAMAX'
        
        do ij = 1, JJ 
            write(*, '(i3, 13f10.3, i10)') ij, c_coh(ij, it)/INC(0), l_coh(ij, it), (/wm(it)*ym_coh(ij,it) + wf(it)*yf_coh(ij,it), & 
                    wn_m(it)*ym_coh(ij,it)+wn_f(it)*yf_coh(ij,it)+rn(it)*a_coh(ij,it), & 
                    INCTAX(it)+taur(it)*r(it)*a_coh(ij,it), & 
                    Tr(it), &
                    pen(ij,it)-taup(it)*(wm(it)*ym_coh(ij,it) + wf(it)*yf_coh(ij,it)), a_coh(ij,it)/)/INC(0), & 
                    cv_c(ij, it), cv_y(ij, it), & 
                    comp_coh(ij, it), V_coh(ij,it), bor_cons(ij,it), iamax(ij, it) 
        enddo 
        
        
        write(*,'(a/)')'--------------------------------------------------------------------'
        
        
        
        ! OUTPUT TO FILE 1 
        write(1,'(a, i3/)')'EQUILIBRIUM YEAR ', it
        write(1,'(a)')'CAPITAL        K       A       B      BA       r    p.a.'
        write(1,'(8x,6f8.2)')KK(it), AA(it), BB(it), BA(it), r(it) , ((1+r(it))**(1d0/mperiod)-1d0)*100.0
        write(1,'(a,4f8.2/)')'(in %)  ',(/KK(it), AA(it), BB(it), BA(it)/)/YY(it)*100.0*mperiod
        
        write(1,'(a)')'LABOR          LFP      HH     INC       wm       wf'
        write(1,'(8x,5f8.2/)')LFP(it), HH(it)*100d0, INC(it), wm(it), wf(it)

        
        write(1,'(a)')'GOODS          Y       C       I       G    DIFF'
        write(1,'(8x,4f8.2,f8.3)')YY(it),CC(it),II(it),GG(it),diff(it)
        write(1,'(a,4f8.2,f8.3/)')'(in %)  ',(/YY(it), CC(it), II(it), GG(it), DIFF(it)/)/YY(it)*100.0

        
        write(1,'(a)')'GOV         TAUC    TAUW    TAUR    TR   TOTAL       G       B'
        write(1,'(8x,7f8.2)') taxrev(1:5, it), GG(it), BB(it)
        write(1,'(a,7f8.2)')'(in %)  ',(/taxrev(1:5, it), GG(it), BB(it)/)/YY(it)*100d0
        write(1,'(a,4f8.2/)')'(rate)  ',(/tauc(it), tauw(it), taur(it), Tr(it)/)*100d0
        
        
        write(1,'(a)')'PENS        TAUP     PEN      PP'
        write(1,'(8x,6f8.2)') taup(it)*(wm(it)*LLM(it)+wf(it)*LLF(it)), pen(JR, it), PP(it)
        write(1,'(a,3f8.2/)')'(in %)  ',(/taup(it), kappa(it), PP(it)/YY(it)/)*100d0

        write(1,'(a)')'LSRA          SV      BA'
        write(1,'(8x,2f8.2)')SV(it), BA(it)
        write(1,'(a,2f8.2/)')'(in %)  ',(/SV(it), BA(it)/)/YY(it)*100d0

        
        ! check for max gridpoint used 
        call check_grid_a(iamax, it) 
        
        write(1, '(a,a)')' IJ      CONS     LABOR  EARNINGS    INCOME    INCTAX      PENS    ASSETS', &
            '    CV(C)    CV(Y)    LSRA     VALUE     BOR_CON    IAMAX'
        
        do ij = 1, JJ 
            write(1, '(i3, 13f10.3, i10)') ij, c_coh(ij, it)/INC(0), l_coh(ij, it), (/wm(it)*ym_coh(ij,it) + wf(it)*yf_coh(ij,it), & 
                    wn_m(it)*ym_coh(ij,it)+wn_f(it)*yf_coh(ij,it)+rn(it)*a_coh(ij,it), & 
                    tauw(it)*(wm(it)*ym_coh(ij,it) + wf(it)*yf_coh(ij,it))+taur(it)*r(it)*a_coh(ij,it), & 
                    Tr(it), &
                    pen(ij,it)-taup(it)*(wm(it)*ym_coh(ij,it) + wf(it)*yf_coh(ij,it)), a_coh(ij,it)/)/INC(0), & 
                    cv_c(ij, it), cv_y(ij, it), & 
                    comp_coh(ij, it), V_coh(ij,it), bor_cons(ij,it), iamax(ij, it) 
        enddo 
        
        
        write(1,'(a/)')'--------------------------------------------------------------------'

    
    END SUBROUTINE output 
    
    
    ! SUBROUTINE FOR PLOTTING RESULTS 
    SUBROUTINE plotting1(it, it1, it2) 
    
        implicit none 
        integer, intent(in) :: it
        integer, intent(in), optional :: it1, it2
        character(len=2) :: period
        integer :: ages(JJ), times(0:TT), ij, ia, ih, ip, ism, isf, ijps
        !real*8 :: exit_LF(JJ) 
        
        ages = 20 + mperiod*(/(ij, ij = 1, JJ)/) 
        
        times = (/(it, it=0, TT)/) 
        
        ! CONSUMPTION AND HOUSEHOLD NON-CAPITAL EARNINGS
        call plot(dble(ages), c_coh(:, it), legend='Init SS: Consumption')
        call plot(dble(ages), c_coh(:, it)/sqrt(2d0+nchild), legend='Init SS: Normalized Consumption')
        call plot(dble(ages), ym_coh(:, it) + yf_coh(:, it) + pen(:, it) + Tr(it), legend='Init SS: Total Non-Capital Earnings')
        if (present(it1)) then 
            call plot(dble(ages), c_coh(:, it1), legend='Final SS: Consumption')
            call plot(dble(ages), c_coh(:, it1)/sqrt(2d0+nchild), legend='Final SS: Normalized Consumption')
            call plot(dble(ages), ym_coh(:, it1) + yf_coh(:, it1) + pen(:, it1) + Tr(it1), &
                                    legend='Final SS: Total Non-Capital Earnings')
        endif 
        if (present(it2)) then 
            write(period, '(i2)') it2
            call plot(dble(ages), c_coh(:, it2), legend='Transition: Consumption, it = '//trim(period))
            call plot(dble(ages), c_coh(:, it2)/sqrt(2d0+nchild), legend='Transition: Normalized Consumption, it = '//trim(period))
            call plot(dble(ages), ym_coh(:, it2) + yf_coh(:, it2) + pen(:, it2) + Tr(it2), &
                                    legend='Transition: Total Non-Capital Earnings, it = '//trim(period))
        endif     
        call execplot(xlabel='Age j', ylabel = 'Mean', ylim = (/0d0, 5d0/))

        
        ! LABOUR EARNINGS 
        call plot(dble(ages), ym_coh(:, it), legend='Init SS: Male')
        call plot(dble(ages), yf_coh(:, it), legend='Init SS: Female')
        call plot(dble(ages), y_coh(:, it), legend='Init SS: Household')
        if (present(it1)) then 
            call plot(dble(ages), ym_coh(:, it1), legend='Final SS: Male')
            call plot(dble(ages), yf_coh(:, it1), legend='Final SS: Female')
            call plot(dble(ages), y_coh(:, it1), legend='Final SS: Household')
        endif 
        if (present(it2)) then 
            write(period, '(i2)') it2 
            call plot(dble(ages), ym_coh(:, it2), legend='Transition: Male, it = '//trim(period))
            call plot(dble(ages), yf_coh(:, it2), legend='Transition: Female, it = '//trim(period))
            call plot(dble(ages), y_coh(:, it2), legend='Transition: Household, it = '//trim(period))
        endif 
        call execplot(xlabel='Age j', ylabel='Earnings', ylim = (/0d0, 5d0/))

        ! FTB A and FTB B 
        call plot(dble(ages), FTBA_coh(:, it), legend='Init SS: FTB A')
        call plot(dble(ages), FTBB_coh(:, it), legend='Init SS: FTB B')
        if (present(it1)) then 
            call plot(dble(ages), FTBA_coh(:, it1), legend='Final SS: FTB A')
            call plot(dble(ages), FTBB_coh(:, it1), legend='Final SS: FTB B')
        endif 
        if (present(it2)) then 
            write(period, '(i2)') it2 
            call plot(dble(ages), FTBA_coh(:, it2), legend='Transition: FTB A, it = '//trim(period))
            call plot(dble(ages), FTBB_coh(:, it2), legend='Transition: FTB B, it = '//trim(period))
        endif 
        call execplot(xlabel='Age j', ylabel='FTB', ylim = (/0d0, 5d0/))

        
        ! FEMALE LABOUR FORCE PARTICIPATION RATE
        call plot(dble(ages), l_coh(:, it), legend='Init SS: female LFP rate')
        if (present(it1)) then 
            call plot(dble(ages), l_coh(:, it1), legend='Final SS: female LFP rate')
        endif 
        if (present(it2)) then 
            write(period, '(i2)') it2 
            call plot(dble(ages), l_coh(:, it2), legend='Transition: female LFP rate, it = '//trim(period))
        endif 
        call execplot(xlabel='Age j', ylabel='Female Labour Force Participation', ylim=(/0d0, 1d0/))

        
        ! HUMAN CAPITAL 
        call plot(dble(ages), eff(1:JJ), legend='Init SS: Men')
        call plot(dble(ages), h_coh(1:JJ, it), legend='Init SS: Women')
        if (present(it1)) then 
            call plot(dble(ages), eff(1:JJ), legend='Final SS: Men')
            call plot(dble(ages), h_coh(1:JJ, it1), legend='Final SS: Women')
        endif 
        if (present(it2)) then 
            write(period, '(i2)') it2 
            call plot(dble(ages), eff(1:JJ), legend='Transition: Men, it = '//trim(period))
            call plot(dble(ages), h_coh(1:JJ, it2), legend='Transition: Women, it = '//trim(period))
        endif 
        call execplot(xlabel='Age j', ylabel='Human Capital', ylim = (/0d0, 2d0/))

        
        ! ASSET HOLDINGS 
        call plot(dble(ages), a_coh(:, it)/INC(0), legend='Init SS: Asset holdings') 
        if (present(it1)) then 
            call plot(dble(ages), a_coh(:, it1)/INC(0), legend='Final SS: Asset holdings')
        endif 
        if (present(it2)) then 
            write(period, '(i2)') it2 
            call plot(dble(ages), a_coh(:, it2), legend='Transition: Asset holdings, it = '//trim(period)) 
        endif 
        call execplot(xlabel='Age j', ylabel = 'Assets/INC(0)')

        ! fraction of borrowing constrained households 
        !bor_cons(:, it) = 0d0 
        !do ij = 1, JJ-1 
        !    do ia = 0, NA 
        !        do ih = 0, NH 
        !            do ip = 1, NP 
        !                do ism = 1, NS 
        !                    do isf = 1, NS 
        !
        !                        if(aplus(ij, ia, ih, ip, ism, isf, it) < 1d-6)then 
        !                            bor_cons(:, it) = bor_cons(:, it) + phi(ij, ia, ih, ip, ism, isf, it)
        !                        endif
        !                        
        !                    enddo !isf
        !                enddo !ism
        !            enddo !ip
        !        enddo !ih
        !    enddo !ia
        !enddo !ij
        !
        !bor_cons(JJ, it) = 1d0

        call plot(dble(ages(1:JJ-1)), bor_cons(1:JJ-1, it), legend='Init SS')
        if (present(it1)) then 
            call plot(dble(ages(1:JJ-1)), bor_cons(1:JJ-1, it1), legend='Final SS')
        endif 
        if (present(it2)) then 
            write(period, '(i2)') it2
            call plot(dble(ages(1:JJ-1)), bor_cons(1:JJ-1, it2), legend='Transition, it = '//trim(period))
        endif 
        call execplot(xlabel='Age j', ylabel='Frac. Borrowing Constrained Households')


        ! fraction of women exiting the labour force
        !exit_LF = 0d0 
        !do ij = 1, JJ
        !    do ia = 0, NA 
        !        do ih = 0, NH 
        !            do ip = 1, NP 
        !                do ism = 1, NS 
        !                    do isf = 1, NS 
        !                        ! indexing 
        !                        ijps = indexing(ij, ip, ism, isf) 
        !                        if(l(ia, ih, ijps, it) < 1d-10)then 
        !                            exit_LF(ij) = exit_LF(ij) + phi(ia, ih, ijps, it) 
        !                        endif
        !                        
        !                    enddo !isf
        !                enddo !ism
        !            enddo !ip
        !        enddo !ih
        !    enddo !ia
        !enddo !ij
        !
        !
        !call plot(dble(ages(1:JJ)), exit_LF(1:JJ), legend='Pre-reform')
        !if present(it1) then 
        !     call plot(dble(ages(1:JJ)), exit_LF(1:JJ), legend='Post-reform')
        !endif 
        !call execplot(xlabel='Age j', ylabel='Frac. of Women Exiting Labour Force')
        
        ! Coefficients of Variation
        call plot(dble(ages), cv_c(:, it), legend='Init SS: CV consumption')
        call plot(dble(ages), cv_y(:, it), legend='Init SS: CV output') 
        if (present(it1)) then 
            call plot(dble(ages), cv_c(:, it1), legend='Final SS: CV consumption')
            call plot(dble(ages), cv_y(:, it1), legend='Final SS: CV output') 
        endif 
        if (present(it2)) then 
            write(period, '(i2)') it2
            call plot(dble(ages), cv_c(:, it2), legend='Transition: CV consumption, it = '//trim(period))
            call plot(dble(ages), cv_y(:, it2), legend='Transition: CV ouptut, it = '//trim(period))
        endif 
        call execplot(xlabel='Age j', ylabel='Coefficient of Variation')


      

    END SUBROUTINE plotting1 
    
    
    
    SUBROUTINE plotting2()
    
        implicit none 
        integer :: ages(JJ), times(2-JJ:TT), ij, it
        
        ages = 20 + mperiod*(/(ij, ij = 1, JJ)/) 
        
        times = (/(it, it=2-JJ, TT)/) 
        
        ! plot output 
        call plot(dble(times(0:TT)), CC(:), legend='Consumption')
        call plot(dble(times(0:TT)), AA(:), legend='Assets')
        call plot(dble(times(0:TT)), LL(:), legend='Total Labour (efficiency unit)') 
        call plot(dble(times(0:TT)), LFP(:), legend='female LFP') 
        call execplot(xlabel='Time t', ylabel = 'Mean') 
        
        ! international capital trade balance 
        call plot(dble(times(0:TT)), ITB(:), legend='International Capital Trade Balance') 
        call execplot(xlabel='Time t', ylabel = 'Mean') 
        
        ! factor prices 
        call plot(dble(times(0:TT)), r(:), legend='rental rate') 
        call plot(dble(times(0:TT)), wm(:), legend='wage rate') 
        call execplot(xlabel='Time t', ylabel = 'Rate') 
        
        ! , ylim = (/0d0, 5d0/))
        
        ! HEV (aggregate efficiency before and after LSRA resource reallocation) 
        call plot(dble(times), HEV(:,1), legend = 'HEV without LSRA') 
        call plot(dble(times), HEV(:,2), legend = 'HEV with LSRA') 
        call plot(dble(times), dble(times)*0d0, color = 'black') ! a straight line at zero 
        call execplot(xlabel='Cohort entry year', ylabel = 'Hicksian Equivalent Variation')
        
        
        ! plot histogram 
        call plot_hist(HEV(:,1), 10, legend = "HEV without LSRA")
        call execplot(xlabel='HEV', ylabel = 'Density')

        ! (for intensive margin problem only) 
        ! Frisch elasticity of labour supply 
        ! (elasticity of labour supply with respect to wage holding 
        ! marginal utility of consumption or wealth (the multiplier lambda) 
        ! constant)
        !frisch = (gamma - nu*(gamma-1d0))*(1d0-l_coh(:, 0))/l_coh(:,0)
        !
        !call plot(dble(ages), frisch(:), marker = 1, color='black', linewidth=2.5d0, legend = 'Frisch elasticity')
        !call execplot(xlabel='Age j', ylabel='Frisch elasticity')
        
  
    END SUBROUTINE plotting2
    
    
    ! write output summary
    SUBROUTINE output_summary()
        
        implicit none 
        integer :: ij, ij1, ia, ih, ip, ism, isf, it, ijps
        real*8 :: HEV1(2-JJ:TT), HEV_help, mas(2-JJ:0), chi_0, chi_1, chi_t
        real*8 :: V1, V0
        
        ! Total ex-post welfare changes of current generation 

        HEV1 = 0d0 
        mas = 0d0 
        chi_0 = 0d0 
        chi_1 = 0d0 
         
        ! old gen in reform period (it==1)
        do ij = JJ, 2, -1 
            do ia = 0, NA 
                do ih = 0, NH 
                    do ip = 1, NP 
                        do ism = 1, NS 
                            do isf = 1, NS               
                                
                                if (ij >= JR .and. ia==0 .and. (kappa(0) <= 1d-10 .or. kappa(1) <= 1d-10)) then 
                                    CYCLE 
                                endif 
                                
                                !chi_0 = 0d0 
                                !chi_1 = 0d0
                                !do ij1 = ij, JJ
                                    !ijps = indexing(ij1, ip, ism, isf)
                                    !ijps = indexing(ij, ip, ism, isf)
                                    !chi_0 = chi_0 + l(ia, ih, ijps, 0)*phi(ia, ih, ijps, 0)*chi  
                                    !chi_1 = chi_1 + l(ia, ih, ijps, 1)*phi(ia, ih, ijps, 1)*chi
                                !enddo 
                                    
                                !ijps = indexing(ij, ip, ism, isf)
                                !chi_0 = l(ia, ih, ijps, 0)*phi(ia, ih, ijps, 0)*chi  
                                !chi_1 = l(ia, ih, ijps, 1)*phi(ia, ih, ijps, 1)*chi
                                !chi_0 = 0d0 
                                !chi_1 = 0d0
                                !! indexing 
                                ijps = indexing(ij, ip, ism, isf)
                                
                                ! compute agg efficiency 
                                !HEV_help = ( ((V(ia, ih, ijps, 1)+chi_1)/(max(V(ia,ih,ijps,0)+chi_0, -1d10)))**(1d0/egam) & 
                                !                    - 1d0 )*100d0 
                                
                                HEV_help = ( (V(ia, ih, ijps, 1)/max(V(ia,ih,ijps,0), -1d10))**(1d0/egam) & 
                                                            - 1d0 )*100d0 
                                !V1 = V(ia, ih, ijps, 1)
                                !V0 = V(ia,ih,ijps,0)
                                !if (V1*V0 >= 0d0) then ! same sign 
                                !    if (V1 >= 0d0) then 
                                !        HEV_help = -( (V(ia, ih, ijps, 1)/max(V(ia,ih,ijps,0), 1d-10))**(1d0/egam) & 
                                !                            - 1d0 )*100d0 
                                !    else 
                                !        HEV_help = ( (V(ia, ih, ijps, 1)/min(V(ia,ih,ijps,0), -1d10))**(1d0/egam) & 
                                !                            - 1d0 )*100d0 
                                !    endif 
                                !    
                                !else 
                                !    error stop "V1 and V2 are of opposite signs"
                                !endif 
                                    
                                HEV1(2-ij) = HEV1(2-ij) + HEV_help*phi(ia, ih, ijps, 1) 
                                mas(2-ij) = mas(2-ij) + phi(ia, ih, ijps, 1) 
                                
                               ! write(*, '(a, 3(i3,a), f12.5)') "V(",ia,",",ih,",",ijps,",1) = ", V(ia, ih, ijps, 1) 
                               ! write(*, '(a, 3(i3,a), f12.5)') "V(",ia,",",ih,",",ijps,",0) = ", V(ia, ih, ijps, 0)
                        
                            enddo 
                        enddo
                    enddo
                enddo
            enddo
        enddo
        
        HEV1(2-JJ:0) = HEV1(2-JJ:0)/mas 
        
        
        ! calculate ex ante welfare for future generations 
        !chi_0 = sum(l(:,:,:,0)*phi(:,:,:,0)*chi)
        !chi_0 = 0d0 
        !chi_t = 0d0 
        !do it = 1, TT 
        !    !chi_t = sum(l(:,:,:,it)*phi(:,:,:,it)*chi)
        !    HEV1(it) = ( ((V_coh(1,it)+chi_t)/(V_coh(1,0)+chi_0))**(1d0/egam) - 1d0 )*100d0 
        !enddo 
        
        do it = 1, TT 
            !chi_t = sum(l(:,:,:,it)*phi(:,:,:,it)*chi)
            HEV1(it) = ( (V_coh(1,it)/V_coh(1,0))**(1d0/egam) - 1d0 )*100d0 
        enddo
        
        ! Store HEV results 
        if (.not. lsra_on) then
            HEV(:, 1) = HEV1
        else 
            HEV(:, 2) = HEV1
        endif 
         
        
        ! headline (write to console)
        write(*,'(/a,a)')'DEV:      A       K       L       H       r       wm', &
            '       C       I       Y       B    tauc    tauw    taur    taup     Tr    HEV    DIFF'
       
        
        do ij = 2-JJ, -1
            write(*,'(i3,125x,f8.2)')ij,HEV1(ij)
        enddo

                
        do ij = 2-JJ, -1
            write(22,'(i3,125x,f8.2)')ij,HEV1(ij)
        enddo
        

        
        ! print deviations from the initial steady state for newborns across time 
        do it = 0, TT 
            write(*, '(i3, 17f8.2, f10.5)') it, (/ AA(it)/AA(0)-1d0, KK(it)/KK(0)-1d0, LFP(it)-LFP(0), & 
                    HH(it)/HH(0)-1d0, r(it)-r(0), wm(it)/wm(0)-1d0, CC(it)/CC(0)-1d0, II(it)/II(0)-1d0, & 
                    YY(it)/YY(0)-1d0, BB(it)/BB(0)-1d0, tauc(it)-tauc(0), tauw(it)-tauw(0), & 
                    taur(it)-taur(0), taup(it)-taup(0), Tr(it)/Tr(0)-1, HEV1(it), DIFF(it)/YY(it) /)*100.0
        enddo 
        
        
        
        ! headline (write to file) 
        write(2,'(/a,a)')'          A       K       L       H       r       wm', &
            '       C       I       Y       B    tauc    tauw    taur    taup     Tr    HEV    DIFF'
        
        ! print deviations from the initial steady state for newborns across time 
        do it = 0, TT 
            write(2, '(i3, 17f8.2, f10.5)') it, (/ AA(it)/AA(0)-1, KK(it)/KK(0)-1, LFP(it)-LFP(0), & 
                    HH(it)/HH(0)-1, r(it)-r(0), wm(it)/wm(0)-1, CC(it)/CC(0)-1, II(it)/II(0)-1, & 
                    YY(it)/YY(0)-1, BB(it)/BB(0)-1, tauc(it)-tauc(0), tauw(it)-tauw(0), & 
                    taur(it)-taur(0), taup(it)-taup(0), Tr(it)/Tr(0)-1, HEV1(it), DIFF(it)/YY(it) /)*100.0
        enddo 
        
        

    END SUBROUTINE output_summary
    
    ! subroutine that checks for max gridpoint used
    SUBROUTINE check_grid_a(iamax, it) 
    
        implicit none 
        integer :: iamax(JJ,0:TT), it, ij, ia, ih, ip, ism, isf, ijps 
        
        iamax(:, it)= 0 
        
        do ij = 1, JJ 

            ! check for the maximum asset grid point used at a certain age
            do ia = 0, NA
                do ih = 0, NH
                    do ip = 1, NP
                        do ism = 1, NS
                            do isf = 1, NS
                                ! indexing 
                                ijps = indexing(ij, ip, ism, isf) 
                                if(phi(ia, ih, ijps, it) > 1d-8)iamax(ij, it) = ia
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo 
    
    END SUBROUTINE check_grid_a
    
    
   SUBROUTINE check_grid_h(ihmax, it) 
    
        implicit none 
        integer :: ihmax(JJ,0:TT), it, ij, ia, ih, ip, ism, isf, ijps
        
        ihmax(:, it)= 0 
        
        do ij = 1, JJ 

            ! check for the maximum asset grid point used at a certain age
            do ih = 0, NH
                do ia = 0, NA
                    do ip = 1, NP
                        do ism = 1, NS
                            do isf = 1, NS
                                ! indexing 
                                ijps = indexing(ij, ip, ism, isf) 
                                if(phi(ia, ih, ijps, it) > 1d-8)ihmax(ij, it) = ih
                            enddo
                        enddo
                    enddo
                enddo
            enddo
        enddo 
    
    END SUBROUTINE check_grid_h 
    
    
    
END PROGRAM MODEL2
