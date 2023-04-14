        /*%NOCOMMENT -------------  rexx procedure  ----------------- */
         _version_ = "3.4"
        /* Name:      lstproc                                         *
         *                                                            *
         * Function:  Display active JES2 proclibs                    *
         *                                                            *
         * Syntax:    %lstproc proc                                   *
         *                                                            *
         *            Where proc is the name of the JES2 proc or      *
         *            blank to default to JES2.                       *
         *                                                            *
         * Usage:     Under ISPF display the ISRDDN display filtered  *
         *            with the PROC DDnames ($PROC)                   *
         *                                                            *
         *            Outside of ISPF use the echo TSO command to     *
         *            display the info (echo is trappable while say   *
         *            is not).  ECHO can be found in File312 on the   *
         *            CBTTape site as EKKO.                           *
         *                                                            *
         *            If invoked as a function (e.g. x=lstproc)       *
         *            then act as if under native TSO and not ISPF.   *
         *                                                            *
         * Assumptions and Caveats:                                   *
         *            1. if proc not specified then use JES2          *
         *            2. only //PROCxx will be looked at              *
         *            3. comments will be ignored                     *
         *            4. symbolics are supported (mostly)             *
         *            5. system symbolics are supported               *
         *            6. only cataloged datasets are supported        *
         *            7. JES2 Dynalloc procs are not supported        *
         *            8. nested symbolics may not work                *
         *            9. if the mstrjcl or jes2 proc changes that     *
         *               is what you will see reflected               *
         *           10. This does NOT report the actual proc usage   *
         *               but what is found from mstrjcl and the       *
         *               jes2 proc.                                   *
         *           11. *IMPORTANT* It is assumed you have RACF      *
         *               read for the LISTDATA IDCAMS command.        *
         *               If not find *LISTDATA* and make the          *
         *               noted changes.  You will then be limited     *
         *               to the cataloged iplparm dataset.            *
         *           12. SDSF REXX is used to capture JES2 defined    *
         *               Proclibs.                                    *
         *                                                            *
         * Author:    Lionel B. Dyck                                  *
         *            Internet: lbdyck@gmail.com                      *
         *                                                            *
         * History:                                                   *
         *                                                            *
         *          2023-04-10 - Check for invocation as Function     *
         *                       and set mode as non-ISPF             *
         *          2023-03-31 - Correct symbolic replacement         *
         *          2023-03-29 - Add dataset in IEFJOBS from MSTRJCL  *
         *                       and allocated to $PROCJOB            *
         *                     - Dynamically get JES2 Defined Procs   *
         *          2023-03-28 - Correct symbolic processing          *
         *                       if symbolic starts with .&           *
         *          2021-09-10 - Updates from Ray Mullins to fix 0C4  *
         *                       when compiled                        *
         *          2014-01-31 - Don't alloc/open/read the iplparm    *
         *                       if it can't be found                 *
         *          2009-07-08 - Change # to $ (thx hartmut)          *
         *          2009-07-08 - MSG("OFF") in FREE routine           *
         *                     - use _junk_ instead of RC             *
         *          2009-05-11 - Check whether proclib member exists  *
         *                       SYS1.PROCLIB(JES2) default may fail. *
         *          2008-11-05 - Correction in handling symbols       *
         *                       custplex and lpar                    *
         *          2008-10-15 - Info about old sysmvs                *
         *                     - Note:                                *
         *                       LISTDATA/LDATA in AUTHCMD in IKJTSO00*
         *                     - substitute vars  left  and  right    *
         *                               by vars _left_ and _right_   *
         *                                                            *
         *          2007-10-16 - If not under ISPF just write         *
         *                       dd and dsnames to screen             *
         *                                                            *
         *          2007-02-20 - Correction from Ian Ramage           *
         *                       Fix if JES2 Proc in >2 proclib       *
         *                                                            *
         *          2006-11-27 - Hartmut Beckmann                     *
         *                       Changes for better member support    *
         *                                                            *
         *          2006-09-25 - Peggy Norton                         *
         *                       Fix call to subroutine for symbolics *
         *                                                            *
         *          2006-05-17 - Jeff Dixon                           *
         *                       Support //PROCxxxx instead of PROCxx *
         *                                                            *
         *          2006-05-11 - Peggy Norton                         *
         *                       Fix if LOADxx does not have defined  *
         *                       SYS1.PARMLIB                         *
         *                                                            *
         *          2006-01-26 - Ian Ramage                           *
         *                       Add Numeric Digits 10 to resolve     *
         *                       issue under z/OS 1.6                 *
         *                                                            *
         *          2003-09-02 - John Bloniarz                        *
         *                       McDonald's Corporation               *
         *                       630-623-3224                         *
         *                       john.bloniarz@mcd.com                *
         *                                                            *
         *                       make callable as a function from     *
         *                       REXX to allocate system Parmlibs,    *
         *                       system Proclibs and/or PROCxx's.     *
         *                       Changed DD name $PROCMST to          *
         *                       $PROCLIB when called as a function.  *
         *                                                            *
         *          2003-08-28 - John Bloniarz                        *
         *                       McDonald's Corporation               *
         *                       630-623-3224                         *
         *                       john.bloniarz@mcd.com                *
         *                                                            *
         *                       add code to resolve all system       *
         *                       symbols before allocation.           *
         *                                                            *
         *          2002-08-19 - correct to work with OS/390 below    *
         *                       2.10 (broke on 8/18 change)          *
         *          2002-08-18 - pass ONLY $PROC to ISRDDN            *
         *                       and bypass former ispf msg           *
         *          2001-08-15 - support INCLUDE in the Proc          *
         *          2001-04-25 - fix from Iam Ramage to correctly     *
         *                       find the parmlib volser.             *
         *                       ian.ramage@rs-components.com         *
         *          2001-02-21 - fix if proc symbol has quotes        *
         *          2000-10-24 - update to get iplparm vol dyn        *
         *                       thx to Philippe Richard of IBM       *
         *          2000-10-23 - update to get iplparm dsn dyn        *
         *                       thx to Philippe Richard of IBM       *
         *          2000-06-20 - general release                      *
         *                     - Change dds to $PROCMST & $PROCnn     *
         *          2000-06-15 - minor clean up                       *
         *          2000-06-14 - various changes                      *
         *                     - add code to dynamically find mstjclxx*
         *                       from Todd Burrell (zpn6@cdc.gov)     *
         *          2000-06-13 - creation                             *
         *                                                            *
         * ---------------------------------------------------------- *
         *                                                            *
         * ---------------------------------------------------------- */
         signal on novalue name sub_novalue
        /* --------------------------------- *
         | Change if SDSF REXX not available |
         * --------------------------------- */
         SDSF_JES2_PROCS = 'Y'    /* Change to N if no SDSF REXX */
        /* ----------------------------------------------------- *
         * Setup defaults in sub routine                         *
         * ----------------------------------------------------- */
         _x_ = sub_init() ;
        /* -------------------------------------------- *
         * Check for any passed options. The only       *
         * supported option at this time is the name of *
         * the JES2 proc to look for.                   *
         * -------------------------------------------- */
         arg options

         ddn = "JCL"random(9999)
         Numeric Digits 10

        /* ----------------------------------------------------- *
         * test options and use for procname or use JES2         *
         * ----------------------------------------------------- */
        if substr(options,1,1) = "." then do
           parse value options with "." lstprocm .
           options = null
        end
        if words(options) > 1 then do
           parse value options with opt1 " " opt2 " " opt3 " " opt4
           if opt1 = "*" then do
              options = null
              end
            else do
              options = opt1
              end
           end
         else do
           if options = "*" then do
              options = null
              end
           end
        if options = null then jes2 = "JES2"
                          else jes2 = options
        isfunc = "N"
        allocdd = null
        retstr  = null
        if length(opt2) > 0 then do
           if abbrev("FUNCTION",opt2,1) = 1 then do
              isfunc = "Y"
              if length(opt3) > 0 then do
                 allocdd = opt3
                 end
              end
           end

        /* ----------------------------- *
         * Test for ISPF and if not then *
         * set flag to only echo Parmlib *
         * info to the "screen".         *
         * ----------------------------- */
         parse source x1 env .
         if env /= 'FUNCTION' then do
         sysispf = sysvar("sysispf")
         if sysispf = "ACTIVE" then ispf = 1
                               else do
                                    ispf = 0
                                    isfunc = "F"
                                    end
         end
         else do
              ispf = 0
              isfunc = 'F'
              end
        /* ----------------------------------------------------- *
         * Code from Todd Burrell to get mstjcl info             *
         * Enhanced by Ian Ramage                                *
         * ----------------------------------------------------- */
        CVT=STORAGE("10",4)
        CVTECVT=STORAGE(D2X(C2D(CVT)+140),4)
        /* GET THE IPL LOADPARMS */
        LOADPARM=STORAGE(D2X(C2D(CVTECVT)+168),8)
        /* address of IHAIPA control block */
        CVTIPA=STORAGE(D2X(C2D(CVTECVT)+392),4)
        /* mstjcl section in IPA */
        MJC=STORAGE(D2X(C2D(CVTIPA)+2448),4)
        /* mstjcl section in IPA length */
        MJCLEN=C2D(STORAGE(D2X(C2D(CVTIPA)+2452),2))
        /* ieasys source in mstjclxx */
        MJCSYS=STORAGE(D2X(C2D(CVTIPA)+2454),2)
        /* mstjcl xx value */
        MJCXX=STORAGE(D2X(C2D(MJC)+0),MJCLEN)
        if left(mjcxx,1) = "(" then
            parse value mjcxx with "(" mjcxx ")" .

         /* address of IHAIPA control block */
         IPLPARM  = Strip(LOADPARM)
         LOADADR  = Substr(IPLPARM,1,4)

        /* mstjcl section in IPA */
        ipalpar  = storage(d2x(c2d(cvtipa)+32), 8)
        ipalpdsn = storage(d2x(c2d(cvtipa)+48),44)
        parmaddr = storage(d2x(c2d(cvtipa)+92), 4)

         /* *LOADDATA* - Important information   *
          *  if you don't have RACF Read for the *
          *  LISTDATA IDCAMS command then set    *
          *  LOADVOL = ""                        */
         /*  LOADVOL  = chk_volser(parmaddr)   */
        loadvol  = null
        lparm = ""strip(IPALPDSN)"(load"substr(loadparm,5,2)")"
        lparm = ""_apost_""lparm""_apost_""

        /* ----------------------------------------------------- *
         * Read loadparm member to find parmlibs                 *
         * ----------------------------------------------------- */
         if loadvol <> null then do
            "Alloc f("ddn") ds("lparm") shr reuse volume("LOADVOL")",
               "unit(sysallda)"
            "Execio * diskr" ddn "(finis stem in."
            'Free f('ddn')'
            end
         else do
            if sysdsn(lparm) = 'OK' then do
               "Alloc f("ddn") ds("lparm") shr reuse"
               "Execio * diskr" ddn "(finis stem in."
               'Free f('ddn')'
               end
               else do
                    in.0 = 0
                    rc   = 4
                    end
            end
         rcode = rc
         _LPAR_ = "true"
         do i = 1 to in.0
            if word(in.i,1) = "LPARNAME" then
               if word(in.i,2) = ipalpar then _LPAR_ = "true"
                                         else _LPAR_ = "false"
            if word(in.i,1) = "PARMLIB" & _LPAR_ = "true" then
               parmlibs = parmlibs word(in.i,2)
            end
        if parmlibs = null then parmlibs = "SYS1.PARMLIB"
        if wordpos("SYS1.PARMLIB",parmlibs) = 0 then     /* PN */
           parmlibs = parmlibs" SYS1.PARMLIB"            /* PN */

        /* ----------------------------------------------------- *
         * Alloc all Parmlibs to DD: #PARMLIB (If Appropriate)   *
         * ----------------------------------------------------- */
         if isfunc = "Y" then do
            if allocdd = sub_allocdd("#PARMLIB") then do
               alloc_parms = null
               do i = 1 to words(parmlibs)
                  parm  = ""_apost_""word(parmlibs,i)""_apost_""
                  alloc_parms = alloc_parms parm
                  end
               if pos("&",alloc_parms) > 0 then do
                  symtext = alloc_parms
                  call fix_sym1
                  alloc_parms = symtext
                  end
               _junk_ = msg("OFF")
               "Free  f(#PARMLIB)"
               _junk_ = msg("ON")
               "Alloc f(#PARMLIB) ds("alloc_parms") shr reuse"
               retstr = strip(retstr "#PARMLIB")
               end
            if allocdd <> null &,
               allocdd = retstr then signal Finish
            end

        /* ----------------------------------------------------- *
         * Find PARMLIB with MSTJCLxx                            *
         * ----------------------------------------------------- */
         do i = 1 to words(parmlibs)
            mstrjcl = ""strip(word(parmlibs,i))"(MSTJCL"mjcxx")"
            mstrjcl = ""_apost_""mstrjcl""_apost_""
            if "OK" = sysdsn(mstrjcl) then leave
            end

        /* ----------------------------------------------------- *
         * Alloc and read master jcl parmlib member              *
         * ----------------------------------------------------- */
        if "OK" <> sysdsn(mstrjcl) then do
           in.0 = 1
           in.1 = "//IEFPDSI DSN=SYS1.PROCLIB  "
           end
        else do
             if pos("&",mstrjcl) > 0 then do
                symtext = mstrjcl
                call fix_sym1
                mstrjcl = symtext
                end
             "Alloc f("ddn") ds("mstrjcl") shr reuse"
             "Execio * diskr" ddn "(finis stem in."
             rcode = rc
             "Free  f("ddn")"
             end

        /* ----------------------------------------------------- *
         * Find all proclibs in IEFPDSI and save them            *
         * Updates by ian.ramage@rs-components.com               *
         * ----------------------------------------------------- */
        hit = 0
        mstdd = null
        do i = 1 to in.0
          in.i  = TRANSLATE(in.i," ",",")
           if hit = 1 then do
              call fix_sym
              if substr(in.i,3,1) = " "
                 then do
                      parse value in.i with . "DSN=" dsn " " .
                      if mstdd /= 'IEFJOBS'
                        then proclibs = proclibs dsn
                        else procjobs = procjobs dsn
                      end
                 else hit = 0
              end
           if left(in.i,9) = "//IEFJOBS" then do        /* @BK */
              mstdd = 'IEFJOBS'                         /* @LD */
              hit = 1                                   /* @BK */
              parse value in.i with . "DSN=" dsn " " .  /* @BK */
              dsn = word(strip(dsn),1)                  /* @BK */
              if pos("&",dsn) > 0 then call fix_sym     /* @BK */
              procjobs = procjobs dsn                   /* @LD */
              end                                       /* @BK */
           if left(in.i,9) = "//IEFPDSI" then do
              hit = 1
              mstdd = null
              parse value in.i with . "DSN=" dsn " " .
              dsn = word(strip(dsn),1)                  /* @pn */
              if pos("&",dsn) > 0 then call fix_sym     /* @pn */
              proclibs = proclibs dsn
              end
           end

        /* ----------------------------------------------------- *
         * Alloc all Master JCL Proclibs to DD: $PROCMST         *
         * (or DD: $PROCLIB if called as a function)             *
         * ----------------------------------------------------- */
        alloc_procs = null
        do i = 1 to words(proclibs)
           proc  = ""_apost_""word(proclibs,i)""_apost_""
           alloc_procs = alloc_procs proc
           end
        if pos("&",alloc_procs) > 0 then do
           symtext = alloc_procs
           call fix_sym1
           alloc_procs = symtext
           end
        procddn = "$PROCMST"
        if isfunc = "Y" then do
           if allocdd = sub_allocdd("$PROCLIB") then do
              procddn = "$PROCLIB"
              end
           end
        _junk_ = msg("OFF")
        "Free  f("procddn")"
        _junk_ = msg("ON")
        if ispf = 1
           then "Alloc f("procddn") ds("alloc_procs") shr reuse"
           else call echo procddn alloc_procs
        if isfunc = "Y" then do
           if allocdd = sub_allocdd("$PROCLIB") then do
              retstr = strip(retstr "$PROCLIB")
              end
           if allocdd <> null &,
              allocdd = retstr then signal Finish
           end

        /* ----------------------------------------------------- *
         * Alloc all Master JCL IEFJOBS datasets to $PROCJOB     *
         * ----------------------------------------------------- */
        alloc_procs = null
        if procjobs /= null then do
        do i = 1 to words(procjobs)
           proc  = ""_apost_""word(procjobs,i)""_apost_""
           alloc_procs = alloc_procs proc
           end
        if pos("&",alloc_procs) > 0 then do
           symtext = alloc_procs
           call fix_sym1
           alloc_procs = symtext
           end
        procddn = "$PROCJOB"
        _junk_ = msg("OFF")
        "Free  f("procddn")"
        _junk_ = msg("ON")
        if ispf = 1
           then "Alloc f("procddn") ds("alloc_procs") shr reuse"
           else call echo procddn alloc_procs
        end

        /* ---------------------------------------------------- *
        * Now get JES2 PROCLIBS using SDSF REXX Interface       *
        * **Comment or delete if you don't have SDSF**          *
        * ----------------------------------------------------- */
        IF (SDSF_JES2_PROCS = "Y") THEN DO                    /* @A1 */
           RC = ISFCALLS("ON")                                /* @A1 */
           ADDRESS SDSF "ISFEXEC PROC"                        /* @A1 */
           RC = ISFCALLS("OFF")                               /* @A1 */
           ALLOC_PROCS = ""                                   /* @A1 */
           K = 1                                              /* @A1 */
           TMPDD = DDNAME.1                                   /* @A1 */
           SDSFPROC.1 = TMPDD                                 /* @A1 */
           DO J = 1 TO ISFROWS                                /* @A1 */
              IF (TMPDD = DDNAME.J) THEN Do                   /* @LD */
                 if wordpos("'"dsname.j"'",alloc_procs) = 0   /* @LD */
                 Then                                         /* @LD */
                 ALLOC_PROCS = ALLOC_PROCS "'"DSNAME.J"'"     /* @A1 */
                 End                                          /* @LD */
              ELSE DO                                         /* @A1 */
                 if ispf = 1                                  /* @A1 */
                    then "Alloc f($"TMPDD")",                 /* @A1 */
                         "ds("alloc_procs") shr reuse"        /* @A1 */
                    else call echo TMPDD alloc_procs          /* @A1 */
                 ALLOC_PROCS = ""                             /* @A1 */
                 TMPDD = DDNAME.J                             /* @A1 */
                 K = K + 1                                    /* @A1 */
                 SDSFPROC.K = '$'TMPDD                        /* @A1 */
              END /* ELSE */                                  /* @A1 */
              IF (J = ISFROWS) THEN DO                        /* @A1 */
                 if ispf = 1                                  /* @A1 */
                    then "Alloc f($"TMPDD")",                 /* @A1 */
                         "ds("alloc_procs") shr reuse"        /* @A1 */
                    else call echo TMPDD alloc_procs          /* @A1 */
                 K = K + 1                                    /* @A1 */
                 SDSFPROC.K = '$'TMPDD                        /* @A1 */
                 ALLOC_PROCS = ""                             /* @A1 */
                 TMPDD = ""                                   /* @A1 */
              END /* IF J */                                  /* @A1 */
           END J                                              /* @A1 */
           DROP ISFROWS.                                      /* @A1 */
           SDSFPROC.0 = K                                     /* @A1 */
        END /* IF */                                          /* @A1 */

        /* ----------------------------------------------------- *
         * Now look thru Master JCL Proclibs for JES2 Proc       *
         * ----------------------------------------------------- */
         call find_lib

        /* ----------------------------------------------------- *
         * Now find all //PROCxx and save proc names             *
         * ----------------------------------------------------- */
         Start:
         procs = null
         hit = 0
         sym = "sym"
         proc = "proc"
         do i = 1 to in.0
            if word(in.i,2) = "INCLUDE" then do
               parse value in.i with . "MEMBER="jes2 .
               in.i = "//******** replaced "
               if pos("&",jes2) > 0 then do
                  symtext = jes2
                  call fix_sym1
                  jes2 = symtext
                  end
               call find_lib
               signal start
               end
            if left(in.i,3) = "//*" then iterate
            If hit = 1 then do
               if substr(in.i,3,1) >= "A" then hit = 3
               end
            If pos(" PROC ",in.i) > 0 then hit = 1
            if hit = 3 then
               if left(in.i,6) = "//PROC" then do
                  parse value in.i with "//" w1 .
                  if length(w1) < 9 then
                      hit = 4
                  end
            if hit = 4 then do
               if left(in.i,6) <> "//PROC" then
                  if substr(in.i,3,1) >= "A" then hit = 3
               if left(in.i,6) = "//PROC" then do
                  parse value in.i with "//" w1 .
                  if length(w1) > 8 then hit = 3
                  end
               if hit = 3 then iterate
               if left(in.i,6) = "//PROC" then do
                  nn = substr(in.i,7,2)
                  proc.nn = null
                  procs = procs nn
                  end
               parse value in.i with . "DSN=" dsn ","
               dsn = word(strip(dsn),1)
               if pos("&",dsn) > 0 then call fix_sym
               if dsn <> null then do
                  if left(dsn,1) <> _apost_ then ,
                     dsn = ""_apost_""dsn""_apost_""
                  proc.nn = proc.nn dsn
                  end
               end
            If hit = 1 then do
               If pos(" PROC ",in.i) > 0
                  then test = word(in.i,3)
                  else test = word(in.i,2)
               test = translate(test," ",",")
               if words(test) > 1 then
                 do j = 1 to words(test)
                    parse value word(test,j) with symb "=" dsn ","
                    sym.symb = strip(dsn)
                    syms = syms symb
                 end
               else do
                    parse value test with  symb "=" dsn ","
                    sym.symb = strip(dsn)
                    syms = syms symb
                    end
               end
            if hit >1 then iterate
            end

        /* ----------------------------------------------------- *
         * Now alloc DD: $PROCxx                                 *
         * ----------------------------------------------------- */

        /* ----------------------------------------------------- *
         * CALL STEMVIEW "VIEW",in.,,,"Debugging mode: STEM IN." *
         * ----------------------------------------------------- */
           alloc_procs = null
           do i = 1 to words(procs)
              if alloc_procs <> null then do
                 if pos("&",alloc_procs) > 0 then do
                    symtext = alloc_procs
                    call fix_sym1
                    alloc_procs = symtext
                    end
                 doalloc = "N"
                 if isfunc = "Y" then do
                    if allocdd = sub_allocdd("$PROC"nn) then do
                       doalloc = "Y"
                       end
                    end
                  else do
                    doalloc = "Y"
                    end
                 if doalloc = "Y" then do
                    _junk_ = msg("OFF")
                    "Free  f($PROC"nn")"
                    _junk_ = msg("ON")
                    if ispf = 1
                    then "Alloc f($PROC"nn") ds("alloc_procs")",
                       "shr reuse"
                    else call echo "$PROC"nn alloc_procs
                    end
                 if isfunc = "Y" then do
                    if allocdd = sub_allocdd("$PROC"nn) then do
                       retstr = strip(retstr "$PROC"nn)
                       end
                    if allocdd <> null &,
                       allocdd = retstr then signal Finish
                    end
                 end
              nn = word(procs,i)
              alloc_procs = null
              do j = 1 to words(proc.nn)
                 pr = word(proc.nn,j)
                 alloc_procs = alloc_procs pr
                 end
              end
           if alloc_procs <> null then do
              if pos("&",alloc_procs) > 0 then do
                 symtext = alloc_procs
                 call fix_sym1
                 alloc_procs = symtext
                 end
              doalloc = "N"
              if isfunc = "Y" then do
                 if allocdd = sub_allocdd("$PROC"nn) then do
                    doalloc = "Y"
                    end
                 end
               else do
                 doalloc = "Y"
                 end
              if doalloc = "Y" then do
                 _junk_ = msg("OFF")
                 "Free  f($PROC"nn")"
                 _junk_ = msg("ON")
                 if ispf = 1
                 then "Alloc f($PROC"nn") ds("alloc_procs")",
                       "shr reuse"
                 else call echo "$PROC"nn alloc_procs
                 end
              if isfunc = "Y" then do
                 if allocdd = sub_allocdd("$PROC"nn) then do
                    retstr = strip(retstr "$PROC"nn)
                    end
                 if allocdd <> null &,
                    allocdd = retstr then signal Finish
                 end
              end

        /* ----------------------------------------------------- *
         * If not called as a function, invoke ISRDDN to display *
         * allocations and then free DD names.  if called as a   *
         * function, simply return the names of the allocated    *
         * files to the caller.                                  *
         * ----------------------------------------------------- */
         Finish:
           if isfunc = "N" then do
              Address ISPExec
              lev = mvsvar("sysmvs")
              lev = substr(lev,3,1)
              if lev < 6 then do
                 zedsmsg = null
                 zedlmsg = "System and JES2 Proclibs",
                           "have been identified and allocated",
                           "using $PROCMST for the Master JCL",
                           "Proclibs and $PROCxx for the JES2",
                           "Proclibs.               ",
                           "Issue ONLY $PROC",
                           "to display only DDs with PROC in the",
                           "ddname."
                 "Setmsg msg(isrz001)"
                 "Select cmd(isrddn)"
                 end
              else do
                   zdel = null
                   "vget (zdel)"
                   zopt = "Only $PROC"zdel'PARM'
                   if lstprocm /= null then do
                      zopt = zopt""zdel"long"zdel" member "lstprocm
                   end
                   "control errors return"
                   zmsg000l = _my_env_""copies(" ",80)""_sysenv_""
                   "setmsg msg(ispz000) cond"
                   "Select cmd(isrddn" zopt")"
                   end

             /* -------------------------- *
              * Now free all allocations   *
              * -------------------------- */
              if ispf = 0 then exit 0
              _junk_ = msg("OFF")
              Address TSO
              "Free f($PROCMST $PROCJOB)"                     /* @A1 */
              DO J = 1 TO SDSFPROC.0                          /* @A1 */
                 "FREE FI("SDSFPROC.J")"                      /* @A1 */
              END J                                           /* @A1 */
              do i = 1 to words(procs)
                 "Free f($PROC"word(procs,i)
              end
              _junk_ = msg("ON")
              retstr = 0
              end
            else do
              if ispf = 0 ,
              then retstr = 0 ;
              else do ;
                      _junk_ = msg("OFF")
                      "Free  f($PROCMST $PROCJOB)"
                      _junk_ = msg("ON")
                      if retstr = null then do
                         retstr = 16
                         end
                   end;
              end
           Exit retstr

        /* ----------------------------------------------------- *
         * Fix up symbolics in the dsname                        *
         * ----------------------------------------------------- */
           Fix_Sym: procedure expose (global_vars) ,
                                     dsn syms sym.
           do while pos("&", dsn) > 0
              parse value dsn with _left_ "&" symbol "." _right_
              syssym = mvsvar("symdef",symbol)
              if syssym <> null then do
                 if right(_left_,1) = '.'
                 then dsn = _left_""syssym""_right_
                 else dsn = _left_"."syssym""_right_
                 end
              else do
                 if wordpos(symbol,syms) = 0 then leave
                 wp   = wordpos(symbol,syms)
                 symb = word(syms,wp)
                 hlq  = sym.symb
                 if left(hlq,1) = _apost_ then ,
                    parse value hlq with (_apost_) hlq (_apost_)
                 dsn  = _left_""hlq""_right_
                 end
              if pos("&",dsn) = 0 then leave
              end
           return

        /* ----------------------------------------------------- *
         * Fix up symbolics in an expression (symtext)           *
         * ----------------------------------------------------- */
           Fix_Sym1:
           /* shift the symbols and their values to an array */
           symb.0 = words(syms)
           do i = 1 to symb.0
                jj = word(syms,i)
                symb.i.1 = word(syms,i)
                symb.i.2 = sym.jj
           end
           fixstart = 1
           srchdone = "N"
           do until srchdone = "Y"
              p1 = pos("&",symtext,fixstart)
              if p1 > 0 then do
                 parse value symtext with _left_"&"fixsymb
                 if left(fixsymb,1) = "&" then do
                    p1 = p1 + 1        /* ignore "&&" */
                    end
                  else do
                    _right_ = null
                    symdone = "N"
                    do r=1 to length(fixsymb) until symdone = "Y"
                       if datatype(substr(fixsymb,r,1),"ALPHA") = 0 ,
                       then do
                          _right_ = substr(fixsymb,r)
                          if left(_right_,1) = "." then do
                             _right_ = substr(_right_,2)
                             end
                          fixsymb = substr(fixsymb,1,r-1)
                          symdone = "Y"
                          end
                       end
                    if length(fixsymb) > 0 then do
                       syssym = null
                       do symidx = 1 to symb.0
                          if fixsymb = symb.symidx.1 ,
                          then do
                                   syssym = symb.symidx.2
                                   leave
                               end
                       end
                       if syssym = null then ,
                          syssym = mvsvar("symdef",fixsymb)
                       if syssym <> null then do
                          if right(_left_,1) = '.'
                          then symtext = _left_""syssym""_right_
                          else symtext = _left_"."syssym""_right_
                          end
                       end
                    end
                 fixstart = p1 + 1
                 if fixstart > length(symtext) then do
                    srchdone = "Y"
                    end
                 end
               else do
                 srchdone = "Y"
                 end
              end
           return

        /* ----------------------------------------------------- *
         * Fix volser for IPLPARM volume                         *
         * ----------------------------------------------------- */
           chk_volser: procedure expose (global_vars)
           parse arg unitnbr
           stat. = null
           dumy = outtrap("stat.")
               "LISTDATA STATUS UNITNUMBER("strip(UNITNBR)")"
                 parse var stat.3   "VOLUME" volser "DEVICE" .
           dumy = outtrap("off")
           return strip(volser)

        /* --------------------------------------------------------- *
         * Find Library for Proc                                     *
         * --------------------------------------------------------- */
         find_lib:
         jes2_proc = null
         do ip = 1 to words(proclibs)
            proc  = ""_apost_""word(proclibs,ip)"("jes2")"_apost_""
            if pos("&",proc) > 0 then do
               symtext = proc
               call fix_sym1
               proc = symtext
               end
            if "OK" = sysdsn(proc) ,
            then do;
                     jes2_proc = proc
                     leave
                 end;
            end

        /* --------------------------------------------------------- *
         * Read in the JES2 Proc                                     *
         * --------------------------------------------------------- */
        if jes2_proc = null ,
        then prc.0 = 0
        else do;
                "Alloc f("ddn") ds("proc") shr reuse"
                "Execio * diskr" ddn "(finis stem prc."
                rcode = rc
                "Free  f("ddn")"
             end;
        if in.0 = 0 then
           do i = 0 to prc.0
              in.i = prc.i
              end
        else do
             c = in.0
             do i = 1 to prc.0
                c = c + 1
                in.c = prc.i
                end
              in.0 = c
              end
        return

        /* -------------------------------- *
         * Echo to the Terminal all DSnames *
         * -------------------------------- */
         Echo: Procedure expose (global_vars) ,
                                echo. _my_env_ _sysenv_
         Parse Arg dd dsns
         echo.0 = echo.0 + 1
         if echo.0 = 1 ,
         then do ;
                   hl = 62
                   _info_    = copies("=",hl)
                   info.1    = "*"left(_info_,hl)"*"
                   _info_    = null
                   _info_    = _info_" "_my_env_
                   info.2    = "*"left(_info_,hl)"*"
                   _info_    = null
                   _info_    = _info_" "_sysenv_
                   info.3    = "*"left(_info_,hl)"*"
                   info.4    = info.1
                   _info_    = null
                   _info_    = _info_""left("*DD",10)""
                   _info_    = _info_""left("VOLSER",10)""
                   _info_    = _info_"DSNAME"
                   info.5    = _info_
                   _info_    = null
                   _info_    = _info_""left("*"copies("-",08),10)
                   _info_    = _info_""left(copies("-",06),10)
                   _info_    = _info_""copies("-",44)
                   info.6    = _info_
                   info.0    = 6
                   do i = 1 to info.0
                      say ""info.i""
                   end
              end;
         dsn.0 = words(dsns) ;
         do i = 1 to dsn.0 ;
            if i = 1 ,
            then dsn.i.1 = dd
            else dsn.i.1 = null
            dsn.i.2 = word(dsns,i)
            sysvolume = null
            _rc_ = listdsi(""dsn.i.2"")
            if _rc_ = 0 ,
            then     dsn.i.3 = sysvolume
            else     dsn.i.3 = "N/A"
         end
         do i = 1 to dsn.0 ;
             say left(dsn.i.1,10)""left(dsn.i.3,10)""dsn.i.2
         end
         return

  sub_allocdd: procedure expose (global_vars)
     parse arg alloc_dd
     select  ;
       when ( symbol("alloc_dd") == "LIT" ) then r_string = alloc_dd ;
       when ( alloc_dd = null         ) then r_string = alloc_dd ;
       otherwise do  ;
                                             r_string = null     ;
                 end ;
     end ;
     return r_string ;

  sub_init:
   /* to get the correct name for MSGID don't use other cmds before */
   parse source ,
     rexxenv rexxinv rexxname rexxdd rexxdsn . rexxenv addrspc .
   parse value "" with _null_
   myname = rexxname
   if myname = "?" ,
   then do ;
            myname = sysvar("sysicmd")
            if length(myname) = 0 ,
            then  myname = sysvar("syspcmd")
        end;
   msgid = left(myname": ",10)

         parse value "" with null ddn test mstrjcl proclibs,
                             proc syms procs parmlibs procjobs ,
                             opt1 opt2 opt3 opt4 lstprocm
         _apost_ = "'"
         parse source xenv xtype xmyname xddname xdsname .
         _sysplex_ = mvsvar("sysplex")
         _sysname_ = mvsvar("sysname")
         _sysmvs_  = MVSVAR("SYSMVS")
         parse value "" with _more_ custname custplex LPAR KSYSENV
         if _sysmvs_ > "SP7.0.5" ,
         then do ;
                 /* null if not defined           *
                  *  other variables can be added */
                 _morevars_ = "CUSTNAME CUSTPLEX LPAR KSYSENV"
                 do _i_ = 1 to words(_morevars_)
                    _morekey_ = word(_morevars_,_i_)
                    _moreval_ = mvsvar('symdef',_morekey_)
                    _valcmd_ = ""_morekey_ "= '"_moreval_"' "
                    interpret _valcmd_
                    select;
                      when ( _morekey_ = "CUSTPLEX" ) then iterate
                      when ( _morekey_ = "LPAR"     ) then iterate
                      otherwise nop;
                    end;
                    if _moreval_ /= null ,
                    then do ;
                              _more_=""_more_""_morekey_"="_moreval_" "
                         end;
                 end;
              end;
         else do ;
                 _more_ = "SYSMVS="_sysmvs_
              end;
         _more_ = strip(_more_)
         _sysenv_  = null
         if custname /= null ,
         then _sysenv_  = ""_sysenv_""custname": "
         _sysenv_  = ""_sysenv_"SYSPLEX="_sysplex_" "
         if custplex /= null ,
         then do;
                 if custplex  /= _sysplex_ ,
                 then _sysenv_ = ""_sysenv_"CUSTPLEX="custplex" "
              end;
         if lpar = null ,
         then lpar = _sysname_
         if lpar       = _sysname_ ,
         then do ;
                  _sysenv_= ""_sysenv_"LPAR/SYSNAME="lpar" "
              end;
         else do ;
                  _sysenv_= ""_sysenv_"SYSNAME="_sysname_" LPAR="lpar" "
              end;
         _sysenv_  = ""_sysenv_""_more_
         _sysenv_  = strip(_sysenv_)
         _my_env_  = ""myname" "_version_""
         echo.0 = 0
         global_vars = "null _apost_"
     return 0

      /**************************************************************
      * Trap uninitialized variables                                *
      ***************************************************************/
      sub_novalue:
      Say ""
      Say "Variable" ,
         condition("Description") "undefined in line" sigl":"
      Say sourceline(sigl)
      if sysvar("sysenv") <> "FORE" then exit 8
      say "Report the error in this application along with the",
          "syntax used."
      exit 8
