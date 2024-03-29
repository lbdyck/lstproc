  /* --------------------  rexx procedure  -------------------- */
  _version_ = "3.4"
  /* Name:      lstproc3                                        *
  *                                                            *
  * Function:  Display active z/OS and JES3 Proclibs           *
  *                           System Parmlib                   *
  *                           JES3 Disk Reader                 *
  *                           JES3 PARMLIB (initdeck)          *
  *                                                            *
  * Syntax:    %lstproc3 proc /S                               *
  *                                                            *
  *            Where proc is the name of the JES3 proc or      *
  *            blank to default to JES3.                       *
  *                                                            *
  *            /S is optional and if used will cause           *
  *            the JES3 Disk Reader, Initdeck, and System      *
  *            Parmlibs to be displayed                        *
  *                                                            *
  * Assumptions and Caveats:                                   *
  *            1. if proc not specified then use JES3          *
  *            2. only DYNALLOC for IATPLBxx in the initdeck   *
  *               will be used along with the MSTJCLxx Procs   *
  *            3. comments will be ignored                     *
  *            4. symbolics are supported (mostly)             *
  *            5. system symbolics are supported               *
  *            6. only cataloged datasets are supported        *
  *            7. nested symbolics may not work                *
  *            8. if the mstrjcl or jes3 proc changes that     *
  *               is what you will see reflected               *
  *            9. This does NOT report the actual proc usage   *
  *               but what is found from mstrjcl and the       *
  *               jes3 proc.                                   *
  *           10. *IMPORTANT* It is assumed you have RACF      *
  *               read for the LISTDATA IDCAMS command.        *
  *               If not find *LISTDATA* and make the          *
  *               noted changes.  You will then be limited     *
  *               to the cataloged iplparm dataset.            *
  *           11. System Parmlib's will be allocated to        *
  *               $PARMLIB                                     *
  *           12. JES3 Disk Reader ($J3DRDS) will be allocated*
  *               to $J3DRDS                                   *
  *                                                            *
  * Author:    Lionel B. Dyck                                  *
  *            Internet: lbdyck@gmail.com                      *
  *                                                            *
  * History:                                                   *
  *                                                            *
  *          2018-09-27 - Allow VOL/UNIT on DYNALLOC statements*
  *          2017-02-06 - Set null variable                    *
  *                     - strip initdeck to remove blanks      *
  *          2016-05-24 - Add /S option                        *
  *          2016-05-22 - Add JES3 Parmlib                     *
  *          2016-04-14 - Support JES3 Disk Reader JES3DRDS    *
  *          2016-03-30 - Rewrite for JES3                     *
  *                     - remove callability as function       *
  *                     - always allocate $PARMLIB             *
  *          2014-01-31 - Don't alloc/open/read the iplparm    *
  *                       if it can't be found                 *
  *          2009-07-08 - Change # to $ (thx hartmut)          *
  *          2009-07-08 - MSG("OFF") in FREE routine           *
  *                     - use _junk_ instead of RC             *
  *          2009-05-11 - Check whether proclib member exists  *
  *                       SYS1.PROCLIB(JES3) default may fail. *
  *          2008-11-05 - Correction in handling symbols       *
  *                       custplex and lpar                    *
  *          2008-10-15 - Info about old sysmvs                *
  *                     - Note:                                *
  *                       LISTDATA/LDATA in AUTHCMD in IKJTSO00*
  *                     - substitute vars  left  and  right    *
  *                               by vars _left_ and _right_   *
  *          2007-10-16 - If not under ISPF just write         *
  *                       dd and dsnames to screen             *
  *          2007-02-20 - Correction from Ian Ramage           *
  *                       Fix if JES3 Proc in >2 proclib       *
  *          2006-11-27 - Hartmut Beckmann                     *
  *                       Changes for better member support    *
  *          2006-09-25 - Peggy Norton                         *
  *                       Fix call to subroutine for symbolics *
  *          2006-05-17 - Jeff Dixon                           *
  *                       Support //PROCxxxx instead of PROCxx *
  *          2006-05-11 - Peggy Norton                         *
  *                       Fix if LOADxx does not have defined  *
  *                       SYS1.PARMLIB                         *
  *          2006-01-26 - Ian Ramage                           *
  *                       Add Numeric Digits 10 to resolve     *
  *                       issue under z/OS 1.6                 *
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
  /* ----------------------------------------------------- *
  * Setup defaults in sub routine                         *
  * ----------------------------------------------------- */
  _x_ = sub_init() ;

  /* ---------------------------------------------- *
  * Check for any passed options.                  *
  *                                                *
  * The two supported options are:                 *
  *                                                *
  * 1) the name of the JES3 Proc (default is JES3) *
  * 2) /S to display system datasets               *
  *    - parmlibs                                  *
  *    - jes3 initdeck                             *
  *    - jes3 disk reader                          *
  * ---------------------------------------------- */
  arg options

  /* --------------- *
  * Define defaults *
  * --------------- */
  ddn = "JCL"random(9999)
  Numeric Digits 10
  sopt = 0
  null = ''

  /* ------------ *
  * Check for /S *
  * ------------ */
  if pos('/S',options) > 0 then do
    sopt = 1
    p = pos('/S',options)
    if p = 1 then options = substr(options,3)
    else do
      options = left(options,p-1)''substr(options,p+2)
    end
    options = strip(options)
  end

  /* ----------------------------------------------------- *
  * test options and use for procname or use JES3         *
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
  if options = null then JES3 = "JES3"
  else JES3 = options
  isfunc = "N"
  allocdd = null
  retstr  = null

  /* ----------------------------- *
  * Test for ISPF and if not then *
  * set flag to only echo Parmlib *
  * info to the "screen".         *
  * ----------------------------- */
  sysispf = sysvar("sysispf")
  if sysispf = "ACTIVE" then ispf = 1
  else do
    ispf = 0
    isfunc = "F"
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
  loadvol  = null
  LOADVOL  = chk_volser(parmaddr)
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
  * Alloc all Parmlibs to DD: $PARMLIB (If Appropriate)   *
  * ----------------------------------------------------- */
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
  "Free  f($PARMLIB)"
  _junk_ = msg("ON")
  "Alloc f($PARMLIB) ds("alloc_parms") shr reuse"
  retstr = strip(retstr "$PARMLIB")

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
  do i = 1 to in.0
    in.i  = TRANSLATE(in.i," ",",")
    if hit = 1 then do
      call fix_sym
      if substr(in.i,3,1) = " "
      then do
        parse value in.i with . "DSN=" dsn " " .
        proclibs = proclibs dsn
      end
      else hit = 0
    end
    if left(in.i,9) = "//IEFPDSI" then do
      hit = 1
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
  _junk_ = msg("OFF")
  "Free  f("procddn")"
  _junk_ = msg("ON")
  if ispf = 1
  then "Alloc f("procddn") ds("alloc_procs") shr reuse"
  else call echo procddn alloc_procs

  /* ----------------------------------------------------- *
  * Now look thru Master JCL Proclibs for JES3 Proc       *
  * ----------------------------------------------------- */
  in.0 = 0
  call find_lib

  /* ----------------------------------------------------- *
  * Now the JES3IN DD and read in the initdeck            *
  * ----------------------------------------------------- */
Start:
  hit = 0
  sym = "sym"
  initdeck = null
  do i = 1 to in.0
    if left(in.i,8) = "//JES3IN" then hit = 1
    if hit = 1 then do
      if pos("DSN=",in.i) > 0 then do
        parse value in.i with . 'DSN='initdeck',' .
        initdeck = strip(initdeck)
        hit = 2
      end
      if hit = 2 then leave
    end
  end

  /* ----------------------------------------- *
  * Now find all IATPLBxx DYNALLOC statements *
  * ----------------------------------------- */
  drop in.
  "Alloc f("ddn") ds('"initdeck"') shr reuse"
  "Execio * diskr" ddn "(finis stem in."
  "Free  f("ddn")"
  proc = "proc"
  proc. = null
  procs = null
  do i = 1 to in.0
    if left(in.i,1) = '*' then iterate
    if left(in.i,8) = 'DYNALLOC' then
    pddn = null
    parse value in.i with 'DYNALLOC,DDN='pddn',DSN='pdsn .
    if pos(',',pdsn) > 0 then parse value pdsn with pdsn','
    if left(pddn,6) = 'IATPLB' then do
      pn = right(pddn,2)
      if pos(pn,procs) = 0 then
      procs = procs pn
      proc.pn = proc.pn "'"pdsn"'"
    end
  end
  rdrs  = null
  do i = 1 to in.0
    if left(in.i,1) = '*' then iterate
    if left(in.i,8) = 'DYNALLOC' then
    pddn = null
    parse value in.i with 'DYNALLOC,DDN='pddn',DSN='pdsn .
    if pos(',',pdsn) > 0 then parse value pdsn with pdsn','
    if left(pddn,8) = 'JES3DRDS' then do
      rdrs = rdrs "'"pdsn"'"
    end
  end

  /* ----------------------------------------------------- *
  * Now alloc DD: $PROCxx, $J3DRDS, and $J3INIT           *
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
      _junk_ = msg("OFF")
      "Free  f($PROC"nn")"
      _junk_ = msg("ON")
      if ispf = 1
      then "Alloc f($PROC"nn") ds("alloc_procs")",
        "shr reuse"
      else call echo "$PROC"nn alloc_procs
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
    _junk_ = msg("OFF")
    "Free  f($PROC"nn" $J3DRDS $J3INIT)"
    _junk_ = msg("ON")
    if ispf = 1 then do
      "Alloc f($PROC"nn") ds("alloc_procs")",
        "shr reuse"
      "Alloc f($J3DRDS) ds("rdrs")",
        "shr reuse"
      parse value initdeck with initdeck'('.
      "Alloc f($J3INIT) ds('"initdeck"') shr reuse"
    end
    else do
      call echo "$PROC"nn alloc_procs
      call echo "$J3DRDS" rdrs
      call echo "$J3INIT" initdeck
    end
  end

  /* ----------------------------------------------------- *
  * If not called as a function, invoke ISRDDN to display *
  * allocations and then free DD names.  if called as a   *
  * function, simply return the names of the allocated    *
  * files to the caller.                                  *
  * ----------------------------------------------------- */
Finish:
  Address ISPExec
  lev = mvsvar("sysmvs")
  lev = substr(lev,3,1)
  if lev < 6 then do
    zedsmsg = null
    zedlmsg = "System and JES3 Proclibs",
      "have been identified and allocated",
      "using $PROCMST for the Master JCL",
      "Proclibs and $PROCxx for the JES3",
      "Proclibs.               ",
      "Issue ONLY $",
      "to display only DDs with PROC in the",
      "ddname."
    "Setmsg msg(isrz001)"
    "Select cmd(isrddn)"
  end
  else do
    "vget (zdel)"
    if sopt = 1 then zopt = 'Only $'
    else zopt = "Only $P"
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
  "Free f($PROCMST $PARMLIB $J3DRDS $J3INIT)"
  do i = 1 to words(procs)
    "Free f($PROC"word(procs,i)
  end
  _junk_ = msg("ON")
  retstr = 0
  Exit retstr

  /* ----------------------------------------------------- *
  * Fix up symbolics in the dsname                        *
  * ----------------------------------------------------- */
Fix_Sym: procedure expose (global_vars) ,
    dsn syms sym.
  do forever
    parse value dsn with _left_ "&" symbol "." _right_
    syssym = mvsvar("symdef",symbol)
    if syssym <> null then do
      dsn = _left_""syssym""_right_
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
            symtext = _left_""syssym""_right_
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
  JES3_proc = null
  do ip = 1 to words(proclibs)
    proc  = ""_apost_""word(proclibs,ip)"("JES3")"_apost_""
    if pos("&",proc) > 0 then do
      symtext = proc
      call fix_sym1
      proc = symtext
    end
    if "OK" = sysdsn(proc) ,
      then do;
      JES3_proc = proc
      leave
    end;
  end

  /* --------------------------------------------------------- *
  * Read in the JES3 Proc                                     *
  * --------------------------------------------------------- */
  if JES3_proc = null ,
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
    _rc_ = listdsi(""dsn.i.2"")
    if _rc_ = 0 ,
      then     dsn.i.3 = sysvolume
    else     dsn.i.3 = "N/A"
  end
  do i = 1 to dsn.0 ;
    say left(dsn.i.1,10)""left(dsn.i.3,10)""dsn.i.2
  end
  return

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
    proc syms procs parmlibs,
    opt1 opt2 opt3 opt4 lstprocm
  _apost_ = "'"
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
