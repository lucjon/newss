/* C file produced by GAC */
#include <src/compiled.h>

/* global variables used in handlers */
static GVar G_InstallGlobalFunction;
static Obj GF_InstallGlobalFunction;
static GVar G_NEWSS__SchreierVector;
static Obj GC_NEWSS__SchreierVector;
static Obj GF_NEWSS__SchreierVector;
static GVar G_Size;
static Obj GF_Size;
static GVar G_Remove;
static Obj GF_Remove;
static GVar G_ExtendSchreierVector;
static Obj GC_ExtendSchreierVector;
static GVar G_SchreierVectorPermToBasePoint;
static Obj GC_SchreierVectorPermToBasePoint;
static Obj GF_SchreierVectorPermToBasePoint;
static GVar G_SchreierVectorWordToBasePoint;
static Obj GC_SchreierVectorWordToBasePoint;
static Obj GF_SchreierVectorWordToBasePoint;

/* record names used in handlers */
static RNam R_gens;
static RNam R_invgens;
static RNam R_sv;
static RNam R_size;
static RNam R_point;

/* information for the functions */
static Obj NameFunc[12];
static Obj FileName;

static Obj Impl_NEWSS_SchreierVector(Obj self, Obj a_sv, Obj a_to__compute) {
    Obj l_pt, l_gen, l_image, l_j, t_1, t_2, t_3, t_4, t_5, sv_sv;
    Int j, image, orbit_size;
    Bag oldFrame;
    OLD_BRK_CURR_STAT

    /* allocate new stack frame */
    SWITCH_TO_NEW_FRAME(self, 0, 0, oldFrame);
    REM_BRK_CURR_STAT();
    SET_BRK_CURR_STAT(0);

    GAP_ASSERT(IS_LIST(a_to__compute));
    orbit_size = INT_INTOBJ(ELM_REC(a_sv, R_size));

    /* while Size( to_compute ) > 0 od */
    while (LEN_LIST(a_to__compute) > 0) {
        /* pt := Remove( to_compute, 1 ); */
        l_pt = CALL_2ARGS(GF_Remove, a_to__compute, INTOBJ_INT(1));
        CHECK_FUNC_RESULT(l_pt)

        /* for j in [ 1 .. Size( sv.gens ) ] do */
        for (j = 1; j <= LEN_LIST(ELM_REC(a_sv, R_gens)); j++) {
            l_j = INTOBJ_INT(j);

            /* gen := sv.invgens[j]; */
            l_gen = ELM_LIST(ELM_REC(a_sv, R_invgens), j);

            /* image := pt ^ gen; */
            image = INT_INTOBJ(POW(l_pt, l_gen));

            /* if not IsBound( sv.sv[image] ) then */
            sv_sv = ELM_REC(a_sv, R_sv);
            if (!ISB_LIST(sv_sv, image)) {
                AddPlist(a_to__compute, INTOBJ_INT(image));
                ASS_LIST(sv_sv, image, l_j);
                orbit_size++;
            }
        }
    }

    ASS_REC(a_sv, R_size, INTOBJ_INT(orbit_size));

    /* return; */
    RES_BRK_CURR_STAT();
    SWITCH_TO_OLD_FRAME(oldFrame);
    return 0;
}

static Obj Impl_ExtendSchreierVector(Obj self, Obj a_sv, Obj a_gen,
                                     Obj a_invgen) {
    Obj l_to__compute = 0;
    Obj l_image = 0;
    Obj l_n = 0;
    Obj l_pt = 0;
    Obj t_1 = 0;
    Obj t_2 = 0;
    Obj t_3 = 0;
    Obj t_4 = 0;
    Obj t_5 = 0;
    Obj t_6 = 0;
    Obj sv_sv;
    Int pt, orbit_size, image;
    Bag oldFrame;
    OLD_BRK_CURR_STAT

    /* allocate new stack frame */
    SWITCH_TO_NEW_FRAME(self, 0, 0, oldFrame);
    REM_BRK_CURR_STAT();
    SET_BRK_CURR_STAT(0);

    /* to_compute := [  ]; */
    l_to__compute = NEW_PLIST(T_PLIST, 0);
    SET_LEN_PLIST(l_to__compute, 0);

    /* Add( sv.gens, gen ); */
    AddPlist(ELM_REC(a_sv, R_gens), a_gen);
    AddPlist(ELM_REC(a_sv, R_invgens), a_invgen);
    l_n = INTOBJ_INT(LEN_LIST(ELM_REC(a_sv, R_gens)));

    orbit_size = INT_INTOBJ(ELM_REC(a_sv, R_size));

    /* for pt in [ 1 .. Size( sv.sv ) ] do */
    sv_sv = ELM_REC(a_sv, R_sv);
    for (pt = 1; pt <= LEN_LIST(sv_sv); pt++) {
        l_pt = INTOBJ_INT(pt);

        if (!ISB_LIST(sv_sv, pt))
            continue;

        /* image := pt ^ invgen; */
        l_image = POW(l_pt, a_invgen);
        image = INT_INTOBJ(l_image);

        /* if not IsBound( sv.sv[image] ) then */
        if (!ISB_LIST(sv_sv, image)) {
            ASS_LIST(sv_sv, image, l_n);
            AddPlist(l_to__compute, l_image);
            orbit_size++;
        }
    }

    ASS_REC(a_sv, R_size, INTOBJ_INT(orbit_size));
    Impl_NEWSS_SchreierVector(self, a_sv, l_to__compute);

    /* return; */
    RES_BRK_CURR_STAT();
    SWITCH_TO_OLD_FRAME(oldFrame);
    return 0;
}

static Obj Impl_SchreierVectorPermToBasePoint(Obj self, Obj a_sv, Obj a_beta) {
    Obj l_u = 0;
    Obj l_k = 0;
    Obj t_1 = 0;
    Obj t_2 = 0;
    Obj t_3 = 0;
    Obj t_4 = 0;
    Bag oldFrame;
    OLD_BRK_CURR_STAT

    /* allocate new stack frame */
    SWITCH_TO_NEW_FRAME(self, 0, 0, oldFrame);
    REM_BRK_CURR_STAT();
    SET_BRK_CURR_STAT(0);

    /* if not IsBound( sv.sv[beta] ) then */
    t_4 = ELM_REC(a_sv, R_sv);
    CHECK_INT_POS(a_beta)
    t_3 = C_ISB_LIST(t_4, a_beta);
    t_2 = (Obj)(UInt)(t_3 != False);
    t_1 = (Obj)(UInt)(!((Int)t_2));
    if (t_1) {

        /* return false; */
        t_1 = False;
        RES_BRK_CURR_STAT();
        SWITCH_TO_OLD_FRAME(oldFrame);
        return t_1;
    }
    /* fi */

    /* u := (); */
    t_1 = IdentityPerm;
    l_u = t_1;

    /* k := sv.sv[beta]; */
    t_2 = ELM_REC(a_sv, R_sv);
    C_ELM_LIST_FPL(t_1, t_2, a_beta)
    l_k = t_1;

    /* while k <> -1 od */
    while (1) {
        t_1 = (Obj)(UInt)(!EQ(l_k, INTOBJ_INT(-1)));
        if (!t_1)
            break;

        /* u := u * sv.gens[k]; */
        t_3 = ELM_REC(a_sv, R_gens);
        CHECK_INT_POS(l_k)
        C_ELM_LIST_FPL(t_2, t_3, l_k)
        C_PROD_FIA(t_1, l_u, t_2)
        l_u = t_1;

        /* beta := beta ^ sv.gens[k]; */
        t_3 = ELM_REC(a_sv, R_gens);
        C_ELM_LIST_FPL(t_2, t_3, l_k)
        t_1 = POW(a_beta, t_2);
        a_beta = t_1;

        /* k := sv.sv[beta]; */
        t_2 = ELM_REC(a_sv, R_sv);
        CHECK_INT_POS(a_beta)
        C_ELM_LIST_FPL(t_1, t_2, a_beta)
        l_k = t_1;
    }
    /* od */

    /* return u; */
    RES_BRK_CURR_STAT();
    SWITCH_TO_OLD_FRAME(oldFrame);
    return l_u;

    /* return; */
    RES_BRK_CURR_STAT();
    SWITCH_TO_OLD_FRAME(oldFrame);
    return 0;
}

static Obj Impl_SchreierVectorWordToBasePoint(Obj self, Obj a_sv, Obj a_beta) {
    Obj l_u = 0;
    Obj l_k = 0;
    Obj t_1 = 0;
    Obj t_2 = 0;
    Obj t_3 = 0;
    Obj t_4 = 0;
    Bag oldFrame;
    OLD_BRK_CURR_STAT

    /* allocate new stack frame */
    SWITCH_TO_NEW_FRAME(self, 0, 0, oldFrame);
    REM_BRK_CURR_STAT();
    SET_BRK_CURR_STAT(0);

    /* if not IsBound( sv.sv[beta] ) then */
    t_4 = ELM_REC(a_sv, R_sv);
    CHECK_INT_POS(a_beta)
    t_3 = C_ISB_LIST(t_4, a_beta);
    t_2 = (Obj)(UInt)(t_3 != False);
    t_1 = (Obj)(UInt)(!((Int)t_2));
    if (t_1) {

        /* return false; */
        t_1 = False;
        RES_BRK_CURR_STAT();
        SWITCH_TO_OLD_FRAME(oldFrame);
        return t_1;
    }
    /* fi */

    /* u := [  ]; */
    t_1 = NEW_PLIST(T_PLIST, 0);
    SET_LEN_PLIST(t_1, 0);
    l_u = t_1;

    /* k := sv.sv[beta]; */
    t_2 = ELM_REC(a_sv, R_sv);
    C_ELM_LIST_FPL(t_1, t_2, a_beta)
    l_k = t_1;

    /* while k <> -1 od */
    while (1) {
        t_1 = (Obj)(UInt)(!EQ(l_k, INTOBJ_INT(-1)));
        if (!t_1)
            break;

        /* Add( u, sv.gens[k] ); */
        t_2 = ELM_REC(a_sv, R_gens);
        CHECK_INT_POS(l_k)
        C_ELM_LIST_FPL(t_1, t_2, l_k)
        C_ADD_LIST_FPL(l_u, t_1)

        /* beta := beta ^ sv.gens[k]; */
        t_3 = ELM_REC(a_sv, R_gens);
        C_ELM_LIST_FPL(t_2, t_3, l_k)
        t_1 = POW(a_beta, t_2);
        a_beta = t_1;

        /* k := sv.sv[beta]; */
        t_2 = ELM_REC(a_sv, R_sv);
        CHECK_INT_POS(a_beta)
        C_ELM_LIST_FPL(t_1, t_2, a_beta)
        l_k = t_1;
    }
    /* od */

    /* return u; */
    RES_BRK_CURR_STAT();
    SWITCH_TO_OLD_FRAME(oldFrame);
    return l_u;

    /* return; */
    RES_BRK_CURR_STAT();
    SWITCH_TO_OLD_FRAME(oldFrame);
    return 0;
}

static Obj HdlrFunc1(Obj self) {
    Obj t_1 = 0;
    Obj t_2 = 0;
    Obj t_3 = 0;
    Obj t_4 = 0;
    Bag oldFrame;
    OLD_BRK_CURR_STAT

    /* allocate new stack frame */
    SWITCH_TO_NEW_FRAME(self, 0, 0, oldFrame);
    REM_BRK_CURR_STAT();
    SET_BRK_CURR_STAT(0);

    /* InstallGlobalFunction( NEWSS_SchreierVector, function ( sv, to_compute )
         local  pt, gen, image, j;
         while Size( to_compute ) > 0  do
             pt := Remove( to_compute, 1 );
             for j  in [ 1 .. Size( sv.gens ) ]  do
                 gen := sv.invgens[j];
                 image := pt ^ gen;
                 if not IsBound( sv.sv[image] )  then
                     Add( to_compute, image );
                     sv.sv[image] := j;
                     sv.size := sv.size + 1;
                 fi;
             od;
         od;
         return;
     end ); */
    t_1 = GF_InstallGlobalFunction;
    t_2 = GC_NEWSS__SchreierVector;
    CHECK_BOUND(t_2, "NEWSS_SchreierVector")
    t_3 = NewFunction(NameFunc[2], 2, 0, Impl_NEWSS_SchreierVector);
    SET_ENVI_FUNC(t_3, STATE(CurrLVars));
    t_4 = NewBag(T_BODY, sizeof(BodyHeader));
    SET_STARTLINE_BODY(t_4, 3);
    SET_ENDLINE_BODY(t_4, 18);
    SET_FILENAME_BODY(t_4, FileName);
    SET_BODY_FUNC(t_3, t_4);
    CHANGED_BAG(STATE(CurrLVars));
    CALL_2ARGS(t_1, t_2, t_3);

    /* InstallGlobalFunction( ExtendSchreierVector, function ( sv, gen, invgen )
         local  to_compute, image, n, pt;
         to_compute := [  ];
         Add( sv.gens, gen );
         Add( sv.invgens, invgen );
         n := Size( sv.gens );
         for pt  in [ 1 .. Size( sv.sv ) ]  do
             if not IsBound( sv.sv[pt] )  then
                 continue;
             fi;
             image := pt ^ invgen;
             if not IsBound( sv.sv[image] )  then
                 Add( to_compute, image );
                 sv.sv[image] := n;
                 sv.size := sv.size + 1;
             fi;
         od;
         NEWSS_SchreierVector( sv, to_compute );
         return;
     end ); */
    t_1 = GF_InstallGlobalFunction;
    t_2 = GC_ExtendSchreierVector;
    CHECK_BOUND(t_2, "ExtendSchreierVector")
    t_3 = NewFunction(NameFunc[5], 3, 0, Impl_ExtendSchreierVector);
    SET_ENVI_FUNC(t_3, STATE(CurrLVars));
    t_4 = NewBag(T_BODY, sizeof(BodyHeader));
    SET_STARTLINE_BODY(t_4, 41);
    SET_ENDLINE_BODY(t_4, 62);
    SET_FILENAME_BODY(t_4, FileName);
    SET_BODY_FUNC(t_3, t_4);
    CHANGED_BAG(STATE(CurrLVars));
    CALL_2ARGS(t_1, t_2, t_3);

    /* InstallGlobalFunction( SchreierVectorPermToBasePoint, function ( sv, beta
     )
         local  u, k;
         if not IsBound( sv.sv[beta] )  then
             return false;
         fi;
         u := ();
         k := sv.sv[beta];
         while k <> -1  do
             u := u * sv.gens[k];
             beta := beta ^ sv.gens[k];
             k := sv.sv[beta];
         od;
         return u;
     end ); */
    t_1 = GF_InstallGlobalFunction;
    t_2 = GC_SchreierVectorPermToBasePoint;
    CHECK_BOUND(t_2, "SchreierVectorPermToBasePoint")
    t_3 = NewFunction(NameFunc[6], 2, 0, Impl_SchreierVectorPermToBasePoint);
    SET_ENVI_FUNC(t_3, STATE(CurrLVars));
    t_4 = NewBag(T_BODY, sizeof(BodyHeader));
    SET_STARTLINE_BODY(t_4, 64);
    SET_ENDLINE_BODY(t_4, 81);
    SET_FILENAME_BODY(t_4, FileName);
    SET_BODY_FUNC(t_3, t_4);
    CHANGED_BAG(STATE(CurrLVars));
    CALL_2ARGS(t_1, t_2, t_3);

    /* InstallGlobalFunction( SchreierVectorWordToBasePoint, function ( sv, beta
     )
         local  u, k;
         if not IsBound( sv.sv[beta] )  then
             return false;
         fi;
         u := [  ];
         k := sv.sv[beta];
         while k <> -1  do
             Add( u, sv.gens[k] );
             beta := beta ^ sv.gens[k];
             k := sv.sv[beta];
         od;
         return u;
     end ); */
    t_1 = GF_InstallGlobalFunction;
    t_2 = GC_SchreierVectorWordToBasePoint;
    CHECK_BOUND(t_2, "SchreierVectorWordToBasePoint")
    t_3 = NewFunction(NameFunc[8], 2, 0, Impl_SchreierVectorWordToBasePoint);
    SET_ENVI_FUNC(t_3, STATE(CurrLVars));
    t_4 = NewBag(T_BODY, sizeof(BodyHeader));
    SET_STARTLINE_BODY(t_4, 87);
    SET_ENDLINE_BODY(t_4, 103);
    SET_FILENAME_BODY(t_4, FileName);
    SET_BODY_FUNC(t_3, t_4);
    CHANGED_BAG(STATE(CurrLVars));
    CALL_2ARGS(t_1, t_2, t_3);

    /* return; */
    RES_BRK_CURR_STAT();
    SWITCH_TO_OLD_FRAME(oldFrame);
    return 0;

    /* return; */
    RES_BRK_CURR_STAT();
    SWITCH_TO_OLD_FRAME(oldFrame);
    return 0;
}

/* 'PostRestore' restore gvars, rnams, functions */
static Int PostRestore(StructInitInfo *module) {

    /* global variables used in handlers */
    G_InstallGlobalFunction = GVarName("InstallGlobalFunction");
    G_NEWSS__SchreierVector = GVarName("NEWSS_SchreierVector");
    G_Size = GVarName("Size");
    G_Remove = GVarName("Remove");
    G_ExtendSchreierVector = GVarName("ExtendSchreierVector");
    G_SchreierVectorPermToBasePoint = GVarName("SchreierVectorPermToBasePoint");
    G_SchreierVectorWordToBasePoint = GVarName("SchreierVectorWordToBasePoint");

    /* record names used in handlers */
    R_gens = RNamName("gens");
    R_invgens = RNamName("invgens");
    R_sv = RNamName("sv");
    R_size = RNamName("size");
    R_point = RNamName("point");

    /* information for the functions */
    NameFunc[1] = 0;
    NameFunc[2] = 0;
    NameFunc[3] = 0;
    NameFunc[4] = 0;
    NameFunc[5] = 0;
    NameFunc[6] = 0;
    NameFunc[7] = 0;
    NameFunc[8] = 0;
    NameFunc[9] = 0;
    NameFunc[10] = 0;
    NameFunc[11] = 0;

    /* return success */
    return 0;
}

/* 'InitKernel' sets up data structures, fopies, copies, handlers */
static Int InitKernel(StructInitInfo *module) {

    /* global variables used in handlers */
    InitFopyGVar("InstallGlobalFunction", &GF_InstallGlobalFunction);
    InitCopyGVar("NEWSS_SchreierVector", &GC_NEWSS__SchreierVector);
    InitFopyGVar("NEWSS_SchreierVector", &GF_NEWSS__SchreierVector);
    InitFopyGVar("Size", &GF_Size);
    InitFopyGVar("Remove", &GF_Remove);
    InitCopyGVar("ExtendSchreierVector", &GC_ExtendSchreierVector);
    InitCopyGVar("SchreierVectorPermToBasePoint",
                 &GC_SchreierVectorPermToBasePoint);
    InitFopyGVar("SchreierVectorPermToBasePoint",
                 &GF_SchreierVectorPermToBasePoint);
    InitCopyGVar("SchreierVectorWordToBasePoint",
                 &GC_SchreierVectorWordToBasePoint);
    InitFopyGVar("SchreierVectorWordToBasePoint",
                 &GF_SchreierVectorWordToBasePoint);

    /* information for the functions */
    InitGlobalBag(&FileName, "lib/orbstab.gi:FileName(-15641547)");
    InitHandlerFunc(HdlrFunc1, "lib/orbstab.gi:HdlrFunc1(-15641547)");
    InitGlobalBag(&(NameFunc[1]), "lib/orbstab.gi:NameFunc[1](-15641547)");
    InitHandlerFunc(Impl_NEWSS_SchreierVector,
                    "lib/orbstab.gi:Impl_NEWSS_SchreierVector(-15641547)");
    InitGlobalBag(&(NameFunc[4]), "lib/orbstab.gi:NameFunc[4](-15641547)");
    InitHandlerFunc(Impl_ExtendSchreierVector,
                    "lib/orbstab.gi:Impl_ExtendSchreierVector(-15641547)");
    InitGlobalBag(&(NameFunc[5]), "lib/orbstab.gi:NameFunc[5](-15641547)");
    InitHandlerFunc(
        Impl_SchreierVectorPermToBasePoint,
        "lib/orbstab.gi:Impl_SchreierVectorPermToBasePoint(-15641547)");
    InitGlobalBag(&(NameFunc[6]), "lib/orbstab.gi:NameFunc[6](-15641547)");
    InitHandlerFunc(
        Impl_SchreierVectorWordToBasePoint,
        "lib/orbstab.gi:Impl_SchreierVectorWordToBasePoint(-15641547)");
    InitGlobalBag(&(NameFunc[8]), "lib/orbstab.gi:NameFunc[8](-15641547)");

    /* return success */
    return 0;
}

/* 'InitLibrary' sets up gvars, rnams, functions */
static Int InitLibrary(StructInitInfo *module) {
    Obj func1;
    Obj body1;

    /* Complete Copy/Fopy registration */
    UpdateCopyFopyInfo();
    FileName = MakeImmString("lib/orbstab.gi");
    PostRestore(module);

    /* create all the functions defined in this module */
    func1 = NewFunction(NameFunc[1], 0, 0, HdlrFunc1);
    SET_ENVI_FUNC(func1, STATE(CurrLVars));
    CHANGED_BAG(STATE(CurrLVars));
    body1 = NewBag(T_BODY, sizeof(BodyHeader));
    SET_BODY_FUNC(func1, body1);
    CHANGED_BAG(func1);
    CALL_0ARGS(func1);

    /* return success */
    return 0;
}

/* <name> returns the description of this module */
static StructInitInfo module = {
    /* type        = */ 3,
    /* name        = */ "lib/orbstab.gi",
    /* revision_c  = */ 0,
    /* revision_h  = */ 0,
    /* version     = */ 0,
    /* crc         = */ -15641547,
    /* initKernel  = */ InitKernel,
    /* initLibrary = */ InitLibrary,
    /* checkInit   = */ 0,
    /* preSave     = */ 0,
    /* postSave    = */ 0,
    /* postRestore = */ PostRestore};

StructInitInfo *Init__Dynamic(void) { return &module; }

/* compiled code ends here */
