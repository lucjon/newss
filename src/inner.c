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

static GVar G_StabilizerChainStrip;
static Obj  GC_StabilizerChainStrip;
static GVar G_StabilizerChainStripWord;
static Obj  GC_StabilizerChainStripWord;
static GVar G_IsList;
static Obj  GF_IsList;
static GVar G_PermWordImage;
static Obj  GF_PermWordImage;
static GVar G_PermWordMul;
static Obj  GF_PermWordMul;

/* record names used in handlers */
static RNam R_gens;
static RNam R_invgens;
static RNam R_sv;
static RNam R_size;
static RNam R_point;

static RNam R_base;
static RNam R_chain;
static RNam R_orbit;
static RNam R_residue;
static RNam R_level;

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

static Obj  Impl_StabilizerChainStrip (
 Obj  self,
 Obj  a_bsgs,
 Obj  a_g )
{
 Obj l_h = 0;
 Obj l_i = 0;
 Obj l_beta = 0;
 Obj l_u = 0;
 Obj t_1 = 0;
 Obj t_2 = 0;
 Obj t_3 = 0;
 Obj t_4 = 0;
 Obj t_5 = 0;
 Obj t_6 = 0;
 Obj t_7 = 0;
 Obj t_8 = 0;
 Obj t_9 = 0;
 Bag oldFrame;
 OLD_BRK_CURR_STAT
 
 /* allocate new stack frame */
 SWITCH_TO_NEW_FRAME(self,0,0,oldFrame);
 REM_BRK_CURR_STAT();
 SET_BRK_CURR_STAT(0);
 
 /* h := g; */
 l_h = a_g;
 
 /* i := 0; */
 l_i = INTOBJ_INT(0);
 
 /* for i in [ 1 .. Size( bsgs!.base ) ] do */
 t_3 = GF_Size;
 if ( TNUM_OBJ(a_bsgs) == T_COMOBJ ) {
  t_4 = ElmPRec( a_bsgs, R_base );
#ifdef HPCGAP
 } else if ( TNUM_OBJ(a_bsgs) == T_ACOMOBJ) {
  t_4 = ElmARecord( a_bsgs, R_base );
#endif
 }
 else {
  t_4 = ELM_REC( a_bsgs, R_base );
 }
 t_2 = CALL_1ARGS( t_3, t_4 );
 CHECK_FUNC_RESULT( t_2 )
 CHECK_INT_SMALL( t_2 )
 for ( t_1 = INTOBJ_INT(1);
       ((Int)t_1) <= ((Int)t_2);
       t_1 = (Obj)(((UInt)t_1)+4) ) {
  l_i = t_1;
  
  /* beta := bsgs!.base[i] ^ h; */
  if ( TNUM_OBJ(a_bsgs) == T_COMOBJ ) {
   t_5 = ElmPRec( a_bsgs, R_base );
#ifdef HPCGAP
  } else if ( TNUM_OBJ(a_bsgs) == T_ACOMOBJ) {
   t_5 = ElmARecord( a_bsgs, R_base );
#endif
  }
  else {
   t_5 = ELM_REC( a_bsgs, R_base );
  }
  C_ELM_LIST_FPL( t_4, t_5, l_i )
  t_3 = POW( t_4, l_h );
  l_beta = t_3;
  
  /* if not IsBound( bsgs!.chain[i].orbit.sv[beta] ) then */
  if ( TNUM_OBJ(a_bsgs) == T_COMOBJ ) {
   t_9 = ElmPRec( a_bsgs, R_chain );
#ifdef HPCGAP
  } else if ( TNUM_OBJ(a_bsgs) == T_ACOMOBJ) {
   t_9 = ElmARecord( a_bsgs, R_chain );
#endif
  }
  else {
   t_9 = ELM_REC( a_bsgs, R_chain );
  }
  C_ELM_LIST_FPL( t_8, t_9, l_i )
  t_7 = ELM_REC( t_8, R_orbit );
  t_6 = ELM_REC( t_7, R_sv );
  CHECK_INT_POS( l_beta )
  t_5 = C_ISB_LIST( t_6, l_beta );
  t_4 = (Obj)(UInt)(t_5 != False);
  t_3 = (Obj)(UInt)( ! ((Int)t_4) );
  if ( t_3 ) {
   
   /* return rec(
    residue := h,
    level := i ); */
   t_3 = NEW_PREC( 2 );
   t_4 = (Obj)R_residue;
   AssPRec( t_3, (UInt)t_4, l_h );
   t_4 = (Obj)R_level;
   AssPRec( t_3, (UInt)t_4, l_i );
   SortPRecRNam( t_3, 0 );
   RES_BRK_CURR_STAT();
   SWITCH_TO_OLD_FRAME(oldFrame);
   return t_3;
   
  }
  /* fi */
  
  /* u := SchreierVectorPermToBasePoint( bsgs!.chain[i].orbit, beta ); */
  if ( TNUM_OBJ(a_bsgs) == T_COMOBJ ) {
   t_7 = ElmPRec( a_bsgs, R_chain );
#ifdef HPCGAP
  } else if ( TNUM_OBJ(a_bsgs) == T_ACOMOBJ) {
   t_7 = ElmARecord( a_bsgs, R_chain );
#endif
  }
  else {
   t_7 = ELM_REC( a_bsgs, R_chain );
  }
  C_ELM_LIST_FPL( t_6, t_7, l_i )
  t_5 = ELM_REC( t_6, R_orbit );
  t_3 = Impl_SchreierVectorPermToBasePoint(self, t_5, l_beta);
  CHECK_FUNC_RESULT( t_3 )
  l_u = t_3;
  
  /* h := h * u; */
  C_PROD_FIA( t_3, l_h, l_u )
  l_h = t_3;
  
 }
 /* od */
 
 /* return rec(
    residue := h,
    level := i + 1 ); */
 t_1 = NEW_PREC( 2 );
 t_2 = (Obj)R_residue;
 AssPRec( t_1, (UInt)t_2, l_h );
 t_2 = (Obj)R_level;
 C_SUM_INTOBJS( t_3, l_i, INTOBJ_INT(1) )
 AssPRec( t_1, (UInt)t_2, t_3 );
 SortPRecRNam( t_1, 0 );
 RES_BRK_CURR_STAT();
 SWITCH_TO_OLD_FRAME(oldFrame);
 return t_1;
 
 /* return; */
 RES_BRK_CURR_STAT();
 SWITCH_TO_OLD_FRAME(oldFrame);
 return 0;
}

/* handler for function 28 */
static Obj  Impl_StabilizerChainStripWord (
 Obj  self,
 Obj  a_bsgs,
 Obj  a_g )
{
 Obj l_h = 0;
 Obj l_i = 0;
 Obj l_beta = 0;
 Obj l_u = 0;
 Obj t_1 = 0;
 Obj t_2 = 0;
 Obj t_3 = 0;
 Obj t_4 = 0;
 Obj t_5 = 0;
 Obj t_6 = 0;
 Obj t_7 = 0;
 Obj t_8 = 0;
 Obj t_9 = 0;
 Bag oldFrame;
 OLD_BRK_CURR_STAT
 
 /* allocate new stack frame */
 SWITCH_TO_NEW_FRAME(self,0,0,oldFrame);
 REM_BRK_CURR_STAT();
 SET_BRK_CURR_STAT(0);
 
 /* h := g; */
 l_h = a_g;
 
 /* i := 0; */
 l_i = INTOBJ_INT(0);
 
 /* if not IsList( h ) then */
 t_4 = GF_IsList;
 t_3 = CALL_1ARGS( t_4, l_h );
 CHECK_FUNC_RESULT( t_3 )
 CHECK_BOOL( t_3 )
 t_2 = (Obj)(UInt)(t_3 != False);
 t_1 = (Obj)(UInt)( ! ((Int)t_2) );
 if ( t_1 ) {
  
  /* h := [ h ]; */
  t_1 = NEW_PLIST( T_PLIST, 1 );
  SET_LEN_PLIST( t_1, 1 );
  SET_ELM_PLIST( t_1, 1, l_h );
  CHANGED_BAG( t_1 );
  l_h = t_1;
  
 }
 /* fi */
 
 /* for i in [ 1 .. Size( bsgs!.base ) ] do */
 t_3 = GF_Size;
 if ( TNUM_OBJ(a_bsgs) == T_COMOBJ ) {
  t_4 = ElmPRec( a_bsgs, R_base );
#ifdef HPCGAP
 } else if ( TNUM_OBJ(a_bsgs) == T_ACOMOBJ) {
  t_4 = ElmARecord( a_bsgs, R_base );
#endif
 }
 else {
  t_4 = ELM_REC( a_bsgs, R_base );
 }
 t_2 = CALL_1ARGS( t_3, t_4 );
 CHECK_FUNC_RESULT( t_2 )
 CHECK_INT_SMALL( t_2 )
 for ( t_1 = INTOBJ_INT(1);
       ((Int)t_1) <= ((Int)t_2);
       t_1 = (Obj)(((UInt)t_1)+4) ) {
  l_i = t_1;
  
  /* beta := PermWordImage( bsgs!.base[i], h ); */
  t_4 = GF_PermWordImage;
  if ( TNUM_OBJ(a_bsgs) == T_COMOBJ ) {
   t_6 = ElmPRec( a_bsgs, R_base );
#ifdef HPCGAP
  } else if ( TNUM_OBJ(a_bsgs) == T_ACOMOBJ) {
   t_6 = ElmARecord( a_bsgs, R_base );
#endif
  }
  else {
   t_6 = ELM_REC( a_bsgs, R_base );
  }
  C_ELM_LIST_FPL( t_5, t_6, l_i )
  t_3 = CALL_2ARGS( t_4, t_5, l_h );
  CHECK_FUNC_RESULT( t_3 )
  l_beta = t_3;
  
  /* if not IsBound( bsgs!.chain[i].orbit.sv[beta] ) then */
  if ( TNUM_OBJ(a_bsgs) == T_COMOBJ ) {
   t_9 = ElmPRec( a_bsgs, R_chain );
#ifdef HPCGAP
  } else if ( TNUM_OBJ(a_bsgs) == T_ACOMOBJ) {
   t_9 = ElmARecord( a_bsgs, R_chain );
#endif
  }
  else {
   t_9 = ELM_REC( a_bsgs, R_chain );
  }
  C_ELM_LIST_FPL( t_8, t_9, l_i )
  t_7 = ELM_REC( t_8, R_orbit );
  t_6 = ELM_REC( t_7, R_sv );
  CHECK_INT_POS( l_beta )
  t_5 = C_ISB_LIST( t_6, l_beta );
  t_4 = (Obj)(UInt)(t_5 != False);
  t_3 = (Obj)(UInt)( ! ((Int)t_4) );
  if ( t_3 ) {
   
   /* return rec(
    residue := h,
    level := i ); */
   t_3 = NEW_PREC( 2 );
   t_4 = (Obj)R_residue;
   AssPRec( t_3, (UInt)t_4, l_h );
   t_4 = (Obj)R_level;
   AssPRec( t_3, (UInt)t_4, l_i );
   SortPRecRNam( t_3, 0 );
   RES_BRK_CURR_STAT();
   SWITCH_TO_OLD_FRAME(oldFrame);
   return t_3;
   
  }
  /* fi */
  
  /* u := SchreierVectorWordToBasePoint( bsgs!.chain[i].orbit, beta ); */
  if ( TNUM_OBJ(a_bsgs) == T_COMOBJ ) {
   t_7 = ElmPRec( a_bsgs, R_chain );
#ifdef HPCGAP
  } else if ( TNUM_OBJ(a_bsgs) == T_ACOMOBJ) {
   t_7 = ElmARecord( a_bsgs, R_chain );
#endif
  }
  else {
   t_7 = ELM_REC( a_bsgs, R_chain );
  }
  C_ELM_LIST_FPL( t_6, t_7, l_i )
  t_5 = ELM_REC( t_6, R_orbit );
  l_u = Impl_SchreierVectorWordToBasePoint(self, t_5, l_beta);
  CHECK_FUNC_RESULT( l_u )
  
  /* h := PermWordMul( h, u ); */
  t_4 = GF_PermWordMul;
  t_3 = CALL_2ARGS( t_4, l_h, l_u );
  CHECK_FUNC_RESULT( t_3 )
  l_h = t_3;
  
 }
 /* od */
 
 /* return rec(
    residue := h,
    level := i + 1 ); */
 t_1 = NEW_PREC( 2 );
 t_2 = (Obj)R_residue;
 AssPRec( t_1, (UInt)t_2, l_h );
 t_2 = (Obj)R_level;
 C_SUM_INTOBJS( t_3, l_i, INTOBJ_INT(1) )
 AssPRec( t_1, (UInt)t_2, t_3 );
 SortPRecRNam( t_1, 0 );
 RES_BRK_CURR_STAT();
 SWITCH_TO_OLD_FRAME(oldFrame);
 return t_1;
 
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

	t_1 = GF_InstallGlobalFunction;
	t_2 = GC_StabilizerChainStrip;
	CHECK_BOUND( t_2, "StabilizerChainStrip" )
	t_3 = NewFunction( NameFunc[2], 2, 0, Impl_StabilizerChainStrip );
	SET_ENVI_FUNC( t_3, STATE(CurrLVars) );
	t_4 = NewBag( T_BODY, sizeof(BodyHeader) );
	SET_STARTLINE_BODY(t_4, 481);
	SET_ENDLINE_BODY(t_4, 497);
	SET_FILENAME_BODY(t_4, FileName);
	SET_BODY_FUNC(t_3, t_4);
	CHANGED_BAG( STATE(CurrLVars) );
	CALL_2ARGS( t_1, t_2, t_3 );

	t_1 = GF_InstallGlobalFunction;
	t_2 = GC_StabilizerChainStripWord;
	CHECK_BOUND( t_2, "StabilizerChainStripWord" )
	t_3 = NewFunction( NameFunc[3], 2, 0, Impl_StabilizerChainStripWord );
	SET_ENVI_FUNC( t_3, STATE(CurrLVars) );
	t_4 = NewBag( T_BODY, sizeof(BodyHeader) );
	SET_STARTLINE_BODY(t_4, 500);
	SET_ENDLINE_BODY(t_4, 520);
	SET_FILENAME_BODY(t_4, FileName);
	SET_BODY_FUNC(t_3, t_4);
	CHANGED_BAG( STATE(CurrLVars) );
	CALL_2ARGS( t_1, t_2, t_3 );


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
	G_StabilizerChainStrip = GVarName( "StabilizerChainStrip" );
	G_StabilizerChainStripWord = GVarName( "StabilizerChainStripWord" );
	G_IsList = GVarName( "IsList" );
	G_PermWordImage = GVarName( "PermWordImage" );
	G_PermWordMul = GVarName( "PermWordMul" );

    /* record names used in handlers */
    R_gens = RNamName("gens");
    R_invgens = RNamName("invgens");
    R_sv = RNamName("sv");
    R_size = RNamName("size");
    R_point = RNamName("point");
	R_base = RNamName( "base" );
	R_chain = RNamName( "chain" );
	R_orbit = RNamName( "orbit" );
	R_residue = RNamName( "residue" );
	R_level = RNamName( "level" );

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
	InitCopyGVar( "StabilizerChainStrip", &GC_StabilizerChainStrip );
	InitCopyGVar( "StabilizerChainStripWord", &GC_StabilizerChainStripWord );
	InitFopyGVar( "IsList", &GF_IsList );
	InitFopyGVar( "PermWordImage", &GF_PermWordImage );
	InitFopyGVar( "PermWordMul", &GF_PermWordMul );

    /* information for the functions */
    InitGlobalBag(&FileName, "lib/orbstab.gi:FileName(-15641547)");
    InitHandlerFunc(HdlrFunc1, "lib/orbstab.gi:HdlrFunc1(-15641547)");
    InitGlobalBag(&(NameFunc[1]), "lib/orbstab.gi:NameFunc[1](-15641547)");
    InitHandlerFunc(Impl_NEWSS_SchreierVector,
                    "src/inner.c:Impl_NEWSS_SchreierVector(-15641547)");
    InitGlobalBag(&(NameFunc[4]), "lib/orbstab.gi:NameFunc[4](-15641547)");
    InitHandlerFunc(Impl_ExtendSchreierVector,
                    "src/inner.c:Impl_ExtendSchreierVector(-15641547)");
    InitGlobalBag(&(NameFunc[5]), "lib/orbstab.gi:NameFunc[5](-15641547)");
    InitHandlerFunc(
        Impl_SchreierVectorPermToBasePoint,
        "src/inner.c:Impl_SchreierVectorPermToBasePoint(-15641547)");
    InitGlobalBag(&(NameFunc[6]), "lib/orbstab.gi:NameFunc[6](-15641547)");
    InitHandlerFunc(
        Impl_SchreierVectorWordToBasePoint,
        "src/inner.c:Impl_SchreierVectorWordToBasePoint(-15641547)");
    InitGlobalBag(&(NameFunc[8]), "lib/orbstab.gi:NameFunc[8](-15641547)");
	InitHandlerFunc(
		Impl_StabilizerChainStrip,
		"src/inner.c:Impl_StabilizerChainStrip(-15641547)");
	InitGlobalBag(&(NameFunc[2]), "lib/ss.gi:NameFunc[2](-15641547)");
	InitHandlerFunc(
		Impl_StabilizerChainStrip,
		"src/inner.c:Impl_StabilizerChainStripWord(-15641547)");
	InitGlobalBag(&(NameFunc[3]), "lib/ss.gi:NameFunc[3](-15641547)");

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
    /* type        = */ MODULE_DYNAMIC,
    /* name        = */ "lib/inner.gi",
    /* revision_c  = */ 0,
    /* revision_h  = */ 0,
    /* version     = */ 0,
    /* crc         = */ 0,
    /* initKernel  = */ InitKernel,
    /* initLibrary = */ InitLibrary,
    /* checkInit   = */ 0,
    /* preSave     = */ 0,
    /* postSave    = */ 0,
    /* postRestore = */ PostRestore};

StructInitInfo *Init__Dynamic(void) { return &module; }

/* compiled code ends here */
