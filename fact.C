#include "nml_runtime.h"
Nword n6_scon = g_mkNum (0);
Ncode n5_cont () {
  return g_call_2 (FRAME (0),n6_scon,CRET);
}
Nword n8_scon = g_mkNum (9);
Nword n9_con_false = g_con0 (1,0);
Nword n10_scon = g_mkNum (10);
Nword n11_con_None = g_con0 (1,0);
Nword n12_scon = g_mkNum (0);
Nword n13_scon = g_mkChar (48);
Ncode n7_loop () {
  Nword q261,q251,c_q252,cs_q253,q255,q256,q254,q258,q257,q260,q259;
  if (g_matchC (ARG (1),1)) {
    q261 = g_NewCon (0,ARG (0));
    return g_returnWith (q261);
  } else {
    q251 = g_DeCon (ARG (1));
    c_q252 = g_DeTuple (q251,0);
    cs_q253 = g_DeTuple (q251,1);
    q255 = builtin_ord (c_q252);
    q256 = builtin_ord (n13_scon);
    q254 = builtin_Dash (q255,q256);
    q258 = builtin_LessEqual (n12_scon,q254);
    if (g_matchC (q258,0)) {
      q257 = builtin_LessEqual (q254,n8_scon);
      goto L_3;
    } else {
      q257 = g_Copy (n9_con_false);
      goto L_3;
    }
    L_3:
    if (g_matchC (q257,0)) {
      q260 = builtin_Star (n10_scon,ARG (0));
      q259 = builtin_Plus (q260,q254);
      return g_call_2 (FRAME (0),q259,cs_q253);
    } else {
      return g_returnWith (n11_con_None);
    }
  }
}
Ncode n4_readInt () {
  Nword loop_q250,q262;
  loop_q250 = g_NewFn (1,2,n7_loop);
  g_SetFrameElement (loop_q250,0,loop_q250);
  g_PushContinuation (1,n5_cont);
  g_SetContFrameElem (0,loop_q250);
  q262 = builtin_explode (ARG (0));
  return g_returnWith (q262);
}
Nword n3_closure_readInt = g_NewFn (0,1,n4_readInt);
Nword n19_con_nil = g_con0 (1,0);
Nword n20_scon = g_mkNum (1);
Ncode n21_cont () {
  Nword q165,q164;
  q165 = g_NewTuple (2);
  g_SetTupleElement (q165,0,FRAME (0));
  g_SetTupleElement (q165,1,CRET);
  q164 = g_NewCon (0,q165);
  return g_returnWith (q164);
}
Ncode n18_upto () {
  Nword a_q159,b_q160,q161,q163,q162;
  a_q159 = g_DeTuple (ARG (0),0);
  b_q160 = g_DeTuple (ARG (0),1);
  q161 = builtin_Greater (a_q159,b_q160);
  if (g_matchC (q161,0)) {
    return g_returnWith (n19_con_nil);
  } else {
    g_PushContinuation (1,n21_cont);
    g_SetContFrameElem (0,a_q159);
    q163 = builtin_Plus (a_q159,n20_scon);
    q162 = g_NewTuple (2);
    g_SetTupleElement (q162,0,q163);
    g_SetTupleElement (q162,1,b_q160);
    return g_call_1 (FRAME (0),q162);
  }
}
extern Nword n17_closure_upto;
Nword n17_closure_upto = g_NewFn (1,1,n18_upto);
Nword n22_scon = g_mkNum (2);
Nword n26_scon = g_mkNum (1);
Ncode n27_cont () {
  Nword q267;
  q267 = builtin_Star (FRAME (0),CRET);
  return g_returnWith (q267);
}
Ncode n25_myProd () {
  Nword q264,x_q265,xs_q266;
  if (g_matchC (ARG (0),1)) {
    return g_returnWith (n26_scon);
  } else {
    q264 = g_DeCon (ARG (0));
    x_q265 = g_DeTuple (q264,0);
    xs_q266 = g_DeTuple (q264,1);
    g_PushContinuation (1,n27_cont);
    g_SetContFrameElem (0,x_q265);
    return g_call_1 (FRAME (0),xs_q266);
  }
}
extern Nword n24_closure_myProd;
Nword n24_closure_myProd = g_NewFn (1,1,n25_myProd);
Ncode n23_cont () {
  return g_call_1 (n24_closure_myProd,CRET);
}
Ncode n16_fact () {
  Nword q269;
  g_PushContinuation (0,n23_cont);
  q269 = g_NewTuple (2);
  g_SetTupleElement (q269,0,n22_scon);
  g_SetTupleElement (q269,1,ARG (0));
  return g_call_1 (n17_closure_upto,q269);
}
Nword n15_closure_fact = g_NewFn (0,1,n16_fact);
Nword n31_scon = g_mkString ("0");
Nword n34_scon = g_mkString ("");
Nword n36_scon = g_mkNum (48);
Ncode n37_cont () {
  Nword q105;
  q105 = builtin_Hat (FRAME (0),CRET);
  return g_returnWith (q105);
}
Ncode n35_cont () {
  Nword q104,q103,q102,q101,q100;
  g_PushContinuation (1,n37_cont);
  g_SetContFrameElem (0,CRET);
  q104 = builtin_Plus (FRAME (0),n36_scon);
  q103 = builtin_chr (q104);
  q102 = g_NewTuple (2);
  g_SetTupleElement (q102,0,q103);
  g_SetTupleElement (q102,1,n19_con_nil);
  q101 = g_NewCon (0,q102);
  q100 = builtin_implode (q101);
  return g_returnWith (q100);
}
Nword n38_scon = g_mkNum (10);
Nword n39_scon = g_mkNum (10);
Ncode n33_stringOfPos () {
  Nword q98,q99;
  if (g_matchNum (ARG (0),0)) {
    return g_returnWith (n34_scon);
  } else {
    q98 = builtin_mod (ARG (0),n39_scon);
    q99 = builtin_div (ARG (0),n38_scon);
    g_PushContinuation (1,n35_cont);
    g_SetContFrameElem (0,q98);
    return g_call_1 (FRAME (0),q99);
  }
}
extern Nword n32_closure_stringOfPos;
Nword n32_closure_stringOfPos = g_NewFn (1,1,n33_stringOfPos);
Nword n41_scon = g_mkString ("~");
Ncode n40_cont () {
  Nword q111;
  q111 = builtin_Hat (n41_scon,CRET);
  return g_returnWith (q111);
}
Nword n42_scon = g_mkNum (0);
Nword n43_scon = g_mkNum (0);
Ncode n30_stringOfInt () {
  Nword q107,q108,q110,q109;
  q107 = builtin_Equal (ARG (0),n43_scon);
  if (g_matchC (q107,0)) {
    return g_returnWith (n31_scon);
  } else {
    q108 = builtin_Less (ARG (0),n42_scon);
    if (g_matchC (q108,0)) {
      g_PushContinuation (0,n40_cont);
      q110 = builtin_Tilda (ARG (0));
      return g_call_1 (n32_closure_stringOfPos,q110);
    } else {
      q109 = g_Copy (ARG (0));
      return g_call_1 (n32_closure_stringOfPos,q109);
    }
  }
}
Nword n29_closure_stringOfInt = g_NewFn (0,1,n30_stringOfInt);
Ncode n28_cont () {
  return g_call_1 (n29_closure_stringOfInt,CRET);
}
Nword n45_scon = g_mkString ("\n");
Ncode n44_cont () {
  Nword q275;
  q275 = builtin_Hat (CRET,n45_scon);
  return g_returnWith (q275);
}
Nword n46_exval = g_mkExname ("Match",1);
Ncode n14_cont () {
  Nword n_q274;
  if (g_matchC (CRET,0)) {
    n_q274 = g_DeCon (CRET);
    g_PushContinuation (0,n44_cont);
    g_PushContinuation (0,n28_cont);
    return g_call_1 (n15_closure_fact,n_q274);
  } else {
    return g_raise (n46_exval);
  }
}
Nword n48_scon = g_mkString (")->");
Nword n49_scon = g_mkString ("fact(");
Ncode n47_cont () {
  Nword q279,q278,q277,q276;
  q279 = builtin_Hat (n49_scon,FRAME (0));
  q278 = builtin_Hat (q279,n48_scon);
  q277 = builtin_Hat (q278,CRET);
  q276 = builtin_print (q277);
  return g_returnWith (q276);
}
Ncode n2_fact_input () {
  Nword q271,arg_q272,q273;
  if (g_matchC (ARG (0),0)) {
    q271 = g_DeCon (ARG (0));
    arg_q272 = g_DeTuple (q271,0);
    q273 = g_DeTuple (q271,1);
    if (g_matchC (q273,1)) {
      g_PushContinuation (1,n47_cont);
      g_SetContFrameElem (0,arg_q272);
      g_PushContinuation (0,n14_cont);
      return g_call_1 (n3_closure_readInt,arg_q272);
    } else {
      return g_raise (n46_exval);
    }
  } else {
    return g_raise (n46_exval);
  }
}
Nword n1_closure_fact_input = g_NewFn (0,1,n2_fact_input);
Nword Init () {
  g_SetFrameElement (n32_closure_stringOfPos,0,n32_closure_stringOfPos);
  g_SetFrameElement (n24_closure_myProd,0,n24_closure_myProd);
  g_SetFrameElement (n17_closure_upto,0,n17_closure_upto);
  return g_unit ();
}
Nword TheProgram = n1_closure_fact_input;
