
\d .shape

del:{![y;();1b;$[0>type x;enlist;(::)] x]}

melt:{(x,`variable`val) xcols ungroup flip(`variable,x,`val)!flip c,'y each x,/:c:cols[y] except x}

cast:{x:?[0>type x;enlist x;x];m:(first 1#0#)each group(y`val)!y`variable;?[y;();x!x;({z,x!y};`variable;`val;m)]}

\d .


\d .xgb

a:{parse["select ",x," from t"]4}
formula:formula!formula:`id`y`feature`val
l:{[y;yhat] neg (y* log p) + (1 - y) * log 1 - p:1 % 1 + exp neg yhat  }
L:{[l;y;yhat] avg l[y;yhat]}l
g:{[y;yhat] neg y  - 1 % 1 + exp neg yhat  }
h:{[y;yhat] (1 - p)*p:1 % 1 + exp neg yhat }

lambda:1f
gamma:2.5
print:1b
debug:0b
max_depth:4
nround:4

opt:``lambda`gamma`max_depth`nround`l`L`g`h`print`debug!{},lambda,gamma,max_depth,nround,l,L,g,h,print,debug

frm:{x!x:value x}

cscore0:{[formula;tbl;dir;g;h;lambda;gamma]
 tbl:![tbl;();0b;`ghat`hhat!((g;formula`y;`yhat);(h;formula`y;`yhat))];
 tbl:![tbl;();0b;`gl`hl`gr`hr!((sums;`ghat);(sums;`hhat);({sum[x] - sums x};`ghat);({sum[x] - sums x};`hhat))];
 tbl:![tbl;();0b;enlist[`score]!enlist(-;({[G;H;lambda;gl;hl;gr;hr] (gl * gl % hl + lambda) + (gr * gr % hr + lambda) - (G * G) % H + lambda };(sum;`ghat);(sum;`hhat);`lambda;`gl;`hl;`gr;`hr);gamma) ];
 update mdir:dir,optimal: 0.5*wneg+wpos from ?[tbl;enlist (=;`score;(max;`score));0b; .xgb.frm[`feature`val#formula],(`score`wneg`wpos)!(`score;({neg x % y + z};`gl;`hl;lambda);({neg x % y + z};`gr;`hr;lambda)) ]
 }

cscore:{[tbl;formula;g;h;iter;lambda;gamma;f]
 f:$[-11h = type f;(),f;f];
 tmp:?[tbl;((=;formula`feature;f);(in;formula`id;iter formula`id));0b;.xgb.frm formula];
 tmp:tmp lj 1! ?[iter;();0b;.xgb.frm (`id`y#formula),(enlist`yhat)!enlist`yhat];
 missing:?[iter;enlist({not x in y};formula`id;tmp formula`id);0b; .xgb.frm `id`y#formula];
 tmpneg:![;();0b;`feature`val!{ ({reverse fills reverse x};x) }@'formula`feature`val]0!(formula[`id] xkey missing) uj (formula[`id] xkey tmp);
 tmppos:![;();0b;`feature`val!{ (fills;x) }@'formula`feature`val] 0!(formula[`id] xkey tmp) uj (formula[`id] xkey missing);
 1# `score xdesc cscore0[formula;tmppos;`wpos;g;h;lambda;gamma],cscore0[formula;tmpneg;`wneg;g;h;lambda;gamma]
 }


node:{[ind;opt;responses]
 tbegin:.z.P;
 if[count[ind] > opt`max_depth;:`max_depth];
 formula:opt`formula;tbl:opt`tbl;g:opt`g;h:opt`h;lambda:opt`lambda;features:opt`features;gamma:opt`gamma;
 tfeatures:$[11h = abs type features;enlist features;features];
 features:?[;enlist(not;`same);();formula`feature] 0!?[tbl;((in;formula`feature;tfeatures);(in;formula`id;(),responses formula`id));enlist[formula`feature]!enlist[formula`feature];enlist[`same]!enlist({ (~) . (first;last) @\:x};formula`val)];
 ascore: raze .xgb.cscore[tbl;formula;g;h;responses;lambda;gamma]@'features;
 if[ascore ~ ();:`zero_features];
 mscore: first select from ascore where score = max score;
 if[ null mscore formula`feature ;:`null_feature];
 if[ .000001 >  mscore`score ;:`zero_score];
 mx:$[-11h=type mx:mscore formula`feature;(),mx;mx];
 ids:distinct ?[;();();formula`id] ?[tbl;enlist (=;formula`feature;mx);0b;()];
 missing:?[responses;enlist({not x in y};formula`id;ids);();formula`id];
 idsneg:?[responses;;();formula`id] enlist (in;formula`id), enlist ?[;();();formula`id] ?[tbl;((=;formula`feature;mx);(<=;formula`val;mscore formula`val));0b;()];
 idspos:?[responses;;();formula`id] enlist (in;formula`id), enlist ?[;();();formula`id] ?[tbl;((=;formula`feature;mx);(>;formula`val;mscore formula`val));0b;()];
 idsneg:$[`wneg ~ mscore`mdir;distinct idsneg,missing;idsneg];
 idspos:$[`wpos ~ mscore`mdir;distinct idspos,missing;idspos];
 iterneg:update yhat:yhat + mscore`wneg from ?[responses;enlist (in;formula`id;idsneg);0b;()];
 iterpos:update yhat:yhat + mscore`wpos from ?[responses;enlist (in;formula`id;idspos);0b;()];
 opt[`features]:opt[`features] except mscore formula`feature;
 tend:.z.P - tbegin;
 mscore:(`dur`ind`cnt!(tend;ind;count responses)),mscore;
 if[opt`debug;0N!value mscore];
 if[opt`debug;mscore[`ascore]:ascore];
 rneg:node[ind,0b;opt;iterneg];
 mscore[`rneg]:$[-11h = type rneg;rneg;`continuous];
 rpos:node[ind,1b;opt;iterpos];
 mscore[`rpos]:$[-11h = type rpos;rpos;`continuous];
 :(mscore;$[-11h = type rneg;iterneg;rneg];$[-11h = type rpos;iterpos;rpos])
 }

leaf:{leaf0[enlist 0b]x }
leaf0:{[x;y]if[0h=type y;: .z.s[x,0b;y 1],.z.s[x,1b;y 2] ];update ind:count[y]#enlist x from y}

gtree0:{if[0h=type x;:x[enlist 0],.z.s[x 1],.z.s[x 2] ]; :() }

gtree:{[num;x]`num`seq`parent xcols update num:num,seq:i,parent:{(first where@)@'(-1_'x)~/:\:x}ind,dir:last@'ind from gtree0 x}

tree0:{[opt;responses]

 result:`tree`train!{[opt;x]
  result:.xgb.node[enlist 0b;opt;x 3];
  if[-11h = type result; :result,1_x];
  train:.xgb.leaf result;
  tree:.xgb.gtree[x 1] result;
  if[opt`print;0N!.t.print["nround = %0 | L = %1"](x 1; opt[`L] . train(opt[`formula]`y),`yhat )];
  :(`continue;1+x 1; x[2],tree ; train)
 }[opt]/[{[nround;x] (nround > x 1) and `continue=x 0 }[opt`nround];(`continue;0;();responses)] 2 3;
  0N!"Ready";
  result
 }

tree:{[opt;data;formula]
 formula:.xgb.formula,(enlist[`feature]!enlist `variable),.xgb.a formula;
 tfeatures:`variable`val xasc .shape.melt[formula`id`y;data];
 opt:.xgb.opt,(``tbl`formula!({};tfeatures;formula)),opt;
 opt[`features]:exec feature from ?[opt`tbl;();enlist[`feature]#opt`formula;()];
 data:update yhat:0.0 from data;
 .xgb.tree0[opt;data] , (enlist `formula)#opt
 }

apply1_:{[ind;opt;responses]
 formula:opt`formula;tbl:opt`tbl;mtree:opt`mtree;
 mscore:mtree[ind];
 if[null mscore`num;:responses];
 mx:$[-11h=type mx:mscore formula`feature;(),mx;mx];
 ids:distinct ?[;();();formula`id] ?[tbl;enlist (=;formula`feature;mx);0b;()];
 missing:?[responses;enlist({not x in y};formula`id;ids);();formula`id];
 idsneg:?[responses;;();formula`id] enlist (in;formula`id), enlist ?[;();();formula`id] ?[tbl;((=;formula`feature;mx);(<=;formula`val;mscore formula`val));0b;()];
 idspos:?[responses;;();formula`id] enlist (in;formula`id), enlist ?[;();();formula`id] ?[tbl;((=;formula`feature;mx);(>;formula`val;mscore formula`val));0b;()];
 idsneg:$[`wneg ~ mscore`mdir;distinct idsneg,missing;idsneg];
 idspos:$[`wpos ~ mscore`mdir;distinct idspos,missing;idspos];
 iterneg:update yhat:yhat + mscore`wneg from ?[responses;enlist (in;formula`id;idsneg);0b;()];
 iterpos:update yhat:yhat + mscore`wpos from ?[responses;enlist (in;formula`id;idspos);0b;()];
 rneg:apply1_[ind,0b;opt;iterneg];
 rpos:apply1_[ind,1b;opt;iterpos];
 :rneg,rpos}


apply0_:{[opt;responses;num0]
 opt[`mtree]:`ind xkey select from opt`tree where num = num0;
 apply1_[enlist 0b;opt;responses]
 }

apply:{[opt;responses]
 formula:opt`formula;
 tfeatures:`variable`val xasc .shape.melt[formula`id`y;responses];
 opt[`tbl]:tfeatures;
 responses:update yhat:0f from responses;
 apply0[opt;responses]}

apply0:{[opt;responses] apply0_[opt]/[enlist[responses],asc distinct exec num from opt`tree]}


/
First Example

data:flip`id`y`x0`x1`x2`x3!()

`data insert 0,11000b
`data insert 1,00100b
`data insert 2,00010b
`data insert 3,00001b
`data insert 4,11100b
`data insert 5,11010b
`data insert 6,11001b

r:.xgb.tree[()!();data;""]


r`tree
r`train

.xgb.apply[r]data



