# file qsum.mpl (A package for q-hypergeometric summation)
#
# Copyright (c) Harald Boeing & Wolfram Koepf,
#               Konrad-Zuse-Zentrum Berlin, 1998
# adapted to Maple 6 by Wolfram Koepf, 2001
# renamed qsum6.mpl
# Copyright (C) Wolfram Koepf, University of Kassel, 2001-2004
# adapted to Maple 9 by Wolfram Koepf, 2004      
# renamed qsum9.mpl  
# adapted to Maple 13 by Wolfram Koepf, 2010      
# renamed qsum13.mpl  
#
# Version 1.0, Feb 12, 1998
# Version 1.1, Jan 02, 2001
# Version 1.2, Feb 28, 2004
# Version 1.3, Apr 12, 2010 
#
#
# History:
# Jan 02, 2001, adaption for Maple 6: .->cat 
# Jan 02, 2001, _EnvExplicit
# Jan 02, 2001, In Maple 6, as a result of Maple's use of the variable 
# `result`, the syntax of `qrecsolve` for hypergeometric output had to 
# be changed. It uses no longer the option `return = qhypergeometric` 
# but instead `output = qhypergeometric`.
# Feb 28, 2004, adaption to Maple 9: bug in gausselim, not yet resolved,
# but instead solve is called, see "Maple 9"
# Mar 13 and Apr 12, 2010, sum->add and bug involving nestes subs resolved (WK)
#
#
# Prof. Dr. Wolfram Koepf
# Department 17 Mathematics/Computer Science
# University of Kassel
# Heinrich-Plett-Str. 40
# 34132 Kassel
# Tel.: 0561/804-4207
# Fax: 0561/804-4646
# Secretary: 0561/804-4692, 0561/804-4632
# e-mail: koepf@mathematik.uni-kassel.de
# URL: http://www.mathematik.uni-kassel.de/~koepf
#
#
# There is an accompanying help file called maple.hdb, which
# will be accessible via Maple by adding the directory where this
# file is stored to the Maple's search path stored in libname:
# E.g. enter in your Maple session
#
#  > libname:= libname, `c:/Maple`;
#
# if you use Windows and have the file maple.hdb is stored in
# c:\Maple\maple.hdb or similar for UNIX
#
#  > libname:= libname, `/home/myhome/maple.hdb`;
#
# After executing this command you should try to enter
#
#  > ?qsum
#
# in your Maple session. (Note that ">" corresponds to the Maple
# prompt!)
#
# If you have any problems or want to report some bugs please
# contact Wolfram Koepf <koepf@mathematik.uni-kassel.de>.
#
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
#
#
########################################################################
#                                                                      #
# `power/combine/recursive`: Use the following rules                   #
#                                                                      #
#       (i)   (x^i)^j  ->  x^(i*j)                                     #
#      (ii)   (x*y)^j  ->  x^j*y^j                                     #
#                                                                      #
# to simplify power expressions, b^e, where the input is the base b    #
# and the exponent e.                                                  #
#                                                                      #
########################################################################

`power/combine/recursive`:= proc(b,e)
	if type(b,`*`) then
		map(procname,b,e);
	elif type(b,`^`) then
		procname(op(1,b),op(2,b)*e);
	else
		b^e;
	fi;
end:

########################################################################
#                                                                      #
# `power/combine`: Tries to collect exponents via the rules            #
#                                                                      #
#       (i)   (x^i)^j  ->  x^(i*j)                                     #
#      (ii)   x^i*x^j  ->  x^(i+j)                                     #
#                                                                      #
########################################################################

`power/combine`:= proc()
	local f, tmp, sub, p, ex, j; 
	option `Copyright (c) 1998 Harald Boeing.`;
	f:= args[1];
	if type(f,`^`) then
		f:= `power/combine/recursive`(op(f));
	fi;
	if not(hastype(f,`^`)) then
		RETURN(f);
	elif not(type(f,`*`)) then
		RETURN(map(procname,f));
	fi;
	ex:= table(sparse);
	for j in f do
		j:= procname(j);
		if type(j,`^`) then
			ex[op(1,j)]:= ex[op(1,j)] + op(2,j);
		else
			ex[j]:= ex[j] + 1;
		fi;
	od;
# MapleV4:
#		mul(j^expand(ex[j]),j=map(op,[indices(ex)]));
# MapleV3:
	convert([seq(j^expand(ex[j]),j=map(op,[indices(ex)]))],`*`);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `power/expand`: power simplificatication                             #
#                                                                      #
########################################################################


`power/expand`:= proc(t)
	local arg, roottreatment, rootunsubs, f, Imagin, eulerE, z, powlist, sub;
	option `Copyright (c) 1998 by Harald Boeing.`;
	# Process optional arguments first
	arg:= [seq(args[z],z=2..nargs)];
	`qsum/hasoption`(arg,root,roottreatment,'arg');
	`qsum/hasoption`(arg,unsubs,rootunsubs,'arg');
	#
	f:= eval(subs(I=Imagin,exp=unapply(eulerE^'z','z'), t));
	f:= `power/expand/map`(f);
#	powlist:= indets(f,((-1)^name)^integer);
#	sub:= select(proc(z) if is(op([1,2],z),integer) then true\
#		else false fi end, powlist);
sub:= indets(f,((-1)^name)^even);
	f:= subs([seq(z=(-1)^op(2,z),z=sub)],f);
	if (roottreatment = 'standardize') then
			f:= `power/expand/root`(f);
	elif (roottreatment = 'eliminate') then
		f:= `power/root/eliminate`(f,rootunsubs);
	fi;
	sub:= [op(indets(f, identical(eulerE)^anything))];
	subs(seq(z=exp(op(2,z)),z=sub),Imagin=I, f);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `power/expand/map`: apply `power/expand/recursive` to all powers     #
#                     of the input expressions                         #
#                                                                      #
########################################################################
#                                                                      #
# `power/expand/recursive`: procname(b,e)->b^e, with an application of #
#                                                                      #
#       (i)   (x^i)^j  ->  x^(i*j)                                     #
#      (ii)   (x*y)^i  ->  x^i*y^i                                     #
#     (iii)   x^(i+j)  ->  x^i*x^j                                     #
#                                                                      #
# Additionally the powers are standardized via Maple's function        #
# expand, e.g. x^(i*y)->(x^y)^i, if i is an integer.                   #
#                                                                      #
########################################################################

`power/expand/map`:= proc(f)
	option `Copyright (c) 1998 by Harald Boeing.`;
	if type(f,`^`) then
		`power/expand/recursive`(procname(op(1,f)),procname(op(2,f)));
	elif hastype(f,`^`) then
		map(procname,f);
	else
		f;
	fi
end:

# ----------------------------------------------------------------------

`power/expand/recursive`:= proc(b,e)
	local j, ba;
	option `Copyright (c) 1998 by Harald Boeing.`;
	if type(e,`+`) then
		convert([seq(procname(b,j),j=e)], `*`);
	elif type(b,`*`) then
		map(procname,b,e);
	elif type(b,`^`) then
		procname(op(1,b),expand(normal(op(2,b)*e)))
	elif (denom(e) <> 1) then
		# substitute the denom of e by symbolic j to get simple form
		subs(ba=b,j=denom(e),expand(ba^(numer(e)/j)));
	else
		subs(ba=b,expand(ba^e));
	fi;
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
#  `power/subs`: substitute powers of type name^name in an expression  #
#                                                                      #
#  Input: subst - a single substitution or a list/set of substitutions #
#         expr - any expression                                        #
#                                                                      #
#  Output: subs(subst,expr), where powers of type name^name should all #
#          be correctly substituted.                                   #
#                                                                      #
#                                                                      #
#    The idea is to use the Maple command subs and check if this one   #
#    does already the job. If not we use the procedure                 #
#    `power/subs/recursive` and tyr to substitute all remaining        #
#    possibilities.                                                    #
#                                                                      #
#    Those 'remaining substitutions' are stored in the global variable #
#    `power/subs/subs` in the form:                                    #
#                                                                      #
#        {b1^e1=x1, b2^e2=x2, ...} --> [[b1,e1,x1],[b2,e2,x2],...]     #
#                                                                      #
#    Also store in `power/subs/subnames` [b1,b2,...].                  #
#                                                                      #
########################################################################

`power/subs`:= proc(subst, expr)
	local sub, subnames, f, z;
	global `power/subs/sub`, `power/subs/subnames`;
	if type(subst,equation) then
		sub:= {subst};
	elif type(subst,{list(equation),set(equation)}) then
		sub:= {op(subst)};
	else
		ERROR(`Wrong arguments.`);
	fi;
	f:= subs(sub, expr);
	# Now we check if all powers were substituted...
	sub:= select(type, sub, name^name=anything);
	subnames:= map(proc(z) op(2,lhs(z)) end, sub) intersect indets(f,name);
	if (subnames <> {}) then
		sub:= select(unapply('has'('op'(1,z),subnames),z), sub);
		`power/subs/sub`:= [seq([op(op(1,z)),op(2,z)],z=sub)];
		`power/subs/subnames`:= [seq(op(1,op(1,z)),z=sub)];
		# Determine all power substitutions via `power/expand/recursive`
		subnames:= indets(f,anything^anything) minus\
			indets(f,{anything^(-1),name^name,name^integer,\
			(name^name)^integer,identical(I)});
		f:= subs([seq(z=`power/subs/recursive`(normal(op(1,z)),\
			expand(normal(op(2,z)))), z=subnames)], f);
	fi;
	RETURN(f);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `power/subs/recursive`: tries to apply the substitutions stored in   #
#    `power/subs/subs` in the form to b^e                              #
#                                                                      #
########################################################################

`power/subs/recursive`:= proc(b,e)
	local z;
   global `power/subs/sub`, `power/subs/subnames`;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if type(e,`+`) then
		convert([seq(procname(b,z),z=e)], `*`);
	elif type(b,`*`) then
		map(procname,b,e);
	elif type(b,`^`) then
		procname(op(1,b),expand(normal(op(2,b)*e)))
	elif type(b,name) and member(b,`power/subs/subnames`,'z') then
		z:= `power/subs/sub`[z];
		if (denom(numer(e)/z[2]) = 1) then
			expand(z[3]^(e/z[2]));
		else
			expand(b^e);
		fi;
	elif hastype(b,`^`) and has(b,`power/subs/subnames`) then
		subs([seq(z=procname(op(z)),\
			z=indets(b,`^`) minus indets(b,anything^(-1)))],b)^e;
	else
		expand(b^e);
	fi;
end:

########################################################################
#                                                                      #
# `power/expand/root/dismantle_power`: Applies transformation          #
#                                                                      #
#   z^((i*x)/(j*y))  ->  [z,x,y,j,i]   , where j integer, x,y anything #
#                                                                      #
########################################################################

`power/expand/root/dismantle_power`:= proc(pow)
	local ex, j, i;
	option `Copyright (c) 1997 by Harald Boeing.`;
	ex:= op(2,pow);
	j:= select(type,'j*i'*ex,rational);
	i:= numer(j);
	j:= denom(j);
	[op(1,pow), numer(ex)/i, denom(ex)/j, j, i];
end:

########################################################################
#                                                                      #
# `power/expand/root`: 'Standardizes roots in an expression, such that #
#                      Maple can better deal with them, e.g.           #
#                                                                      #
#    a^x  ->  (a^(x/i))^i, if i is an integer and a^(1/i) appears also #
#                                                                      #
########################################################################

`power/expand/root`:= proc()
	local f, powlist, z, b, i, j, sub, root, tmp;
	option `Copyright (c) 1997 by Harald Boeing.`;
	f:= args[1];
	powlist:= map(proc(z) [op(z)] end,\
		[op(indets(f,`^`) minus {I})]);
	if (powlist = []) then RETURN(f); fi;
	powlist:= map(proc(z) if type(z[1],`^`) then [op(1,z[1]),\
		op(2,z[1])*z[2],z[1]^z[2]] else [op(z),z[1]^z[2]] fi end, powlist);
	for z in powlist do
		if assigned(b[z[1]]) then
			b[z[1]]:= [op(b[z[1]]), [z[2],z[3]]];
		else
			b[z[1]]:= [[z[2],z[3]]];
		fi;
	od;
	sub:= NULL;
	for z in map(op,[indices(b)]) do
		if (nops(b[z]) = 1) then next fi;
		j:= [seq(`power/expand/root/dismantle_power`(z^i[1]),i=b[z])];
		root:= lcm(seq(i[4], i=j));
		if (root <> 1) then
			sub:= sub, seq(b[z][i][2]=(j[i][1]^(j[i][2]/j[i][3]/root))^\
				(root*j[i][5]/j[i][4]),i=1..nops(b[z]));
		fi;
	od;
	subs({sub},f);
end:

# ----------------------------------------------------------------------

`power/root/eliminate`:= proc()
	local f, powlist, z, sub, unsub, b, root, i, j;
	option `Copyright (c) 1997 by Harald Boeing.`;
	f:= args[1];
	powlist:= [op(indets(f,`^`) minus {I})];
	powlist:= map(proc(z) [op(z)] end,powlist);
	powlist:= map(proc(z) if type(z[1],`^`) then [op(1,z[1]),\
		op(2,z[1])*z[2]] else z fi end, powlist);
	powlist:= select(proc(z) type(denom(z[2])-1,posint) end, powlist);
	if (powlist = []) then
		if (nargs = 2) then assign(args[2],{}); fi;
		RETURN(f);
	fi;
	for z in powlist do
		if assigned(b[z[1]]) then
			b[z[1]]:= b[z[1]], z[2];
		else
			b[z[1]]:= z[2];
		fi;
	od;
	sub:= NULL;
	unsub:= NULL;
	for z in map(op,[indices(b)]) do
		j:= [seq(`power/expand/root/dismantle_power`(z^i),i=[b[z]])];
		root:= lcm(seq(i[4], i=j));
		if (root <> 1) then
			sub:= sub, z=z^root;
			unsub:= unsub, z=z^(1/root);
		fi;
	od;
	if (nargs = 2) then
		assign(args[2],{unsub});
	fi;
	f:= `power/expand`(subs({sub},f));
end:

# ----------------------------------------------------------------------

`power/root/restore`:= proc(f, sub);
	`power/expand`(subs(sub, f));
end:

# ----------------------------------------------------------------------

`power/power2name`:= proc(f, UNSUB)
	local z, eulerE, powlist, unsub, pow, j, sub, tmp, i;
	option `Copyright (c) 1997 by Harald Boeing.`;
	z:= eval(subs(exp=unapply(eulerE^j,j), f));
	powlist:= select(proc(j) evalb(nops(indets(j,name))>1) end,\
		indets(z,name^anything));
	unsub:= NULL;
	for pow in powlist do
		j:= cat(seq(j,j=sort([op(indets(op(2,pow),name))],lexorder)));
		sub:= parse(cat(`(proc() local ```,j,```;```,j,``` end)();`), statement);
		tmp:= subs(pow=sub, z);
		if not(has(tmp,indets(op(2,pow)))) then
			z:= tmp;
			if type(op(1,pow), identical(eulerE)) then
				unsub:= unsub, sub=exp(op(2,pow));
			else
				unsub:= unsub, sub=pow;
			fi;
		fi;
	od;
	UNSUB:= {unsub};
	z;
end:

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
########################################################################
#                                                                      #
#                                                                      #
#                                                                      #
#                                                                      #
#                                                                      #
#                                                                      #
#                                                                      #
#                                                                      #
#                                                                      #
########################################################################
#
# Optionale Parameter:
# - factor, sparsefactor, Factor
# - normal, Normal, normalexpanded
# - useorder (Weist gausselim an, die Variablenreihenfolge zu
#             benutzen, wie sie uebergeben wurde.)
# - Variablenliste von Variablen nach denen zuerst eliminiert wird.
# - specialsolution (Sollte das Gleichungssystem unterbestimmt sein,
#							so werden einige Variablen auf Null gesetzt,
#							so da"s eine eindeutige L"osung herauskommt)
#
# ======================================================================

`gausselim/procedures`:= 
	{factor, sparsefactor, Factor, normal, Normal, normalexpanded}:

# ----------------------------------------------------------------------

`gausselim/normal/procedures/RootOf`:= {Factor, Normal}:

# ======================================================================

`gausselim/normal/factor`:= factor:

`gausselim/normal/Factor`:= proc(z)
	factor(z, indets(z, RootOf));
end:

`gausselim/normal/sparsefactor`:= proc(z)
   if type(z, `*`) then
      if hastype(z, `+`) then
         map(`gausselim/normal/sparsefactor`, z);
      else
         factor(z);
      fi;
   elif (frontend(degree,[z,indets(z,name)]) < 10) then
      factor(z);
   else
      normal(z, expanded);
   fi;
end:

`gausselim/normal/normal`:= normal:
`gausselim/normal/Normal`:= proc(z) evala(Normal(z), independent) end:
`gausselim/normal/normalexpanded`:= proc(z) normal(z, expanded) end:

# ======================================================================

`gausselim/normal/assign`:= proc(ar,AR)
	global `gausselim/normal`;
	option `Copyright (c) 1998 by Harald Boeing.`;
	`qsum/options`(ar,simplify,proc(x) type(x,{procedure,name}) end,\
		normal,'`gausselim/normal`',AR);
	if not(type(`gausselim/normal`,procedure)) then
		`gausselim/normal`:=\
			cat('`gausselim/normal/`',`gausselim/normal`);
	fi;
end:

# ======================================================================

`gausselim/arguments`:= proc(arg, EQ, VA, NR_PREF_VA)
	local ar, va, elim0, eq, i;
	global `gausselim/triangulize/pivot`,\
		`gausselim/all_solutions`;
	option `Copyright (c) 1998 by Harald Boeing.`;
	ar:= [seq(arg[i],i=3..nops(arg))];
	eq:= arg[1];
	if type(arg[2], {set,list}) then 
		va:= [op(arg[2])];
	elif type(arg[2], name) then
		va:= [arg[2]];
	fi;
        userinfo(1,gausselim,nops(eq),cat(`equations (`,(length(eq)),`) in`),\
                nops(va), `variables:`,print(va));
	`gausselim/normal/assign`(ar,'ar');
	`qsum/options`(ar,eliminate_first,\
		proc(x) type(x,{set(name),list(name)}) end,[],'elim0','ar');
	if (elim0 <> []) then
		elim0:= select(has,elim0,va);
		va:= [op(elim0), op(select(proc(x,y) not(has(x,y)) end,va,elim0))];
		NR_PREF_VA:= nops(elim0);
		userinfo(3, gausselim, `Eliminate first: `, elim0);
	else
		NR_PREF_VA:= nops(va);
	fi;
	VA:= va;
   if has(ar, `gausselim/normal/procedures/RootOf`) then
      eq:= convert(eq, RootOf);
   fi;
	EQ:= eq;
	# determine pivot strategy
	`qsum/options`(ar,pivot,{notzero,double,riese},riese,'i','ar');
	`gausselim/triangulize/pivot`:= \
		cat('`gausselim/triangulize/pivot/`',i);
	# check if we need all solutions  (if #variables > #equations)
	`qsum/options`(ar,specialsolution,{true,false},false,\
		'`gausselim/all_solutions`','ar');
end:

# ======================================================================

`gausselim`:= proc(eqns, var)
	local zeit, a, va, nr_pref_va, m, n, sol, restore_roots;
	option `Copyright (c) 1998 by Harald Boeing.`;
	zeit:= time();
	`gausselim/arguments`([args], 'a', 'va', 'nr_pref_va');
	`gausselim/equations2matrix`(a, va, 'a', 'm', 'n');
	nr_pref_va:= min(nr_pref_va, n-1);
	`gausselim/triangulize`(a, m, n, va, 'va', nr_pref_va, zeit);
	`gausselim/sort`(a, m, n, 'm');
	userinfo(5, gausselim, `Triangulized matrix:`, print(a));
	if ((nops(eqns)-m) <> 0) then
		userinfo(2, gausselim, (nops(eqns)-m),\
			`equations were reduced to zero.`); 
	fi;
	sol:= `gausselim/matrix2solution`(a, va, m, n-1, zeit);
	if (sol = {}) then
		userinfo(1, gausselim, `WARNING: no solutions found (`,
			time()-zeit,`s CPU-time.)`);
		RETURN(NULL);
	fi;
	userinfo(1, gausselim, `Determined solution of length `,\
		length(sol),` in `,time()-zeit,`s.`);
	RETURN(sol);
end:

# ======================================================================

`gausselim/equations2matrix/abs_coeff`:= proc(p, x)
	local a, i;
	option `Copyright (c) 1998 by Harald Boeing.`;
	a:= p;
	seq(assign('a',coeff(a,i,0)),i=x);
	a;
end:

# ----------------------------------------------------------------------

`gausselim/gcd`:= proc()
	local z, gcdiv, j;
	z:= select(proc(x) not(type(x,0)) end, args[1]);
	if (z = []) then
		1;
	else
		gcdiv:= z[1];
		seq(assign('gcdiv',frontend(gcd,[gcdiv,j])), j=subsop(1=NULL,z));
		gcdiv
	fi;
end:
		
# ----------------------------------------------------------------------

`gausselim/equations2matrix`:= proc(eq, va, A, M, N)
	local a, i, j, m, n;
	global `gausselim/normal`;
	option `Copyright (c) 1998 by Harald Boeing.`;
	userinfo(2, gausselim, `building matrix, using `,\
		`gausselim/normal`, `for simplification.`);
   if type(eq,{set,list}) then a:= [op(eq)] else a:= [eq] fi;
	a:= map(proc(z) if type(z,`=`) then op(1,z)-op(2,z) else z fi end, a);
	a:= collect(a, va, distributed, `gausselim/normal`);
	a:= select(proc(z) if (z=0) then false else true fi end, a);
	m:= nops(a);
	n:= nops(va);
	a:= array(1..m,1..n+1,[seq([seq(coeff(a[i],va[j]),j=1..n),\
		`gausselim/equations2matrix/abs_coeff`(a[i], va)], i=1..m)]);
######################################################################
#zeit:= time();
#print(seq(`gausselim/gcd`(a[i]),i=1..m));
#print(seq(`gausselim/gcd`([seq(a[j][i],j=1..m)]),i=1..n));
#print((time()-zeit)*seconds);
######################################################################
	M:= m;
	N:= n + 1;
	A:= a;
	if ((nops(eq) - m) <> 0) then
		userinfo(2,gausselim,m,` equations left after normalizing.`);
	fi;
	userinfo(5, gausselim, `Matrix:`, print(a));
end:
 
# ======================================================================

`gausselim/triangulize`:= proc(a, m, n, VAR, VA, nr_pref_va, zeit)
	local va, pivot, i, j, k, l, tmp;
	option `Copyright (c) 1998 by Harald Boeing.`;
	va:= VAR;
	userinfo(3, gausselim, `to do      time     eliminated`);
	userinfo(3, gausselim, `------------------------------`);
	for k from 1 to m do
		pivot:= `gausselim/triangulize/pivot`(a,m,n-1,k,nr_pref_va);
		if (pivot = NULL) then 
			next 
		fi;
		`gausselim/triangulize/swap_column+row`(a,m,n,pivot,k,va,'va');
		seq(assign('a[k,l]',`gausselim/normal`(-a[k,l]/a[k,k])),l=k+1..n);
		a[k,k]:= -1;
		seq(seq(assign('a[i,l]',`gausselim/normal`(\
			a[i,l]+a[i,k]*a[k,l])), l=k+1..n), i=k+1..m);
		seq(assign('a[i,k]', 0), i=k+1..m);
		userinfo(2, gausselim, sprintf(`%5d%10.1f%15s`,\
			n-k-1,time()-zeit,convert(va[k],string)), print());
		userinfo(7, gausselim,\ print(a));
	od; # k
	userinfo(3, gausselim, `------------------------------`);
	VA:= va;
	RETURN();
end:
# ----------------------------------------------------------------------

`gausselim/triangulize/pivot/notzero`:= proc(a, m, n, k)
local pivot, j;
	option `Copyright (c) 1998 by Harald Boeing.`;
   pivot:= NULL;
   for j from k to m do
      if (a[j,k] <> 0) then
         RETURN([j, k]);
      fi;
   od;
	NULL;
end:

# ----------------------------------------------------------------------

`gausselim/triangulize/pivot/double`:= proc(a, m, n, k, nr_pref_va)
local pivot, pivotsize, i, j, l, r, s;
	option `Copyright (c) 1998 by Harald Boeing.`;
	pivot:= NULL;
	pivotsize:= 10^10;
	for j from k to m do
		for i from k to nr_pref_va do
			if (a[j,i] = 0) then next fi;
# 1.3 sum -> add
#			l:= sum('sum('length(a[s,r]+a[s,i]*a[j,r]/a[j,i])',\
#				'r'=k..n)', 's'=k+1..m);
			l:= add(add(length(a[s,r]+a[s,i]*a[j,r]/a[j,i]),\
				r=k..n), s=k+1..m);
			if (l < pivotsize) then
				pivot:= [j,i];
				pivotsize:= l;
			fi;
		od;
	od;
	if (nr_pref_va < n) and (pivot = NULL) then
		pivot:= procname(a, m, n, k, n);
	fi;
	RETURN(pivot);
end:

# ----------------------------------------------------------------------

`gausselim/triangulize/pivot/riese`:= proc(a, m, n, k, nr_pref_va)
local pivot, pivotsize, i, j, l, r, s;
	option `Copyright (c) 1998 by Harald Boeing.`;
	pivot:= NULL;
	pivotsize:= [10^10, 10^10, 10^10];
	for j from k to m do
		l[1]:= nops(select(type, [seq(a[j,r],r=k..n)], 0));
		if (l[1] > pivotsize[1]) then next fi;
		l[3]:= convert([seq(length(a[j,i]),i=1..n+1)], `+`);
		for i from k to nr_pref_va do
			if (a[j,i] = 0) then next fi;
			l[2]:= length(a[j,i]);
			if (evalf(pivotsize[3]/l[3]-(l[2]/pivotsize[2])^3/4)>0) then
				pivot:= [j,i];
				pivotsize:= [l[1], l[2], l[3]];
			fi;
		od;
	od;
	if (nr_pref_va < n) and (pivot = NULL) then
		pivot:= procname(a, m, n, k, n);
	fi;
	RETURN(pivot);
end:

# ----------------------------------------------------------------------

`gausselim/triangulize/swap_column+row`:= proc(a,m,n,pivot,k,va,VA)
	local j, i, tmp;
	option `Copyright (c) 1998 by Harald Boeing.`;
	j:= pivot[2];
   if (j <> k) then
		for i from 1 to m do
			tmp:= a[i,k];
			a[i,k]:= a[i,j];
			a[i,j]:= tmp;
		od;
		VA:= subsop(j=va[k], k=va[j], va);
	fi;
	if (pivot[1] <> k) then
		tmp:= [seq(a[k,i],i=1..n)];
		seq(assign('a[k,i]',a[pivot[1],i]),i=1..n);
		seq(assign('a[pivot[1],i]',tmp[i]),i=1..n);
	fi;
   RETURN();
end:
 
# ======================================================================

`gausselim/diagonalize`:= proc(a, m, n, zeit)
local row, col, i, j;
	option `Copyright (c) 1998 by Harald Boeing.`;
   for row from 1 to m-1 do
      for col from 1 to n while (a[row,col] = 0) do od;
      for j from row+1 to m do
         if (a[j,col] <> 0) then
				seq(assign('a[j,i]',a[j,i]+a[j,col]*a[row,i],i=1..n-1));
				a[j,n]:= `gausselim/normal`(a[j,n]+a[j,col]*a[row,n]);
         fi;
      od; # j
		userinfo(3, gausselim, sprintf(`%5d%10.1f`,\
			m-1-row,time()-zeit),print());
   od; # row
   userinfo(3, gausselim,\
		`------------------------------------------`,print());
   RETURN();
end:

# ======================================================================

# a und b sind Listen. Ferner gilt
# `gausselim/sort/<`(a,b) = true      (d.h. a<b),
# falls a mehr Nullen am Listenanfang besitzt als b.

`gausselim/sort/<`:= proc(a, b)
local az, bz;
	option `Copyright (c) 1998 by Harald Boeing.`;
	for az from 1 to nops(a) while (a[az] = 0) do od;
	for bz from 1 to nops(b) while (b[bz] = 0) do od;
	RETURN(evalb(az>bz));
end:

# ----------------------------------------------------------------------

`gausselim/sort`:= proc(A, m, n, M)
local a, j, i, rows;
	option `Copyright (c) 1998 by Harald Boeing.`;
	a:= [seq([seq(A[j,i],i=1..n)],j=1..m)];
	# Sortiere nun so, da"s erste Zeile die einfachste ist.
	a:= sort(a, `gausselim/sort/<`);
	# L"osche nun reine 'Null-Zeilen'...
	for rows from m to 1 by (-1) while (a[1] = [0 $ n]) do
		a:= subsop(1=NULL, a);
	od;
	# Weise A die triangulisierte Matrix zu, M die Anzahl der Reihen.
	M:= rows;
	A:= array(1..rows,1..n,[seq([seq(a[j][i],i=1..n)],j=1..rows)]);
	RETURN();
end:

# ======================================================================

`gausselim/matrix2solution`:= proc(a, va, m, n, zeit)
local i, j, sol;  
	option `Copyright (c) 1998 by Harald Boeing.`;
	if (m = 0) then
		RETURN({seq(j=j,j=va)});
	fi;
	# All rows with zeroe entries have been cancelled.
	# If the first row has n zeroes as first entries, then the
	# last one has to be different from nil, so that there is no
	# solution.
	if ([seq(a[1,i],i=1..n)] = [0$n]) then RETURN({}); fi;
   #`gausselim/diagonalize`(a, m, n+1, zeit);
	# Those Variables are Parameters...
	for j from m+1 to n do
		sol[j]:= va[j];
	od;
	# Do a backsubstitution...
	for j from m to 1 by -1 do
# 1.3 sum -> add
#		sol[j]:= sum('-a[m+1-j,i]*sol[i]', 'i'=j+1..n);
		sol[j]:= add(-a[m+1-j,i]*sol[i], i=j+1..n);
		sol[j]:= `gausselim/normal`((sol[j]-a[m+1-j,n+1]) / a[m+1-j,j]);
		userinfo(4, gausselim, sprintf(`%5d%10.1f`,j-1,time()-zeit));
	od;
	sol:= {seq(va[j]=sol[j], j=1..n)};
	if member(`gausselim/normal`,\
		`gausselim/normal/procedures/RootOf`) then
		sol:= convert(sol, radical);
	fi;
	RETURN(sol);
end:

# ======================================================================
# ======================================================================

`recursion/order/Sargs`:= proc(Sm, n)
	local m;
	option `Copyright (c) 1997 by Harald Boeing.`;
	m:= [op(Sm)];
	m:= select(proc(z,n) has(z,n) and type(subs(n=0,z),constant) end,\
		m, n);
	op(m);
end:

# ----------------------------------------------------------------------

`recursion/order`:= proc(rec, S, n, maxn, minn)
	local ord;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if type(S,name) then
		ord:= indets(rec,specfunc(anything,S));
		ord:= op(map(op, ord));
	else
		ord:= indets(rec,specfunc(anything, op(0,S)));
		ord:= op(map(`recursion/order/Sargs`, ord, op(1,S)));
	fi;
   if (nargs > 2) then 
		if type(S,name) then 
			n:= op(indets([ord],name));
		else
			n:= op(1, S);
		fi;
	fi;
   if (nargs > 3) then maxn:= max(ord); fi;
   if (nargs > 4) then minn:= min(ord); fi;
   RETURN(max(ord)-min(ord));
end:

# ======================================================================

`recursion/coefflist`:= proc(rec, Sn, FUNC)
	local S, cl, n, i, func;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if (nargs > 2) then
		func:= eval(FUNC);
	else
		func:= proc(z) z end;
	fi;
	if type(Sn, name) then
		S:= Sn;
		cl:= [op(indets(rec, specfunc(anything,S)))];
		n:= op(indets(cl, name));
	else
		S:= op(0, Sn);
		cl:= [op(indets(rec, specfunc(anything,S)))];
		n:= op(1, Sn);
	fi;
	cl:= op(subs(n=0, map(op, cl)));
	cl:= min(cl)..max(cl);
	if type(rec, equation) then
		cl:= [seq(coeff(op(1,rec),S(n+i)), i=cl)];
	else
		cl:= [seq(coeff(rec,S(n+i)), i=cl)];
	fi;
	cl:= map(func, cl, args[4..nargs]);
	RETURN(cl);
end:

# ======================================================================

`recursion/degreelist`:= proc(rec, Sn)
	local S, n;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if type(Sn, name) then
		S:= Sn;
		n:= op(indets(map(op, indets(rec, specfunc(anything,S))), name));
	else
		S:= op(0, Sn);
		n:= op(1, Sn);
	fi;
	`recursion/coefflist`(rec,S,proc(z,n) degree(expand(z),n) end, n);
end:

# ======================================================================

# `recursion/denom`(req:algebraic, S:name, rec:name) 
# liefert den Nenner der Rekursion req.
# Wird das optionale dritte argument
# angegeben, so wird dieser Variablen numer(normal(req)) zugewiesen.

`recursion/denom/lcm`:= proc()
	local LCM;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if (nargs = 1) then
		args[1];
	else
		LCM:= args[2]*numer(normal(args[1]/args[2]));
		if (2 < nargs) then
			procname(LCM, args[3..nargs]);
		else
			LCM;
		fi;
	fi;
end:

# ----------------------------------------------------------------------

`recursion/denom`:= proc(req, S, REC)
	local rec, Sl, cl, i, inhom, den;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if type(req, `=`) then
		rec:= op(1,req)-op(2,req);
	else
		rec:= req;
	fi;
	if type(S, function) then
		Sl:= [op(indets(rec, specfunc(anything,op(0,S))))];
	else
		Sl:= [op(indets(rec, specfunc(anything,S)))];
	fi;
	rec:= collect(rec,Sl,factor);
	cl:= [seq(frontend(coeff, [rec,i]), i=Sl)];
	inhom:= subs(seq(i=0,i=Sl), rec);
	if (inhom <> 0) then cl:= [op(cl),inhom]; fi;
	den:= `recursion/denom/lcm`(op(denom(cl)));
	if (nargs = 3) then
		rec:= convert([seq(normal(cl[i]*den*Sl[i]),i=1..nops(Sl))], `+`);
		if type(req, `=`) then
			REC:= (rec = normal(-inhom*den));
		else
			REC:= (rec + normal(inhom*den));
		fi;
	fi;
	RETURN(den);
end:

# ======================================================================

`recursion/collect`:= proc(req, S)
	local Sl;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if type(S, function) then
		RETURN(procname(req, op(0,S)));
	fi;
	Sl:= indets(req, specfunc(anything,S));
	collect(req, Sl, distributed, args[3..nargs]);
end:

# ======================================================================

`recursion/compare/normalform`:= proc(req, Sn, Su, N, RecOrd)
	local rec, S, record, n, maxn, minn;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if type(req, `=`) then
		rec:= op(1,req) - op(2,req);
	else
		rec:= req;
	fi;
	RecOrd:= `recursion/order`(rec, Sn, n, maxn, minn);
	if type(Sn, name) then
		S:= Sn;
	else
		S:= op(0, Sn);
		if (n <> op(1,Sn)) then 
                        ERROR(cat(`Different names as arguments of `,S,` encountered.`));
		fi;
	fi;
	rec:= collect(rec, indets(rec, specfunc(anything,S)));
	if (minn <> n) then
		minn:= n - coeff(minn, n, 0);
		rec:= subs(n=minn, rec);
	fi;
	Su:= S;
	N:= n;
	S:= coeff(rec,S(n+eval(RecOrd)));
	RETURN(map(unapply('normal'(z/S),z),rec));
end:

# ----------------------------------------------------------------------

`recursion/compare/different_order`:= proc(rec1, rec2, S, n, o1, o2)
	local r1, r2, alpha, Svars, eqn, i;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if (o2 < o1) then
		RETURN(`recursion/compare/different_order`\
			(rec2, rec1, S, n, o2, o1));
	fi;
	printf(`Warning: Different recursion orders. (%a,%a)\n`, o1,o2);
	r1:= unapply(rec1, n);
# 1.3 sum -> add
#	r1:= sum('alpha[i]*r1(n+i)', 'i'=0..o2-o1);
	r1:= add(alpha[i]*r1(n+i), i=0..o2-o1);
	r2:= rec2;
	Svars:= indets(r2, specfunc(anything,S));
	eqn:= collect(r1-r2, Svars, `power/expand`@normal@`power/expand`);
	eqn:= {coeffs(eqn, Svars)};
	eqn:= solve(eqn, {seq(alpha[i], i=0..o2-o1)});
	if (eqn <> NULL) then
		RETURN(`Recursions are compatible.`);
	fi;
	RETURN(`Recursions are NOT compatible!`);
end:

# ----------------------------------------------------------------------

`recursion/compare/check_args`:= proc(r1, r2, Sn)
	local S;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if type(Sn, name) then S:= Sn; else S:= op(0, Sn); fi;
	if (nargs <> 3) then 
		ERROR(`Wrong number of arguments.`);
	elif not(type(S,{name, function(name)})) then
		ERROR(`Third argument has to be a name.`);
	elif not(has(r1,S)) then
                ERROR(cat(`First argument is free of `,S));
	elif not(has(r2,S)) then
                ERROR(cat(`Second argument is free of `,S));
	fi;
	if not(type(r1,`=`)) then
		printf(`Warning: Rhs of first recursion is assumed to be 0.\n`);
	fi;
	if not(type(r2,`=`)) then
		printf(`Warning: Rhs of second recursion is assumed to be 0.\n`);
	fi;
end:
		
# ----------------------------------------------------------------------

`recursion/compare`:= proc(rec1, rec2, Sn)
	local r1, r2, n1, n2, ord1, ord2, Svars, eqn;
	option `Copyright (c) 1997 by Harald Boeing.`;
	`recursion/compare/check_args`(args);
   r1:= `recursion/compare/normalform`(rec1, Sn, S, n1, ord1);
   r2:= `recursion/compare/normalform`(rec2, Sn, S, n2, ord2);
   if (n1 <> n2) then
      RETURN(cat(`Arguments of `,S,` differ: (`,n1,`,`,n2,`).`));
   fi;
   if (ord1 <> ord2) then
		RETURN(`recursion/compare/different_order`\
			(r1, r2, S, n1, ord1, ord2));
   fi;
	Svars:= indets(r1, specfunc(anything,S));
	eqn:= coeff(r2,S(n1+ord1))*r1 - coeff(r1,S(n1+ord1))*r2;
	eqn:= collect(eqn, Svars,expand@`power/expand`@normal@`power/expand`);
   if (eqn = 0) then
      RETURN(`Recursions are identical.`);
   fi;
   RETURN(`Recursions are NOT identical!`);
end:

# ======================================================================


`recursion/primpart/gcd`:= proc(p,q)
	if (2 < nargs) then
		procname(procname(p,q),args[3..nargs]);
	else
		frontend(gcd, [p,q]);
	fi;
end:

# ----------------------------------------------------------------------

`recursion/primpart`:= proc(req, S, CONT)
	local rec, S_inds, S_coeffs, i, cont;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if type(S, function) then
		RETURN(procname(req, op(0,S), args[3..nargs]));
	elif type(req, `=`) then
		rec:= procname(op(1,req), S, 'cont');
		if (nargs = 3) then CONT:= cont; fi;
		RETURN(rec = op(2,req) / cont);
	fi;
   rec:= `power/expand`(req,root=eliminate,unsubs=unsub);
   S_inds:= [op(indets(rec, specfunc(anything,S)))];
   S_coeffs:= [seq(coeff(rec, i), i=S_inds)];
   cont:= `recursion/primpart/gcd`(op(S_coeffs));
	if (nargs = 3) then CONT:= cont; fi;
   S_coeffs:= seq(normal(i/cont), i=S_coeffs);
# 1.3 sum -> add
#   rec:= sum('factor(S_coeffs[i]*S_inds[i])', 'i'=1..nops(S_inds));
   rec:= add(factor(S_coeffs[i]*S_inds[i]), i=1..nops(S_inds));
	rec:= combine(subs(unsub,rec),power);
   RETURN(rec);
end:

# ======================================================================

`recursion/arguments`:= proc(rec, S)
	[op(map(op, indets(rec, specfunc(anything, S))))];
end:
			  
# ----------------------------------------------------------------------

`recursion/sort/collect`:= proc(p, n)
	if type(p, {list,`*`,`^`,`=`}) then 
		map(procname, p, n);
	else
		sort(collect(p, n), n);
	fi;
end:

# ----------------------------------------------------------------------

`recursion/sort/recargs`:= proc(a,b)
	option `Copyright (c) 1997 by Harald Boeing.`;
	if (type(a,function) and type(b,function)) then
		procname(op(1,a),op(1,b));
	elif type(b-a, negint) then
		true
	elif type(b-a, posint) then
		false
	elif type(b/a, name) then
		true
	elif type(a/b, name) then
		false
	else
		type(length(b)-length(a), posint)
	fi;
end:

# ----------------------------------------------------------------------

`recursion/sort`:= proc(req, Su)
	local rec, S, n, i, coeff_list, recargs, lcoeff_sign;
	option `Copyright (c) 1997 by Harald Boeing.`;
	if not type([args], [equation,name]) then
		ERROR(`Wrong type of arguments.`);
	fi;
	rec:= lhs(req);
	if (nargs = 1) or not(has(req,Su)) then 
		S:= {seq(op(0,i), i=indets(req,function(linear)))};
		if (nops(S) <> 1) then RETURN(req) else S:= op(S) fi;
	 else
		S:= Su;
	fi;
   coeff_list:= [op(indets(rec, specfunc(anything,S)))];
	recargs:= `recursion/arguments`(coeff_list, S);
	n:= sort([op(indets(recargs, name))])[1];
	recargs:= sort(recargs, `recursion/sort/recargs`);
	coeff_list:= [seq(S(i), i=recargs)];
	rec:= collect(rec, coeff_list, distributed);
	lcoeff_sign:= collect(coeff(rec, S(recargs[1])), n);
	lcoeff_sign:= traperror(sign(lcoeff(lcoeff_sign, n)));
	if (lcoeff_sign = lasterror) then lcoeff_sign:= 1; fi;
	rec:= [op(rec)];
	rec:= [seq(factor(lcoeff_sign*i), i=rec)];
	rec:= convert(`recursion/sort/collect`(rec, n), `+`);
	RETURN(sort(rec,coeff_list)=lcoeff_sign*rhs(req));
end:

# ======================================================================

`recursion/sortq`:= proc(req, S, N, sub)
	local rec, i, S_list;
	option `Copyright (c) 1997 by Harald Boeing.`;
   rec:= lhs(req);
   S_list:= [op(indets(rec, specfunc(anything,S)))];
	S_list:= sort(S_list, `recursion/sort/recargs`);
   rec:= collect(rec, S_list, distributed,factor);
# bug with nested substitutions (Torsten Sprenger)
#	rec:= map(`power/combine`, subs(sub,rec));
   if type(sub,list) then
	   rec:= map(`power/combine`, subs(op(sub),rec));
	else
	   rec:= map(`power/combine`, subs(sub,rec));	
   fi;
	sort(rec,S_list) = `power/combine`(subs(sub,rhs(req)));
end:
 
# ======================================================================

`recursion/inhomo2homo`:= proc(recursion, S)
   local arglist, n, rec, RHS, subst;
	option `Copyright (c) 1997 by Harald Boeing.`;
	arglist:= op(map(op, indets(recursion, specfunc(linear, S))));
   n:= op(indets(arglist,name));
   RHS:= rhs(recursion);
   if (min(arglist) <> n) then
      subst:= (n=n-1);
      rec:= simpcomb(subs(n=n-1,RHS) / RHS);
   else
      subst:= (n=n+1);
      rec:= simpcomb(subs(n=n+1,RHS) / RHS);
   fi;
   if not type(rec, ratpoly(anything, n)) then
      printf(`WARNING: Unable to make the recurrence homogeneous:\n`);
      printf(`         Righthandside of recursion is not `);
      printf(`hypergeometric`);
		printf(` wrt. %a.\n`, n);
      RETURN(recursion);
   fi;
   rec:= subs(subst,lhs(recursion)) * denom(rec) -\
         lhs(recursion) * numer(rec);
   rec:= `recursion/sort`(rec=0, S);
   RETURN(rec);
end:

# ----------------------------------------------------------------------
# The next line is necessary for making of qsum package
# otherwise this procedure could not be recognized...

qsum[`recursion/qinhomo2qhomo`]:=
	subs(simpcomb=qsimpcomb, `hypergeometric`=`q-hypergeometric`,\
		eval(`recursion/inhomo2homo`)):

# ======================================================================

init:= proc()
	global _qsum_solvemethod, _qsum_gausselim_normal,\
		_qgosper_antidifference, _qgosper_simplify, _qgosper_series,\
		_qsumrecursion_simple_recursion,\
		_qsumrecursion_recursion, _qsumrecursion_recorder,\
		_qsumrecursion_rhs, _qsumrecursion_inhomo2homo,\
		_qsumrecursion_sigmafirst,\
		_qsumrecursion_certificate, _qsumrecursion_rec2qhyper,\
		_qsumdiffeq_evalqdiff, _qsum_profile,\
		_qrecsolve_split, _qrecsolve_return, _qrecsolve_solution,\
		_qsum_specialsolution, _timing;
	option `Copyright (c) 1997-2001 by Harald Boeing & Wolfram Koepf.`;
	# print(`Copyright 1998,  Harald Boeing & Wolfram Koepf`);
	# print(`Konrad-Zuse-Zentrum Berlin`);
   # print(`Package "q-Hypergeometric Summation", Maple V-13`):
   # print(`Copyright 1998-2010, Harald Boeing & Wolfram Koepf, University of Kassel`):

	# ======================================================================

	# Standardeinstellungen...

	_qsum_solvemethod:= auto:          # Gls. l"osen mittels...
	_qsum_gausselim_normal:= 'normal': # Normalisierung die gausselim nimmt.
	_qsum_specialsolution:= false:     #
	_qsum_profile:= false:             # Ausgabe von Zeiten

	_qgosper_antidifference:= up:      # Gosper abw"arts?
	                                   # Simplification after qgosper:
	_qgosper_simplify:= proc(x) combine(factor(x),power) end:
	_qgosper_series:= false:           # Determine antidifference via qrecsolve

	_qsumrecursion_recursion:= down:   # Rekursion abw"arts?
	_qsumrecursion_recorder:= 1..5:    # Ordnung der zu berechnenden Rekursion
	_qsumrecursion_simple_recursion:= false:  # Konstante Faktoren von sigma...
	_qsumrecursion_sigmafirst:= true:
	_qsumrecursion_rhs:= explicit:     # Berechnung der rechten Seite auch 
												  # bei Summation "uber -infty..infty?
	_qsumrecursion_inhomo2homo:= false:# Rekursion 'homogenisieren'?
	_qsumrecursion_certificate:= up:   # Ausgabe der Rekursion+Zertifikat?

	_qsumrecursion_rec2qhyper:= false: # Rekursion 1. Ordnung -> closedform

	# ----------------------------------------------------------------------

	_qsumdiffeq_evalqdiff:= false:

	# ----------------------------------------------------------------------

	_qrecsolve_split:= true:          # factor coefficients completely?
	_qrecsolve_return:= uprec:        # What shall qrecsolve return?
	_qrecsolve_solution:= qhypergeometric: # Look for power series solutions?

	# ----------------------------------------------------------------------

	_timing[noclear]:= false:
	RETURN(NULL);
end:

init();
# ----------------------------------------------------------------------

`qsum/setting`:= proc()
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
   printf(`Current settings are:\n`);
	printf(`  qgosper       -- antidifference:  `);
	lprint(_qgosper_antidifference);
   printf(`  qsumrecursion -- recursion:       `);
	lprint(_qsumrecursion_recursion);
   printf(`                   recursion order: `);
	lprint(_qsumrecursion_recorder);
	printf(`                   determine rhs:   `);
	lprint(_qsumrecursion_rhs);
	printf(`                   certificate:     `);
	lprint(_qsumrecursion_certificate);
   printf(`  qsum          -- solve:           %s`,_qsum_solvemethod);
	if (_qsum_solvemethod = gausselim) then
		printf(` (%s)\n`, _qsum_gausselim_normal);
	else
		lprint();
	fi;
end:

# ----------------------------------------------------------------------

#`qsum/setting`();

# ======================================================================
# ----------------------------------------------------------------------

`qgosper/findf/ABPsolve`:= proc(P,Q,R,q,K,deg,sigma,sigmasol)
	local RHS, i, j, lo, QR, d, a, sol, sigvar;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if (nargs = 6) then
		RHS:= [collect(P,K,factor)];
	else
		sigvar:= select(proc(z) not(type(z,constant)) end, sigma);
		a:= P;
		seq(assign('a',coeff(a,j,0)),j=sigvar);
		RHS:= [a,seq(collect(coeff(P,j),K,factor),j=sigvar)];
	fi;
	# Now turn recurrence for laurent polynomial X(K),
	# P(K)=Q(K)*X(K)-R(K/q)*X(K/q), into an upward recurrence for
	# the POLYNOMIAL XX(K) = X(K) * K^lo (see line below)
	lo:= -op(1,deg);
	RHS:= [seq(collect(subs(K=K*q,j)*K^lo*q^lo,K,factor), j=RHS)];
	QR:= collect(subs(K=K*q,Q),K), collect(-R*q^lo,K);
	d:= max(degree(QR[1],K),degree(QR[2],K));
	for j from 0 to d do
		a[j]:= unapply(factor(coeff(QR[2],K,j)+coeff(QR[1],K,j)*K), K);
	od;
	RHS:= [seq(unapply('coeff'(j,K,'i'),'i'), j=RHS)];
	sol:= `qsum/ABPsolve`(copy(a),eval(RHS),op(2,deg)+lo,0,d,q,K,lo,'j');
	if (6 < nargs) then
		if (sol = NULL) then
			sigmasol:= NULL;
		else
			sigmasol:= {seq(sigvar[i]=j[i],i=1..nops(sigvar))};
		fi;
	fi;
	RETURN(sol);
end:

# ----------------------------------------------------------------------

`qsum/ABPsolve/singularities`:= proc(p,q,MAX)
	local K, z, j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if type(p(K),`*`) then
		z:= {seq(solve(j,K),j=p(K))};
	else
		z:= {solve(p(K),K)};
	fi;
	z:= combine(z,power);
	z:= select(type,z,{identical(q)^posint,identical(q),identical(1)});
	z:= map(proc(y) if (y=1) then 0 elif (y=q) then 1 else op(2,y) fi end,z);
	z:= select(unapply('evalb'(0<='j' and 'j'<=MAX),'j'), z);
end:

# ----------------------------------------------------------------------

`qsum/ABPsolve`:= proc(a,f,N,A,B,q,K,lo,sigmasol)
	local i,j,n,Sin,t,Vec,g,nf,Ind,Eqn,c,eqn,lambda,zeit;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	Sin:= {seq(j,j=0..A-1)} union\
		`qsum/ABPsolve/singularities`(a[-A],q,N+A+B);
	userinfo(3, ABPsolve, `Singularities:`, print(Sin minus {seq(j,j=0..A-1)}));
	t:= max(1,nops(Sin));
	Vec:= array(1..nops(Sin),-A-B..N+A+B, [seq([seq(0,j=-A-B..-1),\
		seq(1,j=0..N+A+B)],i=1..nops(Sin))]);
	nf:= nops(f);
	g:= array(1..nf,-A-B..N+A+B,[seq([seq(0,j=-A-B..N+A+B)],i=1..nf)]);
	Ind:= NULL;
	Eqn:= NULL;
	t:= 0;
	lambda[1]:= 1;
	for n from 0 to N+A+B do
		userinfo(3,ABPsolve,print(N+A+B+1-n));
		if member(n,Sin) then
			seq(assign('Vec[t+1,j]',0),j=0..N+A+B);
			Vec[t+1,n]:= 1;
			seq(assign('Vec[j,n]',0),j=1..t);
			Ind:= Ind, c[n];
			if (n >= A) then
				eqn:= seq(a[-i](q^n/q^A*q^i)*(
						convert([seq(Ind[j]*Vec[j,n-a+i],j=1..t-1)],`+`)+\
						convert([seq(lambda[j]*g[j,n-A+i],j=1..nf)],`+`)),\
					i=-B..A);
				eqn:= convert([eqn, seq(-lambda[j]*f[j](n-A),j=1..nf)], `+`);
				Eqn:= Eqn, collect(eqn,[seq(c[j],j=0..t-1)],distributed,normal);
			fi;
			t:= t+1;
		else
# 1.3 sum -> add
#			seq(assign('Vec[j,n]', normal(-sum('a[i-A](q^n/q^i)*\
#				Vec[j,n-i]', 'i'=1..A+B)/a[-A](q^n))), j=1..t);
#			seq(assign('g[j,n]', normal((f[j](n-A)-sum('a[i-A](q^n/q^i)*\
#				g[j,n-i]','i'=1..A+B))/a[-A](q^n))), j=1..nf);
			seq(assign('Vec[j,n]', normal(-add(a[i-A](q^n/q^i)*\
				Vec[j,n-i], i=1..A+B)/a[-A](q^n))), j=1..t);
			seq(assign('g[j,n]', normal((f[j](n-A)-add(a[i-A](q^n/q^i)*\
				g[j,n-i],i=1..A+B))/a[-A](q^n))), j=1..nf);
		fi;
		seq(Vec[j,n],j=1..t);
		seq(g[j,n],j=1..nf);
	od;
#'g'=eval(g);
#'Vec'=eval(Vec);
	Ind:= [Ind];
# 1.3 sum -> add
#	Eqn:= {Eqn, seq(sum('Ind[j]*Vec[j,n]','j'=1..t) +\
#		sum('lambda[j]*g[j,n]','j'=1..nf), n=N+1..N+A+B)} minus {0};
	Eqn:= {Eqn, seq(add(Ind[j]*Vec[j,n],j=1..t) +\
		add(lambda[j]*g[j,n],j=1..nf), n=N+1..N+A+B)} minus {0};
	if (Eqn <> {}) then
		eqn:= gausselim(Eqn,\
			{op(Ind),seq(lambda[j],j=1..nf)} minus {1}, _qsum_gausselim_normal);
		if (eqn = NULL) or (eqn = {}) then RETURN(NULL); fi;
	else
		eqn:= {};
	fi;
	Ind:= subs(eqn, Ind);
	Eqn:= subs(eqn, [seq(lambda[j],j=1..nf)]);
	if (nargs = 9) then sigmasol:= subsop(1=NULL,Eqn); fi;
# 1.3 sum -> add
#	eqn:= sum('sum('Eqn[j]*g[j,n+lo]*K^n','n'=-lo..N+A+B-lo)','j'=1..nf)+\
#		sum('sum('Ind[j]*Vec[j,n+lo]*K^n','n'=-lo..N+A+B-lo)','j'=1..t);
	eqn:= add(add(Eqn[j]*g[j,n+lo]*K^n,n=-lo..N+A+B-lo),j=1..nf)+\
		add(add(Ind[j]*Vec[j,n+lo]*K^n,n=-lo..N+A+B-lo),j=1..t);
	if (eqn = 0) then RETURN(NULL) fi;
	eqn:= `qsum/var->_Ci`(eqn, [op(Ind),seq(lambda[j],j=1..nf)]);
	RETURN(eqn);
end:

# ----------------------------------------------------------------------
#_qsum_local_specialsolution:= false:
#a[0]:= K -> (1-q^10)*(K-1)*(K-q^10);
#a[1]:= K -> (q^10-q)*(K-q)*(K-q^10);
#f:= j -> coeff((q^21-q^20-q^12+q^10+q^2-q)*K, K, j);
#trace(`qsum/ABPsolve`):
#`qsum/ABPsolve`(copy(a),[eval(f)],10,0,1,q,K,0,'sigmasol');
# ======================================================================

`qgosper/solve`:= proc(eq, va, pref_va)
	local sol;
	global _qsum_local_solvemethod;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	# Maple 9
	# if (_qsum_local_solvemethod = solve) then
	if true then
		sol:= {op(eq)};
		sol:= solve(sol, {op(va)});
	elif (nargs = 3) and (_qsumrecursion_local_sigmafirst) then
		sol:= gausselim(eq, va, eliminate_first=pref_va,\
			simplify=_qsum_local_gausselim_normal);
#sol:= {op(va)} minus {op(subsop(nops(pref_va)=NULL,pref_va))};
#print(pref_va, nops(pref_va), `Preferred variables`=sol);
#		sol:= gausselim(eq, va, eliminate_first=sol,\
#			simplify=_qsum_local_gausselim_normal);
	else
		sol:= gausselim(eq, va, simplify=_qsum_local_gausselim_normal);
	fi;
	RETURN(sol);
end:

# ======================================================================

qpsihyperterm:= proc(num_list, den_list, q, z, n)
local r, s, num, den, j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	r:= nops(num_list);
	s:= nops(den_list);
	num:= convert([seq(qpochhammer(j,q,n), j=num_list)], `*`);
	num:= num * (-1)^((s-r)*n) * q^((s-r)*n*(n-1)/2) * z^n;
	den:= convert([seq(qpochhammer(j,q,n), j=den_list)], `*`);
	RETURN(normal(num/(den * qpochhammer(q,q,n))));
end:

# ----------------------------------------------------------------------

qhyperterm:= qphihyperterm:

qphihyperterm:= proc(num_list, den_list, q, z, n)
local r, s, num, den, j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	r:= nops(num_list);
	s:= nops(den_list);
	num:= convert([seq(qpochhammer(j,q,n), j=num_list)], `*`);
	num:= num * z^n * ((-1)^n*q^(n*(n-1)/2))^(1+s-r);
	den:= convert([seq(qpochhammer(j,q,n), j=den_list)], `*`);
	RETURN(normal(num/(den * qpochhammer(q,q,n))));
end:

# ======================================================================

`type/homogeneous`:= proc(f, z)
	type(f, monomial(anything,z)) or 
	type(combine(subs(z=1/z,f),power), monomial(anything,z));
end:
	 
# ----------------------------------------------------------------------

`qgosper/primedispersion`:= proc(f, g, q, K)
# calculates the dispersion of nonfactorizable polynomials 
# f and g
local a, b, c, d, j, m, n;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	n:= degree(f,K);
	if (n = 0) or (n <> degree(g,K)) then
		RETURN(NULL);
	fi;
	m:= ldegree(f,K);
	if (m = n) or (m <> ldegree(g,K)) then
		RETURN(NULL);
	fi;
	a:= coeff(f,K,n);
	b:= coeff(f,K,m);
	c:= coeff(g,K,n);
	d:= coeff(g,K,m);
	j:= combine(factor(a*d/b/c),power);
	if not type(j, homogeneous(q)) then
		RETURN(NULL);
	fi;
	j:= degree(j,q)/(n-m);
	if not type(j,nonnegint) then
		RETURN(NULL);
	fi;
	m:= expand(subs(K=K*q^j,g));
	c:=coeff(m,K,n);
	if (expand(c*f-a*m) = 0) then
		RETURN(j);
	fi;
	RETURN(NULL);
end:

# ----------------------------------------------------------------------

`qgosper/dispersionset/simple_factorlist`:= proc(poly, x)
local p;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if type(poly,`*`) then p:= [op(poly)] else p:= [poly] fi;
	p:= map(proc(f) if type(f,`^`) then op(1,f) else f fi end, p);
	RETURN(select(has, p, x));
end:

# ----------------------------------------------------------------------

# dispersionset assumes FACTORED polynomials as input!
`qgosper/dispersionset`:=proc(Qp, Rp, q, K)
local Q, R, f, g, disp;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	Q:= `qgosper/dispersionset/simple_factorlist`(Qp, K);
	R:= `qgosper/dispersionset/simple_factorlist`(Rp, K);
	disp:= {seq(seq(`qgosper/primedispersion`(f,g,q,K), g=R), f=Q)};
	userinfo(2, qsum, `Dispersionset(Q, R, K) = `, disp);
	RETURN(disp);
end:

# ======================================================================

`qgosper/update`:= proc(P, Q, R, q, K)
local Pnew, Qnew, Rnew, disp, g, i, j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	Pnew:= 1;
	Qnew:= Q;
	Rnew:= R;
	disp:= `qgosper/dispersionset`(Qnew, Rnew, q, K);
	for j in disp do
		g:= frontend(gcd, [Qnew, subs(K=K*q^j, Rnew)]);
		if has(g,K) then
			Qnew:= normal(Qnew / g);
			g:= unapply(subs(K=K/q^'j',g), 'j');
			Rnew:= normal(Rnew / g(j));
			Pnew:= normal(Pnew * convert([seq(g(i), i=1..j)],`*`));
		fi;
	od;
	# remove any superfluous constant factors from p
	Pnew:= select(has, P*Pnew*'i*j', K);
#	Pnew:= P * factor(Pnew / frontend(gcd,[Pnew,subs(K=K*q,Pnew)]));
	userinfo(2, qsum, `q-Gosper representation [P, Q, R]:`,\
		print([Pnew, Qnew, Rnew]));
	RETURN([Pnew, Qnew, Rnew]);
end:

# ======================================================================

`qgosper/degreebound/q_exponent`:= proc(ff, q)
local f;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	f:= combine(factor(ff), power);
	if type(f, identical(q)^anything) then
		op(2,f);
	elif (f = q) then
		1;
	elif (f = 1) then
		0;
	else 
		FAIL;
	fi;
end:

# ----------------------------------------------------------------------

`qgosper/degreebound`:= proc(P, Q, R, q, K)
local degP, degQ, degR, ldegf, d, e, led, degf;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	# untere Gradschranke:
	degP:= [ldegree(P,K), degree(P,K)];
	degQ:= [ldegree(Q,K), degree(Q,K)];
	degR:= [ldegree(R,K), degree(R,K)];
	if (degQ[1] <> degR[1]) then
		ldegf:= degP[1] - min(degQ[1], degR[1]);
		userinfo(5, qsum, `Only possible lower degree of polynomial X:`,\
			print(ldegf));
	else
		d:= coeff(Q,K,degQ[1]);
		e:= coeff(R,K,degR[1]);
		led:= `qgosper/degreebound/q_exponent`(e/d, q);
		if (led <> FAIL) then
			userinfo(5, qsum, `Possible lower degrees of polynomial X:`,\
				print({led-degQ[1],degP[1]-degQ[1]}));
		else
			userinfo(5, qsum, `Only possible lower degree of polynomial X:`,\
				print(degP[1]-degQ[1]));
		fi;
		if type(led, integer) then
			ldegf:= min(led, degP[1])-degQ[1];
		else
			ldegf:= degP[1] - degQ[1];
		fi;
	fi;
	# oberere Gradschranke:
	if (degQ[2]  <> degR[2]) then
		degf:= degP[2] - max(degQ[2], degR[2]);
		userinfo(5, qsum, `Only possible degree of polynomial X:`,\
			print(degf));
	else
		d:= coeff(Q,K,degQ[2]);
		e:= coeff(R,K,degQ[2]);
		led:= `qgosper/degreebound/q_exponent`(e/d, q);
		if (led <> FAIL) then
			userinfo(5, qsum, `Possible degrees of polynomial X:`,\
				print({led-degQ[2],degP[2]-degQ[2]}));
		else
			userinfo(5, qsum, `Only possible degree of polynomial X:`,\
				print(degP[2]-degQ[2]));
		fi;
		if type(led, integer) then
			degf:= max(led, degP[2]) - degQ[2];
		else
			degf:= degP[2] - degQ[2];
		fi;
	fi; 
	if (degf < ldegf) then
		RETURN(NULL);
	fi;
	RETURN(ldegf..degf);
end:

# ----------------------------------------------------------------------

`qgosper/findf`:= proc(P, Q, R, q, K, sigma_var, sigma_sol)
local PQR, deg, vars, F, a, j, eqn, sol, sigvar;
global _qsum_local_solvemethod;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
#PQR:= collect(P,K,expand), collect(Q,K,expand), collect(R,K,expand);
PQR:= expand(P,K), expand(Q,K), expand(R,K);
deg:= `qgosper/degreebound`(PQR, q, K);
#	deg:= `qgosper/degreebound`(P,Q,R,q,K,args[6..nargs]);
	if (deg = NULL) then 
		if (nargs = 7) then sigma_sol:= NULL; fi;
		RETURN(NULL) 
	fi;
	j:= op(2,deg)-op(1,deg);
	j:= ((nargs=5) and (j>3)) or ((nargs=7) and (j>nops(sigma_var)+10));
	if (_qsum_local_solvemethod = ABP) or\
			((_qsum_local_solvemethod = auto) and j) then
		sol:= `qgosper/findf/ABPsolve`(P,Q,R,q,K,deg,args[6..nargs]);
		RETURN(sol);
	fi;
	F:= convert([seq(a[j]*K^j, j=deg)], `+`);
	eqn:= expand(PQR[2]*F-subs(K=K/q,PQR[3]*F)-PQR[1]);
#	eqn:= collect(Q*F-subs(K=K/q,R*F)-P,K,expand);
	if (nargs = 5) then 
		vars:= [seq(a[j],j=deg)];
		sol:= `qgosper/solve`({coeffs(eqn,K)}, vars); 
	else
		sigvar:= select(proc(z) not(type(z,constant)) end, sigma_var);
		vars:= [op(sigvar), seq(a[j],j=deg)]; 
		sol:= `qgosper/solve`({coeffs(eqn,K)}, vars, sigvar); 
	fi;
	if (sol = NULL) or (subs(sol,{op(vars)}) = {0}) then
		if (nargs = 7) then sigma_sol:= NULL; fi;
		RETURN(NULL);
	fi;
	# Now grab all those variables which can be arbitrary constants..
	eqn:= map(proc(z) if type(op(2,z),identical(op(1,z))) then\
		op(1,z) else NULL fi end, sol);
	if (_qsum_local_specialsolution) then
		eqn:= {seq(eqn[j]=0,j=1..nops(eqn))};
	else
		eqn:= {seq(eqn[j]=cat(`_C`,j),j=1..nops(eqn))};
	fi;
	sol:= {seq(op(1,j)=subs(eqn,op(2,j)), j=sol)};
	F:= subs(sol, F);
	if (nargs = 7) then 
		sigma_sol:= select(has, sol, sigvar);
	fi;
	userinfo(3, qsum, print('X'(K)=F));
	RETURN(F);
end:

# ======================================================================
# ----------------------------------------------------------------------
# Prozedur hasoption aus Maple V Release 4,
# unbedeutsam abgeaendert fuer Maple V Release 3.
# ----------------------------------------------------------------------

`qsum/hasoption`:= proc(opts, xname, value, remopts)
local xopts;
option `Copyright (c) 1995 Wissenschaftliches Rechnen, ETH Zurich. All \
rights reserved.`;
	xopts:= select(type, opts, identical(xname)=anything);
	if nops(xopts) = 0 then 
		RETURN(false)
	fi;
	xopts:= map(rhs, xopts);
	if (2 < nargs) then
		value:= op(xopts);
	fi;
	if (3 < nargs) then
		remopts:= select(proc(z,t) not(type(z,t)) end, opts,\
			identical(xname)=anything);
	fi;
	true
end:

# ----------------------------------------------------------------------

`qsum/options`:= proc(arg,opt,optvalues,globvar,\
	locvar,ARG)
	local oval;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if `qsum/hasoption`(arg,opt,'oval',ARG) then
		if (type(optvalues,{list,set}) and member(oval,optvalues)) or\
			(type(optvalues,procedure) and optvalues(oval)) then
			locvar:= oval;
		else
			`qsumrecursion/bad_option`(opt,oval);
		fi;
	else
		if (nargs=6) then oval:= optvalues; else oval:= args[7]; fi;
		if (type(optvalues,{list,set}) and member(eval(globvar,2),oval))\
			or (type(optvalues,procedure) and optvalues(eval(globvar)))\
		then
			locvar:= eval(globvar);
		elif type(optvalues,procedure) then
			ERROR(cat(`The procedure\n`,eval(optvalues),
				`\napplied to `,globvar,` should yield true.`));
		else
			ERROR(cat(globvar,` should be a member of `,\
				convert(oval,string),`.`));
		fi;
	fi;
	NULL;
end:

# ======================================================================

`type/qsumrecursion/standard`:= proc(arg)
	local i;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	(evalb(nops(arg)>3) and type([seq(arg[i],i=1..4)],\
		[algebraic,name,{name,name=range},{name,function(name)}]))\
	and
	type([seq(arg[i],i=5..nops(arg))], {[],list(name=anything)});
end:

# ----------------------------------------------------------------------

`type/qsumrecursion/phi`:= proc(arg)
	local i;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	((evalb(nops(arg)>4) and type([seq(arg[i],i=1..5)],\
		[list(algebraic),list(algebraic),{name,name^integer},\
		algebraic,{name,function(name)}])) and
		type([seq(arg[i],i=6..nops(arg))], {[],list(name=anything)}))\
	or\
	((evalb(nops(arg)>5) and type([seq(arg[i],i=1..6)],\
		[algebraic, list(algebraic), list(algebraic),\
		{name,name^integer}, algebraic, {name,function(name)}])) and
		type([seq(arg[i],i=7..nops(arg))], {[],list(name=anything)}));
end:

# ======================================================================

`qsumrecursion/arguments/standard`:= proc(arg,F,q,k,sumrange,S,n,ARG)
	local i;
	global _qsumrecursion_local_explicit_range;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	F:= arg[1];
	q:= arg[2];
	if type(arg[3], `=`) then
		k:= op(1, arg[3]);
		sumrange:= op(2, arg[3]);
		_qsumrecursion_local_explicit_range:= true;
	else
		k:= arg[3];
		sumrange:= -infinity..infinity;
	fi;
	if type(arg[4], name) then
		n:= arg[4];
	else
		S:= op(0, arg[4]);
		n:= op(1, arg[4]);
	fi;
	ARG:= [seq(arg[i],i=5..nops(arg))];
end:

# ----------------------------------------------------------------------

`qsumrecursion/arguments/phi`:= proc(arg,F,q,k,sumrange,S,n,ARG)
	local i;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	# check if we have the prefactor form...
	if type(arg[1], algebraic) then 
		procname(subsop(1=NULL,arg),args[2..nargs]);
		F:= arg[1]*eval(F);
		RETURN();
	fi;
	# For compatibility V3 and V4 don't use arg[1..4];
	F:= qphihyperterm(seq(arg[i],i=1..4), k);
	if type(arg[3], name) then
		q:= arg[3];
	else
		q:= op(1, arg[3]);
	fi;
	sumrange:= 0..infinity;
	if type(arg[5], name) then
		n:= arg[5];
	else
		S:= op(0, arg[5]);
		n:= op(1, arg[5]);
	fi;
	ARG:= [seq(arg[i],i=6..nops(arg))];
end:

# ----------------------------------------------------------------------

`qsumrecursion/arguments/recorder`:= proc(hasopt, ord, lo, hi)
	if hasopt and type(ord, posint) then
		lo:= ord;
		hi:= ord;
	elif hasopt and type(ord, range(posint)) then
		if (op(2,ord) < op(1,ord)) then
			`qsumrecursion/bad_option`(`recorder`,opt);
		fi;
		lo:= op(1, ord);
		hi:= op(2, ord);
	elif hasopt then
		`qsumrecursion/bad_option`('recorder',opt);
	elif type(_qsumrecursion_recorder, range(posint)) then
		lo:= op(1,_qsumrecursion_recorder);
		hi:= op(2,_qsumrecursion_recorder);
	elif type(_qsumrecursion_recorder, posint) then
		lo:= _qsumrecursion_recorder;
		hi:= _qsumrecursion_recorder;
	else
		ERROR(`_qsumrecursion_recorder must of type posint or range(posint).`);
	fi;
end:

# ----------------------------------------------------------------------

`qsumrecursion/arguments/rhs`:= proc(hasopt,opt)
	global _qsumrecursion_rhs, _qsumrecursion_local_rhs;
	if hasopt then
		if (opt = true) then
			_qsumrecursion_local_rhs:= always;
		elif (opt = false) then
			_qsumrecursion_local_rhs:= never;
		else
			`qsumrecursion/bad_option`('rhs',opt);
		fi;
	elif member(_qsumrecursion_rhs, {'always','explicit','never'}) then
		_qsumrecursion_local_rhs:= _qsumrecursion_rhs;
	else
		ERROR(`_qsumrecursion_rhs must be set to always, explicit or never.`);
	fi;
end:

# ----------------------------------------------------------------------

`qsumrecursion/bad_option`:= proc(optname, optvalue)
	print(optname=optvalue);
        ERROR(cat(`Invalid value for option `,optname,`.`));
end:

# ----------------------------------------------------------------------

`qsumrecursion/arguments`:= proc(ARGS,F,q,k,SUMRANGE,S,n,lo,hi,info)
local hasopt, opt, arg, i, srange;
global _qsumrecursion_local_certificate,_qsumrecursion_local_explicit_range;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	arg:= ARGS;
	_qsumrecursion_local_explicit_range:= false;
	if type(arg, `qsumrecursion/standard`) then
		`qsumrecursion/arguments/standard`(arg,F,q,k,srange,S,n,'arg');
	elif type(arg, `qsumrecursion/phi`) then
		`qsumrecursion/arguments/phi`(arg,F,q,k,srange,S,n,'arg');
	else
		ERROR(`Wrong type of arguments.`);
	fi;
	#
	`qsum/options`(arg,sumrange,(proc(z) type(z,range) end),\
		srange, SUMRANGE, 'arg', srange);
	if (eval(SUMRANGE) <> srange) then
		_qsumrecursion_local_explicit_range:= true;
	fi;
	#
	hasopt:= `qsum/hasoption`(arg, recorder, 'opt', 'arg');
	`qsumrecursion/arguments/recorder`(hasopt, opt, lo, hi);
	#
	`qsum/options`(arg,recursion,{down,up},\
		'_qsumrecursion_recursion', '_qsumrecursion_local_recursion', 'arg');
	#
	`qsum/options`(arg,certificate,{down,up},\
		'_qsumrecursion_certificate', '_qsumrecursion_local_certificate',\
		'arg');
	#
	`qsum/options`(arg,rec2qhyper,{true,false},\
		'_qsumrecursion_rec2qhyper', '_qsumrecursion_local_rec2qhyper', 'arg');
	#
	hasopt:= `qsum/hasoption`(arg, proof, 'opt', 'arg');
	if hasopt then
		if type(opt,name) then
			assign(info,opt);
		else
			`qsumrecursion/bad_option`(proof,opt);
		fi;
	fi;
	#
	hasopt:= `qsum/hasoption`(arg, rhs, 'opt', 'arg');
	`qsumrecursion/arguments/rhs`(hasopt, opt);
	#
	`qsum/options`(arg,inhomo2homo,{true,false},\
		'_qsumrecursion_inhomo2homo', '_qsumrecursion_local_inhomo2homo',\
		'arg');
	#
	`qsum/options`(arg,solvemethod, {ABP,auto,gausselim,solve},\
		'_qsum_solvemethod', '_qsum_local_solvemethod', 'arg');
	`qsum/options`(arg,gausselim_normal,\
		`gausselim/procedures`, '_qsum_gausselim_normal',\
		'_qsum_local_gausselim_normal', 'arg');
	`qsum/options`(arg,specialsolution,{true,false},\
		'_qsum_specialsolution','_qsum_local_specialsolution','arg');
	`qsum/options`(arg,simple_recursion,{true,false},\
		'_qsumrecursion_simple_recursion',\
		'_qsumrecursion_local_simple_recursion','arg');
	`qsum/options`(arg,sigmafirst, {true,false},\
		'_qsumrecursion_sigmafirst', '_qsumrecursion_local_sigmafirst', 'arg');
	#
	if (arg <> []) then
		lprint(`WARNING: The following options are unknown:`);
		print(arg);
	fi;
	RETURN();
end:

# ======================================================================

`qgosper/arguments`:= proc(ARGS, F, q, kk, sumrange)
	global _qgosper_antidifference, _qgosper_local_antidifference,\
			_qgosper_local_simplify;
	local hasopt, opt, arg, i, k, S, n, dummy, multibasic;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if (nops(ARGS) < 3) then
		ERROR(`Wrong number of arguments.`);
	fi;
	multibasic:= false;
	# Fuege S(n) zu den Argumenten hinzu, um die Prozedure von
	# qsumrecursion nutzen zu koennen.
	arg:= [seq(ARGS[i],i=1..3), S(n), seq(ARGS[i],i=4..nops(ARGS))];
	if type(arg, `qsumrecursion/standard`) then
		`qsumrecursion/arguments/standard`(arg,F,q,k,dummy,S,n,'arg');
	elif type(arg, `qsumrecursion/phi`) then
		`qsumrecursion/arguments/phi`(arg,F,q,k,dummy,S,n,'arg');
	elif type([seq(ARGS[i],i=1..3)],[algebraic,{set(name),list(name)},\
			{name,name=algebraic..algebraic}]) then
		multibasic:= true;
		F:= ARGS[1];
		q:= ARGS[2];
		if type(ARGS[3],name) then
			k:= ARGS[3];
		else
			k:= op(1,ARGS[3]);
		fi;
		arg:= [seq(ARGS[i],i=4..nops(ARGS))];
	else
		ERROR(`Wrong type of arguments.`);
	fi;
	#
	`qsum/options`(arg,antidifference, {up,down},\
		'_qgosper_antidifference', '_qgosper_local_antidifference', 'arg');
	#
	`qsum/options`(arg, simplify,\
		proc(z) (nargs=0) or member(z,{'no','false'}) or\
		type(z,procedure) end, '_qgosper_simplify',\
		'_qgosper_local_simplify', 'arg', '_qgosper_simplify');
	if (_qgosper_local_simplify = NULL) or\
			member(_qgosper_local_simplify,{'false','no'}) then
		_qgosper_local_simplify:= eval;
	fi;
	#
	`qsum/options`(arg,series, {true,false},\
		'_qgosper_series', '_qgosper_local_series', 'arg');
	#
	`qsum/options`(arg,solvemethod, {ABP,auto,gausselim,solve},\
		'_qsum_solvemethod', '_qsum_local_solvemethod', 'arg');
	`qsum/options`(arg,gausselim_normal,\
		`gausselim/procedures`, '_qsum_gausselim_normal',\
		'_qsum_local_gausselim_normal', 'arg');
	`qsum/options`(arg,specialsolution,{true,false},\
		'_qsum_specialsolution','_qsum_local_specialsolution','arg');
	#
	if (arg <> []) then
		lprint(`WARNING: The following options are unknown:`);
		print(arg);
	fi;
	#
	opt:= op(select(type, ARGS, identical(k)=range));
	if (opt <> NULL) then
		sumrange:= op(2,opt);
	else
		sumrange:= NULL;
	fi;
	#
	kk:= k;
	#
	RETURN(multibasic);
end:

# ======================================================================
# ======================================================================

qWZcertificate:= proc(a, q, k, n)
local g, f;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	f:= subs(n=n+1,a) - a;
	g:= qsimpcomb(qgosper(f, q, k) / a);
	g:= factor(g);
	RETURN(g);
end:

# ======================================================================
########################################################################
#                                                                      #
# `qsumrecursion/remove_roots`: Remove all roots in a given expression #
#                                                                      #
#    Input: term, a rational function in all variables of              #
#           qpotlist=[K,N,...] with K=q^k, N=q^n, ...                  #
#                                                                      #
#    Output: the given expression term with all roots eliminated       #
#            i.e. if term has sqrt(a) we did the substitution a->a^2   #
#            If we did any substitution on q, i.e. q->q^i we also did  #
#            K->K^i, N->N^i, ...                                       #
#                                                                      #
#            Furthermore the procedure `power/power2name` is           #
#            applied, which does substitutions of the sort:            #
#                                                                      #
#                      name^(`*`(name)) -> name                        #
#                                                                      #
########################################################################

`qsumrecursion/remove_roots`:= proc(term,qpotlist,q,RESTORE_ROOTS)
	local z, unsub, unsub2, j, K;
	z:= `power/root/eliminate`(term, 'unsub');
	z:= `power/power2name`(z, 'unsub2');
	unsub2:= seq(op(1,j)=subs(unsub,op(2,j)),j=unsub2);
	j:= select(type, unsub, identical(q)=identical(q)^anything);
	if (j <> {}) then
		j:= 1/op(2,op(2,j[1]));
		z:= subs(seq(K[1]=K[1]^j, K=qpotlist), z);
		unsub:= [op(unsub), unsub2, seq(K[1]=q^(K[2]/j),K=qpotlist)];
	else
		unsub:= [op(unsub), unsub2, seq(K[1]=q^K[2], K=qpotlist)];
	fi;
	RESTORE_ROOTS:= unsub;
	userinfo(3, qsum, `Applied the substitutions:`,\
		print([seq(op(2,j)=op(1,j),j=unsub)]));
	factor(z);
end:
	
# ======================================================================

`qsumrecursion/qratios`:= proc(Func,q,k,K,n,N,RK,constRN,RN,UNSUBS)
local Rk, Rn, RkRn, c;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	userinfo(3, qsum, `Input term:`, print('F(n,k)'=Func));
   Rk:= qratio(Func, k, {q^k=K,q^n=N});
	if (_qsumrecursion_local_recursion = down) then
		Rn:= `power/subs`({q^n=N,q^k=K},qsimpcomb(subs(n=n-1,Func)/Func));
		RkRn:= `qsumrecursion/remove_roots`([Rk,Rn],[[K,k],[N,n]],q,UNSUBS);
		userinfo(3, qsum, print('F'(n,k+1)/'F'(n,k)=Rk),\
			print('F'(n-1,k)/'F'(n,k)=RkRn[2]));
	else
		Rn:= qratio(Func,n,{q^n=N,q^k=K});
		RkRn:= `qsumrecursion/remove_roots`([Rk,Rn],[[K,k],[N,n]],q,UNSUBS);
		userinfo(3, qsum, print('F'(n,k+1)/'F'(n,k)=Rk),\
			print('F'(n+1,k)/'F'(n,k)=RkRn[2]));
	fi;
   if not(type(RkRn,list(ratpoly(anything,K)))) or has(RkRn,k) then
      ERROR(`Input term is not hypergeomtric wrt. K and N.`);
   fi;
	RK:= RkRn[1];
	c:= select(has, 'c*(c+1)'*RkRn[2], K);
	RN:= unapply(c, n, N);
	constRN:= unapply(RkRn[2]/c, n, N);
	RETURN();
end:

# ----------------------------------------------------------------------

qsumrecursion:= proc()
	local arg,lo,hi,F,q,k,srange,S,n,rec,K,N,RK,constRN,RN,myrange,\
		restore_vars,info;
	global _qsumrecursion_proof, _qsumrecursion_local_explicit_range;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	# Assign all Variables from F, q,...,hi
	`qsumrecursion/arguments`([args],F,q,k,srange,S,n,lo,hi,info);
########################################################################
########################################################################
# "magic factor" (1 + F(n,f(k))/F(n,k))/2
########################################################################
########################################################################
	_qsumrecursion_proof:= '_qsumrecursion_proof';
	_qsumrecursion_proof['_F']:= F;
	_qsumrecursion_proof['_sumvar']:= k;
	_qsumrecursion_proof['_recvar']:= n;
	_qsumrecursion_proof['_sumrange']:= srange;

	F:= `power/expand`(F);
	# Determine the qratios of F wrt. k and n and assign to RK, RN.
	`qsumrecursion/qratios`(F,q,k,K,n,N,RK,constRN,RN,'restore_vars');

	if (_qsumrecursion_local_rec2qhyper=true) and\
			(_qsumrecursion_local_explicit_range <> true) then
		myrange:= `rat2qhyper/bounds`(\
			subs(N=q^n,RK),K,q,[k,integer,n,posint]); 
	else
		myrange:= srange;
	fi;

	rec:= traperror(`qsumrecursion/find_recurrence`(F,q,k,srange,myrange,\
		S,n,K,N,RK,constRN,RN,lo,hi,restore_vars));

	if type(rec,{`=`,string,name}) then
		_qsumrecursion_proof['_recursion']:= rec;
	else
		_qsumrecursion_proof['_recursion']:= rec[1];
		rec:= [rec[2],rec[3]];
		_qsumrecursion_proof['_Sum']:= rec;
	fi;

	if type(rec, string) then 
		assign(info, rec);
		ERROR(rec); 
	fi;
	assign(info, copy(_qsumrecursion_proof));

	RETURN(rec);
end:

# ----------------------------------------------------------------------

`qsumrecursion/certificate/singularities`:= proc(poly,q,n)
	local p, N, i;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if type(poly,`*`) then
		p:= {op(poly)};
	else
		p:= {poly};
	fi;
	p:= `power/subs`(q^n=N,select(has,p,n));
	p:= map(solve, p, N);
	p:= combine(normal(`power/expand`(p)),power);
	p:= select(type,p,{1,identical(q),identical(q)^posint});
	map(proc(z) if type(z,1) then 0 elif type(z,identical(q)) then 1\
		else op(2,z) fi end,p);
end:

# ----------------------------------------------------------------------

`qsumrecursion/rhs`:= proc(F,req,S,Poly,X,PQR,k,K,n,N,q,sumrange,myrange,unsub)
	local rec, cert, RHS, proofeq, singularity, j;
	global _qsumrecursion_proof;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	cert:=`recursion/denom`(req, S, 'rec');
	if (_qsumrecursion_local_certificate = down) then
		cert:= cert * PQR[2]*X/PQR[1] * Poly;
	else
		cert:= cert * subs(K=K/q,PQR[3]*X)/PQR[1] * Poly;
	fi;
	if (_qsumrecursion_local_rhs=always) or
		((_qsumrecursion_local_rhs=explicit) and
		not(has(sumrange,infinity))) then

      cert:= subs(unsub, factor(cert));
		singularity:= `qsumrecursion/certificate/singularities`(\
			denom(cert),q,n);

		# Store certificate in global info variable
		_qsumrecursion_proof['_certificate']:= cert;
		_qsumrecursion_proof['_restriction']:= singularity;
		_qsumrecursion_proof['_direction']:= _qsumrecursion_local_certificate;

		rec:= `recursion/collect`(rec, S);

		if (_qsumrecursion_local_rhs = always) or
			((_qsumrecursion_local_rhs = explicit) and
			not(has(sumrange,infinity))) then

			assume(j,posint);
			RHS:= `qsumrecursion/inhomogeneous_part`(op(subs(n=j,\
				[rec,S,n,N,cert,F,k=sumrange,q,unsub])));
			RHS:= subs(j=n,RHS);
			
			if (RHS <> 0) then
				if (_qsumrecursion_local_inhomo2homo) then
					_qsumrecursion_proof['_inhomrec']:= subs(N=q^n,(rec = RHS));
					rec:= `qsumrecursion/inhomogeneous_part/inhomo2homo`\
						(rec,RHS,S,q,N,unsub);
				else
					rec:= `recursion/sortq`(rec=RHS, S, N, unsub);
				fi;
			else
				rec:= `qsumrecursion/rec2qhyper`(F,k,n,N,q,rec,S,myrange,unsub);
			fi;
		else
			rec:= `qsumrecursion/rec2qhyper`(F,k,n,N,q,rec,S,myrange,unsub);
		fi;
		proofeq:= eval(subs(S=unapply(F,n),op(1,rec)));
		if (_qsumrecursion_local_certificate = down) then
			proofeq:= (proofeq = cert*F-subs(k=k-1,cert*F));
		else
			proofeq:= (proofeq = subs(k=k+1,cert*F)-cert*F);
		fi;
		_qsumrecursion_proof['_proof']:= proofeq;
   else
		_qsumrecursion_proof['_certificate']:= subs(unsub,cert);
		rec:= `qsumrecursion/rec2qhyper`(F,k,n,N,q,rec,S,myrange,unsub);
   fi;
	RETURN(rec);
end:  # of `qsumrecursion/rhs`

# ----------------------------------------------------------------------
# This procedure was made to preserve the special structure of
#            sum(sigma[j]*F(n+j)/F(n),j=0..J)
# i.e. keep the coefficients of sigma factored and put the whole
# term over a common denominator.

`qsumrecursion/normal_add`:= proc(r1,sigma,constRN,RN,q,n,N,J,SIGMAFAC)
	local a, den, j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if (_qsumrecursion_local_simple_recursion = false) then
		if (_qsumrecursion_local_recursion = down) then
			SIGMAFAC[J]:=  1 / convert(\
				[seq(factor(constRN(n-j,N/q^j)),j=0..J-1)], `*`);
			a:= factor(convert([seq(RN(n-j,N/q^j),j=0..J-1)], `*`));
		else
			SIGMAFAC[J]:=  1 / convert(\
				[seq(factor(constRN(n+j,N*q^j)),j=0..J-1)], `*`);
			a:= factor(convert([seq(RN(n+j,N*q^j),j=0..J-1)], `*`));
		fi;
		userinfo(3, qsum, `Applied substitution:`,
			print(sigma[J]/eval(SIGMAFAC[J])=sigma[J]));
	else
		if (_qsumrecursion_local_recursion = down) then
			SIGMAFAC[J]:=  1;
			a:= factor(convert(\
				[seq(constRN(n-j,N/q^j)*RN(n-j,N/q^j),j=0..J-1)], `*`));
		else
			SIGMAFAC[J]:=  1;
			a:= factor(convert(\
				[seq(constRN(n+j,N*q^j)*RN(n+j,N*q^j),j=0..J-1)], `*`));
		fi;
	fi;
	a:= [numer(a),denom(a)];
	den:= factor(r1[2]*a[2]/frontend(gcd,[r1[2],a[2]]));
#	a:= collect(r1[1], indets(r1,sigma[integer]), distributive,\
#		unapply('j'*factor(den/r1[2]),'j')) + sigma[J]*a[1]*factor(den/a[2]);
#	[a, den];
	if type(r1[1],`+`) then
		j:= map(unapply('j'*factor(den/r1[2]),'j'), r1[1])
	else
		j:= r1[1] * factor(den/r1[2]);
	fi;
	[j + sigma[J]*a[1]*factor(den/a[2]), den];
end:

# ----------------------------------------------------------------------

`qsumrecursion/find_recurrence`:= proc(
		F,q,k,sumrange,myrange,S,n,K,N,RK,constRN,RN,lo,hi,unsub)
	local Poly, sigma, A, PQR, X, J, j, rec, const, sigmafac;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	sigma[0]:= 1;
	sigmafac[0]:= 1;
	Poly:= [sigma[0], 1];
	A:= NULL;
	for J from 1 to hi while (A = NULL) do
		Poly:= `qsumrecursion/normal_add`(Poly,sigma,constRN,RN,q,n,N,J,sigmafac);
		if (lo <= J) then
			A:= factor(RK * Poly[2] / subs(K=K*q,Poly[2]));
			PQR:= `qgosper/update`(Poly[1],numer(A),denom(A),q,K);
			X:= `qgosper/findf`(op(PQR), q, K, [seq(sigma[j], j=0..J)], 'A');
		fi;
	od;
	if (A = NULL) then 
	ERROR(cat(`Found no recursion of order smaller than `,J,`.`));
	fi;
	const:= numer(sigmafac[J-1]);
	if (_qsumrecursion_local_recursion = down) then
# 1.3 sum -> add
#		rec:= subs(A, sum('sigma[j]*sigmafac[j]/const*S(n-j)', 'j'=0..J-1));
		rec:= subs(A, add(sigma[j]*sigmafac[j]/const*S(n-j), j=0..J-1));
	else
# 1.3 sum -> add
#		rec:= subs(A, sum('sigma[j]*sigmafac[j]/const*S(n+j)', 'j'=0..J-1));
		rec:= subs(A, add(sigma[j]*sigmafac[j]/const*S(n+j), j=0..J-1));
	fi;
	Poly:= subs(A, Poly[1]/Poly[2]/const);
	PQR:= subs(A, PQR);
	rec:= `qsumrecursion/rhs`(F,rec,S,Poly,X,PQR,k,K,n,N,q,sumrange,myrange,unsub);
	RETURN(rec);
end:

# ----------------------------------------------------------------------
qkfreerec:=proc(f,q,k,n,kmax,nmax)
local SUM,N,K,variables,i,j,l,solution,rat,F,a;
options remember,
`Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if nargs>6 then F:=args[7] fi; if nargs>7 then a:=args[8] fi;
N:=(kmax+1)*(nmax+1);
variables:={seq(seq(a[i,j],i=0..kmax),j=0..nmax)};
SUM:=0;
rat:=0;
for i from 0 to kmax do
  for j from 0 to nmax do
    SUM:=SUM+
           a[i,j]*qsimpcomb(subs(n=n+j,k=k+i,f)/f);
    rat:=rat+a[i,j]*F(n+j,k+i)
  od;
od;
SUM:=`power/subs`({q^k=K},SUM);
SUM:=collect(numer(normal(SUM)),K);
solution:={solve({coeffs(SUM,K)},variables)};
if solution={} or {seq(op(2,op(l,op(1,solution))),l=1..N)}={0} then
  ERROR(`no kfree recurrence equation of order (`,kmax,nmax,`) exists`);
fi;
rat:=subs(op(1,solution),rat);
rat:=numer(normal(rat));
for i from 0 to kmax do
  for j from 0 to nmax do
    rat:=collect(rat,F(n+j,k+i));
  od;
od;
rat:=map(factor,rat);
RETURN(map(simplify,rat)=0);
end: # qkfreerec

qfasenmyer:=proc(f,q,k,sn,kmax,nmax)
local F,N,a,n,S,i,j,recursion,tmp;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sn,function) then S:=op(0,sn); n:=op(1,sn) else n:=sn fi;
recursion:=op(1,qkfreerec(f,q,k,n,kmax,nmax,F,a));
for i from 0 to kmax do
  for j from 0 to nmax do
    recursion:=subs(F(n+j,k+i)=S(n+j),recursion);
  od;
od;
# tmp:=normal((recursion-coeff(recursion,S(n+nmax))*S(n+nmax))/
# coeff(recursion,S(n+nmax)));
tmp:=normal(solve(recursion,S(n+nmax)));
recursion:=denom(tmp)*S(n+nmax)-numer(tmp);
# recursion:=denom(tmp)*S(n+nmax)+numer(tmp);
recursion:=`power/subs`({q^n=N},recursion);
for j from 0 to nmax do
  recursion:=collect(recursion,S(n+j));
od;
recursion:=map(factor,recursion);
RETURN(map(combine,subs(N=q^n,recursion),power)=0);
end: # qfasenmyer

`print/qdiff`:= proc(f)
	local D, j, qdiffstring;
	#qdiffstring:= cat(`D[`,args[nargs],`][`);
	qdiffstring:= cat(`D`,args[nargs],`[`);
	for j in [args[2..nargs-2]] do
		qdiffstring:= cat(qdiffstring,j,`,`);
	od;
	qdiffstring:= cat(qdiffstring, args[nargs-1],`](`,convert(f,string),`)`);
	parse(qdiffstring);
end:

# ----------------------------------------------------------------------

qdiff:= proc(F,x)
	local q, j;
	options system, remember;
	q:= args[nargs];
	if (nargs = 2) then
		F;
	elif type(F,function) and not(type(op(0,F),procedure)) then
		if not(has([seq(has(F,j),j=args[2..nargs-1])],false)) then
			'procname(args)';
		else
			0;
		fi;
	elif (nargs = 3) then
		normal((F-subs(x=x*q,F))/((1-q)*x));
	elif (nargs > 3) then
		procname(normal((F-subs(x=x*q,F))/((1-q)*x)),args[3..nargs]);
	else
		'procname(args)';
	fi;
end:

# ----------------------------------------------------------------------

`qsumdiffeq/arguments`:= proc(ARGS,F,q,k,S,evalS,z,lo,hi)
	local arg, dummy, oval;
	global _qsumdiffeq_evalqdiff, _qsumdiffeq_local_evalqdiff;
	arg:= ARGS;
	`qsum/options`(arg,evalqdiff,{true,false},\
		'_qsumdiffeq_evalqdiff', '_qsumdiffeq_local_evalqdiff', 'arg');
	if (_qsumdiffeq_local_evalqdiff) then
		evalS:= proc() 'procname(args)' end;
	fi;
	`qsumrecursion/arguments`(arg,F,q,k,dummy,S,z,lo,hi,dummy);
end:

# ----------------------------------------------------------------------

qsumdiffeq:=proc()
	local zeit,F,q,k,S,evalS,z,lo,hi,sigma,sigmasol,Poly,RK,K,j,J,\
		PQR,f,rec;
   options `Copyright 1995  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
   zeit:= time();
	`qsumdiffeq/arguments`([args],F,q,k,S,evalS,z,lo,hi);
	sigma[0]:= 1;
	sigmasol:= NULL;
	Poly:= [sigma[0],1];
	RK:= `power/subs`(q^k=K,qratio(F,k));
	for J from 1 to hi while (sigmasol = NULL) do
		Poly:= normal(Poly[1]/Poly[2] + sigma[J]*\
			`power/subs`(q^k=K,qsimpcomb(qdiff(F,z$J,q)/F)));
		if has([RK,Poly],{k,qpochhammer}) then
			ERROR(`Algorithm not applicable.`);
		fi;
		Poly:= [numer(Poly),denom(Poly)];
		if (J < lo) then next; fi;
		PQR:= factor(RK * Poly[2] / subs(K=K*q,Poly[2]));
		PQR:= `qgosper/update`(Poly[1],numer(PQR),denom(PQR),q,K);
		f:= `qgosper/findf`(op(PQR),q,K,[seq(sigma[j],j=0..J)],'sigmasol');
	od;
	if (sigmasol = NULL) then
                ERROR(cat(`Found no q-differential equation of order smaller than `,J,`.`));
	fi;
# 1.3 sum -> add
#	rec:= subs(sigmasol, sum('sigma[j]*qdiff(evalS(z),z$j,q)', 'j'=0..J-1));
	rec:= subs(sigmasol, add(sigma[j]*qdiff(evalS(z),z$j,q), j=0..J-1));
	rec:= subs(evalS=S, rec);
	if has(rec,qdiff) then
		j:= [seq(qdiff(S(z),z$j,q),j=0..J-1)];
		f:= qdiff;
	else
		j:= [seq(S(z*q^j),j=0..J-1)];
		f:= S;
	fi;
	rec:= collect(rec,j,distributed,factor);
	`recursion/denom`(rec,f,'rec');
	rec:= sort(rec,j) = 0;
	if (_qsum_profile) then
		printf(`CPU-time: %.1f seconds`, time()-zeit);
	fi;
	RETURN(rec);
end:

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qsimpcomb/qlimit`: Determination of limits involving                #
#                     qfac-expressions                                 #
#                                                                      #
#   Input: expr - an expression, sub - an substitution pattern of type #
#          name=algebraic                                              #
#                                                                      #
#   Output: limit(expr,sub), with (hopefully) all qfac-terms correctly #
#           handled.                                                   #
#                                                                      #
########################################################################

`qsimpcomb/qlimit`:= proc(expr,sub)
	local f, fnew, flist, k, knew, lim, inflim, ZERO, restrictions;
	flist:= eval(subs(qpochhammer=`qsimpcomb/qlimit/simplify/infinity`,\
		convert(expr,qpochhammer)));
	flist:= factor(flist);
	k:= op(1,sub);
	knew:= op(2,sub);
	if type(flist,`*`) then
		flist:= [op(flist)];
	else
		flist:= [flist];
	fi;
	flist:= map(proc(z) if type(z,anything^integer) then [op(z)]\
		else [z,1] fi end, flist);
	lim:= 1;
	for f in flist do
		if has(f,qpochhammer) then
			lim:= lim *\
				`qsimpcomb/qlimit/qfac`(f[1],k,knew,ZERO,'restrictions')^f[2];
		elif has(knew,infinity) then
				inflim:= f[1];
		else
			fnew:= expand(subs(k=knew,f[1]));
			if (fnew = 0) then
				lim:= lim * ZERO^f[2];
			else
				lim:= lim * fnew^f[2];
			fi;
		fi;
	od;
	lim:= traperror(subs(ZERO=0, factor(lim)));
	if (lim = lasterror) then
		if (lim = `division by zero`) then
			ERROR(`Limit seems to be infinite.`);
		else
			ERROR(`Encountered an undefined limit.`);
		fi;
	fi;
	if (lim <> 0) and has(knew,infinity) then
		inflim:= traperror(limit(inflim,k=knew));
		if (lim = lasterror) or has(inflim,{limit,FAIL,undefined,infinity}) then
			ERROR(`Encountered an undefined limit.`);
		fi;
		lim:= lim * inflim;
	fi;
	userinfo(3,qlimit,print('limit'(expr,sub)=lim));
	RETURN(lim);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qsimpcomb/qlimit/simplify/infinity`: simplify third argument of a   #
#                                       qfac function involving        #
#                                       infinity                       #
#                                                                      #
#   Input: a,q,k - arguments of a qfac function                        #
#                                                                      #
#   Output: an expression equivalent to qfac(a,q,k) k is simplified.   #
#                                                                      #
########################################################################

`qsimpcomb/qlimit/simplify/infinity`:= proc(a,q,k)
	qpochhammer(a,q,`qsimpcomb/simplify/infinity`(k));

end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qsimpcomb/qlimit/qfac/is`: determine if z is of type typ            #
#                                                                      #
#   Input: z - an expression, typ - a type like posint, negint,        #
#          PREC - a name to which true or false will be assigned, i.e. #
#          true if is(z,typ) and false if z is 'nearly typ'.           #
#                                                                      #
#   Output: is(z,typ)                                                  #
#                                                                      #
########################################################################

`qsimpcomb/qlimit/qfac/is`:= proc(z,typ,WARNING)
	local i, isit, c, l, h;
	i:= expand(z);
	if type(typ,range) then
		l:= op(1,typ);
		h:= op(2,typ);
		isit:= evalb(is(l<=z) and is(z<=l));
		if has(isit,[false,FAIL]) and hastype(z,name) then
			l:= select(type,l+'c'+'c^2',integer);
			h:= select(type,h+'c'+'c^2',integer);
			if (l <> 0) or (h <> 0) then
				if (nargs=3) then
					WARNING:= sprintf(`Assuming %a to be of type %a.`,z,typ);
				fi;
				isit:= procname(z,op(1,typ)-l..op(2,typ)-h);
			fi;
		fi;
	elif has(z,infinity) then
		if ((typ = posint) and (z = infinity)) or\
			 (typ = negint) and (z = -infinity) then
			isit:= true;
		else
			isit:= false;
		fi;
	else
		isit:= is(i,typ);
	fi;
	if isit then
		true;
	elif type(i,`+`) then
		c:= select(type,i,integer);
		if (c <> 0) then
			if (nargs=3) then
				WARNING:= sprintf(`Assuming %a to be of type %a.`,z,typ);
			fi;
			procname(i-c,typ);
		else
			isit;
		fi;
	else
		isit;
	fi;
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qsimpcomb/qlimit/qfac/warning`: Issue a warning message if expr     #
#                                  is only 'nearly' of type typ...     #
#                                  (see above)                         #
#                                                                      #
########################################################################

`qsimpcomb/qlimit/qfac/warning`:= proc(f,sub,lim,w1,w2);
	if (eval(w1) <> w1) then
		lprint(eval(w1));
	fi;
	if (eval(w2) <> w2) then
		lprint(eval(w2));
	fi;
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qsimpcomb/qlimit/qfac`: Rewrite qfac(a,q,k) to                      #
#                                                                      #
#                                                                      #
#     (i)   qfac(a,q,i)*(1-a*q^i)*qfac(a*q^(i+1),q,k-i-1),             #
#                                                                      #
#                  if a=q^i and k>0, 0<=i<k,                           #
#                                                                      #
#                                                                      #
#    (ii)   qfac(a,q,1-i)*(1-a*q^(-i))^(-1)*qfac(a*q^(-i),q,k+i),      #
#                                                                      #
#                  if a=q^i and k<0 and 1<=i<=-k.                      #
#                                                                      #
#    Afterwards substitute any factors that are zero by the symbolic   #
#    variable ZERO.                                                    #
#                                                                      #
#  Input: expr - an expression, where k shall be replaced by knew,     #
#         ZERO - a name, it's used to tag zero factors,                #
#         RESTRICTIONS - a name, where informations will be stored.    #
#                                                                      #
#  Output: subs(k=knew,expr), where zero factors are represented by    #
#          the symbolic variable ZERO.                                 #
#                                                                      #
########################################################################


`qsimpcomb/qlimit/qfac`:= proc(expr,k,knew,ZERO,RESTRICTIONS)
	local qfaclist, qf, lim, a, q, qe, n, sub, warning1, warning2;
	lim:= subs(k=knew, expr);
	qfaclist:= [op(indets(lim,specfunc(anything,qpochhammer)))];
	for qf in qfaclist do
		a:= `power/combine`(op(1,qf));
		if type(op(2,qf),name^integer) then
			q:= op(1,op(2,qf));
			qe:= op(2,op(2,qf));
		else
			q:= op(2,qf);
			qe:= 1;
		fi;
		n:= op(3,qf);
		if (n = 0) then
			lim:= subs(qf=1, lim);
			next;
		elif type(a,1) then
			a:= 0;
		elif type(a,identical(q)) then
			a:= 1/qe;
		elif type(a,identical(q)^anything) then
			a:= op(2,a)/qe;
		else
			next;
		fi;
		if not(is(a,integer)) then
			next;
		fi;
		if `qsimpcomb/qlimit/qfac/is`(n,posint,'warning1') then
			if `qsimpcomb/qlimit/qfac/is`(a,1-n..0,'warning2') then
				lim:= subs(qf=\
					qfac(q^(qe*a),q^qe,-a)*ZERO*qfac(q^qe,q^qe,n+a-1), lim);
				`qsimpcomb/qlimit/qfac/warning`(expr,k=knew,lim,'warning1','warning2');
			fi;
		elif `qsimpcomb/qlimit/qfac/is`(n,negint,'warning1') then
			if `qsimpcomb/qlimit/qfac/is`(a,1..-n,'warning2') then
				lim:= subs(qf=\
					qfac(q^(qe*a),q^qe,1-a)/ZERO*qfac(1,q^qe,n+a), lim);
				`qsimpcomb/qlimit/qfac/warning`(expr,k=knew,lim,'warning1','warning2');
			fi;
		fi;
	od;
	RETURN(lim);
end:

# ----------------------------------------------------------------------

# ======================================================================

`qsumrecursion/determine_range`:= proc(sumrange, n)
   local lo, hi, lm, lb, hm, hb, typ;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
   typ:= FAIL;
	lo:= op(1, sumrange);
	hi:= op(2, sumrange);
   if type(lo, integer) and has(hig,n) then
      if (coeff(hi,n) > 0) then
         RETURN(posint);
      else
         RETURN(negint);
      fi;
   fi;
   lm:= coeff(lo, n);  lb:= coeff(lo, n, 0);
   hm:= coeff(hi, n);  hb:= coeff(hi, n, 0);
   if (lm < 0) then
      if (lm < hm) or ((lm = hm) and (lb < hb)) then
         typ:= posint;
      elif (lm > hm) then
         typ:= negint;
      fi;
   elif (lm = 0) then
      if (hm < 0) then typ:= negint; else typ:= posint; fi;
   else
      if (hm <= 0) then
         typ:= negint;
      elif (hm > lm) or ((hm = lm) and (lb < hb)) then
         typ:= posint;
      fi;
   fi;
   RETURN(typ);
end:
 

# ======================================================================

`qsumrecursion/inhomogeneous_part`:= proc(
	rec,S,n,N,cert,F,sumrange,q,unsub)
	local stamm, k, Func, lower_bound, upper_bound,\
		rec_args, sigma, i, j, lower, upper, RHS;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	Func:= unapply(factor(`power/expand`(F)), n);
	stamm:= factor(cert*Func(n));
	k:= lhs(sumrange);
	lower_bound:= unapply(op(1,rhs(sumrange)), n);
	upper_bound:= unapply(op(2,rhs(sumrange)), n);
	rec_args:= [op(map(op, indets(rec, specfunc(anything,S))))];
	sigma:= table([seq(i=subs(unsub,coeff(rec,S(i))), i=rec_args)]);
	lower:= max(seq(lower_bound(i), i= rec_args));
	upper:= min(seq(upper_bound(i), i= rec_args));
	if (_qsumrecursion_local_certificate = down) then
		RHS:= `qsimpcomb/qlimit`(stamm, k=upper) -\
				`qsimpcomb/qlimit`(stamm, k=lower-1);
	else
		RHS:= `qsimpcomb/qlimit`(stamm, k=upper+1) -\
				`qsimpcomb/qlimit`(stamm, k=lower);
	fi;
	for i in rec_args do
		if lower_bound(i) <> lower then
# 1.3 sum -> add
#			RHS:= RHS + sigma[i] * sum('`qsimpcomb/qlimit`\
#				(Func(i),k=j)', 'j'=lower_bound(i)..lower-1);
			RHS:= RHS + sigma[i] * add(`qsimpcomb/qlimit`\
				(Func(i),k=j), j=lower_bound(i)..lower-1);
		fi;
		if upper_bound(i) <> upper then
# 1.3 sum -> add
#			RHS:= RHS + sigma[i] * sum('`qsimpcomb/qlimit`\
#				(Func(i),k=j)', 'j'=upper+1..upper_bound(i));
			RHS:= RHS + sigma[i] * add(`qsimpcomb/qlimit`\
				(Func(i),k=j), j=upper+1..upper_bound(i));
		fi;
	od;
	RHS:= (qsimplify@qsimpcomb)(RHS);
	userinfo(3, qsum, `Inhomogeneous part of recursion:`, print(RHS));
	RETURN(RHS);
end:   # of `qsumrecursion/inhomogeneous_part`

# =======================================================================

`qsumrecursion/inhomogeneous_part/inhomo2homo`:= proc(Lhs,Rhs,S,q,N,unsub)
local arglist, n, rec, RHS, subst;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	indets(Lhs, specfunc(linear, S));
   arglist:= op(map(op, indets(Lhs, specfunc(linear, S))));
   n:= op(indets([arglist],name));
   RHS:= Rhs;
	if (RHS = 0) then
		RETURN(`recursion/sortq`(Lhs=Rhs, S, N, {N=q^n}));
	fi;
	RHS:= qratio(RHS, n, {q^n=N});
   if (min(arglist) <> n) then
      subst:= {N=N/q, n=n-1};
		RHS:= factor(subs(N=N/q, 1/RHS));
		arglist:= -1;
   else
      subst:= {N=N*q, n=n+1};
      RHS:= factor(RHS);
   fi;
   if not type(RHS, ratpoly(anything,N)) then
      printf(`WARNING: Unable to make the recurrence homogeneous:\n`);
      printf(`         Righthandside of recursion is not `);
      printf(`q-hypergeometric wrt. %a.\n`, q^n);
		rec:= `recursion/sortq`(Lhs=Rhs, S, N, {N=q^n}):
      RETURN(rec);
   fi;
	if (arglist = -1) then
		`recursion/denom`(subs(subst,Lhs)*denom(RHS)-Lhs*numer(RHS),S,'rec');
		rec:= (rec = 0);
	else
		rec:= subs(subst,Lhs)*denom(RHS) - Lhs*numer(RHS) = 0;
	fi;
   rec:= `recursion/sortq`(rec, S, N, unsub);
   RETURN(rec);
end:
 
# ----------------------------------------------------------------------

# ======================================================================

qgosper:= proc()
	local F, X, q, k, sumrange, rat, dummy, PQR, G, K;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;

	if (`qgosper/arguments`([args], F, q, k, sumrange)) then
		RETURN(`qgosper/multibasic`(F, q, k, sumrange));
	fi;

   rat:= qratio(F, k, {q^k=K});
	rat:= factor(`power/expand/root`(rat));
   if not type(rat, ratpoly(anything, K)) or has(rat,k) then
      ERROR(`Input term is not hypergeometric wrt. `.q^k);
   fi;

   PQR:= `qgosper/update`(1, numer(rat), denom(rat), q, K);
   G:= `qgosper/findf`(op(PQR), q, K);

   if (G = NULL) then
		if (_qgosper_local_series = true) then
			G:= subs(K=q^k,PQR[1]=PQR[2]*X(k)-subs(K=K/q,PQR[3])*X(k-1));
			G:= qrecsolve(G,q,X(k),solution=series);
		fi;
		if (G = NULL) or (G = []) then
			ERROR(`No q-hypergeometric antidifference exists.`);
		fi;
		G:= G[1];
		rat:= qratio(op(1,G),op(1,op(2,G)),{q^op(1,op(2,G))=K});
		rat:= op(2,`rat2qhyper/bounds`(rat,K,q,[]));
		if (rat <> infinity) then
			G:= subsop(2=(op(1,op(2,G))=subsop(2=rat,op(2,op(2,G)))), G);
		fi;
		G:= '`power/expand`'(G);
   fi;

	if (_qgosper_local_antidifference = down) then # Gosper downwards
		G:= subs(K=q^k, PQR[2]*(G)/PQR[1]) * F;
		if (sumrange <> NULL) then
			G:= subs(k=op(2,sumrange),G)-subs(k=op(1,sumrange)-1,G);
		fi;
	else # Gosper upwards:
		G:= subs(K=q^k,(subs(K=K/q,k=k-1, PQR[3]*G))/PQR[1]) * F;
		if (sumrange <> NULL) then
			G:= subs(k=op(2,sumrange)+1,G)-subs(k=op(1,sumrange),G);
		fi;
	fi;
	G:= _qgosper_local_simplify(G);

   RETURN(G);
end:

# ----------------------------------------------------------------------
# ======================================================================
#
# Folgende Dateien werden ben"otigt:  - timings
#                                     - power.mpl
#
# ======================================================================
# ======================================================================

`convert/qbinomial2qpochhammer`:= proc(n,k,q)
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	qpochhammer(q,q,n) / qpochhammer(q,q,k) / qpochhammer(q,q,n-k);
end:

# ----------------------------------------------------------------------

`convert/qbrackets2qpochhammer`:= proc(n,q)
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	(q^n-1) / (q-1);
end:

# ----------------------------------------------------------------------

`convert/qfactorial2qpochhammer`:= proc(n,q)
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	qpochhammer(q,q,n) / (1-q)^n;
end:

# ----------------------------------------------------------------------

`convert/qGAMMA2qpochhammer`:= proc(z,q)
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	qpochhammer(q,q,infinity)/qpochhammer(q^z,q,infinity)*(1-q)^(1-z);
end:

# ----------------------------------------------------------------------

`convert/qpochhammer`:= proc()
local f;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	f:= subs(qfac=qpochhammer,\
				qbinomial= `convert/qbinomial2qpochhammer`,\
            qbrackets= `convert/qbrackets2qpochhammer`,\
            qfactorial=`convert/qfactorial2qpochhammer`,\
            qGAMMA=`convert/qGAMMA2qpochhammer`,\
            args[1]);
	eval(f);
end:

# ======================================================================

qpochhammer:= proc(a, q, n)
local f, j, qn;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if (nargs = 2) then
		f:= eval(qpochhammer(a,q,infinity));
	elif (nargs > 3) then
		qn:= args[nargs-1..nargs];
		f:= product('qpochhammer(args[j],qn)', 'j'=1..nargs-2);
	elif (a = 0) or (n = 0) then
		f:= 1;
#	elif (a = 1) and (type(n,posint)) then
#		f:= 0;
#	elif type(n, posint) then 
#		f:= product('1-a*q^j', 'j'=0..n-1);
#	elif type(n, negint) then
#		f:= traperror(1/product('1-a*q^(n+j)', 'j'=0..-n-1));
	else
		f:= 'procname(args)';
	fi;
#	if (f = lasterror) then
#		f:= 'procname(args)';
#	fi;
	RETURN(f);
end:

# ======================================================================

`qsimpcomb/simplify/infinity`:= proc(kl)
local z, inf;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
   z:= expand(kl);
   if (z = 'infinity') or (z = -'infinity') then
      z;
   elif has(z, infinity) then
      z:= traperror(signum(lcoeff(z, infinity)));
      if (z = lasterror) or has(z, signum) then
         ERROR(`Cannot simplify bound: `.kl);
      fi;
      z*infinity;
	else
		z;
   fi;
end:
 
# ======================================================================

`expand/qpochhammer/q_exponent`:= proc(a, q)
local p;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	p:= select(type, 'p*(p+1)'*a, {identical(q)^anything,identical(q)});
	if type(p, `^`) then
		op(2,p);
	elif (p = q) then
		1;
	else
		0;
	fi;
end:

# ----------------------------------------------------------------------

`expand/qpochhammer/standard_integer_part`:= proc(a)
local sip, r; # sip -> standard integer part
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	sip:= select(type, 'sip+sip^2'+expand(a), rational);
	if (sip <> 0) then
		sip:= iquo(numer(sip), denom(sip), 'r');
		if type(r, negative) then 
			sip:= sip - 1;
		fi;
	fi;
	RETURN(sip);
end:

# ----------------------------------------------------------------------

`expand/qpochhammer/infinity`:= proc(a, q, qe, k, m)
local j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if (k = -infinity) then 
		qpochhammer(a,q^qe,k);
	elif (m > 0) then
		qpochhammer(a,q^qe,k) / product('(1-a*q^(qe*j))','j'=0..m-1);
	else
		qpochhammer(a,q^qe,k) * product('(1-a*q^(qe*j))', 'j'=m..-1);
	fi;
end:

# ----------------------------------------------------------------------

`expand/qpochhammer`:= proc()
local a, q, k, qe, m, n, f, j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	a:= `power/combine`(factor(args[1]));
	if (a = 0) then RETURN(1); fi;
	q:= args[2];
	k:= args[3];
	if type(q, `^`) then 
		qe:= op(2,q);
		q:=  op(1,q);
	else
		qe:= 1;
	fi;
	if (a = q^qe) then
		m:= 0;
	else
		m:= `expand/qpochhammer/q_exponent`(a, q);
		m:= `expand/qpochhammer/standard_integer_part`(m / qe);
		if (a = q^(qe*m)) and type(m, posint) then m:= m-1; fi;
	fi;
	n:= `expand/qpochhammer/standard_integer_part`(k);
	if (n = 0) and (m = 0) then
		RETURN(qpochhammer(a, q^qe, k));
	fi;
	a:= a / q^(m*qe);
	if has(k,infinity) then
		k:= `qsimpcomb/simplify/infinity`(k);
		RETURN(`expand/qpochhammer/infinity`(a, q, qe, k, m));
	fi;
	k:= k - n;
	if (n+m >= 0) then
		if (m >= 0) then
			f:=     product('(1-a*q^(qe*j))', 'j'=k..k-1+n+m);
			f:= f / product('(1-a*q^(qe*j))', 'j'=0..m-1);
		else # (m < 0)
			f:=     product('(1-a*q^(qe*j))', 'j'=k..k-1+n+m);
			f:= f * product('(1-a*q^(qe*j))', 'j'=m..-1);
		fi;
	else # (m + n < 0)
		if (m >= 0) then
			f:= 1 / product('(1-a*q^(qe*j))', 'j'=k+m+n..k-1);
			f:= f / product('(1-a*q^(qe*j))', 'j'=0..m-1);
		else # (m < 0)
			f:= 1 / product('(1-a*q^(qe*j))', 'j'=k+m+n..k-1);
			f:= f * product('(1-a*q^(qe*j))', 'j'=m..-1);
		fi;
	fi;
	f * qpochhammer(a,q^qe,k);
end:

# ----------------------------------------------------------------------

`expand/qbinomial`:= proc()
local n, k, q, f;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	n:= args[1];
	k:= args[2];
	q:= args[3];
	f:= `expand/qpochhammer`(q,q,n) / `expand/qpochhammer`(q,q,k);
	f:= f / `expand/qpochhammer`(q,q,n-k);
	normal(f);
end:

# ----------------------------------------------------------------------

`expand/qbrackets`:= proc(n,q)
	(q^n-1) / (q-1);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# Use the following two simple transformations                         #
#                                                                      #
#  (ii)  qfac(a,q,n+k)  ->  qfac(a,q,n) * qfac(a*q^n,q,k)              #
#                                                                      #
#                                                                      #
# (iii)  qfac(a,q,-n)  ->  1 / qfac(a*q^(-n),q,n)                      #
#                                                                      #
########################################################################

`qsimpcomb/simple_transform`:= proc(a,q,nn)
	local n, k, dummy;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	n:= expand(nn);
#	if type(n,`+`) then
#		k:= convert([op(2..nops(n),n)], `+`);
#		n:= op(1,n);
#		procname(a,q,n) * procname(a*q^n,q,k);
#	elif (traperror(sign(subs(I=dummy,n))) = -1) then
#		procname(a*q^n,q,-n)^(-1);
#	else
		`expand/qpochhammer`(a,q,nn);
#	fi;
end:
	
# ======================================================================

qsimpcomb:= proc()
	local f, i, unsub;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;

	f:= convert(args[1], qpochhammer);
	if (`qsum/hasoption`([seq(args[i],i=2..nargs)],assume,unsub)) then
		f:= `qsum/assume+subs`(f,unsub,'unsub');
	else
		unsub:= {};
	fi;

	f:= eval(subs(qpochhammer=`qsimpcomb/simple_transform`, f));
	f:= (`power/expand`@factor@`power/expand`)(f);

	RETURN(subs(unsub,f));
end:

# ----------------------------------------------------------------------

qratio:= proc(f,k,sub)
local rat;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if (nargs = 3) then
		rat:= `power/subs`(sub, qsimpcomb(subs(k=k+1,f)/f));
	else
		rat:= qsimpcomb(subs(k=k+1,f)/f);
	fi;
	RETURN(rat);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
#  Use the following transformations:                                  #
#                                                                      #
#   (i)  qfac(a,1/q,n)  ->  qfac(1/a,q,n) * (-a)^n / q^binomial(n,2)   #
#                                                                      #
#                                                                      #
#  (ii)  qfac(a,q,n+k)  ->  qfac(a,q,n) * qfac(a*q^n,q,k)              #
#                                                                      #
#                                                                      #
# (iii)  qfac(a,q,-n)  ->  1 / qfac(a*q^(-n),q,n)                      #
#                                                                      #
#                                                                      #
#  (iv)  qfac(a*q^(-k*n),q,n)  ->  (-a)^n * q^(binomial(n,2)-k*n^2) *  #
#                                  qfac(q/a,q,k*n) / qfac(q/a,q,k*n-n) #
#                                                                      #
#                                                                      #
#   (v)  qfac(a*q^(k*n),q,n)  ->  qfac(a,q,(k+1)*n) / qfac(a,q,k*n)    #
#                                                                      #
#                                                                      #
#  Use the following rule if in a certain ordering k>n:                #
#                                                                      #
#  (vi)  qfac(a*q^(-n),q,k) -> qfac(a,q,k) * qfac(q/a,q,n) /           #
#                              qfac(q^(1-k)/a,q,n) / q^(n*k)           #
#                                                                      #
########################################################################

`qsimplify/qpochhammer`:= proc()
	local a, q, n, k, j, dummy, qb, qe, qexp;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	a:= `power/combine`(normal(args[1]));
	q:= args[2];
	n:= expand(normal(args[3]));
	if has(n,infinity) or type(n,integer) then
		RETURN(qpochhammer(a,q,n));
	fi;
	if type(q,name^negint) then
      userinfo(5,qsum,`Applying rule (i):`, print(qfac(a,q,n)=\
			qfac(1/a,1/q,n)*(-a)^n*q^binomial(n,2)));
		RETURN((-1)^n*a^n * q^(n^2/2)/q^(n/2) * procname(1/a,1/q,n));
	elif type(n,`+`) then
		k:= convert([op(2..nops(n),n)], `+`);
		n:= op(1,n);
		userinfo(5,qsum,`Applying rule (ii):`,
			print(qfac(a,q,n+k)=qfac(a,q,n)*qfac(a*q^n,q,k)));
		RETURN(procname(a,q,n) * procname(a*q^n,q,k));
	elif (traperror(sign(subs(I=dummy,n))) = -1) then
		userinfo(5,qsum,`Applying rule (iii):`,\
			print(qfac(a,q,n)=1/qfac(a*q^n,q,-n)));
		RETURN(1/procname(a*q^n,q,-n));
	fi;
	if type(q,name^integer) then
		qb:= op(1,q);
		qe:= op(2,q);
	else
		qb:= q;
		qe:= 1;
	fi;
	qexp:= select(type, 'n*k'*a, identical(qb)^anything);
	if ((qexp = 1) or type(op(2,qexp),constant)) then
		RETURN(qpochhammer(a,q,n));
	fi;
	qexp:= expand(op(2,qexp)/qe);
	k:= select(proc(z) if is(z,integer) then true else false fi end,\
		'k'+expand(qexp/n));
	if is(k,negint) then
		j:= `power/combine`(a/q^(n*k));
		userinfo(5,qsum,`Applying rule (iv):`,\
			print(qfac(a,q,n)=(-j)^n*q^(binomial(n,2)+k*n^2)*\
			qfac(q/j,q,-k*n)/qfac(q/j,q,-k*n-n)));
		qexp:= (-j)^n * q^(binomial(n,2)+k*n^2);
		RETURN(qexp * procname(q/j,q,-k*n) / procname(q/j,q,-k*n-n));
	elif is(k,posint) then
		j:= `power/combine`(a/q^(n*k));
		if (j <> 1) then
			userinfo(5,qsum,`Applying rule (v):`,\
				print(qfac(a,q,n)=qfac(j,q,k*n+n)/qfac(j,q,k*n)));
			RETURN(procname(j,q,k*n+n)/procname(j,q,k*n));
		fi;
	fi;
	j:= sort([op(indets([qexp,n],name))]);
	j:= lcoeff(qexp+n,j,'k');
	if type(normal(n/k), numeric) then
		RETURN(qpochhammer(a,q,n));
	fi;
	k:= -j*k;
	a:= `power/combine`(a*q^k);
	userinfo(5,qsum,`Applying rule (vi):`,\
		print(qfac(`power/combine`(a/q^k),q,n)=qfac(a,q,n)\
		*qfac(q/a,q,k)/qfac(`power/combine`(q^(1-n)/a),q,k)/q^(n*k)));
	procname(a,q,n)*procname(q/a,q,k)/procname(q^(1-n)/a,q,k)/q^(n*k);
end:

########################################################################
#                                                                      #
#  Use the following transformation:                                   #
#                                                                      #
# (vii)  qfac(a,a*exp(2*Pi*I/k),...,a*exp(2*Pi*I/k)^(k-1),q,n)  ->     #
#                                                     qfac(a^k,q^k,n)  #
#                                                                      #
########################################################################

`qsimplify/collect/q->q^k/possible`:= proc(a, q, n, k, qpochlist)
	local w, found, arglist, arg, j, r;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	w:= [seq(a*`power/expand`(expand(exp(2*Pi*I/k*j))), j=1..k-1)];
	found:= [seq(false, j=1..k-1)];
	arglist:= select(proc(l,aa,qq,nn) if l[2]=qq and l[3]=nn and\
		type(normal(aa/l[1]),constant) then true else false fi end,\
		qpochlist,a,q,n);
	if (nops(arglist) < k-1) then RETURN(false); fi;
	for arg in arglist while has(found,false) do
		for j from 1 to k-1 do
			r:= normal(arg[1]/w[j]);
			if type(r,{1,identical(q),identical(q)^integer}) or\
				type(radnormal(r),{1,identical(q),identical(q)^integer}) then
				found:= subsop(j=true, found);
			fi;
		od;
	od;
	RETURN(not(has(found,false)));
end:

# ----------------------------------------------------------------------

`qsimplify/collect/q->q^k`:= proc()
	local f, arglist, arg, similar, q, k, a, n, sub, j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	f:= args[1];
	if (denom(f) <> 1) then
		RETURN(procname(numer(f))/procname(denom(f)));
	elif type(f,`+`) then
		RETURN(map(procname,f));
	elif not(type(f,`*`)) then
		RETURN(f);
	fi;
	arglist:= select(type,[op(f)],specfunc(anything,qpochhammer));
	arglist:= map(proc(z) if has(z,infinity) then NULL\
		else [op(z)] fi end, arglist);
	while (arglist <> []) do
		arg:= arglist[1];
		arglist:= subsop(1=NULL, arglist);
		a:= arg[1];
		q:= arg[2];
		n:= arg[3];
		for k from nops(arglist)+1 to 2 by (-1) do
			if `qsimplify/collect/q->q^k/possible`(a,q,n,k,arglist) then
				sub:= [seq(`expand/qpochhammer`\
					(a*expand(exp(2*Pi*I/k*j)),q,n), j=1..k-1)];
				sub:= qpochhammer(a^k,q^k,n) / convert(sub,`*`);
				f:= normal(subs(qpochhammer(op(arg))=sub, f));
				RETURN(procname(f));
			fi;
		od;
	od;
	RETURN(f);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
#  Use the following transformation:                                   #
#                                                                      #
# (viii)   qfac(a,a*q,...,a*q^(k-1),q^k,n)  ->  qfac(a,q,k*n)          #
#                                                                      #
########################################################################

`qsimplify/collect/q^k->kn`:= proc()
	local f, arglist, arg, similar, q, k, a, n, sub, j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	f:= args[1];
	if (denom(f) <> 1) then
		RETURN(procname(numer(f))/procname(denom(f)));
	fi;
	arglist:= [op(indets(f,specfunc(anything,qpochhammer)))];
	arglist:= map(proc(z) if has(z,infinity) then NULL\
		else [op(z)] fi end, arglist);
	arglist:= select(type, arglist, [anything,name^integer,anything]);
	while (arglist <> []) do
		arg:= arglist[1];
		arglist:= subsop(1=NULL, arglist);
		a:= arg[1];
		q:= op(1,arg[2]);
		k:= op(2,arg[2]);
		n:= arg[3];
		similar:= 1;
		for j in arglist while (similar < k) do
			if (j[2] = q^k) and type(normal(j[1]/a),\
					{identical(1),identical(q),identical(q)^integer}) then
				similar:= similar + 1;
			fi;
		od;
		if (similar < k) then next; fi;
		sub:= [seq(`expand/qpochhammer`(a*q^j,q^k,n),j=1..k-1)];
		sub:= `expand/qpochhammer`(a,q,k*n) / convert(sub,`*`);
		f:= normal(subs(qpochhammer(op(arg))=sub, f));
		RETURN(procname(f));
	od;
	RETURN(f);
end:

########################################################################
#                                                                      #
#  Use the following transformation:                                   #
#                                                                      #
#  (xi)  qfac(a,q,infinity)/qfac(a*q^n,infinity) -> qfac(a,q,n)        #
#                                                                      #
########################################################################

`qsimplify/collect/qatoms/argumentlist`:= proc(f)
	local a;
	global qpochhammer;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if type(f,`*`) then a:= [op(f)]; else a:= [f]; fi;
	a:= select(type,a,{specfunc(anything,qpochhammer),\
		specfunc(anything,qpochhammer)^anything});
	a:= map(proc(z) if type(z,`^`) then op(1,z) else z fi end, a);
	map(proc(z) if (op(3,z)=infinity) then [op(z)] else NULL fi end, a);
end:

# ----------------------------------------------------------------------
	
`qsimplify/collect/qatoms`:= proc()
	local f, anum, aden, argnum, argden, q, qe, qpow, a, n, sub;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	f:= factor(eval(subs(qfac=qpochhammer,args[1])));
	anum:= `qsimplify/collect/qatoms/argumentlist`(numer(f));
	aden:= `qsimplify/collect/qatoms/argumentlist`(denom(f));
	while (anum <> []) do
		argnum:= anum[1];
		anum:= subsop(1=NULL, anum);
		if type(argnum[2], name) then
			q:=  argnum[2];
			qe:= 1;
		elif type(argnum[2],name^integer) then
			q:=  op(1,argnum[2]);
			qe:= op(2,argnum[2]);
		else
			next;
		fi;
		for argden in aden do
			if (argnum[2] <> argden[2]) then next; fi;
			a:= normal(`power/expand`(argden[1]/argnum[1]));
			a:= `power/combine`(a);
			qpow:= select(type, 'f*j'*a, identical(q)^anything);
			if type(qpow, constant) then next; fi;
			a:= `power/expand`(argnum[1]);
			f:= f * qpochhammer(op(argden))/qpochhammer(op(argnum));
			f:= f* qpochhammer(a,q^qe,expand(op(2,qpow)/qe));
		od;
	od;
	RETURN(f);
end:

# ----------------------------------------------------------------------

`combine/qpochhammer`:= proc()
	local f;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	f:= convert(args[1], qpochhammer);
	f:= eval(subs(qpochhammer=`expand/qpochhammer`, f));
	f:= normal(`power/expand`(f));
	f:= `qsimplify/collect/qatoms`(f);
	f:= `qsimplify/collect/q->q^k`(f);
	f:= `qsimplify/collect/q^k->kn`(f);
	RETURN(f);
end:

# ======================================================================

qsimplify:= proc()
	local z, tmp, level, zeit, j;
	global qpochhammer;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	zeit:= time();
	if (nargs = 1) then 
		level:= 1;
	else
		level:= args[2];
	fi;
	z:= convert(args[1], qpochhammer);
	zeit:= zeit, time();
	z:= eval(subs(qpochhammer=`qsimplify/qpochhammer`,z));
	zeit:= zeit, time();
	if (_qsum_profile) then
		seq(printf(`   `),j=1..level);
		printf(`Level %2d: %3.1fs  (rules (i)-(vi)).\n`,\
			level, zeit[3]-zeit[2]);
	fi;
	z:= `power/expand`(normal(`power/expand`(z)));
	if has(z, qpochhammer) then
		tmp:= z;
		zeit:= zeit, time();
		z:= `qsimplify/collect/qatoms`(z);
		z:= `qsimplify/collect/q->q^k`(z);
		z:= `qsimplify/collect/q^k->kn`(z);
		zeit:= zeit, time();
		if (_qsum_profile) then
			seq(printf(`   `),j=1..level);
			printf(`Level %2d: %3.1fs  ((vii)-(ix)).\n`,\
				level, zeit[5]-zeit[4]);
		fi;
		if (z <> tmp) then z:= procname(z,level+1); fi;
	fi;
	if (_qsum_profile) then
		seq(printf(`   `),j=1..level);
		printf(`Level %2d: %3.1fs   (total level time).\n`,\
			level, time()-zeit[1]);
	fi;
	RETURN(z);
end:

# ----------------------------------------------------------------------
# 
# readlib(profile)(
# 	`qsimplify/qpochhammer`,
# 	`qsimplify/collect/q->q^k/possible`,
# 	`qsimplify/collect/q->q^k`,
# 	`qsimplify/collect/q^k->kn`,
# 	`qsimplify/collect/qatoms/argumentlist`,
# 	`qsimplify/collect/qatoms`,
# 	`combine/qpochhammer`,
# 	qsimplify
# );
# ======================================================================

`qsumrecursion/profile`:= proc()
local t, lt, pt;
global _qsum_local_solvemethod, _qsum_local_gausselim_normal, _timing;
option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	t:= _timing[qsumrecursion] / 100;
	printf(`%6.0f %%  qratio\n`, _timing[qratio]/t);
	lt:= _timing[`qgosper/update`];
	printf(`%6.0f %%  update, `, lt/t);
	if (lt <> 0) then
		pt:= _timing[`qgosper/dispersionset`] / lt * 100;
		printf(`where %.0f%% dispersionset\n`, pt);
	else
		printf(`\n`);
	fi;
	lt:= _timing[`qgosper/findf`];
	printf(`%6.0f %%  findf,  `, lt/t);
	if (lt <> 0) then
		pt:= _timing[`qgosper/solve`]/t;
		printf(`where %.0f%% total time %s`,pt, _qsum_local_solvemethod);
		if (_qsum_local_solvemethod = gausselim) then
			printf(` (%s)\n`, _qsum_local_gausselim_normal);
		else
			printf(`\n`);
		fi;
	fi;
	pt:= _timing[`qsumrecursion/inhomogeneous_part`] / t;
	if assigned(_timing[`qsumrecursion/inhomogeneous_part`]) and 
		(pt <> 0) then
		printf(`%6.0f %% inhomogenous_part\n`, pt);
	fi;
	pt:= _timing[`recursion/sortq`] / t;
	if type(pt,float) then
		printf(`%6.0f %%  recursion/sortq\n`, pt);
	fi;
	printf(`-------------------------\n`);
	printf(`%6.1f seconds CPU-time\n`, t*100);
end:

# ----------------------------------------------------------------------

`qgosper/profile`:= proc()
local t, lt, pt;
global _timing;
option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	t:= _timing[qgosper] / 100;
	printf(`%6.0f %%  qratio\n`, _timing[qratio]/t);
	lt:= _timing[`qgosper/update`];
	printf(`%6.0f %%  update, `, lt/t);
	if (lt <> 0) then
		pt:= _timing[`qgosper/dispersionset`] / lt * 100;
		printf(`where %.0f%% dispersionset\n`, pt);
	else
		printf(`\n`);
	fi;
	lt:= _timing[`qgosper/findf`];
	printf(`%6.0f %%  findf,  `, lt/t);
	if (lt <> 0) then
		pt:= _timing[`qgosper/solve`]/t;
		if type(pt,float) then
			printf(`where %.0f%% total time %s`,pt, _qsum_local_solvemethod);
			if (_qgosper_local_solvemethod = gausselim) then
				printf(` (%s)\n`, _qsum_local_gausselim_normal);
			else
				printf(`\n`);
			fi;
		else
			printf(`\n`);
		fi;
	fi;
	printf(`-------------------------\n`);
	printf(`%6.1f seconds CPU-time\n`, t*100);
end:

# ----------------------------------------------------------------------


#read `Qzeilberger/qsimpcomb`:
#read `Maple/Utils/power.mpl`;

# ----------------------------------------------------------------------

`sum2qhyper/build_qlist`:= proc(r, K, q, k)
local qrs, rat, qargs, prefac, qlist, p;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	qrs:= select(type,'p*(p+1)'*r,{identical(K),identical(K)^anything});
	rat:= r / qrs;
	qrs:= combine(qrs, power);
	if type(qrs, `^`) then
		qrs:= op(2, qrs);
	elif (qrs = K) then
		qrs:= 1;
	else
		qrs:= 0;
	fi;
	if type(rat,`*`) then
		qargs:= normal([seq(solve(p,K),p=rat)]);
	else
		qargs:= normal([solve(rat, K)]);
	fi;
	prefac:= tcoeff(collect(rat,K,expand), K);
	qlist:= [seq(combine(q/p, power), p=qargs)];
   RETURN([prefac, qlist, qrs]);
end:

 
# ----------------------------------------------------------------------

sum2qhyper:= proc(F, q, k)
local rat, K, shift, num, den, z, qpow, j, r, s, F0, zeit;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	zeit:= time();
	if not type([args], [algebraic,name,name]) then
		ERROR(`Wrong type of arguments.`);
	fi;
	rat:= `power/subs`(q^k=K, qsimpcomb(F/subs(k=k-1,F)));
	rat:= factor(`power/expand`(rat));
	if not type(rat, ratpoly(anything,K)) or has(rat,k) then
		ERROR(`Input term is not hypergeometric wrt. `.q^k);
	fi;
	num:= `sum2qhyper/build_qlist`(numer(rat), K, q, k);
	den:= `sum2qhyper/build_qlist`(denom(rat), K, q, k);
	z:= num[1]/den[1];
	qpow:= num[3] - den[3];
	num:= sort(num[2]);
	den:= sort(den[2]);
	if member(q, den, 'j') then
		den:= subsop(j=NULL, den);
	else
		num:= [op(num),q];
	fi;
	r:= nops(num);
	s:= nops(den);
	qpow:= qpow - (1+s-r);
	if type(qpow, negint) then
		num:= [op(num), (0 $ -qpow)];
		r:= r - qpow;
	elif type(qpow, posint) then
		den:= [op(den), (0 $ qpow)];
		s:= s + qpow;
	fi;
	z:= (-1)^(1+s-r) * q^(1+s-r) * z;
	F0:= simplify(qsimpcomb(subs(k=0, F)));
	if (F0 = 0) then 
		ERROR(`Shift neccessary (not implemented yet).`);
	fi;
	if (_qsum_profile) then printf(`CPU-time: %5.1fs`,time()-zeit); fi;
	RETURN(F0 * phi(num, den, q, z));
end:

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

`qgosper/multibasic/primedispersion/abscoeff`:= proc(p,x)
	if (nops(x) = 1) then
		coeff(p,x[1],0);
	else
		coeff(procname(p,subsop(1=NULL,x)), x[1], 0);
	fi;
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/primedispersion`:= proc(A, B, K, q)
	local a_n, a_0, lmonA, tmonA, b_n, b_0, lmonB, tmonB, i, j, shift;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	a_n:= lcoeff(A,K,'lmonA');
	b_n:= lcoeff(B,K,'lmonB');
#'lmonA'=lmonA, 'lmonB'=lmonB;
	if (lmonA <> lmonB) then
		RETURN(NULL);
	fi;
	a_0:= tcoeff(A,K,'tmonA');
	b_0:= tcoeff(B,K,'tmonB');
#'tmonA'=tmonA, 'tmonB'=tmonB;
	if (tmonA <> tmonB) then
		RETURN(NULL);
	fi;
	j:= normal(a_n/a_0 * b_0/b_n, expanded);
	for i in q do
		if not(type(j,monomial(anything,i))) and\
		   not(type(1/j,monomial(anything,i))) then RETURN(NULL); fi;
	od;
	seq([degree(lmonB,q[i]),degree(tmonB,K[i])],i=1..nops(K));
	seq(degree(lmonB,i)*'shift'=degree(j,i),i=q);
	j:= solve({seq((degree(lmonB,K[i])-degree(tmonB,K[i]))*'shift'=\
		degree(j,q[i]),i=1..nops(K))}, 'shift');
	if (j = NULL) then
		RETURN(NULL);
	fi;
	j:= op([1,2],j);
	if not(type(j,posint) or (j = 0)) then RETURN(NULL); fi;
	b_n:= subs(seq(K[i]=K[i]*q[i]^j,i=1..nops(K)), B);

	# MapleV4
	# b_0:= b_0 * mul(q[i]^(j*degree(tmonB,K[i])),i=1..nops(K));
	# MapleV3
	b_0:= b_0 *\
		convert([seq(q[i]^(j*degree(tmonB,K[i])),i=1..nops(K))],`*`);

	if (expand(b_0*A - a_0*b_n) <> 0) then
		RETURN(NULL);
	fi;
	RETURN(j);
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/dispersion/simple_factorlist`:= proc(p, x)
	local l;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if type(p, `*`) then 
		l:= [op(p)];
	else
		l:= [p];
	fi;
	l:= map(proc(z) if type(z,`^`) then op(1,z) else z fi end, l);
	l:= select(has, l, x);
	RETURN(l);
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/dispersionset`:= proc(P, Q, K, q)
	local A, B, disp, f, g;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	A:= `qgosper/multibasic/dispersion/simple_factorlist`(P,K);
	B:= `qgosper/multibasic/dispersion/simple_factorlist`(Q,K);
	disp:= {seq(seq(`qgosper/multibasic/primedispersion`(f,g,K,q),\
		f=A), g=B)};
	userinfo(3, qsum, `dispersionset= `, disp);
	RETURN(disp);
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/update`:= proc(P, Q, R, K, q)
	local m, Pnew, Qnew, Rnew, disp, g, i, j, l;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	m:= nops(K);
	Pnew:= P;
	Qnew:= Q;
	Rnew:= R;

	disp:= `qgosper/multibasic/dispersionset`(Qnew, Rnew, K, q);
	for j in disp do
		g:= frontend(gcd,[Qnew,subs(seq(K[i]=K[i]*q[i]^j,i=1..m),Rnew)]);
		if has(g,K) then
			Qnew:= normal(Qnew / g);
			g:= unapply(subs(seq(K[l]=K[l]/q[l]^'j',l=1..m),g), 'j');
			Rnew:= normal(Rnew / g(j));

			# MapleV4
			# Pnew:= normal(Pnew * \
			#	mul(subs(seq(K[l]=K[l]/q[l]^i,l=1..m),g), i=0..j-1));
			# MapleV3
			Pnew:= normal(Pnew *\
				convert([seq(g(i), i=1..j)],`*`));

		fi;
	od;
	Pnew:= select(has, 'j*i'*Pnew, K);

	userinfo(3, qsum, `Calculated q-Gosper representation [P,Q,R]:`);
	userinfo(3, qsum, `P=`,print(Pnew)); 
	userinfo(3, qsum, `Q=`,print(Qnew));
	userinfo(3, qsum, `R=`,print(Rnew));

	RETURN([Pnew, Qnew, Rnew]);
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/multidegree`:= proc(p, x, lc)
	local multideg, i;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if (nargs = 2) then
		lcoeff(p, x, 'multideg');
	else
		lc:= lcoeff(p, x, 'multideg');
	fi;
	multideg:= [seq(degree(multideg,i), i=x)];
	RETURN(multideg);
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/multidegree/<`:= proc(d1, d2)
	local n, x, m1, m2, i;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	n:= nops(d1);

	# MapleV4
	# m1:= mul(x[i]^d1[i], i=1..n);
	# m2:= mul(x[i]^d2[i], i=1..n);
	# MapleV3
	m1:= convert([seq(x[i]^d1[i], i=1..n)], `*`);
	m2:= convert([seq(x[i]^d2[i], i=1..n)], `*`);

	if (lcoeff(m1+m2,[seq(x[i],i=1..n)], 'n') <> 1) then
		false;
	elif (n = d2) then
		true;
	else 
		false;
	fi;
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/qmonfree`:= proc(expr, q, q_list)
	local f, mod_factor;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	# build expr mod qlist...
	f:= factor(expr);
	if type(f,`*`) then f:= [op(f)]; else f:= [f]; fi;
	mod_factor:= select(proc(z,v) (type(z,name^integer) or type(z,name))\
		and has(z,v) end, f, q_list);
	# now test if expr=q^anything and return anything if true, otherwise FAIL
	f:= combine(convert(f,`*`)/convert(mod_factor,`*`), power);
	if type(f, identical(q)^anything) then
		op(2,f);
	elif (f = q) then
		1;
	elif (f = 1) then
		0;
	else 
		FAIL;
	fi;
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/degreebound/single`:= proc(P,Q,R,K,q,i,\
		deg_func,coeff_func,max_func)
	local j, dP, dQ, dR, degX, ldegX, alpha, beta,\
			alpha0, beta0, z;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	dP:= frontend(deg_func, [P, K[i]]);
	dQ:= frontend(deg_func, [Q, K[i]]);
	dR:= frontend(deg_func, [R, K[i]]);
	if (dP <> dQ) then
		degX[i]:= dP - max_func(dQ, dR);
	else
		alpha:= coeff_func(Q, K[i]);
		beta:= coeff_func(R, K[i]);
		alpha:= `qgosper/multibasic/multidegree`\
			(alpha,subsop(i=NULL,K),'alpha0');
		beta:= `qgosper/multibasic/multidegree`\
			(beta,subsop(i=NULL,K),'beta0');
		if (alpha <> beta) then
			degX[i]:= dP - dQ;
		else
'alpha0'=alpha0, 'beta0'=beta0;
			z:= `qgosper/multibasic/qmonfree`(beta0/alpha0, q[i], subsop(i=NULL,q));
			if type(z,integer) then
				degX[i]:= max_func(dP,z) - dQ;
			else
				degX[i]:= dP - dQ;
			fi;
		fi;
	fi;
	RETURN(degX[i]);
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/degreebound`:= proc(P, Q, R, K, q)
	local degX, i;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	degX:= [seq(`qgosper/multibasic/degreebound/single`\
		(P,Q,R,K,q,i,ldegree,tcoeff,min)..\
		`qgosper/multibasic/degreebound/single`\
		(P,Q,R,K,q,i,degree,lcoeff,max), i=1..nops(K))];
	
#	degX:= [seq(-1..2, i=K)];
	userinfo(3,qsum,`degreebound for generic polynomial:`,print(degX));

	for i in degX do
		if (op(2,i) < op(1,i)) then RETURN(NULL); fi;
	od;
	RETURN(degX);
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/generic_polynomial`:= proc(a, k, deg)
	local j, i;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if (nargs = 3) then j:= NULL; else j:= args[4..nargs]; fi;
	if (nops(k) = 1) then
		# MapleV4:
		# add(a[j,i]*k[1]^i, i=deg[1]);
		convert([seq(a[j,i]*k[1]^i, i=deg[1])],`+`);
	else
		# MapleV4:
		# expand(add(procname(a,subsop(1=NULL,k),\
		#	subsop(1=NULL,deg),j,i)*k[1]^i, i=deg[1]));
		expand(convert([seq(procname(a,subsop(1=NULL,k),\
			subsop(1=NULL,deg),j,i)*k[1]^i, i=deg[1])], `+`));
	fi;
end:  # `multisum/generic_polynomial`(a, k, deg)

# ----------------------------------------------------------------------

`qgosper/multibasic/solve`:= proc(eqn, var, sigma_var)
	gausselim({op(eqn)}, {op(var)});
end:

# ----------------------------------------------------------------------

`qgosper/multibasic/findf`:= proc(P, Q, R, K, q, sigma_var, sigma_sol)
local PQR, deg, vars, F, a, i, eqn, sol;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	PQR:= collect(P,K,distributed), collect(Q,K,distributed),\
		collect(R,K,distributed);
	deg:= `qgosper/multibasic/degreebound`(PQR, K, q);
	if (deg = NULL) then 
		if (nargs = 7) then sigma_sol:= NULL; fi;
		RETURN(NULL) 
	fi;
	F:= `qgosper/multibasic/generic_polynomial`(a,K,deg);
	vars:= [op(indets(F,indexed) minus {op(K)})];
	eqn:= PQR[2]*F - \
		subs(seq(K[i]=K[i]/q[i],i=1..nops(K)),PQR[3]*F) - PQR[1];
	eqn:= {coeffs(collect(eqn,K,distributed), K)};
	if (nargs = 5) then 
		sol:= `qgosper/multibasic/solve`(eqn, vars); 
	else
		vars:= [op(sigma_var), op(vars)]; 
		sol:= `qgosper/multibasic/solve`(eqn, vars, sigma_var); 
	fi;
	if (sol = NULL) then
		if (nargs = 7) then sigma_sol:= NULL; fi;
		RETURN(NULL);
	fi;
	F:= subs(seq(i=0,i=vars), subs(sol, F));
	if (nargs = 7) then 
		sigma_sol:= select(has, sol, sigma_var);
	fi;
	RETURN(F);
end:

# ----------------------------------------------------------------------

`qgosper/multibasic`:= proc(F,q,k,sumrange)
	local m, rat, i, PQR, X, qK, K, zeit;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	zeit:= time();

	m:= nops(q);
	qK:= [seq(K[i],i=1..m)];
	rat:= factor(qratio(F,k,{seq(q[i]^k=K[i],i=1..m)}));
	if has(rat,k) or has([seq(type(rat,ratpoly(anything,i)),i=qK)],\
			false) then
		rat:= normal(rat,expanded);
		if has(rat,k) or has([seq(type(rat,ratpoly(anything,i)),i=qK)],\
				false) then
			ERROR(`Input term is not hypergeometric.`);
		else
			rat:= factor(rat);
		fi;
	fi;

	userinfo(3, qsum, `applied substitutions`,\
		print(seq(q[i]^k=K[i],i=1..m)));
	userinfo(3, qsum, `down `,q,`-ratio wrt. `, k, `:`, print(rat));

   PQR:= `qgosper/multibasic/update`(1,numer(rat),denom(rat),qK,q);
   X:= `qgosper/multibasic/findf`(op(PQR), qK, q);
   if (X = NULL) then
      ERROR(`No q-hypergeometric antidifference exists.`);
   fi;

	if (_qgosper_local_antidifference = down) then # Gosper downwards
		X:= subs(seq(K[i]=q[i]^k,i=1..m), PQR[2]*X/PQR[1]) * F;
		if (nargs > 3) then
			X:= subs(k=op(2,sumrange),X)-subs(k=op(1,sumrange)-1,X);
		fi;
	else # Gosper upwards:
		X:= subs(seq(K[i]=q[i]^(k-1),i=1..m), PQR[3]*X)/\
			subs(seq(K[i]=q[i]^k,i=1..m), PQR[1]) * F;
		if (nargs > 3) then
			X:= subs(k=op(2,sumrange)+1,X)-subs(k=op(1,sumrange),X);
		fi;
	fi;
	X:= _qgosper_local_simplify(X);

	if (_qsum_profile=true) then
		printf(`CPU-time: %6.1f\n`,time()-zeit);
	fi;
   RETURN(X);
end:

# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
#  `rat2qhyper/poly2list`:                                             #
#                                                                      #
#    Input: poly, a polynomial in N, that HAS TO BE factored!          #
#                                                                      #
#    Output: a list where all nonconstant and nonmonomial factors are  #
#            represented by a list                                     #
#                                                                      #
#                  (a*N^d+b)^m  ->  [a,d,b,m]                          #
#                                                                      #
#            All constant factors are collected in CONST and the sum   #
#            of all the exponents of monomials N^posint in MONEXP.     #
#            Whenever there is a factor that is not of the form above, #
#            this factor is split up into linear factors via solve.    #
#                                                                      #
########################################################################

`rat2qhyper/poly2list`:= proc(poly,N,CONST,MONEXP)
	local p, const, monom, other, j, i, a, zerolist;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if type(poly,`*`) then p:= [op(poly)]; else p:= [poly]; fi;
	p:= map(proc(z) if type(z,anything^integer) then\
		[op(z)] else [z,1] fi end, p);
	const:= 1;
	monom:= 0;
	other:= NULL;
	for j in p do
		a:= [lcoeff(j[1],N), degree(j[1],N), coeff(j[1],N,0), j[2]];
		# check if j is a constant factor:
		if (a[2] = 0) then
			const:= const * a[3]^j[2];
		# check if j is a monomial, i.e. N^posint:
		elif (a[2] = 1) and (a[3] = 0) then
			monom:= monom + a[2]*j[2];
		else
			if (expand(a[1]*N^a[2]+a[3]-j[1]) = 0) then
				other:= other, a;
				const:= const * a[3];
			else
				const:= const * lcoeff(j[1],N)^j[2];
				zerolist:= normal([solve(j[1],N)],expanded);
				other:= other, seq([1,1,i,j[2]],i=zerolist);
			fi;
		fi;
	od;
	CONST:= const;
	MONEXP:= monom;
	[other];
end:
			
# ----------------------------------------------------------------------
########################################################################
#                                                                      #
#  `rat2qhyper/shift`:                                                 #
#                                                                      #
#    Input: a list of lists as returned by `rat2qhyper/poly2list`,     #
#           representing a polynomial P(N)                             #
#                                                                      #
#    Output: an integer n, such that                                   #
#                                                                      #
#                  P(q^k)<>0, for all integer k>n   and   P(q^n)=0     #
#            OR                                                        #
#                  n=-1, if P(q^k)<>0 for all nonnegative integer k.   #
#                                                                      #
#    As the list contains polynomial factors                           #
#                                                                      #
#                [a,d,b,m]   ->  (a*N^d+b)^m                           #
#                                                                      #
#    we use the formula (a*N^d+b)^m=0, where N=q^k iff                 #
#                                                                      #
#      k     / -b \ (1/d)                                              #
#     q   =  | -- |            <==>    k = ln[q]((-b/a)^(1/d))         #
#            \  a /                                                    #
#                                                                      #
########################################################################

`rat2qhyper/shift`:= proc(plist,q)
	local shift, j, s;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	shift:= -1;
	for j in plist do
		s:= combine(normal(-j[3]/j[1],expanded)^(1/j[2]), power);
		if type(s, 1) then
			shift:= max(shift, 0);
		elif type(s, identical(q)) then
			shift:= max(shift, 1);
		elif type(s, identical(q)^posint) then
			shift:= max(shift, op(2,s));
		fi;
	od;
	RETURN(shift+1);
end:


# ----------------------------------------------------------------------
########################################################################
#                                                                      #
#  `rat2qhyper/ratio`: Use the formula                                  #
#                                                                      #
#                         d                         n                  #
#          f(n + 1) = (a N  + b) f(n),   where N = q                   #
#                                                                      #
#                                           (d s)                      #
#                         (n - s)        a q        d                  #
#     ==>         f(n) = b        qfac(- --------, q , n) f(s)         #
#                                           b                          #
#                                                                      #
#   Input: rat, a rational function in N=q^n, that should be factored  #
#                                                                      #
#   Output: a list with first entry a qhypergeometric term f(n), such  #
#           that f(n+1)/f(n)=qpoly. Second entry is a restriction on   #
#           n, of the form n>=posint.                                  #
#                                                                      #
#                                                                      #
#   ASSUMPTION: n is a nonnegative integer and f is a welldefined      #
#               sequence f(0),f(1),f(2),f(3),...                       #
#                                                                      #
########################################################################

`rat2qhyper/ratio`:= proc(Fratio,N,q,n)
	local rat,numlist,denlist,numC,numN,denC,denN,j,shift;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	rat:= factor(Fratio);
	if type(rat,{set,list}) then 
		RETURN(map(procname, args));
	fi;
	numlist:= `rat2qhyper/poly2list`(numer(rat),N,'numC','numN');
	denlist:= `rat2qhyper/poly2list`(denom(rat),N,'denC','denN');
	shift:=  `rat2qhyper/shift`(denlist,q);
	numlist:= convert([seq(qpochhammer(-j[1]/j[3]*q^(j[2]*shift),\
		q^j[2],n-shift)^j[4], j=numlist)],`*`);
	denlist:= convert([seq(qpochhammer(-j[1]/j[3]*q^(j[2]*shift),\
		q^j[2],n-shift)^j[4], j=denlist)],`*`);
	j:= (numC/denC)^(n) * q^((numN-denN)*binomial(n,2));
	RETURN([numlist/denlist*j,n>=shift]);
end:
	
# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `rat2qhyper/singularity`:                                            #
#                                                                      #
#   Input: poly a polynomial in K, q                                   #
#                                                                      #
#   Output: a set with all values k such that q^k is a root of poly    #
#                                                                      #
########################################################################

`rat2qhyper/singularity`:= proc(poly,K,q)
	local plist, const, monexp, shift, j, s;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	plist:= `rat2qhyper/poly2list`(poly,K,'const','monexp');
	shift:= NULL;
	for j in plist do
		s:= combine(normal(-j[3]/j[1],expanded)^(1/j[2]), power);
		if type(s, 1) then
			shift:= shift, 0;
		elif type(s, identical(q)) then
			shift:= shift, 1;
		elif type(s, identical(q)^anything) then
			shift:= shift, op(2,s);
		fi;
	od;
	RETURN({shift});
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qsum/assume+subs`: substitute some variables in an expression by    #
#                     assumed variables                                #
#                                                                      #
#   Input: z an expression, var a list with variables and types as     #
#          [varname1,type1,varname2,type2,...]                         #
#          unsubs: a name, where the 'unsubstitute information' is     #
#          stored.                                                     #
#                                                                      #
#   Output: z, where all variables in varlist are replaced by local    #
#           typed variables. unsubs contains a list with equations     #
#           to restore the original expression z.                      #
#                                                                      #
########################################################################

`qsum/assume+subs`:= proc(z, var, unsub)
	local i, sub;
	sub:= {seq(var[2*i-1]=(proc(a) local z; additionally(z,a); z end)\
		(var[2*i]), i=1..nops(var)/2)};
	unsub:= {seq(op(2,i)=op(1,i),i=sub)};
	subs(sub,z);
end:

# ======================================================================
########################################################################
#                                                                      #
# `rat2qhyper/bounds`: Try to determine the natural summations bounds  #
#                     of a q-hypergeometric summand                    #
#                                                                      #
#   Input: rat a rational function in K, a ratio of a q-hypergeometric #
#          term f(k) in q^k.                                           #
#                                                                      #
#   Output: a range l..h, such that the q-hypergeometric term f(k) is  #
#           zero outside the interval [l,h].                           #
#                                                                      #
#   Note that this procedure uses the assume function, thus any known  #
#   assumptions should be made or specified as a 4-th argument:        #
#   a list containing a sequence like var_name, var_type.              #
#                                                                      #
########################################################################

`rat2qhyper/bounds`:= proc(rat,K,q,assumptions)
	local num, den, j, unsubnum, unsubden;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	num:= `rat2qhyper/singularity`(numer(rat),K,q);
	den:= `rat2qhyper/singularity`(denom(rat),K,q);
	num:= `qsum/assume+subs`(num,assumptions,'unsubnum');
	den:= `qsum/assume+subs`(den,assumptions,'unsubden');
	num:= select(proc(z) if is(z,integer) then true else false fi end,\
		num);
	den:= select(proc(z) if is(z,integer) then true else false fi end,\
		den);
	num:= max(op(num));
	den:= min(seq(j+1,j=den));
	if has([num,den],max) then
		-infinity..infinity;
	elif is(den < num) then
		subs(unsubden,den)..subs(unsubnum,num);
	elif (num = -infinity) then
		subs(unsubden,den)..infinity;
	elif (den = infinity) then
		-infinity..subs(unsubnum,num);
	else
		-infinity..infinity;
	fi;
end:

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qsumrecursion/rec2qhyper`: Convert a first order recurrence into a  #
#                          q-hypergeometric term with appropiate       #
#                          initial conditions...                       #
#                          (i.e. the solution should be equal to       #
#                          sum(f,k=sumrange) for all valid n.)         #
#                                                                      #
#   Input: a recurrence equation that should be equivalent to a sum    #
#          f,k,sumrange, i.e. sum(f,k=sumrange).                        #
#                                                                      #
#   Output: if the recurrence equation is of first order:              #
#             a list with two entries, the first one is                #
#             a q-hypergeometric term F(n), such that for all valid n  #
#                         sum(f,k=sumrange) = F(n),                    #
#             and the second entry is of the form n>=posint.           #
#                                                                      #
#           otherwise the recurrence equation is returned (without     #
#           any changes).                                              #
#                                                                      #
########################################################################

`qsumrecursion/rec2qhyper`:= proc(f,k,n,N,q,rec,S,sumrange,unsub)
	local arg, rat, range, singul, poles, lo, hi, j, F, F0, restriction;
	arg:= op(map(op, indets(rec,specfunc(anything,S))));
	lo:= min(arg);
	hi:= max(arg);
	if (lo - hi > 1) or (_qsumrecursion_local_rec2qhyper=false) then
		RETURN(`recursion/sortq`(rec=0, S, N, unsub));
	fi;
	rat:= -coeff(rec,S(lo)) / coeff(rec,S(hi));
	rat:= `power/subs`(q^n=N, subs(unsub,rat));
	rat:= subs(n=2*n-lo, N=N/q^(lo-n), rat);
	F:= `rat2qhyper/ratio`(rat,N,q,n);
	restriction:= F[2];
	F:= F[1];
	# Check if `rat2qhyper/ratio` still delivers what we expect...
	if not(type(restriction,name>=integer)) then
		printf(`Wrong format in %s encountered.`,procname);
		printf(`Please contact W. Koepf, koepf@zib.de.`);
		ERROR(`Code-BUG!`);
	else
		F0:= op(indets(restriction,integer));
	fi;
	range:= subs(n=F0,sumrange);
	j:= expand(op(2,range)-op(1,range));
	if type(j, integer) then
# 1.3 sum -> add
#		F0:= sum('qsimpcomb(subs(k=j,n=F0,f))','j'=range);
		F0:= add(qsimpcomb(subs(k=j,n=F0,f)),j=range);
	elif (nops(indets(j,name))=1) and is(j,OrProp(negint,0)) then
		j:= op(indets(j,name));
		F0:= qsumrecursion(subs(n=F0,f),q,k=range,S(j),rec2qhyper=true);
		if type(F0,list) then
			F0:= op(2,F0[1]);
		fi;
		if has(F0,S) then
			F0:= S(op(indets(restriction,integer)));
		fi;
	else
		F0:= S(F0);
	fi;
	F:= factor(qsimpcomb(F0)*F);
	RETURN([subs(unsub,rec)=0,S(n)=F,restriction]);
end:

# ----------------------------------------------------------------------
proc(f,k,K,n,N,q,rec,S,stamm,sumrange)
	local singul, poles, j, range, lo;
	# Now determine singularities of the antiderivative...
	singul:= `power/subs`(q^k=K,denom(stamm));
	singul:= `rat2qhyper/singularity`(singul,K,q);
	# ...and check if any of those singularities ly in the summation
	# range:
	poles:= [];
	for j in singul while not(has(poles,true)) do
		lo:= evalb( (j<op(1,range) or is(j<op(1,range))) and\
						(j>op(2,range) or is(j>op(2,range))));
		poles:= [op(poles), lo];
	od;
	if has(poles,true) then
		ERROR(`Poles encountered: Cannot sum recurrence.`);
	elif has(poles,FAIL) then
		printf(`WARNING: Poles encountered. Formula may be invalid!`);
	fi;
end:

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qsum/var->_Ci`: replace all variables of the set v in the given     #
#                  expression z by the global variables _C1,_C2,...    #
#   similar to the output of some builtin maple functions that use     #
#   these symbols for arbitrary constants.                             #
#                                                                      #
########################################################################

`qsum/var->_Ci`:= proc(z, v)
	local inds, i, var;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	var:= select(proc(x) not(type(x,constant)) end, v);
	inds:= indets(z, {seq(identical(i),i=var)});
	if (_qsum_local_specialsolution) then
		subs({seq(i=0, i=inds)}, z);
	else
		subs({seq(inds[i]=cat(`_C`,i), i=1..nops(inds))}, z);
	fi;
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/polynomial/degreebound`: calculate all q^i such that the  #
#                                     given polynomial poly in x       #
#   fulfils the equation:   poly(q^i)=0, i integer                     #
#   Return the minimum or maximum value (which function should be      #
#   applied is specified in maxfunc) of all those i, i.e. if none      #
#   such exists -infinity.                                             #
#                                                                      #
########################################################################

`qrecsolve/polynomial/degreebound`:= proc(poly,x,q,maxfunc)
	local max_root, root_list, i;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	root_list:= `qrecsolve/solve`(poly, x);
	if (maxfunc = max) then
		max_root:= -infinity;
	else
		max_root:= infinity;
	fi;
	for i in root_list do
		if (i = 1) then
			max_root:= maxfunc(max_root, 0);
		elif (i = q) then
			max_root:= maxfunc(max_root, 1);
		elif type(i,name^integer) and (op(1,i) = q) then
			max_root:= maxfunc(max_root, op(2,i));
		fi;
	od;
	RETURN(max_root);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/polynomial`: Search for all laurent polynomial solutions  #
#                         of the given recurrence req, that is a table #
#      of all coefficients, i.e. search for a finite laurent           #
#      polynomial X(N) (finite) with                                   #
#   req[0]*X(N) + req[1]*X(N*q) + ... + req[hi]*X(N*q^hi) = inhomo.    #
#      The last parameter const is a name, that is used to build the   #
#      coefficients of the polynomial X(N), X(N)=sum(const[i]*N^i,i),  #
#      so that if any coefficients remain 'symbolic' they may be       #
#      substituted later.                                              #
#                                                                      #
########################################################################

`qrecsolve/polynomial`:= proc(req, inhomo, N, hi, const)
	local j, co, deg, d, x, poly, var, B;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	co:= [seq(expand(req[j]), j=0..hi)];
	# determine min(ldegree) and max(degree) of all coefficients
	deg:= [min(seq(ldegree(j,N), j=co)), max(seq(degree(j,N), j=co))];
   # build 'lower and upper' polynomial necessary for degreebound
	poly:= convert([seq(coeff(co[j+1],N,deg[2])*x^j,j=0..hi)], `+`);
	d:= `qrecsolve/polynomial/degreebound`(poly,x,q,max);
	if (B <> 0) then
		B:= expand(inhomo);
		d:= max(d,degree(B,N)-deg[2]);
	fi;
	if (d = -infinity) then
		userinfo(5,qsum,`No polynomial solution as upper degreebound equal`, d);
		RETURN(NULL)
	fi;
	poly:= convert([seq(coeff(co[j+1],N,deg[1])*x^j,j=0..hi)], `+`);
	d:= [`qrecsolve/polynomial/degreebound`(poly,x,q,min), d];
	if (B <> 0) then
		d:= [min(d[1],ldegree(B,N)-deg[1]), d[2]];
	fi;
	if (d[1] > d[2]) then
		userinfo(5,qsum,`No polynomial solution as degreebound equal`, d);
		RETURN(NULL)
	fi;
	userinfo(5,qsum,`Degreebound for polynomial:`, d);
	poly:= unapply(convert([seq(const[j]*N^j*q^(j*'j'),\
		j=d[1]..d[2])], `+`), 'j');
	var:= seq(const[j], j=d[1]..d[2]);
	co:= convert([seq(req[j]*poly(j),j=0..hi)], `+`) - B;
#	co:= coeffs(collect(co, [N,var], distributed, expand), N);
   co:= coeffs(expand(co), N);
	co:= `qrecsolve/solve`({co}, {var});
	if (co = {}) then RETURN(NULL) fi;
	poly:= factor(subs(co, poly(0)));
	if (poly = 0) then
		userinfo(5,qsum,`No polynomial solutions exist.`);
		RETURN(NULL)
	fi;
	userinfo(5,qsum,`Found polynomial solution:`,print(poly));
	RETURN(poly);
end:
	
# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/rational/denom`: Use algorithm of S. Abramov to           #
#                             calculate the worst possible denom       #
#   for a inhomogeneous recurrence equation.                           #
#   Return this denominator P(N) as a function, i.e. a polynomial in N #
#                                                                      #
#                       i -> P(N*q^i)                                  #
#                                                                      #
########################################################################

`qrecsolve/rational/denom`:= proc(AA,BB,q,N)
	local A, B, i, j, d, P, G;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	A:= select(has,'i*j'*factor(AA),N);
	B:= select(has,'i*j'*factor(BB),N);
	# remove any monomial Factors N^integer...
	# note the select of `*` returns 1 if nothing is found, thus the -1!
	A:= A / N^(select(type,'i*j'*A,identical(N)^integer)-1);
	B:= B / N^(select(type,'i*j'*B,identical(N)^integer)-1);
	d:= max(op(`qgosper/dispersionset`(A,B,q,N)));
	if (d = -infinity) then RETURN(1) fi;
	P:= 1;
	for i from d to 0 by -1 do
		G:= frontend(gcd, [A,subs(N=N*q^i,B)]);
		if has(G,N) then
			G:= unapply(G,N);
			A:= normal(A/G(N));
			B:= normal(B/G(N/q^i));
			P:= P * convert([seq(G(N/q^j),j=0..i)], `*`);
		fi;
	od;
	P:= unapply(subs(N=N*q^'i',P), 'i');
	RETURN(P);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/rational`: Search for all rational solutions of the       #
#                       given recurrence req, that is a table of all   #
#    coefficients, i.e. search for a rational function R(N) with       #
# req[0]*R(N) + req[1]*R(N*q) + ... + req[hi]*R(N*q^hi) = inhomo,      #
#    by using calculating a worst possible denominator, substituting   #
#    this denominator into the recurrence and then search for all      #
#    polynomial solutions.                                             #
#    The last parameter const is a name, that is used to build the     #
#    coefficients of the numerator polynomial of the rational function #
#    R(N), so that if any coefficients remain 'symbolic' they may be   #
#    substituted later.                                                #
#                                                                      #
########################################################################

`qrecsolve/rational`:= proc(req,inhomo,q,N,hi,const)
	local P, den, j, rec, X;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	# determine a worst denominator
	P:= `qrecsolve/rational/denom`(subs(N=N/q^hi,req[hi]),req[0],q,N);
'P'=eval(P);
	userinfo(2,qsum,`Possible denominator:`,print('P(N)'=P(0)));
	# Now substitute this denominator into the recurrence and normalize
	# it, such that the coefficients remain polynomials
	den:= `recursion/denom/lcm`(seq(P(j),j=0..hi));
	rec:= table([seq(j=normal(req[j]/P(j)*den),j=0..hi)]);
	userinfo(2,qsum,`Recurrence equation for numerator X(N):`,\
		print(sum('rec[j]*X(N*q^j)','j'=0..hi)=den*inhomo));
	X:= `qrecsolve/polynomial`(rec,den*inhomo,N,hi,const);
	if (X <> NULL) then
		X:= factor(X/P(0));
	fi;
	RETURN(X);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/factorlist/generateproducts`: Given the list a, the       #
#                                          procedure returns a list    #
#   containing all combinations of members of a as a products.         #
#                                                                      #
########################################################################

`qrecsolve/factorlist/generateproducts`:= proc(a)
local f,r,n;
options `Copyright 1993 by Waterloo Maple Software`;
# slightly changed `combinat/powerset/multiset`...
	r := [a];
	f := proc(a,x) local p,t;
				if member(x,a,p) then t:=subsop(p=NULL,a); procname(t,x),t fi
			end;
	n := nops(a);
	while (0 < n) do
		r := [op(map(f,r,a[n])), op(r)];
		n := n-1;
		while (0 < n) and (a[n+1] = a[n]) do  n:= n-1; od;
	od;
	RETURN(map(convert, r, `*`));
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/factorlist`: Given the polynomial poly in x, return all   #
#                         possible combinations of factors of          #
#           poly(x)=(x-c[1])*(x-c[2])*...*(x-c[n])                     #
#                                                                      #
########################################################################

`qrecsolve/factorlist`:= proc(poly, x)
	local p, j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if (_qrecsolve_local_split) then
		# ACHTUNG: Es werden hier explizit mehrfache Werte ben"otigt.
		# Deshalb `qrecsolve/solve` mit 3. dummy argument aufrufen.
		p:= `qrecsolve/solve`(poly,x,'return_list');
		p:= [seq(x-j, j=p)];
	else
		p:= factor(poly);
		if type(p,`*`) then p:= [op(p)]; else p:= [p]; fi;
	fi;
	p:= map(numer@factor, select(has,p,x));
	p:= `qrecsolve/factorlist/generateproducts`(p);
	userinfo(3,qsum,print(p));
	map(expand, p);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/solve`: Solve the given system of equations gls for all   #
#                    variables in var.                                 #
#                                                                      #
# NOTE: if the second variable is a name and three arguments were      #
#       passed to the procedure we return a LIST instead of a SET!!!   #
#                                                                      #
#       Thus if you wish to obtain the multiplicities of the roots     #
#       as well, you should supply a third dummy-argument!             #
#                                                                      #
########################################################################

`qrecsolve/solve`:= proc(gls, var)
	local _MaxSols, 
# Maple 6
# _EnvExplicit, 
# 
	sol, unsub, j;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	sol:= `power/expand`(gls, root=eliminate, unsubs='unsub');
	if type(var, name) then
		_EnvExplicit:= true;
		_MaxSols:= infinity;
		sol:= factor(sol);
		if type(sol,`*`) then sol:= [op(sol)]; else sol:= [sol]; fi;
		sol:= map(proc(t) if type(t,anything^posint) then\
			[op(t)] else [t,1] fi end, sol);
		sol:= select(has,sol,var);
		if (nargs = 3) then
			sol:= [seq('op'([solve(j[1],var)])$j[2],j=sol)];
		else
			sol:= {seq(solve(j[1],var),j=sol)};
		fi;
	else
		sol:= gausselim(sol, var);
		if (sol = NULL) then sol:= {}; fi;
	fi;
	`power/root/restore`(sol, unsub);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/normalform`: Given a recurrence in F(n) standardize this  #
#                         recurrence to an homogeneneous upward,       #
#    recurrence, i.e. starting with  F(n),F(n+1),...                   #
#    with polynomial coefficients and return coefficients in a list    #
#    table in REC:   [0=coeff(req,F(n)),1=coeff(req,F(n+1)),...];      #
#    Store inhomogeneous part in parameter INHOMO, recursion order in  #
#    HI.                                                               #
#                                                                      #
########################################################################

`qrecsolve/normalform`:= proc(req,F,q,n,N,REC,INHOMO,HI)
	local rec, Finds, inhomo, i, lo, hi, den;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	if type(req, `=`) then
		rec:= `power/expand`(op(1,req) - op(2,req));
	else
		rec:= `power/expand`(req);
	fi;
	# substitute F(n+i) by F(i) in rec
	rec:= eval(subs(F=unapply(F('i'-n),'i'), rec));
	Finds:= op(map(op, indets(rec,specfunc(anything,F))));
	# determine inhomogeneous part
	inhomo:= subs(seq(F(i)=0,i=Finds),rec);
	rec:= `power/subs`(q^n=N, rec - inhomo);
	lo:= min(Finds);
	hi:= max(Finds);
	# shift recurrence, so it becomes an upward recurrence
	# starting with F(n) and put coefficients in a list
	rec:= [seq(subs(N=N/q^lo,coeff(rec,F(i))),i=lo..hi)];
	inhomo:= `power/subs`(q^n=N,qsimplify(-subs(n=n-lo,inhomo)));
	if not(type(inhomo,ratpoly(anything,N))) and not(has(inhomo,n)) then
		inhomo:= normal(inhomo,expanded);
	fi;
	hi:= hi-lo;
	# Determine denominator of coefficients and of the inhomogeneous
	# part (only if RATIONAL) and multiply recurrence with this denom
	if type(inhomo,ratpoly(anything,N)) and not(has(inhomo,n)) then
		den:= `recursion/denom/lcm`(seq(denom(rec[i]),i=1..hi+1),\
			denom(inhomo));
	elif member(_qrecsolve_local_solution,{rational,polynomial}) then
		ERROR(`Inhomogeneous part is not rational in `.q.`^`.n.`.`);
	else
		den:= `recursion/denom/lcm`(seq(denom(rec[i]),i=1..hi+1));
	fi;
	HI:= hi;
	INHOMO:= normal(den*inhomo);
	REC:= table([seq(i=normal(den*rec[i+1]),i=0..hi)]);
	userinfo(2,qsum,`Standardized input recurrence equation to:`,\
		print(sum('eval(REC)[i]*F(n+i)','i'=0..hi)=eval(INHOMO)));
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/qhypergeometric/inhomogeneous`: given the recurrence req  #
#                                            as a table of the         #
#    coefficients search for all qhypergeometric solutions F(n) of the #
#    inhomogeneous recurrence equation                                 #
#  req[0]*F(n) + req[1]*F(n+1) + ... + req[hi]*F(n+hi) = inhomo,       #
#    where inhomo<>0.                                                  #
#    As any solution has to be a rational multiple L(n) of inhomo,     #
#    we build the corresponding recurrence equation for L(n) and solve #
#    this one.                                                         #
#                                                                      #
########################################################################

`qrecsolve/qhypergeometric/inhomogeneous`:= proc(\
		req,inhomo,F,q,n,N,hi,const)
	local R, dummy, i, j, rec, den;
	global _qrecsolve_local_solution;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	# determine qratio of inhomogeneous part
	R:= qratio(subs(N=q^n,inhomo),n,{q^n=N});
	if not(type(R,ratpoly(anything,N))) then
		R:= normal(R,expanded);
	fi;
	if not(type(R,ratpoly(anything,N))) or has(R,n) then
		ERROR(`Inhomogeneous part is not `.q.`-hypergeometric.`);
	fi;
	R:= unapply('product'(subs(N=N*q^''i'',R),''i''=0..'j'-1),'j');
	# determine common denominator of new recurrence
	den:= denom(R(hi));
	# build new recurrence for rational function L(n)
	rec:= table([seq(normal(j=req[j]*R(j)*den),j=0..hi)]);
	R:= `qrecsolve/rational`(rec,den,q,N,hi,const);
	if (R = NULL) then RETURN(NULL); fi;
	_qrecsolve_local_solution:= rational;
	factor(R*inhomo);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/qhypergeometric/zchoices`: Calculate all choices for z    #
#                                       from the recurrence given      #
#    by already specified values for Q and R. Eliminate all 'double'   #
#    entries, i.e. say                                                 #
#                                                                      #
#              x~y  <=>  x/y = q^integer                               #
#                                                                      #
#    and keep only one representative for each equivalence class.      #
#                                                                      #
########################################################################

`qrecsolve/qhypergeometric/zchoices`:= proc(rec,q,z,N,hi)
	local zlist, j, zchoices, rho, zz;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	zlist:= [seq(rec[j], j=0..hi)];
	rho:= min(seq(ldegree(j,N), j=zlist));
	zlist:= convert([seq(coeff(j,N,rho), j=zlist)], `+`);
	zlist:= [op(`qrecsolve/solve`(zlist, z) minus {0})];
	zlist:= sort(zlist, proc(a,b) evalb(length(a)<length(b)) end);
	# Now choose remove all redundant entries in zlist, i.e. x,y with
	# type(x/y, identical(q)^integer)...
	zchoices:= NULL;
	while (zlist <> []) do
		zz:= zlist[1];
		zlist:= subsop(1=NULL, zlist);
		zchoices:= zchoices, zz;
		zlist:= select(proc(x,y) not(type(combine(normal(x/y),power),\
			{1,identical(q),identical(q)^integer})) end, zlist, zz);
	od;
	RETURN([zchoices]);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/qhypergeometric`: given the recurrence req as a table of  #
#                              the coefficients search for all         #
#    qhypergeometric solutions F(n) of the homogeneous recurrence      #
#    equation  req[0]*F(n) + req[1]*F(n+1) + ... + req[hi]*F(n+hi) = 0 #
#                                                                      #
########################################################################

`qrecsolve/qhypergeometric`:= proc(req,q,n,N,hi,const)
	local i, j, rec, A_choices, B_choices, A, B, z,\
		genpol, genrecbase, genrec, den, zchoices, zz, sol, unsub;
	global _qrecsolve_local_return, _qrecsolve_local_split;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	userinfo(3,qsum,`Searching for q-Gosper represenation (z,P,Q,R)`.\
		`of solutions.`);
	userinfo(3,qsum,`Possible choices for Q and R respectively:`);
	A_choices:= `qrecsolve/factorlist`(req[0], N);
	B_choices:= `qrecsolve/factorlist`(subs(N=N*q/q^hi,req[hi]), N);
	rec:= table([seq(j=expand(req[j]), j=0..hi)]);
	sol:= NULL;

if (nops(A_choices) > nops(B_choices)) or ((nops(A_choices) =\
		nops(B_choices)) and (length(A_choices)>length(B_choices))) then

	for A in A_choices do
		genrecbase:= table([seq(j=expand(rec[j]*
			convert([seq(z*subs(N=N*q^i,A),i=0..j-1)],`*`)), j=0..hi)]);
		for B in B_choices do
#printf(`-%a-`,`qgosper/dispersionset`(A,B,q,N));
			genrec:= table([seq(j=expand(genrecbase[j]*
				convert([seq(subs(N=N*q^i,B),i=j..hi-1)],`*`)), j=0..hi)]);
			zchoices:= `qrecsolve/qhypergeometric/zchoices`(genrec,q,z,N,hi);
			for zz in zchoices do
				userinfo(5,qsum,`Testing:`,print('Q'=A,'R'=B,'z'=zz));
				genpol:= subs(z=zz, copy(genrec));
				genpol:= `qrecsolve/polynomial`(genpol,0,N,hi,const);
				if (genpol <> NULL) then
					userinfo(3,qsum,`Found possible solution:`,\
						print([zz,genpol,A,B]));
					sol:= sol, factor(zz*A/B*subs(N=N*q,genpol)/genpol);
#print('sol'=sol);
				fi;
			od;
		od;
	od;

else

	for B in B_choices do
		genrecbase:= table([seq(j=expand(rec[j]*
			convert([seq(subs(N=N*q^i,B),i=j..hi-1)],`*`)), j=0..hi)]);
		for A in A_choices do
#printf(`-%a-`,`qgosper/dispersionset`(A,B,q,N));
			genrec:= table([seq(j=expand(genrecbase[j]*
				convert([seq(z*subs(N=N*q^i,A),i=0..j-1)],`*`)), j=0..hi)]);
			zchoices:= `qrecsolve/qhypergeometric/zchoices`(genrec,q,z,N,hi);
			for zz in zchoices do
				userinfo(5,qsum,`Testing:`,print('Q'=A,'R'=B,'z'=zz));
				genpol:= subs(z=zz, copy(genrec));
				genpol:= `qrecsolve/polynomial`(genpol,0,N,hi,const);
				if (genpol <> NULL) then
					userinfo(3,qsum,`Found possible solution:`,\
						print([zz,genpol,A,B]));
					sol:= sol, factor(zz*A/B*subs(N=N*q,genpol)/genpol);
#print('sol'=sol);
				fi;
			od;
		od;
	od;

fi;

	RETURN(sol);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `qrecsolve/qhypergeometric`: given the recurrence req as a table of  #
#                              the coefficients search for all         #
#    qhypergeometric solutions F(n) of the homogeneous recurrence      #
#    equation  req[0]*F(n) + req[1]*F(n+1) + ... + req[hi]*F(n+hi) = 0 #
#                                                                      #
########################################################################

`qrecsolve/series`:= proc(req,RHS,q,n,N,hi,const)
	local F, inhomo, s, i, j, d, init, rec, sol, term, l, eqn, _i;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	inhomo:= collect(RHS,N,expand);
	s:= [seq(collect(req[j],N,expand),j=0..hi)];
	d:= max(seq(degree(j,N), j=s));
	init:= [seq([seq(coeff(s[i+1],N,j),j=0..d)],i=0..hi)];
	s:= seq([seq(coeff(s[i+1],N,d-j)*q^(i*j)*N^i,i=0..hi)],j=0..d);
	s:= map(factor@convert, [s], `+`);
	userinfo(2,qsum,`The associated recurrence equation is:`, print(\
		subs(N=q^n,convert([seq(s[j+1]*F(n+j),j=0..d)],`+`))=0));
	rec:= table([seq(j=s[j+1],j=0..d)]);
	if has(inhomo,Sum) then
		ERROR(`Not implemented yet.`);
	fi;
	sol:= {`qrecsolve/qhypergeometric`(rec,q,n,N,d,const)};
	if (sol = {}) then RETURN(NULL); fi;
	term:= `rat2qhyper/ratio`([op(sol)], N, q, n);
	if ({seq(j[2],j=term)} <> {0<=n}) then
		ERROR(`Shifting required, case not implemented yet.`);
	fi;
	term:= convert([seq(const[-j]*term[j][1],j=1..nops(sol))], `+`);
	term:= unapply('qsimpcomb'(term),n);
	eqn:= NULL;
	for l from 0 to d-1 do
		j:= seq([seq(init[i+1][l-j+1]*q^(i*j),i=0..hi)],j=0..l);
		j:= map(convert, [j], `+`);
		j:= convert([seq(j[i+1]*term(i),i=0..l)], `+`);
		eqn:= eqn, j=coeff(inhomo,N,l);
	od;
	# now collect all equations that result from the inhomogeneous part
	# with degree equal or larger than d:
# 1.3 sum -> add
#	rec:= sum('rec[j]*F(n+j)', 'j'=0..d);
	rec:= add(rec[j]*F(n+j), j=0..d);
#rec:= subs(F=unapply(F(n+'i'),'i'), rec);
	for i from d to degree(inhomo,N) do
		j:= qsimpcomb(subs(F=term,n=i,rec));
		eqn:= eqn, j=coeff(inhomo,N,i);
	od;
	sol:= gausselim({eqn},indets([eqn],const[integer]));
	if (sol = NULL) or (sol = {}) then RETURN() fi;
	if not(type(sol,set(set))) then sol:= {sol}; fi;
	sol:= [seq(subs(j,term(n)),j=sol)];
	i:= indets(sol, const[integer]);
	sol:= select((proc(z) not(z) end)@type, sol, identical(0));
	sol:= subs({n=_i,seq(i[j]=cat(`_C`,j),j=1..nops(i))}, sol);
	RETURN(seq(Sum(j*(q^n)^_i,_i=0..infinity),j=sol));
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
#  `qrecsolve/check_recurrence`: Check if rec is a recurrence in F(n), #
#                                i.e. rec should be a sum (type `+`)   #
#                                or an equation                        #
#                                and rec should contain expressions    #
#                                as F(n+i), where i is an integer.     #
#                                                                      #
########################################################################

`qrecsolve/check_recurrence`:= proc(rec,F,n)
	local all_inds, valid_inds;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	all_inds:= indets(rec, specfunc(anything,F));
	valid_inds:= indets(rec,specfunc({identical(n),\
		identical(n)&+integer,integer&+identical(n)},F));
	if not(type(rec,`+`) or type(rec,`=`)) or
			(all_inds = {}) or ((all_inds minus valid_inds) <> {}) then
		ERROR(`First argument is not a proper recurrence wrt. `.F.`(`.n.`).`);
	fi;
end:

# ----------------------------------------------------------------------

`qrecsolve/arguments`:= proc(ARGS,REC,F,q,n)
	local arg, i;
	global _qrecsolve_local_split, _qrecsolve_split,\
	       _qrecsolve_local_return, _qrecsolve_return,\
			 _qrecsolve_local_solution, _qrecsolve_solution,\
			 _qsum_local_specialsolution, _qsum_specialsolution;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	nops(ARGS);
	[seq(ARGS[i],i=1..3)];
	if (nops(ARGS)<3) or not(type([seq(ARGS[i],i=1..3)], [{algebraic,\
			equation(algebraic)},name,anyfunc(name)])) then
		ERROR(`Invalid arguments.`);
	fi;
	REC:= ARGS[1];
	q:= ARGS[2];
	F:= op(0,ARGS[3]);
	n:= op(1,ARGS[3]);
	`qrecsolve/check_recurrence`(eval(REC),eval(F),eval(n));
	arg:= [seq(ARGS[i],i=4..nops(ARGS))];
# Maple 6: return -> "return" ?
	`qsum/options`(arg,output,{downratio,upratio,ratio,downrec,uprec,\
		rec,qhypergeometric},'_qrecsolve_return', '_qrecsolve_local_return',\
		'arg');
	`qsum/options`(arg,split,{true,false},'_qrecsolve_split',\
		'_qrecsolve_local_split','arg');
	`qsum/options`(arg,solution,{series,polynomial,rational,qhypergeometric},\
		'_qrecsolve_solution','_qrecsolve_local_solution','arg');
	`qsum/options`(arg,specialsolution,{true,false},\
		'_qsum_specialsolution', '_qsum_local_specialsolution','arg');
	if (arg <> []) then
		lprint(`WARNING: The following options are unknown:`);
		print(arg);
	fi;
	RETURN();
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
#                                                                      #
#                                                                      #
#                                                                      #
#                                                                      #
########################################################################

qrecsolve:= proc()
	local re, F, q, n, N, inhomo, hi, const, i, sol;
	global polynomial, rational, qhypergeometric, series,\
		_qrecsolve_local_return, _qrecsolve_local_solution;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	`qrecsolve/arguments`([args],'re','F','q','n');
	`qrecsolve/normalform`(re,F,q,n,N,'re','inhomo','hi');
	if not(type([seq(re[i],i=0..hi)],list(polynom(anything,N)))) then
		ERROR(`Recurrence equation must have rational coefficients.`);
	fi;
	# check for homogeneous recurrence of first order -> TRIVIAL!
	if (_qrecsolve_local_solution <> series) and\
			(hi = 1) and (inhomo = 0) then
		sol:= -re[0] / re[1];
	elif (_qrecsolve_local_solution = polynomial) then
		sol:= `qrecsolve/polynomial`(re,inhomo,N,hi,const);
	elif (_qrecsolve_local_solution = rational) then
		sol:= `qrecsolve/rational`(re,inhomo,q,N,hi,const)
	elif (_qrecsolve_local_solution = qhypergeometric) then
		if (inhomo <> 0) then
			sol:= `qrecsolve/qhypergeometric/inhomogeneous`(\
				re,inhomo,F,q,n,N,hi,const)
		else
			sol:= `qrecsolve/qhypergeometric`(re,q,n,N,hi,const);
		fi;
	elif (_qrecsolve_local_solution = series) then
		sol:= `qrecsolve/series`(re,inhomo,q,n,N,hi,const);
	fi; 
	if (sol = NULL) then RETURN([]); fi;
	sol:= `qsum/var->_Ci`([op({sol})], indets([sol],const[integer]));
	if (sol<>NULL) and (_qrecsolve_local_solution=qhypergeometric) then
		if (_qrecsolve_local_return = qhypergeometric) then
			sol:= `rat2qhyper/ratio`(sol, N, q, n);
		elif (_qrecsolve_local_return = downratio) then
			sol:= [seq([factor(subs(N=N/q,i))],i=sol)];
		elif member(_qrecsolve_local_return, {ratio,upratio}) then
			sol:= [seq([i],i=sol)];
		elif (_qrecsolve_local_return = downrec) then
			sol:= [seq(factor(subs(N=N/q,i)), i=sol)];
			sol:= [seq([denom(i)*F(n)-numer(i)*F(n-1)=0],i=sol)];
		elif member(_qrecsolve_local_return, {rec,uprec}) then
			sol:= [seq([denom(i)*F(n+1)-numer(i)*F(n)=0],i=sol)];
		fi;
	fi;
	sol:= combine(subs(N=q^n,sol), power);
	RETURN(sol);
end:

# ----------------------------------------------------------------------
