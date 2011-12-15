# file hsum.mpl
# (C) Wolfram Koepf, Konrad-Zuse-Zentrum Berlin, 1998
# adapted to Maple 6 by Wolfram Koepf, 2001
# (C) Wolfram Koepf, University of Kassel, 2001-2009
# renamed hsum6.mpl
# adapted to Maple 9-13 by Wolfram Koepf, 2004-2009
# renamed hsum9.mpl
# renamed hsum10.mpl
# renamed hsum13.mpl
# 
# Version 1.1, Jan 02, 2001
# Version 1.2, Oct 13, 2002
# Version 1.3, Jul 03, 2003
# Version 1.4, Feb 28, 2004
# Version 1.5, May 29, 2004
# Version 1.6, Aug 21, 2004
# Version 1.7, Jan 29, 2006 for Maple 10
# Version 1.8, Jan 29, 2008 for Maple 11
# Version 1.9, Dec 14, 2009 for Maple 13
# History:
# Feb 12, 1998, recpoly eingefuegt, printf in rechyper entfernt
# Mar 16, 1998, sumdeltanabla, `sumdelta+nabla` eingefuegt
# Mar 16, 1998, new versions of simpcomb and (inhomogeneous) sumrecursion
# Dec 13, 2000, adaption for Maple 6: .->cat 
# Dec 13, 2000, adaption for Maple 6: deletion of support for V.3 and older
# Jan 02, 2001, bug in `sumrecursion/hypfunc` resolved
# Oct 13, 2002, closedform updated for double sums using `sum` for initial value
# Oct 13, 2002, Sumtohyper updated using simpcomb rather than simplify
# Jun 19, 2003, introduce kfreediffeq, fasenmyerdiffeq
# Jun 21, 2003, introduce RESum, REProduct still unfinished, see line 2580
# Jul 03, 2003, introduce DESum
# Dec 01, 2003, Maple 9 doesn't collect: change in `sumrecursion/inhomo` 
# Feb 28, 2004, Maple 9 has problems with local variables ...
# May 29, 2004, rechyper doesn't terminate: It's again an evaluation problem
# Aug 21, 2004, rec2hyper replaced by rechyper since rec2hyper sometimes fails
# Jan 29, 2008, inhomogenous part of Zeilberger: add instead of the new "sum"
# Oct 18, 2009, bug add instead if sum, and backquotes, resolved;
# Oct 18, 2009: allow degreebound -1 with f=0
# Dec 14, 2009: Torsten Sprenger: bugs for inomogeneous RE resolved
# Dec 14, 2009: function inhomotohomo added 
# Dec 14, 2011: hyperrecursion added
#
# Prof. Dr. Wolfram Koepf
# Department 10 Mathematics / Natural Sciences
# University of Kassel
# Heinrich-Plett-Str. 40
# 34132 Kassel
# Tel.: 0561/804-4207
# Fax: 0561/804-4646
# Secretary: 0561/804-4692
# e-mail: koepf@mathematik.uni-kassel.de
# URL: http://www.mathematik.uni-kassel.de/~koepf

MAXORDER:=5:

# BEGIN simpcomb, Harald Boeing
# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# hyperterm: Delivers the summand of a hypergeometric series, i.e.     #
#                                                                      #
#    hypergeom(upper,lower,z) =                                        #
#                        Sum(hyperterm(upper,lower,z,k),k=0..infinity) #
#                                                                      #
########################################################################

hyperterm:=proc(upper,lower,z,k)
	local j;
	option `Copyright 1998 Harald Boeing, Wolfram Koepf`;
	convert([seq(`simpcomb/pochhammer`(j,k),j=upper)],`*`)/\
		convert([seq(`simpcomb/pochhammer`(j,k),j=lower)],`*`)*z^k/k!;
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# simpcomb: Simplify expressions with factorial, binomial and          #
#           pochhammer expressions by converting those to GAMMA        #
#           expressions and use of                                     #
#                                                                      #
#                GAMMA(z+k)  ->  GAMMA(z) * prod(z+i),i=0..k-1)        #
#                                                                      #
#           whenever GAMMA(z+k) and GAMMA(z) appear in the input term  #
#           and k is an integer.                                       #
#                                                                      #
########################################################################

simpcomb:= proc()
	local f, EulerE, Imagin, sub, z;
	option `Copyright 1998 Harald Boeing, Wolfram Koepf`;
	if (nargs <> 1) then
		ERROR(`Wrong type of arguments.`);
	fi;
	f:= eval(subs(I=Imagin,exp=unapply(EulerE^z,z),args[1]));
	f:= `simpcomb/2GAMMA`(f);
	f:= `simpcomb/GAMMA/simplify`(f);
	f:= `simpcomb/power`(f);
	f:= `simpcomb/GAMMA/combine`(factor(f));
	sub:= [op(indets(f, identical(EulerE)^anything))];
	subs(seq(z=expand(exp(op(2,z))),z=sub),Imagin=I, f);
end:

# ----------------------------------------------------------------------

ratio:= proc(f,k)
	option `Copyright 1998 Harald Boeing, Wolfram Koepf`;
	if (nargs <> 2) or\
			not(type(f,{algebraic,list(algebraic),set(algebraic)}) and\
			type(k,name)) then
		ERROR(`Wrong type of arguments.`);
	elif type(f,algebraic) then
		simpcomb(subs(k=k+1,f)/f);
	else
		map(procname,f,k)
	fi;
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `simpcomb/2GAMMA`: Convert all factorial, binomial and pochhammer    #
#                     functions appearing in the input expression to   #
#                     GAMMA expressions by applying the substitutions  #
#                                                                      #
#   (i)  factorial(z)     -> GAMMA(z+1)                                #
#  (ii)  binomial(n,k)    -> `simpcomb/2GAMMA/binomial2GAMMA(n,k)      #
# (iii)  pochhammer(a,k)  -> `simpcomb/2GAMMA/pochhammer2GAMMA(a,k)    #
#                                                                      #
########################################################################

`simpcomb/2GAMMA`:= proc(f)
	option `Copyright 1998  Harald Boeing, Wolfram Koepf`;
	eval(subs(factorial=(proc(z) GAMMA(z+1) end),\
		binomial=`simpcomb/2GAMMA/binomial2GAMMA`,\
		pochhammer=`simpcomb/2GAMMA/pochhammer2GAMMA`,\
		Beta=(proc(z,w) GAMMA(z)*GAMMA(w)/GAMMA(z+w) end), f));
end:

# ----------------------------------------------------------------------

`simpcomb/2GAMMA/binomial2GAMMA`:= proc(n,k)
	if type(n,negint) then
		(-1)^k*GAMMA(k-n)/GAMMA(k+1)/GAMMA(-n);
	elif type(expand(n-k),negint) then
		0;
	else
		GAMMA(n+1)/GAMMA(n+1-k)/GAMMA(k+1);
	fi;
end:

# ----------------------------------------------------------------------

`simpcomb/2GAMMA/pochhammer2GAMMA`:= proc(a,k)
	local i;
	if (a = 1/2) then
		GAMMA(2*k+1)/4^k/GAMMA(k+1)
	elif type(k,integer) then
		# Note that this is correct for type(k,negint) as well
		product(a+i,i=0..k-1);
	elif type(a,integer) then
		if type(a,posint) then
			GAMMA(a+k)/GAMMA(a)
		elif type(a,negint) then
			(-1)^k*GAMMA(-a+1)/GAMMA(-a-k+1);
		elif (a = 0) and type(k,nonnegint) then
			0
		else
			pochhammer(a,k);
		fi;
	elif (expand(a+k) = 0) then
		(-1)^k*GAMMA(k+1);
	elif type(k,negint) then
		GAMMA(a+k)/GAMMA(a);
	else
		GAMMA(a+k)/GAMMA(a);
	fi;
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# simplify_gamma: Rewrite GAMMA function terms according to:           #
#                                                                      #
#           GAMMA(x+i) = (x+i-1)*(x+i-2)...(x)*GAMMA(x),  i posint     #
#                                                                      #
#  if the term GAMMA(x+i) and GAMMA(x) appear in the input expression  #
#                                                                      #
########################################################################

`simpcomb/GAMMA/simplify`:= proc()
	local f, gammalist, equivlist, a, tmp, sub, j, i;
	option `Copyright 1998  Harald Boeing, Wolfram Koepf`;
	f:= args[1];
	# Get a list of all arguments of GAMMA functions in f...
	gammalist:= map(op, [op(indets(f,specfunc(anything,GAMMA)))]);
	# Now group all arguments in equivalence classes, where
	# a ~ b <=> type(a-b,integer)
	equivlist:= NULL;
	while (gammalist <> []) do
		tmp:= gammalist[1];
		a:= tmp;
		gammalist:= subsop(1=NULL, gammalist);
		i:= nops(gammalist);
		for j from i to 1 by -1 do
			if type(normal(gammalist[j]-a,expanded),integer) then
				tmp:= tmp, gammalist[j];
				gammalist:= subsop(j=NULL,gammalist);
			fi;
		od;
		tmp:= [tmp];
		# Now sort the list of shifted arguments tmp, such that the
		# one with the smaller shift appear first...
		tmp:= sort(tmp, proc(x,y) type(normal(x-y,expanded),negint) end);
		equivlist:= equivlist, tmp;
	od;
	equivlist:= select(proc(x) evalb(1<nops(x)) end, [equivlist]);
	# Now substitute all members of an equivalence class by their
	# 'smallest' member...
	for a in equivlist do
		sub:= seq(GAMMA(a[j])=GAMMA(a[1])*product('a[1]+i',\
			'i'=0..normal(a[j]-a[1],expanded)-1), j=2..nops(a));
		f:= subs(sub, f);
	od;
	RETURN(f);
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `simpcomb/power`: power simplificatication (see next procedure) by   #
#                   applying `simpcomb/power/recursive` to all power   #
#                   expressions appearing in the input expression.     #
#                                                                      #
########################################################################

`simpcomb/power`:= proc(f)
	option `Copyright (c) 1998 by Harald Boeing.`;
### WARNING: note that `I` is no longer of type `^`
	if type(f,`^`) then
		`simpcomb/power/recursive`(procname(op(1,f)),procname(op(2,f)));
### WARNING: note that `I` is no longer of type `^`
	elif hastype(f,`^`) then
		map(procname,f);
	else
		f;
	fi;
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `simpcomb/power/recursive`: procname(b,e)->b^e, with an application  #
#                             of the following rules:                  #
#                                                                      #
#       (i)   (x^i)^j  ->  x^(i*j)                                     #
#      (ii)   (x*y)^i  ->  x^i*y^i                                     #
#     (iii)   x^(i+j)  ->  x^i*x^j                                     #
#                                                                      #
# Additionally the powers are standardized via Maple's function        #
# expand, e.g. x^(i*y)->(x^y)^i, if i is an integer.                   #
#                                                                      #
########################################################################

`simpcomb/power/recursive`:= proc(b,e)
	local j;
	option `Copyright (c) 1998 by Harald Boeing.`;
	if type(e,`+`) then
		convert([seq(procname(b,j),j=e)], `*`);
	elif type(b,`*`) then
		map(procname,b,e);
### WARNING: note that `I` is no longer of type `^`
	elif type(b,`^`) then
		procname(op(1,b),expand(normal(op(2,b)*e)))
	elif (denom(e) <> 1) then
		# substitute the denom of e by symbolic j to get simple form
		subs(j=denom(e),expand(b^(numer(e)/j)));
	else
		subs(j=b,expand(j^e));
	fi;
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `simpcomb/pochhammer`: Use for integer k the rule the rule           #
#                                                                      #
#     pochhammer(a,k)  ->  product(a+j,j=0..k-1)                       #
#                                                                      #
########################################################################

`simpcomb/pochhammer`:= proc(a,k)
	local j;
	option `Copyright (c) 1998 by Harald Boeing.`;
	if type(k,integer) then
		product(a+j,j=0..k-1);
	elif type(a,posint) then
		(a+k-1)!/(a-1)!;
	elif (a = 1/2) then
		(2*k)!/4^k/k!
	else
		pochhammer(a,k);
	fi;
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `simpcomb/GAMMA/combine/BuildList`: Given the expression expr        #
#                                                                      #
#   expr = f_1^i_1*...*f_m^i_m * GAMMA(a_1)^j_1*...*GAMMA(a_n)^j_n     #
#                                                                      #
# with integers i_1,...i_m,j_1,...,j_n the procedure assigns to        #
# GAMLIST and OTHLIST the following values                             #
#                                                                      #
#    GAMLIST:= [[a1,j1], ..., [an,jn]]                                 #
#    OTHLIST:= [[f1,i1], ..., [fm,im]]                                 #
#                                                                      #
# and returns true if both lists are nonempty, false otherwise.        #
#                                                                      #
########################################################################

`simpcomb/GAMMA/combine/BuildList`:= proc(expr,GAMLIST,OTHLIST)
	local gam, oth, f, z, sig, lcsig;
	option `Copyright (c) 1998 Harald Boeing.`;
	gam:= NULL;
	oth:= NULL;
	sig:= 1;
	for f in expr do
		if type(f,anything^integer) then
			z:= [op(f)];
		else
			z:= [f,1];
		fi;
		if type(z[1],specfunc(anything,GAMMA)) then
			gam:= gam, [op(1,z[1]),z[2]];
		else
        		lcsig:= traperror(sign(lcoeff(z[1])));
        		if (lcsig = lasterror) then
           		   lcsig:= 1;
        		fi;

			oth:= oth, [lcsig*z[1],z[2]];
			sig:= sig * lcsig^z[2];
		fi;
	od;
	GAMLIST:= expand([gam]);
	OTHLIST:= expand([[sig,1],oth]);
	evalb((gam = NULL) or (oth = NULL));
end:

# ----------------------------------------------------------------------
########################################################################
#                                                                      #
# `simpcomb/GAMMA/combine`: Apply the rules                            #
#                                                                      #
#       (i)  GAMMA(x)*x*(x+1)*...*(x+k) -> GAMMA(x+k)                  #
#                                                                      #
#      (ii)  GAMMA(x)/(x-1)/(x-2)/.../(x-k) -> GAMMA(x-k)              #
#                                                                      #
#  such that the number of non-GAMMA-factors in the denominator is     #
#  minimal, e.g.                                                       #
#                                                                      #
#       GAMMA(x)*x/(x-1)  -> GAMMA(x-1)*x                              #
#                                                                      #
########################################################################

`simpcomb/GAMMA/combine`:= proc()
	local tim,f,sub,gam_list,oth_list,gam,tmp1,tmp2,oth,d,ex1,ex2,\
			lo,hi,i,lc,mem;
	option `Copyright (c) 1998 Harald Boeing.`;
	tim:= time();
	f:= args[1];
	if not(type(f,`*`)) then
		if hastype(f,`*`) and has(f,GAMMA) then
			RETURN(map(procname,f));
		else
			RETURN(f);
		fi;
	elif `simpcomb/GAMMA/combine/BuildList`(f,'gam_list',\
			'oth_list') then
		RETURN(map(procname,f));
	fi;
	sub:= 1;
	for gam in gam_list do
		tmp1:= table(sparse);
		tmp2:= table(sparse);
     		lc:= traperror(sign(lcoeff(gam[1])));
     		if (lc = lasterror) then
     		   lc:= 1;
     		fi;

		for i from 1 to nops(oth_list) do
			d:= lc*oth_list[i][1] - gam[1];
			if type(d,integer) then
				tmp1[d]:= oth_list[i][2];
				tmp2[d]:= i;
			fi;
		od;
		ex1:= table(sparse);
		ex2:= table(sparse);
		if type(gam[2],posint) then
			ex1[0]:= gam[2];
			ex2[0]:= min(gam[2],-tmp1[-1]);
			for lo from -1 by -1 while (tmp1[lo] < 0) do
				ex1[lo]:= -tmp1[lo];
				ex2[lo]:= min(ex2[lo+1],-tmp1[lo]);
			od;
			if (tmp1[0] > 0) then
				ex1[0]:= tmp1[0];
				ex2[0]:= min(gam[2],tmp1[0]);
				for hi from 1 while (tmp1[hi] > 0) do
					ex1[hi]:= tmp1[hi];
					ex2[hi]:= min(ex2[hi-1],tmp1[hi]);
				od;
			else
				hi:= 0;
			fi;
		else
			if (tmp1[0] < 0) then
				ex1[0]:= tmp1[0];
				ex2[0]:= max(gam[2],tmp1[0]);
				for hi from 1 while (tmp1[hi] < 0) do
					ex1[hi]:= tmp1[hi];
					ex2[hi]:= min(ex2[hi-1],tmp1[hi]);
				od;
			else
				hi:= 0;
			fi;
			ex1[0]:= gam[2]-ex2[0];
			ex2[0]:= min(gam[2]-ex2[0],-tmp1[-1]);
			if (ex1[0] < 0) then
				for lo from -1 by -1 while (tmp1[lo] > 0) do
					ex1[lo]:= tmp1[lo];
					ex2[lo]:= min(ex2[lo+1],tmp1[lo]);
				od;
			else
				lo:= -1;
			fi;
		fi;
		if (lo < -1) then
			ex1[0]:= gam[2];
			ex2[0]:= min(gam[2],-tmp1[-1]);
			sub:= sub/GAMMA(gam[1])^ex2[-1]*\
				convert([seq(GAMMA(gam[1]+i)^(ex2[i]-ex2[i-1])*
				(gam[1]+i)^(ex2[i]),i=lo+1..-1)],`*`);
			mem:= [seq([tmp2[i],ex2[i]],i=lo+1..-1)];
			oth_list:= subsop(seq(i[1]=subsop(2=oth_list[i[1]][2]+i[2],\
				oth_list[i[1]]),i=mem),oth_list);
		fi;
		if (hi > 0) then
			ex1[-1]:= gam[2]-ex2[-1];
			ex2[-1]:= gam[2]-ex2[-1];
			ex1[0]:= tmp1[0];
			ex2[0]:= min(ex2[-1],tmp1[0]);
			sub:= sub/GAMMA(gam[1])^(ex2[0])*\
				convert([seq(GAMMA(gam[1]+i)^(ex2[i-1]-ex2[i])*\
				(gam[1]+i-1)^(-ex2[i-1]),i=1..hi)],`*`);
			mem:= [seq([tmp2[i-1],ex2[i-1]],i=1..hi)];
			oth_list:= subsop(seq(i[1]=subsop(2=oth_list[i[1]][2]-i[2],\
				oth_list[i[1]]),i=mem),oth_list);
		fi;
		oth_list:= select(proc(z) not(type(z,[anything,0])) end, oth_list);
	od;
	f:= normal(f*sub);
	RETURN(f);
end:

# ----------------------------------------------------------------------
# END simpcomb

simplify_combinatorial:=proc(x) simpcomb(x) end:

Sumtohyper:=proc(f,k)
local rat,num,den,x,numlist,denlist,init,i,j,result;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
init:=simpcomb(subs(k=0,f));
if init=0 then
 ERROR(`Shift necessary`);
fi;
rat:=simpcomb(subs(k=k+1,f)/f);
if not type(rat,ratpoly(anything,k)) then
 ERROR(`Cannot be converted into hypergeometric form`)
fi:
num:=numer(rat);
den:=denom(rat);
x:=lcoeff(num,k)/lcoeff(den,k);
numlist := normal([solve(num,k)]);
numlist := [seq(-j,j=numlist)];
denlist := normal([solve(den,k)]);
denlist := [seq(-j,j=denlist)];
if not(member(1,denlist,'i')) then
 ERROR(`Shift necessary or no conversion possible`);
fi;
denlist:= subsop(i=NULL,denlist);
result:=init*Hypergeom(numlist,denlist,x);
RETURN(result)
end: # Sumtohyper

Sumtohyper:=proc(f,k)
local Ank, Ank1,DE,rat,num,den,numfactors,denfactors,lc,l,numlist,numset,
denset,numdegree,denlist,opj,tmp,dendegree,i,j,minint,maxint,tmpnew,tmptmp;
global _shiftnumber;
options remember,
`Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
userinfo(3,'summation',`conversion to hypergeometric notation (Koepf)`);
Ank:=simpcomb(f);
Ank1:=subs(k=k+1,Ank);
DE:=simpcomb(Ank1/Ank);
userinfo(3,'summation',`a(`,k,`+1)/a(`,k,`):=`,DE);
if not type(DE,ratpoly(anything, k)) then
  userinfo(3,'summation',`is not rational`):
  ERROR(`Cannot be converted into hypergeometric form`)
fi:
rat:=DE;
num:=numer(rat);
den:=denom(rat);
numfactors:=frontend(factors,[num]);
denfactors:=frontend(factors,[den]);
lc:=lcoeff(num,k)/lcoeff(den,k);
numfactors:=op(2,numfactors);
denfactors:=op(2,denfactors);
numlist:=[];
denlist:=[];
for j from 1 to nops(numfactors) do
 for i from 1 to op(2,op(j,numfactors)) do
 opj:=op(1,op(j,numfactors));
  if has(opj,k) then
   if degree(opj,k)>2 then
     ERROR(`Nonrational factors occur: not yet implemented`)
   elif degree(opj,k)=2 then tmp:=[solve(opj,k)];
     numlist:=[op(numlist),-op(1,tmp)];
     numlist:=[op(numlist),-op(2,tmp)];
   else numlist:=[op(numlist),-solve(opj,k)] fi;
  fi;
 od;
od;
for j from 1 to nops(denfactors) do
 for i from 1 to op(2,op(j,denfactors)) do
 opj:=op(1,op(j,denfactors));
  if has(opj,k) then
   if degree(opj,k)>2 then
     ERROR(`Nonrational factors occur: not yet implemented`)
   elif degree(opj,k)=2 then tmp:=[solve(opj,k)];
     denlist:=[op(denlist),-op(1,tmp)];
     denlist:=[op(denlist),-op(2,tmp)];
   else denlist:=[op(denlist),-solve(opj,k)] fi;
  fi
 od;
od;
minint:=mininteger(denlist);
_shiftnumber:=0;
if minint<> 1 then
 numset:=map(k->k+1-minint,convert(numlist,set));
 _shiftnumber:=1-minint;
fi;
numset:=convert(numlist,set);
if evalb(numset union {0} = numset) then
 while evalb(numset union {0} = numset) do
   _shiftnumber:=_shiftnumber+1;
   numset:=map(k->k+1,numset);
 od;
 userinfo(3,'summation',`summing over`,k,` = `,_shiftnumber,`.. infinity`);
fi;
if _shiftnumber<>0 then
 userinfo(3,'summation',`shifting by`,_shiftnumber);
fi;
userinfo(3,'summation',`calculating initial value`);
numlist:=map((k,_shiftnumber)->k+_shiftnumber,numlist,_shiftnumber);
denlist:=map((k,_shiftnumber)->k+_shiftnumber,denlist,_shiftnumber);
tmptmp:=traperror(eval(subs(k=_shiftnumber,f)));
if tmptmp=lasterror then
 tmp:=traperror(limit(Ank,k=_shiftnumber));
 if tmp=lasterror or has(tmp,undefined) or has(tmp,infinity) or 
    has(tmp,range) or has(tmp,unevaluated) or has(tmp,value) then
 ERROR(`Cannot be converted into hypergeometric form`) fi
else
 tmp:=tmptmp;
fi;
# currently: pochhammer(a,0)=>`1;
tmp:=traperror(eval(subs(pochhammer=`simpcomb/pochhammer`,tmp)));
if tmp=lasterror then
 tmp:=traperror(limit(Ank,k=_shiftnumber));
 if tmp=lasterror or has(tmp,undefined) or has(tmp,infinity) or 
    has(tmp,range) or has(tmp,unevaluated) or has(tmp,value) then
 ERROR(`Cannot be converted into hypergeometric form`) fi
fi;
# typical correction since 1 must be a denumerator parameter
if member(1,denlist,'i') then
 denlist:= subsop(i=NULL,denlist);
else
 numlist:= [op(numlist),1];
fi;
### old 
### tmp:=simplify_combinatorial(tmp)*Hypergeom(numlist,denlist,lc);
### new
### tmp:=simplify(tmp)*Hypergeom(numlist,denlist,lc);
tmp:=simpcomb(tmp)*Hypergeom(numlist,denlist,lc);
userinfo(3,'summation',`finished conversion in hypergeometric notation`);
userinfo(3,'summation',tmp);
RETURN(tmp)
end: # Sumtohyper

mininteger:=proc(list)
local j,tmp;
 option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 tmp:=infinity;
 tmp:= min(op(select(type,list,integer)));
 if (tmp = infinity) then
   tmp:= select(proc(z) if is(z,integer) then true else false fi end, list);
	 tmp:= min(op(tmp));
	 if has(tmp,min) then
	   tmp:= op(1,tmp);
	 elif (tmp = infinity) then
     userinfo(0,'summation',`warning: no actual initialization found`);
     tmp:=1;
	 fi;
 fi;
 tmp;
end: # mininteger

# simpcomb:=proc(a)
#   simplify_combinatorial(a)
# end: # simpcomb

kfreerec:=proc(f,k,n,kmax,nmax)
local SUM,N,variables,i,j,l,solution,rat,F,a;
options remember,
`Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if nargs>5 then F:=args[6] fi; if nargs>6 then a:=args[7] fi;
N:=(kmax+1)*(nmax+1);
variables:={seq(seq(a[i,j],i=0..kmax),j=0..nmax)};
SUM:=0;
rat:=0;
for i from 0 to kmax do
 for j from 0 to nmax do
   SUM:=SUM+a[i,j]*simpcomb(subs(n=n+j,k=k+i,f)/f);
   rat:=rat+a[i,j]*F(n+j,k+i)
 od;
od;
SUM:=collect(numer(normal(SUM)),k);
solution:={solve({coeffs(SUM,k)},variables)};
if {seq(op(2,op(l,op(1,solution))),l=1..N)}={0} then 
 ERROR(cat(`No kfree recurrence equation of order (`,kmax,`,`,nmax,`) exists`));
fi;
rat:=subs(op(1,solution),rat);
rat:=numer(normal(rat));
rat:= collect(rat,[seq(seq(F(n+nmax-j,k+kmax-i),j=0..nmax),i=0..kmax)]);
RETURN(map(factor,rat)=0);
end: # kfreerec

# old
fasenmyer:=proc(f,k,sn,nmax)
local F,a,n,S,i,j,recursion;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if nargs>4 then a:=args[5] fi;
if type(sn,function) then S:=op(0,sn); n:=op(1,sn) else n:=sn fi;
recursion:=op(1,kfreerec(f,k,n,nmax,nmax,F,a));
for i from 0 to nmax do
 for j from 0 to nmax do
   recursion:=subs(F(n+j,k+i)=S(n+j),recursion);
   recursion:=collect(recursion,S(n+j));
 od;
od;
RETURN(map(factor,recursion)=0);
end: # fasenmyer

fasenmyer:=proc(f,k,sn,nmax)
local F,a,n,S,i,j,recursion,tmp;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sn,function) then S:=op(0,sn); n:=op(1,sn) else n:=sn fi;
recursion:=op(1,kfreerec(f,k,n,nmax,nmax,F,a));
for i from 0 to nmax do
 for j from 0 to nmax do
   recursion:=subs(F(n+j,k+i)=S(n+j),recursion);
 od;
od;
tmp:=normal(solve(recursion,S(n+nmax)));
recursion:=denom(tmp)*S(n+nmax)-numer(tmp);
recursion:=collect(recursion,[seq(S(n+nmax-j),j=0..nmax)]);
RETURN(map(factor,recursion)=0);
end: # fasenmyer

kfreediffeq:=proc(f,k,n,kmax,nmax)
local SUM,N,variables,i,j,l,solution,rat,F,a;
options remember,
`Copyright 2003  Wolfram Koepf, University of Kassel`;
if nargs>5 then F:=args[6] fi; if nargs>6 then a:=args[7] fi;
N:=(kmax+1)*(nmax+1);
variables:={seq(seq(a[i,j],i=0..kmax),j=0..nmax)};
SUM:=0;
rat:=0;
for i from 0 to kmax do
 SUM:=SUM+a[i,0]*simpcomb(subs(k=k+i,f)/f);
 rat:=rat+a[i,0]*F(n,k+i);
 for j from 1 to nmax do
   SUM:=SUM+a[i,j]*simpcomb(diff(subs(k=k+i,f),n$j)/f);
   rat:=rat+a[i,j]*diff(F(n,k+i),n$j)
 od;
od;
SUM:=collect(numer(normal(SUM)),k);
solution:={solve({coeffs(SUM,k)},variables)};
if {seq(op(2,op(l,op(1,solution))),l=1..N)}={0} then 
 ERROR(cat(`No kfree recurrence differential equation of order (`,kmax,`,`,nmax,`) exists`));
fi;
rat:=subs(op(1,solution),rat);
rat:=numer(normal(rat));
rat:= collect(rat,[seq(seq(diff(F(n,k+kmax-i),n$nmax-j),j=0..nmax-1),i=0..kmax)]);
RETURN(map(factor,rat)=0);
end: # kfreediffeq

fasenmyerdiffeq:=proc(f,k,sn,nmax)
local F,a,n,S,i,j,recursion,tmp;
option `Copyright 2003  Wolfram Koepf, University of Kassel`;
if type(sn,function) then S:=op(0,sn); n:=op(1,sn) else n:=sn fi;
recursion:=op(1,kfreediffeq(f,k,n,nmax,nmax,F,a));
for i from 0 to nmax do
 recursion:=subs(F(n,k+i)=S(n),recursion);
 for j from 1 to nmax do
   recursion:=subs(diff(F(n,k+i),n$j)=diff(S(n),n$j),recursion);
 od;
od;
tmp:=solve(recursion,diff(S(n),n$nmax));
recursion:=denom(tmp)*diff(S(n),n$nmax)-numer(tmp);
recursion:=collect(recursion,[seq(diff(S(n),n$nmax-j),j=0..nmax-1)]);
recursion:=collect(recursion,S(n));
RETURN(map(factor,recursion)=0);
end: # fasenmyerdiffeq

# ratio:=proc(a,k)
# option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
#   simpcomb(subs(k=k+1,a)/a)
# end: # ratio

primedispersion:=proc(q,r,k)
# calculates the dispersion of nonfactorizable polynomials q and r of 
# equal degree
local f,g,n,a,b,c,d,j;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
f:=collect(q,k);
g:=collect(r,k);
n:=degree(f,k);
if n=0 or n<>degree(g,k) then RETURN({}) fi;
a:=coeff(f,k,n);
b:=coeff(f,k,n-1);
c:=coeff(g,k,n);
d:=coeff(g,k,n-1);
j:=normal((b*c-a*d)/(a*c*n));
if not type(j,nonnegint) then RETURN({}) fi;
if collect(c*f-a*subs(k=k+j,g),k)=0 then 
 RETURN({j}) 
else
 RETURN({});
fi;
end: # primedispersion

dispersionset:=proc(q,r,k)
# calculates the dispersion set of polynomials q and r, i.e. the set 
# of nonnegative integers j such that q(k) and r(k+j) are divisible
local f,g,m,n,i,j,result,tmp,op1,op2;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
f:=factor(q);
g:=factor(r);
if not(type(f,`*`)) then m:=1 else m:=nops(f) fi;
if not(type(g,`*`)) then n:=1 else n:=nops(g) fi;
result:={};
for i from 1 to m do
 if type(f,`*`) then op1:=op(i,f) else op1:=f fi;
### WARNING: note that `I` is no longer of type `^`
 if type(op1,`^`) then op1:=op(1,op1) fi;
 for j from 1 to n do
   if type(g,`*`) then op2:=op(j,g) else op2:=g fi;
### WARNING: note that `I` is no longer of type `^`
   if type(op2,`^`) then op2:=op(1,op2) fi;
   tmp:=primedispersion(op1,op2,k);
   if tmp<>{} then result:=result union {op(1,tmp)} fi;
 od;
od;
RETURN(result);
end: # dispersionset

resultantdispersionset:=proc(q,r,k)
local j;
 RETURN(isolve(resultant(q,subs(k=k+j,r),k),j))
end: # resultantdispersionset

update:=proc(p,q,r,k)
# updates the triple [p,q,r] according to gcd-condition
local dis,g,pnew,qnew,rnew,j,l;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 # j=1
 g:=frontend(gcd,[q,subs(k=k+1,r)]);
#  g:=factorgcd(q,subs(k=k+1,r),k);
 if has(g,k) then
   pnew:=normal(p*g);
   qnew:=normal(q/g);
   rnew:=normal(r/subs(k=k-1,g));
 else
   pnew:=p; qnew:=q; rnew:=r;
 fi;
 dis:=convert(dispersionset(qnew,rnew,k),list);
 for j in dis do
   g:=frontend(gcd,[qnew,subs(k=k+j,rnew)]);
#    g:=factorgcd(qnew,subs(k=k+j,rnew),k);
   if has(g,k) then
     pnew:=normal(pnew*product(subs(k=k-'l',g),'l'=0..j-1));
     qnew:=normal(qnew/g);
     rnew:=normal(rnew/subs(k=k-j,g));
   fi;
 od;
 RETURN([pnew,qnew,rnew]);
end: # update

degreebound:=proc(p,q,r,k)
# calculates the degree bound for f
local pol1,pol2,deg1,deg2,a,b;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 pol1:=collect(subs(k=k+1,q)+r,k);
 pol2:=collect(subs(k=k+1,q)-r,k);
 if pol1=0 then deg1:=-1 else deg1:=degree(pol1,k) fi;
 if pol2=0 then deg2:=-1 else deg2:=degree(pol2,k) fi;
 if deg1<=deg2 then RETURN(degree(p,k)-deg2) fi;
 a:=coeff(pol1,k,deg1);
 if deg2<deg1-1 then b:=0 else b:=coeff(pol2,k,deg2) fi;
 if not(type(-2*b/a,nonnegint)) then RETURN(degree(p,k)-deg1+1) else
   RETURN(max(-2*b/a,degree(p,k)-deg1+1)) fi;
end: # degreebound

findf:=proc(p,q,r,k)
# finds f, given the triple [p,q,r]
local deg,f,a,j,rec,sol,result;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 deg:=degreebound(p,q,r,k);
 if deg<0 then ERROR(`No polynomial f exists`) fi;
 f:=add(a[j]*k^j,j=0..deg);
 rec:=collect(subs(k=k+1,q)*f-r*subs(k=k-1,f)-p,k);
 sol:={solve({coeffs(rec,k)},{seq(a[j],j=0..deg)})};
 if sol={} then ERROR(`No polynomial f exists`) else
   result:=subs(op(1,sol),f);
 fi;
 for j from 0 to deg do
   result:=subs(a[j]=0,result);
 od;
 RETURN(result);
end: # findf

gosper:=proc(a,k)
local rat,p,q,r,pqr,f,tmp;
# implements the Gosper algorithm
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 rat:=ratio(a,k);
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 p:=1;
 q:=subs(k=k-1,numer(rat));
 r:=subs(k=k-1,denom(rat));
 pqr:=update(p,q,r,k);
 p:=op(1,pqr); q:=op(2,pqr); r:=op(3,pqr);
 f:=traperror(findf(p,q,r,k));
 if f=lasterror then 
   ERROR(`No hypergeometric term antidifference exists`)
 fi;
 RETURN(normal(r/p*subs(k=k-1,f))*a);
end: # gosper

expandedgosper:=proc(a,k)
local rat,p,q,r,pqr,f,tmp;
# implements the Gosper algorithm
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 rat:=ratio(a,k);
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 if not(type(rat,ratpoly)) then
   rat:=normal(rat,'expanded');
 fi;
 p:=1;
 q:=subs(k=k-1,numer(rat));
 r:=subs(k=k-1,denom(rat));
 pqr:=update(p,q,r,k);
 p:=op(1,pqr); q:=op(2,pqr); r:=op(3,pqr);
 f:=traperror(findf(p,q,r,k));
 if f=lasterror then
   ERROR(`No hypergeometric term antidifference exists`)
 fi;
 RETURN(normal(r/p*subs(k=k-1,f))*a);
end: # expandedgosper

primefactorgcd:=proc(q,r,k)
# calculates the gcd of q and r, not factorizable in lower terms
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if not(has(normal(q/r),k)) then
 RETURN(q);
else
 RETURN(1);
fi;
end: # primefactorgcd

factorgcd:=proc(q,r,k)
# calculates the gcd of q and r with respect to k from their factors
local i,j,f,g,m,n,result,exponent,exponent1,op1,op2,Op1,Op2,tmp;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 f:=factor(q);
 g:=factor(r);
 if degree(f,k)=0 or degree(g,k)=0 then RETURN(1) fi;
 if not(type(f,`*`)) then m:=1 else m:=nops(f) fi;
 if not(type(g,`*`)) then n:=1 else n:=nops(g) fi;
 result:=1;
 for i from 1 to m do
   exponent1:=1;
   if type(f,`*`) then op1:=op(i,f) else op1:=f fi;
### WARNING: note that `I` is no longer of type `^`
   if type(op1,`^`) then
     Op1:=op(1,op1);
     exponent1:=op(2,op1);
   else
     Op1:=op1;
   fi;
   for j from 1 to n do
     if type(g,`*`) then op2:=op(j,g) else op2:=g fi;
### WARNING: note that `I` is no longer of type `^`
     if type(op2,`^`) then
       Op2:=op(1,op2);
       exponent:=min(exponent1,op(2,op2));
     else
       Op2:=op2;
       exponent:=1;
     fi;
     tmp:=primefactorgcd(Op1,Op2,k);
     if has(tmp,k) then
       result:=result*tmp^exponent
     fi;
   od;
 od;
RETURN(result);
end: #factorgcd

WZcertificate:=proc(F,k,n)
local a,gos;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 a:=subs(n=n+1,F)-F;
 gos:=traperror(gosper(a,k));
 if gos=lasterror then ERROR(`WZ method fails`) fi;
 RETURN(simpcomb(gos/F));
end: # WZcertificate

WZgospercertificate:=proc(F,k,n)
local a,gos;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 a:=subs(n=n+1,F)-F;
 gos:=gosper(a,k);
RETURN(simpcomb(gos/a));
end: # WZgospercertificate

argumentlist:=proc(f)
local list1,list2;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
### WARNING: note that `I` is no longer of type `^`
 list1:= indets(f,`^`);
 list1:=  map(x -> op(2,x), list1);
 list2:= indets(f,'{factorial(anything),GAMMA(anything),exp(anything),
                binomial(anything,anything),pochhammer(anything,anything)}');
 list2 := map(op,list2);
 RETURN( list1 union list2 );
end:

find_substitution:=proc(term,n)
local listn,m;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
listn := map(linearfactor,term,n);
m:=ilcm(op(listn));
RETURN(m);
end: #find_substitution

linearfactor:=proc(term1,n)
local p,a0,a1;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if not ispoly(term1,linear,n,'a0','a1') then RETURN(1) fi;
p := denom(a1);
if type(p,integer) then RETURN(p) else RETURN(1) fi;
end: # linearfactor

extended_gosper:=proc()
#extended_gosper:=proc(ff::algebraic,kk::{name,name=range})
local al,k1,k2,l,nnargs,newsummand,tk,tmpterm,tmp,j,summand,k,tmp1,tmp2;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
summand:=args[1];
k:=args[2];
if type(k,`=`) and type(op(2,k),`..`) and type(op(1,k),name) then
 k1:=op(1,op(2,k));
 k2:=op(2,op(2,k));
 k:=op(1,k);
 nnargs:=4
elif type(k,name) then
 nnargs:=nargs
else ERROR(`Second argument of wrong type`)
fi:
if nargs=3 then
 l:=args[3]
else
 ### New argumentlist called with 1 argument
 al:=argumentlist(summand);
 l:=find_substitution(al,k);
 if l<>1 then
   userinfo(2,'summation',`Koepf extension of Gosper algorithm applied`):
   userinfo(2,'summation',`linearizing integer with respect to`,k,` is `,l);
 fi;
fi;
if nargs>3 then
 k1:=args[3];
 k2:=args[4];
fi:
newsummand:=subs(k=k*l,summand);
userinfo(2,'summation',`applying Gosper algorithm to a(`,k,`):=`,newsummand);
tk:=subs(k=k/l,gosper(newsummand,k));
if l<>1 then
  userinfo(2,'summation',`Koepf's algorithm successful`) fi:
if nnargs = 3 then
 RETURN(tk)
elif nnargs = 2 then
# down version
#  RETURN(sum(subs(k=k-j,tk),'j'=0..l-1))
# up version
  RETURN(add(subs(k=k+j,tk),j=0..l-1));
else
# down version
#  tk:= sum(subs(k=k-j,tk),'j'=0..l-1);
# up version
 tk:= add(subs(k=k+j,tk),j=0..l-1);
#  tmp:=traperror(subs(k=k2,tk)-subs(k=k1-1,tk));
#  if tmp=lasterror then tmp:=limit(tk,k=k2)-limit(tk,k=k1-1) fi;
#  RETURN(tmp)
 k1:=simplify(k1);
 k2:=simplify(k2+1);
 tmp2:=traperror(subs(k=k2,tk));
 if (tmp2=lasterror) or (tmp2=0) or (has(tmp2,infinity))
   then tmp2:=limit(tk,k=k2) fi;
 tmp1:=traperror(subs(k=k1,tk));
 if (tmp1=lasterror) or (tmp1=0) or (has(tmp1,infinity))
 then tmp1:=limit(tk,k=k1) fi;
 if tmp2=infinity and not(has(tmp1,{infinity,undefined})) then
   RETURN(infinity) fi;
 if tmp1=infinity and not(has(tmp2,{infinity,undefined})) then
   RETURN(-infinity) fi;
 if has(tmp2-tmp1,undefined) then RETURN(undefined)
 else RETURN(tmp2-tmp1) fi
fi;
end: #extended_gosper

WZcertificate:=proc(F,k,n)
local a,gos,m,l;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 if nargs>3 then m:=args[4] else m:=1 fi;
 if nargs>4 then l:=args[5] else l:=1 fi;
 a:=subs(n=n+m,F)-F;
 gos:=traperror(extended_gosper(a,k,l));
 if gos=lasterror then ERROR(`Extended WZ method fails`) fi;
 RETURN(simpcomb(gos/F));
end: # WZcertificate

zeilberger:=proc(F,k,sn)
local n,A,S,sigma,rat,p,q,r,upd,deg,f,b,j,var,rec,sol,num,den;
options remember,
`Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sn,function) then S:=op(0,sn); n:=op(1,sn) else n:=sn fi;
 A:=F+sigma[1]*subs(n=n+1,F);
 rat:=ratio(A,k);
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 # p:=1: q:=subs(k=k-1,numer(rat)): r:=subs(k=k-1,denom(rat)):
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=update(p,q,r,k);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=degreebound(p,q,r,k);
# Maple 13: if deg<0 then 
 if deg<-1 then  
   ERROR(`Algorithm finds no recurrence equation of first order`) 
 fi;
 f:=add(b[j]*k^j,j=0..deg);
 var:={sigma[1],seq(b[j],j=0..deg)};
 rec:=collect(subs(k=k+1,q)*f-r*subs(k=k-1,f)-p,k);
 sol:={solve({coeffs(rec,k)},var)};
 if sol={} then 
   ERROR(`Algorithm finds no recurrence equation of first order`) 
 fi;
 sigma[1]:=subs(op(1,sol),sigma[1]);
 sigma[1]:=normal(sigma[1]);
 for j from 0 to deg do
   sigma[1]:=subs(b[j]=0,sigma[1]);
 od;
 num:=factor(numer(sigma[1]));
 den:=factor(denom(sigma[1]));
 rec:=den*S(n)+num*S(n+1)=0;
 RETURN(rec);
end: # zeilberger

closedform:=proc(F,k,sn)
global SUM;
local zeilberg,S,n,rat,num,den,lc,numlist,denlist,j,i,init,cert;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 ## NEW BEGIN
 ## init:=simplify(subs(n=0,simplify(subs(k=0,F))));
 ## We could also submit several values n and use closedform recursively.
 ## If S is given as argument, we don't need a global variable
 if type(sn,function) then SUM:=op(0,sn); n:=op(1,sn) else n:=sn fi;
 ## Here we ASSUME that the summation is for k=0..infinity !
 init:=traperror(sum(subs(n=0,F),k=0..infinity));
 if init=lasterror then
   printf(`Warning: initial value cound not be computed!`);
   init:=SUM(0);
 fi;
 ## NEW END
 if init=0 then
   ERROR(`Shift necessary`);
 fi;
 if hastype([args[4..nargs]],identical(certificate)={identical(on),identical(true),identical(yes)}) then
   zeilberg:=sumrecursion(F,k,S(n),recorder=1..1,certificate=true);
   cert:= op(2,zeilberg);
   zeilberg:= op(1,zeilberg);
 else
   cert:= NULL;
   zeilberg:=zeilberger(F,k,S(n));
 fi;
 rat:=normal(solve(zeilberg,S(n+1))/S(n));
 num:=numer(rat);
 den:=denom(rat);
 lc:=lcoeff(num,n)/lcoeff(den,n);
 numlist:=normal([solve(num,n)]);
 numlist:=[seq(-j,j=numlist)];
 denlist:=normal([solve(den,n)]);
 denlist:=[seq(-j,j=denlist)];
 if member(1,denlist,'i') then
   denlist:= subsop(i=NULL,denlist);
 else
   numlist:= [op(numlist),1];
 fi;
 if (cert = NULL) then
   RETURN(eval(subs(pochhammer=`simpcomb/pochhammer`,
          init*hyperterm(numlist,denlist,lc,n))));
 else
   RETURN([eval(subs(pochhammer=`simpcomb/pochhammer`,
          init*hyperterm(numlist,denlist,lc,n))),cert]);
 fi;
end: # closedform

Closedform:=proc(F,k,sn)
global SUM;
local zeilberg,S,n,rat,num,den,lc,numlist,denlist,j,i,init,cert;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 ## NEW BEGIN
 ## init:=simplify(subs(n=0,simplify(subs(k=0,F))));
 ## We could also submit several values n and use closedform recursively.
 ## If S is given as argument, we don't need a global variable
 if type(sn,function) then SUM:=op(0,sn); n:=op(1,sn) else n:=sn fi;
 ## Here we ASSUME that the summation is for k=0..infinity !
 init:=traperror(sum(subs(n=0,F),k=0..infinity));
 if init=lasterror then
   printf(`Warning: initial value cound not be computed!`);
   init:=SUM(0);
 fi;
 ## NEW END
 if init=0 then
   ERROR(`Shift necessary`);
 fi;
 if hastype([args[4..nargs]],identical(certificate)={identical(on),identical(true),identical(yes)}) then
   zeilberg:=sumrecursion(F,k,S(n),recorder=1..1,certificate=true);
   cert:= op(2,zeilberg);
   zeilberg:= op(1,zeilberg);
 else
   cert:= NULL;
   zeilberg:=zeilberger(F,k,S(n));
 fi;
 rat:=normal(solve(zeilberg,S(n+1))/S(n));
 num:=numer(rat);
 den:=denom(rat);
 lc:=lcoeff(num,n)/lcoeff(den,n);
 numlist:=normal([solve(num,n)]);
 numlist:=[seq(-j,j=numlist)];
 denlist:=normal([solve(den,n)]);
 denlist:=[seq(-j,j=denlist)];
 if member(1,denlist,'i') then
   denlist:= subsop(i=NULL,denlist);
 else
   numlist:= [op(numlist),1];
 fi;
 if (cert = NULL) then
   RETURN(eval(subs(pochhammer=`simpcomb/pochhammer`,
          init*Hyperterm(numlist,denlist,lc,n))));
 else
   RETURN([eval(subs(pochhammer=`simpcomb/pochhammer`,
          init*Hyperterm(numlist,denlist,lc,n))),cert]);
 fi;
end: # Closedform

sumrecursion:=proc(F,k,sn)
local n,S,b,sigma,Rk,Rn,rat,p,q,r,upd,deg,f,i,j,jj,l,var,req,sol,num,den,J; 
options remember,
`Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sn,function) then S:=op(0,sn); n:=op(1,sn) else n:=sn fi;
Rk:=ratio(F,k);
Rn:=ratio(F,n);
for J from 1 to MAXORDER do
#  a:=F+sum(sigma['j']*subs(n=n+'j',F),'j'=1..J);
#  rat:=ratio(a,k);
 rat:=Rk*
   (1+add(sigma[j]*Product(subs(k=k+1,n=n+i,Rn),i=0..j-1),j=1..J))/
   (1+add(sigma[j]*Product(subs(n=n+i,Rn),i=0..j-1),j=1..J));
#  rat:=factor(value(rat));
 rat:=factor(eval(subs(Product=product,rat)));
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 # p:=1: q:=subs(k=k-1,numer(rat)): r:=subs(k=k-1,denom(rat)):
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=update(p,q,r,k);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=degreebound(p,q,r,k);
 # lprint(`degreebound = `,degreebound);
 # lprint(`length[p,q,r] =`,length(p),length(q),length(r));
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   f:=add(b[j]*k^j,j=0..deg);
   var:={seq(sigma[jj],jj=1..J),seq(b[jj],jj=0..deg)};
   req:=collect(subs(k=k+1,q)*f-r*subs(k=k-1,f)-p,k);
   sol:={solve({coeffs(req,k)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0}) 
   then
     req:=S(n)+add(sigma[j]*S(n+j),j=1..J);
     req:=subs(op(1,sol),req);
     req:=numer(normal(req));
     req:=collect(req,[seq(S(n+J-j),j=0..J)]);
     RETURN(map(factor,req)=0);
   fi;
 fi;
od;
ERROR(cat(`Algorithm finds no recurrence equation of order <= `,MAXORDER))
end: # sumrecursion

# 3-term recurrence equation of Wilson type
Sumrecursion:=proc(F,k,snx)
local n,x,S,a,b,A,B,C,Rk,Rn,rat,p,q,r,upd,deg,f,i,j,jj,l,var,req,sol,num,den,J;
options remember,
`Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(snx,function) then
 S:=op(0,snx); n:=op(1,snx); x:=op(2,snx);
else
 n:=op(1,snx); x:=op(2,snx);
fi;
 a:=F-(A*subs(n=n+1,F)-(A+C)*F+C*subs(n=n-1,F));
 rat:=ratio(a,k);
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 # p:=1: q:=subs(k=k-1,numer(rat)): r:=subs(k=k-1,denom(rat)):
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=update(p,q,r,k);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=degreebound(p,q,r,k);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   f:=add(b[j]*k^j,j=0..deg);
   var:={A,C,seq(b[jj],jj=0..deg)};
   req:=collect(subs(k=k+1,q)*f-r*subs(k=k-1,f)-p,k);
   sol:={solve({coeffs(req,k)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0})
   then
     sol:=factor(sol);
     A:=subs(op(sol),A);
     B:=1;
     C:=subs(op(sol),C);
     for i from 1 to nops(denom(A)) do
       if has(op(i,denom(A)),x) then
         B:=B*op(i,denom(A));
       fi;
     od;
     if not(B=1) then
       A:=factor(B*A);
       C:=factor(B*C);
     fi;
     RETURN(B*S(n,x)=A*S(n+1,x)-(A+C)*S(n,x)+C*S(n-1,x));
   fi;
 fi;
ERROR(`No Wilson type 3-term recurrence equation found`)
end: # Sumrecursion

delta:=proc(f,x) subs(x=x+1,f)-f end:
nabla:=proc(f,x) f-subs(x=x-1,f) end:

sumdeltanabla:=proc(F,k,sx)
local a,x,S,b,sigma,tau,rat,p,q,r,upd,deg,f,i,j,jj,l,var,req,sol,num,den,J;
options remember,
`Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sx,function) then S:=op(0,sx); x:=op(1,sx) else x:=sx fi;
 a:=F+tau*nabla(F,x)+sigma*delta(nabla(F,x),x);
 rat:=ratio(a,k);
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 p:=1: q:=subs(k=k-1,numer(rat)): r:=subs(k=k-1,denom(rat)):
 upd:=update(p,q,r,k);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=degreebound(p,q,r,k);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   f:=add(b[j]*k^j,j=0..deg);
   var:={sigma,tau,seq(b[jj],jj=0..deg)};
   req:=collect(subs(k=k+1,q)*f-r*subs(k=k-1,f)-p,k);
   sol:={solve({coeffs(req,k)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0})
   then
     req:=S(x)+tau*Nabla(S(x),x)+sigma*Delta(Nabla(S(x),x),x);
     req:=subs(op(1,sol),req);
     req:=numer(normal(req));
     req:=collect(req,[Delta(Nabla(S(x),x),x),Nabla(S(x),x),S(x)]);
     RETURN(map(factor,req)=0);
   fi;
 fi;
ERROR(`Algorithm finds no difference equation of second order`)
end: # sumdeltanabla

`sumdelta+nabla`:=proc(F,k,sx)
local
a,x,S,b,sigma,sigmaplustau,rat,p,q,r,upd,deg,f,i,j,jj,l,var,req,sol,num,den,J;
options remember,
`Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sx,function) then S:=op(0,sx); x:=op(1,sx) else x:=sx fi;
 a:=(sigmaplustau)*delta(F,x)-sigma*nabla(F,x)+F;
 rat:=ratio(a,k);
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 p:=1: q:=subs(k=k-1,numer(rat)): r:=subs(k=k-1,denom(rat)):
 upd:=update(p,q,r,k);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=degreebound(p,q,r,k);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   f:=add(b[j]*k^j,j=0..deg);
   var:={sigma,sigmaplustau,seq(b[jj],jj=0..deg)};
   req:=collect(subs(k=k+1,q)*f-r*subs(k=k-1,f)-p,k);
   sol:={solve({coeffs(req,k)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0})
   then
     req:=(sigmaplustau)*Delta(S(x),x)-sigma*Nabla(S(x),x)+S(x);
     req:=subs(op(1,sol),req);
     req:=numer(normal(req));
     req:=collect(req,[Delta(S(x),x),Nabla(S(x),x),S(x)]);
     RETURN(map(factor,req)=0);
   fi;
 fi;
ERROR(`Algorithm finds no difference equation of second order`)
end: # sumdelta+nabla

sumdiffeq:=proc(F,k,sx)
local x,S,a,b,sigma,rat,p,q,r,upd,deg,f,j,jj,l,var,deq,sol,num,den,J,cert;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sx,function) then S:=op(0,sx); x:=op(1,sx) else x:=sx fi;
for J from 1 to MAXORDER do
 a:=F+add(sigma[j]*diff(F,x$j),j=1..J);
 rat:=simpcomb(a/subs(k=k-1,a));
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 #p:=1: q:=subs(k=k-1,numer(rat)): r:=subs(k=k-1,denom(rat)):
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=update(p,q,r,k);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=degreebound(p,q,r,k);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   f:=add(b[j]*k^j,j=0..deg);
   var:={seq(sigma[jj],jj=1..J),seq(b[jj],jj=0..deg)};
   deq:=collect(subs(k=k+1,q)*f-r*subs(k=k-1,f)-p,k);
   sol:={solve({coeffs(deq,k)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0}) 
   then
     deq:=S(x)+add(sigma[j]*diff(S(x),x$j),j=1..J);
     deq:=normal(subs(op(1,sol),deq));
	    if hastype([args[4..nargs]],identical(certificate)={identical(on),identical(true),identical(yes)}) then
				cert:= simpcomb(denom(deq)*subs(op(1,sol),r/p*subs(k=k-1,f)*a)/F);
     else
       cert:= NULL;
     fi;
     deq:=numer(deq);
     deq:=collect(deq,[seq(diff(S(x),x$(J-j)),j=0..J-1),S(x)]);
     if (cert = NULL) then
       RETURN(map(factor,deq)=0);
     else
       RETURN([map(factor,deq)=0,cert]);
     fi;
   fi;
 fi;
od;
ERROR(cat(`Algorithm finds no differential equation of order <= `,MAXORDER))
end: # sumdiffeq

sumdiffrule:=proc(F,k,snx)
local n,x,S,a,b,sigma,rat,p,q,r,upd,deg,f,j,jj,l,var,req,sol,sol2,num,den,J;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(snx,function) then 
 S:=op(0,snx); n:=op(1,snx); x:=op(2,snx); 
else 
 n:=op(1,snx); x:=op(2,snx); 
fi;
for J from 1 to MAXORDER do
 a:=diff(F,x)-add(sigma[j]*subs(n=n+j,F),j=0..J);
 rat:=ratio(a,k);
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 # p:=1: q:=subs(k=k-1,numer(rat)): r:=subs(k=k-1,denom(rat)):
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=update(p,q,r,k);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=degreebound(p,q,r,k);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   f:=add(b[j]*k^j,j=0..deg);
   var:={seq(sigma[jj],jj=0..J),seq(b[jj],jj=0..deg)};
   req:=collect(subs(k=k+1,q)*f-r*subs(k=k-1,f)-p,k);
   sol:={solve({coeffs(req,k)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0})
   then
     sol2:=add(sigma[j]*S(n+j,x),j=0..J);
     sol2:=subs(op(1,sol),sol2);
     RETURN(diff(S(n,x),x)=map(factor,sol2));
   fi;
 fi;
od;
ERROR(cat(`Algorithm finds no derivative rule of order <= `,MAXORDER))
end: # sumdiffrule

# BEGIN sumrecursion, inhomogeneous, Harald Boeing
# ----------------------------------------------------------------------

## Maple 6: no longer supported
## `hsum/mapleversion`:= proc()
##	local l;
##	l:= searchtext(`release`,interface(version));
##	parse(substring(interface(version),l+7..l+8));
## end:

# ----------------------------------------------------------------------

`sumrecursion/arguments`:= proc(\
		arg,F,k,sumrange,n,S,unsub,CERTIFICATE,ORDER)
	local kk,srange,svars,nn,j,i,z;
	global MAXORDER;
	options `Copyright 1998  Wolfram Koepf & Harald Boeing`;
	if (nops(arg) < 3) or not(type([seq(arg[j],j=1..3)],\
		[algebraic,{name,name=range},{name,function(name)}])) then
		ERROR(`Wrong type of arguments.`);
	fi;
	if type(arg[3],name) then
		nn:= arg[3];
	else
		nn:= op(1,arg[3]);
		S:= op(0,arg[3]);
	fi;
	if type(arg[2],name) then
		k:= arg[2];
		n:= nn;
		F:= arg[1];
		sumrange:= [];
		unsub:= [];
	else
		kk:= op(1,arg[2]);
		srange:= op(2,arg[2]);
		svars:= indets(srange);
		if svars <> {} then
         if not(type(srange,range({seq(linear(svars[i]),i=1..nops(svars)),constant}))) then
            ERROR(`Sorry! Only linear summation ranges are supported yet.`);
	      fi;
		fi;
		F:= `sumrecursion/assumptions`(\
			arg[1],srange,kk,nn,k,sumrange,n,'unsub');
	fi;
	z:= select(type,arg,identical(recorder)={posint,posint..posint});
	if (z = []) then
		ORDER:= [seq(j,j=1..MAXORDER)];
	elif type(op(2,z[1]),posint) then
		ORDER:= [seq(j,j=1..op(2,z[1]))];
	else
		ORDER:= [seq(j,j=op(2,z[1]))];
	fi;
	CERTIFICATE:= hastype(arg,identical(certificate)=\
		{identical(on),identical(true),identical(yes)});
	NULL;
end:

# ----------------------------------------------------------------------

sumrecursion:= proc()
	local F,S,k,sumrange,n,Rk,constRn,Rn,Poly,J,sigma,sigmafac,f,pqr,\
		deg,b,var,req,sol,j,cert,const,unsub,tim,certificate,recorder;
	options `Copyright 1998  Wolfram Koepf & Harald Boeing`;
	tim:= time();
	`sumrecursion/arguments`([args],F,k,sumrange,n,S,unsub,certificate,recorder);
	Rk:= ratio(F,k);
	constRn:= ratio(F,n);
	userinfo(1,proofsum,\
		cat(`The summand F(`,n,`,`,k,`) is defined as`),\
		print('F'(n,k)=F),`with the ratios`,\
		print('F'(n,k+1)/'F'(n,k)=Rk),`and`,\
		print('F'(n+1,k)/'F'(n,k)=constRn));
	if not type([Rk,constRn],list(ratpoly(anything,k))) then
		ERROR(`Algorithm not applicable.`)
	fi;
	Rn:= unapply(select(has,'b*j'*constRn,k), n);
	constRn:= unapply(constRn/Rn(n), n);
	Poly:= 1;
	for J in recorder do
		sigmafac[J]:=  1 / product('factor(constRn(n+j))','j'=0..J-1);
		Poly:= normal(Poly + sigma[J]*(product('Rn(n+j)','j'=0..J-1)));
		f:= denom(Poly);
		f:= factor(subs(k=k-1,Rk*f) / f);
		pqr:= update(numer(Poly),numer(f),denom(f),k);
		deg:= degreebound(op(pqr),k);
# Maple 13: if deg>=0 then   
                       if deg>=-1 then
			f:= add(b[j]*k^j,j=0..deg);
			var:= {seq(sigma[j],j=1..J),seq(b[j],j=0..deg)};
			req:= expand(subs(k=k+1,pqr[2])*f-pqr[3]*subs(k=k-1,f)-pqr[1]);
			sol:= {solve({coeffs(req,k)},var)};
			if (sol <> {}) then
				req:= S(n) + add(sigmafac[j]*sigma[j]*S(n+j),j=1..J);
				req:= normal(subs(sol[1],req));
				cert:= denom(req)*\
					subs(sol[1], pqr[3]/pqr[1]*subs(k=k-1,f)*Poly);
				req:= subs(unsub,`sumrecursion/inhomo`(\
					numer(req),F,n,k,sumrange,cert,S,J,Rk,certificate));
				userinfo(1,sumrecursion,time()-tim, `seconds CPU time`, print());
				# RETURN(req); Feb 28, 2004: Maple 9 needs another evaluation
				RETURN(eval(req));
			fi;
		fi;
	od;
	ERROR(cat(`Algorithm finds no recurrence equation of order smaller than `,max(op(recorder))+1));
end: # sumrecursion

# ----------------------------------------------------------------------

`sumrecursion/inhomo`:= proc(re,Fu,n,k,sumrange,ce,S,J,Rk,certificate)
	local rec, cert, j, Func, stamm, lo_bound, up_bound, rec_args,\
		sigma, lo, up, RHS, i, tim, F, G;
	option `Copyright (c) 1997 by Harald Boeing & Wolfram Koepf.`;
	tim:= time();
	rec_args:= [seq(S(n+J-j),j=0..J)];
	# rec:= sort(map(factor,collect(re,rec_args)),rec_args);
	# Dec 01, 2003, Maple 9: collect w.r.t. S instead of rec_args
	# sort does not work either
	rec:= sort(map(factor,collect(re,S)),rec_args);
	if (sumrange = []) then
		if certificate then
			RETURN([rec=0,factor(ce)]);
		else
			RETURN(rec=0);
		fi;
	fi;
	cert:= factor(ce);
	userinfo(1,proofsum,\
		`It satisfies the inhomogeneous recurrence equation:`,\
		print(eval(subs(S=unapply(F(n,k),n),rec))=G(n,k+1)-G(n,k)),\
		`where`,print(G(n,k)=cert*Fu));
	Func:= simpcomb(Fu);
	stamm:=`simpcomb/GAMMA/combine`(cert*Func);
	Func:= `sumrecursion/hypfunc`(Func,Rk,k,n);
	stamm:= `sumrecursion/hypfunc`(stamm,factor(ratio(cert,k)*Rk),k,n);
	lo_bound:= unapply('expand'(op(1,sumrange)), n);
	up_bound:= unapply('expand'(op(2,sumrange)), n);
	rec_args:= [op(map(op, indets(rec, specfunc(anything,S))))];
	sigma:= table([seq(i=coeff(rec,S(i)), i=rec_args)]);
	lo:= min(seq(lo_bound(i), i= rec_args));
	up:= max(seq(up_bound(i), i= rec_args));
	RHS:= stamm(n,up+1)-stamm(n,lo);
# change from sum to add with shift, (add(#,k=n-1..n) not allowed), sum yields wrong results
	for i in rec_args do
		if lo_bound(i) <> lo then
#			RHS:= RHS - sigma[i]*sum(Func(i,j), j=lo..lo_bound(i)-1);
			RHS:= RHS - sigma[i]*add(Func(i,j+lo), j=0..lo_bound(i)-1-lo);
		fi;
		if up_bound(i) <> up then
#			RHS:= RHS - sigma[i]*sum(Func(i,j), j=up_bound(i)+1..up);
			RHS:= RHS - sigma[i]*add(Func(i,j+(up_bound(i)+1)), j=0..up-(up_bound(i)+1));
		fi;
		if has(RHS,{limit,undefined}) then
			ERROR(`Unknown limit encountered.`);
		fi;
	od;
	RHS:= (simpcomb(RHS));
	userinfo(1,proofsum,sprintf(`By summing this equation over %a from \
%a to %a we obtain:`,k,lo,up),print(rec=RHS),\
	`where`,print(S(n)=Sum(Fu,k=sumrange)));
	userinfo(3, hsum, `Inhomogeneous part of recursion:`, print(RHS));
	userinfo(1,sumrecursion,time()-tim,`seconds CPU time`,print());
	if certificate then
		RETURN([rec=RHS,cert]);
	else
		RETURN(rec=RHS);
	fi;
end:

# ----------------------------------------------------------------------

`sumrecursion/assumptions`:= proc(F,srange,kk,nn,k,sumrange,n,UNSUB)
	local tmp, sub;
	tmp:= `sumrecursion/assume+subs`(\
		[F,srange,kk,nn],[kk,integer,nn,posint],'sub');
	if not(is(expand(op(2,tmp[2])-op(1,tmp[2]))>0)) then
		tmp:= `sumrecursion/assume+subs`(\
			[F,srange,kk,nn],[kk,integer,nn,negint],'sub');
		if not(is(expand(op(2,tmp[2])-op(1,tmp[2]))>0)) then
			tmp:= `sumrecursion/assume+subs`(\
				[F,srange,kk,nn],[kk,integer,nn,integer],'sub');
		fi;
	fi;
	sumrange:= tmp[2];
	k:= tmp[3];
	n:= tmp[4];
	UNSUB:= sub;
	RETURN(tmp[1]);
end:
	
# ----------------------------------------------------------------------

`sumrecursion/assume+subs/localvar`:= proc(z,assum)
	local s,l;
	s:= convert(z,string);
	l:= length(s);
	# Note that substring in MapleV3 only takes a range as 2. argument!
	if (substring(s,l..l) = `~`) then
		s:= substring(s,1..l-1);
	fi;
	eval(parse(cat(`(proc() local `,s,`;assume(`,s,`,`,assum,`);`,\
		s,` end)();`)));
end:

# ----------------------------------------------------------------------

`sumrecursion/assume+subs`:= proc(z, var, unsub)
	local i, sub;
	sub:= {seq(var[2*i-1]=`sumrecursion/assume+subs/localvar`(\
		var[2*i-1],var[2*i]), i=1..nops(var)/2)};
	unsub:= {seq(op(2,i)=op(1,i),i=sub)};
	subs(sub,z);
end:

# ----------------------------------------------------------------------

`sumrecursion/singularities`:= proc(poly,k)
	local p, N, i;
	option `Copyright (c) 1998 by Harald Boeing & Wolfram Koepf.`;
	p:= select(has, [op(poly*'i*j')], k);
	# determine roots of poly, dropping any already known muliplicities
	p:= map(proc(z,k) if type(z,anything^integer) then\
		solve(op(1,z),k) else solve(z,k) fi end, p,k);
	select(proc(z) if is(z,integer) then true else false fi end,\
		normal(p));
end:

# ----------------------------------------------------------------------

## Maple 6: no longer supported
## if (`hsum/mapleversion`() > 3) then

`sumrecursion/hypfunc`:= proc(F,Rk,k,n)
	local Func, num, den, j, tim, z, locvar;
	option `Copyright (c) 1998 by Harald Boeing & Wolfram Koepf.`;
	tim:= time();
	num:= `sumrecursion/singularities`(numer(Rk),k);
	den:= `sumrecursion/singularities`(denom(Rk),k);
	num:= max(op(num));
	den:= min(seq(j+1,j=den));
	locvar:= `_tmp$$$`;
	Func:= sprintf(`%a:=traperror(%a); if (%a=lasterror) then \
limit(%a,%a) else %a fi`,locvar,F,locvar,subs(k=z,F),z=k, locvar);
	if has([num,den],max) then         	 # -infinity..infinity;
		Func:= sprintf(\
`if type(%a,{identical(-infinity),identical(infinity)}) then \
limit(%a,%a=%a) \
else %s fi`,\
k,subs(k=z,F),z,k,Func);
	elif is(den < num) or is(den-select(type,den+'j+j^2',integer) < \
		num-select(type,num+'j+j^2',integer)) then # den..num
		# the second or is to recognize boundaries like 1..z, where
		# z is a posint, i.e. is(1<z) would be false in general, but true
		# in nearly all cases
		Func:= sprintf(\
### WARNING: %x or %X format should be %y or %Y if used with floating point arguments
`if type(%a,{identical(-infinity),identical(infinity)}) or \
(is(expand(%a),integer) and is(%a)) or \
(is(expand(%a),integer) and is(%a)) then 0 else %s fi`,\
k,k-den,k<den,k-num,k>num,Func);
	elif (num = -infinity) then          # den..infinity
		Func:= sprintf(
### WARNING: %x or %X format should be %y or %Y if used with floating point arguments
`if type(%a,identical(-infinity)) or \
(is(expand(%a),integer) and is(%a)) then 0 \
elif type(%a,identical(infinity)) then limit(%a,%a=infinity) \
else %s fi`,\
k,k-den,k<den,k,subs(k=z,F),z,Func);
	elif (den = infinity) then           # -infinity..num
userinfo(1,proofsum,`den is infinity`);
		Func:= sprintf(\
### WARNING: %x or %X format should be %y or %Y if used with floating point arguments
`if type(%a,identical(infinity)) or \
(is(expand(%a),integer) and is(%a)) then 0 \
elif type(%a,identical(-infinity)) then limit(%a,%a=-infinity) \
else %s fi`,\
k,k-num,num<k,k,subs(k=z,F),z,Func);
	else                                 # -infinity..infinity
		Func:= sprintf(\
`if type(%a,{identical(-infinity),identical(infinity)}) then \
limit(%a,%a=%a) \
else %s fi`,\
k,subs(k=z,F),z,k,Func);
	fi;
	Func:= parse(sprintf(`proc(%a,%a) local %a; %s end;`,n,k,locvar,Func));
	userinfo(1,sumrecursion,time()-tim, `seconds CPU time`, print());
	RETURN(eval(Func));
end:

########################################################################
## else ##### alternative version of `sumrecursion/hypfunc` for Release 3 #
########################################################################

## Maple 6: no longer supported
## `sumrecursion/hypfunc`:= proc(F,Rk,k,n)
## 	local Func, num, den, j, tim, z, locvar;
## 	option `Copyright (c) 1998 by Harald Boeing & Wolfram Koepf.`;
## 	unapply('limit'(subs(k=z,F),z=k),n,k);
## end:
## 
## fi:

# ----------------------------------------------------------------------
#
#certify:= proc(F,k,sn)
#	local rec, stamm, S;
#	rec:= sumrecursion(F,k,sn,certificate=true);
#	S:= op(0,sn);
#	stamm:= F*rec[2];
#	rec:= eval(subs(S=unapply(F,op(1,sn)),op(1,rec[1])));
#	simpcomb(rec - subs(k=k+1,stamm) + stamm);
#end:
#
# ----------------------------------------------------------------------
# END sumrecursion, inhomogeneous

sumintrule:=proc(F,k,snx)
local n,x,S,a,b,sigma,rat,p,q,r,upd,deg,f,j,jj,l,var,req,sol,pol,coefflist,
DS,sol2,num,den;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(snx,function) then 
 S:=op(0,snx); n:=op(1,snx); x:=op(2,snx); 
else 
 n:=op(1,snx); x:=op(2,snx); 
fi;
 a:=F-add(sigma[j]*diff(subs(n=n+j,F),x),j=-1..1);
 rat:=ratio(a,k);
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 # p:=1: q:=subs(k=k-1,numer(rat)): r:=subs(k=k-1,denom(rat)):
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=update(p,q,r,k);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=degreebound(p,q,r,k);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   f:=add(b[j]*k^j,j=0..deg);
   var:={seq(sigma[jj],jj=-1..1),seq(b[jj],jj=0..deg)};
   req:=collect(subs(k=k+1,q)*f-r*subs(k=k-1,f)-p,k);
   sol:={solve({coeffs(req,k)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0})
   then
     req:=S(n,x)-add(sigma[j]*DS(n+j,x),j=-1..1);
     req:=subs(op(1,sol),req);
     req:=numer(normal(req));
     req:=collect(req,S(n,x));
     pol:=collect(coeff(req,S(n,x)),x);
     coefflist:={coeffs(pol,x)} minus {coeff(pol,x,0)}; 
     for j from -1 to 1 do
       req:=collect(req,DS(n+j,x));
       pol:=collect(coeff(req,DS(n+j,x)),x);
       coefflist:={op(coefflist),op({coeffs(pol,x)} minus {coeff(pol,x,0)})};
     od;
     sol2:={solve(coefflist,indets(var) minus {x,n})};
     if sol2={} then ERROR(`No such identity exists`) fi;
     req:=add(sigma[j]*S(n+j,x),j=-1..1);
     req:=subs(op(1,sol),req);
     req:=subs(op(1,sol2),req);
     req:=map(factor,req);
     RETURN(Int(S(n,x),x)=req);
   fi;
 fi;
ERROR(`Algorithm finds no integration rule`)
end: # sumintrule

sumholodiffeq:=proc(F,k,x,J)
local a,b,sigma,rat,p,q,r,upd,deg,f,j,jj,l,var,deq,sol,num,den; # global S;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 unassign(b,sigma);
 a:=F+add(sigma[j]*diff(F,x$j),j=1..J);
 rat:=ratio(a,k);
 if not type(rat,ratpoly(anything,k)) then
   ERROR(`Algorithm not applicable`)
 fi;
 # p:=1: q:=subs(k=k-1,numer(rat)): r:=subs(k=k-1,denom(rat)):
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=update(p,q,r,k);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=degreebound(p,q,r,k);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   f:=add(b[j]*k^j,j=0..deg);
   var:={seq(sigma[jj],jj=1..J),seq(b[jj],jj=0..deg)};
   deq:=collect(subs(k=k+1,q)*f-r*subs(k=k-1,f)-p,k);
   sol:={solve({coeffs(deq,k)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0}) 
   then
     assign(sol);
     deq:=S(x)+add(sigma[j]*diff(S(x),x$j),j=1..J);
     deq:=numer(normal(deq));
     deq:=collect(deq,[seq(diff(S(x),x$(J-j)),j=0..J-1),S(x)]);
     RETURN(map(factor,deq)=0);
   fi;
 fi;
ERROR(`Algorithm finds no differential equation of order`,J)
end: # sumholodiffeq

contratio:=proc(f,x)
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 simpcomb(diff(f,x)/f);
end: # contratio

contdispersionset:=proc(q,r,x)
# finds the nonnegative integer dispersion values j
local j,res,s,l;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 res:=frontend(resultant,[r,q-j*diff(r,x),x]); # (11.2)
 s:=simplify({solve(res,j)});
 l:={};
 for j in s do
   if type(j,nonnegint) then l:=l union {j} fi;
 od;
 RETURN(convert(l,set))
end: # contdispersionset

contupdate:=proc(p,q,r,x)
# updates the triple [p,q,r] according to gcd-condition
local dis,g,pnew,qnew,rnew,j;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 g:=frontend(gcd,[r,q-diff(r,x)]); # (11.2), j=1
 if has(g,x) then
   pnew:=normal(p*g); # (11.3), j=1
   qnew:=normal(diff(r/g,x)+(q-diff(r,x))/g); # (11.3), j=1
   rnew:=normal(r/g); # (11.3), j=1
 else
   pnew:=p; qnew:=q; rnew:=r;
 fi;
 dis:=contdispersionset(qnew,rnew,x);
 for j in dis do
   g:=frontend(gcd,[rnew,qnew-j*diff(rnew,x)]); # (11.2)
   if has(g,x) then
     pnew:=normal(pnew*g^j); # (11.3)
     qnew:=normal(j*diff(rnew/g,x)+(qnew-j*diff(rnew,x))/g); # (11.3)
     rnew:=normal(rnew/g); # (11.3)
   fi;
 od;
 RETURN([pnew,qnew,rnew]);
end: # contupdate

contdegreebound:=proc(p,q,r,x)
# calculates the degree bound for f
local pol1,pol2,deg1,deg2,a,b;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 pol1:=collect(r,x);
 pol2:=collect(q+diff(r,x),x);
 if pol1=0 then deg1:=-1 else deg1:=degree(pol1,x) fi;
 if pol2=0 then deg2:=-1 else deg2:=degree(pol2,x) fi;
 if deg1<=deg2 then RETURN(degree(p,x)-deg2) fi;
 a:=coeff(pol1,x,deg1);
 b:=coeff(pol2,x,deg1-1);
 if not(type(-b/a,nonnegint)) then RETURN(degree(p,x)-deg1+1) else
   RETURN(max(-b/a,degree(p,x)-deg1+1)) fi;
end: # contdegreebound

contfindf:=proc(p,q,r,x)
# finds ftilde, given the triple [p,q,r]
local deg,ftilde,a,j,deq,sol,result;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 deg:=contdegreebound(p,q,r,x);
 if deg<0 then ERROR(`No polynomial ftilde exists`) fi;
 ftilde:=add(a[j]*x^j,j=0..deg);
 deq:=collect((q+diff(r,x))*ftilde+r*diff(ftilde,x)-p,x); # (11.5)
 sol:={solve({coeffs(deq,x)},{seq(a[j],j=0..deg)})};
 if sol={} then ERROR(`No polynomial ftilde exists`) else
   result:=subs(op(1,sol),ftilde);
 fi;
 for j from 0 to deg do
   result:=subs(a[j]=0,result);
 od;
 RETURN(result);
end: # contfindf

contgosper:=proc(f,x)
# implements the continuous version of Gosper's algorithm
local rat,p,q,r,pqr,ftilde;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 rat:=contratio(f,x);
 if not type(rat,ratpoly(anything,x)) then
   ERROR(`Algorithm not applicable`)
 fi;
 p:=1; q:=numer(rat); r:=denom(rat);
 pqr:=contupdate(p,q,r,x);
 p:=op(1,pqr); q:=op(2,pqr); r:=op(3,pqr);
 ftilde:=traperror(contfindf(p,q,r,x));
 if ftilde=lasterror then
   ERROR(`No hyperexponential antiderivative exists`)
 else
   RETURN(normal(r*ftilde*f/p))
 fi; # (11.4)
end: # contgosper

intrecursion:=proc(F,t,sn)
local S,n,f,b,sigma,rat,p,q,r,upd,deg,ftilde,j,jj,l,var,req,sol,
num,den,J,cert; 
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sn,function) then S:=op(0,sn); n:=op(1,sn) else n:=sn fi;
for J from 1 to MAXORDER do
 f:=F+add(sigma[j]*subs(n=n+j,F),j=1..J);
 rat:=contratio(f,t);
 if not type(rat,ratpoly(anything,t)) then
   ERROR(`Algorithm not applicable`)
 fi;
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=contupdate(p,q,r,t);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=contdegreebound(p,q,r,t);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   ftilde:=add(b[j]*t^j,j=0..deg);
   var:={seq(sigma[jj],jj=1..J),seq(b[jj],jj=0..deg)};
   req:=collect((q+diff(r,t))*ftilde+r*diff(ftilde,t)-p,t);
   sol:={solve({coeffs(req,t)},var)};
   if not(sol={} or 
      {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0}) 
   then
     req:=S(n)+add(sigma[j]*S(n+j),j=1..J);
     req:=normal(subs(op(1,sol),req));
	    if hastype([args[4..nargs]],identical(certificate)={identical(on),identical(true),identical(yes)}) then
				cert:= simpcomb(denom(deq)*subs(op(1,sol),r/p*subs(k=k-1,f)*f)/F);
     else
       cert:= NULL;
     fi;
     req:=collect(numer(req),[seq(S(n+J-j),j=0..J)]);
     if (cert = NULL) then
       RETURN(map(factor,req)=0);
     else
       RETURN([map(factor,req)=0,cert]);
     fi;
   fi;
 fi;
od;
ERROR(cat(`Algorithm finds no recurrence equation of order <= `,MAXORDER))
end: # intrecursion

defintrecursion:=proc(F,t,sn,a,b)
local S,n,f,bb,sigma,rat,p,q,r,upd,deg,ftilde,j,jj,l,var,req,sol,
num,den,J,G,RHS; 
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sn,function) then S:=op(0,sn); n:=op(1,sn) else n:=sn fi;
for J from 1 to MAXORDER do
 unassign(bb,sigma);
 f:=F+add(sigma[j]*subs(n=n+j,F),j=1..J);
 rat:=contratio(f,t);
 if not type(rat,ratpoly(anything,t)) then
   ERROR(`Algorithm not applicable`)
 fi;
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=contupdate(p,q,r,t);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=contdegreebound(p,q,r,t);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   ftilde:=add(bb[j]*t^j,j=0..deg);
   var:={seq(sigma[jj],jj=1..J),seq(bb[jj],jj=0..deg)};
   req:=collect((q+diff(r,t))*ftilde+r*diff(ftilde,t)-p,t);
   sol:={solve({coeffs(req,t)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0}) 
   then
     assign(sol);
     req:=S(n)+add(sigma[j]*S(n+j),j=1..J);
     req:=numer(normal(req));
     req:=collect(req,[seq(S(n+J-j),j=0..J)]);
     G:=factor(r*ftilde*f/p);
     RHS:=factor(limit(G,t=b)-limit(G,t=a));
     RETURN(map(factor,req)=RHS);
   fi;
 fi;
od;
ERROR(cat(`Algorithm finds no recurrence equation of order <= `,MAXORDER))
end: # defintrecursion

intdiffeq:=proc(F,t,sx)
local x,S,f,b,sigma,rat,p,q,r,upd,deg,ftilde,j,jj,l,var,deq,sol,
num,den,J,cert;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sx,function) then S:=op(0,sx); x:=op(1,sx) else x:=sx fi;
for J from 1 to MAXORDER do
 f:=F+add(sigma[j]*diff(F,x$j),j=1..J);
 rat:=contratio(f,t);
 if not type(rat,ratpoly(anything,t)) then
   ERROR(`Algorithm not applicable`)
 fi;
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=contupdate(p,q,r,t);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=contdegreebound(p,q,r,t);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   ftilde:=add(b[j]*t^j,j=0..deg);
   var:={seq(sigma[jj],jj=1..J),seq(b[jj],jj=0..deg)};
   deq:=collect((q+diff(r,t))*ftilde+r*diff(ftilde,t)-p,t);
   sol:={solve({coeffs(deq,t)},var)};
   if not(sol={} or 
      {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0}) 
   then
     deq:=S(x)+add(sigma[j]*diff(S(x),x$j),j=1..J);
     deq:=normal(subs(op(1,sol),deq));
	    if hastype([args[4..nargs]],identical(certificate)={identical(on),identical(true),identical(yes)}) then
				cert:= simpcomb(denom(deq)*subs(op(1,sol),r/p*ftilde*f)/F);
     else
       cert:= NULL;
     fi;
     deq:=numer(deq);
     deq:=collect(deq,[seq(diff(S(x),x$(J-j)),j=0..J-1),S(x)]);
     deq:=numer(normal(deq));
     deq:=collect(deq,[seq(diff(S(x),x$(J-j)),j=0..J-1),S(x)]);
     if (cert = NULL) then
       RETURN(map(factor,deq)=0);
     else
       RETURN([map(factor,deq)=0,cert]);
     fi;
   fi;
 fi;
od;
ERROR(cat(`Algorithm finds no differential equation of order <= `,MAXORDER))
end: # intdiffeq

defintdiffeq:=proc(F,t,sx,a,b)
local x,S,f,bb,sigma,rat,p,q,r,upd,deg,ftilde,j,jj,l,var,deq,sol,
num,den,J,G,RHS; 
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sx,function) then S:=op(0,sx); x:=op(1,sx) else x:=sx fi;
for J from 1 to MAXORDER do
 unassign(bb,sigma);
 f:=F+add(sigma[j]*diff(F,x$j),j=1..J);
 rat:=contratio(f,t);
 if not type(rat,ratpoly(anything,t)) then
   ERROR(`Algorithm not applicable`)
 fi;
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=contupdate(p,q,r,t);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=contdegreebound(p,q,r,t);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   ftilde:=add(bb[j]*t^j,j=0..deg);
   var:={seq(sigma[jj],jj=1..J),seq(bb[jj],jj=0..deg)};
   deq:=collect((q+diff(r,t))*ftilde+r*diff(ftilde,t)-p,t);
   sol:={solve({coeffs(deq,t)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0}) 
   then
     assign(sol);
     deq:=S(x)+add(sigma[j]*diff(S(x),x$j),j=1..J);
     deq:=numer(normal(deq));
     deq:=collect(deq,[seq(diff(S(x),x$(J-j)),j=0..J-1),S(x)]);
     G:=factor(r*ftilde*f/p);
     RHS:=factor(limit(G,t=b)-limit(G,t=a));
     RETURN(map(factor,deq)=RHS);
   fi;
 fi;
od;
ERROR(cat(`Algorithm finds no differential equation of order <= `,MAXORDER))
end: # defintdiffeq

intdiffrule:=proc(F,t,snx)
local n,x,S,f,b,sigma,rat,p,q,r,upd,deg,ftilde,j,jj,l,var,req,sol,num,den,J; 
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(snx,function) then 
 S:=op(0,snx); n:=op(1,snx); x:=op(2,snx); 
else 
 n:=op(1,snx); x:=op(2,snx); 
fi;
for J from 1 to MAXORDER do
 unassign(b,sigma);
 f:=diff(F,x)-add(sigma[j]*subs(n=n+j,F),j=0..J);
 rat:=contratio(f,t);
 if not type(rat,ratpoly(anything,t)) then
   ERROR(`Algorithm not applicable`)
 fi;
 p:=1: q:=numer(rat): r:=denom(rat):
 upd:=contupdate(p,q,r,t);
 p:=op(1,upd): q:=op(2,upd): r:=op(3,upd):
 deg:=contdegreebound(p,q,r,t);
# Maple 13: if deg>=0 then   
 if deg>=-1 then
   ftilde:=add(b[j]*t^j,j=0..deg);
   var:={seq(sigma[jj],jj=0..J),seq(b[jj],jj=0..deg)};
   req:=collect((q+diff(r,t))*ftilde+r*diff(ftilde,t)-p,t);
   sol:={solve({coeffs(req,t)},var)};
   if not(sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0}) 
   then
     assign(sol);
     sol:=add(factor(sigma[j])*S(n+j,x),j=0..J);
     RETURN(diff(S(n,x),x)=map(factor,sol));
   fi;
 fi;
od;
ERROR(cat(`Algorithm finds no derivative rule of order <= `,MAXORDER))
end: # intdiffrule

intWZcertificate:=proc(F,t,n)
local a,gos;
 a:=subs(n=n+1,F)-F;
 gos:=traperror(contgosper(a,t));
 if gos=lasterror then ERROR(`Continuous WZ method fails`) fi;
 RETURN(simpcomb(gos/F));
end: # intWZcertificate

contWZcertificate:=proc(F,t,x)
local a,gos;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 a:=diff(F,x)-F;
 gos:=traperror(contgosper(a,t));
 if gos=lasterror then ERROR(`Continuous WZ method fails`) fi;
 RETURN(simpcomb(gos/F));
end: # contWZcertificate

contWZgospercertificate:=proc(F,t,x)
local a,gos;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 a:=diff(F,x);
 gos:=traperror(contgosper(a,t));
 if gos=lasterror then ERROR(`Continuous WZ method fails`) fi;
RETURN(simpcomb(gos/a));
end: # contWZgospercertificate

rodriguesrecursion:=proc(g,h,x,sn)
local S,n,t,result;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sn,function) then
 S:=op(0,sn); n:=op(1,sn);
else
 n:=op(1,sn); 
fi;
 result:=intrecursion(n!*g*subs(x=t,h)/(t-x)^(n+1),t,S(n));
RETURN(result);
end: # rodriguesrecursion

rodriguesdiffeq:=proc(g,h,n,sx)
local S,x,t,result;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sx,function) then
 S:=op(0,sx); x:=op(1,sx);
else
 x:=op(1,sx);
fi;
 result:=intdiffeq(g*subs(x=t,h)/(t-x)^(n+1),t,S(x));
RETURN(result);
end: # rodriguesdiffeq

GFrecursion:=proc(F,a,z,sn)
local S,n;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sn,function) then
 S:=op(0,sn); n:=op(1,sn);
else
 n:=op(1,sn);
fi;
RETURN(intrecursion(F/a/z^(n+1),z,S(n)));
end: # GFrecursion

GFdiffeq:=proc(F,a,z,n,sx)
local S,x;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sx,function) then
 S:=op(0,sx); x:=op(1,sx);
else
 x:=op(1,sx);
fi;
RETURN(intdiffeq(F/a/z^(n+1),z,S(x)));
end: # GFdiffeq

rodrigues2integrand:=proc(a,rho,g,n,x,t)
local prefactor,derivativeterm,result;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 prefactor:=1/a/rho;
 derivativeterm:=rho*g^n;
 result:=n!*prefactor*subs(x=t,derivativeterm)/(t-x)^(n+1);
RETURN(result);
end: # rodrigues2integrand

CTrecursion:=proc(F,z,sn)
local S,n;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sn,function) then
 S:=op(0,sn); n:=op(1,sn);
else
 n:=op(1,sn);
fi;
RETURN(intrecursion(F/z,z,S(n)));
end: #CTrecursion

CTdiffeq:=proc(F,z,sx)
local S,x;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
if type(sx,function) then
 S:=op(0,sx); x:=op(1,sx);
else
 x:=op(1,sx);
fi;
RETURN(intdiffeq(F/z,z,S(x)));
end: #CTdiffeq

recursionorder:=proc(rec,sn)
# returns the order of a recurrence equation
local hass,n,s,tmp,order;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 s:=op(0,sn);
 n:=op(1,sn);
 order:=0;
 tmp:=rec;
 hass:=true;
 while hass do
   tmp:=subs(s(n+order)=1,tmp);
   if not has(tmp,s) then
      hass:=false;
   else
     order:=order+1;
   fi;
 od;
 RETURN(order);
end: #recursionorder


rec2poly:=proc()
local rec,s,n,P,Q,R,M,N,alpha,beta,gamma,delta,sol,tmp,l,S,REC;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 rec:=expand(args[1]):
 if type(rec,`=`) then rec:=op(1,rec)-op(2,rec) fi;
 s:=op(0,args[2]);
 n:=op(1,args[2]);
 P:=collect(coeff(rec,s(n+2)),n);
 Q:=collect(coeff(rec,s(n+1)),n);
 R:=collect(coeff(rec,s(n)),n);
 M:=max(degree(P,n),degree(Q,n),degree(R,n));
 alpha[0]:=coeff(P,n,M);
 beta[0]:=coeff(Q,n,M);
 gamma[0]:=coeff(R,n,M);
 # check first condition
 if not(simplify(alpha[0]+beta[0]+gamma[0])=0) then
   ERROR(`No polynomial solution exists`);
 fi;
 alpha[1]:=coeff(P,n,M-1);
 beta[1]:=coeff(Q,n,M-1);
 gamma[1]:=coeff(R,n,M-1);
 # check second condition
 if not(simplify(beta[0]+2*gamma[0])=0) then
   N:=normal((alpha[1]+beta[1]+gamma[1])/(beta[0]+2*gamma[0]));
 # check third condition
 elif not(simplify(alpha[1]+beta[1]+gamma[1])=0) then
   ERROR(`No polynomial solution exists`);
 else
   alpha[2]:=coeff(P,n,M-2);
   beta[2]:=coeff(Q,n,M-2);
   gamma[2]:=coeff(R,n,M-2);
   sol:={solve(N^2*gamma[0]-(beta[1]+gamma[0]+2*gamma[1])*N+
         alpha[2]+beta[2]+gamma[2],N)};
   N:=max(op(select(type,sol,nonnegint)));
 fi;
 if not(type(N,nonnegint)) then
   ERROR(`No polynomial solution exists`);
 fi;
 S:=add(delta[N-l]*n^l,l=0..N);
 REC:=collect(P*subs(n=n+2,S)+Q*subs(n=n+1,S)+R*S,n);
 sol:={solve(normal({coeffs(REC,n)}),{seq(delta[l],l=0..N)})};
 if sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0}
   then ERROR(`No polynomial solution exists`) fi;
 RETURN(factor(subs(op(1,sol),S)));
end: # rec2poly

factor_completely:= proc(poly,n,shift,rootlist)
local p,i,l;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 l:= normal([solve(subs(n=n+shift,poly),n)]);
 if (nargs = 4) then
   rootlist:= [1,seq(n-i,i=l)];
 fi;
 lcoeff(expand(poly),n) * product('n-l[i]-shift','i'=1..nops(l));
end:

rec2hyper:=proc()
local rec,s,n,P,Q,R,i,j,Qfactors,Qchoices,Rfactors,Rchoices,
     p,q,r,c,sol,C,tmp,t,cchoices;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 rec:=expand(args[1]):
 if type(rec,`=`) then rec:=op(1,rec)-op(2,rec) fi;
 s:=op(0,args[2]);
 n:=op(1,args[2]);
 P:=coeff(rec,s(n+2));
 Q:=coeff(rec,s(n+1));
 R:=coeff(rec,s(n));
 if (P = 0) then
   RETURN({-factor_completely(R,n,0)/factor_completely(Q,n,0)});
 fi;
 R:= factor_completely(R,n,-1,Qfactors);
 Qchoices:=generateproducts(Qfactors);
 P:= factor_completely(P,n,-2,Rfactors);
 Rchoices:=generateproducts(Rfactors);
 sol:={};
 for q in Qchoices do
   for r in Rchoices do
     cchoices:=(C^2*(P/subs(n=n+2,r))*subs(n=n+2,q)+C*Q+
       (R/subs(n=n+1,q))*subs(n=n+1,r));
     cchoices:= normal({solve(traperror(lcoeff(expand(cchoices),n),C))});
     for c in cchoices do
       tmp:= (c^2*(P/subs(n=n+2,r))*subs(n=n+2,q)*p(n+2)+c*Q*p(n+1)+
            (R/subs(n=n+1,q))*subs(n=n+1,r)*p(n));
       tmp:=traperror(rec2poly(tmp,p(n)));
       if not(tmp=lasterror) then 
         t:=normal(c*subs(n=n+1,tmp)*subs(n=n+1,q)/(tmp*subs(n=n+1,r)));
         sol:={op(sol),t} 
       fi;
     od; 
   od;
 od;
 RETURN(sol);
end: # rec2hyper
# ----------------------------------------------------------------------

# Maple 9
# replace rec2hyper by rechyper since rec2hyper sometimes fails
rec2hyper:=rechyper:

recpoly:=proc()
local rec,s,n,J,i,j,PP,M,m,b,l,S,N,solution,alpha,delta,REC,sol;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
 rec:=expand(args[1]):
 if type(rec,`=`) then rec:=op(1,rec)-op(2,rec) fi;
 s:=op(0,args[2]);
 n:=op(1,args[2]);
 J:=recursionorder(rec,s(n));
 M:=0;
 for j from 0 to J do
   PP[j]:=collect(coeff(rec,s(n+j)),n);
   if degree(PP[j],n)>M then M:=degree(PP[j],n) fi;
 od;
 m:=-1;
 b:=[0];
 while convert(b,set)={0} do
   m:=m+1;
   b:=[];
   for l from 0 to m do
     for j from 0 to J do
       alpha[j,m-l]:=coeff(PP[j],n,M-m+l);
     od;
     b:=[op(b),simplify(add(j^l*alpha[j,m-l],j=0..J))];
   od;
 od;
 solution:={solve(expand(add(binomial(N,l)*op(l+1,b),l=0..m)),N)};
 N:={};
 for i from 1 to nops(solution) do
   if type(op(i,solution),nonnegint) then N:={op(N),op(i,solution)} fi;
 od;
 if N={} then ERROR(`No polynomial solution exists`) fi;
 N:=max(op(N));
 S:=add(delta[N-l]*n^l,l=0..N);
 REC:=collect(add(PP[j]*subs(n=n+j,S),j=0..J),n);
 sol:={solve(normal({coeffs(REC,n)}),{seq(delta[l],l=0..N)})};
 if sol={} or {seq(op(2,op(l,op(1,sol))),l=1..nops(op(1,sol)))}={0}
   then ERROR(`No polynomial solution exists`) fi;
 RETURN(factor(subs(op(1,sol),S)));
end: # recpoly

generateproducts:= proc(a)
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

rechyper:=proc()
local rec,s,n,PP,J,M,i,j,Qsol,Qfactors,Qchoices,Rsol,Rfactors,Rchoices,
		p,q,r,c,C,Clist,h,sol,tmp,t,REC,alpha,zeit;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
	zeit:= time();
	rec:=args[1];
	if type(rec,`=`) then rec:=op(1,rec)-op(2,rec) fi;
	s:=op(0,args[2]);
	n:=op(1,args[2]);
	J:=recursionorder(rec,s(n));
	for j from 0 to J do
		PP[j]:=collect(coeff(rec,s(n+j)),n,expand);
	od;
	Qsol:=[solve(subs(n=n-1,PP[0]),n)];
	Qsol:= normal(Qsol);
	Qfactors:=[seq(n-op(i,Qsol),i=1..nops(Qsol))];
	Qchoices:=generateproducts(Qfactors);
	Rsol:=[solve(subs(n=n-J,PP[J]),n)];
	Rsol:= normal(Rsol);
	Rfactors:=[seq(n-op(i,Rsol),i=1..nops(Rsol))];
	Rchoices:=generateproducts(Rfactors);
	sol:={};
	for q in Qchoices do
		for r in Rchoices do
			M:=0;
			for j from 0 to J do
				h[j]:=PP[j]*product(subs(n=n+'l',q),'l'=1..j)*
				product(subs(n=n+'l',r),'l'=j+1..J);
				h[j]:=collect(h[j],n,expand);
				M:= max(M, degree(h[j],n));
			od;
			for j from 0 to J do alpha[j]:=coeff(h[j],n,M) od;
			Clist:= {solve(add(alpha[j]*C^j, j=0..J), C)};
			Clist:= normal(Clist) minus {0};
			for c in Clist do
				REC:= add(c^j*h[j]*p(n+j), j=0..J);
# no longer working in Maple 9:	tmp:=traperror(recpoly(REC,p(n)));
				tmp:=traperror(recpoly(eval(REC),p(n)));
				if not(tmp=lasterror) then
					t:=normal(c*subs(n=n+1,tmp)*subs(n=n+1,q)/(tmp*subs(n=n+1,r)));
					sol:={op(sol),t}
				fi;
			od;
		od;
	od;
	# printf(`CPU-time: %7.1fs`, time()-zeit);
	RETURN(sol);
end: # rechyper

# ----------------------------------------------------------------------

`rationalrechyper/factorlist`:= proc(term, n)
	local z, f;
	z:= factor(term);
	if type(z,`*`) then z:= [op(z)]; else z:= [z]; fi;
### WARNING: note that `I` is no longer of type `^`
	z:= [seq((proc(y) if type(y,`^`) then [op(1,y),op(2,y)]\
		else [y,1] fi end)(f), f=z)];
	z:= [seq(normal(f[1]/lcoeff(f[1],n)) $ f[2], f=select(has,z,n))];
	RETURN(z);
end:

# ----------------------------------------------------------------------


rationalrechyper:=proc()
local rec,s,n,PP,J,M,i,j,Qfactors,Qchoices,Rfactors,Rchoices,
		p,q,r,c,C,Clist,h,sol,tmp,t,REC,alpha,zeit;
option `Copyright 1998  Wolfram Koepf, Konrad-Zuse-Zentrum Berlin`;
	zeit:= time();
	rec:=args[1];
	if type(rec,`=`) then rec:=op(1,rec)-op(2,rec) fi;
	s:=op(0,args[2]);
	n:=op(1,args[2]);
	J:=recursionorder(rec,s(n));
	for j from 0 to J do
		PP[j]:=collect(coeff(rec,s(n+j)),n,expand);
	od;
	Qfactors:= `rationalrechyper/factorlist`(subs(n=n-1,PP[0]), n);
	Qchoices:=generateproducts(Qfactors);
	Rfactors:= `rationalrechyper/factorlist`(subs(n=n-J,PP[J]), n);
	Rchoices:=generateproducts(Rfactors);
	sol:={};
	for q in Qchoices do
		for r in Rchoices do
			M:=0;
			for j from 0 to J do
				h[j]:=PP[j]*product(subs(n=n+'l',q),'l'=1..j)*
				product(subs(n=n+'l',r),'l'=j+1..J);
				h[j]:=collect(h[j],n,expand); #######
				M:= max(M, degree(h[j],n));
			od;
			for j from 0 to J do alpha[j]:=coeff(h[j],n,M) od;
			Clist:= {solve(add(alpha[j]*C^j, j=0..J), C)};
			Clist:= normal(Clist) minus {0};
			for c in Clist do
				REC:= add(c^j*h[j]*p(n+j), j=0..J);
				tmp:=traperror(recpoly(REC,p(n)));
				if not(tmp=lasterror) then
					t:=normal(c*subs(n=n+1,tmp)*subs(n=n+1,q)/(tmp*subs(n=n+1,r)));
					sol:={op(sol),t}
				fi;
			od;
		od;
	od;
	printf(`CPU-time: %7.1fs`, time()-zeit);
	RETURN(sol);
end: # rationalrechyper

RESum:=proc(re1,re2,Ak)
local A,k,i,jj,fre,flist,gre,glist,forder,gorder,frule,grule,Done,relist,variab,variablist,solution,kk,a,b,c,numberlist,Min,Max;
`Copyright 2009  Wolfram Koepf, University of Kassel`;
if type(Ak,function) then A:=op(0,Ak); k:=op(1,Ak) else k:=Ak fi;
# normalization
if type(re1,equation) then fre:=op(1,re1)-op(2,re1) else fre:=re1 fi;
fre:=collect(fre,A);
numberlist:=indets(fre,specfunc(anything,A));
numberlist:= map(x->op(1,x)-k, numberlist);
Min:=min(op(numberlist));
Max:=max(op(numberlist));
fre:=A(k+Max)=solve(fre=0,A(k+Max));
fre:=subs(A=a,fre);
fre:=subs(k=k-Min,fre);
fre:=map(normal,fre);
forder:=Max-Min;
if type(re2,equation) then gre:=op(1,re2)-op(2,re1) else gre:=re2 fi;
gre:=collect(gre,A);
numberlist:=indets(gre,specfunc(anything,A));
numberlist:= map(x->op(1,x)-k, numberlist);
Min:=min(op(numberlist));
Max:=max(op(numberlist));
gre:=A(k+Max)=solve(gre=0,A(k+Max));
gre:=subs(A=b,gre);
gre:=subs(k=k-Min,gre);
gre:=map(normal,gre);
gorder:=Max-Min;
# generating replacements rules for shifts of f 
frule:={};
for jj from forder to forder+gorder do
  frule:={op(frule),fre};
  fre:=map(normal,subs(frule,subs(k=k+1,fre))) 
od;
# generating replacements rules for shifts of g 
grule:={};
for jj from gorder to forder+gorder do
  grule:={op(grule),gre};
  gre:=map(normal,subs(grule,subs(k=k+1,gre))) 
od;
Done:=false;
kk:=max(forder,gorder);
while kk<=forder+gorder and not(Done) do
  relist:=[seq(c(k+jj)=a(k+jj)+b(k+jj),jj=0..kk)]; jj:='jj':
		 relist:=subs(frule,relist);
  relist:=subs(grule,relist); 
  variab:={seq(a(k+i),i=0..forder-1),seq(b(k+jj),jj=0..gorder-1)};
  i:='i': jj:='jj':
  variablist:={seq(op(jj,variab)=var[jj],jj=1..nops(variab))};
  jj:='jj':
  relist:=subs(variablist,relist);
  relist:={seq(op([jj,1],relist)-op([jj,2],relist),jj=1..nops(relist))};
  jj:='jj':
  relist:=map(normal,relist);
  relist:=map(numer,relist);
  solution:=eliminate(relist,{seq(var[jj],jj=1..nops(variab))});
  if not(op(2,solution)={}) then Done:=true fi;  
  kk:=kk+1;
od;
solution:=op([2,1],solution);
solution:=subs(c=A,solution);
solution:=collect(solution,A,factor)=0;
end: # RESum

REProduct:=proc(re1,re2,Ak)
local A,k,i,jj,fre,flist,gre,glist,forder,gorder,frule,grule,Done,relist,variab,variablist,solution,kk,a,b,c,numberlist,Min,Max;
`Copyright 2009  Wolfram Koepf, University of Kassel`;
if type(Ak,function) then A:=op(0,Ak); k:=op(1,Ak) else k:=Ak fi;
# normalization
if type(re1,equation) then fre:=op(1,re1)-op(2,re1) else fre:=re1 fi;
fre:=collect(fre,A);
numberlist:=indets(fre,specfunc(anything,A));
numberlist:= map(x->op(1,x)-k, numberlist);
Min:=min(op(numberlist));
Max:=max(op(numberlist));
fre:=A(k+Max)=solve(fre=0,A(k+Max));
fre:=subs(A=a,fre);
fre:=subs(k=k-Min,fre);
fre:=map(normal,fre);
forder:=Max-Min;
if type(re2,equation) then gre:=op(1,re2)-op(2,re1) else gre:=re2 fi;
gre:=collect(gre,A);
numberlist:=indets(gre,specfunc(anything,A));
numberlist:= map(x->op(1,x)-k, numberlist);
Min:=min(op(numberlist));
Max:=max(op(numberlist));
gre:=A(k+Max)=solve(gre=0,A(k+Max));
gre:=subs(A=b,gre);
gre:=subs(k=k-Min,gre);
gre:=map(normal,gre);
gorder:=Max-Min;
# generating replacements rules for shifts of f 
frule:={};
for jj from forder to forder+gorder do
  frule:={op(frule),fre};
  fre:=map(normal,subs(frule,subs(k=k+1,fre))) 
od;
# generating replacements rules for shifts of g 
grule:={};
for jj from gorder to forder+gorder do
  grule:={op(grule),gre};
  gre:=map(normal,subs(grule,subs(k=k+1,gre))) 
od;
Done:=false;
kk:=max(forder,gorder);
while kk<=forder*gorder and not(Done) do
  relist:=[seq(c(k+jj)=a(k+jj)*b(k+jj),jj=0..kk)]; jj:='jj':
  relist:=subs(frule,relist);
  relist:=subs(grule,relist); 
  relist:=collect(relist,[a,b]);
lprint(relist);
  # the following pattern matching a(k+i)*b(k+jj)->var[..] does not work
  variab:={seq(seq(a(k+i)*b(k+jj),i=0..forder-1),jj=0..gorder-1)};
  i:='i': jj:='jj':
  variablist:=[seq(op(jj,variab)=var[jj],jj=1..nops(variab))]; 
lprint(variablist);
  jj:='jj':
  # problem
  relist:=applyrule(variablist,relist); 
lprint(relist);
  relist:={seq(op([jj,1],relist)-op([jj,2],relist),jj=1..nops(relist))};
  jj:='jj':
  relist:=map(normal,relist);
  relist:=map(numer,relist);
  solution:=eliminate(relist,{seq(var[jj],jj=1..nops(variab))});
  if not(op(2,solution)={}) then Done:=true fi;  
  kk:=kk+1;
od;
solution:=op([2,1],solution);
solution:=subs(c=A,solution);
solution:=subs(a(k)=A(k)/b(k),solution);
solution:=collect(solution,A,factor)=0;
end: # REProduct

DESum:=proc(re1,re2,Ak)
local A,k,i,jj,fre,flist,gre,glist,forder,gorder,frule,grule,Done,relist,variab,variablist,solution,kk,a,b,c,numberlist,Min,Max;
`Copyright 2009  Wolfram Koepf, University of Kassel`;
if type(Ak,function) then A:=op(0,Ak); k:=op(1,Ak) else k:=Ak fi;
# normalization
if type(re1,equation) then fre:=op(1,re1)-op(2,re1) else fre:=re1 fi;
fre:=collect(fre,A);
numberlist:=indets(fre,specfunc(anything,diff));
forder:=0:
while has(numberlist,diff) do
 numberlist:=map(x->op(1,x), numberlist);
 forder:=forder+1;
od;
fre:=diff(A(k),k$forder)=solve(fre=0,diff(A(k),k$forder));
fre:=subs(A=a,fre);
fre:=map(normal,fre);
if type(re2,equation) then gre:=op(1,re2)-op(2,re1) else gre:=re2 fi;
gre:=collect(gre,A);
numberlist:=indets(gre,specfunc(anything,diff));
gorder:=0:
while has(numberlist,diff) do
 numberlist:=map(x->op(1,x), numberlist);
 gorder:=gorder+1;
od;
gre:=diff(A(k),k$gorder)=solve(gre=0,diff(A(k),k$gorder));
gre:=subs(A=b,gre);
gre:=map(normal,gre);
# generating replacements rules for shifts of f 
frule:={};
for jj from forder to forder+gorder do
  frule:={op(frule),fre};
  fre:=diff(a(k),k$jj+1)=normal(subs(frule,diff(op(2,fre),k))) 
od;
# generating replacements rules for shifts of g 
grule:={};
for jj from gorder to forder+gorder do
  grule:={op(grule),gre};
  gre:=diff(b(k),k$jj+1)=normal(subs(grule,diff(op(2,gre),k))) 
od;
Done:=false;
kk:=max(forder,gorder);
while kk<=forder+gorder and not(Done) do
  relist:=[c(k)=a(k)+b(k),seq(diff(c(k),k$jj)=diff(a(k),k$jj)+diff(b(k),k$jj),jj=1..kk)]; jj:='jj':
  relist:=subs(frule,relist);
  relist:=subs(grule,relist); 
  variab:={a(k),b(k),seq(diff(a(k),k$i),i=1..forder-1),seq(diff(b(k),k$jj),jj=1..gorder-1)};
  i:='i': jj:='jj':
  variablist:={seq(op(jj,variab)=var[jj],jj=1..nops(variab))};
  jj:='jj':
  # ich weiss nicht, ob das klappt...
  relist:=subs(variablist,relist);
  relist:={seq(op([jj,1],relist)-op([jj,2],relist),jj=1..nops(relist))};
  jj:='jj':
  relist:=map(normal,relist);
  relist:=map(numer,relist);
  solution:=eliminate(relist,{seq(var[jj],jj=1..nops(variab))});
  if not(op(2,solution)={}) then Done:=true fi;  
  kk:=kk+1;
od;
solution:=op([2,1],solution);
solution:=subs(c=A,solution);
solution:=collect(solution,diff,factor)=0;
end: # DESum

inhomotohomo:=proc(RE,sn)
local S,n,rat,u,v;
`Copyright 2009  Wolfram Koepf, University of Kassel`;
if type(sn,function) then S:=op(0,sn); n:=op(1,sn) else n:=sn fi;
rat:=ratio(rhs(RE),n);
u:=numer(rat);
v:=denom(rat);
collect(u*lhs(RE)-v*subs(n=n+1,lhs(RE)),S,factor)=0
end: # inhomotohomo

hyperrecursion:=proc(upper,lower,z,Sn)
local k;
sumrecursion(hyperterm(upper,lower,z,k),k,Sn);
end:

Timing:=proc(f)
local TIME,result;
 TIME:=time():
 result:=eval(f);
 RETURN([(time()-TIME)*seconds,result]);
end: # Timing

# print(`Package "Hypergeometric Summation", Maple V - Maple 15`):
# print(`Copyright 1998-2011, Wolfram Koepf, University of Kassel`):