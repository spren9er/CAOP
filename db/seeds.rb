###
#
# CONTENTR ELEMENTS
#
###

Contentr::Node.delete_all

@site = Contentr::Site.create!(name: 'cms')

application_page = Contentr::LinkedPage.new(name: 'Entry_Page', linked_to: 'application#index', parent: @site)
application_page.position = 0
application_page.published = true
application_page.paragraphs << HeaderParagraph.new(area_name: 'Entry_Page', title: 'CAOP - Computer Algebra & Orthogonal Polynomials')
application_page.paragraphs << ParagraphParagraph.new(area_name: 'Entry_Page', body: "CAOP is a package for calculating formulas for orthogonal polynomials belonging to the Askey scheme by <a href=\"http://www.maplesoft.com/\">Maple</a>.
With the present version users can compute recurrence relations, differential and difference equations or make a plot of every polynomial in the Askey scheme, without having Maple installed on
their own computer. It is also possible to multiply the polynomial by a scaling function, to change the argument and to give values to the parameters by filling out a form, before doing the calculation. As an extra option the user can choose the layout of the output: prettyprint, lineprint or LaTeX. The latter two options make it possible to insert the output in another Maple worksheet respectively in a LaTeX document by a simple mouse-action.
")
application_page.paragraphs << ParagraphParagraph.new(area_name: 'Entry_Page', body: "Furthermore there are some help pages available for users who are not familiar with Maple. They can be viewed simultaneously while filling in the form.")
application_page.paragraphs << ParagraphParagraph.new(area_name: 'Entry_Page', body: "All computations in CAOP are performed by calling procedures either from <a href=\"http://www.mathematik.uni-kassel.de/%7Ekoepf/Publikationen/#down\">hsum15</a> (\"Hypergeometric Summation\") or <a href=\"http://www.mathematik.uni-kassel.de/%7Ekoepf/Publikationen/#down\">qsum15</a> (\"q-Hypergeometric Summation\") by <a href=\"http://www.mathematik.uni-kassel.de/%7Ekoepf/\">Wolfram Koepf</a>, University of Kassel.")
application_page.paragraphs << ParagraphParagraph.new(area_name: 'Entry_Page', body: "The implementation of CAOP was originally done by <a href=\"http://www.cs.vu.nl/%7Erene/\">Ren&eacute; Swarttouw</a> as part of the <a href=\"http://www.cs.vu.nl/%7Erene/Onderzoek/AW.html\">Askey-Wilson-Scheme Project</a> performed at <a href=\"http://www.riaca.win.tue.nl/\">RIACA</a> in Eindhoven in 2004. A completely revised version of this project has been done by <a href=\"http://www.mathematik.uni-kassel.de/~sprenger\">Torsten Sprenger</a> under supervision of Wolfram Koepf in 2012. The present version is maintained by <a href=\"http://www.mathematik.uni-kassel.de/~koepf\">Wolfram Koepf</a>.")
application_page.save!

polynomials_page = Contentr::LinkedPage.new(name: 'Orthogonal_Polynomials_Overview', linked_to: 'polynomials#index', parent: @site)
polynomials_page.position = 0
polynomials_page.published = true
polynomials_page.paragraphs << HeaderParagraph.new(area_name: 'Orthogonal_Polynomials_Overview', title: 'Orthogonal Polynomials')
polynomials_page.paragraphs << ParagraphParagraph.new(area_name: 'Orthogonal_Polynomials_Overview', body: 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.')
polynomials_page.paragraphs << MathParagraph.new(area_name: 'Orthogonal_Polynomials_Overview', math: 'x^2')
polynomials_page.paragraphs << ParagraphParagraph.new(area_name: 'Orthogonal_Polynomials_Overview', body: 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.')
polynomials_page.save!

qpolynomials_page = Contentr::LinkedPage.new(name: 'qOrthogonal_Polynomials_Overview', linked_to: 'qpolynomials#index', parent: @site)
qpolynomials_page.position = 0
qpolynomials_page.published = true
qpolynomials_page.paragraphs << HeaderParagraph.new(area_name: 'qOrthogonal_Polynomials_Overview', title: 'q-Orthogonal Polynomials')
qpolynomials_page.paragraphs << ParagraphParagraph.new(area_name: 'qOrthogonal_Polynomials_Overview', body: 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.')
qpolynomials_page.paragraphs << MathParagraph.new(area_name: 'qOrthogonal_Polynomials_Overview', math: 'x^2')
qpolynomials_page.paragraphs << ParagraphParagraph.new(area_name: 'qOrthogonal_Polynomials_Overview', body: 'Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.')
qpolynomials_page.save!

###
#
#  CATEGORIES
#
###

Category.delete_all

polynomials = Category.create({
  name:       'Orthogonal Polynomials',
  sid:        'polynomials'
})

qpolynomials = Category.create({
  name:       'q-Orthogonal Polynomials',
  sid:        'qpolynomials'
})


###
#
#  ORTHOGONAL POLYNOMIALS
#
###

Polynomial.delete_all

#
# Wilson 
#
wilson = Polynomial.new({
  name:       'Wilson',
  sid:        'wilson',
  definition: 'W_n(x^2;a,b,c,d) = (a+b)_n (a+c)_n (a+d)_n \sum_{k=0}^n \frac{(-n)_k (n+a+b+c+d-1)_k (a+i x)_k (a-i x)_k}{(a+b)_k (a+c)_k (a+d)_k k!} = (a+b)_n (a+c)_n (a+d)_n {}_4F_3 \left(\left. {-n, n+a+b+c+d-1, a+i x, a-i x\atop a+b, a+c, a+d} \; \right| 1 \right)',
  maple:      'pochhammer(a+b,n)*pochhammer(a+c,n)*pochhammer(a+d,n)*pochhammer(-n,k)*pochhammer(n+a+b+c+d-1,k)*pochhammer(a+I*x,k)*pochhammer(a-I*x,k)/pochhammer(a+b,k)/pochhammer(a+c,k)/pochhammer(a+d,k)/k!',
  type:       'discrete'
})
wilson.parameters.create(name: 'a', lower_bound: '0', upper_bound: 'infinity' )
wilson.parameters.create(name: 'b', lower_bound: '0', upper_bound: 'infinity' )
wilson.parameters.create(name: 'c', lower_bound: '0', upper_bound: 'infinity' )
wilson.parameters.create(name: 'd', lower_bound: '0', upper_bound: 'infinity' )
wilson.category = polynomials
wilson.save

#
# Racah 
#
racah = Polynomial.new({
  name:       'Racah',
  sid:        'racah',
  definition: 'R_n(\lambda(x);\alpha,\beta,\gamma,\delta) = \sum_{k=0}^n \frac{(-n)_k (n+\alpha+\beta+1)_k (-x)_k (x+\gamma+\delta+1)_k}{(\alpha+1)_k (\beta+\delta+1)_k (\delta+1)_k k!} = {}_4F_3 \left(\left. {-n, n+\alpha+\beta+1, -x, x+\gamma+\delta+1\atop \alpha+1, \beta+\delta+1, \gamma+1} \; \right| 1 \right)',
  maple:      'pochhammer(-n,k)*pochhammer(n+alpha+beta+1,k)*pochhammer(-x,k)*pochhammer(x+gamma+delta+1,k)/pochhammer(alpha+1,k)/pochhammer(beta+delta+1,k)/pochhammer(gamma+1,k)/k!',
  type:       'discrete'
})
racah.parameters.create(name: 'alpha', lower_bound: '0', upper_bound: 'infinity' )
racah.parameters.create(name: 'beta', lower_bound: '0', upper_bound: 'infinity' )
racah.parameters.create(name: 'gamma', lower_bound: '0', upper_bound: 'infinity' )
racah.parameters.create(name: 'delta', lower_bound: '0', upper_bound: 'infinity' )
racah.category = polynomials
racah.save

#
# Continuous Dual Hahn
#
continuous_dual_hahn = Polynomial.new({
  name:       'Continuous Dual Hahn',
  sid:        'continuous_dual_hahn',
  definition: 'S_n(x^2;a,b,c) = (a+b)_n (a+c)_n \sum_{k=0}^n \frac{(-n)_k (a+i x)_k (a-i x)_k}{(a+b)_k (a+c)_k k!} = (a+b)_n (a+c)_n {}_3F_2 \left(\left. {-n, a+i x, a-i x \atop a+b, a+c} \; \right| 1 \right)',
  maple:      'pochhammer(a+b,n)*pochhammer(a+c,n)*pochhammer(-n,k)*pochhammer(a+I*x,k)*pochhammer(a-I*x,k)/pochhammer(a+b,k)/pochhammer(a+c,k)/k!',
  type:       'discrete'
})
continuous_dual_hahn.parameters.create(name: 'a', lower_bound: '0', upper_bound: 'infinity' )
continuous_dual_hahn.parameters.create(name: 'b', lower_bound: '0', upper_bound: 'infinity' )
continuous_dual_hahn.parameters.create(name: 'c', lower_bound: '0', upper_bound: 'infinity' )
continuous_dual_hahn.category = polynomials
continuous_dual_hahn.save

#
# Continuous Hahn
#
continuous_hahn = Polynomial.new({
  name:       'Continuous Hahn',
  sid:        'continuous_hahn',
  definition: 'p_n(x;a,b,c,d) = i^n\frac{(a+c)_n (a+d)_n}{n!} \sum_{k=0}^n \frac{(-n)_k (n+a+b+c+d-1)_k (a+i x)_k}{(a+c)_k (a+d)_k k!} = i^n\frac{(a+c)_n (a+d)_n}{n!} {}_3F_2 \left(\left. {-n, n+a+b+c+d-1, a+i x \atop a+c, a+d} \; \right| 1 \right)',
  maple:      'I^n*pochhammer(a+c,n)*pochhammer(a+d,n)/n!*pochhammer(-n,k)*pochhammer(n+a+b+c+d-1,k)*pochhammer(a+I*x,k)/pochhammer(a+c,k)/pochhammer(a+d,k)/k!',
  type:       'discrete'
})
continuous_hahn.parameters.create(name: 'a', lower_bound: '0', upper_bound: 'infinity' )
continuous_hahn.parameters.create(name: 'b', lower_bound: '0', upper_bound: 'infinity' )
continuous_hahn.parameters.create(name: 'c', lower_bound: '0', upper_bound: 'infinity' )
continuous_hahn.parameters.create(name: 'd', lower_bound: '0', upper_bound: 'infinity' )
continuous_hahn.category = polynomials
continuous_hahn.save

#
# Hahn
#
hahn = Polynomial.new({
  name:       'Hahn',
  sid:        'hahn',
  definition: 'Q_n(x;\alpha,\beta,N) = \sum_{k=0}^n \frac{(-n)_k (n+\alpha+\beta+1)_k (-x)_k}{(\alpha+1)_k (-N)_k k!} = {}_3F_2 \left(\left. {-n, n+\alpha+\beta+1, -x \atop \alpha+1, -N} \; \right| 1 \right)',
  maple:      'pochhammer(-n,k)*pochhammer(n+alpha+beta+1,k)*pochhammer(-x,k)/pochhammer(alpha+1,k)/pochhammer(-N,k)/k!',
  type:       'discrete'
})
hahn.parameters.create(name: 'alpha', lower_bound: '-1', upper_bound: 'infinity' )
hahn.parameters.create(name: 'beta', lower_bound: '-1', upper_bound: 'infinity' )
hahn.category = polynomials
hahn.save

#
# Dual Hahn
#
dual_hahn = Polynomial.new({
  name:       'Dual Hahn',
  sid:        'dual_hahn',
  definition: 'R_n(\lambda(x);\gamma,\delta,N) = \sum_{k=0}^n \frac{(-n)_k (-x)_k (x+\gamma+\delta+1)_k}{(\gamma+1)_k (-N)_k k!} = {}_3F_2 \left(\left. {-n, -x, x+\gamma+\delta+1 \atop \gamma+1, -N} \; \right| 1 \right)',
  maple:      'pochhammer(-n,k)*pochhammer(-x,k)*pochhammer(x+gamma+delta+1,k)/pochhammer(gamma+1,k)/pochhammer(-N,k)/k!',
  type:       'discrete'
})
dual_hahn.parameters.create(name: 'gamma', lower_bound: '-1', upper_bound: 'infinity' )
dual_hahn.parameters.create(name: 'delta', lower_bound: '-1', upper_bound: 'infinity' )
dual_hahn.category = polynomials
dual_hahn.save

#
# Meixner-Pollaczek
#
meixner_pollaczek = Polynomial.new({
  name:       'Meixner-Pollaczek',
  sid:        'meixner_pollaczek',
  definition: 'P_n^\lambda(x;\phi) = \frac{(2 \lambda)_n}{n!} e^{i n \phi} \sum_{k=0}^n \frac{(-n)_k (\lambda + i x)_k}{(2\lambda)_k k!} \left(1-e^{-2i\phi}\right)^k = \frac{(2 a)_n}{n!} e^{i n \phi} {}_2F_1 \left(\left. {-n, \lambda+i x \atop 2\lambda} \; \right| 1-e^{-2i \phi} \right)',
  maple:      'pochhammer(2*lambda,n)/n!*exp(I*n*phi)*pochhammer(-n,k)*pochhammer(lambda+I*x,k)/pochhammer(2*lambda,k)/k!*(1-exp(-2*I*phi))^k',
  type:       'discrete'
})
meixner_pollaczek.parameters.create(name: 'lambda', lower_bound: '0', upper_bound: 'infinity' )
meixner_pollaczek.parameters.create(name: 'phi', lower_bound: '0', upper_bound: 'pi')
meixner_pollaczek.category = polynomials
meixner_pollaczek.save

#
# Jacobi
#
jacobi = Polynomial.new({
  name:       'Jacobi',
  sid:        'jacobi',
  definition: 'P_n^{(\alpha,\beta)}(x) = \frac{(\alpha+1)_n}{n!} \sum_{k=0}^n \frac{(-n)_k (n+\alpha+\beta+1)_k}{(\alpha+1)_k k!} \left(\frac{1-x}{2}\right)^k = \frac{(\alpha+1)_n}{n!} {}_2F_1 \left(\left. {-n, n+\alpha+\beta+1 \atop \alpha+1} \; \right| \frac{1-x}{2} \right)',
  maple:      'pochhammer(-n,k)*pochhammer(n+alpha+beta+1,k)/pochhammer(alpha+1,k)/k!*((1-x)/2)^k',
  type:       'continuous'
})
jacobi.parameters.create(name: 'alpha', lower_bound: '-1', upper_bound: 'infinity' )
jacobi.parameters.create(name: 'beta', lower_bound: '-1', upper_bound: 'infinity' )
jacobi.category = polynomials
jacobi.save

#
# Meixner
#
meixner = Polynomial.new({
  name:       'Meixner',
  sid:        'meixner',
  definition: 'M_n(x;\beta,c) = \sum_{k=0}^n \frac{(-n)_k (-x)_k}{(\beta)_k k!}\left(1-\frac{1}{c}\right)^k = {}_2F_1 \left(\left. {-n, -x \atop \beta} \; \right| 1-\frac{1}{c} \right)',
  maple:      'pochhammer(-n,k)*pochhammer(-x,k)/pochhammer(beta,k)/k!*(1-1/c)^k',
  type:       'discrete'
})
meixner.parameters.create(name: 'beta', lower_bound: '0', upper_bound: 'infinity' )
meixner.parameters.create(name: 'c', lower_bound: '0', upper_bound: 1)
meixner.category = polynomials
meixner.save

#
# Krawtchouk
#
krawtchouk = Polynomial.new({
  name:       'Krawtchouk',
  sid:        'krawtchouk',
  definition: 'K_n(x;p,N) = \sum_{k=0}^n \frac{(-n)_k (-x)_k}{(-N)_k k!} \left(\frac{1}{p}\right)^k = {}_2F_1 \left(\left. {-n, -x \atop -N} \; \right| \frac{1}{p} \right)',
  maple:      'pochhammer(-n,k)*pochhammer(-x,k)/pochhammer(-N,k)/k!*(1/p)^k',
  type:       'discrete'
})
krawtchouk.parameters.create(name: 'p', lower_bound: '0', upper_bound: 1)
krawtchouk.category = polynomials
krawtchouk.save

#
# Laguerre
#
laguerre = Polynomial.new({
  name:       'Laguerre',
  sid:        'laguerre',
  definition: 'L_n^{(\alpha)}(x) = \frac{(\alpha+1)_n}{n!} \sum_{k=0}^n \frac{(-n)_k}{(\alpha+1)_k k!} x^k = \frac{(\alpha+1)_n}{n!} {}_1F_1 \left(\left. {-n \atop \alpha+1} \; \right| x \right)',
  maple:      'pochhammer(alpha+1,n)/n!*pochhammer(-n,k)/pochhammer(alpha+1,k)/k!*x^k',
  type:       'continuous'
})
laguerre.parameters.create(name: 'alpha', lower_bound: '-1', upper_bound: 'infinity' )
laguerre.category = polynomials
laguerre.save

#
# Charlier
#
charlier = Polynomial.new({
  name:       'Charlier',
  sid:        'charlier',
  definition: 'C_n(x,a) = \sum_{k=0}^n \frac{(-n)_k (-x)_k}{k!} \left(-\frac{1}{a}\right)^k = {}_2F_0 \left(\left. {-n, -x \atop -} \; \right| -\frac{1}{a} \right)',
  maple:      'pochhammer(-n,k)*pochhammer(-x,k)/k!*(-1/a)^k',
  type:       'discrete'
})
charlier.parameters.create(name: 'a', lower_bound: '0', upper_bound: 'infinity' )
charlier.category = polynomials
charlier.save 

#
# Hermite
#
hermite = Polynomial.new({
  name:       'Hermite',
  sid:        'hermite',
  definition: 'H_n(x) = \sum_{k=0}^n \frac{(-n/2)_k - ((n-1)/2)_k}{k!} \left(-\frac{1}{x^2}\right)^k = (2x)^n {}_2F_0 \left(\left. {-n/2, -(n-1)/2 \atop -} \; \right| -\frac{1}{x^2} \right)',
  maple:      'pochhammer(-n/2,k)*pochhammer(-(n-1)/2,k)*(-1/x^2)^k',
  type:       'continuous' 
})
hermite.category = polynomials
hermite.save


###
#
#  Q-ORTHOGONAL POLYNOMIALS
#
###

#
# Askey-Wilson
#
askey_wilson = Polynomial.new({
  name:       'Askey Wilson',
  sid:        'askey_wilson',
  definition: 'p_n(x;a,b,c,d|q) = a^{-n}(a b, a c, a d; q)_n \sum_{k=0}^\infty \frac{(q^{-n};q)_k (abcdq^{n-1};q)_k (ae^{it};q)_k (ae^{-it};q)_k}{(ab;q)_k (ac;q)_k (ad;q)_k (q;q)_k} = a^{-n}(a b, a c, a d; q)_n {}_{4}\phi_{3}\!\left(\left. {q^{-n}, abcdq^{n-1}, a e^{it}, a e^{-it} \atop ab, ac, ad} \; \right| q ; q \right)\quad , x=\cos(t)',
  maple:      'a^(-n)*qpochhammer(a*b,q,n)*qpochhammer(a*c,q,n)*qpochhammer(a*d,q,n)*qpochhammer(q^{-n},q,k)*qpochhammer(a*b*c*d*q^{n-1},q,k)*qpochhammer(a*exp(I*t),q,k)*qpochhammer(a*exp(-I*t),q,k)/qpochhammer(a*b,q,k)/qpochhammer(a*c,q,k)/qpochhammer(a*d,q,k)/qpochhammer(q,q,k)',
  type:       'discrete'
})
askey_wilson.parameters.create(name: 'a', lower_bound: '-infinity', upper_bound: 'infinity' )
askey_wilson.parameters.create(name: 'b', lower_bound: '-infinity', upper_bound: 'infinity' )
askey_wilson.parameters.create(name: 'c', lower_bound: '-infinity', upper_bound: 'infinity' )
askey_wilson.parameters.create(name: 'd', lower_bound: '-infinity', upper_bound: 'infinity' )
askey_wilson.category = qpolynomials
askey_wilson.save

#
# q-Racah
#
qracah = Polynomial.new({
  name:       'q-Racah',
  sid:        'qracah',
  definition: 'R_n(\lambda(x);b,c,d,N;q) = \sum_{k=0}^\infty \frac{(q^{-n};q)_k (bq^{n-N};q)_k (q^{-x};q)_k (c d q^{x+1};q)_k}{(q^{-N};q)_k (bdq;q)_k (cq;q)_k (q;q)_k} = {}_{4}\phi_{3}\!\left(\left. {q^{-n}, bq^{n-N}, q^{-x}, cdq^{x+1} \atop q^{-N}, bdq, cq} \; \right| q ; q \right)',
  maple:      'qpochhammer(q^{-n},q,k)*qpochhammer(b*q^{n-N},q,k)*qpochhammer(q^{-x},q,k)*qpochhammer(cdq^{x+1},q,k)/qpochhammer(q^{-N},q,k)/qpochhammer(bdq,q,k)/qpochhammer(cq,q,k)/qpochhammer(q,q,k)',
  type:       'discrete'
})
qracah.parameters.create(name: 'b', lower_bound: '-infinity', upper_bound: 'infinity' )
qracah.parameters.create(name: 'c', lower_bound: '-infinity', upper_bound: 'infinity' )
qracah.parameters.create(name: 'd', lower_bound: '-infinity', upper_bound: 'infinity' )
qracah.category = qpolynomials
qracah.save

#
# Continuous Dual q-Hahn 
#
continuous_dual_qhahn = Polynomial.new({
  name:       'Continuous Dual q-Hahn',
  sid:        'continuous_dual_qhahn',
  definition: 'p_n(x;a,b,c|q) = a^{-n}(a b, a c; q)_n \sum_{k=0}^\infty \frac{(q^{-n};q)_k (a e^{it};q)_k (ae^{-it};q)_k}{(ab;q)_k (ac;q)_k (q;q)_k} = a^{-n}(a b, a c; q)_n {}_{3}\phi_{2}\!\left(\left. {q^{-n}, a e^{it}, a e^{-it} \atop ab, ac} \; \right| q ; q \right)\quad , x=\cos(t)',
  maple:      'a^(-n)*qpochhammer(a*b,q,n)*qpochhammer(a*c,q,n)*qpochhammer(q^(-n),q,k)*qpochhammer(a*exp(I*t),q,k)*qpochhammer(a*exp(-I*t),q,k)/qpochhammer(a*b,q,k)/qpochhammer(a*c,q,k)/qpochhammer(q,q,k)',
  type:       'discrete'
})
continuous_dual_qhahn.parameters.create(name: 'a', lower_bound: '-infinity', upper_bound: 'infinity' )
continuous_dual_qhahn.parameters.create(name: 'b', lower_bound: '-infinity', upper_bound: 'infinity' )
continuous_dual_qhahn.parameters.create(name: 'c', lower_bound: '-infinity', upper_bound: 'infinity' )
continuous_dual_qhahn.category = qpolynomials
continuous_dual_qhahn.save

#
# Continuous q-Hahn 
#
continuous_qhahn = Polynomial.new({
  name:       'Continuous q-Hahn',
  sid:        'continuous_qhahn',
  definition: 'p_n(x;a,b,c|q) = a^{-n} e^{-inu} (abe^{2iu}, a c, a d; q)_n \sum_{k=0}^\infty \frac{(q^{-n};q)_k (abcdq^{n-1};q)_k (ae^{i(t+2u};q)_k (ae^{-it};q)_k}{(abe^{2iu};q)_k (ac;q)_k (ad;q)_k (q;q)_k} = a^{-n} e^{-inu} (abe^{2iu}, a c, a d; q)_n {}_{4}\phi_{3}\!\left(\left. {q^{-n}, abcdq^{n-1}, ae^{i(t+2u)}, a e^{-it} \atop abe^{2iu}, ac, ad} \; \right| q ; q \right)\quad , x=\cos(t+u)',
  maple:      'a^(-n)*exp(i*n*u)*qpochhammer(a*b*e^(2*i*u),q,n)*qpochhammer(a*c,q,n)*qpochhammer(a*d,q,n)*qpochhammer(q^(-n),q,k)*qpochhammer(abcdq^(-n),q,k)*qpochhammer(a*exp(I*(t+2*u)),q,k)*qpochhammer(a*exp(-I*t),q,k)/qpochhammer(a*b*exp(2*i*u),q,k)/qpochhammer(a*c,q,k)/qpochhammer(a*d,q,k)/qpochhammer(q,q,k)',
  type:       'discrete'
})
continuous_qhahn.parameters.create(name: 'a', lower_bound: '-infinity', upper_bound: 'infinity' )
continuous_qhahn.parameters.create(name: 'b', lower_bound: '-infinity', upper_bound: 'infinity' )
continuous_qhahn.parameters.create(name: 'c', lower_bound: '-infinity', upper_bound: 'infinity' )
continuous_qhahn.parameters.create(name: 'd', lower_bound: '-infinity', upper_bound: 'infinity' )
continuous_qhahn.category = qpolynomials
continuous_qhahn.save

#
# Big q-Jacobi
#
big_qjacobi = Polynomial.new({
  name:       'Big q-Jacobi',
  sid:        'big_qjacobi',
  definition: 'P_n(x;a,b,c;q) = \sum_{k=0}^\infty \frac{(q^{-n};q)_k (abq^{n+1};q)_k (x;q)_k}{(aq;q)_k (cq;q)_k (q;q)_k} = {}_{3}\phi_{2}\!\left(\left. {q^{-n}, abq^{n+1}, x \atop aq, cq} \; \right| q ; q \right)',
  maple:      'qpochhammer(q^(-n),q,k)*qpochhammer(abq^(n+1),q,k)*qpochhammer(x,q,k)/qpochhammer(a*q,q,k)/qpochhammer(c*q,q,k)/qpochhammer(q,q,k)',
  type:       'continuous'
})
big_qjacobi.parameters.create(name: 'a', lower_bound: '0', upper_bound: 'q^{-1}' )
big_qjacobi.parameters.create(name: 'b', lower_bound: '0', upper_bound: 'q^{-1}' )
big_qjacobi.parameters.create(name: 'c', lower_bound: '-infinity', upper_bound: '0' )
big_qjacobi.category = qpolynomials
big_qjacobi.save

#
# q-Hahn
#
qhahn = Polynomial.new({
  name:       'q-Hahn',
  sid:        'qhahn',
  definition: 'Q_n(q^{-x};a,b,N|q) = \sum_{k=0}^\infty \frac{(q^{-n};q)_k (abq^{n+1};q)_k (q^{-x};q)_k}{(aq;q)_k (q^{-N};q)_k (q;q)_k} = {}_{3}\phi_{2}\!\left(\left. {q^{-n}, abq^{n+1}, q^{-x} \atop aq, q^{-N}} \; \right| q ; q \right)',
  maple:      'qpochhammer(q^(-n),q,k)*qpochhammer(abq^(n+1),q,k)*qpochhammer(q^{-x},q,k)/qpochhammer(a*q,q,k)/qpochhammer(q^{-N},q,k)/qpochhammer(q,q,k)',
  type:       'discrete'
})
qhahn.parameters.create(name: 'a', lower_bound: '0', upper_bound: 'q^{-1}' )
qhahn.parameters.create(name: 'b', lower_bound: '0', upper_bound: 'q^{-1}' )
qhahn.category = qpolynomials
qhahn.save

#
# Al-Saham-Chihara
#
al_salam_chihara = Polynomial.new({
  name:       'Al Salam Chihara',
  sid:        'al_salam_chihara',
  definition: 'Q_n(q^{-x};a,b|q) = a^{-n} (ab;q)_n \sum_{k=0}^\infty \frac{(q^{-n};q)_k (a e^{it};q)_k (a e^{-it};q)_k}{(ab;q)_k (0;q)_k (q;q)_k} = {}_{3}\phi_{2}\!\left(\left. {q^{-n}, a e^{it}, a e^{-it} \atop ab, 0} \; \right| q ; q \right)\quad x=\cos(t)',
  maple:      'qpochhammer(q^(-n),q,k)*qpochhammer(a*exp(I*t),q,k)*qpochhammer(a*exp(-I*t),q,k)/qpochhammer(a*b,q,k)/qpochhammer(0,q,k)/qpochhammer(q,q,k)',
  type:       'discrete'
})
al_salam_chihara.parameters.create(name: 'a', lower_bound: '-infinity', upper_bound: 'infinity' )
al_salam_chihara.parameters.create(name: 'b', lower_bound: '-infinity', upper_bound: 'infinity' )
al_salam_chihara.category = qpolynomials
al_salam_chihara.save

#
# q-Meixner-Pollaczek
#
qmeixner_pollaczek = Polynomial.new({
  name:       'q-Meixner Pollaczek',
  sid:        'qmeixner_pollaczek',
  definition: 'P_n(x;a|q) = a^{-n} e^{-inu} \frac{(a^2;q)_n}{(q;q)_n} \sum_{k=0}^\infty \frac{(q^{-n};q)_k (a e^{i(t+2u)};q)_k (a e^{-it};q)_k}{(a^2;q)_k (0;q)_k (q;q)_k} =  a^{-n} e^{-inu} \frac{(a^2;q)_n}{(q;q)_n} {}_{3}\phi_{2}\!\left(\left. {q^{-n}, a e^{i(t+2u)}, a e^{-it} \atop a^2, 0} \; \right| q ; q \right)\quad x=\cos(t)',
  maple:      'qpochhammer(q^(-n),q,k)*qpochhammer(a*exp(I*(t+2u)),q,k)*qpochhammer(a*exp(-I*t),q,k)/qpochhammer(a^2,q,k)/qpochhammer(0,q,k)/qpochhammer(q,q,k)',
  type:       'discrete'
})
qmeixner_pollaczek.parameters.create(name: 'a', lower_bound: '-infinity', upper_bound: 'infinity' )
qmeixner_pollaczek.category = qpolynomials
qmeixner_pollaczek.save

#
# Continuous q-Jacobi
#
continuous_qjacobi = Polynomial.new({
  name:       'Continuous q-Jacobi',
  sid:        'continuous_qjacobi',
  definition: 'P_n^{(a,b)}(x|q) = \frac{(q^{a+1};q,n)}{(q;q,n)} \sum_{k=0}^\infty \frac{(q^{-n};q)_k (q^{n+a+b+1};q)_k (q^{1/2a+1/4}e^{it};q)_k (q^{1/2a+1/4}e^{-it};q)_k}{(q^{a+1};q)_k (-q^{1/2(a+b+1)};q)_k (-q^{1/2(a+b+2)};q)_k (q;q)_k} = \frac{(q^{a+1};q,n)}{(q;q,n)} {}_{3}\phi_{2}\!\left(\left. {q^{-n}, q^{n+a+b+1}, q^{1/2a+1/4}e^{it}, q^{1/2a+1/4}e^{-it} \atop q^{a+1}, -q^{1/2(a+b+1)}, -q^{1/2(a+b+2)}} \; \right| q ; q \right)\quad x=\cos(t)',
  maple:      'qpochhammer(q^(a+1),q,n)/qpochhammer(q,q,n)*qpochhammer(q^(-n),q,k)*qpochhammer(q^(n+a+b+1),q,k)*qpochhammer(q^(1/2*a+1/4)*exp(I*t),q,k)/qpochhammer(a*q,q,k)/qpochhammer(c*q,q,k)/qpochhammer(q,q,k)',
  type:       'continuous'
})
continuous_qjacobi.parameters.create(name: 'a', lower_bound: '-\frac{1}{2}', upper_bound: 'q^{-1}' )
continuous_qjacobi.parameters.create(name: 'b', lower_bound: '-\frac{1}{2}', upper_bound: 'q^{-1}' )
continuous_qjacobi.category = qpolynomials
continuous_qjacobi.save

#
# q-Laguerre
#
qlaguerre = Polynomial.new({
  name:       'q-Laguerre',
  sid:        'qlaguerre',
  definition: 'L_n^{(a)}(x;q) = \frac{(q^{a+1};q)_n}{(q;q)_n} {}_{1}\phi_{1}\!\left(\left. {q^{-n} \atop q^{a+1}} \; \right| q ; -q^{n+a+1}x \right)',
  maple:      'qpochhammer(q^(-n),q,k)/qpochhammer(q^(a+1),q,k)/qpochhammer(q,q,k)*(-q^(n+a+1)*x)^k*(-1)^k*q^binomial(k,2)',
  type:       'continuous'
})
qlaguerre.parameters.create(name: 'a', lower_bound: '-1', upper_bound: 'infinity' )
qlaguerre.category = qpolynomials
qlaguerre.save

#
# q-Charlier
#
qcharlier = Polynomial.new({
  name:       'q-Charlier',
  sid:        'qcharlier',
  definition: 'C_n(q^{-x}; a; q) = (-a^{-1}q;q)_n {}_{1}\phi_{1}\!\left(\left. {q^{-n} \atop -a^{-1}q} \; \right| q ; -\frac{q^{n+1-x}}{a} \right)',
  maple:      'qpochhammer(q^(-n),q,k)/qpochhammer(-q/a,q,k)/qpochhammer(q,q,k)*(-q^(n+1)*q^(-x)/a)^k*(-1)^k*q^binomial(k,2)',
  type:       'discrete'  
})
qcharlier.parameters.create(name: 'a', lower_bound: '0', upper_bound: 'infinity' )
qcharlier.category = qpolynomials
qcharlier.save

#
# Discrete q-Hermite I
#
discrete_qhermiteI = Polynomial.new({
  name:       'Discrete q-Hermite I',
  sid:        'discrete_qhermiteI',
  definition: 'h_n(x;q) = q^{n \choose 2} {}_{2}\phi_{1}\!\left(\left. {q^{-n}, x^{-1} \atop 0} \; \right| q ; -q x \right)',
  maple:      'qpochhammer(q^(-n),q,k)*qpochhammer(1/x,q,k)/qpochhammer(q,q,k)*(-q*x)^k',
  type:       'continuous' # in fact its type is 'discrete', but to invoke the correct command here it is necessary to have type continuous  
})
discrete_qhermiteI.category = qpolynomials
discrete_qhermiteI.save