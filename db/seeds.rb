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
application_page.paragraphs << ParagraphParagraph.new(area_name: 'Entry_Page', body: "Most computations in CAOP are performed by calling procedures from <a href=\"http://www.mathematik.uni-kassel.de/%7Ekoepf/Publikationen/#down\">hsum13</a> (Package \"Hypergeometric Summation\", Maple 6-13, Copyright 1998-2009, <a href=\"http://www.mathematik.uni-kassel.de/%7Ekoepf/\">Wolfram Koepf</a>, University of Kassel).")
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
# Jacobi
#
jacobi = Polynomial.new({
  name:       'Jacobi',
  sid:        'jacobi',
  definition: 'P_n^{(a,b)}(x) = \frac{(a+1)_n}{n!} {}_2F_1 \left(\left. {-n, n+a+b+1 \atop a+1} \; \right| \frac{1-x}{2} \right)',
  maple:      'pochhammer(-n,k)*pochhammer(n+a+b+1,k)/pochhammer(a+1,k)/k!*((1-x)/2)^k',
  type:       'continuous'
})
jacobi.parameters.create(name: 'a', lower_bound: -1, upper_bound: Float::MAX)
jacobi.parameters.create(name: 'b', lower_bound: -1, upper_bound: Float::MAX)
jacobi.category = polynomials
jacobi.save

#
# Charlier
#
charlier = Polynomial.new({
  name:       'Charlier',
  sid:        'charlier',
  definition: 'C_n(x,a) = {}_2F_0 \left(\left. {-n, -x \atop -} \; \right| -\frac{1}{a} \right)',
  maple:      'pochhammer(-n,k)*pochhammer(-x,k)/k!*(-1/a)^k',
  type:       'discrete'
})
charlier.parameters.create(name: 'a', lower_bound: 0, upper_bound: Float::MAX)
charlier.category = polynomials
charlier.save

#
# Hermite
#
hermite = Polynomial.new({
  name:       'Hermite',
  sid:        'hermite',
  definition: 'H_n(x) = (2x)^n {}_2F_0 \left(\left. {-n/2, -(n-1)/2 \atop -} \; \right| -\frac{1}{x^2} \right)',
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
# q-Laguerre
#
qlaguerre = Polynomial.new({
  name:       'q-Laguerre',
  sid:        'qlaguerre',
  definition: 'L_n^{(a)}(x;q) = \frac{(q^{a+1};q)_n}{(q;q)_n} {}_{1}\phi_{1}\!\left(\left. {q^{-n} \atop q^{a+1}} \; \right| q ; -q^{n+a+1}x \right)',
  maple:      'qpochhammer(q^(-n),q,k)/qpochhammer(q^(a+1),q,k)/qpochhammer(q,q,k)*(-q^(n+a+1)*x)^k*(-1)^k*q^binomial(k,2)',
  type:       'continuous'
})
qlaguerre.parameters.create(name: 'a', lower_bound: -1, upper_bound: Float::MAX)
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
qcharlier.parameters.create(name: 'a', lower_bound: 0, upper_bound: Float::MAX)
qcharlier.category = qpolynomials
qcharlier.save

#
# Discrete q-Hermite I
#
discqhermiteI = Polynomial.new({
  name:       'Discrete q-Hermite I',
  sid:        'discqhermiteI',
  definition: 'h_n(x;q) = q^{n \choose 2} {}_{2}\phi_{1}\!\left(\left. {q^{-n}, x^{-1} \atop 0} \; \right| q ; -q x \right)',
  maple:      'qpochhammer(q^(-n),q,k)*qpochhammer(1/x,q,k)/qpochhammer(q,q,k)*(-q*x)^k',
  type:       'continuous' # in fact its type is 'discrete', but to invoke the correct command here it is necessary to have type continuous  
})
discqhermiteI.category = qpolynomials
discqhermiteI.save