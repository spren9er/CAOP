###
#
#  CATEGORIES
#
###

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