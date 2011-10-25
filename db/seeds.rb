# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rake db:seed (or created alongside the db with db:setup).
#
# Examples:
#
#   cities = City.create([{ name: 'Chicago' }, { name: 'Copenhagen' }])
#   Mayor.create(name: 'Emanuel', city: cities.first)


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
  maple:      '',
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
  maple:      '',
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
  maple:      '',
})
hermite.category = polynomials
hermite.save


