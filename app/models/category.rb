class Category
  include Mongoid::Document
  field :name,       type: String
  field :sid,        type: String
  
  has_many :polynomials
end