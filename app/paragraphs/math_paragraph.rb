class MathParagraph < Contentr::Paragraph
  
  field :math,      type: String
  field :inline,    type: Boolean,  default: false

end