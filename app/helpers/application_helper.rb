module ApplicationHelper
  
  def menu_item(options = {:item => 'item_name', :label => 'label_name', :url => root_path, :class => nil, :span => nil}, &block)
    css = (options[:item] == @menu_item ? 'active' : '')
    css << ' ' + options[:class] if options[:class].present?
    css.strip!
    css = nil if css.blank?
    label = options[:label] || ''
    label << (options[:span].present? ? content_tag(:span, options[:span]) : '')
    
    content_tag(:li, :class => css) do
      concat(link_to(label.html_safe, options[:url]) + (block_given? ? yield : ''))
    end 
  end
  
  def link_to_polynomial(polynomial)
    sid = polynomial.sid
    link_to polynomial.name, polynomial.q? ? qpolynomial_path(sid) : polynomial_path(sid), sid == params[:id] ? {:class => 'active'} : nil
  end
  
  def html_title
    title = "CAOP - Computer Algebra & Orthogonal Polynomials"
    @polynomial_title ? "#{title} - #{@polynomial_title} Polynomials" : title
  end
  
end
