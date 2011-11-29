class QpolynomialsController < ApplicationController

  before_filter :set_menu_item

  def index
    q = Category.where(sid: 'qpolynomials').first
    @polynomials = q.polynomials
  end
  
  def show
    q = Category.where(sid: 'qpolynomials').first
    @polynomials = q.polynomials
    @polynomial = Polynomial.where(sid: params[:id]).first
    
    @diffeq = (@polynomial.type == 'continuous') ?  t('qdifferential_equation') : t('qdifference_equation')
    @receq = t('qrecurrence_equation')
    
    respond_to do |format|
      format.html 
      format.js 
    end
  end
  
  def compute
    @polynomial = Polynomial.where(sid: params[:id]).first
    render :text => @polynomial.compute(params[:parameters], params[:type])  
  end
  
  private
  
  def set_menu_item
    @menu_item = 'qpolynomials'
  end
  
end