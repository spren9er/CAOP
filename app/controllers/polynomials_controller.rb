class PolynomialsController < ApplicationController

  before_filter :set_menu_item

  def index
    nonq = Category.where(sid: 'polynomials').first
    @polynomials = nonq.polynomials
  end
  
  def show
    nonq = Category.where(sid: 'polynomials').first
    @polynomials = nonq.polynomials
    @polynomial = Polynomial.where(sid: params[:id]).first
    
    @diffeq = (@polynomial.type == 'continuous') ?  t('differential_equation') : t('difference_equation')
    @receq = t('recurrence_equation')
    
    respond_to do |format|
      format.html 
      format.js 
    end
  end
  
  def compute
    @polynomial = Polynomial.where(sid: params[:id]).first
    render :text => @polynomial.compute(params[:parameters], params[:type], params[:factor])  
  end  
  
  private
  
  def set_menu_item
    @menu_item = 'polynomials'
  end
  
end