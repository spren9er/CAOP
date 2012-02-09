class StatsController < ApplicationController

  before_filter :check_authorized
  
  def index
    @menu_item = 'stats'

    polynomials_stats
    qpolynomials_stats

    # last five ip_addresses
    n = 10
    used_ip_addresses = []    
    latest_stats = Stat.order_by('created_at').reverse
    latest_stats.each do |s|
      unless used_ip_addresses.include?(s.ip_address) 
        used_ip_addresses << s.ip_address
      end
      break if used_ip_addresses.count > n-1
    end
    
    @latest_stats = used_ip_addresses.inject([]) do |s, ip|
      ip_stats = Stat.where(ip_address: ip).order_by('created_at').reverse
        last_ip_stat = ip_stats.first
        last_ip_stat.count = ip_stats.count
        s << last_ip_stat
        s
    end    
  end

  private
  
  ['polynomials_stats', 'qpolynomials_stats'].each do |method_name|
    define_method method_name do
      stats = Stat.where(category: method_name.gsub('_stats',''))
      aggregated_stats = stats.inject({}) do |h, stat|
        h[stat.polynomial_id] ||= 0
        h[stat.polynomial_id] += 1
        h
      end
      instance_variable_set "@#{method_name}", aggregated_stats
    end 
  end
  
  def check_authorized
    render :nothing => true, :status => 500 unless session[:authorized]
    return
  end
  
end