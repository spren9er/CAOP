class ApplicationController < ActionController::Base
  protect_from_forgery

  def contentr_authorized?
    session[:authorized]
  end

  def login
    if request.post?
      set_credentials
      password = ::ApplicationController::Password
      username = ::ApplicationController::Username
      
      if params[:password] == password and params[:username] == username
        session[:authorized] = true
        redirect_to root_path, :notice => 'Successfully logged in!'
        return
      end     
      
      flash[:alert] = 'Authorization failed!'
    end
    render :layout => 'login'
  end
  
  def logout
    session[:authorized] = false
    redirect_to root_path, :notice => 'Successfully logged out!'
  end
  
  protected

  def set_credentials
    if !::ApplicationController.const_defined? "Password"
      un, pw = nil
      fname = File.join(Rails::root, 'config', 'htpasswd.txt')
      if File.exists? fname
        s = File.new(fname).read.strip
        raise 'wrong format for config/htpasswd.txt, should be username:password' unless /^\w+:\w+$/.match s
        un, pw = s.split(':')
      end
      ::ApplicationController.const_set("Password", pw)
      ::ApplicationController.const_set("Username", un)
    end
  end
  
  private
  
  def stats_log
    ip_address = request.env['REMOTE_ADDR']
    @stat = Stat.where(:polynomial_sid => params[:id], :ip_address => ip_address).first
    unless @stat
      polynomial = Polynomial.where(sid: params[:id]).first
      Stat.create!(:polynomial_id => polynomial.id, :polynomial_sid => polynomial.sid, :ip_address => ip_address, :category => polynomial.category.sid, :created_at => Time.now)  
    end
  end
  
end