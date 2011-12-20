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
    redirect_to :back, :notice => 'Successfully logged out!'
  end
  
  protected

  def set_credentials
    if !::ApplicationController.const_defined? "Password"
      un, pw = nil
      fname = File.join(Rails::root, 'config', 'htpasswd.txt')
      if File.exists? fname
        s = File.new(fname).read.strip
        raise 'wrong format for config/htpasswd.txt, shoud be username:password' unless /^\w+:\w+$/.match s
        un, pw = s.split(':')
      end
      ::ApplicationController.const_set("Password", pw)
      ::ApplicationController.const_set("Username", un)
    end
  end
  
end
