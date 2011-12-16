class ApplicationController < ActionController::Base
  protect_from_forgery

  def contentr_authorized?
    session[:htaccess_authenticated]
  end

  def login
    htaccess
    redirect_to :root, :notice => 'Successfully logged in!'
  end
  
  def logout
    session[:htaccess_authenticated] = false
    ::ApplicationController.const_set("Password", nil)
    ::ApplicationController.const_set("Username", nil)
    redirect_to :back, :notice => 'Successfully logged out!'
  end
  
  protected

  def htaccess
    return true if session[:htaccess_authenticated] == true
    if ! ::ApplicationController.const_defined? "Password"
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
    password = ::ApplicationController::Password
    username = ::ApplicationController::Username
    return true unless password.present?
    session[:htaccess_authenticated] = authenticate_or_request_with_http_basic do |user_name, passwd|
      user_name == username && password == passwd
    end
  end
  
end
