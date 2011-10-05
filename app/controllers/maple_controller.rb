class MapleController < ActionController::Base
  
  # MAPLE_PATH = 'usr/local/maple12/bin'
  MAPLE_PATH = 'ssh sprenger@pool-serv1 "/usr/local/maple12/bin/maple -q -s < /tmp/test.txt"'
  
  def calculate(input)
    
    
    flags = ' -q -s '
    system(MAPLE_PATH + '/maple' + flags + input)
    return output
  end
end