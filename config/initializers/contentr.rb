# coding: utf-8

Contentr.setup do |config|
  config.site_name    = 'CAOP'
  config.default_page = 'application'
  #config.run_node_checks = false

  config.register_paragraph(OverviewParagraph, 'Overview')
end
