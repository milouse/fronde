# frozen_string_literal: true

begin
  require 'yard'
  require 'rspec/core/rake_task'

  RSpec::Core::RakeTask.new(:spec)

  YARD::Rake::YardocTask.new do |t|
    t.options = ['-m', 'org', '--no-progress',
                 '--files', 'CONFIG.org',
                 '--asset', 'Firma_Pablo_Neruda.png']
  end
rescue LoadError
  warn 'You are not using the development bundle.'
end
