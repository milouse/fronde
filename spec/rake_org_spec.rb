# frozen_string_literal: true

require 'rake'
require 'open-uri'
require 'fileutils'

describe 'With working org files' do
  before(:all) do
    @org_last_version = Neruda::Config.org_last_version
    Dir.chdir 'spec/data'
    rakefile = <<~RAKE
      # frozen_string_literal: true

      Dir.pwd
      Dir.glob('../../lib/tasks/*.rake').each { |r| import r }

      task default: 'site:build'
    RAKE
    IO.write('Rakefile', rakefile)
    @rake = Rake.application
    Rake.verbose(false)
    @rake.raw_load_rakefile
    @rake.options.build_all = true
  end

  after(:all) do
    FileUtils.rm_r "org-#{@org_last_version}"
    FileUtils.rm ['org-config.el', 'Rakefile']
  end

  it 'Should do stuff' do
    @rake.invoke_task('org:install')
    expect(File.exist?('org-config.el')).to be(true)
    expect(Dir.exist?("org-#{@org_last_version}")).to be(true)
  end
end
