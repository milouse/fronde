# frozen_string_literal: true

require 'rake'
require 'open-uri'
require 'fileutils'

describe 'With working org files' do
  before(:all) do
    @org_dir = "org-#{Neruda::Config.org_last_version}"
    Dir.chdir 'spec/data'
    rakefile = <<~RAKE
      # frozen_string_literal: true

      Dir.pwd
      Dir.glob('../../lib/tasks/*.rake').each { |r| import r }

      Neruda::Config.load_test('TEST' => 'test')

      task default: 'site:build'
    RAKE
    IO.write('Rakefile', rakefile)
    @rake = Rake.application
    Rake.verbose(false)
    @rake.raw_load_rakefile
    @rake.options.build_all = true
  end

  after(:all) do
    FileUtils.rm_r @org_dir, force: true
    FileUtils.rm ['org-config.el', 'Rakefile'], force: true
  end

  it 'Should do stuff' do
    @rake.invoke_task('org:install')
    expect(File.exist?('org-config.el')).to be(true)
    expect(Dir.exist?(@org_dir)).to be(true)
  end
end
