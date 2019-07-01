# frozen_string_literal: true

require 'rake'
require 'open-uri'

describe 'With working org files' do
  before(:all) do
    init_testing_website
    @org_dir = "org-#{Neruda::Config.org_last_version}"
    # When run with all other specs, config may have been already loaded
    Neruda::Config.send(:load_settings)
    @rake = Rake.application
    @rake.raw_load_rakefile
    Rake.verbose(false)
  end

  before(:each) do
    @rake.options.build_all = true
    @rake.tasks.each(&:reenable)
  end

  after(:each) do
    FileUtils.rm 'org-config.el', force: true
  end

  after(:all) do
    Dir.chdir File.expand_path('../', __dir__)
    FileUtils.rm_r 'spec/data/website_testing', force: true
  end

  it 'should compile org-config.el', rake: true do
    @rake.invoke_task('org-config.el')
    expect(File.exist?('org-config.el')).to be(true)
    proof = File.expand_path('data/org-config-proof.el', __dir__)
    proof_content = IO.read(proof).gsub(/__TEST_DIR__/, Dir.pwd)
    expect(IO.read('org-config.el')).to eq(proof_content)
  end

  it 'Should install org-mode', rake: true do
    @rake.invoke_task('org:install')
    expect(File.exist?('org-config.el')).to be(true)
    expect(File.exist?("#{@org_dir}/lisp/org-loaddefs.el")).to be(true)
  end

  it 'Should install org-mode in verbose mode', rake: true do
    # This one is mainly for coverage
    FileUtils.rm_r @org_dir, force: true
    Rake.verbose(true)
    @rake.invoke_task('org:install')
    expect(File.exist?('org-config.el')).to be(true)
    expect(File.exist?("#{@org_dir}/lisp/org-loaddefs.el")).to be(true)
  end
end
