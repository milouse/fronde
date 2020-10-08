# frozen_string_literal: true

require 'rake'

describe 'With working org files' do
  before(:all) do
    init_testing_website
    @org_dir = "org-#{Neruda::Config.org_last_version}"
    @rake = Rake.application
    @rake.raw_load_rakefile
    Rake.verbose(false)
  end

  before(:each) do
    @rake.options.build_all = true
    @rake.tasks.each(&:reenable)
    Neruda::Config.send(:load_settings)
  end

  after(:each) do
    Dir.glob('org-[0-9.]*').each { |ov| FileUtils.rm_r(ov, force: true) }
    FileUtils.rm ['.dir-locals.el', 'org-config.el'], force: true
    Neruda::Config.load_test({})
  end

  after(:all) do
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/website_testing', force: true
  end

  it 'should compile org-config.el', rake: true do
    @rake.invoke_task('org-config.el')
    expect(File.exist?('org-config.el')).to be(true)
    proof = File.expand_path('data/org-config-proof.el', __dir__)
    base_dir = File.expand_path('../', __dir__)
    proof_content = IO.read(proof)
                      .gsub(/__TEST_DIR__/, Dir.pwd)
                      .gsub(/__BASE_DIR__/, base_dir)
                      .gsub(/__VERSION__/, Neruda::VERSION)
                      .gsub(/__ORG_VERSION__/, Neruda::Config.org_last_version)
    expect(IO.read('org-config.el')).to eq(proof_content)
  end

  it 'should compile org-config.el for blog', rake: true do
    old_conf = Neruda::Config.settings.dup
    old_conf['theme'] = 'toto'
    Neruda::Config.load_test(old_conf)
    @rake.invoke_task('org-config.el')
    expect(File.exist?('org-config.el')).to be(true)
    proof = File.expand_path('data/org-config-blog-proof.el', __dir__)
    base_dir = File.expand_path('../', __dir__)
    proof_content = IO.read(proof)
                      .gsub(/__TEST_DIR__/, Dir.pwd)
                      .gsub(/__BASE_DIR__/, base_dir)
                      .gsub(/__VERSION__/, Neruda::VERSION)
                      .gsub(/__ORG_VERSION__/, Neruda::Config.org_last_version)
    expect(IO.read('org-config.el')).to eq(proof_content)
  end

  it 'should create .dir-locals.el', rake: true do
    @rake.invoke_task('.dir-locals.el')
    expect(File.exist?('.dir-locals.el')).to be(true)
    proof = File.expand_path('org-config.el', Dir.pwd)
    expect(IO.read('.dir-locals.el')).to(
      eq("((org-mode . ((eval . (load-file \"#{proof}\")))))")
    )
  end

  it 'Should install org-mode', rake: true do
    @rake.invoke_task('org:install')
    expect(File.exist?('org-config.el')).to be(true)
    expect(File.exist?('.dir-locals.el')).to be(true)
    expect(File.exist?("#{@org_dir}/lisp/org-loaddefs.el")).to be(true)
  end

  it 'Should remove old org-mode version while installing', rake: true do
    FileUtils.mkdir 'org-2.3'
    expect(Dir.exist?('org-2.3')).to be(true)
    @rake.invoke_task('org:install')
    expect(Dir.exist?('org-2.3')).to be(false)
  end

  it 'Should install org-mode in verbose mode', rake: true do
    # This one is mainly for coverage
    FileUtils.rm_r @org_dir, force: true
    Rake.verbose(true)
    @rake.invoke_task('org:install')
    @rake.tasks.each(&:reenable)
    @rake.invoke_task('org:install') # To also check coverage
    expect(File.exist?('org-config.el')).to be(true)
    expect(File.exist?('.dir-locals.el')).to be(true)
    expect(File.exist?("#{@org_dir}/lisp/org-loaddefs.el")).to be(true)
  end
end
