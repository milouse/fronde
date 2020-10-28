# frozen_string_literal: true

require 'rake'

context 'with working org files' do
  let(:org_dir) { "org-#{Fronde::Config.org_last_version}" }

  before do
    init_testing_website
    Fronde::Config.send(:load_settings)
  end

  after do
    Dir.glob('org-[0-9.]*').each { |ov| FileUtils.rm_r(ov, force: true) }
    FileUtils.rm ['.dir-locals.el', 'org-config.el'], force: true
    Fronde::Config.load_test({})
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/website_testing', force: true
  end

  it 'compiles org-config.el', rake: true do
    rake.invoke_task('org-config.el')
    expect(File.exist?('org-config.el')).to be(true)
    proof = File.expand_path('data/org-config-proof.el', __dir__)
    base_dir = File.expand_path('../', __dir__)
    proof_content = IO.read(proof)
                      .gsub(/__TEST_DIR__/, Dir.pwd)
                      .gsub(/__BASE_DIR__/, base_dir)
                      .gsub(/__VERSION__/, Fronde::VERSION)
                      .gsub(/__ORG_VERSION__/, Fronde::Config.org_last_version)
    expect(IO.read('org-config.el')).to eq(proof_content)
  end

  it 'compiles org-config.el for blog', rake: true do
    old_conf = Fronde::Config.settings.dup
    old_conf['theme'] = 'toto'
    Fronde::Config.load_test(old_conf)
    rake.invoke_task('org-config.el')
    expect(File.exist?('org-config.el')).to be(true)
    proof = File.expand_path('data/org-config-blog-proof.el', __dir__)
    base_dir = File.expand_path('../', __dir__)
    proof_content = IO.read(proof)
                      .gsub(/__TEST_DIR__/, Dir.pwd)
                      .gsub(/__BASE_DIR__/, base_dir)
                      .gsub(/__VERSION__/, Fronde::VERSION)
                      .gsub(/__ORG_VERSION__/, Fronde::Config.org_last_version)
    expect(IO.read('org-config.el')).to eq(proof_content)
  end

  it 'creates .dir-locals.el', rake: true do
    rake.invoke_task('.dir-locals.el')
    expect(File.exist?('.dir-locals.el')).to be(true)
    proof = File.expand_path('org-config.el', Dir.pwd)
    expect(IO.read('.dir-locals.el')).to(
      eq("((org-mode . ((eval . (load-file \"#{proof}\")))))")
    )
  end

  it 'installs Org', rake: true do
    copy_org_tarball_to_fake_tmp
    rake(verbose: false).invoke_task('org:install')
    expect(File.exist?('org-config.el')).to be(true)
    expect(File.exist?('.dir-locals.el')).to be(true)
    expect(File.exist?("#{org_dir}/lisp/org-loaddefs.el")).to be(true)
  end

  it 'installs Org in verbose mode', rake: true do
    # This one is mainly for coverage
    FileUtils.rm_r org_dir, force: true
    copy_org_tarball_to_fake_tmp
    rake(verbose: true).invoke_task('org:install')
    expect(File.exist?('org-config.el')).to be(true)
    expect(File.exist?('.dir-locals.el')).to be(true)
    expect(File.exist?("#{org_dir}/lisp/org-loaddefs.el")).to be(true)
  end

  it 'removes old Org version while installing', rake: true do
    FileUtils.mkdir 'org-2.3'
    expect(Dir.exist?('org-2.3')).to be(true)
    copy_org_tarball_to_fake_tmp
    rake(verbose: true).invoke_task('org:install')
    rake(verbose: false).invoke_task('org:install') # To check coverage
    expect(Dir.exist?('org-2.3')).to be(false)
  end
end
