# frozen_string_literal: true

require 'rake'

context 'with working org files' do
  let(:org_dir) { "lib/org-#{Fronde::CONFIG.org_last_version}" }

  before do
    init_testing_environment
    Fronde::CONFIG.reset
  end

  after do
    Dir.glob('lib/org-[0-9.]*').each { |ov| FileUtils.rm_r(ov, force: true) }
    FileUtils.rm ['.dir-locals.el', 'var/lib/org-config.el'], force: true
    tear_down 'tmp/website_testing'
  end

  it 'compiles org-config.el', rake: true do
    rake.invoke_task('var/lib/org-config.el')
    expect(File.exist?('var/lib/org-config.el')).to be(true)
    proof = proof_content 'org-config-proof.el'
    expect(File.read('var/lib/org-config.el')).to eq(proof)
  end

  it 'compiles org-config.el for blog', rake: true do
    old_conf = Fronde::CONFIG.settings.merge
    old_conf['theme'] = 'toto'
    Fronde::CONFIG.load_test(old_conf)
    rake.invoke_task('var/lib/org-config.el')
    expect(File.exist?('var/lib/org-config.el')).to be(true)
    proof = proof_content('org-config-blog-proof.el')
    expect(File.read('var/lib/org-config.el')).to eq(proof)
  end

  it 'creates .dir-locals.el', rake: true do
    rake.invoke_task('.dir-locals.el')
    expect(File.exist?('.dir-locals.el')).to be(true)
    proof = File.expand_path('var/lib/org-config.el', Dir.pwd)
    expect(File.read('.dir-locals.el')).to(
      eq("((org-mode . ((eval . (load-file \"#{proof}\")))))")
    )
  end

  it 'installs Org', rake: true do
    copy_org_tarball_to_fake_tmp
    rake(verbose: false).invoke_task('org:install')
    expect(File.exist?('var/lib/org-config.el')).to be(true)
    expect(File.exist?('.dir-locals.el')).to be(true)
    expect(File.exist?("#{org_dir}/lisp/org-loaddefs.el")).to be(true)
  end

  it 'installs Org in verbose mode', rake: true do
    copy_org_tarball_to_fake_tmp
    rake(verbose: true).invoke_task('org:install')
    expect(File.exist?('var/lib/org-config.el')).to be(true)
    expect(File.exist?('.dir-locals.el')).to be(true)
    expect(File.exist?("#{org_dir}/lisp/org-loaddefs.el")).to be(true)
  end

  it 'removes old Org version while installing', rake: true do
    FileUtils.mkdir_p 'lib/org-2.3'
    expect(Dir.exist?('lib/org-2.3')).to be(true)
    copy_org_tarball_to_fake_tmp
    rake(verbose: true).invoke_task('org:install')
    rake(verbose: false).invoke_task('org:install') # To check coverage
    expect(Dir.exist?('lib/org-2.3')).to be(false)
  end
end
