# frozen_string_literal: true

require 'rake'

context 'when managing Org installation' do
  let(:org_dir) { "lib/org-#{Fronde::Org.last_version}" }

  before(:all) { init_fake_org_install } # rubocop:disable RSpec/BeforeAfterAll

  before do
    init_testing_environment
    Fronde::CONFIG.reset
  end

  after do
    Dir.glob('lib/org-[0-9.]*').each { |ov| FileUtils.rm_r(ov, force: true) }
    FileUtils.rm ['.dir-locals.el', 'var/lib/org-config.el'], force: true
    tear_down 'tmp/website_testing'
  end

  it 'compiles org-config.el' do
    rake.invoke_task('var/lib/org-config.el')
    expect(File.exist?('var/lib/org-config.el')).to be true
    proof = proof_content 'org-config-proof.el'
    expect(File.read('var/lib/org-config.el')).to eq(proof)
  end

  it 'creates .dir-locals.el' do
    rake.invoke_task('.dir-locals.el')
    expect(File.exist?('.dir-locals.el')).to be true
    proof = File.expand_path('var/lib/org-config.el')
    expect(File.read('.dir-locals.el')).to(
      eq("((org-mode . ((eval . (load-file \"#{proof}\")))))")
    )
  end

  it 'tries to download Org verbosely' do
    allow(Fronde::Org).to receive(:download).and_return('test')
    expect do
      rake(verbose: true).invoke_task('var/tmp/org.tar.gz')
    end.to output("Org version test has been downloaded.\n").to_stderr
  end

  it 'does not download Org if a copy is already cached' do
    copy_org_tarball_to_fake_tmp
    expect(File.exist?('var/tmp/org.tar.gz')).to be true
    expect do
      rake(verbose: true).invoke_task('var/tmp/org.tar.gz')
    end.to output('').to_stderr
  end

  it 'warns user if no last version can be found' do
    allow(Fronde::Org).to receive(:fetch_version_number).and_return(nil)
    expect { Fronde::Org.download }.to raise_error RuntimeError
    expect { Fronde::Org.download }.to(
      raise_error('No remote Org version found')
    )
    expect { rake(verbose: true).invoke_task('org:install') }.to(
      output(
        /Impossible to download Org now\. Please try again later\./
      ).to_stderr
    )
    expect { rake(verbose: false).invoke_task('org:install') }.to(
      output(/An error occurred\./).to_stderr
    )
  end

  it 'installs Org' do
    copy_org_tarball_to_fake_tmp
    rake(verbose: false).invoke_task('org:install')
    expect(File.exist?('var/lib/org-config.el')).to be true
    expect(File.exist?('.dir-locals.el')).to be true
    expect(File.exist?("#{org_dir}/lisp/org-loaddefs.el")).to be true
  end

  it 'installs Org in verbose mode' do
    copy_org_tarball_to_fake_tmp
    rake(verbose: true).invoke_task('org:install')
    expect(File.exist?('var/lib/org-config.el')).to be true
    expect(File.exist?('.dir-locals.el')).to be true
    expect(File.exist?("#{org_dir}/lisp/org-loaddefs.el")).to be true
  end

  it 'compiles Org successfully' do
    copy_org_tarball_to_fake_tmp
    expect { rake(verbose: true).invoke_task('org:compile') }.to(
      output(/Org version [0-9.]+ has been locally installed./).to_stderr
    )
  end

  it 'does not compile org twice if already there' do
    copy_org_tarball_to_fake_tmp
    copy_org_lisp_files_to_fake_tmp
    # Second call should not output the same sentence, as the
    # compilation will be skipped
    expect { rake(verbose: true).invoke_task('org:compile') }.not_to(
      output(/Org version [0-9.]+ has been locally installed./).to_stderr
    )
    allow(Fronde::Org).to receive(:compile)
    rake.invoke_task('org:compile')
    expect(Fronde::Org).not_to have_received(:compile)
  end

  it 'creates only a public_html folder with no gemini sources' do
    copy_org_tarball_to_fake_tmp
    # Speed up rake test
    allow(Fronde::Org).to receive(:last_version).and_return('test')
    FileUtils.mkdir_p 'lib/org-test/lisp'

    config = { 'sources' => [{ 'path' => 'data', 'type' => 'html' }] }
    Fronde::CONFIG.load_test config
    rake(verbose: false).invoke_task('org:install')
    expect(Dir.exist?('public_gmi')).to be false
    expect(Dir.exist?('public_html')).to be true
  end

  it 'creates only a public_gmi folder with no html sources' do
    copy_org_tarball_to_fake_tmp
    # Speed up rake test
    allow(Fronde::Org).to receive(:last_version).and_return('test')
    FileUtils.mkdir_p 'lib/org-test/lisp'

    config = { 'sources' => [{ 'path' => 'data', 'type' => 'gemini' }] }
    Fronde::CONFIG.load_test config
    rake(verbose: false).invoke_task('org:install')
    expect(Dir.exist?('public_gmi')).to be true
    expect(Dir.exist?('public_html')).to be false
  end

  it 'creates both public_gmi and public_html folders when needed' do
    copy_org_tarball_to_fake_tmp
    # Speed up rake test
    allow(Fronde::Org).to receive(:last_version).and_return('test')
    FileUtils.mkdir_p 'lib/org-test/lisp'

    config = {
      'sources' => [{ 'path' => 'src', 'type' => 'html' },
                    { 'path' => 'data', 'type' => 'gemini' }]
    }
    Fronde::CONFIG.load_test config
    rake(verbose: false).invoke_task('org:install')
    expect(Dir.exist?('public_gmi')).to be true
    expect(Dir.exist?('public_html')).to be true
  end

  it 'removes old Org version while installing' do
    FileUtils.mkdir_p 'lib/org-2.3'
    expect(Dir.exist?('lib/org-2.3')).to be true
    copy_org_tarball_to_fake_tmp
    allow(Fronde::Org).to receive(:compile)
    rake.invoke_task('org:install')
    expect(Dir.exist?('lib/org-2.3')).to be false
  end

  it 'does not touch org install files if no upgrade needed' do
    copy_org_tarball_to_fake_tmp
    copy_org_lisp_files_to_fake_tmp
    rake.invoke_task 'org:install'
    config_time = File.mtime 'var/lib/org-config.el'
    locals_time = File.mtime '.dir-locals.el'
    orgball_time = File.mtime 'var/tmp/org.tar.gz'
    org_version = Fronde::Org.current_version
    orglisp_time = File.mtime "lib/org-#{org_version}/lisp/org-loaddefs.el"
    allow(Fronde::Org).to receive(:download).and_return org_version
    allow(Fronde::Org).to receive(:compile)
    sleep 1 # To change mtimes
    rake.invoke_task 'org:upgrade'
    expect(File.mtime('var/lib/org-config.el')).not_to eq config_time
    expect(File.mtime('.dir-locals.el')).not_to eq locals_time
    expect(Fronde::Org).not_to have_received(:download)
    expect(File.mtime('var/tmp/org.tar.gz')).to eq orgball_time
    expect(Fronde::Org).not_to have_received(:compile)
    expect(File.mtime("lib/org-#{org_version}/lisp/org-loaddefs.el")).to(
      eq(orglisp_time)
    )
  end

  it 'changes org install files when an upgrade is needed' do
    copy_org_tarball_to_fake_tmp
    copy_org_lisp_files_to_fake_tmp
    rake.invoke_task 'org:install'
    config_time = File.mtime 'var/lib/org-config.el'
    locals_time = File.mtime '.dir-locals.el'
    org_version = Fronde::Org.current_version
    FileUtils.mv "lib/org-#{org_version}", 'lib/org-2.3'
    expect(Dir.exist?("lib/org-#{org_version}")).to be false
    allow(Fronde::Org).to receive(:download).and_return org_version
    allow(Fronde::Org).to receive(:compile)
    sleep 1 # To change mtimes
    rake.invoke_task('org:upgrade')
    expect(File.mtime('var/lib/org-config.el')).not_to eq config_time
    expect(File.mtime('.dir-locals.el')).not_to eq locals_time
    # Removed by upgrade
    expect(File.exist?('var/tmp/org.tar.gz')).to be false
    expect(Dir.exist?('lib/org-2.3')).to be false
    expect(Fronde::Org).to have_received(:download).once
    expect(Fronde::Org).to have_received(:compile).once
  end

  it 'accepts to upgrade even when no previous install exists' do
    allow(Fronde::Org).to receive(:download).and_return 'test'
    allow(Fronde::Org).to receive(:compile)
    rake.invoke_task('org:upgrade')
    expect(Fronde::Org).to have_received(:download).once
    expect(Fronde::Org).to have_received(:compile).once
  end

  it 'skips an upgrade when it fails to fetch org version' do
    copy_org_tarball_to_fake_tmp
    copy_org_lisp_files_to_fake_tmp
    allow(Fronde::Org).to(
      receive_messages(fetch_version_number: nil, download: 'test')
    )
    allow(Fronde::Org).to receive(:compile)
    expect { rake.invoke_task('org:upgrade') }.not_to raise_error
    expect(Fronde::Org).not_to have_received(:download)
    expect(Fronde::Org).not_to have_received(:compile)
  end
end
