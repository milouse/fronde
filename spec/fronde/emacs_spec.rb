# frozen_string_literal: true

require_relative '../../lib/fronde/emacs'

describe Fronde::Emacs do
  it 'publishes default website' do
    emacs = described_class.new
    allow(emacs).to receive(:system) do |command|
      expect(command).to eq(
        'emacs -Q --batch -nw --eval \'(setq inhibit-message t)\' ' \
        '-l ./var/lib/org-config.el --eval \'(org-publish "website")\''
      )
    end
    emacs.publish
  end

  it 'publishes verbosely default' do
    emacs = described_class.new(verbose: true)
    allow(emacs).to receive(:system) do |command|
      expect(command).to eq(
        'emacs -Q --batch -nw -l ./var/lib/org-config.el ' \
        '--eval \'(org-publish "website")\''
      )
    end
    emacs.publish
  end

  it 'publishes a given website' do
    emacs = described_class.new
    allow(emacs).to receive(:system) do |command|
      expect(command).to eq(
        'emacs -Q --batch -nw --eval \'(setq inhibit-message t)\' ' \
        '-l ./var/lib/org-config.el --eval \'(org-publish "test_test")\''
      )
    end
    emacs.publish('test_test')
  end

  it 'forces the publication of a website' do
    emacs = described_class.new
    allow(emacs).to receive(:system) do |command|
      expect(command).to eq(
        'emacs -Q --batch -nw --eval \'(setq inhibit-message t)\' ' \
        '-l ./var/lib/org-config.el --eval \'(org-publish "website" t)\''
      )
    end
    emacs.publish(force: 'yes')
  end

  it 'publishes a single file' do
    emacs = described_class.new
    allow(emacs).to receive(:system) do |command|
      expect(command).to eq(
        'emacs -Q --batch -nw --eval \'(setq inhibit-message t)\' ' \
        '-l ./var/lib/org-config.el --visit "src/test.org" ' \
        '--eval \'(org-publish-current-file)\''
      )
    end
    emacs.publish_file('src/test.org')
  end

  it 'forces the publication of a single file' do
    emacs = described_class.new
    allow(emacs).to receive(:system) do |command|
      expect(command).to eq(
        'emacs -Q --batch -nw --eval \'(setq inhibit-message t)\' ' \
        '-l ./var/lib/org-config.el --visit "src/test.org" ' \
        '--eval \'(org-publish-current-file t)\''
      )
    end
    emacs.publish_file('src/test.org', force: 'please')
  end

  it 'forces the publication of a single file verbosely' do
    emacs = described_class.new(verbose: true)
    allow(emacs).to receive(:system) do |command|
      expect(command).to eq(
        'emacs -Q --batch -nw -l ./var/lib/org-config.el ' \
        '--visit "src/test.org" --eval \'(org-publish-current-file t)\''
      )
    end
    emacs.publish_file('src/test.org', force: 'please')
  end
end
