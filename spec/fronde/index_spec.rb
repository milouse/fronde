# frozen_string_literal: true

require_relative '../../lib/fronde/index'

SAMPLE_ALL_INDEX = <<~INDEX
  #+title: All tags
  #+author: Test
  #+language: en

  * By alphabetical order
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-tags
  :UNNUMBERED: notoc
  :END:

  - [[http://perdu.com/tags/toto.html][toto]] (1)
  - [[http://perdu.com/tags/tutu.html][tutu]] (2)

  * By publication number
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-tags
  :UNNUMBERED: notoc
  :END:

  - [[http://perdu.com/tags/tutu.html][tutu]] (2)
  - [[http://perdu.com/tags/toto.html][toto]] (1)
INDEX

SAMPLE_ALL_INDEX_GEMINI = <<~INDEX
  #+title: All tags
  #+author: Test
  #+language: en

  * By alphabetical order
  :PROPERTIES:
  :UNNUMBERED: notoc
  :END:

  [[http://perdu.com/tags/toto.gmi][toto (1)]]
  [[http://perdu.com/tags/tutu.gmi][tutu (2)]]

  * By publication number
  :PROPERTIES:
  :UNNUMBERED: notoc
  :END:

  [[http://perdu.com/tags/tutu.gmi][tutu (2)]]
  [[http://perdu.com/tags/toto.gmi][toto (1)]]
INDEX

SAMPLE_PROJECT_INDEX = <<~BLOG_INDEX
  #+title: Blog
  #+author: Test
  #+language: en

  * 2019
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-year
  :UNNUMBERED: notoc
  :END:

  - *[[http://perdu.com/other/test3.html][My third article]]* / Published on Tuesday 11th of June at 23:42
  - *[[http://perdu.com/test2.html][My second article]]* / Published on Tuesday 11th of June \\\\
    Lorem ipsum

  * Unsorted
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-year
  :UNNUMBERED: notoc
  :END:

  - *[[http://perdu.com/test1.html][My sweet article]]*
BLOG_INDEX

SAMPLE_PROJECT_INDEX_GEMINI = <<~BLOG_INDEX
  #+title: Blog
  #+author: Test
  #+language: en

  * 2019
  :PROPERTIES:
  :UNNUMBERED: notoc
  :END:

  [[http://perdu.com/other/test3.gmi][2019-06-11 My third article]]
  [[http://perdu.com/test2.gmi][2019-06-11 My second article]]

  * Unsorted
  :PROPERTIES:
  :UNNUMBERED: notoc
  :END:

  [[http://perdu.com/test1.gmi][My sweet article]]
BLOG_INDEX

SAMPLE_EMPTY_INDEX = <<~INDEX
  #+title: All tags
  #+author: %<author>s
  #+language: en

  * By alphabetical order
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-tags
  :UNNUMBERED: notoc
  :END:


  * By publication number
  :PROPERTIES:
  :HTML_CONTAINER_CLASS: index-tags
  :UNNUMBERED: notoc
  :END:

INDEX

SAMPLE_EMPTY_PROJECT_INDEX = <<~INDEX
  #+title: Blog
  #+author: Test
  #+language: en
INDEX

SAMPLE_ATOM = <<~ATOM
  <?xml version="1.0" encoding="utf-8"?>
  <feed xmlns="http://www.w3.org/2005/Atom"
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xml:lang="en">

    <title>Blog</title>
    <link href="http://perdu.com/feeds/index.xml" rel="self" type="application/atom+xml"/>
    <link href="http://perdu.com" rel="alternate" type="text/html" title="Blog"/>
    <updated>%<date>s</updated>
    <author><name>Test</name></author>
    <id>urn:md5:75d53866bcb20465b3287cf237234464</id>
    <generator uri="https://git.umaneti.net/fronde/about/">Fronde</generator>

    <entry>
      <title>My third article</title>
      <link href="http://perdu.com/other/test3.html" rel="alternate"
            type="text/html"
            title="My third article"/>
      <id>urn:md5:8865383febd94ddf9df318267af5ae85</id>
      <published>2019-06-11T23:42:10%<expected_tz>s</published>
      <updated>%<mtime>s</updated>
      <author><name>Test</name></author>
      <dc:subject>toto</dc:subject>
      <dc:subject>tutu</dc:subject>
      <content type="html"></content>
    </entry>

    <entry>
      <title>My second article</title>
      <link href="http://perdu.com/test2.html" rel="alternate"
            type="text/html"
            title="My second article"/>
      <id>urn:md5:123104bd8bb4c61e02a1e2a136e2fd6b</id>
      <published>2019-06-11T00:00:00%<expected_tz>s</published>
      <updated>%<mtime>s</updated>
      <author><name>Titi</name></author>
      <content type="html">Lorem ipsum</content>
    </entry>

    <entry>
      <title>My sweet article</title>
      <link href="http://perdu.com/test1.html" rel="alternate"
            type="text/html"
            title="My sweet article"/>
      <id>urn:md5:c47532bbb1e2883c902071591ae1ec9b</id>
      <published></published>
      <updated>%<mtime>s</updated>
      <author><name>Test</name></author>
      <dc:subject>tutu</dc:subject>
      <content type="html"></content>
    </entry>
  </feed>
ATOM

SAMPLE_EMPTY_ATOM = <<~ATOM
  <?xml version="1.0" encoding="utf-8"?>
  <feed xmlns="http://www.w3.org/2005/Atom"
        xmlns:dc="http://purl.org/dc/elements/1.1/"
        xml:lang="en">

    <title>%<title>s</title>
    <link href="http://perdu.com/feeds/index.xml" rel="self" type="application/atom+xml"/>
    <link href="http://perdu.com" rel="alternate" type="text/html" title="%<title>s"/>
    <updated>%<date>s</updated>
    <author><name>%<author>s</name></author>
    <id>urn:md5:75d53866bcb20465b3287cf237234464</id>
    <generator uri="https://git.umaneti.net/fronde/about/">Fronde</generator>
  </feed>
ATOM

describe Fronde::Index do
  let(:index) { described_class.new Fronde::CONFIG.sources.first }
  let(:tags_path) { "#{Fronde::CONFIG.sources.first['path']}/tags" }
  let(:feeds_path) { "#{Fronde::CONFIG.sources.first.publication_path}/feeds" }

  context 'with blog settings and working org files' do
    before do
      FileUtils.mkdir_p 'tmp/blog/writings/other'
      FileUtils.mkdir_p 'tmp/blog/output'
      FileUtils.cp(
        ['data/test1.org',
         'data/test2.org'],
        'tmp/blog/writings'
      )
      FileUtils.cp 'data/test3.org', 'tmp/blog/writings/other'
      Dir.chdir 'tmp/blog'
      Fronde::CONFIG.load_test(
        'author' => 'Test',
        'html_public_folder' => 'output',
        'domain' => 'http://perdu.com',
        'sources' => [
          { 'path' => 'writings',
            'title' => 'Blog',
            'target' => '.',
            'is_blog' => true }
        ]
      )
    end

    after do
      tear_down 'tmp/blog'
    end

    it 'has generated two indexes', :aggregate_failures do
      expect(index.all_tags.length).to eq(2)
      expect(index.empty?).to be(false)
    end

    it 'excludes specified pattern from indexes', :aggregate_failures do
      old_conf = Fronde::CONFIG.settings.merge
      old_conf['sources'][0]['exclude'] = 'test3\.org$'
      Fronde::CONFIG.load_test(old_conf)
      expect(index.all_tags.length).to eq(1)
      expect(index.all_tags.include?('toto')).to be(false)
    end

    it 'generates a main index', :aggregate_failures do
      expect(index.to_s).to eq(SAMPLE_ALL_INDEX)
      expect(index.blog_home_page).to eq(SAMPLE_PROJECT_INDEX)
    end

    it 'generates a main index for gemini sources', :aggregate_failures do
      old_conf = Fronde::CONFIG.settings
      old_conf['sources'][0]['type'] = 'gemini'
      Fronde::CONFIG.load_test old_conf
      expect(index.to_s).to eq(SAMPLE_ALL_INDEX_GEMINI)
      expect(index.blog_home_page).to eq(SAMPLE_PROJECT_INDEX_GEMINI)
    end

    it 'displays the right date for the index' do
      now_str = Time.now.strftime('%Y-%m-%d %H:%M')
      index_date_str = index.date.strftime('%Y-%m-%d %H:%M')
      expect(index_date_str).to eq(now_str)
    end

    it 'generates an atom feed' do
      expected_tz = Time.new(2019, 6, 13).strftime('%:z')
      comp = format(
        SAMPLE_ATOM,
        date: index.date.xmlschema,
        mtime: File.mtime('writings/test1.org').xmlschema,
        expected_tz: expected_tz
      )
      expect(index.to_atom).to eq(comp)
    end

    it 'lists tags by name' do
      expect(index.sort_by(:name)).to eql(['tutu (2)', 'toto (1)'])
    end

    it 'lists tags by weight' do
      expect(index.sort_by(:weight)).to eql(['toto (1)', 'tutu (2)'])
    end

    it 'raises an error if sort_by is called with bad argument' do
      expect { index.sort_by(:test) }.to raise_error(ArgumentError)
    end

    it 'correctly saves one index', :aggregate_failures do
      FileUtils.mkdir_p tags_path
      index.write_org('index')
      expect(File.exist?("#{tags_path}/index.org")).to be(true)
      expect(File.read("#{tags_path}/index.org")).to eq(SAMPLE_ALL_INDEX)
    end

    it 'correctly saves one blog index', :aggregate_failures do
      index.send(:write_blog_home_page, false)
      expect(File.exist?('writings/index.org')).to be(true)
      expect(File.read('writings/index.org')).to eq(SAMPLE_PROJECT_INDEX)
    end

    it 'correctly saves one blog index, even verbosely', :aggregate_failures do
      index.send(:write_blog_home_page, true)
      expect(File.exist?('writings/index.org')).to be(true)
      expect(File.read('writings/index.org')).to eq(SAMPLE_PROJECT_INDEX)
    end

    it 'correctly saves one atom feed', :aggregate_failures do
      FileUtils.mkdir_p feeds_path
      index.write_atom('index')
      expect(File.exist?("#{feeds_path}/index.xml")).to be(true)
      expected_tz = Time.new(2019, 6, 13).strftime('%:z')
      comp = format(
        SAMPLE_ATOM,
        date: index.date.xmlschema,
        mtime: File.mtime('writings/test1.org').xmlschema,
        expected_tz: expected_tz
      )
      expect(File.read("#{feeds_path}/index.xml")).to eq(comp)
    end

    it 'writes them all', :aggregate_failures do
      index.write_all_org
      expect(File.exist?('writings/index.org')).to be(true)
      expect(File.exist?("#{tags_path}/index.org")).to be(true)
      expect(File.exist?("#{tags_path}/toto.org")).to be(true)
      expect(File.exist?("#{tags_path}/tutu.org")).to be(true)

      index.write_all_feeds
      expect(File.exist?("#{feeds_path}/index.xml")).to be(true)
      expect(File.exist?("#{feeds_path}/toto.xml")).to be(true)
      expect(File.exist?("#{feeds_path}/tutu.xml")).to be(true)
    end

    it 'does not attempt to include tags into feeds', :aggregate_failures do
      # Run it once to ensure index is instanciated a first time and
      # index files are generated
      index.write_all_org
      entries = index.instance_variable_get(:@entries)
      expect(entries.length).to eq(3)
      # Run a second time to check that it does not parse previously
      # generated tags
      second_index = described_class.new Fronde::CONFIG.sources.first
      entries = second_index.instance_variable_get(:@entries)
      expect(entries.length).to eq(3)
    end

    context 'without recursive config' do
      before do
        # This config will generate the same result as if it was
        # recursive: true as is_blog necessarily requires the project
        # to be recursive
        old_conf = Fronde::CONFIG.settings
        old_conf['sources'][0]['recursive'] = false
        Fronde::CONFIG.load_test old_conf
      end

      it 'has generated two index', :aggregate_failures do
        expect(index.all_tags.length).to eq(2)
        expect(index.empty?).to be(false)
      end

      it 'generates a main index', :aggregate_failures do
        expect(index.to_s).to eq(SAMPLE_ALL_INDEX)
        expect(index.blog_home_page).to eq(SAMPLE_PROJECT_INDEX)
      end
    end
  end

  context 'with blog settings and without org files' do
    before do
      FileUtils.mkdir_p 'tmp/blog/writings'
      FileUtils.mkdir_p 'tmp/blog/output'
      Dir.chdir 'tmp/blog'
      Fronde::CONFIG.load_test(
        'author' => 'Test',
        'html_public_folder' => 'output',
        'domain' => 'http://perdu.com',
        'sources' => [
          { 'path' => 'writings',
            'title' => 'Blog',
            'target' => '.',
            'is_blog' => true }
        ]
      )
    end

    after do
      tear_down 'tmp/blog'
    end

    it 'has generated no index', :aggregate_failures do
      expect(index.all_tags.length).to eq(0)
      expect(index.empty?).to be(true)
    end

    it 'generates a main index', :aggregate_failures do
      expect(index.to_s).to eq(format(SAMPLE_EMPTY_INDEX, author: 'Test'))
      expect(index.blog_home_page).to eq(SAMPLE_EMPTY_PROJECT_INDEX)
    end

    it 'generates an atom feed', :aggregate_failures do
      now_str = Time.now.strftime('%Y-%m-%d %H:%M')
      index_date_str = index.date.strftime('%Y-%m-%d %H:%M')
      expect(index_date_str).to eq(now_str)
      comp = format(
        SAMPLE_EMPTY_ATOM,
        date: index.date.xmlschema,
        author: 'Test', title: 'Blog'
      )
      expect(index.to_atom).to eq(comp)
    end

    it 'correctly saves one index', :aggregate_failures do
      FileUtils.mkdir_p tags_path
      index.write_org('index')
      expect(File.exist?("#{tags_path}/index.org")).to be(true)
      expect(File.read("#{tags_path}/index.org")).to(
        eq(format(SAMPLE_EMPTY_INDEX, author: 'Test'))
      )
    end

    it 'correctly saves one blog index', :aggregate_failures do
      index.send(:write_blog_home_page, false)
      expect(File.exist?('writings/index.org')).to be(true)
      expect(File.read('writings/index.org')).to(
        eq(SAMPLE_EMPTY_PROJECT_INDEX)
      )
    end

    it 'correctly saves one atom feed', :aggregate_failures do
      FileUtils.mkdir_p feeds_path
      index.write_atom('index')
      expect(File.exist?("#{feeds_path}/index.xml")).to be(true)
      comp = format(
        SAMPLE_EMPTY_ATOM,
        date: index.date.xmlschema,
        author: 'Test', title: 'Blog'
      )
      expect(File.read("#{feeds_path}/index.xml")).to eq(comp)
    end

    it 'writes them all', :aggregate_failures do
      index.write_all_org
      expect(File.exist?('writings/index.org')).to be(true)
      expect(File.exist?("#{tags_path}/index.org")).to be(true)

      index.write_all_feeds
      expect(File.exist?("#{feeds_path}/index.xml")).to be(true)
    end
  end

  context 'without blog settings' do
    before do
      FileUtils.mkdir_p 'tmp/txt/writings'
      FileUtils.mkdir_p 'tmp/txt/output'
      FileUtils.cp(
        ['data/test1.org',
         'data/test2.org',
         'data/test3.org'],
        'tmp/txt/writings'
      )
      Dir.chdir 'tmp/txt'
      Fronde::CONFIG.load_test(
        'author' => 'Test',
        'html_public_folder' => 'output',
        'domain' => 'http://perdu.com',
        'sources' => [
          { 'path' => 'writings',
            'target' => '.' }
        ]
      )
    end

    after do
      tear_down 'tmp/txt'
    end

    it 'does not have generated any indexes', :aggregate_failures do
      expect(index.all_tags.length).to eq(0)
      expect(index.empty?).to be(true)
    end

    it 'generates an empty main index' do
      expect(index.to_s).to(
        eq(format(SAMPLE_EMPTY_INDEX, author: 'Test'))
      )
    end

    it 'displays the right date for the empty index' do
      now_str = Time.now.strftime('%Y-%m-%d %H:%M')
      index_date_str = index.date.strftime('%Y-%m-%d %H:%M')
      expect(index_date_str).to eq(now_str)
    end

    it 'generates an empty atom feed' do
      comp = format(
        SAMPLE_EMPTY_ATOM,
        date: index.date.xmlschema,
        author: 'Test',
        title: 'writings'
      )
      expect(index.to_atom).to eq(comp)
    end
  end

  context 'without neither blog settings nor site title' do
    before do
      FileUtils.mkdir_p 'tmp/txt/writings'
      FileUtils.mkdir_p 'tmp/txt/output'
      FileUtils.cp 'data/test1.org', 'tmp/txt/writings'
      Dir.chdir 'tmp/txt'
      Fronde::CONFIG.load_test(
        'html_public_folder' => 'output',
        'domain' => 'http://perdu.com',
        'sources' => [{ 'path' => 'writings', 'target' => '.' }]
      )
    end

    after do
      tear_down 'tmp/txt'
    end

    it 'does not have generated any indexes', :aggregate_failures do
      expect(index.all_tags.length).to eq(0)
      expect(index.empty?).to be(true)
    end

    it 'generates an empty main index' do
      expect(index.to_s).to(
        eq(format(SAMPLE_EMPTY_INDEX, author: 'alice'))
      )
    end

    it 'displays the right date for the empty feed' do
      now_str = Time.now.strftime('%Y-%m-%d %H:%M')
      index_date_str = index.date.strftime('%Y-%m-%d %H:%M')
      expect(index_date_str).to eq(now_str)
    end

    it 'generates an empty atom feed' do
      comp = format(
        SAMPLE_EMPTY_ATOM,
        date: index.date.xmlschema,
        author: 'alice',
        title: 'writings'
      )
      expect(index.to_atom).to eq(comp)
    end
  end
end
