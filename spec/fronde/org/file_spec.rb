# frozen_string_literal: true

require_relative '../../../lib/fronde/org/file'

describe Fronde::Org::File do
  before { Fronde::CONFIG.reset }

  context 'with working org files' do
    it 'parses without date' do
      o = described_class.new('data/test1.org')
      expect(o.title).to eq('My sweet article')
      expect(o.date).to be_an_instance_of(NilTime)
      expect(o.date.strftime('%Y-%m-%d')).to eq('')
      expect(o.timekey).to eq('00000000000000')
      expect(o.format('%i - (%t)')).to eq(' - (My sweet article)')
    end

    it 'parses with a partial date' do
      o = described_class.new('data/test2.org')
      expect(o.title).to eq('My second article')
      expect(o.date).to eq(Time.strptime('2019-06-11 00:00:00', '%Y-%m-%d %H:%M:%S'))
      expect(o.date.strftime('%Y-%m-%d')).to eq('2019-06-11')
      expect(o.timekey).to eq('20190611000000')
      expect(o.format('%i - (%t)')).to eq('2019-06-11 - (My second article)')
      expect(o.lang).to eq('es')
    end

    it 'parses with a complete date' do
      o = described_class.new('data/test3.org')
      expect(o.title).to eq('My third article')
      expect(o.date).to eq(Time.strptime('2019-06-11 23:42:10', '%Y-%m-%d %H:%M:%S'))
      expect(o.timekey).to eq('20190611234210')
      expect(o.format('%i - (%t)')).to eq('2019-06-11 - (My third article)')
    end

    it 'parses with a complete date, but partial time' do
      o = described_class.new('data/test4.org')
      expect(o.title).to eq('Fourth test')
      expect(o.date).to eq(Time.strptime('2019-07-25 20:45:00', '%Y-%m-%d %H:%M:%S'))
      expect(o.timekey).to eq('20190725204500')
      expect(o.format('%i - (%t)')).to eq('2019-07-25 - (Fourth test)')
    end

    it 'finds a subtitle' do
      o = described_class.new('data/content.org')
      expect(o.title).to eq('My first blog post?')
      expect(o.subtitle).to eq('What to do with that')
    end

    it 'can be cast to a Hash' do
      o = described_class.new('data/content.org')
      data = o.to_h
      expect(data['author']).to eq 'alice'
      expect(data['title']).to eq 'My first blog post?'
      expect(data['excerpt']).to(
        eq('An article stating whether this is my first blog post or not')
      )
      expect(data['keywords']).to eq %w[test tata]
      expect(data['timekey']).to eq '20190613000000'
      expect(data['published']).to eq 'Thursday 13th of June'
      expected_tz = Time.new(2019, 6, 13).strftime('%:z')
      expect(data['published_xml']).to eq "2019-06-13T00:00:00#{expected_tz}"
      # Not published
      expect(data['url']).to be_nil
      expect(data['published_body']).to(
        eq('An article stating whether this is my first blog post or not')
      )
      expect(data['updated_xml']).to be_nil
    end

    it 'accepts dynamic setters' do
      o = described_class.new('data/content.org')
      expect(o.title).to eq('My first blog post?')
      o.title = 'Actually not my first one'
      expect(o.title).to eq('Actually not my first one')
    end

    it 'raises NameError if it cannot resolves a method' do
      o = described_class.new('data/content.org')
      expect { o.not_a_key }.to raise_error(NameError)
      expect { o.not_a_key = 'test' }.to raise_error(NameError)
    end

    it 'answers correctly to respond_to_missing?' do
      o = described_class.new('data/content.org')
      expect(o.respond_to?(:title)).to be true
      expect(o.respond_to?(:title=)).to be true
      expect(o.respond_to?(:not_a_key)).to be false
      expect(o.respond_to?(:not_a_key=)).to be false
    end
  end

  context 'without a working file' do
    before do
      FileUtils.mkdir_p 'tmp/org_test'
      Dir.chdir 'tmp/org_test'
    end

    after do
      tear_down 'tmp/org_test'
    end

    it 'raises if file_name is nil and try to write' do
      expect { described_class.new(nil).write }.to raise_error(RuntimeError)
      expect { described_class.new('').write }.to raise_error(RuntimeError)
    end

    it 'does not raise without file_name, but with a title' do
      expect { described_class.new(nil, title: 'Test 1').write }.not_to(
        raise_error
      )
      expect(File.exist?('test-1.org')).to be true
      expect { described_class.new('', title: 'Test 2').write }.not_to(
        raise_error
      )
      expect(File.exist?('test-2.org')).to be true
    end

    it 'returns a new org file structure' do
      now_str = Time.now.strftime('%Y-%m-%d %H:%M')
      o = described_class.new('__test__.org', title: 'test')
      expect(o.title).to eq('test')
      o_date_str = o.date.strftime('%Y-%m-%d %H:%M')
      expect(o_date_str).to eq(now_str)
      date = o.date.strftime('%Y-%m-%d %a. %H:%M:%S')
      expect(o.author).not_to be_nil
      expect(o.author).to be_an_instance_of(String)
      o.write
      empty_content = <<~CONTENT
        #+title: test
        #+date: <#{date}>
        #+author: #{o.author}
        #+language: en


      CONTENT
      expect(File.exist?('__test__.org')).to be(true)
      expect(File.read('__test__.org')).to eq(empty_content)
    end

    it 'creates new Org file, even in a new folder' do
      o = described_class.new(
        'not/existing/test.org',
        title: 'My Test', content: 'Lorem ipsum'
      )
      o.write
      expect(File.exist?('not/existing/test.org')).to be(true)
      date = o.date.strftime('%Y-%m-%d %a. %H:%M:%S')
      content = <<~CONTENT
        #+title: My Test
        #+date: <#{date}>
        #+author: #{o.author}
        #+language: en

        Lorem ipsum
      CONTENT
      expect(File.read('not/existing/test.org')).to eq(content)
    end

    it 'uses file name as title when title is empty' do
      FileUtils.mkdir 'src'
      File.write 'src/no_title.org', 'Lorem ipsum.'
      o = described_class.new('src/no_title.org')
      expect(o.title).to eq('src/no_title.org')
    end
  end

  context 'with configuration' do
    it 'respects author name' do
      Fronde::CONFIG.load_test('author' => 'Test')
      o = described_class.new('data/test1.org')
      expect(o.author).to eq('Test')
      o = described_class.new('data/test2.org')
      expect(o.author).to eq('Titi')
      o = described_class.new('data/test3.org')
      expect(o.author).to eq('Test')
    end

    it 'computes the right pub_file path for existing sources' do
      o = described_class.new('data/test1.org')
      expect(o.pub_file).to be_nil # No source match

      config = { 'sources' => [{ 'path' => 'data' }] }
      Fronde::CONFIG.load_test config
      o = described_class.new('data/test1.org')
      expect(o.pub_file).to eq('/data/test1.html')
      o = described_class.new('data/content.org')
      expect(o.pub_file).to eq('/data/content.html')

      config['domain'] = 'http://perdu.com'
      Fronde::CONFIG.load_test config
      o = described_class.new('data/test1.org')
      expect(o.pub_file).to eq('/data/test1.html')
      o = described_class.new('data/content.org')
      expect(o.pub_file).to eq('/data/content.html')
    end

    it 'computes the right url for existing sources' do
      o = described_class.new('data/test1.org')
      expect(o.url).to be_nil # No source match

      config = { 'sources' => [{ 'path' => 'data' }] }
      Fronde::CONFIG.load_test config
      o = described_class.new('data/test1.org')
      expect(o.url).to eq('/data/test1.html')
      o = described_class.new('data/content.org')
      expect(o.url).to eq('/data/content.html')

      config['domain'] = 'http://perdu.com'
      Fronde::CONFIG.load_test config
      o = described_class.new('data/test1.org')
      expect(o.url).to eq('http://perdu.com/data/test1.html')
      o = described_class.new('data/content.org')
      expect(o.url).to eq('http://perdu.com/data/content.html')
    end
  end

  context 'with an html file as file name' do
    before do
      FileUtils.mkdir_p 'tmp/test_target/src/blog/toto'
      FileUtils.mkdir_p 'tmp/test_target/writings'
      Dir.chdir 'tmp/test_target'
      File.write 'src/test.org', 'Lorem ipsum.'
      FileUtils.touch(
        ['src/blog/test.org', 'src/blog/toto/tata.org',
         'src/blog/toto/content.org', 'writings/notes.org']
      )
      Fronde::CONFIG.load_test(
        'sources' => [{ 'path' => 'src', 'target' => '.' }, 'writings']
      )
    end

    after do
      tear_down 'tmp/test_target'
    end

    it 'computes the right source path' do
      o = described_class.new('public_html/test.html')
      expect(o.file).to eq(File.expand_path('src/test.org'))
      o = described_class.new('public_html/blog/test.html')
      expect(o.file).to eq(File.expand_path('src/blog/test.org'))
      o = described_class.new(
        'public_html/blog/toto/tata.html'
      )
      expect(o.file).to eq(File.expand_path('src/blog/toto/tata.org'))
      o = described_class.new(
        'public_html/blog/toto/content.html'
      )
      expect(o.file).to eq(File.expand_path('src/blog/toto/content.org'))
    end

    it 'identifies a source for a given published file' do
      o = described_class.new('public_html/writings/notes.html')
      expect(o.file).to eq(File.expand_path('writings/notes.org'))
      expect(o.project).not_to be_nil
      o = described_class.new('public_html/test.html')
      expect(o.file).to eq(File.expand_path('src/test.org'))
      expect(o.project).not_to be_nil
      o = described_class.new('public_html/not/known.html')
      expect(o.file).to eq(File.expand_path('public_html/not/known.html'))
      expect(o.project).to be_nil
    end
  end
end
