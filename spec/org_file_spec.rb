# frozen_string_literal: true

# rubocop:disable Style/FormatStringToken
describe 'With working org files' do
  it 'should parse without date', core: true do
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.title).to eq('My sweet article')
    expect(o.date).to be_nil
    expect(o.timekey).to eq('00000000000000')
    expect(o.format('%i - (%t)')).to eq(' - (My sweet article)')
  end

  it 'should parse with a partial date', core: true do
    o = Neruda::OrgFile.new('spec/data/test2.org')
    expect(o.title).to eq('My second article')
    expect(o.date).to eq(DateTime.strptime('2019-06-11 00:00:00', '%Y-%m-%d %H:%M:%S'))
    expect(o.timekey).to eq('20190611000000')
    expect(o.datestring(:short)).to eq('2019-06-11')
    expect(o.format('%i - (%t)')).to eq('2019-06-11 - (My second article)')
    expect(o.lang).to eq('es')
  end

  it 'should parse with a complete date', core: true do
    o = Neruda::OrgFile.new('spec/data/test3.org')
    expect(o.title).to eq('My third article')
    expect(o.date).to eq(DateTime.strptime('2019-06-11 23:42:10', '%Y-%m-%d %H:%M:%S'))
    expect(o.timekey).to eq('20190611234210')
    expect(o.datestring(:short)).to eq('2019-06-11')
    expect(o.format('%i - (%t)')).to eq('2019-06-11 - (My third article)')
  end

  it 'should parse with a complete date, but partial time', core: true do
    o = Neruda::OrgFile.new('spec/data/test4.org')
    expect(o.title).to eq('Fourth test')
    expect(o.date).to eq(DateTime.strptime('2019-07-25 20:45:00', '%Y-%m-%d %H:%M:%S'))
    expect(o.timekey).to eq('20190725204500')
    expect(o.datestring(:short)).to eq('2019-07-25')
    expect(o.format('%i - (%t)')).to eq('2019-07-25 - (Fourth test)')
  end

  it 'should find a subtitle', core: true do
    o = Neruda::OrgFile.new('spec/data/content.org')
    expect(o.title).to eq('My first blog post?')
    expect(o.subtitle).to eq('What to do with that')
  end
end
# rubocop:enable Style/FormatStringToken

describe 'With various titles' do
  it 'should transliterate them into slugs', core: true do
    expect(Neruda::OrgFile.slug('toto')).to eq('toto')
    expect(Neruda::OrgFile.slug('TotO')).to eq('toto')
    expect(Neruda::OrgFile.slug('Tôto')).to eq('toto')
    expect(Neruda::OrgFile.slug('Tôto tata')).to eq('toto-tata')
    expect(Neruda::OrgFile.slug('ÀùéïỸç/+*= truñlu°`')).to eq('aueiyc-trunlu')
  end
end

describe 'Without a working file' do
  before(:each) do
    FileUtils.mkdir_p 'tmp/org_test'
    Dir.chdir 'tmp/org_test'
  end

  after(:each) do
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/org_test', force: true
  end

  it 'should raise if file_name is nil and try to write', core: true do
    expect { Neruda::OrgFile.new(nil).write }.to raise_error(TypeError)
    expect { Neruda::OrgFile.new('').write }.to raise_error(TypeError)
  end

  it 'should return a new org file structure', core: true do
    now = DateTime.now
    o = Neruda::OrgFile.new('__test__.org', title: 'test')
    expect(o.title).to eq('test')
    o_date_str = o.date.strftime('%Y-%m-%d %H:%M')
    expect(o_date_str).to eq(now.strftime('%Y-%m-%d %H:%M'))
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
    expect(IO.read('__test__.org')).to eq(empty_content)
  end
end

describe 'With configuration' do
  after(:each) do
    # Reset config
    Neruda::Config.load_test({})
  end

  it 'should respect author name', core: true do
    Neruda::Config.load_test('author' => 'Test')
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.author).to eq('Test')
    o = Neruda::OrgFile.new('spec/data/test2.org')
    expect(o.author).to eq('Titi')
    o = Neruda::OrgFile.new('spec/data/test3.org')
    expect(o.author).to eq('Test')
  end

  it 'should compute the right html_file path for existing sources', core: true do
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.html_file).to eq('data/test1.html')
    Neruda::Config.load_test('domain' => 'http://perdu.com')
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.html_file).to eq('data/test1.html')
    o = Neruda::OrgFile.new('spec/data/content.org')
    expect(o.html_file).to eq('data/content.html')
    # The following are weird tests. We begin to test theoric stuff here
    Neruda::Config.load_test(
      'domain' => 'http://perdu.com',
      'sources' => [
        { 'path' => 'data' }
      ]
    )
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.html_file).to eq('data/test1.html')
    o = Neruda::OrgFile.new('spec/data/content.org')
    expect(o.html_file).to eq('data/content.html')
  end

  it 'should compute the right url for existing sources', core: true do
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.url).to eq('/data/test1.html')
    Neruda::Config.load_test('domain' => 'http://perdu.com')
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.url).to eq('http://perdu.com/data/test1.html')
    o = Neruda::OrgFile.new('spec/data/content.org')
    expect(o.url).to eq('http://perdu.com/data/content.html')
    # The following are weird tests. We begin to test theoric stuff here
    Neruda::Config.load_test(
      'domain' => 'http://perdu.com',
      'sources' => [
        { 'path' => 'data' }
      ]
    )
    o = Neruda::OrgFile.new('spec/data/test1.org')
    expect(o.url).to eq('http://perdu.com/data/test1.html')
    o = Neruda::OrgFile.new('spec/data/content.org')
    expect(o.url).to eq('http://perdu.com/data/content.html')
  end

  it 'should compute the right html_file path for theoritical sources', core: true do
    expect(Neruda::OrgFile.target_for_source(nil, nil)).to be(nil)
    target = Neruda::OrgFile.target_for_source('src/test.org', nil)
    expect(target).to eq('public_html/src/test.html')
    target = Neruda::OrgFile.target_for_source('not/known/at/all.org', nil)
    expect(target).to eq('public_html/at/all.html')
    target = Neruda::OrgFile.target_for_source(
      'not/known/at/all.org', nil, with_public_folder: false
    )
    expect(target).to eq('at/all.html')
    project = { 'path' => 'src', 'target' => '.' }
    target = Neruda::OrgFile.target_for_source('src/test.org', project)
    expect(target).to eq('public_html/test.html')
    target = Neruda::OrgFile.target_for_source(
      'src/test.org', project, with_public_folder: false
    )
    expect(target).to eq('test.html')
    target = Neruda::OrgFile.target_for_source(
      'src/blog/test.org', project
    )
    expect(target).to eq('public_html/blog/test.html')
    target = Neruda::OrgFile.target_for_source(
      'src/blog/toto/tata.org', project
    )
    expect(target).to eq('public_html/blog/toto/tata.html')
    target = Neruda::OrgFile.target_for_source(
      'src/blog/toto/content.org', project
    )
    expect(target).to eq('public_html/blog/toto/content.html')
    project = { 'path' => '~/tata', 'target' => '.' }
    target = Neruda::OrgFile.target_for_source(
      '~/tata/tutu/content.org', project
    )
    expect(target).to eq('public_html/tutu/content.html')
    target = Neruda::OrgFile.target_for_source(
      '~/tata/blog/content.org', project
    )
    expect(target).to eq('public_html/blog/content.html')
  end
end

describe 'With a fake sources structure' do
  before(:each) do
    FileUtils.mkdir_p 'tmp/test_target/src/blog/toto'
    FileUtils.mkdir_p 'tmp/test_target/writings'
    Dir.chdir 'tmp/test_target'
    IO.write 'src/test.org', 'Lorem ipsum.'
    FileUtils.touch(
      ['src/blog/test.org', 'src/blog/toto/tata.org',
       'src/blog/toto/content.org', 'writings/notes.org']
    )
    Neruda::Config.load_test(
      'sources' => [{ 'path' => 'src', 'target' => '.' }, 'writings']
    )
  end

  after(:each) do
    Dir.chdir File.expand_path('..', __dir__)
    FileUtils.rm_r 'tmp/test_target', force: true
  end

  it 'should use file name as title when title is empty' do
    o = Neruda::OrgFile.new('src/test.org')
    expect(o.title).to eq('src/test.org')
  end

  it 'should compute the right source path', core: true do
    lp = Neruda::OrgFile.source_for_target('public_html/test.html')
    expect(lp).to eq(File.expand_path('src/test.org'))
    lp = Neruda::OrgFile.source_for_target('public_html/blog/test.html')
    expect(lp).to eq(File.expand_path('src/blog/test.org'))
    lp = Neruda::OrgFile.source_for_target('public_html/blog/toto/tata.html')
    expect(lp).to eq(File.expand_path('src/blog/toto/tata.org'))
    lp = Neruda::OrgFile.source_for_target(
      'public_html/blog/toto/content.html'
    )
    expect(lp).to eq(File.expand_path('src/blog/toto/content.org'))
  end

  it 'should identify a project for a given file path' do
    ps = Neruda::OrgFile.project_for_source('src/test.org')
    expect(ps).to eq(Neruda::Config.sources[0])
    ps = Neruda::OrgFile.project_for_source('not/known.org')
    expect(ps).to be(nil)
  end

  it 'should identify a source for a given published file' do
    s = Neruda::OrgFile.source_for_target('public_html/writings/notes.html')
    expect(s).to eq(File.expand_path('writings/notes.org'))
    s = Neruda::OrgFile.source_for_target('public_html/test.html')
    expect(s).to eq(File.expand_path('src/test.org'))
    s = Neruda::OrgFile.source_for_target('public_html/not/known.html')
    expect(s).to be(nil)
  end
end
