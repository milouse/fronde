# frozen_string_literal: true

describe Fronde::Source do
  it 'can not be instantiated directly', :aggregate_failures do
    expect { described_class.new({}) }.to raise_error NameError
    expect { described_class.new({}) }.to \
      raise_error(/`fill_in_specific_config'/)
  end

  it 'stores new settings' do
    project = described_class.new_from_config(
      'path' => 'src', 'type' => 'gemini'
    )
    project['test'] = 'Lorem ipsum'
    expect(project['test']).to eq('Lorem ipsum')
  end

  it 'always says no for gemini blog' do
    project = described_class.new_from_config(
      'path' => 'src', 'type' => 'gemini', 'is_blog' => true
    )
    expect(project.blog?).to be(false)
  end

  it 'computes the right name' do
    test_cases = {
      'src' => 'src',
      'src/test' => 'src-test',
      './src' => 'src',
      '.' => '-',
      '~/test' => 'test',
      '~/test/deeper' => 'test-deeper',
      '~/.hidden_dir' => '-hidden_dir'
    }
    test_cases.each do |path, result|
      source = described_class.new_from_config(
        'path' => path, 'type' => 'html'
      )
      expect(source['name']).to eq(result)
    end
  end

  it 'computes the right title' do
    test_cases = [
      'src',
      'src/test',
      './src',
      '.',
      '~/test',
      '~/test/deeper',
      '~/.hidden_dir'
    ]
    test_cases.each do |path|
      source = described_class.new_from_config(
        'path' => path, 'type' => 'html'
      )
      expect(source['title']).to eq(path)
    end
  end

  it 'computes the right target' do
    test_cases = {
      'src' => 'src',
      'src/test' => 'test',
      './src' => 'src',
      '.' => '',
      '~/test' => 'test',
      '~/test/deeper' => 'deeper',
      '~/.hidden_dir' => 'hidden_dir'
    }
    test_cases.each do |path, result|
      source = described_class.new_from_config(
        'path' => path, 'type' => 'html'
      )
      expect(source['target']).to eq(result)
    end
  end

  it 'computes the right pub_file path for theoritical relative sources' do
    config = described_class.canonical_config(
      'path' => 'src', 'target' => '.'
    )
    project = described_class.new_from_config(config)
    {
      'src/test.org' => '/test.html',
      'src/blog/test.org' => '/blog/test.html',
      'src/blog/toto/tata.org' => '/blog/toto/tata.html',
      'src/blog/toto/content.org' => '/blog/toto/content.html'
    }.each do |target, value|
      expect(project.target_for(target)).to eq(value)
    end
  end

  it 'computes the right pub_file path for theoritical home-based sources' do
    config = described_class.canonical_config(
      'path' => '~/tata', 'target' => '.'
    )
    project = described_class.new_from_config(config)
    {
      '~/tata/tutu/content.org' => '/tutu/content.html',
      '~/tata/blog/content.org' => '/blog/content.html'
    }.each do |target, value|
      expect(project.target_for(target)).to eq(value)
    end
  end
end
