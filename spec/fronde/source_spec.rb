# frozen_string_literal: true

describe Fronde::Source do
  it 'can not be instantiated directly' do
    expect { described_class.new({}) }.to raise_error(NameError)
    expect { described_class.new({}) }.to raise_error(/`fill_in_specific_config'/)
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

  it 'computes the right pub_file path for theoritical sources' do
    config = described_class.canonical_config('path' => 'src', 'target' => '.')
    project = described_class.new_from_config(config)
    target = project.target_for 'src/test.org'
    expect(target).to eq('/test.html')
    target = project.target_for 'src/blog/test.org'
    expect(target).to eq('/blog/test.html')
    target = project.target_for 'src/blog/toto/tata.org'
    expect(target).to eq('/blog/toto/tata.html')
    target = project.target_for 'src/blog/toto/content.org'
    expect(target).to eq('/blog/toto/content.html')

    config = described_class.canonical_config('path' => '~/tata', 'target' => '.')
    project = described_class.new_from_config(config)
    target = project.target_for '~/tata/tutu/content.org'
    expect(target).to eq('/tutu/content.html')
    target = project.target_for '~/tata/blog/content.org'
    expect(target).to eq('/blog/content.html')
  end
end
