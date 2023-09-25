# frozen_string_literal: true

describe Fronde::Source do
  it 'computes the right pub_file path for theoritical sources', core: true do
    config = described_class.canonical_config('path' => 'src', 'target' => '.')
    project = described_class.new_from_config(config)
    target = project.target_for 'src/test.org'
    expect(target).to eq('test.html')
    target = project.target_for 'src/blog/test.org'
    expect(target).to eq('blog/test.html')
    target = project.target_for 'src/blog/toto/tata.org'
    expect(target).to eq('blog/toto/tata.html')
    target = project.target_for 'src/blog/toto/content.org'
    expect(target).to eq('blog/toto/content.html')

    config = described_class.canonical_config('path' => '~/tata', 'target' => '.')
    project = described_class.new_from_config(config)
    target = project.target_for '~/tata/tutu/content.org'
    expect(target).to eq('tutu/content.html')
    target = project.target_for '~/tata/blog/content.org'
    expect(target).to eq('blog/content.html')
  end
end
