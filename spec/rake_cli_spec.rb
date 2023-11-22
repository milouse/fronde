# frozen_string_literal: true

require 'rake'

context 'with an example project' do
  before { init_testing_environment }

  after { tear_down 'tmp/website_testing' }

  it 'generates a zsh completion file' do
    proof = File.expand_path 'data/fronde.zsh', __dir__
    proof_content = File.read(proof)
    expect { rake.invoke_task('cli:zsh_complete') }.to(
      output(proof_content).to_stdout
    )
  end
end
