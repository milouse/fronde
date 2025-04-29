# frozen_string_literal: true

module Fronde
  # @return [String] the version number of the current Fronde release.
  VERSION = '0.6.3'

  USER_AGENT = "Fronde/#{Fronde::VERSION} (#{RUBY_ENGINE} #{RUBY_VERSION} #{RUBY_PLATFORM}) (+https://etienne.pflieger.bzh/fronde/)".freeze
end
