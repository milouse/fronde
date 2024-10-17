# frozen_string_literal: true

require 'nokogiri'
require 'digest/md5'
require_relative 'org/file'

module Fronde
  # Insert custom part inside generated HTML files.
  class Templater
    def initialize(source, dom, config = {})
      @dom = dom
      @org_file = source
      @config = { 'type' => 'after' }.merge(config)
      digest = Digest::MD5.hexdigest(config.to_s)
      @config['check_line'] = " Fronde Template: #{digest} "
    end

    def apply
      # Flag the file for this template
      html = @dom.xpath('//html').first
      html.add_child("<!--#{@config['check_line']}-->\n")
      content = @org_file.format extract_content
      # Remove source element if necessary to avoid doubling it during
      # the insert action
      @config['source'].unlink if @config.has_key? 'source'
      # Insert new content
      @dom.css(@config['selector']).map do |element|
        insert_new_node_at element, content
      end
    end

    def applied?
      @dom.xpath('//html').children.any? do |child|
        next false unless child.comment?

        child.text == @config['check_line']
      end
    end

    def valid?
      return false unless @config.has_key?('selector')

      unless @config.has_key?('content') || @config.has_key?('source')
        return false
      end

      check_path
    end

    class << self
      def customize_output(file_name)
        source = Fronde::Org::File.new file_name
        # Return if no org file found for this published file
        return unless source.pub_file

        apply_templates source
      end

      def apply_templates(source)
        public_file = source.pub_file absolute: true
        dom = File.open(public_file, 'r') { Nokogiri::HTML _1 }
        changes = Fronde::CONFIG.get('templates', []).map do |config|
          template = Fronde::Templater.new(source, dom, config)
          next if !template.valid? || template.applied?

          template.apply
        end
        File.open(public_file, 'w') { dom.write_to _1 } if changes.any?
      end
    end

    private

    def insert_new_node_at(element, content)
      case @config['type']
      when 'before'
        element.add_previous_sibling content
      when 'replace'
        element.replace content
      else
        element.add_next_sibling content
      end
    end

    def warn_no_element(source)
      public_file = @org_file.pub_file(absolute: true)
      warn(
        I18n.t(
          'fronde.error.templater.no_element_found',
          source: source, file: public_file.sub(/^#{Dir.pwd}/, '.')
        )
      )
      '' # Return empty string
    end

    def extract_content
      # We must either have a source or a content key
      source = @config.delete 'source'
      unless source.is_a?(String) && source != ''
        return @config['content'] || ''
      end

      node = @dom.css(source)
      if node.any?
        # Put it back in config
        @config['source'] = node
        return node.to_s
      end

      # Do nothing if we don’t have a reliable content to work with
      warn_no_element source
    end

    def check_path
      paths = @config['path']
      return true unless paths

      paths = [paths] unless paths.is_a? Array

      paths.any? do |template_path|
        File.fnmatch?(template_path, @org_file.pub_file)
      end
    end
  end
end
