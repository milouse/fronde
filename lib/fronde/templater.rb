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
      @dom.css(@config['selector']).each do |element|
        insert_new_node_at element, content
      end
    end

    def applied?
      @dom.xpath('//html').children.any? do |child|
        next false unless child.comment?

        child.text == @config['check_line']
      end
    end

    def valid?(file_name)
      return false unless @config.has_key?('selector')

      unless @config.has_key?('content') || @config.has_key?('source')
        return false
      end

      check_path(file_name)
    end

    class << self
      def customize_output(file_name)
        source = Fronde::Org::File.new(file_name)
        # Return if no org file found for this published file
        return if source.file.end_with?(file_name)

        dom = open_dom(file_name)
        updated = apply_templates(source, dom, file_name)
        write_dom(file_name, dom) if updated
      end

      private

      def apply_templates(source, dom, file_name)
        Fronde::CONFIG.get('templates', []).map do |config|
          template = Fronde::Templater.new(source, dom, config)
          next if !template.valid?(file_name) || template.applied?

          template.apply
          true
        end.any?
      end

      def open_dom(file_name)
        File.open(file_name, 'r') do |file|
          Nokogiri::HTML file
        end
      end

      def write_dom(file_name, dom)
        File.open(file_name, 'w') do |file|
          dom.write_to file
        end
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
      pub_folder = Fronde::CONFIG.get('html_public_folder').sub(
        /^#{Dir.pwd}/, '.'
      )
      warn(
        R18n.t.fronde.error.templater.no_element_found(
          source: source, file: "#{pub_folder}#{@org_file.pub_file}"
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

    def check_path(file_name)
      paths = @config['path']
      return true unless paths

      paths = [paths] unless paths.is_a? Array

      pub_folder = Fronde::CONFIG.get('html_public_folder')
      paths.any? do |template_path|
        File.fnmatch?("#{pub_folder}#{template_path}", file_name)
      end
    end
  end
end
