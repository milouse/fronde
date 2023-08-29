# frozen_string_literal: true

require 'nokogiri'
require 'digest/md5'
require_relative 'org_file'

module Fronde
  # Insert custom part inside generated HTML files.
  class Templater
    def initialize(source, dom, opts = {})
      @dom = dom
      @org_file = source
      @position = opts['type'] || 'after'
      @content = extract_content opts
      @element = @dom.css(opts['selector'])
      digest = Digest::MD5.hexdigest(@content)
      @check_line = " Fronde Template: #{digest} "
    end

    def apply
      flag_head
      content = @org_file.format(@content)
      @element.each do |e|
        insert_new_node_at e, content
      end
    end

    def in_head?
      @dom.xpath('//head').children.to_a.filter(&:comment?).each do |c|
        return true if c.text == @check_line
      end
      false
    end

    class << self
      def customize_output(file_name)
        templates_to_apply = filter_templates(file_name)
        return if templates_to_apply.empty?

        source = Fronde::OrgFile.new(file_name)
        # Return if no org file found for this published file
        return if source.file == file_name

        dom = open_dom(file_name)
        templates_to_apply.each do |t|
          tpl = Fronde::Templater.new(source, dom, t)
          next if tpl.in_head?
          tpl.apply
        end
        write_dom(file_name, dom)
      end

      private

      def filter_templates(file_name)
        templates = Fronde::CONFIG.get('templates')
        return [] if templates.nil? || templates.empty?
        templates.filter { |t| check_required_keys(t, file_name) }
      end

      def open_dom(file_name)
        file = File.new file_name, 'r'
        dom = Nokogiri::HTML file
        file.close
        dom
      end

      def write_dom(file_name, dom)
        file = File.new file_name, 'w'
        dom.write_to file
        file.close
      end

      def check_path(file_name, pathes)
        pub_folder = Fronde::CONFIG.get('html_public_folder')
        if pathes.is_a?(Array)
          pathes.each do |tp|
            return true if File.fnmatch?("#{pub_folder}#{tp}",
                                         file_name, File::FNM_DOTMATCH)
          end
          return false
        end
        File.fnmatch?("#{pub_folder}#{pathes}",
                      file_name, File::FNM_DOTMATCH)
      end

      def check_required_keys(opts, file_name)
        return false unless opts.has_key?('selector')
        return false unless opts.has_key?('content') || opts.has_key?('source')
        return check_path(file_name, opts['path']) if opts.has_key?('path')
        true
      end
    end

    private

    def flag_head
      @dom.xpath('//head').first.prepend_child("<!--#{@check_line}-->\n")
    end

    def insert_new_node_at(elem, content)
      case @position
      when 'before'
        elem.add_previous_sibling content
      when 'replace'
        elem.replace content
      else
        elem.add_next_sibling content
      end
    end

    def extract_content(opts)
      return opts['content'] if opts['content']
      # If we don't have a content option, then we must have a source
      # one.
      @dom.css(opts['source']).unlink.to_s
    end
  end
end
