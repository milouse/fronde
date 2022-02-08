# frozen_string_literal: true

module Fronde
  # Fronde commands
  module CLICommands
    def fronde_update
      update_config
      @rake.options.build_all = true
      @rake.invoke_task('org:upgrade')
    end
    alias_method :fronde_config, :fronde_update

    def fronde_init
      update_config
      @rake.options.build_all = true
      @rake.invoke_task('org:install')
      return if File.exist? 'src/index.org'
      Fronde::OrgFile.new('src/index.org', @options).write
      fronde_open 'src/index.org'
    end

    def fronde_build
      @rake.options.build_all = true
      task = 'site:build'
      task = "#{task}[true]" if @options[:force]
      @rake.invoke_task task
    end

    def fronde_preview
      Thread.new do
        sleep 1
        port = Fronde::Config.settings.dig('preview', 'server_port') || 5000
        uri = "http://127.0.0.1:#{port}/"
        current_os = Fronde::Utils.current_os
        case current_os
        when 'windows'
          system 'start', uri
        when 'apple'
          system 'open', uri
        else
          system 'gio', 'open', uri
        end
      end
      @rake.invoke_task('site:preview')
    end

    def fronde_open(file_path = ARGV[0])
      editor = ENV['EDITOR'] || ENV['VISUAL'] || 'emacs'
      cmd = [editor]
      if file_path.nil? || !File.file?(file_path)
        # file_path may be updated with title given in options
        file_path = create_new_file(file_path)
        # Only move to the end of file for new file. Let the editor handle
        # the best position for already existing files
        cmd << '+6'
      end
      cmd << file_path
      system(*cmd)
    end
    alias_method :fronde_edit, :fronde_open

    def fronde_publish
      @rake.invoke_task('sync:push')
    end

    def fronde_help(command = 'basic', error: false)
      warn R18n.t.fronde.bin.error.no_command if error
      cmd_opt = Fronde::Utils.command_options(command)
      label = cmd_opt[:label] || command
      warn format("%<label>s\n\n", label: R18n.t.fronde.bin.usage(label))
      cmd = cmd_opt[:name] || command
      if R18n.t.fronde.bin.commands[cmd].translated?
        warn format("%<label>s\n\n", label: R18n.t.fronde.bin.commands[cmd])
      end
      warn help_command_body(cmd).join("\n")
      exit 1 if error
      exit
    end

    private

    def update_config
      cnf = @options.merge
      cnf.delete(:verbose)
      cnf.transform_keys!(&:to_s)
      Fronde::Config.save(Fronde::Config.settings.merge(cnf))
    end

    def new_file_name(file_path)
      file_path = File.expand_path(file_path || '')
      return file_path if file_path[-4..] == '.org'
      # file_path seems to be a dir path. Thus we have to create the new
      # filename from its title
      title = @options[:title]
      # No title, nor a reliable file_path? Better abort
      return nil if title.nil? || title == ''
      filename = "#{Fronde::OrgFile.slug(title)}.org"
      File.join file_path, filename
    end

    def create_new_file(file_path)
      filename = new_file_name(file_path)
      if filename.nil?
        warn R18n.t.fronde.bin.error.no_file
        exit 1
      end
      FileUtils.mkdir_p File.dirname(filename)
      Fronde::OrgFile.new(filename, @options).write
      filename
    end

    def help_command_body(command)
      body = [
        R18n.t.fronde.bin.options.cmd_title,
        Fronde::Utils.summarize_command(command)
      ]
      return body unless command == 'basic'
      body + [
        '',
        R18n.t.fronde.bin.commands.cmd_title,
        Fronde::Utils.list_commands
      ]
    end
  end
end
