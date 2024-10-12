function __fronde_main_commands
  echo new\t'Initialize a new Fronde instance.'\nupdate\t'Update Fronde configuration and dependency (to be run after each modification of the config.yml file and once in a while to stay up-to-date with Org).'\npreview\t'Start a test web server to preview the generated website.'\nopen\t'Open or create an org file.'\nbuild\t'Compile all org files to HTML or gemtext.'\npublish\t'Push local changes to the public web server.'\nhelp\t'Alias for the -h switch.'
end

function __fronde_no_command_yet
  set -f cmd (__fish_print_cmd_args_without_options)
  not contains new $cmd
  and not contains update $cmd
  and not contains preview $cmd
  and not contains open $cmd
  and not contains build $cmd
  and not contains publish $cmd
  and not contains help $cmd
end

complete -c fronde -e
complete -c fronde -f
complete -c fronde -x -n '__fronde_no_command_yet' -a '(__fronde_main_commands)'
complete -c fronde -n 'contains help (__fish_print_cmd_args_without_options)' -a 'new update preview open build publish help'
complete -c fronde -n 'contains new (__fish_print_cmd_args_without_options)' -F
complete -c fronde -n 'contains open (__fish_print_cmd_args_without_options)' -F
complete -c fronde -n 'contains new (__fish_print_cmd_args_without_options)' -s a -l author -r -d Author
complete -c fronde -n 'contains new (__fish_print_cmd_args_without_options)' -s l -l lang -r -d Lang
complete -c fronde -n 'contains new (__fish_print_cmd_args_without_options)' -s o -l output -r -a 'gemini html' -d Output
complete -c fronde -n 'contains new (__fish_print_cmd_args_without_options)' -s t -l title -r -d Title
complete -c fronde -n 'contains new (__fish_print_cmd_args_without_options)' -s v -l verbose -d Verbose
complete -c fronde -n 'contains open (__fish_print_cmd_args_without_options)' -s a -l author -r -d Author
complete -c fronde -n 'contains open (__fish_print_cmd_args_without_options)' -s l -l lang -r -d Lang
complete -c fronde -n 'contains open (__fish_print_cmd_args_without_options)' -s t -l title -r -d Title
complete -c fronde -n 'contains open (__fish_print_cmd_args_without_options)' -s v -l verbose -d Verbose
complete -c fronde -n 'contains build (__fish_print_cmd_args_without_options)' -s f -l force -d Force
complete -c fronde -s h -l help -d 'Display help for a command and exit.'
complete -c fronde -s V -l version -d 'Display Fronde version and exit.'
