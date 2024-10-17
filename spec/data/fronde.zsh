#compdef fronde
#autoload


(( $+functions[__fronde_new] )) ||
__fronde_new(){
    _arguments \
        '(-a --author)'{-a,--author}':AUTHOR:' \
        '(-l --lang)'{-l,--lang}':LOCALE:' \
        '(-o --output)'{-o,--output}':FORMAT:' \
        '(-t --title)'{-t,--title}':TITLE:' \
        '(-v --verbose)'{-v,--verbose}
}

(( $+functions[__fronde_update] )) ||
__fronde_update(){
    _arguments \
        '(-v --verbose)'{-v,--verbose}
}

(( $+functions[__fronde_open] )) ||
__fronde_open(){
    _arguments \
        '(-a --author)'{-a,--author}':AUTHOR:' \
        '(-l --lang)'{-l,--lang}':LOCALE:' \
        '(-t --title)'{-t,--title}':TITLE:' \
        '1:file:_files -g \*.org'
}

(( $+functions[__fronde_build] )) ||
__fronde_build(){
    _arguments \
        '(-f --force)'{-f,--force} \
        '(-v --verbose)'{-v,--verbose}
}

(( $+functions[__fronde_publish] )) ||
__fronde_publish(){
    _arguments \
        '(-v --verbose)'{-v,--verbose}
}


(( $+functions[__fronde_help] )) ||
__fronde_help(){
    _arguments \
        "1:command:((new\:'Initialize a new Fronde instance.' update\:'Update Fronde configuration and dependency (to be run after each modification of the config.yml file and once in a while to stay up-to-date with Org).' preview\:'Start a test web server to preview the generated website.' open\:'Open or create an org file.' build\:'Compile all org files to HTML or gemtext.' publish\:'Push local changes to the public web server.' help\:'Alias for the -h switch.' ))"
}

local state

_arguments -C \
    '(-)-h[help]' \
    '(-)-V[version]' \
    "1:command:((new\:'Initialize a new Fronde instance.' update\:'Update Fronde configuration and dependency (to be run after each modification of the config.yml file and once in a while to stay up-to-date with Org).' preview\:'Start a test web server to preview the generated website.' open\:'Open or create an org file.' build\:'Compile all org files to HTML or gemtext.' publish\:'Push local changes to the public web server.' help\:'Alias for the -h switch.' ))" \
    '*::arg:->args'

if [ "$state" = args ]; then
    _call_function ret __fronde_${words[1]}
fi
