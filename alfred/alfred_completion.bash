# Alfred — Bash completion
# Source this file: source ~/.alfred/alfred_completion.bash

_alfred_completions() {
    local cur prev
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    # Top-level commands
    local commands="project task note vault chat ask shell remind culture search briefing suggest summarize prioritize think cortex arms soul dashboard health status help memory daemon simplex user"

    case "${prev}" in
        alfred)
            COMPREPLY=( $(compgen -W "${commands}" -- "${cur}") )
            return 0
            ;;
        project)
            COMPREPLY=( $(compgen -W "new list delete" -- "${cur}") )
            return 0
            ;;
        task)
            COMPREPLY=( $(compgen -W "add list done delete priority" -- "${cur}") )
            return 0
            ;;
        note)
            COMPREPLY=( $(compgen -W "add list" -- "${cur}") )
            return 0
            ;;
        vault)
            COMPREPLY=( $(compgen -W "setup status store get list delete note notes destroy migrate" -- "${cur}") )
            return 0
            ;;
        cortex)
            COMPREPLY=( $(compgen -W "trends stats analyze productivity culture correlations" -- "${cur}") )
            return 0
            ;;
        arms)
            COMPREPLY=( $(compgen -W "status disk memory backup" -- "${cur}") )
            return 0
            ;;
        daemon)
            COMPREPLY=( $(compgen -W "start stop status" -- "${cur}") )
            return 0
            ;;
        simplex)
            COMPREPLY=( $(compgen -W "connect status send disconnect" -- "${cur}") )
            return 0
            ;;
        soul)
            COMPREPLY=( $(compgen -W "init check history reset" -- "${cur}") )
            return 0
            ;;
        memory)
            COMPREPLY=( $(compgen -W "facts search episodes forget" -- "${cur}") )
            return 0
            ;;
        culture)
            COMPREPLY=( $(compgen -W "learn search list suggestions approve dismiss" -- "${cur}") )
            return 0
            ;;
        remind)
            COMPREPLY=( $(compgen -W "list done delete" -- "${cur}") )
            return 0
            ;;
        user)
            COMPREPLY=( $(compgen -W "add list delete" -- "${cur}") )
            return 0
            ;;
        think)
            COMPREPLY=( $(compgen -W "about culture" -- "${cur}") )
            return 0
            ;;
    esac

    # Default: complete with commands
    if [ "${COMP_CWORD}" -eq 1 ]; then
        COMPREPLY=( $(compgen -W "${commands}" -- "${cur}") )
    fi
}

complete -F _alfred_completions alfred
