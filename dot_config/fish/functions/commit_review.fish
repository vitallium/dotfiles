function commit_review
    set -l start_commit $argv[1]
    test -z "$start_commit"; and set start_commit HEAD~1

    set -l temp_file (mktemp)
    git diff --no-color "$start_commit" -- ':!sorbet/rbi/dsl' ':!sorbet/rbi/gems' ':!yarn.lock' ':!fixtures' >$temp_file

    if test -s $temp_file
        cat $temp_file | llm -m claude-3.7-sonnet -t commit_review
    else
        echo "No changes to review."
    end

    rm -f $temp_file
end
