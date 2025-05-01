function commit_msg
    set -l temp_file (mktemp)
    git diff --no-color --staged >$temp_file

    if test -s $temp_file
        cat $temp_file | llm -m claude-3.7-sonnet -t commit_message
    else
        echo "No changes to commit."
    end

    rm -f $temp_file
end
