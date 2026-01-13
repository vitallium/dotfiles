function commit_msg
    set -l temp_file (mktemp)
    git diff --no-color --staged >$temp_file

    if test -s $temp_file
        llm -m claude-sonnet-4 -t commit_message < $temp_file
    else
        echo "No changes to commit."
    end

    rm -f $temp_file
end
