# https://gist.github.com/nateberkopec/eb00665c213fff30ca3894814d3975f6
function gc-ai
    # Parse arguments
    argparse 'c/context' -- $argv
    or return

    # Get the git diff
    set diff (git diff --cached)

    if test -z "$diff"
        echo "No staged changes to commit"
        return 1
    end

    # Collect context if -c flag is provided
    set context ""
    if set -q _flag_context
        set context (gum write --placeholder "What motivated this change? Is there context a future reader will need to know? What problem did you solve?")
    end

    # Start with default temperature (no -o flag)
    set temp_options
    set reroll_count 0
    set base_prompt "Write a git commit message for these changes. Format it as:
- First line: a summary of 72 characters or less
- Second line: blank
- Remaining lines: detailed description
Return only the commit message."

    if test -n "$context"
        set prompt "$base_prompt
Additional context from the developer:
$context"
    else
        set prompt "$base_prompt"
    end

    # Loop to allow rerolls
    while true
        # Generate commit message and save to temp file to preserve newlines
        set temp_file (mktemp)
        if test (count $temp_options) -eq 0
            echo "$diff" | llm -s "$prompt" > $temp_file
        else
            echo "$diff" | llm -s "$prompt" $temp_options > $temp_file
        end

        # Read first line to check length
        set summary (head -n 1 $temp_file)
        set summary_length (string length "$summary")

        if test $summary_length -gt 72
            echo "Summary too long ($summary_length chars), regenerating..."
            rm $temp_file
            continue
        end

        # Clean up excessive blank lines (condense 2+ blank lines into 1)
        set cleaned_file (mktemp)
        cat $temp_file | sed '/^$/N;/^\n$/D' > $cleaned_file
        rm $temp_file
        set temp_file $cleaned_file

        # Show the generated message
        echo "Generated commit message:"
        gum style --foreground 212 < $temp_file
        echo ""

        # Let user choose what to do
        set action (gum choose "Commit" "Reroll" "Cancel")

        switch $action
            case "Commit"
                git commit -F $temp_file
                rm $temp_file
                return 0
            case "Reroll"
                set reroll_count (math $reroll_count + 1)
                set temp (math "0.5 + $reroll_count * 0.3")
                set temp_options -o temperature $temp
                echo "Generating new message (temperature: $temp)..."
                rm $temp_file
                continue
            case "Cancel"
                echo "Commit cancelled"
                return 1
        end
    end
end
