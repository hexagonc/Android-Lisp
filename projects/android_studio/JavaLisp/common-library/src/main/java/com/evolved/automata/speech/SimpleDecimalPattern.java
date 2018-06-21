package com.evolved.automata.speech;

import java.util.LinkedList;

public class SimpleDecimalPattern extends Pattern {
    @Override
    public LinkedList<MatchContinuation> getBestMatchingPrefix(String[] inputTokens, MatchConfiguration config) {

        LinkedList<String> remaining = new LinkedList<String>();
        LinkedList<MatchContinuation> results = new LinkedList<MatchContinuation>();
        int error = 0, matchError = 0;
        double value = 0;
        int i, j = 0;
        for (i = 0; i < inputTokens.length;i++)
        {
            String term = inputTokens[i];
            try
            {
                value = Double.parseDouble(term);
                j = i+1;
                matchError = error;
                break;
            }
            catch (NumberFormatException nf)
            {
                error++;
                matchError = 3; // this is the cost of inserting the number.
                // if the number eventually gets parsed then this will be
                // replaced by the cost of deleting extraneous tokens ahead of it
            }

        }

        for (;j < inputTokens.length;j++)
            remaining.add(inputTokens[j]);
        MatchContinuation result = new MatchContinuation(SpeechValue.make(value), matchError, remaining.toArray(new String[0]), null);
        results.add(result);

        return results;
    }

    @Override
    public String getName() {
        return "number";
    }
}
