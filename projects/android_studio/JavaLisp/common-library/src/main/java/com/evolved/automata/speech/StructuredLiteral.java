package com.evolved.automata.speech;

import java.util.*;

public class StructuredLiteral extends Pattern {

    String[] mGrammarTokens;

    public String[] getGrammar()
    {
        return mGrammarTokens;
    }

    private class EditDistanceStep {
        int[] _editDistances;

        HashMap<String, EditDistanceStep> _stepLinks;
        String _token;
        int _index = -1;
        public EditDistanceStep(MatchConfiguration config)
        {
            _editDistances = new int[mGrammarTokens.length + 1];
            for (int i = 0; i < _editDistances.length;i++)
                _editDistances[i] = i * config.getInsertCost(_token);
            _stepLinks = new HashMap<String, EditDistanceStep>();
            _token = "";
        }

        public EditDistanceStep(String token, EditDistanceStep prev, MatchConfiguration config)
        {
            _index = prev._index + 1;
            _token = token;
            _editDistances = getNextEditDistanceLine(prev.getStepErrors(), config);

            _stepLinks = new HashMap<String, EditDistanceStep>();

        }

        public EditDistanceStep add(String converted, MatchConfiguration config)
        {

            EditDistanceStep next = _stepLinks.get(converted);
            if (next == null) {
                next = new EditDistanceStep(converted, this, config);
                _stepLinks.put(converted, next);
            }
            return next;
        }

        public EditDistanceStep get(String converted)
        {

            EditDistanceStep next = _stepLinks.get(converted);
            return next;
        }

        public int[] getStepErrors()
        {
            return _editDistances;
        }

        public int[] getNextEditDistanceLine(int[] prevError, MatchConfiguration config)
        {
            int[] out = new int[prevError.length];

            int deleteStepCost = config.getDeleteCost(_token);
            for (int i = 0; i < out.length;i++)
            {
                if (i == 0)
                    out[i] = prevError[i] + deleteStepCost;
                else
                {
                    String currentPattern = mGrammarTokens[i-1];
                    int prevLeft = out[i - 1];
                    int prevTopLeft = prevError[i - 1];
                    int prevTop = prevError[i];

                    int minStart = Math.min(prevLeft, Math.min(prevTop, prevTopLeft));
                    if (currentPattern.equals(_token))
                    {
                        out[i] = minStart;
                    }
                    else
                    {

                        int insertCost = prevLeft + config.getInsertCost(currentPattern);
                        int subCost = prevTopLeft + config.getSubstitutionCost(_token, currentPattern);
                        int deleteCost = prevTop + deleteStepCost;
                        out[i] = Math.min(insertCost, Math.min(subCost, deleteCost));
                    }

                }
            }
            return out;
        }

    }



    HashMap<String, EditDistanceStep> mBaseTrie;

    private StructuredLiteral(String[] grammar)
    {
        mGrammarTokens = grammar;
        mBaseTrie = new HashMap<String, EditDistanceStep>();

    }

    public int match(String[] input, MatchConfiguration config)
    {
        String token;
        int i;
        EditDistanceStep prev = null, next = null;
        for (i = 0; i < input.length;i++)
        {
            prev = next;
            token = input[i];
            if (i == 0)
            {
                if (mBaseTrie.containsKey(token))
                {
                    prev = mBaseTrie.get(token);
                }
                else
                {
                    prev = new EditDistanceStep(config);
                    prev.add(token, config);
                    mBaseTrie.put(token, prev);
                }
            }
            next = prev.add(token, config);

        }

        return next.getStepErrors()[mGrammarTokens.length];
    }

    public int[] getPrefixMatches(String[] input, MatchConfiguration config)
    {
        int[] out = new int[input.length + 1];
        String token;
        int i;
        EditDistanceStep prev = new EditDistanceStep(config), next = null;
        out[0] = prev.getStepErrors()[mGrammarTokens.length];

        for (i = 0; i < input.length;i++)
        {
            prev = next;
            token = config.getGroupWord(input[i]);
            if (i == 0)
            {
                if (mBaseTrie.containsKey(token))
                {
                    prev = mBaseTrie.get(token);
                }
                else
                {
                    prev = new EditDistanceStep(config);
                    prev.add(token, config);
                    mBaseTrie.put(token, prev);
                }
            }
            next = prev.add(token, config);
            out[i + 1] = next.getStepErrors()[mGrammarTokens.length];
        }
        return out;
    }


    public synchronized LinkedList<MatchContinuation> getBestMatchingPrefix(String[] input, MatchConfiguration config)
    {
        LinkedList<MatchContinuation> prefixes = new LinkedList<MatchContinuation>();

        int[] prefixMatches = getPrefixMatches(input, config);

        int minDistance = Integer.MAX_VALUE;
        int numMinimum = 0;
        LinkedHashSet<Integer> minimal = null;

        for (int i = 0; i < prefixMatches.length;i++)
        {
            if (minDistance >= prefixMatches[i])
            {
                if (minDistance > prefixMatches[i])
                {
                    minimal = new LinkedHashSet<Integer>();
                    minimal.add(i);
                    minDistance = prefixMatches[i];
                }
                else {
                    minimal.add(i);
                }
            }
        }

        ValueConstructor constructor = config.getValueConstructor();
        int smallestViablePrefixIndex = -1, offset = 0;
        for (Integer index:minimal)
        {
            if (smallestViablePrefixIndex == -1)
            {
                smallestViablePrefixIndex = index;
            }

            int error = prefixMatches[index];

            String[] out = new String[input.length  - index];
            for (int i = index; i < input.length;i++)
            {
                out[i - index] = input[i];
            }
            prefixes.add(new MatchContinuation(getValue(error, constructor), error, out, null));
        }
        return prefixes;
    }



    private SpeechValue getValue(int error, ValueConstructor constructor)
    {


        if (constructor != null)
        {
            return constructor.getValue(SpeechValue.make(error));
        }
        else
        {
            return SpeechValue.make(mGrammarTokens);
        }
    }


    public static StructuredLiteral make(String[] grammar)
    {
        return new StructuredLiteral(grammar);
    }



}
