package com.evolved.automata.speech;

import java.util.*;

public class AlternativeStructuredLiteral extends  Pattern {


    HashMap<String, LinkedList<Pattern>> mPatternIndex = new HashMap<String, LinkedList<Pattern>>();
    LinkedList<Pattern> mChildren;
    LinkedList<Pattern> mUnIndexChildren;


    public LinkedList<MatchContinuation> getBestMatchingPrefix(String[] inputTokens, MatchConfiguration config)
    {
        ValueConstructor constructor = config.removeValueConstructor();
        PriorityQueue<MatchContinuation> mProcessQueue = new PriorityQueue<MatchContinuation>(inputTokens.length, new Comparator<MatchContinuation>() {
            @Override
            public int compare(MatchContinuation lvalue, MatchContinuation rvalue) {
                int lscore = lvalue.getPriorDistance() + lvalue.getRemainingTokens().length;
                int rscore = rvalue.getPriorDistance() + rvalue.getRemainingTokens().length;

                return Integer.compare(lscore, rscore);
            }
        });


        LinkedList<Pattern> viableChildren = getViablePatterns(inputTokens);
        for (Pattern p:viableChildren)
        {
            for (MatchContinuation continuation:p.getBestMatchingPrefix(inputTokens, config.setValueConstructor(constructor)))
            {
                if (continuation.getValue() == null || !continuation.getValue().isException())
                    mProcessQueue.add(continuation);
            }
        }

        int min = Integer.MAX_VALUE;
        LinkedList<MatchContinuation> results = new LinkedList<MatchContinuation>();

        while (mProcessQueue.size() > 0)
        {
            MatchContinuation top = mProcessQueue.poll();
            int score = top.getPriorDistance() + top.getRemainingTokens().length;
            if (score < min)
            {
                results = new LinkedList<MatchContinuation>();
                results.add(top);
                min = score;
            }
            else if (score == min)
            {
                results.add(top);
            }
            else
                break;
        }

        return results;
    }

    private AlternativeStructuredLiteral(LinkedList<Pattern> children)
    {

        mChildren = new LinkedList<Pattern>();
        mUnIndexChildren = new LinkedList<Pattern>();

        for (Pattern p: children)
        {
            addPattern(p);
        }
    }

    private AlternativeStructuredLiteral(TokenizedString[] phrases)
    {
        mChildren = new LinkedList<Pattern>();
        mUnIndexChildren = new LinkedList<Pattern>();

        for (TokenizedString phrase: phrases)
        {
            Pattern p = StructuredLiteral.make(phrase.getArray());
            addPattern(p);
        }

    }

    private void addPattern(Pattern p)
    {
        mChildren.add(p);
        if (!(p instanceof StructuredLiteral))
        {
            mUnIndexChildren.add(p);
        }
        else
        {
            StructuredLiteral literal = (StructuredLiteral)p;
            String[] compoments = literal.getGrammar();
            HashSet<String> already = new HashSet<String>();
            for (String grammar:compoments)
            {
                if (already.contains(grammar))
                    continue;
                LinkedList<Pattern> index = mPatternIndex.get(grammar);
                if (index == null) {
                    index = new LinkedList<Pattern>();
                    mPatternIndex.put(grammar, index);
                }
                index.add(p);
                already.add(grammar);
            }
        }
    }

    LinkedList<Pattern> getViablePatterns(String[] input)
    {
        HashSet<Pattern> mergeTokens = new HashSet<Pattern>();
        LinkedList<Pattern> selected = new LinkedList<Pattern>(mUnIndexChildren);
        HashSet<String> repeatedTokens = new HashSet<String>();
        for (String token:input)
        {
            if (!repeatedTokens.contains(token))
            {
                repeatedTokens.add(token);
                LinkedList<Pattern> viable = mPatternIndex.get(token);
                if (viable != null)
                {
                    mergeTokens.addAll(viable);
                }
            }

        }
        selected.addAll(mergeTokens);
        return selected;
    }

    public static AlternativeStructuredLiteral make(LinkedList<Pattern> children)
    {
        return new AlternativeStructuredLiteral(children);
    }

    public static AlternativeStructuredLiteral make(TokenizedString[] phrases)
    {
        return new AlternativeStructuredLiteral(phrases);
    }

    public static AlternativeStructuredLiteral make(String[] phrases)
    {
        TokenizedString[] t = new TokenizedString[phrases.length];
        for (int i = 0; i < phrases.length;i++)
        {
            t[i] = TokenizedString.make(phrases[i]);
        }
        return new AlternativeStructuredLiteral(t);
    }




}
