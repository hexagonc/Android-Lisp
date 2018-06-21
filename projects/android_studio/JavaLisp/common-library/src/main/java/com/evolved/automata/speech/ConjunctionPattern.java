package com.evolved.automata.speech;

import java.util.*;

public class ConjunctionPattern extends Pattern {


    ArrayList<Pattern> mChildren;


    public LinkedList<MatchContinuation> getBestMatchingPrefix(String[] inputTokens, MatchConfiguration config)
    {
        ValueConstructor constructor = config.removeValueConstructor();

        Comparator<MatchContinuation> comp = new Comparator<MatchContinuation>() {
            @Override
            public int compare(MatchContinuation lvalue, MatchContinuation rvalue) {
                int lscore = lvalue.getPriorDistance() + lvalue.getRemainingTokens().length;
                int rscore = rvalue.getPriorDistance() + rvalue.getRemainingTokens().length;

                return Integer.compare(lscore, rscore);
            }
        };

        PriorityQueue<MatchContinuation> nextQueue, processQueue = new PriorityQueue<MatchContinuation>(inputTokens.length, comp),
                resultQueue = new PriorityQueue<MatchContinuation>(inputTokens.length, comp);;

        nextQueue = processQueue;

        Pattern first = mChildren.get(0);

        for (MatchContinuation m:first.getBestMatchingPrefix(inputTokens, config))
        {
            int index = m.getIndex();
            String[] tokens = m.getRemainingTokens();
            int distance = m.getPriorDistance();
            SpeechValue obj = m.getValue();

            if (mChildren.size() == 1)
            {
                resultQueue.add(new MatchContinuation(obj, distance + tokens.length, tokens,1));
            }
            else
                nextQueue.add(new MatchContinuation(obj, distance, tokens,1));

        }

        LinkedList<MatchContinuation> results = null;

        while (nextQueue.size() > 0)
        {
            processQueue = nextQueue;
            nextQueue = new PriorityQueue<MatchContinuation>(inputTokens.length, comp);

            while (processQueue.size() > 0)
            {
                MatchContinuation top = processQueue.poll(), next;

                String[] tokens = top.getRemainingTokens();
                int index = top.getIndex();
                int editDistance = top.getPriorDistance();

                Pattern pattern = mChildren.get(index);

                for (MatchContinuation cont:pattern.getBestMatchingPrefix(tokens, config))
                {
                    if (cont.getValue() == null || !cont.getValue().isException())
                    if (index + 1 == mChildren.size())
                    {
                        resultQueue.add(new MatchContinuation(cont.getValue(), editDistance + cont.getPriorDistance() + cont.getRemainingTokens().length, cont.getRemainingTokens(), index + 1, top));
                    }
                    else
                        nextQueue.add(new MatchContinuation(cont.getValue(), editDistance + cont.getPriorDistance(), cont.getRemainingTokens(), index + 1, top));
                }
            }
        }

        LinkedList<MatchContinuation> out = null;

        int min = Integer.MAX_VALUE;
        boolean f = true;

        while (resultQueue.size() > 0)
        {
            MatchContinuation cont = resultQueue.poll();
            int distance = cont.getPriorDistance() + cont.getRemainingTokens().length;
            if (f)
            {
                min = distance;
                out = new LinkedList<MatchContinuation>();
                setValue(cont, constructor);
                out.add(cont);

                f = false;
            }
            else if (distance == min)
            {
                setValue(cont, constructor);
                out.add(cont);
            }
            else
                break;
        }

        return out;
    }

    private LinkedList<MatchContinuation> getNamedChildren(MatchContinuation lastSegment)
    {
        LinkedList<MatchContinuation> namedSegments = new LinkedList<MatchContinuation>();
        LinkedList<MatchContinuation> path = lastSegment.getPath();
        int i = 0;
        for (MatchContinuation segment:path)
        {
            String segmentName = mChildren.get(i).getName();
            if (segmentName != null && segmentName.length() > 0)
                namedSegments.add(segment);
            i++;
        }
        return namedSegments;
    }

    private void setValue(MatchContinuation finalResult, ValueConstructor constructor)
    {
        LinkedList<MatchContinuation> namedParameters = getNamedChildren(finalResult);

        SpeechValue[] args = new SpeechValue[namedParameters.size()];
        int i = 0;
        for (MatchContinuation cont:namedParameters)
        {
            args[i] = cont.getValue();
            i++;
        }

        SpeechValue listValue = SpeechValue.make(args);

        if (constructor != null)
        {

            SpeechValue computedResult = constructor.getValue(namedParameters);
            finalResult.setValue(computedResult);
        }
        else
            finalResult.setValue(listValue);
    }




    private ConjunctionPattern(LinkedList<Pattern> children)
    {
        mChildren = new ArrayList<Pattern>();
        mChildren.addAll(children);

    }

    private ConjunctionPattern(TokenizedString[] phrases)
    {
        mChildren = new ArrayList<Pattern>();
        for (TokenizedString p:phrases)
        {
            mChildren.add(StructuredLiteral.make(p.getArray()));
        }
    }

    public static ConjunctionPattern make(LinkedList<Pattern> children)
    {
        return new ConjunctionPattern(children);
    }

    public static ConjunctionPattern make(TokenizedString[] phrases)
    {
        return new ConjunctionPattern(phrases);
    }

    public static ConjunctionPattern make(String[] phrases)
    {
        TokenizedString[] t = new TokenizedString[phrases.length];
        for (int i = 0; i < phrases.length;i++)
        {
            t[i] = TokenizedString.make(phrases[i]);
        }
        return new ConjunctionPattern(t);
    }



}
