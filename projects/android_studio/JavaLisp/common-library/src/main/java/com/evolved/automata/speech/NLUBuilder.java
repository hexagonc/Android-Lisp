package com.evolved.automata.speech;


import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.PriorityQueue;

public class NLUBuilder {


    private static class GrammarContinuation {
        TerminatingGroup _prefixGroup;
        String[] _remaining;

        public GrammarContinuation(TerminatingGroup group, String[] remaining)
        {
            _prefixGroup = group;
            _remaining = remaining;
        }
    }



    public class TerminatingGroup {
        public String _grammarToken;
        Pattern _terminatingPattern;

        HashMap<String, TerminatingGroup> _links = null;


        public TerminatingGroup(String token)
        {
            _grammarToken = token;
        }

        public TerminatingGroup(String token, Pattern terminator)
        {
            _grammarToken = token;
            _terminatingPattern = terminator;
        }

        public TerminatingGroup navigateTo(String token)
        {

            if (_links == null || !_links.containsKey(token))
            {
                if (_links == null)
                    _links = new HashMap<String, TerminatingGroup>();
                TerminatingGroup next = new TerminatingGroup(token);
                _links.put(token, next);
                return next;
            }
            else
                return _links.get(token);
        }

        public TerminatingGroup setTerminatorPattern(Pattern p)
        {
            _terminatingPattern = p;
            return this;
        }

        public Pattern getTerminator()
        {
            return _terminatingPattern;
        }

        public TerminatingGroup get(String token)
        {
            if (_links == null)
                return null;
            else
                return _links.get(token);
        }


    }


    GrammarContinuation getNextPattern(String[] tokens)
    {

        TerminatingGroup next, current = mTrieTop;

        LinkedList<String> remaining = new LinkedList<String>();

        boolean finished = false;
        for (int i = 0; i < tokens.length;i++)
        {
            String token = tokens[i];

            if (!finished)
            {
                next = current.get(token);
                if (next != null)
                {
                    current = next;
                }
                else
                {
                    finished = true;
                    remaining.add(token);
                }
            }
            else
                remaining.add(token);

        }

        return new GrammarContinuation(current, remaining.toArray(new String[0]));
    }


    TerminatingGroup mTrieTop;


    private HashMap<String, MatchConfiguration> mDefaultConfig;

    public NLUBuilder()
    {

        mTrieTop = new TerminatingGroup("");
        mDefaultConfig = new HashMap<String, MatchConfiguration>();
    }

    public NLUBuilder addSpeechType(String function, Pattern basePattern)
    {
        mDefaultConfig.put(ReferencePattern.makeReferenceName(function), new MatchConfiguration());
        TerminatingGroup terminator = buildPath(new String[]{ReferencePattern.makeReferenceName(function)});
        terminator._terminatingPattern = basePattern;
        return this;
    }

    public NLUBuilder addSpeechType(String function, ValueConstructor constructor, Pattern basePattern)
    {
        mDefaultConfig.put(ReferencePattern.makeReferenceName(function), (new MatchConfiguration()).setValueConstructor(constructor) );

        TerminatingGroup terminator = buildPath(new String[]{ReferencePattern.makeReferenceName(function)});
        terminator._terminatingPattern = basePattern;
        return this;
    }

    public NLUBuilder addSpeechType(String function, ValueConstructor constructor, String[] grammar)
    {
        mDefaultConfig.put(ReferencePattern.makeReferenceName(function), (new MatchConfiguration()).setValueConstructor(constructor));
        Pattern p = getGrammar(grammar);

        TerminatingGroup terminator = buildPath(new String[]{ReferencePattern.makeReferenceName(function)});
        terminator._terminatingPattern = p;
        return this;
    }

    public MatchConfiguration getDefaultConfiguration(String functionReferenceName)
    {
        return mDefaultConfig.get(functionReferenceName);
    }

    public NLUBuilder addSpeechType(String function, String[] grammarList)
    {
        mDefaultConfig.put(ReferencePattern.makeReferenceName(function), new MatchConfiguration());
        LinkedList<Pattern> options = new LinkedList<Pattern>();

        for (String patternString:grammarList)
        {
            if (ReferencePattern.isReferencePattern(patternString))
                options.add( ReferencePattern.make(this, patternString));
            else
            {
                String[] tokenizedPattern = TokenizedString.make(patternString).getArray();
                Pattern representation = getGrammar(tokenizedPattern);
                options.add(representation);
            }
        }
        AlternativeStructuredLiteral alternative = AlternativeStructuredLiteral.make(options);

        TerminatingGroup terminator = buildPath(new String[]{ReferencePattern.makeReferenceName(function)});
        terminator._terminatingPattern = alternative;


        return this;
    }



    public NLUBuilder addSpeechVariants(String function, ValueConstructor constructor, String[] variants)
    {

        if (variants.length == 1)
        {
            return addSpeechType(function, constructor, TokenizedString.make(variants[0]).getArray());
        }

        String[] grammar = new String[]{ReferencePattern.makeReferenceName(function)};

        mDefaultConfig.put(ReferencePattern.makeReferenceName(function), (new MatchConfiguration()).setValueConstructor(constructor));
        LinkedList<Pattern> patterns = new LinkedList<Pattern>();

        for (int i = 0;i < variants.length;i++)
        {
            Pattern p = getGrammar(TokenizedString.make(variants[i]).getArray());
            patterns.add(p);
        }

        AlternativeStructuredLiteral alternative = AlternativeStructuredLiteral.make(patterns);


        TerminatingGroup terminator = buildPath(grammar);
        terminator.setTerminatorPattern(alternative);
        return this;
    }

    public LinkedList<MatchContinuation> evaluate(String[] speechFunctionNames, String input)
    {
        String[] tokenizedInput = TokenizedString.make(input).getArray();

        Comparator<MatchContinuation> comp = new Comparator<MatchContinuation>() {
            @Override
            public int compare(MatchContinuation o1, MatchContinuation o2) {


                int lvalue = o1.getPriorDistance() + o1.getRemainingTokens().length;
                int rvalue = o2.getPriorDistance() + o2.getRemainingTokens().length;

                if (lvalue <= rvalue)
                    return -1;
                else
                    return 1;
            }
        };

        PriorityQueue<MatchContinuation> heap = new PriorityQueue<>(speechFunctionNames.length, comp);

        for (String speechName:speechFunctionNames)
        {
            MatchConfiguration config = mDefaultConfig.get(ReferencePattern.makeReferenceName(speechName));
            String[] tokenizedPattern = new String[]{ ReferencePattern.makeReferenceName(speechName)};
            Pattern p = getGrammar(tokenizedPattern);

            LinkedList<MatchContinuation> out = p.getBestMatchingPrefix(tokenizedInput, config.copy());
            for (MatchContinuation m:out)
            {
                if (m.getValue()!= null && !m.getValue().isException())
                {
                    heap.add(m);
                }

            }
        }


        boolean first = true;
        int score = 0;

        LinkedList<MatchContinuation> result=null;
        while (heap.size() > 0)
        {
            MatchContinuation match = heap.poll();
            int currentScore = match.getPriorDistance() + match.getRemainingTokens().length;

            if (first)
            {
                score = currentScore;
                result = new LinkedList<MatchContinuation>();
                result.add(match);
                first = false;
            }
            else if (score == currentScore)
            {
                result.add(match);
            }
        }

        return result;
    }



    public TerminatingGroup buildPath(String[] nodes)
    {
        TerminatingGroup node = mTrieTop;
        for (int i = 0;i<nodes.length;i++)
        {
            node = node.navigateTo(nodes[i]);
        }
        return node;
    }

    private String[] appendPrefixGrammar(String[] tokens, LinkedList<Pattern> patternSegments)
    {
        if (tokens == null || tokens.length < 1)
        {
            throw new IllegalArgumentException("Tokens variable must be non-null and longer then 0");
        }

        if (patternSegments == null)
        {
            throw new IllegalArgumentException("Pattern variable cannot be null");
        }

        GrammarContinuation cont = getNextPattern(tokens);
        TerminatingGroup prefix = cont._prefixGroup;
        String[] remaining = cont._remaining;
        Pattern prefixPattern = prefix._terminatingPattern;

        if (prefixPattern == null) {
            int i = 0;
            while (i < tokens.length && !isReference(tokens[i])) {
                i++;
            }
            if (i == 0) {
                String rootName = ReferencePattern.getReferenceName(tokens[i]);
                String[] updatedTokens = new String[tokens.length];
                for (int j = 0;j<updatedTokens.length;j++)
                {
                    if (j == 0)
                        updatedTokens[j] = rootName;
                    else
                        updatedTokens[j] = tokens[j];
                }

                GrammarContinuation innerCont = getNextPattern(updatedTokens);
                prefix = innerCont._prefixGroup;
                prefixPattern = prefix._terminatingPattern;
                remaining = innerCont._remaining;

                if (prefixPattern == null)
                    throw new IllegalStateException("speech function [" + rootName + "] doesn't exist");
                prefixPattern = ReferencePattern.make(this, tokens[i]);
            }
            else
            {
                String[] literal = new String[i];

                remaining = new String[tokens.length - i];
                TerminatingGroup last = null;

                int k = 0;
                for (k = 0; k < tokens.length; k++) {
                    if (k < i) {
                        literal[k] = tokens[k];
                        if (k == 0)
                            last = mTrieTop.navigateTo(tokens[k]);
                        else
                            last = last.navigateTo(tokens[k]);
                    } else {
                        remaining[k - i] = tokens[k];
                        if (k == i && last != null) {
                            prefixPattern = StructuredLiteral.make(literal);
                            last._terminatingPattern = prefixPattern;
                        }
                    }
                }

                if (k == i && last != null) {
                    prefixPattern = StructuredLiteral.make(literal);
                    last._terminatingPattern = prefixPattern;
                }
            }
        }
        else
        {
            String first = tokens[0];
            if (ReferencePattern.isReferencePattern(first))
            {
                prefixPattern = ReferencePattern.make(this, first);
            }
        }

        patternSegments.add(prefixPattern);

        if (remaining.length > 0)
        {
            return appendPrefixGrammar(remaining, patternSegments);
        }
        else
            return remaining;

    }

    public Pattern getConceptReference(String conceptName)
    {
        TerminatingGroup current = mTrieTop;

        TerminatingGroup next = current.get(conceptName);
        if (next != null)
            return next.getTerminator();
        else
            return null;
    }

    public Pattern getGrammar(String[] tokens )
    {
        LinkedList<Pattern> conj = new LinkedList<Pattern>();

        appendPrefixGrammar(tokens, conj);

        if (conj.size() == 1)
            return conj.getFirst();

        return ConjunctionPattern.make(conj);
    }


    public static boolean isReference(String token)
    {
        return token.endsWith(">") && token.startsWith("<") && token.length()>2;
    }



}
