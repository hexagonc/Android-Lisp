package com.evolved.automata.speech;

import org.apache.commons.lang3.StringUtils;
import org.junit.Assert;
import org.junit.Test;

import javax.security.auth.login.Configuration;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;

public class SpeechBasicTests {

    @Test
    public void testComplexPatternRecognition()
    {
        String errorMessage = "Failed to create NLUBuilder";

        try
        {
            NLUBuilder nlu = new NLUBuilder();
            nlu.addSpeechType("number", new SimpleDecimalPattern());
            nlu.addSpeechType("feet", new ValueConstructor() {
                        @Override
                        public SpeechValue getValue(Object arg) {
                            LinkedList<MatchContinuation> args  = (LinkedList<MatchContinuation>)arg;

                            if (args.size() != 1)
                                return SpeechValue.make(SpeechArgumentException.make(new String[]{"<number>"}));
                            MatchContinuation cont = args.getFirst();
                            SpeechValue v = cont.getValue();
                            if (v == null || !v.isNumber())
                                return SpeechValue.make(SpeechArgumentException.make("<number>"));
                            v.addType(SpeechValueType.STRING, "feet");
                            return v;
                        }
                    },
                    TokenizedString.make("by <number> feet").getArray());

            nlu.addSpeechType("seconds", new ValueConstructor() {
                        @Override
                        public SpeechValue getValue(Object arg) {
                            LinkedList<MatchContinuation> args  = (LinkedList<MatchContinuation>)arg;

                            if (args.size() != 1)
                                return SpeechValue.make(SpeechArgumentException.make(new String[]{"<number>"}));
                            MatchContinuation cont = args.getFirst();
                            SpeechValue v = cont.getValue();
                            if (v == null || !v.isNumber())
                                return SpeechValue.make(SpeechArgumentException.make("<number>"));
                            v.addType(SpeechValueType.STRING, "seconds");
                            return v;
                        }
                    },
                    TokenizedString.make("for <number> seconds").getArray());


            nlu.addSpeechType("move-forward", new ValueConstructor() {
                        @Override
                        public SpeechValue getValue(Object arg) {
                            return SpeechValue.make("forward");
                        }
                    },
                    TokenizedString.make("forward").getArray());

            nlu.addSpeechType("move-left", new ValueConstructor() {
                        @Override
                        public SpeechValue getValue(Object arg) {
                            SpeechValue distanceValue = (SpeechValue)arg;
                            double distanceScore = distanceValue.getDoubleValue();
                            if ((int)(distanceScore) == 0)
                                return SpeechValue.make("left");
                            else
                                return SpeechValue.make(SpeechArgumentException.make("Too low score"));
                        }
                    },
                    TokenizedString.make("left").getArray());

            nlu.addSpeechType("move-right", new ValueConstructor() {
                        @Override
                        public SpeechValue getValue(Object arg) {
                            return SpeechValue.make("right");
                        }
                    },
                    TokenizedString.make("right").getArray());

            nlu.addSpeechVariants("move-backwards", new ValueConstructor() {
                        @Override
                        public SpeechValue getValue(Object arg) {
                            return SpeechValue.make("backward");
                        }
                    },
                    new String[]{"back", "backward", "backwards"});



            nlu.addSpeechType("duration-action-type", new String[]{"<move-forward>", "<move-backwards>", "<move-left>", "<move-right>"});


            nlu.addSpeechVariants("move", new ValueConstructor() {
                @Override
                public SpeechValue getValue(Object arg) {
                    LinkedList<MatchContinuation> args  = (LinkedList<MatchContinuation>)arg;

                    if (args.size() != 2)
                        return SpeechValue.make(SpeechArgumentException.make(new String[]{"<number>", "<duration-action-type>"}));
                    MatchContinuation cont = args.getFirst();
                    SpeechValue v = cont.getValue();
                    if (v == null || !v.isString())
                        return SpeechValue.make(SpeechArgumentException.make("<duration-action-type>"));

                    String actionType = v.getStringValue();
                    cont = args.get(1);
                    SpeechValue durationValue = cont.getValue();
                    if (durationValue == null || !durationValue.isString() || !durationValue.isNumber())
                        return SpeechValue.make(SpeechArgumentException.make("incorrect feet or seconds"));
                    double distance = durationValue.getDoubleValue();
                    String durationType = durationValue.getStringValue();
                    String desc = "moving " + actionType + " by " + distance + " " + durationType;
                    return SpeechValue.make(desc);
                }
            },
            new String[]{"move <duration-action-type> <feet>", "move <duration-action-type> <seconds>"});


            String input = "move left for 15 seconds";
            LinkedList<MatchContinuation> results = nlu.evaluate(new String[]{"move"}, input);

            System.out.println("Result of " + input + " is: " + results);


        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testPatternRecognition()
    {
        String errorMessage = "failed to create NLUBuilder";

        try
        {
            NLUBuilder nlu = new NLUBuilder();

            nlu.addSpeechType("number",new SimpleDecimalPattern());

            nlu.addSpeechType("speech-plus", new ValueConstructor() {
                @Override
                public SpeechValue getValue(Object arg) {

                    LinkedList<MatchContinuation> argList = (LinkedList<MatchContinuation>)arg;


                    double out = 0;
                    for (MatchContinuation match:argList)
                    {
                        SpeechValue value = match.getValue();
                        if (value.isNumber())
                            out += value.getDoubleValue();
                        else
                            return SpeechValue.make(new IllegalArgumentException("numbers requires"));
                    }


                    return SpeechValue.make(out);
                }
            },
            TokenizedString.make("<number:lvalue> plus <number:rvalue>").getArray());

            String[] speechFunctions = new String[]{"speech-plus"};
            String input = "10 plus 34";
            LinkedList<MatchContinuation> result = nlu.evaluate(speechFunctions, input);

            System.out.println("[" + input + "] is " + result);
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testNumericAlternativePatterns()
    {
        String errorMessage = "Failed to create alternative arithmetic pattern";

        try
        {
            final LinkedList<Pattern> terms = new LinkedList<Pattern>();
            terms.add(new Pattern()
            {

                @Override
                public LinkedList<MatchContinuation> getBestMatchingPrefix(String[] inputTokens, MatchConfiguration config) {

                    LinkedList<String> remaining = new LinkedList<String>();
                    LinkedList<MatchContinuation> results = new LinkedList<MatchContinuation>();
                    int error = 0, matchError = 0;
                    Double value = null;
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

            });

            terms.add(StructuredLiteral.make("plus".split("\\ ")));
            terms.add(StructuredLiteral.make("over the hill".split("\\ ")));
            terms.add(StructuredLiteral.make("over there".split("\\ ")));


            MatchConfiguration config = new MatchConfiguration();
            AlternativeStructuredLiteral optionMatcher = AlternativeStructuredLiteral.make(terms);

            String[] input = new String[]{"over"};

            LinkedList<MatchContinuation> results = optionMatcher.getBestMatchingPrefix(input[0].split("\\ "), config);
            System.out.println("Results: " + results.toString());
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }


    }

    @Test
    public void testNumericPatterns()
    {
        String errorMessage = "Failed to create arithmetic pattern";

        try
        {
            final LinkedList<Pattern> terms = new LinkedList<Pattern>();
            terms.add(new Pattern()
            {

                @Override
                public LinkedList<MatchContinuation> getBestMatchingPrefix(String[] inputTokens, MatchConfiguration config) {

                    LinkedList<String> remaining = new LinkedList<String>();
                    LinkedList<MatchContinuation> results = new LinkedList<MatchContinuation>();
                    int error = 0, matchError = 0;
                    Double value = null;
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
                        }

                    }

                    for (;j < inputTokens.length;j++)
                        remaining.add(inputTokens[j]);
                    MatchContinuation result = new MatchContinuation(SpeechValue.make(value.doubleValue()), matchError, remaining.toArray(new String[0]), null);
                    results.add(result);

                    return results;
                }

                @Override
                public String getName() {
                    return "lvalue";
                }


            });

            terms.add(StructuredLiteral.make(new String[]{"plus"}));
            terms.add(new Pattern()
            {

                @Override
                public LinkedList<MatchContinuation> getBestMatchingPrefix(String[] inputTokens, MatchConfiguration config) {

                    LinkedList<String> remaining = new LinkedList<String>();
                    LinkedList<MatchContinuation> results = new LinkedList<MatchContinuation>();
                    int error = 0, matchError = 0;
                    Double value = null;
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
                        }

                    }

                    for (;j < inputTokens.length;j++)
                        remaining.add(inputTokens[j]);
                    MatchContinuation result = new MatchContinuation(SpeechValue.make(value.doubleValue()), matchError, remaining.toArray(new String[0]), null);
                    results.add(result);

                    return results;
                }

                @Override
                public String getName() {
                    return "rvalue";
                }


            });


            ConjunctionPattern arithmetic = ConjunctionPattern.make(terms);

            String[] input = new String[]{"10 plus 40"};

            MatchConfiguration config = new MatchConfiguration();
            config.setValueConstructor(new ValueConstructor() {
                @Override
                public SpeechValue getValue(Object arg) {
                    LinkedList<MatchContinuation> namedArguments = (LinkedList<MatchContinuation>)arg;
                    double out = 0;
                    for (MatchContinuation cont:namedArguments)
                    {
                        if (cont.getPriorDistance() == 0)
                        {
                            SpeechValue termValue = cont.getValue();
                            if (termValue!=null && termValue.isNumber())
                                out+= termValue.getDoubleValue();
                        }

                    }
                    return SpeechValue.make(out);
                }
            });
            LinkedList<MatchContinuation> results = arithmetic.getBestMatchingPrefix(input[0].split("\\ "), config);
            System.out.println("Results: " + results.toString());
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }


    }


    @Test
    public void testConjunctionPattern()
    {
        String errorMessage = "Failed to create conjuction matcher";

        try
        {
            String[] patterns = new String[]{"the barbarian", "leaped over the mountain", "today"};

            String[][] phrases = new String[][]{{"barbarian", "leaped", "over", "today"}, {"the", "barbarian", "leaped", "over", "mountain", "today"}, {"the warrior"}};
            ConjunctionPattern literal = ConjunctionPattern.make(patterns);

            errorMessage = "Failed to find best matching";
            LinkedList<MatchContinuation> matches = literal.getBestMatchingPrefix(phrases[0], new MatchConfiguration());

            for (MatchContinuation result:matches)
            {
                System.out.println("Found results: " + result + " with grammar: " + result.getValue());
            }

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }


    @Test
    public void testAlternativePattern()
    {
        String errorMessage = "Failed to create alternative matcher";

        try
        {
            String[] patterns = new String[]{"the barbarian", "the amazing randi", "the warrior"};

            String[][] phrases = new String[][]{{"barbarian"}, {"the", "amazing"}, {"the",  "warrior"}, {"the", "randi"}};
            AlternativeStructuredLiteral literal = AlternativeStructuredLiteral.make(patterns);
            MatchConfiguration config = new MatchConfiguration();
            config.addDeleteCostWord("the", 0);
            errorMessage = "Failed to find best matching";
            LinkedList<MatchContinuation> matches = literal.getBestMatchingPrefix(phrases[1], config);

            for (MatchContinuation result:matches)
            {
                System.out.println("Found results: " + result + " with grammar: " + result.getValue());
            }

        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testBestContinuation()
    {
        String errorMessage = "Failed to create StructuredLiteral";

        int defaultInsertCost = 1;
        int defaultDeleteCost = 1;
        int defaultSubCost = 1;

        String[] pattern = StringUtils.splitByWholeSeparator("a history of violence", " ");
        String[][] inputs = new String[][]{StringUtils.splitByWholeSeparator("um a history of violence", " "),
                StringUtils.splitByWholeSeparator("a", " "),
                StringUtils.splitByWholeSeparator("um history of um violence", " "),
                StringUtils.splitByWholeSeparator("violence and history", " "),
                StringUtils.splitByWholeSeparator("whats love got to do with it", " ")};

        HashMap<String, Integer> deleteCostMap = new HashMap<String, Integer>()
        {
            {
                put("um", 0);
            }

        };

        HashMap<String, Integer> insertCostMap = null;

        HashMap<String, String> groupMap = null;
        try
        {
            int testPattern = 4;

            StructuredLiteral literal = StructuredLiteral.make(pattern);
            MatchConfiguration config = new MatchConfiguration();
            config.setDeleteCostMap(deleteCostMap).setInsertCostMap(insertCostMap).setSynonymMap(groupMap);

            String[] input = inputs[testPattern];
            System.out.println("Finding best matching prefix of " + Arrays.toString(input) + " to " + Arrays.toString(pattern));

            errorMessage = "Failed to match tokens";

            LinkedList<MatchContinuation> continuations = literal.getBestMatchingPrefix(input, config);

            System.out.println("Best prefixes: " + continuations);


        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testEditDistanceTrie()
    {
        String errorMessage = "Failed to create StructuredLiteral";

        int defaultInsertCost = 1;
        int defaultDeleteCost = 1;
        int defaultSubCost = 1;

        String[] pattern = StringUtils.splitByWholeSeparator("a history of violence", " ");
        String[][] inputs = new String[][]{StringUtils.splitByWholeSeparator("um a history of violence", " "),
                StringUtils.splitByWholeSeparator("a", " "),
                StringUtils.splitByWholeSeparator("history of violence", " "),
                StringUtils.splitByWholeSeparator("violence and history", " ")};

        HashMap<String, Integer> deleteCostMap = new HashMap<String, Integer>()
        {
            {
                put("um", 0);
                put("uh", 0);
            }

        };

        HashMap<String, Integer> insertCostMap = new HashMap<String, Integer>()
        {
            {
                put("the", 0);
            }
        };

        HashMap<String, String> groupMap = null;
        try
        {
            int testPattern = 3;
            MatchConfiguration config = new MatchConfiguration();

            StructuredLiteral literal = StructuredLiteral.make(pattern);
            config.setDeleteCostMap(deleteCostMap).setInsertCostMap(insertCostMap).setSynonymMap(groupMap);

            String[] input = inputs[testPattern];
            System.out.println("Comparing " + Arrays.toString(pattern) + " to input " + Arrays.toString(input));

            errorMessage = "Failed to match tokens";
            int distance = literal.match(input, config);

            System.out.println("Calculated distance of: " + distance);

            errorMessage = "Failed to compute prefix distances";
            int[] subsequenceErrors = literal.getPrefixMatches(input, config);

            Assert.assertTrue(errorMessage, subsequenceErrors != null && subsequenceErrors.length == input.length + 1);

            System.out.println("Prefix errors: " + Arrays.toString(subsequenceErrors));
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    @Test
    public void testIncrementalEditDistance()
    {
        String errorMessage = "Failed to get incremental errors";

        int defaultInsertCost = 1;
        int defaultDeleteCost = 1;
        int defaultSubCost = 1;

        String[] pattern = StringUtils.splitByWholeSeparator("a history of violence", " ");
        String[][] inputs = new String[][]{StringUtils.splitByWholeSeparator("um a history of violence", " "),
                StringUtils.splitByWholeSeparator("a", " "),
                StringUtils.splitByWholeSeparator("history of violence", " "),
                StringUtils.splitByWholeSeparator("violence and history", " ")};

        HashMap<String, Integer> deleteCostMap = new HashMap<String, Integer>()
        {
            {
                put("um", 0);
            }

        };

        HashMap<String, Integer> insertCostMap = null;

        HashMap<String, String> groupMap = null;
        try
        {
            int testPattern = 3;

            String[] input = inputs[testPattern];
            System.out.println("Comparing " + Arrays.toString(pattern) + " to input " + Arrays.toString(input));

            int[] lastError = new int[pattern.length+1];

            for (int i = 0; i <= pattern.length;i++)
            {
                lastError[i] = i;
            }

            for (int i = 0; i < input.length;i++)
            {
                lastError = getNextEditDistanceLine(input, pattern, i, lastError, insertCostMap, deleteCostMap, defaultInsertCost, defaultDeleteCost, defaultSubCost, groupMap);
                Assert.assertTrue(errorMessage, lastError != null);
                System.out.println("Next error line: " + Arrays.toString(lastError));
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
            Assert.assertTrue(errorMessage, false);
        }
    }

    public int[] getNextEditDistanceLine(String[] input, String[] pattern, int inputIndex, int[] prevError, HashMap<String, Integer> insertCostMap, HashMap<String, Integer> deleteCostMap, int defaultInsertCost, int defaultDeleteCost, int substitutionCost, HashMap<String, String> canonicalWordMap)
    {
        int[] out = new int[prevError.length];
        String inputChar = getGroupWord(input[inputIndex], canonicalWordMap);
        int deleteStepCost = getDeleteCost(inputChar, deleteCostMap, defaultDeleteCost);
        for (int i = 0; i < out.length;i++)
        {


            if (i == 0)
                out[i] = prevError[i] + deleteStepCost;
            else
            {
                String currentPattern = pattern[i-1];
                int prevLeft = out[i - 1];
                int prevTopLeft = prevError[i - 1];
                int prevTop = prevError[i];

                int minStart = Math.min(prevLeft, Math.min(prevTop, prevTopLeft));
                if (currentPattern.equals(inputChar))
                {
                    out[i] = minStart;
                }
                else
                {

                    int insertCost = prevLeft + getInsertCost(currentPattern, insertCostMap, defaultInsertCost);
                    int subCost = prevTopLeft + substitutionCost;
                    int deleteCost = prevTop + deleteStepCost;
                    out[i] = Math.min(insertCost, Math.min(subCost, deleteCost));
                }

            }
        }
        return out;
    }

    public String getGroupWord(String input, HashMap<String, String> canonicalWordMap)
    {
        String nextWord = null;
        while (canonicalWordMap!=null && (nextWord = canonicalWordMap.get(input)) != null && !nextWord.equals(input))
        {
            input = nextWord;
        }
        return input;
    }


    public int getDeleteCost(String inputToken, HashMap<String, Integer> deleteCostMap, int defaultDeleteCost)
    {
        if (deleteCostMap == null)
            return defaultDeleteCost;

        Integer mappedCost = deleteCostMap.get(inputToken);
        if (mappedCost != null)
            return mappedCost;
        else
            return defaultDeleteCost;
    }

    public int getInsertCost(String patternToken, HashMap<String, Integer> insertCostMap, int defaultInsertCost)
    {
        if (insertCostMap == null)
            return defaultInsertCost;

        Integer mappedCost = insertCostMap.get(patternToken);
        if (mappedCost != null)
            return mappedCost;
        else
            return defaultInsertCost;

    }
}
