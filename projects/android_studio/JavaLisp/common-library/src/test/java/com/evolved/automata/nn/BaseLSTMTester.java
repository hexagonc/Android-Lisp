package com.evolved.automata.nn;

import com.evolved.automata.AITools;
import com.evolved.automata.ArrayMapper;
import com.evolved.automata.nn.grammar.CharacterNode;
import com.evolved.automata.nn.grammar.GrammarNode;
import com.evolved.automata.nn.grammar.GrammarStateMachine;
import com.sun.org.apache.regexp.internal.RE;

import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by Evolved8 on 1/8/17.
 */
public class BaseLSTMTester {

    public enum ReberGrammar {
        B("B", "T1", "P1"),
        E("E", ReberGrammar.EXIT_STATE_REP),
        P1("P", "T2", "V1"),
        P2("P", "X2", "S2"),
        S1("S", "S1", "X1"),
        S2("S", "E"),
        T1("T", "S1", "X1"),
        T2("T", "T2", "V1"),
        V1("V", "P2", "V2"),
        V2("V", "E"),
        X1("X", "S2", "X2"),
        X2("X", "T2", "V1");

        String token;
        String[] next;
        static final String EXIT_STATE_REP = "^";
        static final String[] ALPHABET = new String[]{"B", "E", "P", "S", "T", "V", "X"};
        ReberGrammar(String t, String... successors)
        {
            token = t;
            next = successors;
        }

        public GrammarNode getNode()
        {
            return CharacterNode.make(token);
        }

        int[] getTransitions()
        {
            int[] nextStates = new int[next.length];
            int exitState = values().length;
            ReberGrammar nodeRep;
            for (int i = 0;i<nextStates.length;i++)
            {
                if (EXIT_STATE_REP.equals(next[i]))
                    nextStates[i] = exitState;
                else
                {
                    nodeRep = ReberGrammar.valueOf(next[i]);
                    nextStates[i] = nodeRep.ordinal();
                }

            }
            return nextStates;
        }
    }

    public GrammarStateMachine makeReberGrammar()
    {
        int i = 0;
        ReberGrammar[] states = ReberGrammar.values();
        int[][] transitions = new int[states.length][];

        GrammarNode[] nodes = new GrammarNode[states.length];
        for (i = 0;i < states.length;i++)
        {
            ReberGrammar g = states[i];
            transitions[i] = g.getTransitions();
            nodes[i] = g.getNode();
        }

        return GrammarStateMachine.make(ReberGrammar.B.ordinal(), nodes, transitions);
    }

    public GrammarStateMachine makeEmbeddedReber()
    {
        int B = 0;
        int RB1 = 1;
        int RB2 = 2;
        int E = 3;
        int P1 = 4;
        int P2 = 5;
        int T1 = 6;
        int T2 = 7;

        GrammarNode[] nodes = new GrammarNode[]{
                CharacterNode.make("B"),
                makeReberGrammar(),
                makeReberGrammar(),
                CharacterNode.make("E"),
                CharacterNode.make("P"),
                CharacterNode.make("P"),
                CharacterNode.make("T"),
                CharacterNode.make("T")};

        int[][] trans = new int[nodes.length][];
        trans[B] = new int[]{T1, P1};
        trans[T1] = new int[]{RB1};
        trans[P1] =new int[]{RB2};
        trans[RB1] = new int[]{T2};
        trans[RB2] = new int[]{P2};
        trans[T2] = new int[]{E};
        trans[P2] = new int[]{E};
        trans[E] = new int[]{nodes.length};

        return GrammarStateMachine.make(B, nodes, trans);
    }


    String getGrammarString(GrammarStateMachine machine)
    {
        machine.reset();
        String v;
        StringBuilder builder = new StringBuilder();
        while ((v = machine.next())!=null)
        {
            builder.append(v);
        }
        return builder.toString();
    }

    String getNewGrammarString(GrammarStateMachine machine, int maxTries, HashSet<String> excludedSet)
    {

        String rstring;
        for (int i = 0;i < maxTries;i++)
        {
            rstring = getGrammarString(machine);
            if (!excludedSet.contains(rstring))
                return rstring;
        }
        return null;
    }

    Pair<HashSet<String>, HashSet<String>> getTrainingTestGrammarSets(GrammarStateMachine machine, int trainingSize, int testSize, int maxTotaltries, int maxStepTries)
    {
        HashSet<String> trainingSet = new HashSet<String>();
        HashSet<String> testSet = new HashSet<String>();
        HashSet<String> totalExcludedSet = new HashSet<String>();


        int failureCount = 0;
        while (trainingSet.size() < trainingSize && failureCount < maxTotaltries)
        {
            String training = getNewGrammarString(machine, maxStepTries, totalExcludedSet);
            if (training != null)
            {
                trainingSet.add(training);
                totalExcludedSet.add(training);
            }
            else
                failureCount++;
        }

        if (trainingSet.size() == trainingSize)
        {
            failureCount = 0;
            while (testSet.size() < testSize && failureCount < maxTotaltries)
            {
                String test = getNewGrammarString(machine, maxStepTries, totalExcludedSet);
                if (test != null)
                {
                    testSet.add(test);
                    totalExcludedSet.add(test);
                }
                else
                    failureCount++;
            }

            if (testSet.size() == testSize)
                return Pair.of(trainingSet, testSet);
        }
        return null;
    }



    float[] getOneHotArray(int hot, int alphabetSize)
    {
        float[] out = new float[alphabetSize];
        out[hot] = 1;
        return out;
    }

    float[][] getOneHotStringRep(String s, String[] alphabet)
    {
        float[][] out = new float[s.length()][];
        HashMap<String, Integer> alphabetMap = new HashMap<String, Integer>();
        String c;
        int i = 0;
        for ( ;i < alphabet.length;i++)
        {
            alphabetMap.put(alphabet[i], i);
        }

        for (i = 0;i < s.length();i++)
        {
            out[i] = getOneHotArray(alphabetMap.get(s.substring(i, i+1)), alphabet.length);
        }
        return out;
    }

    /**
     * Only works well with vectorsStrings constructed from softmax
     * @param vectorString
     * @param alphabet
     * @return
     */
    String getStringFromOneHotVectorRep(float[][] vectorString, String[] alphabet)
    {
        StringBuilder sbuilder = new StringBuilder();
        float[] vectorChar;
        outer: for (int c = 0;c<vectorString.length;c++)
        {
            vectorChar = vectorString[c];
            for (int i = 0;i < vectorChar.length;i++)
            {
                if (roundToInt(vectorChar[i]) == 1)
                {
                    sbuilder.append(alphabet[i]);
                    continue outer;
                }
            }

        }
        return sbuilder.toString();
    }

    int roundToInt(double v)
    {
        if (v < 0.5)
            return 0;
        else
            return 1;
    }

    int roundToInt(float v)
    {
        if (v < 0.5)
            return 0;
        else
            return 1;
    }

    ArrayList<Pair<Vector, Vector>> getSequenceTrainingPairs(float[][] sequenceInput)
    {
        ArrayList<Pair<Vector, Vector>> out = new ArrayList<Pair<Vector, Vector>>();
        Pair<Vector, Vector> pair;
        for (int i = 0;i < sequenceInput.length - 1;i++)
        {
            pair = Pair.of(new Vector(sequenceInput[i]), new Vector(sequenceInput[i+1]));
            out.add(pair);
        }
        return out;
    }


    VectorViewer getNumericVectorViewer(final int range)
    {
        return new VectorViewer() {
            @Override
            public String toString(Vector v)
            {
                if (v == null)
                    return "null";
                else
                    return "" + NNTools.getNumericValueOutput(v, range);
            }
        };
    }

    String viewVectorList(final Vector[] v, final VectorViewer viewer)
    {
        final StringBuilder s = new StringBuilder();
        for (Vector item: v)
        {
            s.append(viewer.toString(item));
            s.append("\n");
        }
        return s.toString();
    }



    Vector[] getNeuralNetworkInputRepresentation(int[] rawInput,  int range, int bitResolution)
    {
        return getNeuralNetworkInputRepresentation(boxArray(rawInput), range, bitResolution);
    }


    Vector[] getNeuralNetworkInputRepresentation(Integer[] numericInput, final int range, final int bitResolution)
    {
        return AITools.mapValues(numericInput, new VectorValueMapper<Integer>() {
            @Override
            public Vector map(Integer input, int index)
            {
                return getNeuralNetworkInputRepresentation(input, range, bitResolution);
            }
        });
    }

    Vector getNeuralNetworkInputRepresentation(Integer numericInput, final int range, final int bitResolution)
    {
        return NNTools.getVector(NNTools.stageDiscretize(numericInput.intValue(), range, bitResolution));
    }

    Vector getNeuralNetworkInputRepresentation(int numericInput, final int range, final int bitResolution)
    {
        return NNTools.getVector(NNTools.stageDiscretize(numericInput, range, bitResolution));
    }


    Double[] boxArray(double[] d)
    {
        Double[] o = new Double[d.length];
        for (int i = 0;i<o.length;i++)
        {
            o[i] = Double.valueOf(d[i]);
        }
        return o;
    }

    Integer[] boxArray(int[] d)
    {
        Integer[] o = new Integer[d.length];
        for (int i = 0;i<o.length;i++)
        {
            o[i] = Integer.valueOf(d[i]);
        }
        return o;
    }


}
